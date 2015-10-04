/*
 * Part of plml: Prolog-Matlab interface
 * Copyright Samer Abdallah (Queen Mary University of London) 2004-2015
 *           Christophe Rhodes (Goldsmiths College University of London) 2005
 *
 *	This program is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either version 2
 *	of the License, or (at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 * These are some foreign procedures to enable SWI Prolog to run
 * and communicate with a MATLAB computational engine, which is
 * started as a separate process on the local or a remote machine.
 *
 * Communication is handled by the MATLAB engine API (eng*
 * functions) which in turn use a pair of pipes connected to the
 * standard input and output of the MATLAB process.
 *
 * Computations are carried out in the workspace of the MATLAB
 * process, but results can be transported in and out of Prolog's
 * memory space using the {Get,Put}Variable APIs.
 *
 * mxArrays can be created and manipulated on the Prolog side.
 * For example, a large numeric array can be created and send to
 * the engine instead of building a huge expression to evaluate.
 *
 * NOTE: memory management of mxArrays on the Prolog side is complicated
 *   by the use of cell arrays. Currently, there are two kinds of
 *   mxArray reference atom: garbage collected ones and non-garbage
 *   collected ones. You should generally use GCed atoms, but mxArrays
 *   going in or out of cell arrays should not be GCed because the
 *   parent cell array should manage them. Hence, newly created arrays
 *   (using CREATENUMERIC, CREATECELL and CREATESTRING) are NOT
 *   marked for GC because you might want to put them straight into a
 *   cell array. Also, mx atoms returned from GETCELL are not GCed.
 *   If a new array is not going into a cell array, you should use 
 *   NEWREFGC to obtain a managed atom as soon as possible.
 * 
 *   If you have a managed array you want to put into a cell array,
 *   you should use COPYNOGC to make an unmanaged  DEEP COPY of the 
 *   original which can safely be put in the cell array using PUTCELL
 *
 *   A better solution would be to flip the management status of a given
 *   mx_blob atom as necessary.
 *
 *
 * - (See plmatlab.pl for comments about the syntax for Prolog-side
 * users)
 *
 * - There is a problem if the Matlab script decides to pause - there
 * is apparently no way to communicate a keypress to the engine.
 *
 * - There is no established protocol for freeing variables from
 * engGetVariable: they are likely to persist for ever, or at least
 * for a long time, except for those handled by the finalization of
 * prolog terms.
 *
 * - Memory management of mxArray references (see above notes)
 *
 * Changes
 *      3/10/04: Added code to retrieve logical variables.
 *               Added error checking - eval predicates should fail on error.
 *
 *      5/10/04: Added eng::fp which points to input and output streams
 *               of matlab process. This will enable asynchronous evals
 *
 *      22/10/04: Blob handling for mxArray corrected by liberal sprinkling
 *               of asterisks.
 *
 *      12/12/04: Removed non-blob mxArray code and added blobs for Matlab
 *              workspace variables.
 *
 *      13/12/04: Removed all traces of old ws var handling code.
 *
 *      (Later changes may be found in the CHANGES file)
 */

#include <SWI-Stream.h>
#include <SWI-cpp.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <queue>
#include <sstream>

//#ifdef __CHAR16_TYPE__
//typedef __CHAR16_TYPE__ char16_t; // fix for Mavericks
//#endif

#include "engine.h"


/* The maximum number of simultaneous connections to Matlab from one
   Prolog process. */
#define MAXENGINES 8
#define BUFSIZE  65535 // buffer for matlab output
#define MAXCMDLEN 256
#define EVALFMT "lasterr('');disp('{');%s,disp('}')"
#define MAX_RELEASE_QUEUE 2000

/* using namespace std; */

// This is for a SWI Prolog BLOB type to manage mxArray pointers.  It
// means that the Prolog garbage collector can deal with freeing
// unreferenced mxArrays automatically.


#ifdef MX_API_VER
#if MX_API_VER >= 0x07030000
#else
typedef int mwSize;
typedef int mwIndex;
#endif
#else
typedef int mwSize;
typedef int mwIndex;
#endif

#ifdef DEBUG
# define out(c) fputc(c,stderr)
#else
# define out(c)
#endif

#ifdef NOLOCK
# define LOCK 
#else
# define LOCK lock l
#endif

/* 
// debugging aid
class bracket {
public:
	bracket(char c){ out('{'); out(c); }
	bracket(){ out('{'); }
	~bracket() { out('}'); }
};
*/

static PL_blob_t mx_blob;
static PL_blob_t mxnogc_blob;
static functor_t ml_error2, plml_error2, mleng_error2, error2;

// extract a blob from atom
static void atom_to_blob(atom_t a, void **pdata, PL_blob_t **ptype) { 
  size_t    len;
  *pdata=PL_blob_data(a, &len, ptype);
}

// Extract an mxArray * from a BLOB atom
static mxArray *term_to_mx(term_t t) {
  PL_blob_t *type;
  size_t    len;
  void *p;
  
  PL_get_blob(t, &p, &len, &type);
  if (type != &mx_blob && type != &mxnogc_blob) throw PlTypeError("mx(_)|mxnogc(_)",t);
  return *(mxArray **) p;
}

static mxArray *ablob_to_mx(atom_t a) { 
  PL_blob_t *t;
  mxArray **p;

  atom_to_blob(a,(void **)&p,&t);
  return *p;
}

// This is for a SWI Prolog BLOB type to manage Matlab workspace
// variables. The variable is cleared and the name reclaimed
// when the blob is garbage collected. This kind of blob has no data
// apart from the atom's name (ie the variable's name)

static PL_blob_t ws_blob;

class eng; // forward declaration

// structure for keeping track of workspace variables
struct wsvar {
  char name[8]; // designed for short machine generated names
  eng  *engine; // the engine object which owns this variable
};

struct wsname {
  char name[8]; // designed for short machine generated names
};

// Mutexes to protect engines and WS var release queues
static pthread_mutex_t EngMutex, QueueMutex;

class lock {
public:
  lock() { out('-'); pthread_mutex_lock(&EngMutex); out('('); }
  ~lock() { out(')'); pthread_mutex_unlock(&EngMutex); }
};

class qlock {
public:
  qlock() { out('='); pthread_mutex_lock(&QueueMutex); out('['); }
  ~qlock() { out(']'); pthread_mutex_unlock(&QueueMutex); }
};

class qlock_nb {
public:
	class busy {};
  qlock_nb() { if (!pthread_mutex_trylock(&QueueMutex)) throw busy(); out('['); }
  ~qlock_nb() { pthread_mutex_unlock(&QueueMutex); out(']'); }
};

class charbuf {
public:
  char *p;
  charbuf(size_t size): p(new char[size]) { 
    if (p==NULL) throw PlResourceError("memory"); 
  }
  ~charbuf() { delete [] p; }
};

// extract wsvar from blob term
static struct wsvar *term_to_wsvar(term_t t) {
  PL_blob_t *type;
  size_t     len;
  void *p;
  
  PL_get_blob(t, &p, &len, &type);
  if (type != &ws_blob) throw PlTypeError("ws_reference",t);
  return (struct wsvar *)p;
}

static char *append_at(char *p, const char *str) {
  int n=strlen(str);
  memcpy(p,str,n);
  p+=n;
  return p;
}

static PlException api_error(const char *function, const char *arg) {
  term_t ex = PL_new_term_ref();
  if (!PL_unify_term(ex, PL_FUNCTOR, error2,
        PL_FUNCTOR, mleng_error2, PL_CHARS, function, PL_CHARS, arg,
        PL_VARIABLE)) 
    throw PlResourceError();
  return PlException(ex);
}

static PlException plml_error(const char *got, const char *expected) {
  term_t ex = PL_new_term_ref();
  if (!PL_unify_term(ex, PL_FUNCTOR, error2,
        PL_FUNCTOR, plml_error2, PL_CHARS, got, PL_CHARS, expected,
        PL_VARIABLE)) 
    throw PlResourceError();
  return PlException(ex);
}

static PlException ml_error(const char *msg, term_t cmd) {
  term_t ex = PL_new_term_ref();
  if (!PL_unify_term(ex, PL_FUNCTOR, error2, 
        PL_FUNCTOR, ml_error2, PL_CHARS, msg, PL_TERM, cmd, 
        PL_VARIABLE))
    throw PlResourceError();
  return PlException(ex);
}

/* MATLAB engine wrapper class */
class eng {
public:
  const char *magic;
  Engine *ep;   // MATLAB API engine pointer
  atom_t id;    // atom associated with this engine
  char *outbuf; // buffer for textual output from MATLAB
  std::queue<struct wsname> gcqueue; 
        
  eng(): ep(NULL), id(PL_new_atom("")), outbuf(NULL) { magic="mleng"; }

  int enqueue_for_release(const struct wsname& nm) {
    try { // non-blocking attempt to grab mutex
			qlock_nb l; 
			if (gcqueue.size()>=MAX_RELEASE_QUEUE) { return FALSE; } // queue full
			else { gcqueue.push(nm); return TRUE; }
		} catch (qlock_nb::busy x) { // if busy, then just fail
			return FALSE;
		}
  }

  void flush_release_queue_batched() {
	 // NB. must be called with EngMutex held.
	 if (!gcqueue.empty()) {
		char	buf[256], *p0=append_at(buf,"clear"), *pp;
		int	bad=0, n;
		qlock l;

		pp=p0; n=0;
		do {
		  pp=append_at(append_at(pp," "),gcqueue.front().name); 
		  gcqueue.pop(); n++;
		  if (n>=30) { // "clear " + 30*8 < 256 = sizeof(buf)
			 *pp=0;
			 if (engEvalString(ep,buf)!=0) bad++; 
			 pp=p0; n=0;
		  }
		} while (!gcqueue.empty());
		if (n>0) {
		  *pp=0;
		  if (engEvalString(ep,buf)!=0) bad++; 
		}

		if (bad>0) Sfprintf(Serror,"%% plml WARNING: Failed to release %d batches of workspace variables.\n",bad);
	 }
  }

  void open(const char *cmd, atom_t id) {
    ep=engOpen(cmd);
    if (!ep) throw api_error("engOpen",cmd);
    
    this->id=id;
    outbuf=new char[BUFSIZE+1];
    outbuf[BUFSIZE]=0;
    engOutputBuffer(ep,outbuf,BUFSIZE);
  }
  void close() { 
    engClose(ep); 
    id = PL_new_atom(""); 
    delete [] outbuf; 
    ep=0; 
  }
  
  bool matches(atom_t id) const { return id==this->id; }
  bool isOpen() const { return ep!=NULL; }

  void eval(const char *cmd) { 
    if (engEvalString(ep,cmd)) throw api_error("engEvalString",cmd);
  }
  void putvar(const char *nm, mxArray *mx) { 
    if (engPutVariable(ep,nm,mx)) throw api_error("engPutVariable",nm);
  }
  mxArray *getvar(const char *nm) {
    mxArray *p = engGetVariable(ep, nm);
		if (p) return p; else throw api_error("engGetVariable",nm);
  }
};

// pool of engines, all initially closed
static eng engines[MAXENGINES]; 
// functor to be used to wrap array pointers


extern "C" {
// Functions for mx array atom type
  int mx_release(atom_t a);
  int mx_compare(atom_t a, atom_t b);
  // int mx_write(IOSTREAM *s, atom_t a, int flags);
  int mxnogc_release(atom_t a);

// Functions for WS variable atom type
  int ws_release(atom_t a);
  // int ws_write(IOSTREAM *s, atom_t a, int flags);
}

extern "C" {
  install_t install();
  foreign_t mlOpen(term_t servercmd, term_t engine);
  foreign_t mlClose(term_t engine);
  foreign_t mlExec(term_t engine, term_t cmd); 
  foreign_t mlWSGet(term_t var, term_t val);
  foreign_t mlWSPut(term_t var, term_t val);
  foreign_t mlWSName(term_t engine, term_t var, term_t id); 
  foreign_t mlWSAlloc(term_t engine, term_t var); 
  foreign_t mlMx2Atom(term_t mx, term_t atom); 
  foreign_t mlMx2Float(term_t mx, term_t num); 
  foreign_t mlMx2Logical(term_t mx, term_t num); 
  foreign_t mlMx2String(term_t mx, term_t num); 
  foreign_t mlMxInfo(term_t mx, term_t size, term_t type);
  foreign_t mlMxSub2Ind(term_t mx, term_t subs, term_t ind);
  foreign_t mlMxGetFloat(term_t mx, term_t index, term_t value);
  foreign_t mlMxGetLogical(term_t mx, term_t index, term_t value);
  foreign_t mlMxGetCell(term_t mx, term_t index, term_t value);
  foreign_t mlMxGetField(term_t mx, term_t index, term_t field, term_t value);
  foreign_t mlMxGetReals(term_t mx, term_t values);
  foreign_t mlMxCreateNumeric(term_t size, term_t mx);
  foreign_t mlMxCreateCell(term_t size, term_t mx);
  foreign_t mlMxCreateString(term_t string, term_t mx);
  foreign_t mlMxPutFloat(term_t mx, term_t index, term_t value);
  foreign_t mlMxPutFloats(term_t mx, term_t index, term_t values);
  foreign_t mlMxPutCell(term_t mx, term_t index, term_t value);
  foreign_t mlMxCopyNoGC(term_t src, term_t dst);
  foreign_t mlMxNewRefGC(term_t src, term_t dst);
}

install_t install() { 
  PL_register_foreign("mlOPEN", 2, (pl_function_t)mlOpen, 0);
  PL_register_foreign("mlCLOSE", 1, (pl_function_t)mlClose, 0);
  PL_register_foreign("mlEXEC", 2, (pl_function_t)mlExec, 0);
  PL_register_foreign("mlWSNAME", 3, (pl_function_t)mlWSName, 0);
  PL_register_foreign("mlWSALLOC", 2, (pl_function_t)mlWSAlloc, 0);
  PL_register_foreign("mlWSGET", 2, (pl_function_t)mlWSGet,0);
  PL_register_foreign("mlWSPUT", 2, (pl_function_t)mlWSPut, 0);
  PL_register_foreign("mlMX2ATOM", 2, (pl_function_t)mlMx2Atom, 0);
  PL_register_foreign("mlMX2FLOAT", 2, (pl_function_t)mlMx2Float, 0);
  PL_register_foreign("mlMX2LOGICAL", 2, (pl_function_t)mlMx2Logical, 0);
  PL_register_foreign("mlMX2STRING", 2, (pl_function_t)mlMx2String, 0);
  PL_register_foreign("mlMXINFO", 3, (pl_function_t)mlMxInfo, 0);
  PL_register_foreign("mlSUB2IND", 3, (pl_function_t)mlMxSub2Ind, 0);
  PL_register_foreign("mlGETFLOAT", 3, (pl_function_t)mlMxGetFloat, 0);
  PL_register_foreign("mlGETLOGICAL", 3, (pl_function_t)mlMxGetLogical, 0);
  PL_register_foreign("mlGETCELL", 3, (pl_function_t)mlMxGetCell, 0);
  PL_register_foreign("mlGETFIELD", 4, (pl_function_t)mlMxGetField, 0);
  PL_register_foreign("mlGETREALS", 2, (pl_function_t)mlMxGetReals, 0);
  PL_register_foreign("mlCREATENUMERIC", 2, (pl_function_t)mlMxCreateNumeric, 0);
  PL_register_foreign("mlCREATECELL", 2, (pl_function_t)mlMxCreateCell, 0);
  PL_register_foreign("mlCREATESTRING", 2, (pl_function_t)mlMxCreateString, 0);
  PL_register_foreign("mlPUTFLOAT", 3, (pl_function_t)mlMxPutFloat, 0);
  PL_register_foreign("mlPUTFLOATS", 3, (pl_function_t)mlMxPutFloats, 0);
  PL_register_foreign("mlPUTCELL", 3, (pl_function_t)mlMxPutCell, 0);
  PL_register_foreign("mlCOPYNOGC", 2, (pl_function_t)mlMxCopyNoGC, 0);
  PL_register_foreign("mlNEWREFGC", 2, (pl_function_t)mlMxNewRefGC, 0);
  
  mx_blob.magic = PL_BLOB_MAGIC;
  mx_blob.flags = PL_BLOB_UNIQUE;
  mx_blob.name = (char *)"mx";
  mx_blob.acquire = 0; 
  mx_blob.release = mx_release;
  mx_blob.compare = mx_compare;
  mx_blob.write   = 0;

  mxnogc_blob.magic = PL_BLOB_MAGIC;
  mxnogc_blob.flags = PL_BLOB_UNIQUE;
  mxnogc_blob.name = (char *)"mxnogc";
  mxnogc_blob.acquire = 0; 
  mxnogc_blob.release = mxnogc_release;
  mxnogc_blob.compare = mx_compare;
  mxnogc_blob.write   = 0;

  ws_blob.magic = PL_BLOB_MAGIC;
  ws_blob.flags = PL_BLOB_UNIQUE; 
  ws_blob.name = (char *)"ws";
  ws_blob.acquire = 0; 
  ws_blob.release = ws_release;
  ws_blob.compare = 0; 
  ws_blob.write   = 0; 

  ml_error2=PL_new_functor(PL_new_atom("ml_error"),2);
  plml_error2=PL_new_functor(PL_new_atom("plml_error"),2);
  mleng_error2=PL_new_functor(PL_new_atom("mleng_error"),2);
  error2=PL_new_functor(PL_new_atom("error"),2);
  pthread_mutex_init(&EngMutex,NULL);
}

static char *chomp(int n, const char *match, char *p) {
  if (strncmp(p,match,n)!=0) throw plml_error(p,match);
  return p+n;
}

static char *skip_blanks(char *p) {
	while(strncmp(p,">> \n",4)==0) p+=4;
	return p;
}

int array_index(mxArray *mx, term_t index) 
{
	long   n = mxGetNumberOfElements(mx);
  int    i = (int)PlTerm(index);
	if (i<=0 || i>n) {
		std::ostringstream ss;
    ss << "array index in " << 1 << ".." << n;
    throw PlDomainError(ss.str().c_str(),index);
  }
  return i;
}

int unify_list_sizes(term_t list, const mwSize *ints, int num)
{
	list=PL_copy_term_ref(list);

	for (int i=0; i<num; i++) {
		term_t head=PL_new_term_ref();
		term_t tail=PL_new_term_ref();
		if (!PL_unify_list(list,head,tail)) PL_fail; 
		if (!PL_unify_integer(head,ints[i])) PL_fail;
		list=tail;
	}
	return PL_unify_nil(list);
}

int unify_list_doubles(term_t list, double *x, int n)
{
	list=PL_copy_term_ref(list);

	for (int i=0; i<n; i++) {
		term_t head=PL_new_term_ref();
		term_t tail=PL_new_term_ref();
		if (!PL_unify_list(list,head,tail)) PL_fail; 
		if (!PL_unify_float(head,x[i])) PL_fail;
		list=tail;
	}
	return PL_unify_nil(list);
}

// read list of integers from term and write to int array
int get_list_integers(term_t list, long *len, int *vals)
{
	term_t  head=PL_new_term_ref();
	long 		n;

	// copy term ref so as not to modify original
	list=PL_copy_term_ref(list);
	for (n=0;PL_get_list(list,head,list);n++) {
			if (!PL_get_integer(head,&vals[n])) return false;
	}
	if (!PL_get_nil(list)) return false; 
	*len=n;
	return true;
}

// read list of floats from term and write to double array
int get_list_doubles(term_t list, long *len, double *vals)
{
	term_t  head=PL_new_term_ref();
	long 		n;

	// copy term ref so as not to modify original
	list=PL_copy_term_ref(list);
	for (n=0;PL_get_list(list,head,list);n++) {
			if (!PL_get_float(head,&vals[n])) return false;
	}
	if (!PL_get_nil(list)) return false; 
	*len=n;
	return true;
}

/*
 * Member functions for SWIs blob atoms, which allow SWI to manage
 * garbage collection for user-defined data types.
 */
int mx_release(atom_t a) {
  mxArray *p=ablob_to_mx(a);
  mxDestroyArray(p);
  return TRUE;
}

int mx_compare(atom_t a, atom_t b) {
  mxArray *pa=ablob_to_mx(a);
  mxArray *pb=ablob_to_mx(b);
  if (pa<pb) return -1;
  else if (pa>pb) return 1;
  else return 0;
}

int mxnogc_release(atom_t a) { return TRUE; }

int ws_release(atom_t a) {
  struct wsvar *x;
  struct wsname nm;
  PL_blob_t *type;
  
  atom_to_blob(a,(void **)&x,&type);
  if (type!=&ws_blob) return FALSE;
  memcpy(nm.name,x->name,sizeof(nm.name));
  return x->engine->enqueue_for_release(nm);
}


/* Finds the engine associated with the given term
 * (which should just be an atom). Throws an exception
 * if the engine is not found.
 */
static eng *findEngine(term_t id_term) 
{
  atom_t id;
  if (!PL_get_atom(id_term, &id)) throw PlTypeError("atom",id_term);
  for (int i=0; i<MAXENGINES; i++) {
    if (engines[i].matches(id)) return &engines[i];
  }
  throw PlException(PlCompound("plml_unknown_engine", PlTermv(PlTerm(id_term))));
}

static void displayOutput(const char *prefix,const char *p) { Sfputs(p,Scurrent_output); }

/* utility function to extract UTF-8 encoded character array from 
 * a Prolog string, code list, or atom. */
static const char *term_to_utf8_string(term_t t)
{
	const char *s; 
	if (!PL_get_chars(t,(char **)&s, CVT_ATOM | CVT_STRING | CVT_LIST | BUF_RING | REP_UTF8))
		throw PlTypeError("text",t);
	return s;
}

/* 
 * Open a matlab engine using the given command and associate
 * it with the second term, which should be an atom.
 */

foreign_t mlOpen(term_t servercmd, term_t id_term) 
{
  try { 
    findEngine(id_term);
    Sfprintf(Serror,"%% plml WARNING: Engine %s already open\n",(const char *)PlTerm(id_term));
		PL_succeed;
  } catch (...) {}
  
  try {
    // look for an unused engine structure
    atom_t id;
    if (!PL_get_atom_ex(id_term, &id)) return FALSE;
  
    for (int i=0; i<MAXENGINES; i++) {
      if (!engines[i].isOpen()) {
        engines[i].open(PlTerm(servercmd), id);
		    displayOutput("| ",engines[i].outbuf);
        PL_succeed;
      }
    }
    throw PlResourceError("Matlab engines");
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Close a previously opened Matlab engine
foreign_t mlClose(term_t engine) {
  try {   
    findEngine(engine)->close(); 
    PL_succeed; 
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}


/*
 * Workspace variable handling
 */

// This will create a new workspace variable with an unused name,
// initialise it to an empty array (to reserve the name) and unify
// the term (which must be a prolog variable) with a blob representing
// the variable. This in turn points back to this engine so that
// if garbage collected, the workspace variable is cleared.
foreign_t mlWSAlloc(term_t eng, term_t blob) {
  // if varname is already bound, we should check
  // that the name has not been used in the workspace
  try { 
    class eng *engine=findEngine(eng); 
    struct wsvar x;


    { LOCK;
      engine->eval("uniquevar([])"); 

      // skip blank lines (see mlExec())
      const char *pos=chomp(9,"ans =\n\nt_",skip_blanks(engine->outbuf))-2;

      unsigned int len=strlen(pos)-2;
      if (len+1>sizeof(x.name)) 
        throw plml_error(pos,"<uniquevar name>");

      memcpy(x.name,pos,len);
      x.name[len]=0;
      x.engine = engine;
    }
    return PL_unify_blob(blob,&x,sizeof(x),&ws_blob);
  }
  catch (PlException &ex) { return ex.plThrow(); }
}

foreign_t mlWSName(term_t blob, term_t name, term_t engine) {
  // if varname is already bound, we should check
  // that the name has not been used in the workspace
  try {
    struct wsvar *x = term_to_wsvar(blob);
    return ( PL_unify_atom_chars(name, x->name)
			&& PL_unify_atom(engine, x->engine->id));
  } catch (PlException &e) {
    PL_fail; // return e.plThrow(); 
  }
}

// Get a named variable from the MATLAB workspace and return a term
// containing a pointer to an mxArray (in Prolog's memory space).
foreign_t mlWSGet(term_t var, term_t val) {
  try { 
    struct wsvar *x = term_to_wsvar(var);
		mxArray *p;
    { LOCK; p=x->engine->getvar(x->name); }
		return PL_unify_blob(val, (void **)&p, sizeof(p), &mx_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}


// Put an array back in Matlab workspace under given variable name
foreign_t mlWSPut(term_t var, term_t val) {
  try { 
    struct wsvar *x=term_to_wsvar(var);
    x->engine->putvar(x->name, term_to_mx(val));
    return TRUE;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

/*
 * Executing MATLAB code
 */

// Call a Matlab engine to execute the given command
foreign_t mlExec(term_t engine, term_t cmd) 
{
  try {
    eng *eng=findEngine(engine);
    const char *cmdstr=term_to_utf8_string(cmd);
    int	cmdlen=strlen(cmdstr);
    LOCK;

    eng->flush_release_queue_batched();

    // if string is very long, send it via local mxArray
    if (cmdlen>MAXCMDLEN) {
      mxArray *mxcmd=mxCreateString(cmdstr);
      eng->putvar("t__cmd",mxcmd);
      mxDestroyArray(mxcmd);
		  cmdstr="eval(t__cmd)";
		  cmdlen=strlen(cmdstr);
	  }

    {  // scope for eval_cmd
      charbuf eval_cmd(cmdlen+strlen(EVALFMT)-1);
      sprintf(eval_cmd.p, EVALFMT, cmdstr);
      eng->eval(eval_cmd.p);
    }

    char *pos=skip_blanks(eng->outbuf);
    bool completed, parsed=(strncmp(pos,">> {\n",5)==0);

    if (parsed) {
      pos+=5;
    	int len=strlen(pos);	
      
      // check that "}\n" is present and end of output if no buffer overflow
      if (pos+len < eng->outbuf+BUFSIZE) {
        char *trailer_pos=pos+len-2;
        completed=(strncmp(trailer_pos,"}\n",2)==0);
        *trailer_pos=0;
        displayOutput("| ", pos);
      } else {
        displayOutput("| ", pos);
        displayOutput("* ", "\n[TRUNCATED]\n");
        Sfprintf(Serror,"%% plml WARNING: output truncated\n");
        completed=false;
      }
    } 

    // call engine to eval lasterr, then scrape from output buffer
    eng->eval("lasterr");
    pos=chomp(5,"ans =",skip_blanks(eng->outbuf))+2;

    if (strncmp(pos,"     ''",7)!=0) {
      pos[strlen(pos)-2]=0;
      throw ml_error(pos,cmd);
    }
    if (!parsed) throw PlException("ml_syntax_error");
    if (!completed) throw PlException("ml_interrupted");
	  return TRUE;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// ================== mx array handling predicates ================

// Get a Prolog string out of a matlab char array 
foreign_t mlMx2String(term_t mx, term_t a)
{
  try {
    char *str = mxArrayToString(term_to_mx(mx));
    if (!str) return PL_type_error("mx(char)",mx);
    int rc = PL_unify_chars(a, PL_STRING | REP_UTF8, -1, str);
    mxFree(str);
    return rc;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Convert Matlab char array to a Prolog atom
foreign_t mlMx2Atom(term_t mx, term_t a)
{
  try {
    char *str = mxArrayToString(term_to_mx(mx));
    if (!str) return PL_type_error("mx(char)",mx);
    int rc = PL_unify_chars(a, PL_ATOM | REP_UTF8, -1, str);
    mxFree(str);
    return rc;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Convert Matlab numerical array with one element to Prolog float
foreign_t mlMx2Float(term_t mxterm, term_t a)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
    if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm);
    if (mxGetNumberOfElements(mx)!=1) return  PL_domain_error("Matlab scalar",mxterm);
    double x = mxGetScalar(mx);
    
    return PL_unify_float(a, x);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Convert Matlab numerical (REAL) array to list
foreign_t mlMxGetReals(term_t mxterm, term_t a)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
		int       n = mxGetNumberOfElements(mx);

    if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm);
		return unify_list_doubles(a,mxGetPr(mx),n);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Convert Matlab logical or numeric array with one element to 
// Prolog integer 0 or 1 (does not fail or succeed depending on 
// logical value - this is can be done by prolog code).
foreign_t mlMx2Logical(term_t mxterm, term_t a)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
		if (mxGetNumberOfElements(mx) != 1) return PL_type_error("mx(scalar)",mxterm);

    int f;
    if (mxIsLogical(mx)) {
      f = mxIsLogicalScalarTrue(mx) ? 1 : 0;
    } else if (mxIsDouble(mx)) {
      f = (mxGetScalar(mx) > 0) ? 1 : 0;
    } else {
      return PL_type_error("mx(logical)",mxterm);
    }
    
    return PL_unify_integer(a,f);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Get array information (size and type of elements)
foreign_t mlMxInfo(term_t mxterm, term_t size, term_t type)
{
	try {
		mxArray     *mx = term_to_mx(mxterm);
		long      ndims = mxGetNumberOfDimensions(mx);
		const mwSize *dims = mxGetDimensions(mx);
		const char *cnm = mxGetClassName(mx);

		if (PL_unify_atom_chars(type, cnm)) {
			if (dims[ndims-1]==1) ndims--; // remove trailing singletons
			return unify_list_sizes(size,dims,ndims);
		}
		PL_fail;
	} catch (PlException &e) { 
		return e.plThrow(); 
	}
}

// Convert multidimensional subscript to linear index
foreign_t mlMxSub2Ind(term_t mxterm, term_t substerm, term_t indterm)
{
  try {
		mxArray *mx=term_to_mx(mxterm);
		mwIndex	subs[64]; // 64 dimensional should be enough!
		long		nsubs;

		// get substerm as int array
		if (!get_list_integers(substerm,&nsubs,(int *)subs)) 
			return PL_type_error("list(integer)",substerm); 

		// switch to zero-based subscripts
		for (int i=0; i<nsubs; i++) subs[i]--;
		return PlTerm(indterm)=mxCalcSingleSubscript(mx,nsubs,subs)
        && (array_index(mx,indterm),TRUE);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Dereference double from mx array
foreign_t mlMxGetFloat(term_t mxterm, term_t index, term_t value)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
    long i=array_index(mx,index);
    if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm);

    double   *p = (double *)mxGetData(mx);
    return PL_unify_float(value, p[i-1]);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Dereference logical from mx array
foreign_t mlMxGetLogical(term_t mxterm, term_t index, term_t value)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
	  long   i=array_index(mx,i);

    if (mxIsLogical(mx)) {
		 mxLogical *p = mxGetLogicals(mx);
		 return PL_unify_integer(value,(p[i-1]) ? 1 : 0);
	 } else if (mxIsDouble(mx)) {
		 double   *p = (double *)mxGetData(mx);
		 return PL_unify_integer(value, (p[i-1]>0) ? 1 : 0);
    } else {
      return PL_type_error("mx(logical)",mxterm);
	 }

  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Dereference mxArray from cell array
// Note that we return a non-gargage collected atom, otherwise,
// the parent cell array would be spoiled when one of its elements
// is released and destroyed. However, if the parent cell is 
// released and destroyed, any remaining references to elements
// will be prematurely invalidated.
// FIXME: This is almost certain to confuse the garbage collector
foreign_t mlMxGetCell(term_t mxterm, term_t index, term_t value)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
	  long   i=array_index(mx,i);
    if (!mxIsCell(mx)) return PL_type_error("mx(cell)",mxterm);

    mxArray   *p = mxGetCell(mx,i-1);
    return PL_unify_blob(value, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

foreign_t mlMxGetField(term_t mxterm, term_t index, term_t field, term_t value)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
    long   i=array_index(mx,index);
    char    *fname;

    if (!PL_get_atom_chars(field,&fname)) return PL_type_error("atom",field);
    if (!mxIsStruct(mx)) return PL_type_error("mx(struct)",mxterm);

    mxArray   *p = mxGetField(mx,i-1,fname);
    if (!p) return PL_domain_error("field in struct",field);
    return PL_unify_blob(value, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Create numeric array. Currently only real double arrays created
foreign_t mlMxCreateNumeric(term_t size, term_t mx) {
  try { 
		mwSize dims[64];
		long ndims;

		// get size as int array
		if (!get_list_integers(size,&ndims,(int *)dims)) 
			return PL_type_error("list(natural)",mx);

    mxArray *p = mxCreateNumericArray(ndims,dims,mxDOUBLE_CLASS,mxREAL);
    return PL_unify_blob(mx, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Create cell array. 
foreign_t mlMxCreateCell(term_t size, term_t mx) {
  try { 
		mwSize dims[64];
		long ndims;

		// get size as int array
		if (!get_list_integers(size,&ndims,(int *)dims)) 
			return PL_type_error("list(natural)",mx);

    mxArray *p = mxCreateCellArray(ndims,dims);
    return PL_unify_blob(mx, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Create character array. 
foreign_t mlMxCreateString(term_t string, term_t mx) {
  try { 
    mxArray *p = mxCreateString(term_to_utf8_string(string));
    return PL_unify_blob(mx, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}


// Write float into double array
foreign_t mlMxPutFloat(term_t mxterm, term_t index, term_t value)
{
  try {
		mxArray *mx = term_to_mx(mxterm);
		long    i=array_index(mx,index);
		double  val=(double)PlTerm(value);

		if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm);
		*(mxGetPr(mx)+i-1)=val;
		return true;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Write list of floats into double array starting at given index
foreign_t mlMxPutFloats(term_t mxterm, term_t index, term_t values)
{
  try {
		mxArray *mx = term_to_mx(mxterm);
		long   len, i=array_index(mx,index);

		if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm);
		if (!get_list_doubles(values,&len,mxGetPr(mx)+i-1)) return PL_type_error("list(float)",values);
		return true;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Put an mxArray into a cell array
// IMPORTANT: the object being put must in a non-memory managed atom
foreign_t mlMxPutCell(term_t mxterm, term_t index, term_t element)
{
  try {
		mxArray *mx = term_to_mx(mxterm);
		mxArray *el = term_to_mx(element);
		long   i=array_index(mx,index);

		if (!mxIsCell(mx)) return PL_type_error("mx(cell)",mxterm);
		mxSetCell(mx,i-1,el);
		return true;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

foreign_t mlMxCopyNoGC(term_t in, term_t out)
{
  try {
    mxArray *mx = term_to_mx(in);
		mxArray *p = mxDuplicateArray(mx);
    return PL_unify_blob(out, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

foreign_t mlMxNewRefGC(term_t in, term_t out)
{
  try {
    mxArray *p = term_to_mx(in);
    return PL_unify_blob(out, (void **)&p, sizeof(p), &mx_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

/* vim: set sw=2 ts=2 softtabstop=2 expandtab : */
