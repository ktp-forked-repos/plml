/*
 * Prolog-MATLAB interface - Version 2
 * Samer Abdallah (2015,2004)
 * Christophe Rhodes (2005)
 *
 * These are some foreign for procedures to enable SWI Prolog to run
 * and communicate with a MATLAB computational engine, which is
 * started as a separate process on the local or a remote machine.
 *
 * Communication is handled by the MATLAB engine API (engFoo
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
 * TODO
 *
 * - There is a problem if the Matlab script decides to pause - there
 * is apparently no way to communicate a keypress to the engine.
 *
 * - Similarly, there is no way to interrupt a long computation.
 * Pressing Ctrl-C to interrupt Prolog seems to have some effect but
 * it seems to confuse the Matlab engine.  Empirically, matlab
 * processes handle some signals (try kill -SEGV `pidof MATLAB`) but
 * not in a useful way.
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
 *      (Later changes may be found in the README file)
 */

#include <SWI-cpp.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>

#ifdef __CHAR16_TYPE__
typedef __CHAR16_TYPE__ char16_t; // fix for Mavericks
#endif

#include "engine.h"



/* The maximum number of simultaneous connections to Matlab from one
   Prolog process. */
#define MAXENGINES 8    
#define BUFSIZE  65536 // buffer for matlab output
#define MAXCMDLEN 256

#define OUTPUT_MAGIC ">> #\n"
#define OUTPUT_MAGIC_LEN 5
#define STATUS_MAGIC "#$&!^"
#define RETURNS_MAGIC "!-)(?\n"
#define STATUS_OFFSET 5
#define ERROR_OFFSET  9
#define EXECFMT "disp('#');try,%s,disp('#$&!^OK');catch Ex,fprintf('#$&!^ERR:%%s\\n',Ex.message);end"
#define EVALFMT "t__ret=cell(1,%d);[t__ret{:}]=%s;disp('!-)(?');pr_uniquevars(t__ret)"

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

static PL_blob_t mx_blob;
static PL_blob_t mxnogc_blob;
static functor_t mlerror;

// Extract an mxArray * from a BLOB atom
static mxArray *term_to_mx(term_t t) {
  PL_blob_t *type;
  size_t    len;
  void *p;
  
  PL_get_blob(t, &p, &len, &type);
  if (type != &mx_blob && type != &mxnogc_blob) {
    throw PlException("Not an mx variable"); 
  }
  return *(mxArray **) p;
}

static mxArray *ablob_to_mx(atom_t a) { 
  return term_to_mx(PlTerm(PlAtom(a))); 
}

// This is for a SWI Prolog BLOB type to manage Matlab workspace
// variables. The variable is cleared and the name reclaimed
// when the blob is garbage collected. This kind of blob has no data
// apart from the atom's name (ie the variable's name)

static PL_blob_t ws_blob;

// structure for keeping track of workspace variables
struct wsvar {
  char name[8]; // designed for short machine generated names
  Engine *engine; // the matlab engine which owns this variable
  atom_t id;    // the id of this engine
};

// extract wsvar from blob term
static struct wsvar *term_to_wsvar(term_t t) {
  PL_blob_t *type;
  size_t     len;
  void *p;
  
  PL_get_blob(t, &p, &len, &type);
  if (type != &ws_blob) {
    throw PlException("Not a ws variable"); 
  }
  return (struct wsvar *) p;
}

// extract wsvar from atom by converting to term first
static struct wsvar *atom_to_wsvar(atom_t a) { 
  return term_to_wsvar(PlTerm(PlAtom(a)));
}


/* MATLAB engine wrapper class */
class eng {
public:
  const char *magic;
  Engine *ep;   // MATLAB API engine pointer
  atom_t id;    // atom associated with this engine
  char *outbuf; // buffer for textual output from MATLAB
        
  eng(): ep(NULL), id(PL_new_atom("")), outbuf(NULL) { magic="mleng"; }
        
  void open(const char *cmd, atom_t id) {
    ep=engOpen(cmd);
    
    if (ep) {
      this->id=id;
      outbuf=new char[BUFSIZE];
      outbuf[BUFSIZE-1]=0;
      engOutputBuffer(ep,outbuf,BUFSIZE-1);
      fprintf(stderr,"plml: Matlab engine (%s) open.\n",PL_atom_chars(id));
    } else {
      throw PlException("open engine failed");
    }
  }
  void close() { 
    engClose(ep); 
    id = PL_new_atom(""); 
    delete [] outbuf; 
    ep=0; 
  }
  
  bool matches(atom_t id) const { return id==this->id; }
  bool isOpen() const { return ep!=NULL; }
};

// pool of engines, all initially closed
static eng engines[MAXENGINES]; 
// functor to be used to wrap array pointers

static pthread_mutex_t EngMutex;

class lock {
public:
	lock() { pthread_mutex_lock(&EngMutex); }
	~lock() { pthread_mutex_unlock(&EngMutex); }
};


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
  foreign_t mlGetOutput(term_t engine, term_t string);
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
  PL_register_foreign("mlGETOUTPUT", 2, (pl_function_t)mlGetOutput, 0);
  
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

  mlerror=PL_new_functor(PL_new_atom("mlerror"),3);
  pthread_mutex_init(&EngMutex,NULL);
}

char *rstrstr(char *s1, const char * s2, size_t *len)
{
	size_t  s1len = strlen(s1);
	size_t  s2len = strlen(s2);
	char *s;
	*len=s1len;

	if (s2len > s1len) return NULL;
	for (s = s1 + s1len - s2len; s >= s1; --s) 
		if (strncmp(s, s2, s2len) == 0) return s;
	return NULL;
}

void check(int rc) { if (!rc) printf("plml: *** Something failed.\n");}

void check_array_index(mxArray *mx, long i) 
{
	 long   n = mxGetNumberOfElements(mx);
	 if (i<=0 || i>n) throw PlException("Index out of bounds");
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
  struct wsvar *x=atom_to_wsvar(a);
  int rc;
  
  char buf[16];
  sprintf(buf,"clear %s",x->name);
  if (pthread_mutex_trylock(&EngMutex)==0) {
     rc=engEvalString(x->engine,buf) ? FALSE : TRUE; 
	  pthread_mutex_unlock(&EngMutex);
	} else {
		rc=FALSE;
	}

	if (rc) {
	  x->name[0]=0;
	  x->engine=0;
  } 
  
  return rc;
}

/* see mx_write */
//int ws_write(IOSTREAM *s, atom_t a, int flags) {
//      struct wsvar *p=atom_to_wsvar(a);
//      mxArray *p=ablob_to_mx(a);
//      fprintf(s,"%s",p->name);
//}


/* Finds the engine associated with the given term
 * (which should just be an atom). Throws an exception
 * if the engine is not found.
 */
static eng *findEngine(term_t id_term) 
{
  atom_t id; 
  if(!PL_get_atom(id_term, &id)) {
    throw PlException("id is not an atom");
  }
  for (int i=0; i<MAXENGINES; i++) {
    if (engines[i].matches(id)) return &engines[i];
  }
  throw PlException("engine not found");
}


static void displayOutput(const char *prefix,const char *p) 
{
	while (*p) {
		fputs(prefix,stderr); 
		while (*p && *p!='\n') fputc(*p++,stderr); 
		fputc('\n',stderr); if (*p) p++;
	}
}

/* utility function to extract UTF-8 encoded character array from 
 * a Prolog string, code list, or atom. */
static const char *get_utf8_string_from_term(term_t t)
{
	const char *s;
	if (!PL_get_chars(t,(char **)&s, CVT_ATOM | CVT_STRING | CVT_LIST | BUF_RING | REP_UTF8)) {
		throw PlException("plml: Could not get UTF-8 string from term");
	}
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
    fprintf(stderr,"plml: mlOPEN/2: Engine %s already open\n",(const char *)PlTerm(id_term));
                PL_succeed;
  } catch (...) {}
  
  try {
    // look for an unused engine structure
    for (int i=0; i<MAXENGINES; i++) {
      if (!engines[i].isOpen()) {
        atom_t id;
        check(PL_get_atom(id_term,&id));
        engines[i].open(PlTerm(servercmd), id);
		  displayOutput("| ",engines[i].outbuf);
        PL_succeed;
      }
    }
    return PL_warning("mlOPEN/2: no more engines available.");
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


static int raise_exception(const char *msg, const char *loc, const char *info) {
  term_t ex = PL_new_term_ref();
  return PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
			 PL_FUNCTOR_CHARS, "plml_error", 3, PL_CHARS, msg, PL_CHARS, loc, PL_CHARS, info,
			 PL_VARIABLE)

		  && PL_raise_exception(ex);
}


/* #include <mach/mach.h> */
/* #include <mach/mach_time.h> */
/* #include <unistd.h> */

/* static double get_time() */
/* { */
/*     static mach_timebase_info_data_t    sTimebaseInfo; */
/*     if (sTimebaseInfo.denom == 0) { (void) mach_timebase_info(&sTimebaseInfo); } */
/*     return (double)mach_absolute_time() * sTimebaseInfo.numer / sTimebaseInfo.denom / 1000000000; */
/* } */

/*
 * Workspace variable handling
 */

static int make_ws_blob(class eng *engine, const char *name, unsigned int len, term_t blob) 
{
  struct wsvar x;

  x.engine = engine->ep;
  x.id     = engine->id;

	if (len+1>sizeof(x.name)) {
	  return raise_exception("name_too_long","uniquevar",name);
	 }
	memcpy(x.name,name,len);
	x.name[len]=0;

  return PL_unify_blob(blob,&x,sizeof(x),&ws_blob);
}

// This will create a new workspace variable with an unused name,
// initialise it to an empty array (to reserve the name) and unify
// the term (which must be a prolog variable) with a blob representing
// the variable. This in turn points back to this engine so that
// if garbage collected, the workspace variable is cleared.
foreign_t mlWSAlloc(term_t eng, term_t blob) {
  // if varname is already bound, we should check
  // that the name has not been used in the workspace
  class eng *engine;
  try { engine=findEngine(eng); }
  catch (PlException &ex) { return ex.plThrow(); }

  {  lock l;
	  if (engEvalString(engine->ep, "uniquevar([])")) 
		  return raise_exception("eval_failed","uniquevar","none");
  }
 
  if (strncmp(engine->outbuf,">> \nans =\n\nt_",13)!=0) {
	  return raise_exception("bad_output_buffer","uniquevar",engine->outbuf);
  }

	unsigned int len=strlen(engine->outbuf+11)-2;
  return make_ws_blob(engine, engine->outbuf+11, len, blob);
}

foreign_t mlWSName(term_t blob, term_t name, term_t engine) {
  // if varname is already bound, we should check
  // that the name has not been used in the workspace
  try {
    struct wsvar *x = term_to_wsvar(blob);
    return ( PL_unify_atom_chars(name, x->name)
			&& PL_unify_atom(engine, x->id));
  } catch (PlException &e) {
    PL_fail; // return e.plThrow(); 
  }
}

// Get a named variable from the MATLAB workspace and return a term
// containing a pointer to an mxArray (in Prolog's memory space).
foreign_t mlWSGet(term_t var, term_t val) {
  try { 
    struct wsvar *x = term_to_wsvar(var);
	 lock l;
    mxArray *p = engGetVariable(x->engine, x->name);
	 if (p) return PL_unify_blob(val, (void **)&p, sizeof(p), &mx_blob);
	 else {
		 return raise_exception("get_variable_failed","mlWSGET",x->name);
	 }
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}


// Put an array back in Matlab workspace under given variable name
foreign_t mlWSPut(term_t var, term_t val) {
  try { 
    struct wsvar *x=term_to_wsvar(var);
	 lock   l;
    return engPutVariable(x->engine, x->name, term_to_mx(val)) ? FALSE : TRUE;
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

/*
 * Executing MATLAB code
 */

static char *decode_output(char *output)
{
	size_t output_len;
	 // EXECFMT starts with disp('#'). This means that the output buffer should
	 // contain at least the 5 characters: ">> #\n". If they are not there,
	 // something is terribly wrong and we must throw an exception.
    if (strncmp(output,OUTPUT_MAGIC,OUTPUT_MAGIC_LEN)!=0) {
		 throw PlException(PlCompound("bad_output_buffer",PlTermv("exec",output)));
	 }
	
	char *p_status = rstrstr(output,STATUS_MAGIC,&output_len);
	if (!p_status) {
		 throw PlException(PlCompound("no_status_magic",PlTermv("exec",output)));
	}
	*p_status = 0; // null terminate string before status 
	output[output_len-1]=0; // get rid of trailing return
	if (!strncmp(p_status+STATUS_OFFSET,"OK",2)) return NULL;
	else if (!strncmp(p_status+STATUS_OFFSET,"ERR:",4)) return p_status+ERROR_OFFSET;
	else throw PlException(PlCompound("no_status_result",PlTermv("exec",output)));
}

class matlab_error {
  public:
    const char *msg, *cmd;
    matlab_error(const char *c, const char *m): cmd(c), msg(m) {}
};

// Call a Matlab engine to execute the given command
static void exec(eng *eng, const char *cmdstr) 
{
	 int	rc, cmdlen=strlen(cmdstr);
    
    // if string is very long, send it via local mxArray
    if (cmdlen>MAXCMDLEN) {
      mxArray *mxcmd=mxCreateString(cmdstr);
      engPutVariable(eng->ep,"t__cmd",mxcmd);
      mxDestroyArray(mxcmd);
		cmdstr="eval(t__cmd)";
		cmdlen=strlen(cmdstr);
	 }

	 {  // limit scope for eval_cmd
		 char *eval_cmd = new char[cmdlen+strlen(EXECFMT)-1];
	 	 if (eval_cmd==NULL) throw PlException("Failed to allocate memory for command");
	 	 sprintf(eval_cmd, EXECFMT, cmdstr);
		 rc=engEvalString(eng->ep,eval_cmd); 
	 	 delete [] eval_cmd;
	}

    if (rc) throw PlException("mlExec: engEvalString failed.");

	 // checks output trailer containing success/error information. Null terminates
	 // just before error information.
	 char *error_msg=decode_output(eng->outbuf);
	 if (error_msg) throw matlab_error(cmdstr,error_msg); 
}

static PlException matlab_exception(term_t engine, matlab_error &e) 
{
	 term_t desc=PL_new_term_ref();
	 term_t cmd=PL_new_term_ref();
	 term_t ex=PL_new_term_ref();

	 PL_put_atom_chars(desc,e.msg);
	 PL_put_atom_chars(cmd,e.cmd);

	 PL_cons_functor(ex,mlerror,engine,desc,cmd);
	return PlException(ex);
}

static size_t list_length(term_t list)
{
	size_t len;
	PL_skip_list(list,0,&len);
	return len;
}

// Call a Matlab engine to execute the given command
foreign_t mlEval(term_t engine, term_t expr, term_t returns) 
{
  try {
    eng *eng=findEngine(engine);
	 int num_rets=list_length(returns);
	 
	 const char *expr_str=get_utf8_string_from_term(expr);
	 char *cmd_str=new char [strlen(expr_str) + strlen(EVALFMT) - 4 + 3 + 1]; // room for 999 rets
	 sprintf(cmd_str,EVALFMT,num_rets,expr_str);

	 lock l;
	 exec(eng,cmd_str);
	
	 // next we must:
    // 1. parse unique variable names from end of output buffer
	 // 2. create ws blobs for them all
	 // 3. unify with returns list

	 return TRUE;
  } catch (matlab_error &e) { return matlab_exception(engine,e).plThrow(); }
  catch (PlException &e) { return e.plThrow(); }
}

// Call a Matlab engine to execute the given command
foreign_t mlExec(term_t engine, term_t cmd) 
{
  try {
    eng *eng=findEngine(engine);
	 const char *cmdstr=get_utf8_string_from_term(cmd);
	 lock l;
	     
	 exec(eng,cmdstr);
    displayOutput("| ", eng->outbuf+5);
	 return TRUE;
  } catch (matlab_error &e) { return matlab_exception(engine,e).plThrow(); }
  catch (PlException &e) { return e.plThrow(); }
}

foreign_t mlGetOutput(term_t engine, term_t s)
{
  try {
    eng *eng=findEngine(engine);
	 return PL_unify_chars(s, PL_STRING | REP_UTF8, -1, eng->outbuf+OUTPUT_MAGIC_LEN);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Get a Prolog string out of a matlab char array 
foreign_t mlMx2String(term_t mx, term_t a)
{
  try {
    char *str = mxArrayToString(term_to_mx(mx));
    if (!str) {
      return PL_warning("array is not a character array");
    }
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
    if (!str) {
      return PL_warning("array is not a character array");
    }
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
    if (!mxIsDouble(mx)) {
      return PL_warning("not numeric");
    }
    if (mxGetNumberOfElements(mx)!=1) {
      return PL_warning("Not a scalar");
    }
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

    if (!mxIsDouble(mx)) return PL_warning("not numeric");
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
		if (mxGetNumberOfElements(mx) != 1) return PL_warning("Not a scalar");

    int f;
    if (mxIsLogical(mx)) {
      f = mxIsLogicalScalarTrue(mx) ? 1 : 0;
    } else if (mxIsDouble(mx)) {
      f = (mxGetScalar(mx) > 0) ? 1 : 0;
    } else {
      return PL_warning("neither numeric nor logical (captain)");
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
		if (!get_list_integers(substerm,&nsubs,(int *)subs)) // !!
			return PL_warning("Bad subscript list");

		// switch to zero-based subscripts
		for (int i=0; i<nsubs; i++) subs[i]--;

		int ind = mxCalcSingleSubscript(mx,nsubs,subs);
		check_array_index(mx,ind);

		return PL_unify_integer(indterm, ind);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Dereference double from mx array
foreign_t mlMxGetFloat(term_t mxterm, term_t index, term_t value)
{
  try {
    mxArray *mx = term_to_mx(mxterm);
	 long   i;

	 check(PL_get_long(index,&i));
	 check_array_index(mx,i);
    if (!mxIsDouble(mx)) { return PL_warning("not numeric"); }

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
	 long   i;

	 check(PL_get_long(index,&i));
	 check_array_index(mx,i);

    if (mxIsLogical(mx)) {
		 mxLogical *p = mxGetLogicals(mx);
		 return PL_unify_integer(value,(p[i-1]) ? 1 : 0);
	 } else if (mxIsDouble(mx)) {
		 double   *p = (double *)mxGetData(mx);
		 return PL_unify_integer(value, (p[i-1]>0) ? 1 : 0);
    } else {
      return PL_warning("neither logical nor numeric");
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
	 long   i;

	 check(PL_get_long(index,&i));
	 check_array_index(mx,i);
    if (!mxIsCell(mx)) { return PL_warning("not numeric"); }

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
	 long   i;
	 char    *fname;

	 check(PL_get_long(index,&i));
	 check(PL_get_atom_chars(field,&fname));
	 check_array_index(mx,i);
    if (!mxIsStruct(mx)) { return PL_warning("not a structure"); }

    mxArray   *p = mxGetField(mx,i-1,fname);
	 if (!p) return PL_warning("Field not present");
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
			return PL_warning("Bad size list");

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
			return PL_warning("Bad size list");

    mxArray *p = mxCreateCellArray(ndims,dims);
    return PL_unify_blob(mx, (void **)&p, sizeof(p), &mxnogc_blob);
  } catch (PlException &e) { 
    return e.plThrow(); 
  }
}

// Create character array. 
foreign_t mlMxCreateString(term_t string, term_t mx) {
  try { 
    mxArray *p = mxCreateString(get_utf8_string_from_term(string));
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
		long   i;
		double val;

		if (!mxIsDouble(mx)) { return PL_warning("not numeric"); }
		check(PL_get_long(index,&i));
		check(PL_get_float(value,&val));
		check_array_index(mx,i);
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
		long   i, len;

		if (!mxIsDouble(mx)) { return PL_warning("not numeric"); }
		check(PL_get_long(index,&i));
		check_array_index(mx,i);
		get_list_doubles(values,&len,mxGetPr(mx)+i-1);
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
		long   i;

		if (!mxIsCell(mx)) { return PL_warning("not a cell array"); }
		check(PL_get_long(index,&i));
		check_array_index(mx,i);
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


/* 
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */

