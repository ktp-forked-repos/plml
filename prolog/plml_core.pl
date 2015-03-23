:- module(plml_core, 
	[	ml_open/1      	% (+Id)
	,	ml_open/2      	% (+Id, +Host)
	,	ml_open/3      	% (+Id, +Host, +Options)
	,	ml_close/1      	% (+Id)

	,	ml_exec/2    		% (+Id, +Expr)
	,	ml_eval/4     		% (+Id, +Expr, +Types, -Vals)
	,	ml_test/2      	% (+Id, +Expr)
   ,  ml_ws_name/3
   ,  leftval/3

	,	(??)/1            % (+Expr)        ~execute Matlab expression
	,	(???)/1           % (+Expr)        ~test Matlab boolean expression
	,	(===)/2           % (-Vals,+Expr)  ~evaluate Matlab expression
	,	wsvar/3      		% (+WSBlob, -Name, -Id)

	% MATBASE
	,	persist_item/2 	% (+Expr,-Expr)     ~ convert volatile subterms to persistent form
	,	matbase_mat/2     % (+Dir, -Loc)      ~ Find matbase MAT files
	,	dropmat/2         % (+Id, +Loc)       ~ remove MAT file from matbase
	,	exportmat/3 		% (+Id, +Loc, +Dir) ~ export MAT file from matbase

	% exported after being imported from ops
	,	op(1100,xfx,::)	% type specification (esp for arrays)
   ,  op(700,xfx,===)   % variable binding/assignment in matlab query
   ,  op(951,fx,??)     % evaluate term as matlab
   ,  op(951,fx,???)    % evaluate term as matlab boolean
	]).
	

:- multifile(user:optionset/2).
:- multifile(user:matlab_path/2).
:- multifile(user:matlab_init/2).


/** <module> Prolog-Matlab interface 

	---++++ Types

	*|ml_eng|* -  Any atom identifying a Matlab engine.

   See plml_dcg.pl for information about Matlab term language.


	@tbd

	Use mat(I) and tmp(I) as types to include engine Id.

	Clarify relationship between return values and valid Matlab denotation.

	Reshape/2 array representation: reshape([ ... ],Size)
	Expression language: arr(Vals,Shape,InnerFunctor) - allows efficient
	representation of arrays of arbitrary things. Will require more strict
	nested list form.

	Deprecate old array(Vals::Type) and cell(Vals::Type) left-value syntax.

	Remove I from ml_expr//2 and add to mx type?
*/
	  
:- use_module(library(apply_macros)).
:- use_module(library(dcg_codes)).
:- use_module(library(plml_dcg)).

:- set_prolog_flag(back_quotes,symbol_char).
:- set_prolog_flag(double_quotes,codes).

:- op(700,xfx,===). % variable binding/assignment in matlab query
:- op(951,fx,??).  % evaluate term as matlab
:- op(951,fx,???). % evaluate term as matlab boolean
% :- op(650,fy,`).	 % quoting things
% :- op(160,xf,``).	 % postfix transpose operator
% :- op(100,fy,@).	 % function handles
% :- op(200,xfy,.^). % array exponentiation
% :- op(410,yfx,.*). % array times
% :- op(410,yfx,./). % array division
% :- op(410,xfy,.\). % array reverse division
% :- op(400,xfy,\).  % matrix reverse division
% :- op(100,yfx,#).  % field indexing (note left-associativity)

:- dynamic current_engine/1.

% NB: Loading Matlab library can change LANG in environment,
% so we have to remember what it was and restore it after loading.
% See also mlOpen: we are going to talk to Matlab via UTF-8 strings.
:- getenv('LANG',Lang), nb_setval(plml_env_lang,Lang).
:-	use_foreign_library(foreign(plml2)).
:- nb_getval(plml_env_lang,Lang), setenv('LANG',Lang), 
   nb_delete(plml_env_lang).

:- initialization(at_halt(ml_closeall)).

ml_closeall :-
	forall(current_engine(Id), ml_close(Id)).


% from utils.pl
bt_call(Do,Undo)  :- Do, (true ; once(Undo), fail).
user:goal_expansion( bt_call(Do,Undo), (Do, (true;	once(Undo), fail))).

%% matlab_init( -Key, -Cmd:ml_expr) is nondet.
%  Each user-defined clause of matlab_init/2 causes Cmd to be executed
%  whenever a new Matlab session is started.

%% matlab_path( -Key, -Path:list(atom)) is nondet.
%  Each user-defined clause of matlab_path/2 causes the directories in Path
%  to be added to the Matlab path of every new Matlab session. Directories
%  are relative to the root directory where padd.m is found.

%% pl2ml_hook(+X:term,-Y:ml_expr) is nondet.
%  Clauses of pl2ml_hook/2 allow for extensions to the Matlab expression 
%  language such that =|V[$X] = V[Y]|= if =|pl2ml_hook(X,Y)|=.



%% ml_open(+Id:ml_eng,+Host:atom,+Options:list(_)) is det.
%% ml_open(+Id:ml_eng, +Host:atom) is det.
%% ml_open(+Id:ml_eng) is det.
%
%  Start a Matlab session on the given host. If Host=localhost
%  or the name of the current current host as returned by hostname/1,
%  then a Matlab process is started directly. Otherwise, it is
%  started remotely via SSH. Options defaults to []. Host defaults to 
%  localhost.
%
%  Start a Matlab session on the specified host using default options.
%  If Host is not given, it defaults to localhost. Session will be
%  associated with the given Id, which should be an atom. See ml_open/3.
%
%  Valid options are below. Note that matlab is always called with
%  the -nodesktop and -nosplash options.
%    * noinit
%      If present, do not run initialisation commands specified by
%      matlab_path/2 and matlab_init/2 clauses. Otherwise, do run them.
%    * debug(In,Out)
%      if present, Matlab is started in a script which captures standard 
%      input and output to files In and Out respectively. (tbd)
%    * cmd(Cmd:atom)
%      Call Cmd as the matlab executable. Default is 'matlab' (i.e. search
%      for matlab on the PATH). Can be used to select a different executable
%      or to add command line options.
%    * awt(Flag:bool)
%      If false (default), call Matlab with -noawt option. Otherwise, Java graphics
%      will be available.

ml_open(Id) :- ml_open(Id,localhost,[]). 
ml_open(Id,Host) :- ml_open(Id,Host,[]).
ml_open(Id,Host,Options) :- 
	ground(Id),
   pack_dir(PackDir), % needed to locate package files
	options_flags(Options,Flags),
	option(cmd(Bin),Options,matlab),
	(	(Host=localhost;hostname(Host))
	-> format(atom(Exec),'exec ~w',[Bin]) % using exec fixes Ctrl-C bug 
	;	format(atom(Exec),'ssh ~w ~w',[Host,Bin])
	),
	(	member(debug(In,Out),Options)
	-> debug(plml,'Running Matlab with protocol logging.',[]),
		debug(plml,'| Prolog > Matlab logged to "~w"',[In]),
		debug(plml,'| Prolog < Matlab logged to "~w"',[Out]),
		absolute_file_name(PackDir/scripts/logio,Spy,[access(execute)]),
		format(atom(Exec1),'~w ~w ~w ~w',[Spy,In,Out,Exec])
	;	Exec1=Exec
	),
	format(atom(Cmd),'~w ~w',[Exec1,Flags]),
	debug(plml,'About to start Matlab with: ~w',[Cmd]),
	mlOPEN(Cmd,Id),
   getenv('LANG',Lang),
	debug(plml,'Setting LANG to ~w and character set to UTF-8.',[Lang]),
   ml_exec(Id,hide(feature(`'DefaultCharacterSet',`'UTF-8'))),
   ml_exec(Id,hide(setenv(`'LANG',`Lang))),
   directory_file_path(PackDir,matlab,MatlabDir),
   ml_exec(Id,addpath(q(MatlabDir))),
   expand_file_name('~/var/matbase',[DBROOT]),
   debug(plml,'Setting MATBASE root to ~q.',[DBROOT]),
   ml_exec(Id,dbroot(q(DBROOT))),

	assert(current_engine(Id)),
	(	member(noinit,Options) -> true
	;	forall( matlab_path(_,Dir), maplist(nofail(addpath),Dir)),
		forall( matlab_init(_,Cmd), nofail(Cmd))
	).
					                                                                   
pack_dir(PackDir) :-
   module_property(plml_core,file(ThisFile)),
   file_directory_name(ThisFile,PrologDir), 
   file_directory_name(PrologDir,PackDir). 

addpath(local(D)) :- !, ml_exec(ml,padl(q(D))).
addpath(D) :- !, ml_exec(ml,padd(q(D))).

%% ml_close(+Id:ml_eng) is det.
%  Close Matlab session associated with Id.
ml_close(Id) :- ground(Id), mlCLOSE(Id), retract(current_engine(Id)).

nofail(P) :- catch(ignore(call(P)), E, print_message(warning,E)).
nofail(P,X) :- catch(ignore(call(P,X)), E, print_message(warning,E)).

options_flags(Opts,Flags) :-
	option(awt(AWT),Opts,false),
	(	AWT=true 
	-> Flags='-nodesktop -nosplash'
	;	Flags='-nodesktop -nosplash -noawt'
	).


%% ml_exec(+Id:ml_eng, +Expr:ml_expr) is det.
%
%  Execute Matlab expression without returning any values.
ml_exec(Id,X)  :- 
	debug(plml,'plml:ml_exec term ~W',[X,[max_depth(10)]]),
	term_mlstring(Id,X,C), !, 
	debug(plml(commands),'plml:ml_exec>> ~s',[C]),
	mlEXEC(Id,C).

%% ml_eval(+Id:ml_eng, +Expr:ml_expr, +Types:list(type), -Res:list(ml_val)) is det.
%
%  Evaluate Matlab expression binding return values to results list Res. This new
%  form uses an explicit output types list, so Res can be completely unbound on entry
%  even when multiple values are required.
ml_eval(Id,X,Types,Vals) :-
	maplist(alloc_ws(Id),Types,Vars,Names), 
	ml_exec(Id,hide(atom_list(Names)=X)), 
	maplist(convert_ws,Types,Vars,Vals).

% alternative approach, hopefully faster
ml_eval_alt(Id,X,Types,Vals) :-
   length(Vals,N),
	ml_exec(Id,[p__rets=cell(1,N);cref(p__rets,[1:N])]=X;map(@uniquevar,__rets)), 
   % now extract ws var names from output and attach to ws blobs in Vals
	time(maplist(convert_ws,Types,Vars,Vals)).

alloc_ws(I,_,Z,N) :- mlWSALLOC(I,Z), mlWSNAME(Z,N,I).

%% ml_test(+Id:ml_eng, +X:ml_expr(bool)) is semidet.
%  Succeeds if X evaluates to true in Matlab session Id.
ml_test(Id,X)   :- ml_eval(Id,X,[bool],[1]).

ml_ws_name(X,Y,Z) :- mlWSNAME(X,Y,Z).

%% ??(X:ml_expr(_)) is det.
%  Execute Matlab expression X as with ml_exec/2, without returning any values.
?? X    :- ml_exec(ml,X).

%% ???(X:ml_expr(bool)) is semidet.
%  Evaluate Matlab boolean expression X as with ml_test/2.
??? Q   :- ml_test(ml,Q).


%% ===(Y:ml_vals(A), X:ml_expr(A)) is det.
%  Evaluate Matlab expression X as in ml_eval/4, binding one or more return values
%  to Y. If Y is unbound or a single ml_val(_), only the first return value is bound.
%  If Y is a list, multiple return values are processed.
Y === X :- 
	(	is_list(Y) 
	-> maplist(leftval,Y,TX,VX), ml_eval(ml,X,TX,VX)
	;	leftval(Y,T,V), ml_eval(ml,X,[T],[V])
	).

%% leftval( +TVal:tagged(T), -T:type, -Val:T) is det.
%  True if TVal is a tagged value whos type is T and value is Val.
leftval( ws(X),     ws,    ws(X)).    
leftval( mx(X),     mx,    mx(X)).
leftval( float(X),  float, X).
leftval( int(X),    int,   X).
leftval( bool(X),   bool,  X).
leftval( atom(X),   atom,  X).
leftval( term(X),   term,  X).
leftval( string(X), string,X).
leftval( mat(X),    mat,   X).
leftval( tmp(X),    tmp,   X).
leftval( loc(X),    loc,   X).
leftval( wsseq(X),  wsseq, wsseq(X)).
leftval( list(T,X), list(T), X).
leftval( array(X::[Size->Type]),  array(Type,Size), X) :- !.
leftval( array(X::[Size]),        array(float,Size), X) :- !.
leftval( cell(X::[Size->Type]),   cell(Type,Size),  X) :- !.
leftval( cell(X::[Size]),         cell(mx,Size),  X) :- !.
leftval( Val:Type,  Type,  Val).


% Once the computation has been done, the MATLAB workspace contains
% the results which must be transferred in the appropriate form the
% specified left-values, in one of several forms, eg mxArray pointer,
% a float, an atom, a string or a locator. 
%
% Note that requesting a locator causes a further call
% to MATLAB to do a dbsave.
%
% If no type requestor tag is present, then a unique variable name
% is generated to store the result in the Matlab workspace. This name
% is returned in the variable as a ws blob.
% The idea is to avoid unnecessary traffic over the Matlab engine pipe.

% conversion between different representations of values
% !! FIXME: check memory management of mxArrays here


%% convert_ws( +Type:type, +In:ws_blob, -Out:Type) is det.
%  Convert value of Matlab workspace variable to representation
%  determined by Type.
convert_ws(ws, Z, ws(Z)) :- !.
convert_ws(wsseq, Z, wsseq(Z)) :- !.
convert_ws(mx, Z, mx(Y)) :- !, mlWSGET(Z,Y). 

% conversions that go direct from workspace variables to matbase.
convert_ws(tmp, Z, Y) :- !, mlWSNAME(Z,_,I), bt_call(db_tmp(I,ws(Z),Y), db_drop(I,Y)).
convert_ws(mat, Z, Y) :- !, mlWSNAME(Z,_,I), bt_call(db_save(I,ws(Z),Y), db_drop(I,Y)).

% return cell array as list of temporary or permanent mat file locators
% (this avoids getting whole array from WS to MX).
convert_ws(cell(tmp,Size), Z, L) :- !, 
	mlWSNAME(Z,_,I),
	bt_call(db_tmp_all(I,ws(Z),L,Size), db_drop_all(I,L,Size)).

convert_ws(cell(mat,Size), Z, L) :- !, 
	mlWSNAME(Z,_,I),
	bt_call(db_save_all(I,ws(Z),L,Size), db_drop_all(I,L,Size)).

% Most other conversions from ws(_) go via mx(_)
convert_ws(T,Z,A) :- 
	mlWSGET(Z,X), 
	convert_mx(T,X,A).


%% convert_mx( +Type:type, +In:mx_blob, -Out:Type) is det.
%  Convert value of in-process Matlab array In to representation
%  determined by Type.
convert_mx(atom,   X, Y) :- !, mlMX2ATOM(X,Y).
convert_mx(bool,   X, Y) :- !, mlMX2LOGICAL(X,Y).
convert_mx(float,  X, Y) :- !, mlMX2FLOAT(X,Y).
convert_mx(int,    X, Y) :- !, mlMX2FLOAT(X,Z), Y is truncate(Z).
convert_mx(string, X, Y) :- !, mlMX2STRING(X,Y).
convert_mx(term,   X, Y) :- !, mlMX2ATOM(X,Z), term_to_atom(Y,Z).
convert_mx(loc,    X, mat(Y,W)) :- !, mlMX2ATOM(X,Z), term_to_atom(Y|W,Z).

convert_mx(mat,    X, Y) :- !, % !!! use first engine to save to its matbase
	current_engine(I),  
	bt_call( db_save(I,mx(X),Y), db_drop(I,Y)).
convert_mx(tmp,    X, Y) :- !, % !!! use first engine to save to its matbase
	current_engine(I),  
	bt_call( db_tmp(I,mx(X),Y), db_drop(I,Y)).

convert_mx(list(float), X, Y) :- !, mlGETREALS(X,Y). 

convert_mx(cell(Type,Size), X, L) :- !, 
	mx_size_type(X,Size,cell),
	prodlist(Size,1,Elems), % total number of elements
	mapnats(conv_cref(Type,X),Elems,[],FL),
	reverse(Size,RSize),
	unflatten(RSize,FL,L).

convert_mx(array(Type,Size), X, L) :- !, 
	mx_size_type(X,Size,MXType),
	compatible(MXType,Type),
	prodlist(Size,1,Elems), % total number of elements
	mapnats(conv_aref(Type,X),Elems,[],FL),
	reverse(Size,RSize),
	unflatten(RSize,FL,L).

compatible(double,float).
compatible(double,int).
compatible(double,bool).
compatible(logical,float).
compatible(logical,int).
compatible(logical,bool).

% !! Need to worry about non gc mx atoms
conv_aref(bool,  X,I,Y) :- !, mlGETLOGICAL(X,I,Y). 
conv_aref(float, X,I,Y) :- !, mlGETFLOAT(X,I,Y). 
conv_aref(int,   X,I,Y) :- !, mlGETFLOAT(X,I,W), Y is truncate(W). 

conv_cref(mx,Z,I,Y) :- !, mlGETCELL(Z,I,Y). % !! non gc mx
conv_cref(Ty,Z,I,Y) :- !, conv_cref(mx,Z,I,X), convert_mx(Ty,X,Y).

%convert(W, field(Z,N,I)) :- convert(mx(X),Z), mlGETFIELD(X,I,N,Y), convert_mx(W,Y). 
%convert(W, field(Z,N))   :- convert(mx(X),Z), mlGETFIELD(X,1,N,Y), convert_mx(W,Y). 

% Utilities used by convert/2

mapnats(P,N,L1,L3) :- succ(M,N), !, call(P,N,PN), mapnats(P,M,[PN|L1],L3).
mapnats(_,0,L,L) :- !.

prodlist([],P,P).
prodlist([X1|XX],P1,P3) :- P2 is P1*X1, prodlist(XX,P2,P3).

concat(0,_,[]) --> !, [].
concat(N,L,[X1|XX]) --> { succ(M,N), length(X1,L) }, X1, concat(M,L,XX).

plml_dcg:pl2ml_hook(I,ws(A),\atm(N)) :- mlWSNAME(A,N,I).
plml_dcg:pl2ml_hook(I,mx(X),$ws(Z)) :- mlWSALLOC(I,Z), mlWSPUT(Z,X).


% convert a flat list into a nested-list array representation 
% using given size specification
unflatten([N],Y,Y) :- !, length(Y,N).
unflatten([N|NX],Y,X) :-
	length(Y,M),
	L is M/N, integer(L), L>=1,
	phrase(concat(N,L,Z),Y),
	maplist(unflatten(NX),Z,X).

% thin wrappers
mx_size_type(X,Sz,Type) :- mlMXINFO(X,Sz,Type).
mx_sub2ind(X,Subs,Ind) :- mlSUB2IND(X,Subs,Ind).


% these create memory managed arrays, which are not suitable
% for putting into a cell array

% roughly, mx_create :: type -> mxarray.
mx_create([Size],mx(X))    :- mlCREATENUMERIC(Size,Z), mlNEWREFGC(Z,X).
mx_create({Size},mx(X))    :- mlCREATECELL(Size,Z), mlNEWREFGC(Z,X).
mx_string(string(Y),mx(X)) :- mlCREATESTRING(Y,Z), mlNEWREFGC(Z,X).

% MX as MUTABLE variables
mx_put(aref(mx(X),I),float(Y)) :- mlPUTFLOAT(X,I,Y).
mx_put(cref(mx(X),I),mx(Y))    :- mlPUTCELL(X,I,Y). % !! ensure that Y is non gc
mx_put(mx(X),list(float,Y))    :- mlPUTFLOATS(X,1,Y). 

%% wsvar(+X:ws_blob(A), -Nm:atom, -Id:ml_eng) is semidet.
%  True if X is a workspace variable in Matlab session Id.
%  Unifies Nm with the name of the Matlab variable.
wsvar(A,Name,Engine) :- mlWSNAME(A,Name,Engine).

/* __________________________________________________________________________________
 * Dealing with the Matbase
 *
 * The Matbase is a file system tree which contains lots of
 * MAT files which have been created by using the dbsave
 * Matlab function.
 */


% saving and dropping matbase files
db_save(I,Z,Y)   :- ml_eval(I,dbsave(Z),[loc],[Y]).
db_tmp(I,Z,Y)    :- ml_eval(I,dbtmp(Z),[loc],[Y]).
db_drop(I,mat(A,B)) :- ml_exec(I,dbdrop(\loc(A,B))).

db_save_all(I,Z,L,Size) :- ml_eval(I,dbcellmap(@dbsave,Z),[cell(loc,Size)],[L]). 
db_tmp_all(I,Z,L,Size)  :- ml_eval(I,dbcellmap(@dbtmp,Z),[cell(loc,Size)],[L]). 
db_drop_all(I,L,Size)   :- 
	length(Size,Dims), 
	ml_exec(I,hide(foreach(@dbdrop,arr(Dims,L,X\\{loc(X)})))).


%% dropmat(+Id:ml_id, +Mat:ml_loc) is det.
%  Deleting MAT file from matbase.
dropmat(Eng,mat(A,B))       :- db_drop(Eng,mat(A,B)).

%% exportmat(+Id:ml_id, +Mat:ml_loc, +Dir:atom) is det.
%  Export specified MAT file from matbase to given directory.
exportmat(Eng,mat(A,B),Dir) :- ml_exec(Eng,copyfile(dbpath(\loc(A,B)),\q(wr(Dir)))).

%% matbase_mat(+Id:ml_eng,-X:ml_loc) is nondet.
%  Listing mat files actually in matbase at given root directory.
matbase_mat(Id,mat(SubDir/File,x)) :-
	ml_eval(Id,[dbroot,q(/)],[atom],[DBRoot]), % NB with trailing slash

	atom_concat(DBRoot,'*/d*',DirPattern),
	expand_file_name(DirPattern,Dirs),
	member(FullDir,Dirs), 
	atom_concat( DBRoot,SubDirAtom,FullDir),
	term_to_atom(SubDir,SubDirAtom),
	atom_concat(FullDir,'/m*.mat',FilePattern),
	expand_file_name(FilePattern,Files),
	member(FullFile,Files),
	file_base_name(FullFile,FN),
	atom_concat(File,'.mat',FN).
	

%% persist_item(+X:ml_expr(A),-Y:ml_expr(A)) is det.
%  Convert Matlab expression to persistent form not dependent on
%  current Matlab workspace or MX arrays in Prolog memory space.
%  Large values like arrays and structures are saved in the matbase
%  replaced with matbase locators. Scalar values are converted to 
%  literal numeric values. Character strings are converted to Prolog atoms.
%  Cell arrays wrapped in the wsseq/1 functor are converted to literal
%  form.
%
%  NB. any side effects are undone on backtracking -- in particular, any
%  files created in the matbase are deleted.
persist_item($T,$T) :- !.
persist_item(mat(A,B),mat(A,B)) :- !.  

persist_item(ws(A),B) :- !,
	mlWSNAME(A,_,Eng), 
	ml_eval(Eng,typecode(ws(A)),[int,bool,bool],[Numel,IsNum,IsChar]),
	(	Numel=1, IsNum=1
	->	convert_ws(float,A,B) 
	;	IsChar=1
	-> convert_ws(atom,A,AA), B= `AA
	;	convert_ws(mat,A,B)
	).


% !! TODO - 
%     deal with collections - we can either save the aggregate
%     OR save the elements individually and get a prolog list of the
%     locators.
persist_item(wsseq(A),cell(B)) :- 
	mlWSNAME(A,_,Eng), 
	ml_test(Eng,iscell(ws(A))), 
	ml_eval(Eng,wsseq(A),[cell(mat,_)],[B]).

persist_item(mx(X),B) :- 
	mx_size_type(X,Size,Type),
	(	Size=[1], Type=double
	->	convert_mx(float,X,B) 
	;	Type=char
	-> convert_mx(atom,X,AA), B= `AA
	;	convert_mx(mat,X,B)
	).

persist_item(A,A)   :- atomic(A).


prolog:message(ml_illegal_expression(Expr),[ 'Illegal Matlab expression: ~w'-[Expr] | Z], Z).
prolog:message(mlerror(Eng,Msg,Cmd),[
'Error in Matlab engine (~w):\n   * ~w\n   * while executing "~w"'-[Eng,Msg,Cmd] | Z], Z).

hostname(H) :-
   (  getenv('HOSTNAME',H) -> true
   ;  read_line_from_pipe(hostname,H)
   ).

read_line_from_pipe(Cmd,Atom) :-
   setup_call_cleanup(
      open(pipe(Cmd),read,S),
      (read_line_to_codes(S,Codes), atom_codes(Atom,Codes)),
      close(S)).

