/*
 * Part of plml: Prolog-Matlab interface
 * Copyright Samer Abdallah (Queen Mary University of London; UCL) 2004-2015
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

:- module(plml,
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

	,	term_mlstring/3   % (+Id, +Expr, -String)  ~Prolog term to Matlab string
	,	term_texatom/2		% (+Expr, -Atom)         ~Prolog term to TeX expression
	,	wsvar/3      		% (+WSBlob, -Name, -Id)

	% MATBASE
	,	persist_item/2 	% (+Expr,-Expr)     ~ convert volatile subterms to persistent form
	,	matbase_mat/2     % (+Dir, -Loc)      ~ Find matbase MAT files
	,	dropmat/2         % (+Id, +Loc)       ~ remove MAT file from matbase
	,	exportmat/3 		% (+Id, +Loc, +Dir) ~ export MAT file from matbase


	% Utilities
	,	compileoptions/2
	,	multiplot/2
	,  mhelp/1

	,	op(650,fy,'`')	 	% quoting things
	,	op(160,xf,'``')		% postfix transpose operator
	,	op(100,fy,@)		% function handles

	% note slightly reduced precedence of array operators -
	% hope this doesn't break everything...
	,	op(210,xfy,.^) 	% array exponentiation
	,	op(410,yfx,.*) 	% array times
	,	op(410,yfx,./) 	% array division
	,	op(410,xfy,.\) 	% reverse array division
	,	op(400,xfy,\) 		% reverse matrix division
	,	op(700,xfx,===) 	% variable binding/assignment in matlab query
	,	op(700,xfx,:==) 	% variable binding/assignment in matlab query
	,	op(951,fx,??) 		% evaluate term as matlab
	,	op(951,fx,???) 	% evaluate term as matlab boolean
	,	op(100,yfx,#)     % field indexing (note left-associativity)
	,	op(750,fy,\\)		% thunk abdstraction
	,  op(750,xfy,\\)    % lambda abdstraction

	% exported after being imported from ops
	,	op(1100,xfx,::)	% type specification (esp for arrays)
	,	op(550,xfx,..)		% range of integers
	]).


:- multifile(user:optionset/2).
:- multifile(user:matlab_path/2).
:- multifile(user:matlab_init/2).
:- multifile(user:pl2ml_hook/2).


/** <module> Prolog-Matlab interface

	---++++ Types

	*|ml_eng|* -  Any atom identifying a Matlab engine.

	*|ml_stmt|* - A Matlab statement
	==
	X;Y     :: ml_stmt :-  X::ml_stmt, Y::ml_stmt.
	X,Y     :: ml_stmt :-  X::ml_stmt, Y::ml_stmt.
	X=Y     :: ml_stmt :-  X::ml_lval, Y::ml_expr.
	hide(X) :: ml_stmt :-  X::ml_stmt.
	==

	==
	ml_expr(A)       % A Matlab expression, possibly with multiple return values
	ml_loc ---> mat(atom,atom).  % Matbase locator
	==

	---++++ Matlab expression syntax

	The Matlab expression syntax adopted by this module allows Prolog terms to represent
	or denote Matlab expressions. Let T be the domain of recognised Prolog terms (corresponding to
	the type ml_expr), and M be the domain of Matlab expressions written in Matlab syntax.
	Then V : T->M is the valuation function which maps Prolog term X to Matlab expression V[X].
	These are some of the constructs it recognises:

	Constructs valid only in top level statements, not subexpressions:
	==
	X;Y             % |--> V[X]; V[Y]  (sequential evaluation hiding first result)
	X,Y             % |--> V[X], V[Y]  (sequential evaluation displaying first result)
	X=Y             % |--> V[X]=V[Y] (assignment, X must denote a valid left-value)
	hide(X)         % |--> V[X]; (execute X but hide return value)
	if(X,Y)         % |--> if V[X], V[Y], end
	if(X,Y,Z)       % |--> if V[X], V[Y], else V[Z], end
	==

	Things that look and work like Matlab syntax (more or less):
	==
	+X              % |--> uplus(V[X])
	-X              % |--> uminus(V[X])
	X+Y             % |--> plus(V[X],V[Y])
	X-Y             % |--> minus(V[X],V[Y])
	X^Y             % |--> mpower(V[X],V[Y])
	X*Y             % |--> mtimes(V[X],V[Y])
	X/Y             % |--> mrdivide(V[X],V[Y])
	X\Y             % |--> mldivide(V[X],V[Y])
	X.^Y            % |--> power(V[X],V[Y])
	X.*Y            % |--> times(V[X],V[Y])
	X./Y            % |--> rdivide(V[X],V[Y])
	X.\Y            % |--> ldivide(V[X],V[Y])
	X:Y:Z           % |--> colon(V[X],V[Y],V[Z])
	X:Z             % |--> colon(V[X],V[Z])
	X>Z             % |--> gt(V[X],V[Y])
	X>=Z            % |--> ge(V[X],V[Y])
	X<Z             % |--> lt(V[X],V[Y])
	X=<Z            % |--> le(V[X],V[Y])
	X==Z            % |--> eq(V[X],V[Y])
	[X1,X2,...]     % |--> [ V[X1], V[X2], ... ]
	[X1;X2;...]     % |--> [ V[X1]; V[X2]; ... ]
	{X1,X2,...}     % |--> { V[X1], V[X2], ... }
	{X1;X2;...}     % |--> { V[X1]; V[X2]; ... }
	@X              % |--> @V[X] (function handle)
	==

	Things that do not look like Matlab syntax but provide standard Matlab features:
	==
	'Infinity'      % |--> inf (positive infinity)
	'Nan'           % |--> nan (not a number)
	X``             % |--> ctranpose(V[X]) (conjugate transpose, V[X]')
	X#Y             % |--> getfield(V[X],V[q(Y)])
	X\\Y            % |--> @(V[X])V[Y] (same as lambda(X,Y))
	\\Y             % |--> @()V[Y] (same as thunk(Y))
	lambda(X,Y)     % |--> @(V[X])V[Y] (anonymous function with arguments X)
	thunk(Y)        % |--> @()V[Y] (anonymous function with no arguments)
	vector(X)       % |--> horzcat(V[X1],V[X2], ...)
	atvector(X)     % as vector but assumes elements of X are assumed all atomic
	cell(X)         % construct 1xN cell array from elements of X
	`X              % same as q(X)
	q(X)            % wrap V[X] in single quotes (escaping internal quotes)
	tq(X)           % wrap TeX expression in single quotes (escape internal quotes)
	==

	Referencing different value representations.
	==
	mat(X,Y)           % denotes a value in the Matbase using a dbload expression
	mx(X:mx_blob)      % denotes an MX Matlab array in SWI memory
	ws(X:ws_blob)      % denotes a variable in a Matlab workspace
	wsseq(X:ws_blob)   % workspace variable containing list as cell array.
	==

	Tricky bits.
	==
	apply(X,AX)        % X must denote a function or array, applied to list of arguments AX.
	cref(X,Y)          % cell dereference, |--> V[X]{ V[Y1], V[Y2], ... }
	arr(Lists)         % multidimensional array from nested lists.
	arr(Lists,Dims)    % multidimensional array from nested lists.
	==

	Things to bypass default formatting
	==
	noeval(_)          % triggers a failure when processed
	atom(X)            % write atom X as write/1
	term(X)            % write term X as write/1
	\(P)               % escape and call phrase P directly to generate Matlab string
	$(X)               % calls pl2ml_hook/2, denotes V[Y] where plml_hook(X,Y).
	'$VAR'(N)          % gets formatted as p_N where N is assumed to be atomic.
	==

	All other Prolog atoms are written using write/1, while other Prolog terms
	are assumed to be calls to Matlab functions named according to the head functor.
	Thus V[ <head>( <arg1>, <arg2>, ...) ] = <head>(V[<arg1>, V[<arg2>], ...).

	There are some incompatibilities between Matlab syntax and Prolog syntax,
	that is, syntactic structures that Prolog cannot parse correctly:

		* 'Command line' syntax, ie where a function of string arguments:
		  "save('x','Y')" can be written as "save x Y" in Matlab,
		  but in Prolog, you must use function call syntax with quoted arguments:
		  save(`x,`'Y').

		* Matlab's postfix transpose operator "x'" must be written using a different
		  posfix operator "x``" or function call syntax "ctranspose(x)".

		* Matlab cell referencing using braces, as in x{1,2} must be written
		  as "cref(x,1,2)".

		* Field referencing using dot (.), eg x.thing - currently resolved
		  by using hash (#) operator, eg x#thing.

		* Using variables as arrays and indexing them. The problem is that
		  Prolog doesn't let you write a term with a variable as the head
		  functor.


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
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes),except([q//1])).

:- set_prolog_flag(back_quotes,symbol_char).
:- set_prolog_flag(double_quotes,codes).

:- op(700,xfx,===). % variable binding/assignment in matlab query
:- op(951,fx,??).  % evaluate term as matlab
:- op(951,fx,???). % evaluate term as matlab boolean
:- op(650,fy,`).	 % quoting things
:- op(160,xf,``).	 % postfix transpose operator
:- op(100,fy,@).	 % function handles
:- op(200,xfy,.^). % array exponentiation
:- op(410,yfx,.*). % array times
:- op(410,yfx,./). % array division
:- op(410,xfy,.\). % array reverse division
:- op(400,xfy,\).  % matrix reverse division
:- op(100,yfx,#).  % field indexing (note left-associativity)

:- dynamic current_engine/1.

read_line_from_pipe(Cmd,Atom) :-
   setup_call_cleanup(
      open(pipe(Cmd),read,S),
      (read_line_to_codes(S,Codes), atom_codes(Atom,Codes)),
      close(S)).

% NB: Loading Matlab library can change LANG in environment,
% so we have to remember what it was and restore it after loading.
% See also mlOpen: we are going to talk to Matlab via UTF-8 strings.
:- (getenv('LANG',Lang) -> LL=just(Lang); LL=nothing), nb_setval(plml_env_lang,LL).
:-	use_foreign_library(foreign(plml)).
:- nb_getval(plml_env_lang,LL),
   nb_delete(plml_env_lang),
   (LL=just(Lang) -> setenv('LANG',Lang); unsetenv('LANG')).

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
%    * stderr(S:{share|redirect(path)})
%      Determines what happens to Matlab's standard error output. If =|share|=,
%      then it is merged with SWI Prolog's standard error. If =|redirect(Path)|=,
%      it is sent to the named file. Default is =|share|=.
%    * path(Dirs:list(filespec))
%      Another mechanism for determining the Matlab path. Items are expanded
%      using absolute_file_name/2. Default is [].

ml_open(Id) :- ml_open(Id,localhost,[]).
ml_open(Id,Host) :- ml_open(Id,Host,[]).
ml_open(Id,Host,Options) :-
	ground(Id),
   pack_dir(PackDir),
	option(cmd(Bin),Options,matlab),
	option(stderr(StdErr),Options,share),
	option(awt(AWT),Options,false), must_be(boolean,AWT),
   option(path(Path),Options,[]),  must_be(list,Path),
   once(seqmap(build,[flags,awt(AWT),host(Host),stderr(StdErr),debug(PackDir,Options),exec],Bin,Exec)),

	debug(plml,'About to start Matlab with: ~q',[Exec]),
	setup_call_catcher_cleanup(
      mlOPEN(Exec,Id),
      ml_init(Id,PackDir,Path),
      exception(_),
      mlCLOSE(Id)
   ),
   assert(current_engine(Id)),
   expand_file_name('~/var/matbase',[DBROOT]),
   debug(plml,'Setting MATBASE root to ~q.',[DBROOT]),
   nofail(ml_exec(Id,dbroot(q(DBROOT)))),

	(	member(noinit,Options) -> true
	;	forall( matlab_path(_,Dir), maplist(nofail(addpath(Id)),Dir)),
		forall( matlab_init(_,Exec), nofail(Exec))
	).

% this is the part of the initialisation that is not allowed to fail
ml_init(Id,PackDir,Path) :-
   directory_file_path(PackDir,matlab,MatlabDir),
   (  getenv('LANG',Lang) -> true
   ;  Lang='UTF-8', print_message(warning,no_lang(Lang))
   ),
   debug(plml,'Setting LANG to ~w and character set to UTF-8.',[Lang]),
   ml_exec(Id,hide(feature(`'DefaultCharacterSet',`'UTF-8'))),
   ml_exec(Id,hide(setenv(`'LANG',`Lang))),
   debug(plml,'Adding ~q to Matlab path.',[MatlabDir]),
   ml_exec(Id,addpath(`MatlabDir)),
   forall( member(Spec,Path),
           (  absolute_file_name(Spec,Dir,[file_type(directory)]),
              ml_exec(Id,addpath(q(Dir))))).

%% build(+Spec,+Cmd1:string,-Cmd2:string) is det.
%  This predicate is responsible for building the command to run Matlab.
build(flags(Flags)) --> wappend([Flags]).
build(host(localhost)) --> !.
build(host(Host)) --> {hostname(Host)}, !.
build(host(Host)) --> prepend([ssh,Host|T],T).
build(stderr(share)) --> [].
build(stderr(discard)) --> build(stderr(redirect('/dev/null'))).
build(stderr(redirect(Dest)),Cmd1,Cmd2) :-
   format(string(Cmd2),'sh -c "exec ~w 2>~w"',[Cmd1,Dest]).
build(debug(PackDir,Options)) -->
   {  member(debug(In,Out),Options), !,
      debug(plml,'Running Matlab with protocol logging.',[]),
      debug(plml,'| Prolog > Matlab logged to "~w"',[In]),
      debug(plml,'| Prolog < Matlab logged to "~w"',[Out]),
      absolute_file_name(PackDir/scripts/logio,Spy,[access(execute)])
   },
   prepend([Spy,In,Out|T],T).
build(debug(_,_)) --> [].
build(exec) --> prepend([exec|T],T).
build(flags) --> wappend(['-nodesktop','-nosplash']).
build(awt(false)) --> wappend(['-noawt']).
build(awt(true)) --> [].

prepend(Words,[S1],S1,S2) :- concat(Words,S2).
wappend(Words,S1,S2) :- concat([S1|Words],S2).
concat(Words,String) :- atomics_to_string(Words,' ',String).


pack_dir(PackDir) :-
   module_property(plml,file(ThisFile)),
   file_directory_name(ThisFile,PrologDir),
   file_directory_name(PrologDir,PackDir).

addpath(Id,local(D)) :- !, ml_exec(Id,padl(q(D))).
addpath(Id,D) :- !, ml_exec(Id,padd(q(D))).

%% ml_close(+Id:ml_eng) is det.
%  Close Matlab session associated with Id.
ml_close(Id) :- ground(Id), mlCLOSE(Id), retract(current_engine(Id)).

nofail(P) :- catch(ignore(call(P)), E, print_message(warning,E)).
nofail(P,X) :- catch(ignore(call(P,X)), E, print_message(warning,E)).


%% ml_exec(+Id:ml_eng, +Expr:ml_expr) is det.
%
%  Execute Matlab expression without returning any values.
ml_exec(Id,X)  :-
	debug(plml,'plml:ml_exec term ~W',[X,[quoted(true),max_depth(10)]]),
	term_mlstring(Id,X,C), !,
	debug(plml(commands),'plml:ml_exec>> ~s',[C]),
	mlEXEC(Id,C).

%% ml_eval(+Id:ml_eng, +Expr:ml_expr, +Types:list(type), -Res:list(ml_val)) is det.
%
%  Evaluate Matlab expression binding return values to results list Res. This new
%  form uses an explicit output types list, so Res can be completely unbound on entry
%  even when multiple values are required.
ml_eval(Id,X,Types,Vals) :-
	maplist(alloc_ws(Id),Types,Vars),
	ml_exec(Id,hide(wsx(Vars)=X)),
	maplist(convert_ws,Types,Vars,Vals).

alloc_ws(I,_,Z) :- mlWSALLOC(I,Z).

%% ml_test(+Id:ml_eng, +X:ml_expr(bool)) is semidet.
%  Succeeds if X evaluates to true in Matlab session Id.
ml_test(Id,X)   :- ml_eval(Id,X,[bool],[1]).

ml_ws_name(X,Y,Z) :- mlWSNAME(X,Y,Z).


%% ===(Y:ml_vals(A), X:ml_expr(A)) is det.
%  Evaluate Matlab expression X as in ml_eval/4, binding one or more return values
%  to Y. If Y is unbound or a single ml_val(_), only the first return value is bound.
%  If Y is a list, multiple return values are processed.
Y === X :-
   current_engine(Id),
	(	is_list(Y)
	-> maplist(leftval,Y,TX,VX), ml_eval(Id,X,TX,VX)
	;	leftval(Y,T,V), ml_eval(Id,X,[T],[V])
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
leftval( arr(X),    array(_,_), X).
leftval( array(X::[Size->Type]),  array(Type,Size), X) :- !.
leftval( array(X::[Size]),        array(float,Size), X) :- !.
leftval( cell(X::[Size->Type]),   cell(Type,Size),  X) :- !.
leftval( cell(X::[Size]),         cell(mx,Size),  X) :- !.
leftval( Val:Type,  Type,  Val).


%% ??(X:ml_expr(_)) is det.
%  Execute Matlab expression X as with ml_exec/2, without returning any values.
%  Uses current (last opened) engine
?? X    :- current_engine(Id), ml_exec(Id,X).

%% ???(X:ml_expr(bool)) is semidet.
%  Evaluate Matlab boolean expression X as with ml_test/2.
%  Uses current (last opened) engine
??? Q   :- current_engine(Id), ml_test(Id,Q).


/*
 * DCG for term to matlab conversion
 *  the big problem with Matlab syntax is that you cannot always replace
 *  a name representing a value with an expression that reduces to that
 *  value. Eg
 *     X=magic(5), X(3,4)
 *  is ok, but
 *     (magic(5))(3,4)
 *  is not. Similarly x=@sin, x(0.5)  but not (@sin)(0.5)
 *  This is really infuriating.
 */


% top level statement rules
stmt(I,hide(A))    --> !, stmt(I,A), ";".
stmt(I,(A;B))      --> !, stmt(I,A), ";", stmt(I,B).
stmt(I,(A,B))      --> !, stmt(I,A), ",", stmt(I,B).
stmt(I,A=B)        --> !, ml_expr(I,A), "=", ml_expr(I,B).
stmt(I,if(A,B))    --> !, "if ",ml_expr(I,A), ", ", stmt(I,B), ", end".
stmt(I,if(A,B,C))  --> !, "if ",ml_expr(I,A), ", ", stmt(I,B), ", else ", stmt(I,C), ", end".
stmt(I,Expr)       --> !, ml_expr(I,Expr).


%% ml_expr(+Id:ml_eng,+X:ml_expr(A))// is nondet.
%  Convert Matlab expression as a Prolog term to string representation.
ml_expr(_,\X)         --> !, phrase(X).
ml_expr(I,$X)         --> !, {pl2ml_hook(X,Y)}, ml_expr(I,Y).
ml_expr(I,q(X))       --> !, ({string(X)} -> q(str(X)); q(stmt(I,X))).
ml_expr(_,tq(X))      --> !, q(pl2tex(X)).
ml_expr(_,atom(X))    --> !, atm(X).
ml_expr(_,term(X))    --> !, wr(X). % this could be dangerous
ml_expr(_,mat(X,Y))   --> !, "dbload(", loc(X,Y), ")".
ml_expr(_,loc(L))     --> !, { L=mat(X,Y) }, loc(X,Y).
ml_expr(I,mx(X))      --> !, { mlWSALLOC(I,Z), mlWSPUT(Z,X) }, ml_expr(I,ws(Z)).
ml_expr(I,ws(A))      --> !, { mlWSNAME(A,N,I) }, atm(N).
ml_expr(I,wsx([A|B])) --> !, { mlWSNAME(A,N,I) }, "[", atm(N), wsx(B), "]".
ml_expr(I,wsseq(A))   --> !, { mlWSNAME(A,N,I) }, atm(N).
ml_expr(_,noeval(_))  --> !, {fail}. % causes evaluation to fail.

ml_expr(_,'Infinity') --> !, "inf".
ml_expr(_,'Nan') --> !, "nan".

ml_expr(I,A+B) --> !, "plus", args(I,A,B).
ml_expr(I,A-B) --> !, "minus", args(I,A,B).
ml_expr(I, -B) --> !, "uminus", args(I,B).
ml_expr(I, +B) --> !, "uplus", args(I,B).
ml_expr(I,A^B) --> !, "mpower", args(I,A,B).
ml_expr(I,A*B) --> !, "mtimes", args(I,A,B).
ml_expr(I,A/B) --> !, "mrdivide", args(I,A,B).
ml_expr(I,A\B) --> !, "mldivide", args(I,A,B).
ml_expr(I,A.^B)--> !, "power", args(I,A,B).
ml_expr(I,A.*B)--> !, "times", args(I,A,B).
ml_expr(I,A./B)--> !, "rdivide", args(I,A,B).
ml_expr(I,A.\B)--> !, "ldivide", args(I,A,B).
ml_expr(I,A>B) --> !, "gt",args(I,A,B).
ml_expr(I,A<B) --> !, "lt",args(I,A,B).
ml_expr(I,A>=B)--> !, "ge",args(I,A,B).
ml_expr(I,A=<B)--> !, "le",args(I,A,B).
ml_expr(I,A==B)--> !, "eq",args(I,A,B).
ml_expr(I,A:B) --> !, range(I,A,B).

ml_expr(_,[])     --> !, "[]".
ml_expr(_,{})     --> !, "{}".
ml_expr(I,[X])    --> !, "[", matrix(v,I,X), "]".
ml_expr(I,[X|XX]) --> !, "[", ml_expr(I,X), seqmap(do_then_call(",",ml_expr(I)),XX), "]".
ml_expr(I,{X})    --> !, "{", matrix(_,I,X), "}".

ml_expr(I, `B) --> !, q(stmt(I,B)).
ml_expr(I,A#B) --> !, "getfield", args(I,A,q(B)).
ml_expr(I,B``) --> !, "ctranspose", args(I,B).
ml_expr(_,@B)  --> !, "@", atm(B).
ml_expr(I, \\B)  --> !, "@()", ml_expr(I,B).
ml_expr(I, A\\B) --> !, { term_variables(A,V), varnames(V) },
	"@(", varlist(A), ")", ml_expr(I,B).
ml_expr(I,lambda(A,B)) --> !, ml_expr(I,A\\B).
ml_expr(I,thunk(B))    --> !, ml_expr(I, \\B).


% !! This is problematic: we are using apply to represent both
% function application and array dereferencing. For function
% calls, A must be a function name atom or a function handle
% If A is an array, it cannot be an expression, unless we
% switch to using the paren Matlab function, which will be slower.
ml_expr(I,apply(A,B)) --> !, ml_expr(I,A), arglist(I,B).
ml_expr(I,cref(A,B))  --> !, ml_expr(I,A), "{", clist(I,B), "}".

% array syntax
ml_expr(I,arr($X))    --> !, { pl2ml_hook(X,L) }, ml_expr(I,arr(L)).
ml_expr(I,arr(L))     --> !, { array_dims(L,D) }, array(D,I,L).
ml_expr(I,arr(D,L))   --> !, array(D,I,L).
ml_expr(I,arr(D,L,P)) --> !, array(D,I,P,L).
ml_expr(I,atvector(L))--> !, "[", clist_at(I,L), "]".
ml_expr(I,vector(L))  --> !, "[", clist(I,L), "]".
ml_expr(I,cell(L))    --> !, "{", clist(I,L), "}".
ml_expr(_,'$VAR'(N))  --> !, "p_", atm(N).

% catch these and throw exception
ml_expr(_,hide(A))    --> {throw(ml_illegal_expression(hide(A)))}.
ml_expr(_,(A;B))      --> {throw(ml_illegal_expression((A;B)))}.
ml_expr(_,(A,B))      --> {throw(ml_illegal_expression((A,B)))}.
ml_expr(_,A=B)        --> {throw(ml_illegal_expression(A=B))}.

% these are the catch-all clauses which will deal with matlab names, and literals
% should we filter on the head functor?
ml_expr(_,A) --> {string(A)}, !, q(str(A)).
ml_expr(_,A) --> {atomic(A)}, !, atm(A).
ml_expr(I,F) --> {F=..[H|AX]}, atm(H), arglist(I,AX).

ml_expr_with(I,Lambda,Y) --> {copy_term(Lambda,Y\\PY)}, ml_expr(I,PY).


% take output of DCG phrase P and generate properly escaped Matlab single quoted string.
q(P) --> {phrase(P,Codes)}, "'", esc(ml_quote,Codes), "'".

ml_quote([0''|T],T) --> !, "''".
ml_quote([0'\n|T],T) --> !, "\\n".
ml_quote([C|T],T) --> [C].

% dimensions implicit in nested list representation
array_dims([X|_],M) :- !, array_dims(X,N), succ(N,M).
array_dims(_,0).

% efficiently output row vector of workspace variable names
wsx([]) --> [].
wsx([A|AX]) --> { mlWSNAME(A,N,_) }, ",", atm(N), wsx(AX).

%% array(+Dims:natural, +Id:ml_eng, +Array)// is det.
%
%  Format nested lists as Matlab multidimensional array.
%  Dims is the number of dimensions of the resulting array and
%  should equal the nesting level of Array, ie if Array=[1,2,3],
%  Dims=1; if Array=[[1,2],[3,4]], Dims=2, etc.
array(0,I,X) --> !, ml_expr(I,X).
array(1,I,L) --> !, "[", seqmap_with_sep(";",ml_expr(I),L), "]".
array(2,I,L) --> !, "[", seqmap_with_sep(",",array(1,I),L), "]".
array(N,I,L) --> {succ(M,N)}, "cat(", atm(N), ",", seqmap_with_sep(",",array(M,I),L), ")".

array(0,I,P,X) --> !, ml_expr_with(I,P,X).
array(1,I,P,L) --> !, "[", seqmap_with_sep(";",ml_expr_with(I,P),L), "]".
array(2,I,P,L) --> !, "[", seqmap_with_sep(",",array(1,I,P),L), "]".
array(N,I,P,L) --> {succ(M,N)}, "cat(", atm(N), ",", seqmap_with_sep(",",array(M,I,P),L), ")".

matrix(h,I,(A,B)) --> !, ml_expr(I,A), ",", matrix(h,I,B).
matrix(v,I,(A;B)) --> !, ml_expr(I,A), ";", matrix(v,I,B).
matrix(_,I,A) --> !, ml_expr(I,A).


% colon syntax for ranges
range(I,A,B:C) --> !, "colon", arglist(I,[A,B,C]).
range(I,A,B)   --> !, "colon", args(I,A,B).


%% varlist(+Term)// is det.
%  Format comma separated list of lambda expression arguments.
varlist((A,B)) --> !, atm(A), ",", varlist(B).
varlist(A) --> !, atm(A).


%% clist(+Id:ml_eng, +Items:list(ml_expr))// is det.
%  Format list of Matlab expressions in a comma separated list.
clist(_,[]) --> [].
clist(I,[L1|LX])  --> ml_expr(I,L1), seqmap(do_then_call(",",ml_expr(I)),LX).


%% clist_at(+Id:ml_eng, +Items:list(ml_expr))// is det.
%  Format list of atoms in a comma separated list.
clist_at(_,[]) --> [].
clist_at(_,[L1|LX])  --> atm(L1), seqmap(do_then_call(",",atm),LX).


%% arglist(+Id:ml_eng, +Args:list(ml_expr))// is det.
%  DCG rule to format a list of Matlab expressions as function arguments
%  including parentheses.
arglist(I,X) --> "(", clist(I,X), ")".


%% args(+Id:ml_eng, +A1:ml_expr, +A2:ml_expr)// is det.
%% args(+Id:ml_eng, +A1:ml_expr)// is det.
%
%  DCG rule to format one or two Matlab expressions as function arguments
%  including parentheses.
args(I,X,Y) --> "(", ml_expr(I,X), ",", ml_expr(I,Y), ")".
args(I,X) --> "(", ml_expr(I,X), ")".


%% atm(+A:atom)// is det.
%  DCG rule to format an atom using write/1.
atm(A,C,T) :- format(codes(C,T),'~w',[A]).

varnames(L) :- varnames(1,L).
varnames(_,[]).
varnames(N,[TN|Rest]) :-
	atom_concat(p_,N,TN), succ(N,M),
	varnames(M,Rest).


%% term_mlstring(+Id:ml_eng,+X:ml_expr,-Y:list(code)) is det.
%  Convert term representing Matlab expression to a list of character codes.
term_mlstring(I,Term,String) :- phrase(stmt(I,Term),String), !.

%% term_texatom(+X:tex_expr,-Y:atom) is det.
%  Convert term representing TeX expression to a string in atom form.
term_texatom(Term,Atom) :- phrase(pl2tex(Term),String), !, atom_codes(Atom,String).



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
% compatible(logical,float).
% compatible(logical,int).
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


%% loc(Dir,File)// is det.
%  DCG rule for matbase locator strings. Dir must be an atom slash-separated
%  list of atoms representing a path relative to the matbase root (see Matlab
%  function dbroot). File must be an atom. Outputs a single-quoted locator
%  string acceptable to Matlab db functions.
loc(X,Y) --> "'", wr(X),"|",atm(Y), "'".


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


/* -----------------------------------------------------------------------
 * From here on, we have straight Matlab utilities
 * rather than basic infrastructure.
 */


% for dealing with option lists

%% mhelp(+Name:atom) is det.
%  Lookup Matlab help on the given name. Equivalent to executing help(`X).
%  If using a colour terminal, output is written in blue.
mhelp(X) :- current_engine(Id), ansi_format([fg(blue)],'~@',[plml:ml_exec(Id,help(q(X)))]).


%% compileoptions(+Opts:list(ml_options), -Prefs:ml_expr(options)) is det.
%
%  Convert list of option specifiers into a Matlab expression representing
%  options (ie a struct). Each specifier can be a Name:Value pair, a name
%  to be looked up in the optionset/2 predicate, a nested list of ml_options
% compileoptions :: list (optionset | atom:value | struct) -> struct.
%  NB. option types are as follows:
%  ==
%  X :: ml_options :- optionset(X,_).
%  X :: ml_options :- X :: ml_option(_).
%  X :: ml_options :- X :: list(ml_options).
%  X :: ml_options :- X :: ml_expr(struct(_)).
%
%  ml_option(A) ---> atom:ml_expr(A).
%  ==
compileoptions(Opts,Prefs) :-
	rec_optslist(Opts,OptsList),
	Prefs=..[prefs|OptsList].

rec_optslist([],[]).
rec_optslist([H|T],L) :-
	( % mutually exclusive types for H
		optionset(H,Opts1) -> rec_optslist(Opts1,Opts)
	;  H=Name:Value       -> Opts=[`Name,Value]
	;	is_list(H)         -> rec_optslist(H,Opts)
	; /* assume struct */    Opts=[H]
	),
	rec_optslist(T,TT),
	append(Opts,TT,L).

rtimes(X,Y,Z) :-
	( var(X) -> X is Z/Y
	; var(Y) -> Y is Z/X
	;           Z is X*Y).


% Execute several plots as subplots. The layout can be
% vertical, horizontal, or explicity given as Rows*Columns.


% mplot is a private procedure used by multiplot
mplot(subplot(H,W),N,Plot,Ax) :- ?? (subplot(H,W,N); Plot), Ax===gca.
mplot(figure,N,Plot,Ax) :- ?? (figure(N); Plot), Ax===gca.

%% multiplot(+Type:ml_plot, +Cmds:list(ml_expr(_))) is det.
%% multiplot(+Type:ml_plot, +Cmds:list(ml_expr(_)), -Axes:list(ml_val(handle))) is det.
%
%  Executes plotting commands in Cmds in multiple figures or axes as determined
%  by Type. Valid types are:
%    * figs(Range)
%      Executes each plot in a separate figure, Range must be P..Q where P
%      and Q are figure numbers.
%    * vertical
%      Executes each plot in a subplot;
%      subplots are arranged vertically top to bottom in the current figure.
%    * horizontal
%      Executes each plot in a subplot;
%      subplots are arranged horizontally left to right in the current figure.
%    * [Type, link(Axis)]
%      As for multplot type Type, but link X or Y axis scales as determined by Axis,
%      which can be `x, `y, or `xy.
%
%  Three argument form returns a list containing the Matlab handles to axes objects,
%  one for each plot.
multiplot(Type,Plots) :- multiplot(Type,Plots,_).

multiplot([Layout|Opts],Plots,Axes) :- !,
	multiplot(Layout,Plots,Axes),
	member(link(A),Opts) ->
		?? (linkaxes(Axes,`off); hide(linkaxes(Axes,`A)))
	;	true.

multiplot(figs(P..Q),Plots,Axes) :- !,
	length(Plots,N),
	between(1,inf,P), Q is P+N-1,
	numlist(P,Q,PlotNums),
	maplist(mplot(figure),PlotNums,Plots,Axes).

multiplot(Layout,Plots,Axes) :-
	length(Plots,N),
	member(Layout:H*W,[vertical:N*1, horizontal:1*N, H*W:H*W]),
	rtimes(H,W,N), % bind any remaining variables
	numlist(1,N,PlotNums),
	maplist(mplot(subplot(H,W)),PlotNums,Plots,Axes).


%% optionset( +Key:term,  -Opts:list(ml_options)) is semidet.
%
%  Extensible predicate for mapping arbitrary terms to a list of options
%  to be processed by compileoptions/2.

%user:portray(Z) :- mlWSNAME(Z,N,ID), format('<~w:~w>',[ID,N]).

prolog:message(no_lang(Lang)) --> ['Environment variable LANG not set -- using ~w'-[Lang]].
prolog:message(plml_unknown_engine(Id)) --> ['Matlab engine (~w) does not exist'-[Id]].
prolog:message(ml_illegal_expression(Expr)) --> ['Illegal Matlab expression: ~w'-[Expr]].
prolog:message(ml_syntax_error) --> ['Unknown Matlab syntax error'].
prolog:message(ml_interrupted) --> ['Matlab computation was interrupted'].
prolog:message(error(mleng_error(Function,Arg),_)) -->
   ['Matlab engine API function ~w failed on "~s"',[Function,Arg]].
prolog:message(error(plml_error(Got,Expected),_)) -->
   ['plml protocol error: got ~q, expected ~q'-[Got,Expected]].
prolog:message(error(ml_error(Msg,Cmd),_)) -->
   {shorten(Cmd, ShortCmd)},
   ['Matlab error:'-[], nl, '>> ~w'-[Msg], nl],
   ['while executing "~s"'-[ShortCmd]].

shorten(S1, S2) :-
   length(S1, L1),
   (  L1 =< 1000 -> S2=S1
   ;  length(Pre,300), append(Pre,Rest,S1),
      length(Suff,300), append(_,Suff,Rest),
      append("  ...  ", Suff, Inter),
      append(Pre, Inter, S2)
   ).


%% pl2tex(+Exp:tex_expr)// is det.
%
% DCG for texifying expressions (useful for matlab text)
pl2tex(A=B)  --> !, pl2tex(A), "=", pl2tex(B).
pl2tex(A+B)  --> !, pl2tex(A), "+", pl2tex(B).
pl2tex(A-B)  --> !, pl2tex(A), "-", pl2tex(B).
pl2tex(A*B)  --> !, pl2tex(A), "*", pl2tex(B).
pl2tex(A.*B) --> !, pl2tex(A), "*", pl2tex(B).
pl2tex(A/B)  --> !, pl2tex(A), "/", pl2tex(B).
pl2tex(A./B) --> !, pl2tex(A), "/", pl2tex(B).
pl2tex(A\B)  --> !, pl2tex(A), "\\", pl2tex(B).
pl2tex(A.\B) --> !, pl2tex(A), "\\", pl2tex(B).
pl2tex(A^B)  --> !, pl2tex(A), "^", brace(pl2tex(B)).
pl2tex(A.^B) --> !, pl2tex(A), "^", brace(pl2tex(B)).
pl2tex((A,B))--> !, pl2tex(A), ", ", pl2tex(B).
pl2tex(A;B)--> !, pl2tex(A), "; ", pl2tex(B).
pl2tex(A:B)--> !, pl2tex(A), ": ", pl2tex(B).
pl2tex({A})  --> !, "\\{", pl2tex(A), "\\}".
pl2tex([])   --> !, "[]".
pl2tex([X|XS])  --> !, "[", seqmap_with_sep(", ",pl2tex,[X|XS]), "]".

pl2tex(A\\B) --> !, "\\lambda ", pl2tex(A), ".", pl2tex(B).
pl2tex(@A)   --> !, "@", pl2tex(A).
pl2tex(abs(A)) --> !, "|", pl2tex(A), "|".
pl2tex(A)    --> {atomic(A)}, escape_with(0'\\,0'_,at(A)).
pl2tex(A) -->
	{compound(A), A=..[H|T] },
	pl2tex(H), paren(seqmap_with_sep(", ",pl2tex,T)).

hostname(H) :-
   (  getenv('HOSTNAME',H) -> true
   ;  read_line_from_pipe(hostname,H)
   ).

