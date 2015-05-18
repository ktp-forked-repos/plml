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

:- module(plml_dcg, 
	[	term_mlstring/3   % (+Id, +Expr, -String)  ~Prolog term to Matlab string
	,	term_texatom/2		% (+Expr, -Atom)         ~Prolog term to TeX expression

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
	,	op(100,yfx,#)     % field indexing (note left-associativity)
	,	op(750,fy,\\)		% thunk abdstraction
	,  op(750,xfy,\\)    % lambda abdstraction
	]).
	

:- multifile user:pl2ml_hook/2, pl2ml_hook/3.

   

/** <module> Matlab DCG 

	---++++ Types

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
	qq(X)           % wrap V[X] in double quotes (escaping internal double quotes)
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
	  
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(back_quotes,symbol_char).
:- set_prolog_flag(double_quotes,codes).

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


%% pl2ml_hook(+I:engine,+X:term,-Y:ml_expr) is nondet.
%% pl2ml_hook(+X:term,-Y:ml_expr) is nondet.
%  Clauses of pl2ml_hook/2 allow for extensions to the Matlab expression 
%  language such that =|V[$X] = V[Y]|= if =|pl2ml_hook(X,Y)|=.
pl2ml_hook(_,X,Y) :- pl2ml_hook(X,Y).

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
stmt(I,hide(A))    --> !, stmt(I,(A;nop)).
stmt(I,(A;B))      --> !, stmt(I,A), ";", stmt(I,B).
stmt(I,(A,B))      --> !, stmt(I,A), ",", stmt(I,B).
stmt(I,A=B)        --> !, ml_expr(I,A), "=", ml_expr(I,B).
stmt(I,if(A,B))    --> !, "if ",ml_expr(I,A), ", ", stmt(I,B), ", end".
stmt(I,if(A,B,C))  --> !, "if ",ml_expr(I,A), ", ", stmt(I,B), ", else ", stmt(I,C), ", end".
stmt(I,Expr)       --> !, ml_expr(I,Expr).


%% ml_expr(+Id:ml_eng,+X:ml_expr(A))// is nondet.
%  Convert Matlab expression as a Prolog term to string representation.
ml_expr(_,\X)         --> !, phrase(X).
ml_expr(I,$X)         --> !, {pl2ml_hook(I,X,Y)}, ml_expr(I,Y).
ml_expr(I,q(X))       --> !, q(stmt(I,X)).
ml_expr(I,qq(X))      --> !, qq(stmt(I,X)).
ml_expr(_,tq(X))      --> !, q(pl2tex(X)).
ml_expr(_,atom(X))    --> !, atm(X).
ml_expr(_,term(X))    --> !, wr(X). % this could be dangerous
ml_expr(_,mat(X,Y))   --> !, "dbload(", loc(X,Y), ")".
ml_expr(_,loc(L))     --> !, { L=mat(X,Y) }, loc(X,Y).
ml_expr(I,mx(X))      --> !, ml_expr(I,$mx(X)). % punt these out to the pl2ml_hook, to be picked up plml_core
ml_expr(I,ws(A))      --> !, ml_expr(I,$ws(A)). % ditto
ml_expr(I,wsseq(A))   --> !, ml_expr(I,$ws(A)).
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
ml_expr(_,atom_list(L)) --> !, "[", seqmap_with_sep(",",atm,L), "]".

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
ml_expr(I,arr($X))    --> !, { pl2ml_hook(I,X,L) }, ml_expr(I,arr(L)).
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
	

% dimensions implicit in nested list representation
array_dims([X|_],M) :- !, array_dims(X,N), succ(N,M).
array_dims(_,0).


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

