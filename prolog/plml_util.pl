:- module(plml_utils, 
	[	compileoptions/2
	,	multiplot/2
	,  mhelp/1
	,	op(550,xfx,..)		% range of integers
	]).
	

:- multifile user:optionset/2.


/** <module> Prolog-Matlab utilities 

*/
	  
:- use_module(library(plml_core)).
:- use_module(library(plml_dcg)).

:- set_prolog_flag(back_quotes,symbol_char).
:- set_prolog_flag(double_quotes,codes).


% for dealing with option lists

%% mhelp(+Name:atom) is det.
%  Lookup Matlab help on the given name. Equivalent to executing help(`X).
mhelp(X) :- ml_exec(ml,help(q(X))).



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

%user:portray(A|B) :- print(A), write('|'), print(B).
%user:portray(Z) :- mlWSNAME(Z,N,ID), format('<~w:~w>',[ID,N]).
