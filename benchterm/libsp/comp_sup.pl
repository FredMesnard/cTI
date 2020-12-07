/* Copyright (C) 1991 Swedish Institute of Computer Science. */

%% Predicates used by the compiler *and* elsewhere.

dlist([]) --> [].
dlist([X|Y]) --> [X], dlist(Y).

% Impure membership.
contains_ro([], _) :- !, fail.
contains_ro([X|_], X).
contains_ro([_|Xs], X) :- contains_ro(Xs, X).

% Used in plwam.pl, walql.pl, gauge.pl

get_key(N/Ar, Key) :- get_key(N, Ar, Key).

get_key(N/Ar/_-_, _, Key) :- !, get_key(N, Ar, Key).
get_key(N, Ar, Key) :- functor(Key, N, Ar).

%-----------------------------------------------------------------------------

block_convert(Decl, Term) :-
	var(Term), !,
	functor(Decl, F, A),
	functor(Term, F, A),
	block_convert(A, Decl, Term).
block_convert(Decl, Term) :-
	functor(Term, F, A),
	functor(Decl, F, A),
	block_convert(A, Decl, Term).

block_convert(0, _, _) :- !.
block_convert(N, T, S) :- 
	arg(N, T, -),
	arg(N, S, 0), !,
	M is N-1,
	block_convert(M, T, S).
block_convert(N, T, S) :- 
	arg(N, T, ?),
	arg(N, S, 1),
	M is N-1,
	block_convert(M, T, S).

meta_convert(F0, F) :-
	functor(F0, N, A),
	functor(F, N, A),
	meta_convert(0, A, F0, F).

meta_convert(A, A, _, _) :- !.
meta_convert(I, A, F0, F) :-
	J is I+1,
	arg(J, F0, X),
	arg(J, F, Y),
	(   X = : -> Y = :
	;   number(X) -> Y = :
	;   Y = ?
	),
	meta_convert(J, A, F0, F).

%%----------------------------------------------------------------------
%% Moved from Library/*.pl to support meta expansion also in the
%% bootstrap compiler

% meta_pred/4
% Gets the recorded meta-declaration for Head, if there is one.

meta_pred(fcompile(_), Module, Head, Decl) :-
	functor(Head, N, A),
	functor(Decl, N, A),
	clause_internal('$predicate'(Head,Module,N/A,Props), _),
	contains_ro(Props, (meta_predicate Decl)).
meta_pred(_, Module, Head, Decl) :-
	'$meta_declaration'(Head, Module, Decl).


%---------------------------------------------------------------------------
% goal_exp/5
% Performs meta expansion on Goal

goal_exp(Goal, Goal1, HV, Module, Mode) :-
	compound(Goal),
	meta_pred(Mode, Module, Goal, Decl), !,
	functor(Decl, Name, Arity),
	functor(Goal1, Name, Arity),
	exp_args(0, Arity, Goal, Goal1, Decl, Module, HV).
goal_exp(Goal, Goal, _, _, _).


%---------------------------------------------------------------------------
% exp_vars/4.
% Collects in HeadVars the variables of Head which are subjects
% to meta-expansion

exp_vars(Head, HeadVars, Module, Mode) :-
	meta_pred(Mode, Module, Head, Decl), !,
	functor(Head, _, Arity),
	exp_vars(0, Arity, Decl, Head, HeadVars).
exp_vars(_, [], _, _).

exp_vars(Arity, Arity, _, _, []) :- !.
exp_vars(N0, Arity, Decl, Head, [A|Vars]) :-
	N is N0+1,
	arg(N, Head, A),
	var(A),
	arg(N, Decl, :), !,
	exp_vars(N, Arity, Decl, Head, Vars).
exp_vars(N0, Arity, Decl, Head, Vars) :-
	N is N0+1,
	exp_vars(N, Arity, Decl, Head, Vars).


%---------------------------------------------------------------------------
% exp_args/7
% Unifies the args of Goal1 with the possible module name expanded
% args of Goal.

exp_args(Arity, Arity, _, _, _, _, _) :- !.
exp_args(N0, Arity, Goal, Goal1, Decl, M, HeadVars) :-
	N is N0+1,
	arg(N, Goal, A),
	arg(N, Goal1, A1),
	(   arg(N, Decl, :),
	    permit_exp(A, HeadVars) ->
	    A1 = M:A
	;   A1 = A
	),
	exp_args(N, Arity, Goal, Goal1, Decl, M, HeadVars).

permit_exp(Arg, _) :- nonvar(Arg), \+Arg=_:_.
permit_exp(Arg, HeadVars) :- var(Arg), free_of_var(HeadVars, Arg).

free_of_var([], _).
free_of_var([X1|Xs], X) :- X\==X1, free_of_var(Xs, X).


/* Pre-ISO
%---------------------------------------------------------------------------
% wellformed_body/10
% Parses the body of a clause, performs meta expansion.
% Massages the Layout information according to transformations performed.
% Also deletes layout information below the call level (cf condense_layout/2).

wellformed_body(Body, L0, Env, Body1, L, HV, Module, Context, Mode, Goal) :-
	var(Body), !,
        illformed_body(Body, L0, Env, Body1, L, HV, Module, Context, Mode, Goal).
wellformed_body(!, L0, +, Body, L, _, _, _, _, _) :- !,
	Body = !,
	L = L0.
wellformed_body(!, _, -, _, _, _, _, _, _, Goal) :- !,
	illarg(context(if,cut), Goal, 0).
wellformed_body(true, L0, _, Body, L, _, _, _, _, _) :- !,
	Body = true,
	L = L0.
wellformed_body(Body, L0, _, Body1, L, _, _, _, _, _) :-
	Body = '$object call'(_,_), !,
	Body1 = Body,
	condense_layout(L0, L).
wellformed_body((A,B), L0, Env, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = (A1,B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, Env, A1, L1, HV, Module, Context, Mode, Goal),
	wellformed_body(B, L02, Env, B1, L2, HV, Module, Context, Mode, Goal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((A->B), L0, Env, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = (A1->B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, -, A1, L1, HV, Module, Context, Mode, Goal),
	wellformed_body(B, L02, Env, B1, L2, HV, Module, Context, Mode, Goal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((A;B), L0, Env, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = (A1;B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, Env, A1, L1, HV, Module, Context, Mode, Goal),
	wellformed_body(B, L02, Env, B1, L2, HV, Module, Context, Mode, Goal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((\+ A), L0, _, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = (\+ A1),
	arg1_layout(L0, L01),
        wellformed_body(A, L01, -, A1, L1, HV, Module, Context, Mode, Goal),
	comp_layout1(L0, L1, L).
wellformed_body(if(A,B,C), L0, Env, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = if(A1,B1,C1),
	decomp_layout3(L0, L01, L02, L03),
        wellformed_body(A, L01, -,   A1, L1, HV, Module, Context, Mode, Goal),
	wellformed_body(B, L02, Env, B1, L2, HV, Module, Context, Mode, Goal),
	wellformed_body(C, L03, Env, C1, L3, HV, Module, Context, Mode, Goal),
	comp_layout3(L0, L1, L2, L3, L).
wellformed_body(X^A, L0, Env, Body, L, HV, Module, Context, Mode, Goal) :- !,
	Body = X^A1,
	decomp_layout2(L0, _L01, L02),
%	condense_layout(_L01, L1),                 % ignore layout for X (almost)
        wellformed_body(A, L02, Env, A1, L2, HV, Module, Context, Mode, Goal),
	comp_layout2(L0, [], L2, L).
wellformed_body(M:A, L0, Env, A1, L, HV, Module, Context, Mode, Goal) :- !,
	(   nonvar(M)
	->  arg2_layout(L0, L02),
	    wellformed_body(A, L02, Env, A1, L, HV, M, Context, Mode, Goal)
	;   illformed_body(M:A, L0, Env, A1, L, HV, Module, Context, Mode, Goal)
	).
wellformed_body(Body, L0, Env, Body1, L, HV, Module, Context, Mode, Goal) :-
	callable(Body), !,
	condense_layout(L0, L),
        wellformed_body_expand(Body, Env, Body1, HV, Module, Context, Mode, Goal).
wellformed_body(Body, _, _, _, _, _, _, _, _, Goal) :-
	illarg(type(callable), Goal, 0, Body).

illformed_body(Body, L0, Env, Body1, L, HV, Module, Context, Mode, Goal) :-
	condense_layout(L0, L),
        wellformed_body_expand(call(Body), Env, Body1, HV, Module, Context, Mode, Goal).

wellformed_body_expand(Body0, Env, Body, HV, Module, Context, Mode, Goal) :-
	nonvar(Module), !,
	goal_exp(Body0, Body1, HV, Module, Mode), % meta expansion
	(   call_goal_expansion(Body1, Module, Body2) ->
	    wellformed_body(Body2, []\*undef layout*\, Env, Body, _, HV, Module, Context, Mode, Goal)
	;   Module=Context,			% safe for interpreted code
	    wf_source_module(Mode, Module) ->	% safe for compiled code
	    Body = Body1
	;   Body = Module:Body1
	).
wellformed_body_expand(Body, _, Module:Body, _, Module, _, _, _).
*/


%---------------------------------------------------------------------------
% wellformed_body/9
% Parses the body of a clause, performs meta expansion.
% Massages the Layout information according to transformations performed.
% Also deletes layout information below the call level (cf condense_layout/2).

wellformed_body(Goal, L0, Goal1, L, HV, Module, Context, Mode, ErrGoal) :-
	wellformed_body(Goal, L0, +, _Cut, Goal1, L, HV, Module, Context, Mode, ErrGoal).

wellformed_body(Goal, L0, Env, Cut, Goal1, L, HV, Module, Context, Mode, ErrGoal) :- var(Goal), !,
	condense_layout(L0, L),
        wellformed_body_expand(call(Goal), Env, Cut, Goal1, HV, Module, Context, Mode, ErrGoal).
wellformed_body(!, L, Env, Cut, !, L, _, _, _, _, ErrGoal) :- !,
	(   Env == + -> true
	;   iso_mode -> Cut = cut % ignore cut context errors in iso mode
	;   illarg(context(if,cut), ErrGoal, 0)
	).
wellformed_body(true, L0, _, _, Goal, L, _, _, _, _, _ErrGoal) :- !, Goal = true, L = L0.
wellformed_body(Goal, L0, _, _, Goal1, L, _, _, _, _, _ErrGoal) :-
	Goal = '$object call'(_,_),
	!,
	Goal1 = Goal,
	condense_layout(L0, L).
wellformed_body((A,B), L0, Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = (A1,B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, Env, Cut, A1, L1, HV, Module, Context, Mode, ErrGoal),
	wellformed_body(B, L02, Env, Cut, B1, L2, HV, Module, Context, Mode, ErrGoal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((A->B), L0, Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = (A1Packed->B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, - , Cut0, A1, L1, HV, Module, Context, Mode, ErrGoal),
	package_cut_condition(Cut0, Module:A, A1, A1Packed), Cut = Cut0,
	wellformed_body(B, L02, Env, Cut, B1, L2, HV, Module, Context, Mode, ErrGoal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((A;B), L0, Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = (A1;B1),
	decomp_layout2(L0, L01, L02),
        wellformed_body(A, L01, Env, Cut, A1, L1, HV, Module, Context, Mode, ErrGoal),
	wellformed_body(B, L02, Env, Cut, B1, L2, HV, Module, Context, Mode, ErrGoal),
	comp_layout2(L0, L1, L2, L).
wellformed_body((\+ A), L0, _Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = (\+ A1Packed),
	arg1_layout(L0, L01),
        wellformed_body(A, L01, -, Cut0, A1, L1, HV, Module, Context, Mode, ErrGoal),
	package_cut_condition(Cut0, Module:A, A1, A1Packed), Cut = Cut0,
	comp_layout1(L0, L1, L).
wellformed_body(once(A), L0, _Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = (once(A1Packed)),
	arg1_layout(L0, L01),
        wellformed_body(A, L01, -, Cut0, A1, L1, HV, Module, Context, Mode, ErrGoal),
	package_cut_condition(Cut0, Module:A, A1, A1Packed), Cut = Cut0,
	comp_layout1(L0, L1, L).
wellformed_body(if(A,B,C), L0, Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = if(A1Packed,B1,C1),
	decomp_layout3(L0, L01, L02, L03),
        wellformed_body(A, L01, -  , Cut0, A1, L1, HV, Module, Context, Mode, ErrGoal),
	package_cut_condition(Cut0, Module:A, A1, A1Packed), Cut = Cut0,
        wellformed_body(B, L02, Env, Cut, B1, L2, HV, Module, Context, Mode, ErrGoal),
        wellformed_body(C, L03, Env, Cut, C1, L3, HV, Module, Context, Mode, ErrGoal),
	comp_layout3(L0, L1, L2, L3, L).
wellformed_body(X^A, L0, Env, Cut, Goal, L, HV, Module, Context, Mode, ErrGoal) :- !,
	Goal = X^A1,
	decomp_layout2(L0, _L01, L02),
%	condense_layout(_L01, L1),                 % ignore layout for X (almost)
        wellformed_body(A, L02, Env, Cut, A1, L2, HV, Module, Context, Mode, ErrGoal),
	comp_layout2(L0, [], L2, L).
wellformed_body(M:A, L0, Env, Cut, A1, L, HV, Module, Context, Mode, ErrGoal) :- !,
	(   nonvar(M)
	->  arg2_layout(L0, L02),
	    wellformed_body(A, L02, Env, Cut, A1, L, HV, M, Context, Mode, ErrGoal)
	;   condense_layout(L0, L),
	    wellformed_body_expand(call(M:A), Env, Cut, A1, HV, Module, Context, Mode, ErrGoal)
	).
wellformed_body(Goal, L0, Env, Cut, Goal1, L, HV, Module, Context, Mode, ErrGoal) :-
	callable(Goal), !,
	condense_layout(L0, L),
        wellformed_body_expand(Goal, Env, Cut, Goal1, HV, Module, Context, Mode, ErrGoal).
wellformed_body(Goal, _, _, _, _, _, _, _, _, _, ErrGoal) :-
	illarg(type(callable), ErrGoal, 0, Goal).

wellformed_body_expand(Goal0, Env, Cut, Goal, HV, Module, Context, Mode, ErrGoal) :-
	nonvar(Module), !,
	goal_exp(Goal0, Goal1, HV, Module, Mode), % meta expansion
	(   call_goal_expansion(Goal1, Module, Goal2, HomeModule) ->
	    wellformed_body(Goal2, []/*undef layout*/, Env, Cut, Goal, _, HV, HomeModule, Context, Mode, ErrGoal)
	;   Module=Context,			% safe for interpreted code
	    wf_source_module(Mode, Module) ->	% safe for compiled code
	    Goal = Goal1
	;   Goal = Module:Goal1
	).
wellformed_body_expand(Goal, _, _, Module:Goal, _, Module, _, _, _ErrGoal).

wf_source_module(assert, _) :- !.
wf_source_module(_, Module) :- '$typein_module'(Module, Module).


% NOTE: Cross module references are resolved at compile time,
%       if not enclosed in a call/1.

package_cut_condition(Cut, Source, Expanded, Packed) :-
	(   Cut == cut -> Packed = call(Source)
	;   Packed = Expanded
	).

%---------------------------------------------------------------------------
% Support for layout handling


/* The predicates below accept a Layout or the constant [] (undefined) */ 


% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

% Return the layout of the first argument
arg1_layout([_,L0|_], L) :- !, L=L0.
arg1_layout(L, L).

% Return the layout of the second argument
arg2_layout([_,_,L0|_], L) :- !, L=L0.
arg2_layout(L, L).


/* The predicates below are for decomposing and composing layout,
   to aid in the transformation of layout.
   Predicates comp_layoutN/(N+2) receive the layout of the term
   before the transformation, the possibly transformed args, and
   return the new layout.
*/

% Decompose the layout of a one-argument term
decomp_layout1([_LN,A01], A1) :- !, A1=A01.
decomp_layout1(L, L).
   
% Build the layout of a single-argument term
comp_layout1([LN|_], A, Layout) :-
%	A \== [], 
	!, Layout = [LN,A].
comp_layout1(LN, A, Layout) :-
        integer(LN), 
	!, Layout = [LN,A].
comp_layout1(_, _, []).


% Decompose the layout of a two-argument term
decomp_layout2([_LN,L0,R0], L, R) :- !, L=L0, R=R0.
decomp_layout2(L, L, L).

% Build the layout of a two-argument term
comp_layout2([LN|_], L0, R0, Layout) :-
%	L0 \== [], R0 \== [], 
	!, Layout = [LN,L0,R0].
comp_layout2(LN, L0, R0, Layout) :-
        integer(LN), 
	!, Layout = [LN,L0,R0].
comp_layout2(_, _, _, []).


% Decompose the layout of a three-argument term
decomp_layout3([_LN,A01,A02,A03], A1, A2, A3) :- !, A1=A01, A2=A02, A3=A03.
decomp_layout3(L, L, L, L).

% Build the layout of a three-argument term
comp_layout3([LN|_], A01, A02, A03, Layout) :-
%	A01 \== [], A02 \== [], A03 \== [], 
        !, Layout = [LN,A01,A02,A03].
comp_layout3(LN, A01, A02, A03, Layout) :-
        integer(LN),
        !, Layout = [LN,A01,A02,A03].
comp_layout3(_, _, _, _, []).



%---------------------------------------------------------------------------
% Shared support for load contexts

create_load_context(Module, Hiding, File, In, Dir, Doing, ImportSet, StreamPos, IncdFile) :-
	asserta_internal('$load context'(Module,Hiding,File,In,Dir,Doing,
					 ImportSet,StreamPos,IncdFile),
			 _).


first_load_context(Head) :-
	most_general_load_context(Head),
	clause_internal(Head, _), !. % first only

first_load_context(Key, Value) :-
	most_general_load_context(Head),
	clause_internal(Head, _), !, % first only
	load_context(Key, Head, Value).

any_load_context(Key, Value) :-
	most_general_load_context(Head),
	clause_internal(Head, _), % any
	load_context(Key, Head, Value).

load_context(module,        '$load context'(X,_,_,_,_,_,_,_,_), X).
load_context(hidden,        '$load context'(_,X,_,_,_,_,_,_,_), X).
load_context(source,        '$load context'(_,_,X,_,_,_,_,_,_), X).
load_context(stream,        '$load context'(_,_,_,X,_,_,_,_,_), X).
load_context(directory,     '$load context'(_,_,_,_,X,_,_,_,_), X).
load_context(mode,          '$load context'(_,_,_,_,_,X,_,_,_), X).
load_context(import_set,    '$load context'(_,_,_,_,_,_,X,_,_), X).
load_context(term_position, '$load context'(_,_,_,_,_,M,_,X,_), X) :-
	M\==load.
load_context(file,          '$load context'(_,_,_,_,_,_,_,_,X), X).

% support for loading .po files
set_load_context(mode, F,   '$load context'(A,B,C,D,E,_,G,H,I),
			    '$load context'(A,B,C,D,E,F,G,H,I)).


% Just replace the 1st clause if bootstrap compiler
update_load_context(Module, Hidden) :-
	clause_internal(user:'$load context'(_,_,CF,CS,CD,Mode,Use,Pos,IF), Ptr), !,
	'$erase'(Ptr),
	asserta_internal(user:'$load context'(Module,Hidden,CF,CS,CD,Mode,Use,Pos,IF), _).
% Replace the first two args in clauses up to the first non-included file.
update_load_context(Module, Hidden) :-
	clause_internal('$load context'(_,_,CF,_,_,_,_,_,IF), Ptr),
	'$patch_fact'(Ptr, 1, Module), 
	'$patch_fact'(Ptr, 2, Hidden),
	IF = CF, !.

remove_load_context :-
	most_general_load_context(Head),
	clause_internal(Head, Ptr), !, % first only
	'$erase'(Ptr).

%-----------------------------------------------------------------------
% A fragment of an AVL tree package.
% Used in library(assoc).


dic_lookup(Dic0, Key, Val, Dic) :-
	get_assoc(Key, Dic0, Val0), !,
	Val = Val0,
	Dic = Dic0.
dic_lookup(Dic0, Key, Val, Dic) :-
	put_assoc(Key, Dic0, Val, Dic).



empty_assoc(t).

assoc_to_list(t) --> [].
assoc_to_list(t(K,V,_,L,R)) -->
	assoc_to_list(L),
	[K-V],
	assoc_to_list(R).


gen_assoc(Key, t(K,V,_,L,R), Val) :-
	(   gen_assoc(Key, L, Val)
	;   Key = K, Val = V
	;   gen_assoc(Key, R, Val)
	).


get_assoc(Key, t(K,V,_,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, Val, V, L, R).

get_assoc(<, Key, Val, _, Tree, _) :- get_assoc(Key, Tree, Val).
get_assoc(=, _, Val, Val, _, _).
get_assoc(>, Key, Val, _, _, Tree) :- get_assoc(Key, Tree, Val).


get_assoc(Key, t(K0,V0,B,L0,R0), Val0, t(K,V,B,L,R), Val) :-
	compare(Rel, Key, K0),
	get_assoc(Rel, Key, K0, V0, L0, R0, Val0, K, V, L, R, Val).

get_assoc(<, Key, K, V, Tree0, R, Val0, K, V, Tree, R, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).
get_assoc(=, _, K, Val0, L, R, Val0, K, Val, L, R, Val).
get_assoc(>, Key, K, V, L, Tree0, Val0, K, V, L, Tree, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).


put_assoc(Key, Assoc0, Val, Assoc1) :-
	put_assoc(Assoc0, Key, Val, Assoc1, _).


put_assoc(t,            Key, Val, t(Key,Val,0,t,t), 1).
put_assoc(t(K,V,B,L,R), Key, Val, Result, Delta) :-
	compare(O, Key, K),
	put_assoc(O, Key, Val, Result, Delta, K, V, B, L, R).


put_assoc(<, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(L, Key, Val, Lassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B-D1,
	assoc(B1, K, V, Lassoc, R, Assoc).
put_assoc(=, Key, Val, t(Key,Val,B,L,R), 0, _, _, B, L, R).
put_assoc(>, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(R, Key, Val, Rassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B+D1,
	assoc(B1, K, V, L, Rassoc, Assoc).



assoc(-2, K, V, L, R, Assoc) :-
	L = t(K1,V1,B1,L1,R1),
	assoc_left(B1, K1, V1, L1, R1, K, V, R, Assoc).
assoc(-1, K, V, L, R, t(K,V,-1,L,R)).
assoc( 0, K, V, L, R, t(K,V, 0,L,R)).
assoc( 1, K, V, L, R, t(K,V, 1,L,R)).
assoc( 2, K, V, L, R, Assoc) :-
	R = t(K1,V1,B1,L1,R1),
	assoc_right(B1, K1, V1, L1, R1, K, V, L, Assoc).

assoc_left(-1, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 0,L1,t(K,V, 0,R1,R))).
assoc_left( 0, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 1,L1,t(K,V,-1,R1,R))).
assoc_left( 1, K1, V1, L1, R1, K, V, R,		% double LR rotation
	    t(K2,V2, 0,t(K1,V1,BK1,L1,L2),t(K,V,BK,R2,R))) :-
        R1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK1, BK).

assoc_right( 1, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1, 0,t(K,V, 0,L,L1),R1)).
assoc_right( 0, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1,-1,t(K,V, 1,L,L1),R1)).
assoc_right(-1, K1, V1, L1, R1, K, V, L,	% double RL rotation
	     t(K2,V2, 0,t(K,V,BK,L,L2),t(K1,V1,BK1,R2,R1))) :-
        L1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK, BK1).

assoc(-1,  0, 1).
assoc( 0,  0, 0).
assoc( 1, -1, 0).



%-----------------------------------------------------------------------
% Quasi built-ins - verbatim from library(lists).

%   append(?Prefix, ?Suffix, ?Combined)
%   is true when Combined is the combined list of the elements in Prefix 
%   followed by the elements in Suffix. It can be used to form Combined oro
%   it can be used to find Prefix and/or Suffix from a given Combined  

append([], List, List).
append([Head|Tail], List, [Head|Rest]) :- 
	append(Tail, List, Rest).

%   member(?Element, +List)
%   is true when Element is a member of List.  It may be used to test 
%   for membership in a list, but it can also be used to enumerate all 
%   the elements in List.

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).


%   memberchk(+Element, +List)
%   is true when Element is a member of List, but memberchk/2 only succeeds
%   once and can therefore not be used to enumerate the elements in List.

memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :-
	memberchk(Element, Rest).


%   reverse(?List, ?Reversed)
%   is true when Reversed is has the same element as List but in a reversed 
%   order. List must be a proper list.

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], SoFar, Reversed) :-
	reverse(Tail, [Head|SoFar], Reversed).

%   max_list(+ListOfNumbers, ?Max)
%   is true when Max is the greatest of the numbers in the ListOfNumbers.

/* not used 
max_list([Head|Tail], Max) :- 
	max_list(Tail, Head, Max).
*/

max_list([], Max, Max).
max_list([Head|Tail], Element, Max) :-
	Head =< Element, !,
	max_list(Tail, Element, Max).
max_list([Head|Tail], _, Max) :-
	max_list(Tail, Head, Max).

