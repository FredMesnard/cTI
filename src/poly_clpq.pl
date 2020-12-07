%% Fred
%% juillet 1998, octobre 1999, fevrier 2000, avril 2000, juillet 2000
%% Fred & Roberto
%% mai 2002

%% Representation of convex polyhedra using CLP(Q) constraints
%%
%% ph(Variables, Constraints)
%%
%% where
%%
%%   - Variables is a (possibly empty) list of distinct free variables;
%%   - Constraints is a (possibly empty) list of constraints over Variables.

:- module(poly_clpq,[]).
/*
[
	   create_dont_know/2,
	   create_impossible/2,
	   dispose/1,
	   copy/2,
	   add_constraints/3,
	   get_constraints/2,
	   get_variables/2,
	   satisfiable/1,
	   more_precise_or_equal/2,
	   restrict/3,
	   join/3,
	   widening/3,
	   extend_and_add_constraints/3,
	   extend_and_meet/3,
	   rename/3,
	   poly_print/1
	  ]).
*/

:- use_module(library(lists), [append/3]).
:- use_module(library(assoc)).
%:- use_module(library(terms),[term_variables/2]).
:- use_module(library(clpq)).
:- use_module(num_op,[]).

poly_print(ph(Vars,Cs)):-write(Vars-Cs),nl.

%% create_dont_know(+N, -P)
%%
%% Binds P to an N-dimensional, full polyhedron.
%% This corresponds to the "don't know" description for
%% a tuple of N variables.

%create_dont_know(N, ph(Vars, [])) :-
%	length(Vars, N).
create_dont_know(Vars, ph(Vars, [])).


%% create_impossible(+N, -P)
%%
%% Binds P to an N-dimensional, empty polyhedron.
%% This corresponds to the "impossible" description for
%% a tuple of N variables.

create_impossible(N, ph(Vars, [0 = 1])) :-
	length(Vars, N).

%% dispose(+P)
%%
%% Informs the run-time system that the polyhedron P
%% will never be referenced again.

dispose(_).

%% copy(+P, -Q)
%%
%% Binds Q to an exact copy of P.

copy(P, Q) :-
	copy_term(P, Q).

%% add_constraints(+P, +Cs, -Q)
%%
%% If Cs is a list of constraints involving only variables from P,
%% binds Q to a polyhedron that has been obtained from P by adding
%% the constraints in Cs.  The result is undefined otherwise.
%% Upon return, the polyhedron P does not exist any longer.

add_constraints(ph(Vs, Cs1), Cs2, ph(Vs, Cs)) :-
	append(Cs1, Cs2, Cs).

%% get_constraints(+P, -Cs)
%%
%% Binds Cs to a list of constraints that define P.

get_constraints(ph(_Vs, Cs), Cs).

%% get_variables(+P, -Vs)
%%
%% Binds Vs to the list of variables that define P.

get_variables(ph(Vs,_Cs),Vs).


%% satisfiable(+P)
%%
%% Succeeds if and only if P is not empty.

satisfiable(ph(_Vs, Cs)) :-
	 num_op:satisfiable(Cs).

%% more_precise_or_equal(+P1, +P2)
%%
%% Succeeds if and only if P1 is contained in or is equal to P2.

more_precise_or_equal(ph(Vs1, Cs1), ph(Vs2, Cs2)) :-
	num_op:entail(Vs1,Cs1,Vs2,Cs2).

%% restrict(+N, +P, -Q)
%%
%% Binds Q to the polyhedron that is obtained from P
%% by restricting it to the first N variables (dimensions) in Vs.
%% The result is undefined otherwise.
%% Upon return, the polyhedron P does not exist any longer.

restrict(N, ph(Vs, Cs), ph(RVs, RCs)) :-
	prefix(N,Vs,RVs),
	num_op:project(RVs, Cs, RVs, RCs).

prefix(0, _, []) :-
	!.
prefix(N, [X|Xs], [X|Ys]) :-
	N > 0,
	M is N-1,
	prefix(M, Xs, Ys).

%% join(+P1, +P2, -P)
%%
%% Assuming that P1 and P2 are polyhedra of the same dimension,
%% binds P to the polyhedron that is the smallest convex polyhedra
%% that contains both P1 and P2.  The result is undefined otherwise.
%% Upon return, the polyhedron P1 does not exist any longer.
%% PERFORMANCE NOTE: the case where P1 contains or is equal to P2
%%                   is frequent enough to be taken into account
%%                   in case it may help to improve performance.

join(ph(Xs, Cxs), ph(Ys, Cys), ph(Zs, Czs)) :-
	(num_op:entail(Ys, Cys, Xs, Cxs) ->
	    Zs = Xs, Czs = Cxs
	;
	    num_op:convex_hull(Xs, Cxs, Ys, Cys, Zs, Czs)
	).

%% widening(+P1, +P2, -P)
%%
%% Assuming that P1 and P2 are polyhedra of the same dimension
%% and that P1 contains or is equal to P2, binds P to the polyhedron
%% that is the (Cousot & Cousot @ PLILP'92?) widening of P1 and P2.
%% The result is undefined otherwise.
%% Upon return, the polyhedron P1 does not exist any longer.

widening(ph(Xs, Cxs), ph(Ys, Cys), ph(Zs, Czs)) :-
	num_op:widening(Ys, Cys, Xs, Cxs, Zs, Czs).
%	num_op:widening(Xs, Cxs, Ys, Cys, Zs, Czs).

%% extend_and_add_constraints(+P, +Cs, -Q)
%%
%%

extend_and_add_constraints(ph(Vs,Cs),Ds,ph(Ws,Es)):-
	term_variables(Ds,VarsDs),
	ng_union(VarsDs,Vs,Ws),
	append(Ds,Cs,Es).

%% ng_union(Xs,Ys,S) iff
%% S is the (non-ground, ordered) set Ys union Xs
ng_union([],Ys,Ys).
ng_union([X|Xs],Ys,L) :-
	(not_member(Ys,X) ->
	    (ng_union(Xs,Ys,Zs),append(Zs,[X],L))
	;
	    ng_union(Xs,Ys,L)).

not_member([],_).
not_member([X|Xs],Y) :-
	X \== Y,
	not_member(Xs,Y).

%% extend_and_meet(+P1,+P2,-P)
%%
%%

extend_and_meet(ph(Vs,Cs),ph(Ws,Ds),ph(Xs,Es)) :-
	ng_union(Ws,Vs,Xs),
	append(Cs,Ds,Es).

%% rename(+P,+Vs1,-Q)
%%
%% The polyhedra P is renamed relatively to Vs1 and
%% then is unified to Q

rename(ph(Vs,Cs),Vs1,ph(Vs1,Ds)):-
	copy_term(ph(Vs,Cs),ph(Vs1,Ds)).
