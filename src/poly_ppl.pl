% Interface to the Parma Polyhedra Library.

%% Representation of convex polyhedra using PPL polyhedra
%%
%% vp(Variables, Polyhedron)
%%
%% where
%%
%%   - Variables is a (possibly empty) list of distinct free variables;
%%   - Polyhedron is a PPL closed polyhedron.

:- module(poly_ppl,[]).
/*
	  [
	   create_dont_know/2,
	   create_impossible/2,
	   dispose/1,
	   copy/2,
	   get_constraints/2,
	   get_variables/2,
	   satisfiable/1,
	   more_precise_or_equal/2,
	   restrict/3,
	   join/3,
	   widening/3,
	   extend_and_add_constraints/3,
	   extend_and_meet/3,
	   rename/3
	  ]).
*/
:- use_module(library(lists), [append/3,member/2]).
:- use_module(library(terms), [term_variables/2]).
:- use_module(clpq_clpn, [clpq_clpn/2]).

% current_cti_flag(time_out,10000).  % = 10 seconds

%:- ensure_loaded('/usr/local/lib/ppl_sicstus.pl').

%% create_dont_know(+N, -P)
%%
%% Binds P to an N-dimensional, full polyhedron.
%% This corresponds to the "don't know" description for
%% a tuple of N variables.

create_dont_know(Vars, vp(Vars, Ph)) :-
	length(Vars, N),
	user:ppl_new_C_Polyhedron_from_space_dimension(N, universe, Ph).


%% create_impossible(+N, -P)
%%
%% Binds P to an N-dimensional, empty polyhedron.
%% This corresponds to the "impossible" description for
%% a tuple of N variables.

create_impossible(N, vp(Vars, Ph)) :-
	length(Vars, N),
	user:ppl_new_C_Polyhedron_from_space_dimension(N, empty, Ph).


%% dispose(+P)
%%
%% Informs the run-time system that the polyhedron P
%% will never be referenced again.

dispose(vp(_Vs, Ph)) :-
	user:ppl_delete_Polyhedron(Ph).


%% copy(+P, -Q)
%%
%% Binds Q to an exact copy of P.

copy(vp(Vs1, Ph1), vp(Vs2, Ph2)) :-
	copy_term(Vs1, Vs2),
	user:ppl_new_C_Polyhedron_from_C_Polyhedron(Ph1, Ph2).


%% get_constraints(+P, -Cs)
%%
%% Binds Cs to a list of constraints that define P.

get_constraints(vp(Vs, Ph), Cs) :-
	user:ppl_Polyhedron_get_constraints(Ph, PPL_Cs),
	translate_constraints_ppl_cTI(PPL_Cs, Vs, Cs).


%% get_variables(+P, -Vs)
%%
%% Binds Vs to the list of variables that define P.

get_variables(vp(Vs, _Cs), Vs).


%% satisfiable(+P)
%%
%% Succeeds if and only if P is not empty.

satisfiable(vp(_Vs, Ph)) :-
	 \+ user:ppl_Polyhedron_check_empty(Ph).


%% more_precise_or_equal(+P1, +P2)
%%
%% Succeeds if and only if P1 is contained in or is equal to P2.

more_precise_or_equal(vp(_Vs1, Ph1), vp(_Vs2, Ph2)) :-
	user:ppl_Polyhedron_contains_Polyhedron(Ph2, Ph1).


%% restrict(+N, +P, -Q)
%%
%% Binds Q to the polyhedron that is obtained from P
%% by restricting it to the first N variables (dimensions) in Vs.
%% The result is undefined otherwise.
%% Upon return, the polyhedron P does not exist any longer.

restrict(N, vp(Vs, Ph), vp(RVs, RPh)) :-
	main:current_cti_flag(time_out,T0),
	T1 is T0//10,
	prefix(N, Vs, RVs),
	catch(
	      (
		user:ppl_set_timeout(T1),
		user:ppl_Polyhedron_remove_higher_space_dimensions(Ph, N),
		user:ppl_reset_timeout,
		RPh = Ph
	      ),
	      time_out,
 	      (
 		user:ppl_delete_Polyhedron(Ph),
 		user:ppl_reset_timeout,
 		throw(time_out))).
%%%  	      (
%%%  		write('% restrict: time_out!'), nl,
%%%  		user:ppl_delete_Polyhedron(Ph),
%%%  		create_dont_know(RVs, VP),
%%%  		make_non_negativity_constraints(RVs, Cs),
%%%  		extend_and_add_constraints(VP, Cs, vp(RVs, RPh))
%%%  	      )
%%%  	     ).

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

join(vp(Vs1, Ph1), vp(_Vs2, Ph2), vp(Vs, Ph1)) :-
	copy_term(Vs1, Vs),
	user:ppl_Polyhedron_poly_hull_assign(Ph1, Ph2).

%% widening(+P1, +P2, -P)
%%
%% Assuming that P1 and P2 are polyhedra of the same dimension
%% and that P1 contains or is equal to P2, binds P to the polyhedron
%% that is the (Cousot & Cousot @ PLILP'92?) widening of P1 and P2.
%% The result is undefined otherwise.
%% Upon return, the polyhedron P1 does not exist any longer.

widening(vp(Vs1, Ph1), vp(_Vs2, Ph2), vp(Vs, Ph1)) :-
	copy_term(Vs1, Vs),
	% Begin kludge
	copy_term(Vs1, NNVs),
	numbervars(NNVs, 0, _),
	make_non_negativity_constraints(NNVs, NNCs),
	% End kludge
	user:ppl_Polyhedron_limited_H79_extrapolation_assign(Ph1, Ph2, NNCs).
	%user:ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(Ph1, Ph2, NNCs).

make_non_negativity_constraints([], []).
make_non_negativity_constraints([V|Vs], [V >= 0 | NNCs]) :-
	make_non_negativity_constraints(Vs, NNCs).


%% extend_and_add_constraints(+P, +Cs, -Q)
%%
%% Upon return, the polyhedron P does not exist any longer.

extend_and_add_constraints(vp(Vs, Ph), Cs, vp(Ws, Ph)) :-
%	pp('Before ', Ph),
	%write(Cs),write('-'),
	term_variables(Cs, Cs_Vars),
	ng_union(Cs_Vars, Vs, Ws),
	length(Vs, Old_Dimension),
	length(Ws, New_Dimension),
	Dimensions_To_Add is New_Dimension-Old_Dimension,
	user:ppl_Polyhedron_add_space_dimensions_and_embed(Ph,
							   Dimensions_To_Add),
	translate_constraints_cTI_ppl(Ws, Cs, PPL_Cs),
	%write(Cs),nl,
%	write(PPL_Cs),nl,
	user:ppl_Polyhedron_add_constraints(Ph, PPL_Cs).
%	pp('After ', Ph).


%% extend_and_meet(+P1,+P2,-P)
%%
%% Upon return, the polyhedron P does not exist any longer.

extend_and_meet(vp(Vs1, Ph1), vp(Vs2, Ph2), vp(Vs, Ph1)) :-
	%ng_union(Vs1, Vs2, Vs),
	get_constraints(vp(Vs2, Ph2), Cs2),
	extend_and_add_constraints(vp(Vs1, Ph1), Cs2, vp(Vs, Ph1)).

%% rename(+P, +Vs1, -Q)
%%
%% The polyhedra P is renamed relatively to Vs1 and
%% then is unified to Q
%% RB: what?

%rename(ph(Vs,Cs),Vs1,ph(Vs1,Ds)):-
%	copy_term(ph(Vs,Cs),ph(Vs1,Ds)).
rename(vp(_Vs, Ph), Vs1, vp(Vs1, Ph1)) :-
	user:ppl_new_C_Polyhedron_from_C_Polyhedron(Ph, Ph1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PPL -> cTI

translate_constraints_ppl_cTI(PPL_Cs, Vs, Cs) :-
	construct_term(Vs,VA),
	%VA =.. [array|Vs],
	translate_vars_ints_list(PPL_Cs, Cs, VA).

translate_vars_ints_list([], [], _VA).
translate_vars_ints_list([In|In_Tail], [Out|Out_Tail], VA) :-
	translate_vars_ints(In, Out, VA),
	translate_vars_ints_list(In_Tail, Out_Tail, VA).

translate_vars_ints(In, Out, _VA) :-
	integer(In),
	!,
	Out = rat(In, 1).
translate_vars_ints(In, Out, VA) :-
	In = '$VAR'(N),
	!,
	N1 is N+1,
	%arg(N1, VA, Out).
	get_nth(N1,VA,Out).
translate_vars_ints(In, Out, VA) :-
	In =.. [F|In_Args],
	translate_vars_ints_list(In_Args, Out_Args, VA),
	Out =.. [F|Out_Args].


% construct_term(+Xs,-T)
%     T is an indexed sequence (starting from 1) containing Xs
construct_term(Xs,T) :-
	functor(T,array,255),
	construct_term(Xs,1,T).

construct_term([],_,_).
construct_term([X|Xs],N,T) :-
	ct(N,X,T,M,T2),
	construct_term(Xs,M,T2).

ct(N,X,T,M,T) :-
	N<255,!,M is N+1,arg(N,T,X).
ct(255,X,T1,2,T2) :-
	arg(255,T1,T2),
	functor(T2,array,255),
	arg(1,T2,X).

% get_nth(+N,+T,-E)
%     E is the nth elt (starting from 1) of T
get_nth(N,T,E) :-
	N<255,!,arg(N,T,E).
get_nth(N,T,E) :-
	N>=255,
	arg(255,T,T2),
	M is N-254,
	get_nth(M,T2,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cTI -> PPL

translate_constraints_cTI_ppl(Vs, Cs, PPL_Cs) :-
	clpq_clpn(Cs, Cs1), 
	copy_term(Vs-Cs1, PPL_Vs-Tmp_Cs),
	numbervars(PPL_Vs, 0, _),
	PPL_Cs = Tmp_Cs.

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

pp(I, vp(_Vs, Ph)) :-
	!,
	pp(I, Ph).
pp(I, Ph) :-
	write(I),
	user:ppl_Polyhedron_get_constraints(Ph, Cs),
	write(Cs),
	nl.
