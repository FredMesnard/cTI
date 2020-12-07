%:- entry user_goal.

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

member(X,[X|_]).
member(X,[_|Y]) :-
	member(X,Y).

with_input_from_chars(A,B) :-
    ground([A,B]).

absolute_file_name(A,B,C) :-
    ground([A,B,C]).

must_be_nonneg(Arg, N, Goal) :-
    integer(Arg), Arg >= 0, !.
should_be(array, Array, 2, Goal).

init_rep.
conclude_rep.
variable_rep(A, B) :-
    ground([A,B]).
lub(A,B,C) :-
    ground([A,B,C]).
glb(A,B,C) :-
    ground([A,B,C]).
restrict_threshold(A,B,C) :-
    ground([A,B,C]).
bool_implies(A,B,C) :-
    ground([A,B,C]).
restricted_glb(A,B,C,D) :-
    ground([A,B,C,D]).
rename_term(A,B,C) :-
    ground([A,C]).
reverse_rename_term(A,B,C) :-
    ground([A,C]).
iff_conj(A,B,C) :-
    ground([A,C]).
bool_abstract_unify(A,B,C,D,E) :-
    ground([A,B,D,E]).
bool_abstract_exit(A,B,C,D,E) :-
    ground([A,B,D,E]).

print_out(X) :-
    ground(X).
milli_time(X) :-
    integer(X).

avl_next(Key, node(K,V,_,L,R), Knext, Vnext) :-
        (   K @=< Key ->
            avl_next(Key, R, Knext, Vnext)
        ;   avl_next(Key, L, K1, V1) ->
            Knext = K1, Vnext = V1
        ;   Knext = K,  Vnext = V
        ).
avl_member(Key, node(K,V,_,L,R), Val) :-
        (   avl_member(Key, L, Val)
        ;   Key = K, Val = V
        ;   avl_member(Key, R, Val)
        ).
avl_fetch(Key, node(K,V,_,L,R), Val) :-
        compare(O, Key, K),
        avl_fetch_1(O, Key, Val, V, L, R).


avl_fetch_1(=, _,   Val, Val, _, _).
avl_fetch_1(<, Key, Val, _, node(K,V,_,L,R), _) :-
        compare(O, Key, K),
        avl_fetch_1(O, Key, Val, V, L, R).
avl_fetch_1(>, Key, Val, _, _, node(K,V,_,L,R)) :-
        compare(O, Key, K),
        avl_fetch_1(O, Key, Val, V, L, R).
avl_store(Key, AVL0, Val, AVL1) :-
        avl_store(AVL0, Key, Val, AVL1, _).


avl_store(empty,           Key, Val, node(Key,Val,0,empty,empty), 1).
avl_store(node(K,V,B,L,R), Key, Val, Result, Delta) :-
        compare(O, Key, K),
        avl_store(O, Key, Val, Result, Delta, K, V, B, L, R).


avl_store(=, Key, Val, node(Key,Val,B,L,R), 0, _, _, B, L, R).
avl_store(<, Key, Val, Result,          Delta, K, V, B, L, R) :-
        avl_store(L, Key, Val, Lavl, Ldel),
        Delta is \(B) /\ Ldel,  % this grew iff left grew and was balanced
        B1 is B-Ldel,
        (   B1 =:= -2 ->        % rotation needed
            Lavl = node(Y,VY,OY,A,CD),      
            (   OY =< 0 ->
                NY is OY+1, NK is -NY,
                Result = node(Y,VY,NY,A,node(K,V,NK,CD,R))
            ;/* OY = 1, double rotation needed */
                CD = node(X,VX,OX,C,D),
                NY is 0-((1+OX) >> 1),
                NK is (1-OX) >> 1,
                Result = node(X,VX,0,node(Y,VY,NY,A,C),node(K,V,NK,D,R))
            )
        ;   Result = node(K,V,B1,Lavl,R)
        ).
avl_store(>, Key, Val, Result,          Delta, K, V, B, L, R) :-
        avl_store(R, Key, Val, Ravl, Rdel),
        Delta is \(B) /\ Rdel,  % this grew iff right grew and was balanced
        B1 is B+Rdel,
        (   B1 =:= 2 ->         % rotation needed
            Ravl = node(Y,VY,OY,AC,D),
            (   OY >= 0 ->
                NY is OY-1, NK is -NY,
                Result = node(Y,VY,NY,node(K,V,NK,L,AC),D)
            ;/* OY = -1, double rotation needed */
                AC = node(X,VX,OX,A,C),
                NY is (1-OX) >> 1,
                NK is 0-((1+OX) >> 1),
                Result = node(X,VX,0,node(K,V,NK,L,A),node(Y,VY,NY,C,D))
            )
        ;   Result = node(K,V,B1,L,Ravl)
        ).

empty_avl(empty).
avl_min(node(K,V,_,L,_), MinKey, MinVal) :-
        avl_min_1(L, MinKey, MinVal, K, V).

avl_min_1(empty, K, V, K, V).
avl_min_1(node(K,V,_,L,_), MinKey, MinVal, _, _) :-
        avl_min_1(L, MinKey, MinVal, K, V).

%  file    : analysis.pl
%  Authors : Peter Schachte
%  Purpose : Storage and manipulation of predicate analyses
%
%				Abstract
%
%  This code maintains an abstract datatype storing predicate analyses.

:- module(analysis, [
	anal_top/1,
	anal_bottom/1,
	anal_meet/3,
	anal_meet/4,
	anal_join/3,
	anal_join/4,
	anal_restrict/3,
	anal_print/1,
	analyze_unif/6,
	analyze_eval/6,
	analyze_builtin/4,
	analyze_call/6,
	analyze_call_pattern/3,
	analyze_foreign/2,
	builtin_analysis/2	
   ]).


%:- use_module(boolfn).
%:- use_module(predstore).


%  anal_top(-Top)
%  Top is the topmost (weakest) element in our analysis domain (lattice).

anal_top(1).


%  anal_bottom(-Bottom)
%  Bottom is the bottommost (strongest) element in out analysis domain
%  (lattice).


anal_bottom(0).


%  anal_meet(+Analysis1, +Analysis2, -Analysis)
%  anal_meet(+Analysis1, +Analysis2, +Restriction, -Analysis)
%  Analysis is the meet (greatest lower bound) of Analysis1 and Analysis2, two
%  predicate analyses, restricted to the variables numbered strictly lower
%  than Restriction if Restriction is supplied.

anal_meet(Analysis1, Analysis2, Analysis) :-
	bool_meet(Analysis1, Analysis2, Analysis).

anal_meet(Analysis1, Analysis2, Restriction, Analysis) :-
	bool_meet_restricted(Analysis1, Analysis2, Restriction, Analysis).
% 	bool_meet(Analysis1, Analysis2, Analysis3),
% 	bool_restrict(Analysis3, Restriction, Analysis).


%  anal_join(+Analysis1, +Analysis2, -Analysis)
%  anal_join(+Analysis1, +Analysis2, +Restriction, -Analysis)
%  Analysis is the join (least upper bound) of Analysis1 and Analysis2, two
%  predicate analyses, restricted to the variables numbered strictly lower
%  than Restriction if Restriction is supplied.


anal_join(Analysis1, Analysis2, Analysis) :-
	bool_join(Analysis1, Analysis2, Analysis).

anal_join(Analysis1, Analysis2, Restriction, Analysis) :-
	bool_join(Analysis1, Analysis2, Analysis3),
	bool_restrict(Analysis3, Restriction, Analysis).


%  anal_restrict(+Analysis0, +Restriction, -Analysis)
%  Analysis is Analysis0 restricted to variables numbered strictly lower than
%  Restriction.

anal_restrict(Analysis0, Restriction, Analysis) :-
	bool_restrict(Analysis0, Restriction, Analysis).
	

%  anal_print(+Analysis)
%  Print out Analysis in some suitable format.

anal_print(Analysis) :-
	bool_print(Analysis).


%  analyze_unif(Var, Term, Vars, Restriction, Analysis0, Analysis)
%  Our abstract unification operation.  Analysis is the meet of Analysis0 and
%  the analysis of the unification of variable Var with term Term (which
%  contains all and only the variables on list Vars), restricted to the
%  variables numbered strictly lower than Restriction.

analyze_unif(Var, _Term, Vars, Restriction, Analysis0, Analysis) :-
	bool_abstract_unify(Analysis0, Var, Vars, Restriction, Analysis).

% 	iff_conj(Var, Vars, Analysis1),
% 	bool_meet_restricted(Analysis0, Analysis1, Restriction, Analysis).

% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  analyze_eval(Var, Term, Vars, Restriction, Analysis0, Analysis)
%  Analysis is meet of Analysis0 and the analysis of the binding of Var to the
%  evaluation of expression Term, which contains all the vars on the list
%  Vars, all restricted to the variables numbered strictly lower than
%  Restriction.

analyze_eval(Var, _, Vars, Restriction, Analysis0, Analysis) :-
	bool_rep_conj([Var|Vars], Analysis1),
	bool_meet_restricted(Analysis0, Analysis1, Restriction, Analysis).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  analyze_builtin(Call, Restriction, Analysis0, Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to a
%  builtin predicate, restricted to the variables numbered strictly lower than
%  Restriction.  Preds is a predicate store containing the analysis of Pred.
%  Essentially this just looks up the analysis of Pred in Preds and renames
%  variables in it to match those in Call, and then does a restricted meet.

analyze_builtin(Call, Restriction, Analysis0, Analysis) :-
	builtin_analysis(Call, Analysis1),
	bool_meet_restricted(Analysis0, Analysis1, Restriction, Analysis).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  anal_call(+Pred, +Preds, +Call, +Restriction, +Analysis0, -Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to
%  predicate Pred, restricted to the variables numbered strictly lower than
%  Restriction.  Preds is a predicate store containing the analysis of Pred.
%  Essentially this just looks up the analysis of Pred in Preds and renames
%  variables in it to match those in Call, and then does a restricted meet.

analyze_call(Pred, Preds, Call, Restriction, Analysis0, Analysis) :-
	get_pred_success(Pred, Preds, Analysis1),
	bool_abstract_exit(Analysis0, Analysis1, Call, Restriction, Analysis).

% 	bool_rename(Analysis1, Call, Analysis2),
% 	bool_meet_restricted(Analysis0, Analysis2, Restriction, Analysis).

% 	bool_meet(Analysis0, Analysis2, Analysis3),
% 	bool_restrict(Analysis3, Restriction, Analysis).


%  analyze_call_pattern(+Call, +Analysis, -Callpat)
%  Callpat is the call pattern for call Call if Analysis is the analysis of
%  the variables at the program point just before the call.

analyze_call_pattern(Call, Analysis, Callpat) :-
	bool_reverse_rename(Analysis, Call, Callpat).


%  analyze_foreign(+Foreignspec, -Analysis)
%  Analysis is the success pattern for the foreign predicate with foreign
%  specification Foreignspec.

analyze_foreign(Foreignspec, Analysis) :-
	functor(Foreignspec, _, Arity),
	analyze_foreign_args(Arity, Foreignspec, 1, Analysis).

analyze_foreign_args(N, Foreignspec, Analysis0, Analysis) :-
	(   N =:= 0 ->
		Analysis = Analysis0
	;   arg(N, Foreignspec, Argspec),
	    (	definite_argspec(Argspec) ->
		    variable_rep(N, Var),
		    bool_meet(Analysis0, Var, Analysis1)
	    ;	Analysis1 = Analysis0
	    ),
	    N1 is N - 1,
	    analyze_foreign_args(N1, Foreignspec, Analysis1, Analysis)
	).

definite_argspec(+Spec) :-
	definite_argspec1(Spec).
definite_argspec(-Spec) :-
	definite_argspec1(Spec).
definite_argspec([-Spec]) :-
	definite_argspec1(Spec).

definite_argspec1(integer).
definite_argspec1(float).
definite_argspec1(single).
definite_argspec1(double).
definite_argspec1(atom).
definite_argspec1(string).
definite_argspec1(address).
definite_argspec1(address(_)).
%  the only argspec that isn't definite is 'term'.


%  builtin_analysis(+Goal, -Analysis)
%  Analysis is the boolean function describing the groundness of Goal's args on
%  successful exit from a call to the builtin predicate described by Goal.
%  The args of Goal are integers in increasing order.  Note that if new
%  predicaes are added here, they should also be added to builtins:builtin/2.

builtin_analysis(!, 1).			% No args
builtin_analysis(true, 1).			% No args
builtin_analysis(otherwise, 1).		% No args
builtin_analysis(fail, 0).			% Can't succeed!
builtin_analysis(false, 0).			% Can't succeed!
builtin_analysis(abort, 0).			% Can't succeed!
builtin_analysis(raise_exception(_), 0).	% Can't succeed!
builtin_analysis(halt, 0).			% Can't succeed!
builtin_analysis(halt(_), 0).			% Can't succeed!
builtin_analysis(call(_), 1).		% No info
builtin_analysis(unix(A), X):-			% A
	variable_rep(A, X).
builtin_analysis(repeat, 1).			% No args
builtin_analysis(compare(A,_,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis((_ @<_ ), 1).		% No info
builtin_analysis((_ @=<_ ), 1).		% No info
builtin_analysis((_ ==_ ), 1).		% No info
builtin_analysis((_ @>=_ ), 1).		% No info
builtin_analysis((_ @>_ ), 1).		% No info
builtin_analysis((_ \==_ ), 1).		% No info
builtin_analysis((A < B), X) :-			% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A =< B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A =:= B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A >= B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A > B), X) :-			% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A =\= B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A is B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis((A =.. B), X) :-		% A <-> B
	a_iff_b_rep(A, B, X).
builtin_analysis(atom(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(atomic(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(callable(_), 1).		% No info
builtin_analysis(compound(_), 1).		% No info
builtin_analysis(db_reference(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(float(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ground(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(integer(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(nonvar(_), 1).		% No info
builtin_analysis(number(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(simple(_), 1).		% No info
builtin_analysis(var(_), 1).			% No info (can't say ~A)
builtin_analysis(functor(_,B,C), X) :-		% B & C
	a_and_b_rep(B, C, X).
builtin_analysis(arg(A,B,C), X) :-		% A & (B -> C)
	variable_rep(A, Va),
	variable_rep(B, Vb),
	variable_rep(C, Vc),
	bool_implies(Vb, Vc, Imp),
	bool_meet(Va, Imp, X).
builtin_analysis(name(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(atom_chars(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(number_chars(A,B), X) :-	% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(numbervars(A,B,C), X) :-	% A & B & C
	a_and_b_rep(A, B, X1),
	variable_rep(C, X2),
	bool_meet(X1, X2, X).
builtin_analysis(hash_term(A,B), X) :-		% A & B (error when var(A))
	a_and_b_rep(A, B, X).
builtin_analysis(subsumes_chk(A,B), X) :-	% A -> B
	variable_rep(A, Va),
	variable_rep(B, Vb),
	bool_implies(Va, Vb, X).
builtin_analysis(copy_term(A,B), X) :-		% A <-> B
	a_iff_b_rep(A, B, X).
% This is commented out because it's in so many test suites:
% builtin_analysis(append(A,B,C), X) :-		% (A & B) <-> C
%	iff_conj(C, [A,B], X).
builtin_analysis(length(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(sort(A,B), X) :-		% A <-> B
	a_iff_b_rep(A, B, X).
builtin_analysis(keysort(A,B), X) :-		% A <-> B
	a_iff_b_rep(A, B, X).
builtin_analysis('C'(A,B,C), X) :-		% A <-> (B & C)
	iff_conj(A, [B,C], X).
builtin_analysis(statistics, 1).		% No args
builtin_analysis(statistics(A,B), X) :-
	a_and_b_rep(A, B, X).
builtin_analysis(see(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(seeing(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(seen, 1).			% No args
builtin_analysis(tell(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(telling(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(told, 1).			% No args
builtin_analysis(open(A,B,C), X) :-		% A & B & C
	a_and_b_and_c_rep(A, B, C, X).
builtin_analysis(open(A,B,C,D), X) :-		% A & B & C & D
	a_and_b_and_c_rep(A, B, C, X0),
	variable_rep(D, Vd),
	bool_meet(X0, Vd, X).
builtin_analysis(close(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(current_input(A), X) :-	% A
	variable_rep(A, X).
builtin_analysis(current_output(A), X) :-	% A
	variable_rep(A, X).
builtin_analysis(set_input(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(set_output(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(current_stream(A,B,C), X) :-	% A & B & C
	a_and_b_and_c_rep(A, B, C, X).
builtin_analysis(stream_code(A,B), X) :-	% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(absolute_file_name(A,B), X) :-	% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(absolute_file_name(A,B,C), X) :- % A & B & C
	a_and_b_and_c_rep(A, B, C, X).
builtin_analysis(current_op(A,B,C), X) :-	% A & B & C
	a_and_b_and_c_rep(A, B, C, X).
builtin_analysis(op(A,B,C), X) :-		% A & B & C
	a_and_b_and_c_rep(A, B, C, X).
builtin_analysis(read(_), 1).		% No info
builtin_analysis(read(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write(_), 1).		% No info
builtin_analysis(write(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write_canonical(_), 1).	% No info
builtin_analysis(write_canonical(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(print(_), 1).		% No info
builtin_analysis(print(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(writeq(_), 1).		% No info
builtin_analysis(writeq(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(display(_), 1).		% No info
builtin_analysis(display(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,B,_), X) :-		% A
	a_and_b_rep(A, B, X).
builtin_analysis(get(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(get0(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get0(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(nl, 1).			% No args
builtin_analysis(nl(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(peek_char(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(peek_char(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(put(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(put(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(skip(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(skip(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(skip_line, 1).		% No args
builtin_analysis(skip_line(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(tab(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(tab(A,B), X) :-		% A & B
	a_and_b_rep(A, B, X).
builtin_analysis(ttyget(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyget0(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttynl, 1).			% No args
builtin_analysis(ttyput(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyskip(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttytab(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(assert(_), 1).		% No info
builtin_analysis(assert(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(asserta(_), 1).		% No info
builtin_analysis(asserta(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(assertz(_), 1).		% No info
builtin_analysis(assertz(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(clause(_,_), 1).		% No info
builtin_analysis(clause(_,_,C), X) :-		% C
	variable_rep(C, X).
builtin_analysis(current_key(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(erase(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(instance(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(recorda(A,_,C), X) :-		% A & C
	a_and_b_rep(A, C, X).
builtin_analysis(recorded(A,_,C), X) :-		% A & C
	a_and_b_rep(A, C, X).
builtin_analysis(recordz(A,_,C), X) :-		% A & C
	a_and_b_rep(A, C, X).
builtin_analysis(retract(_), 1).		% No info
builtin_analysis(retractall(_), 1).		% No info
builtin_analysis(expand_term(_,_), 1).	% No info

%  metapredicates:   these should be handled more carefully, so that if we can
%  determine at compile time the predicate that will be called, we use that
%  predicate's analysis.  But this will have to be done before we determine
%  the SCCs, otherwise we won't be guaranteed the bottom-up order.
builtin_analysis(phrase(_,_), 1).		% No info
builtin_analysis(phrase(_,_,_), 1).		% No info

% Nonstandard (or non-Quintus) builtins added for specific tests
builtin_analysis(inc(A,B), X) :-
	a_and_b_rep(A, B, X).			% A & B
builtin_analysis(dec(A,B), X) :-
	a_and_b_rep(A, B, X).			% A & B
builtin_analysis(real(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(not(_), 1).			% No info



a_and_b_rep(A, B, X) :-
	variable_rep(A, Va),
	variable_rep(B, Vb),
	bool_meet(Va, Vb, X).

a_iff_b_rep(A, B, X) :-
	variable_rep(A, Va),
	variable_rep(B, Vb),
	bool_iff(Va, Vb, X).


a_and_b_and_c_rep(A, B, C, X) :-
	variable_rep(A, Va),
	variable_rep(B, Vb),
	variable_rep(C, Vc),
	bool_meet(Va, Vb, X0),
	bool_meet(X0, Vc, X).
%  file    : analyze
%  Authors : Peter Schachte
%  Purpose : Groundness analysis of a Prolog source file
%
%				Abstract
%
%  The top level of our Prolog global analyzer.

:- module(analyze, [
	analyze/1,
	anz/1
   ]).


/**********************************************************************

			   The Abstract Interpreter

The basic algorithm:

++enumerate
    1.	Read in the whole program, forming the Clark completion.  This
	is done in calls.pl.
    2.  Find the strongly-connected components in the program call
	graph.  This is done in scc.pl.
    3.  Analyze the program one strongly-connected component at a time,
	starting from the bottom, giving us success patterns.  This is done in
	bottomup.pl.
    4.  Analyze the program one strongly-connected component at a time,
	starting from the top, giving us call patterns.  This is done in
	topdown.pl.
--enumerate

Note that in this approach we must have the whole program in memory
all at once.  This is probably not a very good approach for very large
programs.  A more sophisticated algorithm could avoid this by
analyzing the program bottom up a file at a time, and then again top
down a file at a time.  This could be done by making one pass over all
the files to construct the file dependency graph and collect module
imports and exports, and all multifile predicates, then making a
second pass over all the files for the bottom up analysis, and then a
third pass for the top down analysis.  Where there are circular file
dependencies, multiple files must be processed at once for the second
and third passes, but this is relatively rare.  We must be given the
names of all files in the program, or, better yet, most of the files
in the program must be loaded by other files in the program, so that
only one or a few of the files need be explicitly given to the
analyzer.

A planned later version of this analyzer will be incremental, which
means we must be careful in this version to choose data structures
that are amenable to incremental update.  This means that we resist
numbering our predicates in strongly-connected component order because
the strongly-connected components may change and we don't want to have
to renumber our predicates just to maintain the order.

**********************************************************************/


/*======================================================================

			     Future Work

For representations of boolean functions where equality checking is
very cheap (e.g., bryant graphs), we can sometimes substitute an
equality check for computing some meets and joins.  We can do this by
using the *Fixed* part of a clause's analysis to store the current
best approximation of that clause, and associating with each goal in
the variable part its current best approximation.  Then when analyzing
a clause, if the new analysis of a goal is the same as the old, we
don't meet it with the other analyses of the other goals.  And if the
analysis of a clause is the same as the previous analysis of that
clause, then we need not join it with the analyses of the other
clauses.  We get away with this by always meeting the previous
analysis of a clause with the analyses of the calls, and always
joining the analysis of the predicate with the analyses of the
clauses.  We can also tell that the analysis of a call will be the
same as last time if the analysis of the called predicate hasn't
changed since last time.  This could save a fair amount of meeting,
joining, and renaming.  But it only makes sense if equality comparison
is cheaper than meeting and joining.

Note that since the variable part only contains calls to predicates in
the same strongly-connected component, this trick would only save
effort for strongly-connected components with more than a single
predicate.  Still, it might be worth the effort for large
strongly-connected components.

  ======================================================================*/
/*======================================================================

			    Required External Code

'calls.pl' contains code to load a number of Prolog source files and
all the files they require, and return all the predicates in all those
files as result.  'scc.pl' finds the strongly connected components in
the program call graph.  'bottomup.pl' and 'topdown.pl' contain the
code for the bottom up and top down analysis, respectively.
'boolfn.pl' contains the code to maniuplate Boolean functions; this is
an interface to foreign code which does all the work.  'predstore.pl'
contains code to manipulate a predicate store abstract dataype; the
predicate store holds the collection of predicates in the program and
whatever analysis has been computed at a given point in the analysis.
'analysis.pl' contains code to store and manipulate predicate
analyses; at the moment, the analysis is just the groundness analysis,
but it will later be extended to contain other analyses as well.
'misc.pl', of course, contains miscellaneous auxiliary predicates used
by various parts of the analyzer.  'library(charsio)' is in the
standard Quintus Prolog library; it causes "chars" (lists of character
codes) to be printed out as double-quoted strings, making them much
more readable.

  ======================================================================*/

% :- ensure_loaded(calls).
% :- ensure_loaded(scc).
% :- ensure_loaded(bottomup).
% :- ensure_loaded(topdown).
% :- ensure_loaded(boolfn).
% :- ensure_loaded(predstore).
% :- ensure_loaded(analysis).
% :- ensure_loaded(misc).
% :- ensure_loaded(library(charsio)).

/*======================================================================
			    Command Line Handling
  ======================================================================*/

%  runtime_entry(+Condition)
%  Main predicate of a Quintus Prolog program.  Condition is 'start' if
%  program is just starting, or 'abort' if restarting after an abort.

user_runtime_entry(start) :-
	nogc,
	(   unix(args([File|Files])) ->
		analyze_list(File, Files, true, user_runtime_entry(start))
	;   format('usage:  analyze [-q] [-g initial_goal] file file ...~n',
		   []),
	    halt(1)
	).


%  goal
%  This is here only to make the program consistent with the other programs I
%  analyze.  I don't actually call this.

user_goal :- user_runtime_entry(start).



%  anz(List)
%  Do what runtime_entry(start) does, but take command line as argument, and
%  initial goal defaults to `goal' rather than runtime_entry(start).

anz(List) :-
	nogc,
	analyze_list(List, true, goal).


%  analyzeq(+File)
%  Analyze File and print the results in a somewhat readable format.

analyzeq(File) :-
	analyze(File, user_runtime_entry(start), Preds),
	print_analyses(Preds).


%  analyze(+File)
%  Analyze File and print the results in a somewhat readable format.

analyze(File) :-
	analyze(File, user_goal, Preds),
	print_analyses(Preds).


%  analyze_list(+Files, +Print, +Goal)
%  analyze_list(+File, +OtherFiles, +Print, +Goal)
%  Analyze File and OtherFiles and, if Print is 'true' and [File|OtherFiles]
%  doesn't contain a -q switch, print the results in a somewhat readable
%  format.  Goal is the initial goal, unless [File|OtherFiles] contains a -g
%  *Usergoal* switch, in which case *Usergoal* is the initial goal.

analyze_list([], _, _).
analyze_list([File|Files], Print, Goal) :-
	analyze_list(File, Files, Print, Goal).


analyze_list('-q', Files, _, Goal) :-
	!,
	analyze_list(Files, false, Goal).
analyze_list('-g', [Goalatom|Files], Print, _) :-
	!,
	atom_chars(Goalatom, Goalchars),
	append(Goalchars, ".", Goalchars1),
	(   with_input_from_chars(read_term([syntax_errors(quiet)], Goal),
				  Goalchars1) ->
		true
	;   Goal = user_runtime_entry(start),
	    format('Asyntactic goal "~w"~nusing "~w" as initial goal',
		   [Goalatom, Goal])
	),
	analyze_list(Files, Print, Goal).
analyze_list(File, Files, Print, Goal) :-
	format('===> ~w~n', File),
	(   analyze(File, Goal, Preds) ->
		true
	;   format('! Analysis failed !~n', []),
	    empty_predstore(Preds)		% go on if analyze/3 fails
	),
	(   Print == true ->
		print_analyses(Preds)
	;   true
	),
	analyze_list(Files, Print, Goal).


/**
This is the main top level predicate, which analyzes a single file, and all
the files it loads.  Most of the code handles keeping runtime statistics; the
work of the predicate is done in the calls to file_contents/2, sccs/3,
analyze_bottom_up/3, and analyze_top_down/3.
**/

%  analyze(+File, +Goal, -Preds)
%  

analyze(File, Goal, Preds) :-
	init_rep,
	milli_time(Tread0),
%	statistics(runtime, _),
	file_contents(File, Preds0),
	simplify_goal(Goal, user, 1, _, Preds0, Preds1, Goal1),
	milli_time(Tread1),
	T1 is Tread1-Tread0,
%	statistics(runtime, [_,T1]),
	print_pred_stats(Preds1),
    	format('    Read time                   :  ~w msecs~n', [T1]),
	milli_time(Tscc0),
%	statistics(runtime, _),
	sccs(Preds1, Preds2, Goal1, SCCs),	% NB: SCCs is topologically
						% sorted at this point
	milli_time(Tscc1),
	T2 is Tscc1-Tscc0,
%	statistics(runtime, [_,T2]),
	length(SCCs, SCCcount),
	sum_of_lengths(SCCs, 0, Reachable),
    	format('    SCCs found                  :  ~w~n', [SCCcount]),
    	format('    Reachable predicates found  :  ~w~n', [Reachable]),
    	format('    SCCs time                   :  ~w msecs~n', [T2]),
	milli_time(Tbu0),
%	statistics(runtime, _),
	analyze_bottom_up(SCCs, Preds2, Preds3),
	milli_time(Tbu1),
	T3 is Tbu1-Tbu0,
%	statistics(runtime, [_,T3]),
	format('    Bottom-up analysis time     :  ~w msecs~n', [T3]),
	reverse(SCCs, SCCsrev),
	milli_time(Ttd0),
%	statistics(runtime, _),
	analyze_single_goal(Goal1, Preds3, Preds4),
	analyze_top_down(SCCsrev, Preds4, Preds),
	milli_time(Ttd1),
	T4 is Ttd1-Ttd0,
%	statistics(runtime, [_,T4]),
	format('    Top-down analysis time      :  ~w msecs~n~n', [T4]),
	conclude_rep.


/**********************************************************************

			Printing Out Analysis Results

**********************************************************************/

%  print_analyses(+Preds)
%  Print the bottom-up and top-down analyses of all the predicates in the
%  predstore Preds so the user can hope to understand them.  Well, actually,
%  we print them out however the foreign code handles printing internal
%  representations.

print_analyses(Preds) :-
	(   get_old_pred_ref(Spec, Preds, Ref),
	    get_pred_success(Ref, Preds, BU),
	    get_pred_call(Ref, Preds, TD),

	    write(Spec),
	    write(' (call):  '),
	    anal_print(TD),
	    nl,
	    write(Spec),
	    write(' (exit):  '),
	    anal_print(BU),
	    nl,
	    write(Spec),
	    write(' (full):  '),
	    anal_meet(TD, BU, Full),
	    anal_print(Full),
	    nl,

% 	    % temporary code for comparing bu ans results to old analyzer.
% 	    Spec=_:Spec1,
% 	    write(Spec1),
% 	    write(' (exit):  '),
% 	    anal_print(BU),
% 	    nl,
% 	    nl,
% 	    write(Spec1),
% 	    write(' (call):  '),
% 	    anal_print(BU),
%	    nl,
%	    nl,

	    nl,
	    fail
	;   true
	).


/*****************************************************************

			     Predicate Statistics

Here we collect information on the number of predicates, clauses, arguments,
and variables appearing in a file.

*****************************************************************/


%  print_pred_stats(+Preds)
%  Print out interesting statistics about the predicates stored in Preds, a
%  predicate store.

print_pred_stats(Preds) :-
	pred_stats(Preds, PredCount, ClauseCount, TotalArity, MaxArity,
		   TotalVars, MaxVars),
	AvgArity is TotalArity / PredCount,
	AvgVars is TotalVars / ClauseCount,
	format('    Total predicates            :  ~w~n', [PredCount]),
	format('    Total clauses               :  ~w~n', [ClauseCount]),
	format('    Total arity                 :  ~w~n', [TotalArity]),
	format('    Max arity                   :  ~w~n', [MaxArity]),
	format('    Average arity               :  ~w~n', [AvgArity]),
	format('    Total distinct variables    :  ~w~n', [TotalVars]),
    	format('    Maximum variables per clause:  ~w~n', [MaxVars]),
    	format('    Average variables per clause:  ~w~n', [AvgVars]).


%  pred_stats(+Preds, -Predcount, -ClauseCount, -TotalArity, -Maxarity,
%	      -TotalVars, -Maxvars)
%  pred_stats(+Preds, +Cursor, +ClauseCount0, -ClauseCount,
%	      +TotalArity0, -TotalArity, +Maxarity0, +Maxarity,
%	      +TotalVars0, -TotalVars, +Maxvars0, +Maxvars)
%  ClauseCount is the number of clauses in Preds, a predicate store.
%  TotalArity is the sum of the arities of all the preds on Preds.  Maxarity
%  is the greatest arity on Predicates.  TotalVars is the total number of vars
%  in all the clause on preds.  Maxvars is the greatest number of variables in
%  any clause for any predicate on Preds.

pred_stats(Preds, PredCount, ClauseCount, TotalArity, MaxArity, TotalVars,
		MaxVars) :-
	predstore_size(Preds, PredCount),
	predstore_first(Preds, Cursor),
	pred_stats(Preds, Cursor, 0, ClauseCount, 0, TotalArity, 0,
		   MaxArity, 0, TotalVars, 0, MaxVars).


pred_stats(Preds, Cursor0, CC0, CC, TA0, TA, MA0, MA, TV0, TV, MV0, MV) :-
	(   predstore_next(Preds, Cursor0, Predref, Cursor1) ->
		get_pred_code(Predref, Preds, Code),
		get_old_pred_ref(_:_/A, Preds, Predref),
		TA1 is TA0 + A,
		max(MA0, A, MA1),
		clause_count(Code, CC0, CC1),
		code_vars(Code, A, TV0, TV1, MV0, MV1),
		(   TV1 - TV0 >= 64 ->
			get_old_pred_ref(Spec, Preds, Predref),
			format('! Predicate ~w has too many variables~n',
			       [Spec])
		;   true
		),
		pred_stats(Preds, Cursor1, CC1, CC, TA1, TA, MA1, MA, TV1, TV,
			   MV1, MV)
	;   CC = CC0,
	    TA = TA0,
	    MA = MA0,
	    TV = TV0,
	    MV = MV0
	).


%  clause_count(+Code, +Count0, -Count)
%  Count is Count0 + the number of clauses in code.

clause_count(undefined, Count, Count).
clause_count(foreign(_), Count, Count).
clause_count(disj(Clauses), Count0, Count) :-
	length(Clauses, Count1),
	Count is Count0 + Count1.
% We handle other cases just in case we get around to putting in some code
% factoring or something.
clause_count(conj(L), Count0, Count) :-
	clause_count(L, Count0, Count).		% too lazy to add new pred
clause_count([], Count, Count).
clause_count([G|Gs], Count0, Count) :-		% This isn't really right
	clause_count(G, Count0, Count1),
	clause_count(Gs, Count1, Count).
clause_count(equal(_,_,_,_,_), Count, Count).
clause_count(eval(_,_,_,_,_), Count, Count).
clause_count(builtin(_,_,_), Count, Count).
clause_count(call(_,_,_,_), Count, Count).
clause_count(!, Count, Count).
clause_count(if_then_else(_,_,_), Count, Count).



%  code_vars(+Code, +Arity, +TotalVars0, -TotalVars, +Maxvars0, +Maxvar)
%  TotalVars is the total the number of distinct variables in each clause on
%  Code plus TotalVars0.  Maxvars is the greatest number of variables in any
%  clause on Code, or Maxvars0, whichever is greater.  Arity is the arity of
%  the predicate in question.

code_vars(undefined, _, TV, TV, MV, MV).
code_vars(foreign(_), _, TV, TV, MV, MV).
code_vars(disj(Code), Arity, TV0, TV, MV0, MV) :-
	(   Code = [Code1|_] ->
		code_vars(Code1, Arity, TV0, TV, MV0, MV)
	;   TV = TV0,
	    MV = MV0
	).
code_vars(conj(Code), Arity, TV0, TV, MV0, MV) :-
	(   Code = [Code1|_] ->
		code_vars(Code1, Arity, TV0, TV, MV0, MV)
	;   TV = TV0,
	    MV = MV0
	).
code_vars(equal(_,_,_,_,V0), _, TV0, TV, MV0, MV) :-
	V is V0 - 1,
	TV is TV0 + V,
	max(MV0, V, MV).
code_vars(eval(_,_,_,_,V0), _, TV0, TV, MV0, MV) :-
	V is V0 - 1,
	TV is TV0 + V,
	max(MV0, V, MV).
code_vars(builtin(_,_,V0), _, TV0, TV, MV0, MV) :-
	V is V0 - 1,
	TV is TV0 + V,
	max(MV0, V, MV).
code_vars(call(_,_,_,V0), _, TV0, TV, MV0, MV) :-
	V is V0 - 1,
	TV is TV0 + V,
	max(MV0, V, MV).
code_vars(!, _, TV, TV, MV, MV).		% This isn't right!
code_vars(if_then_else(G1,_G2,_G3), Arity, TV0, TV, MV0, MV) :-
	code_vars(G1, Arity, TV0, TV, MV0, MV).


sum_of_lengths([], S, S).
sum_of_lengths([L|Ls], S0, S) :-
	length(L, Len),
	S1 is S0 + Len,
	sum_of_lengths(Ls, S1, S).
%  file    : boolfn
%  Authors : Peter Schachte
%  Purpose : Manipulation of (positive or definite) boolean functions
%
%				Abstract
%
%  This file contains the code to manipulate boolean functions.  Actually, the
%  bulk of the code for handling boolean functions is written in C and loaded
%  by this file, this file really only provides a Prolog interface to the C
%  code.

:- module(boolfn, [
	init_rep/0,
	conclude_rep/0,
	variable_rep/2,
	bool_meet/3,
	bool_join/3,
	bool_rep_conj/2,
	bool_meet_restricted/4,
	bool_rename/3,
	bool_reverse_rename/3,
	bool_implies/3,
	iff_conj/3,
	bool_iff/3,
	bool_abstract_unify/5,
	bool_abstract_exit/5,
	bool_restrict/3,
	bool_print/1,
	print_bryant/1,
	milli_time/1
   ]).

/**********************************************************************

			  Foreign Interface

**********************************************************************/





%  bool_meet(+G1, +G2, -G)
%  G is the meet of G1 and G2, which are either '1' or '0' or an integer
%  which is the address of a C data structure representing the boolean
%  function.

% bool_meet(1, G2, G) :-
% 	!,
% 	G = G2.
% bool_meet(0, _, G) :-
% 	!,
% 	G = 0.
% bool_meet(G1, G2, G) :-
% 	!,
% 	bool_meet_1(G2, G1, G).
% 
% bool_meet_1(1, G1, G) :-
% 	!,
% 	G = G1.
% bool_meet_1(0, _, G) :-
% 	!,
% 	G = 0.
% bool_meet_1(G2, G1, G) :-
% 	!,
% 	glb(G1, G2, G).

bool_meet(G1, G2, G) :-
 	glb(G1, G2, G).

%  bool_meet_restricted(+G1, +G2, +Thresh, -G)
%  G is the meet of G1 and G2, restricted to variables numbered lower than
%  Thresh.  G1 and G2 are either '1' or '0' or an integer
%  which is the address of a C data structure representing the boolean
%  function.

bool_meet_restricted(G1, G2, Thresh, G) :-
	Thresh1 is Thresh - 1,
	restricted_glb(Thresh1, G1, G2, G).


%  bool_join(+G1, +G2, -G)
%  G is the join of G1 and G2, which are either '1' or '0' or an integer
%  which is the address of a C data structure representing the boolean
%  function.

% bool_join(0, G2, G) :-
% 	!,
% 	G = G2.
% bool_join(1, _, G) :-
% 	!,
% 	G = 1.
% bool_join(G1, G2, G) :-
% 	!,
% 	bool_join_1(G2, G1, G).
% 
% bool_join_1(0, G1, G) :-
% 	!,
% 	G = G1.
% bool_join_1(1, _, G) :-
% 	!,
% 	G = 1.
% bool_join_1(G2, G1, G) :-
% 	!,
% 	lub(G1, G2, G).

bool_join(G1, G2, G) :-
 	lub(G1, G2, G).

%  bool_rep_conj(+Vars, -Fn)
%  Fn is the boolean function which is the conjunction of all the variables on
%  the list Vars.

bool_rep_conj([], 1).
bool_rep_conj([V|Vs], Fn) :-
	variable_rep(V, Fn0),
	bool_rep_conj1(Vs, Fn0, Fn).

bool_rep_conj1([], Fn, Fn).
bool_rep_conj1([V|Vs], Fn0, Fn) :-
	variable_rep(V, Fn1),
	glb(Fn0, Fn1, Fn2),
	bool_rep_conj1(Vs, Fn2, Fn).


%  bool_rename(+G0, +Mapping, -G)
%  G represents the same boolean function as G0, except that every variable *I*
%  of G0, where *I* ranges from 1 through N, have been renamed to the values of
%  argument *I* of term Mapping.


% bool_rename(1, _, 1) :-
% 	!.
% bool_rename(0, _, 0) :-
% 	!.
bool_rename(G0, Map, G) :-
	(   atom(Map) ->
		G = G0
	;  % format('renaming ~p by ~w~n', [G0,Map]),
	    rename_term(G0, Map, G)
	).




%  bool_reverse_rename(+Mapping, +G0, -G)
%  G represents the same boolean function as G0, except that every variable *I*
%  of G0, where *I* ranges from 1 through N, have been renamed to the values of
%  argument *I* of term Mapping.  If N is not supplied, the arity of Mapping
%  is used.
%
%  This implementation could use some work.  In particular, the metacall is
%  rather unfortunate.  I couldn't think of another way to do it without
%  having 16 separate cases.
 
% bool_reverse_rename(1, _, 1) :-
% 	!.
% bool_reverse_rename(0, _, 0) :-
% 	!.
bool_reverse_rename(G0, Map, G) :-
	(   atom(Map) ->
		G = 1
	;   %  format('reverse renaming ~p by ~w~n', [G0,Map]),
	    reverse_rename_term(G0, Map, G)
	).


%  iff_conj(+V, +Vars, -G)
%  G is the boolean function \( V <-> \bigwedge Vars \), where V is a variable
%  number and Vars is a list of them.
% 
% iff_conj(V, Vars, G) :-
% 	length(Vars, N),
% 	(   N =:= 0 ->
% 		variable_rep(V, G)
% 	;   append(Vars, Tail, [V1, V2, V3, V4, V5, V6, V7, V8, V9, V10,
% 				V11, V12, V13, V14, V15, V16]) ->
% 		% Vars isn't too long, use efficient C implementation
% 		uniform_list(Tail, 0),
% 		iff_conj1(V, N, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10,
% 			  V11, V12, V13, V14, V15, V16, G)
% 	;   Vars = [V1|Vars1],
% 	    variable_rep(V, G1),
% 	    variable_rep(V1, G2),
% 	    var_conj(Vars1, G2, G3),
% 	    bool_iff(G1, G3, G)
% 	).
% 
% 
% 
% %  uniform_list(+L, -E)
% %  L is a list each element of which is E
% 
% uniform_list([], _).
% uniform_list([E|L], E) :-
% 	uniform_list(L, E).
% 
% 
% 
% var_conj([], G, G).
% var_conj([V|Vs], G0, G) :-
% 	variable_rep(V, G1),
% 	glb(G0, G1, G2),
% 	var_conj(Vs, G2, G).


%  bool_iff(+X, +Y, -Z)
%  Z is the representation of X <-> Y.

bool_iff(X, Y, Z) :-
	bool_implies(X, Y, XimpY),
	bool_implies(Y, X, YimpX),
	glb(XimpY, YimpX, Z).

	
%  bool_restrict(+Graph0, +Threshold, -Graph)
%  Graph is Graph0 with variables greater than or equal to Threshold
%  restricted away.

% bool_restrict(1, _, 1) :-
% 	!.
% bool_restrict(0, _, 0) :-
% 	!.
bool_restrict(Graph0, Threshhold, Graph) :-
	Threshhold1 is Threshhold - 1,
	restrict_threshold(Threshhold1, Graph0, Graph).



%  constant_graph(+Fn).
%  Fn is a constant boolean function graph (i.e., '1' or '0').

constant_graph(1).
constant_graph(0).


%  bool_print(+Graph)
%  Print out boolean function Graph.

% bool_print(1) :-
% 	!,
% 	write(true).
% bool_print(0) :-
% 	!,
% 	write(false).
bool_print(Graph) :-
	print_out(Graph).


:- multifile user_portray/1.
user_portray(X) :-
	integer(X),				% A really ugly hack:
	( X > 65536 ; X < -65536),		% assume any number > 64K and
	0 =:= X /\ 3,				% divisible by 4 is a boolfn
	!,
	print_out(X).


print_bryant(X) :-
	print_bryant(X, 0).

print_bryant(0, N) :-
	!,
	tab(N),
	write(false),
	nl.
print_bryant(1, N) :-
	!,
	tab(N),
	write(true),
	nl.
print_bryant(Addr, N) :-
	!,
	tab(N),
	N1 is N+4,
	V is integer_at(Addr),
	Tr is address_at(Addr+4),
	Fa is address_at(Addr+8),
	write(V),
	nl,
	print_bryant(Tr, N1),
	print_bryant(Fa, N1).


%  file    : bottomup
%  Authors : Peter Schachte
%  Purpose : The bottom-up phase of the groundness analyzer
%
%				Abstract
%
%  The bottom-up part of our global analyzer.  We analyze bottom-up, one
%  strongly-connected component at a time, so that at the time we analyze
%  each scc, we know all there is to know about all the predicates it calls,
%  except for those in that scc.  We iterate analyzing the predicates in the
%  scc until we reach a fixpoint.


:- module(bottomup, [
	analyze_bottom_up/3
   ]).


%:- use_module(prep).
%:- use_module(analysis).
%:- use_module(predstore).



/*****************************************************************
			   Analyzing SCCs Bottom-Up

The file has been read and the strongly-connected components (SCCs) have been
found; now we perform a bottom-up analysis of the file.

For each strongly-connected component, we first fully analyze all calls to
predicates defined *outside* that SCC, and collect all calls to predicates
defined *in* that SCC.  This is accomplished by topologically sorting the SCCs
(this is done as we collect the SCCs), and then analyzing the SCCs in order.
This allows us to analyze the *fixed* part of a predicate, comprising
everything but recursive (or mutually recursive) calls, outside the fixpoint
iteration.  The preparation of a SCC is done by prepare_scc/3, defined in
prep.pl.  After that, we perform the fixpoint computation on only the
*variable* part of the clause, comprising the recursive calls, yielding the
final (approximate) analysis of the predicates in that SCC.

Our approach, computing the glb of all the fixed parts and the meeting this
with the variable parts, requires that our meet operation be commutative.
And, in fact, out meet must be associative for our bottom-up approach to work
at all, otherwise we would not be able to compute an analysis for a predicate
independent of its call pattern.  We could remove the requirement for a
commutative meet with a fairly small performance penalty, by keeping a
separate glb for each contiguous group of calls in a clause.  So the fixpoint
iteration would often require an extra glb or two for each recursive clause.
There's no way to remove the requirement that meet be associative without
tossing the whole two-phase approach.

*****************************************************************/

%  analyze_bottom_up(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed bottom-up for success
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, topologically sorted in bottom-up order.  Each SCC is
%  represented as a list of predicate references.

analyze_bottom_up([], Preds, Preds).
analyze_bottom_up([SCC|SCCs], Preds0, Preds) :-
	prepare_scc(SCC, Preds0, Preds1),
	analyze_to_fixpoint(SCC, Preds1, Preds2),
	analyze_bottom_up(SCCs, Preds2, Preds).



/*======================================================================

			    The Fixpoint Iteration

Here we perform the fixpoint iteration for an SCC.  We do this by meeting the
fixed part of each clause and the current best approximation of the variable
parts.  We join these results for each clause with a non-empty variable part
along with the previous approximation to compute the next approximation for
that predicate.  If the previous and next approximations for each predicate in
the SCC are the same, then we're done, otherwise we must try again.

  ======================================================================*/


%  analyze_to_fixpoint(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed bottom-up for success
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.

analyze_to_fixpoint(SCC, Preds0, Preds) :-
	analyze_once(SCC, Preds0, Preds1, false, Changed),
	(   Changed == true ->
		analyze_to_fixpoint(SCC, Preds1, Preds)
	;   Preds = Preds1
	).


%  analyze_once(+SCC, +Preds0, -Preds, +Changed0, -Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed once bottom-up for success
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.


analyze_once([], Preds, Preds, Changed, Changed).
analyze_once([Pred|SCC], Preds0, Preds, Changed0, Changed) :-
	get_pred_prep(Pred, Preds0, Prep),
	(   Prep = fixed(_) ->			% nonrecursive:  do no more!
		Changed = Changed0,
		Preds = Preds0
	;   anal_top(Top),
	    analyze_prep(Prep, Preds0, Top, Anal1),
	    (   Changed0 == true ->
		    put_pred_success(Pred, Anal1, Preds0, Preds1),
		    Changed1 = true		% don't compare graphs if
						% we've already found a change
	    ;   get_pred_success(Pred, Preds0, Anal0),
		Anal0 = Anal1 ->	% are graphs are the same?
		    Preds1 = Preds0,		% yes:  no need to update Preds
		    Changed1 = false
	    ;	put_pred_success(Pred, Anal1, Preds0, Preds1),
		Changed1 = true
	    ),
	    analyze_once(SCC, Preds1, Preds, Changed1, Changed)
	).


%  analyze_prep(+Prep, +Preds, +Anal0, -Anal)
%  Anal is the glb of Anal0 and an approximate analysis of the predicate whose
%  prep term is Prep.  Preds is a predicate store containing the current
%  approximation of all predicates referred to by Prep.  Note that Prep will
%  not be 'fixed'(_), as we have dealt with that case earlier.

analyze_prep(conj_prep(Fixed,Var), Preds, Anal0, Anal) :-
	anal_meet(Anal0, Fixed, Anal1),
	analyze_conj(Var, Preds, Anal1, Anal).
analyze_prep(disj_prep(Fixed0,Var,Restriction), Preds, Anal0, Anal) :-
	analyze_disj(Var, Preds, Fixed0, Anal1),
	anal_meet(Anal0, Anal1, Restriction, Anal).
analyze_prep(call_prep(Pred,Call,_,Restriction), Preds, Anal0, Anal) :-
	analyze_call(Pred, Preds, Call, Restriction, Anal0, Anal).


%  analyze_conj(+Preps, +Preds, +Anal0, -Anal)
%  Anal is the glb of Anal0 and the analysis of Preps, a list of prep terms.
%  Preds is a predicate store containing the current approximation of all
%  predicates referred to by Preps.

analyze_conj([], _, Anal, Anal).
analyze_conj([Prep|Preps], Preds, Anal0, Anal) :-
	analyze_prep(Prep, Preds, Anal0, Anal1),
	analyze_conj(Preps, Preds, Anal1, Anal).


%  analyze_disj(+Preps, +Preds, +Anal0, -Anal)
%  Anal is the lub of Anal0 and the analyses of Preps, a list of prep terms.
%  Preds is a predicate store containing the current approximation of all
%  predicates referred to by Preps.

analyze_disj([], _, Anal, Anal).
analyze_disj([Prep|Preps], Preds, Anal0, Anal) :-
	anal_top(Top),
	analyze_prep(Prep, Preds, Top, Anal1),
	anal_join(Anal0, Anal1, Anal2),
	analyze_disj(Preps, Preds, Anal2, Anal).

%  file    : builtins
%  Authors : Peter Schachte
%  Purpose : Groundness analysis of Prolog buiiltins
%
%				   Abstract
%
%  This file lists Prolog's builtin predicates.

:- module(builtins, [
	builtin/2
   ]).


%  builtin(-Name, -Arity)
%  The predicate Name/Arity is a Prolog builtin.  Note that if new
%  predicaes are added here, they should also be added to
%  analysis:builtin_analysis/2.

builtin(!, 0).
builtin(true, 0).
builtin(otherwise, 0).
builtin(fail, 0).
builtin(false, 0).
builtin(abort, 0).
builtin(raise_exception, 1).
builtin(halt, 0).
builtin(halt, 1).
builtin(call, 1).
builtin(unix, 1).
builtin(repeat, 0).
builtin(compare, 3).
builtin((@<), 2).
builtin((@=<), 2).
builtin((==), 2).
builtin((@>=), 2).
builtin((@>), 2).
builtin((\==), 2).
builtin((<), 2).
builtin((=<), 2).
builtin((=:=), 2).
builtin((>=), 2).
builtin((>), 2).
builtin((=\=), 2).
builtin((is), 2).
builtin((=..), 2).
builtin(atom, 1).
builtin(atomic, 1).
builtin(callable, 1).
builtin(compound, 1).
builtin(db_reference, 1).
builtin(float, 1).
builtin(ground, 1).
builtin(integer, 1).
builtin(nonvar, 1).
builtin(number, 1).
builtin(simple, 1).
builtin(var, 1).
builtin(functor, 3).
builtin(arg, 3).
builtin(name, 2).
builtin(atom_chars, 2).
builtin(number_chars, 2).
builtin(numbervars, 3).
builtin(hash_term, 2).
builtin(subsumes_chk, 2).
builtin(copy_term, 2).
builtin(length, 2).
builtin(sort, 2).
builtin(keysort, 2).
builtin('C', 3).
builtin(statistics, 0).
builtin(statistics, 2).
builtin(see, 1).
builtin(seeing, 1).
builtin(seen, 0).
builtin(tell, 1).
builtin(telling, 1).
builtin(told, 0).
builtin(open, 3).
builtin(open, 4).
builtin(close, 1).
builtin(current_input, 1).
builtin(current_output, 1).
builtin(set_input, 1).
builtin(set_output, 1).
builtin(current_stream, 3).
builtin(stream_code, 2).
builtin(absolute_file_name, 2).
builtin(absolute_file_name, 3).
builtin(current_op, 3).
builtin(op, 3).
builtin(read, 1).
builtin(read, 2).
builtin(write, 1).
builtin(write, 2).
builtin(write_canonical, 1).
builtin(write_canonical, 2).
builtin(print, 1).
builtin(print, 2).
builtin(writeq, 1).
builtin(writeq, 2).
builtin(display,1).
builtin(display,2).
builtin(format, 2).
builtin(format, 3).
builtin(get, 1).
builtin(get, 2).
builtin(get0, 1).
builtin(get0, 2).
builtin(nl, 0).
builtin(nl, 1).
builtin(peek_char, 1).
builtin(peek_char, 2).
builtin(put, 1).
builtin(put, 2).
builtin(skip, 1).
builtin(skip, 2).
builtin(skip_line, 0).
builtin(skip_line, 1).
builtin(tab, 1).
builtin(tab, 2).
builtin(ttyget, 1).
builtin(ttyget0, 1).
builtin(ttynl, 0).
builtin(ttyput, 1).
builtin(ttyskip, 1).
builtin(ttytab, 1).
builtin(assert, 1).
builtin(assert, 2).
builtin(asserta, 1).
builtin(asserta, 2).
builtin(assertz, 1).
builtin(assertz, 2).
builtin(clause, 2).
builtin(clause, 3).
builtin(current_key, 2).
builtin(erase, 1).
builtin(instance, 2).
builtin(recorda, 3).
builtin(recorded, 3).
builtin(recordz, 3).
builtin(retract, 1).
builtin(retractall, 1).
builtin(expand_term, 2).

% Nonstandard (or non-Quintus) builtins added for specific tests
builtin(inc,2).
builtin(dec,2).
builtin(real, 1).
builtin(not,1).
%  File   : calls
%  Authors: Peter Schachte
%  Purpose: Load a Prolog source file to be analyzed
%
%				Abstract
%
%  This program reads a Prolog source file returning a list of (the Clark
%  completions of) the predicates in the file, and all the files it loads.
%  This program also pays some attention to modules, although at the moment it
%  is not complete.

:- module(calls, [
	file_contents/2,
	simplify_goal/7
   ]).

%:- ensure_loaded(builtins).

/************************************************************************

			       Loading a File

This code is responsible for reading a file and all the files it
loads, returning a collection of all the predicates in all these
files, with the code in a form suitable for analysis.  This code is
also responsible for handling other file-related issues in the code,
including modules and multi-file and discontiguous predicates.

The collection of predicates returned by this code is represented as a
mapping from *Module:Name/Arity* to a predicate descriptor.  A predicate
descriptor is one of
++enumerate
    'import'(*Number,Predspec*), where *Predspec* names another predicate,
		whose predicate number is *Number*, which this predicate is an
		alias for; or
    'pred'(*Number, Num2, Props, Code*), where the arguments are 
++description
   *Number*	is the predicate's number
   *Num2*	is a temporary number used in computing strongly-connected
		components
   *Props*	is a bitset specifying the predicate's properties
   *Code*	is a goal specification containing all the code for
		the predicate.
--description
--enumerate
*Code* represents the Clark completion of that predicate (without
inequality axioms).  All predicate arguments are variables
(non-variable arguments are replaced by variables, with explicit
unifications added to the body).  All variables are numbered, with the
head arguments in particular numbered from 1 through *Arity*; this
means that the "head" of the Clark completion can be ignored
altogether.

*Code* is a goal specification, which is one of:
++description
    'conj'(*Gs*)
	specifying a conjunction of goals, where *Gs* is a list of goal specs.
    'equal'(*V,T,Vs,A,R*)
	specifying an explicit or implicit unification, where *V* is a
	variable number, *T* is a term description, *Vs* is a list of the
	variable numbers appearing in *T*, *A* is an analysis term containing
	analysis information for the point of the call (before the call), and
	*R* is the smallest variable number that does not appear later in the
	clause;
    'eval'(*V,E,Vs,A,R*)
	 specifying an explicit or implicit numeric calculation, where *V* is
	a variable number, *E* is a term description of the expression to
	evaluate, *Vs* is a list of the variable numbers appearing in *E*, and
	*A* and *R* are as for 'equal'/5;
    'builtin'(*T,A,R*)
	specifying a call to a Prolog builtin, where *T* is a term
	representing the goal and *T*'s arguments are all distinct variable
	numbers, and *A* and *R* are as for 'equal'/5;
    'call'(*P,T,A,R*)
	specifying a call to a user predicate, where *P* is the predicate
	number and *T*, *A*, and *R* are as for 'builtin'/3;
    '!'	(ordinary cut, cuts to outermost disjunction);
    'disj'(*Gs*)
	specifying a disjunction of goals, where *Gs* is a list of goal specs.
    'if_then_else'(*G1,G2,G3*)
	representing a Prolog if->then;else construct, where *G1*, *G2*, and
	*G3* are goal specs.
--description
We store analysis information for unifications, evaluations, and
builtin calls because it may prove useful in compiling these
operations.

A term description is one of:
++enumerate
    1.  'var'(*N*), where *N* is a variable number;
    2.  'atom'(*C*), where *C* is an atom;
    3.  'integer'(*C*), where *C* is an integer;
    4.  'float'(*C*), where *C* is a float;
    5.  'compound'(*F, A, Args*), where *F* is ther term's principal functor,
	*A* is its arity, and *Args* is a list of term descriptions.
--enumerate

This is a ground representation, with variables represented as
numbers.  Note that the *R* argument in each of these terms specifies
a threshold; variables numbered at or above that threshold do not
appear later in the predicate body, nor in the head.  In order for
this to be possible, we must number first the predicate arguments, and
then the body arguments, beginning with the *last* goal and working
toward the beginning.  Variable numbers appearing in a disjunction but not
outside it may be shared among the disjuncts, though they do not
represent the same variable.

TODO: Note that at present, we don't use the 'eval'/4 goal specification
properly.  The idea here is to simplify arithmetic operations by removing
unification from 'is'/2 (generating an explicit unification after the
evaluation when argument 1 of 'is' is not a first occurrance of a variable)
and by removing computation from the arithmetic comparisons (by generating one
or two explicit computations first when either argument of an arithmetic
comparison is not a variable).

************************************************************************/

/************************************************************************

		   Thoughts on Incremental Analysis

Since predicate numbers are included in the representation of calls to
user defined predicates, we don't want to have to renumber predicates
due to the addition, removal, or modification of predicates.  This
means that we can't use a predicate's topological sort order as a
predicate number.  Therefore we might as well just assign a predicate
number when we see the first clause for or call to the predicate.

************************************************************************/


% :- ensure_loaded(mymaps).
% :- ensure_loaded(predstore).
% :- ensure_loaded(library(basics)).


%  file_contents(+File, -Preds)
%  file_contents(+File, +Relfile, -Absfiles, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds)
%  Preds is a predicate store containing all the code from File and all the
%  files it depends upon, plus whatever Preds0 contained.  Files0 is a mapping
%  from file names to either 'loading', indicating that that file is currently
%  being processed, or a list of the modules defined in that file.  Files is a
%  similar mapping after File has been processed.  Mods is a mapping from
%  module name to the list of that module's exported predicates; Mods0 is the
%  same mapping before processing File.  Relfile is a directory or file name
%  relative to which relative file names will be taken.  File is the name of
%  the file or files to be loaded, possibly relative file names; Absfiles is a
%  list of the absolute file name for (the files on list) File.

file_contents(File, Preds) :-
	map_empty(Mods0),
	map_empty(Files0),
	empty_predstore(Preds0),
	file_contents(File, '.', _, Files0, _, Mods0, _, Preds0, Preds1),
	report_undefined_preds(Preds1, Preds).

file_contents([], _, [], Files, Files, Mods, Mods, Preds, Preds) :-
	!.
file_contents([F|Fs], Rel, List, Files0, Files, Mods0, Mods, Preds0, Preds) :- 
	!,
	file_contents(F, Rel, L0, Files0, Files1, Mods0, Mods1, Preds0, Preds1),
	append(L0, L1, List),
	file_contents(Fs, Rel, L1, Files1, Files, Mods1, Mods, Preds1, Preds).
file_contents(F, Rel, [File], Files0, Files, Mods0, Mods, Preds0, Preds) :-
	source_file_name(F, Rel, File),
	(   map_fetch(File, Files0, FMods) ->	% already process{ed,ing} File
		Mods = Mods0,
		Preds = Preds0,
		seen_file_contents(FMods, File, Files0, Files)
	;   seeing(Oldfile),
	    see(File),
	    map_store(File, loading, Files0, Files1),
	    process_stream(File, user, Files1, Files2, Mods0, Mods, Preds0,
			 Preds, none, _, FileMods),
	    map_store(File, FileMods, Files2, Files),
	    seen,
	    see(Oldfile)
	).


%  seen_file_contents(+FMods, +File, +Files0, -Files)
%  FMods is the atom 'loading' or a list of the modules defined in file File
%  (not including files loaded by File).  Other args are as above.

seen_file_contents(loading, _File, Files0, Files) :-
	!,
	% TODO:  We don't handle circular dependencies yet.  We'll have to
	% open the file and just scan it for the modules it defines, and then
	% make File map to that list in Files.  For now we do the wrong thing.
	Files = Files0.
seen_file_contents(_, _, Files, Files).


%  process_stream(+File, +Defmod, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds, +Prevpred, -Clauses, -FileMods)
%  Defmod is the current default module.  File is the file we are currently
%  reading.  Other arguments are as above.

process_stream(File, Defmod, Files0, Files, Mods0, Mods, Preds0, Preds,
		Prevpred, Clauses, Fmods) :-
	read(Clause0),
	expand_term(Clause0, Clause),
	(   var(Clause) ->
		write('Warning:  unbound clause in '),
		write(File),
		nl,
		process_stream(File, Defmod, Files0, Files, Mods0, Mods,
			     Preds0, Preds, Prevpred, Clauses,
			     Fmods)
	;   process_term(Clause, File, Defmod, Files0, Files, Mods0,
			 Mods, Preds0, Preds, Prevpred, Clauses,
			 Fmods)
	).


%  process_term(+Clause, +File, +Defmod, +Files0, -Files, +Mods0,
%		  -Mods, +Preds0, -Preds, +Prevpred, -Clauses,
%		  -FileMods)
%  Same as process_stream, except that Clause is the clause that has just been
%  read.

process_term(end_of_file, _, _, Files0, Files, Mods0, Mods, Preds0, Preds, _, Clauses, Fmods) :-
	!,
	Files = Files0,
	Mods = Mods0,
	Preds = Preds0,
	Clauses = [],
	Fmods = [].
process_term((:- Directive), File, Defmod, Files0, Files, Mods0, Mods, Preds0,
	     Preds, Prevpred, Clauses, Fmods) :-
	!,
	process_directive(Directive, File, Defmod, Defmod1, Files0, Files1,
			  Mods0, Mods1, Preds0, Preds1, Fmods, Fmods1),
	process_stream(File, Defmod1, Files1, Files, Mods1, Mods, Preds1,
		       Preds, Prevpred, Clauses, Fmods1).
process_term(foreign(_,_,Spec), File, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, Prevpred, Clauses, Fmods) :-
	!,
	process_foreign_decl(Spec, Defmod, Preds0, Preds1),
	process_stream(File, Defmod, Files0, Files, Mods0, Mods, Preds1,
		       Preds, Prevpred, Clauses, Fmods).
process_term(foreign(_,Spec), File, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, Prevpred, Clauses, Fmods) :-
	!,
	process_foreign_decl(Spec, Defmod, Preds0, Preds1),
	process_stream(File, Defmod, Files0, Files, Mods0, Mods, Preds1,
		       Preds, Prevpred, Clauses, Fmods).
process_term(Clause, File, Defmod, Files0, Files, Mods0, Mods, Preds0, Preds,
		Prevpred, PrevClauses, Fmods):-
	simplify_clause(Clause, Defmod, Preds0, Preds1, Predspec, Simple),
	(   Prevpred == Predspec ->
		PrevClauses = [Simple|Simples],
		Preds3 = Preds1
	;   get_pred_ref(Predspec, Preds1, Predref, Preds2) ->
		get_pred_code(Predref, Preds2, Code),
		PrevClauses = [],
		(   Code == undefined ->
			Clauses0 = []
		;   Code = disj(Clauses0)
		    % continued pred:  must be discontiguous or multifile
		    % TODO:  we really should check!
		),
		append(Clauses0, [Simple|Simples], Clauses),
						% add new clauses after old
		put_pred_code(Predref, disj(Clauses), Preds2, Preds3)
	),
	process_stream(File, Defmod, Files0, Files, Mods0, Mods, Preds3,
		       Preds, Predspec, Simples, Fmods).


%  process_directive(+Directive, +Relfile, +Defmod0, -Defmod, +Files0, -Files,
%		     +Mods0, -Mods, +Preds0, -Preds, -Fmods,
%		     +Fmods0)
%  After handling directive :- Directive, Defmod is the default module;
%  Defmod0 was the default module before processing Directive.  Fmods is a
%  list of the modules defined by the file loaded by this directive, if any,
%  followed by Fmods0.  Relfile is the file that we are currently loading.
%  Other args are as above.

process_directive(module(Mod,Exports), _, _, Mod, Files, Files, Mods0,
		Mods, Preds, Preds, [Mod|Fmods], Fmods) :-
	!,
	handle_exports(Exports, Mod, Mods0, Mods).
process_directive(dynamic(Predspec), _, Defmod, Defmod, Files, Files, Mods,
		Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, dynamic, Preds0, Preds).
process_directive(discontiguous(Predspec), _, Defmod, Defmod, Files, Files,
		Mods, Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, discontiguous, Preds0, Preds).
process_directive(multifile(Predspec), _, Defmod, Defmod, Files, Files,
		Mods, Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, multifile, Preds0, Preds).
process_directive(consult(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, Defmod).
process_directive(compile(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, Defmod).
process_directive(ensure_loaded(File), R, Defmod, Defmod, Files0, Files,
		Mods0, Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, Defmod).
process_directive(use_module(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, Defmod).
process_directive(use_module(File,Imports), R, Defmod, Defmod, Files0, Files,
		Mods0, Mods, Preds0, Preds, Fmods, Fmods) :-
	!,
	file_contents(File, R, List, Files0, Files, Mods0, Mods, Preds0,
		      Preds1),
	collect_file_exports(List, Files, Exp),
 	handle_imports(Imports, Defmod, Exp, Mods, Preds1, Preds).
process_directive((X,Y), R, Defmod0, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, Fmods, Fmods0) :-
	!,
	process_directive(X, R, Defmod0, Defmod1, Files0, Files1, Mods0, Mods1,
			  Preds0, Preds1, Fmods1, Fmods0),
	process_directive(Y, R, Defmod1, Defmod, Files1, Files, Mods1, Mods,
			  Preds1, Preds, Fmods, Fmods1).
process_directive(op(P,T,O), _, Defmod, Defmod, Files, Files, Mods, Mods,
		Preds, Preds, Fmods, Fmods) :-
	!,
	op(P, T, O).
process_directive(_, _, Defmod, Defmod, Files, Files, Mods, Mods, Preds,
		Preds, Fmods, Fmods).	% couldn't be too important


%  handle_exports(+Exports, +Module, +Mods0, -Mods)
%  Export predicates on the list Exports from Module.  Mods0 and Mods
%  are as described above.

handle_exports(Exports0, Module, Mods0, Mods) :-
	(   map_fetch(Module, Mods0, OldExports) ->
		append(OldExports, Exports0, Exports1),
		sort(Exports1, Exports)		% remove duplicates
	;   Exports = Exports0
	),
	map_store(Module, Exports, Mods0, Mods).



%  import_file_contents(+File, +Relfile, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds, +Importmod)
%  Load File, whose name is taken relative to Relfile, and import all its
%  exported preds into module Importmod.  Other args are as above.

import_file_contents(File, Relfile, Files0, Files, Mods0, Mods, Preds0, Preds,
		Defmod) :-
	file_contents(File, Relfile, List, Files0, Files, Mods0, Mods, Preds0,
		      Preds1),
	collect_file_exports(List, Files, Exp),
 	handle_imports(all, Defmod, Exp, Mods, Preds1, Preds).
	

%  collect_file_exports(+List, +Files, -Exp)
%  Exp is a list of the modules defined by all the files on List.

collect_file_exports([], _, []).
collect_file_exports([F|Fs], Files, Exp) :-
	map_fetch(F, Files, Exp0),
	append(Exp0, Exp1, Exp),
	collect_file_exports(Fs, Files, Exp1).

%  handle_imports(+Imports, +ToMod, +Exp, +Mods, +Preds0, -Preds)
%  Preds is Preds0 extended to include import/1 terms as described above for
%  each of the predicates listed on Imports, importing them into ToMod.  Exp
%  is a list of the modules available from which to import these preds, and
%  Mods is a mapping from module name to the list of that module's exports.
%  As a (common) special case, if Imports is the atom 'all', then all
%  predicates exported from all the modules on Exps are imported.

handle_imports(all, ToMod, Exp, Mods, Preds0, Preds) :-
	collect_exports(Exp, Mods, Imports),
	handle_imports(Imports, ToMod, Exp, Mods, Preds0, Preds).
handle_imports([], _, _, _, Preds, Preds).
handle_imports([Spec|Specs], ToMod, Exp, Mods, Preds0, Preds) :-
	handle_1_import(Spec, ToMod, Exp, Mods, Preds0, Preds1),
	handle_imports(Specs, ToMod, Exp, Mods, Preds1, Preds).

handle_1_import(Spec, ToMod, Exp, Mods, Preds0, Preds) :-
	(   ( Spec = Mod:Name/Arity ; Spec = Name/Arity ) ->
		(   member(Mod, Exp),
		    map_fetch(Mod, Mods, Exports),
		    member(Name/Arity, Exports),
		    Fromspec = Mod:Name/Arity,
		    put_pred_alias(Fromspec, ToMod:Name/Arity, Preds0,
				   Preds) ->
			true
		;   format('! Can''t import predicate ~q~n', [Spec]),
		    % TODO:  should handle the case where we've already found
		    % calls to the imported pred and so assigned it a ref.  In
		    % this case we should make the code part of the new ref
		    % some sort of reference to the Fromspec pred.
		    Preds = Preds0
		)
	;   format('Invalid import spec:  ~q~n', [Spec]),
	    Preds = Preds0
	).


%  collect_exports(+Expmods, +Mods, -Exports)
%  Exports is a list of all the predicates exported by all the modules on the
%  list Expmods.  Mods is a mapping from module name to the list of the
%  predicates it exports.

collect_exports([], _, []).
collect_exports([M|Ms], Mods, Exports) :-
	map_fetch(M, Mods, Exports0),
	append(Exports0, Exports1, Exports),
	collect_exports(Ms, Mods, Exports1).



%  set_property(+Predspec, +Defmod, -Pred, +Property, +Preds0, -Preds).

set_property(Predspec, Defmod, Pred, Property, Preds0, Preds) :-
	get_pred_ref(Predspec, Defmod, Preds0, Pred, Preds1),
	get_pred_status(Pred, Preds1, Status0),
	property_mask(Property, Mask),
	Status is Status0 \/ Mask,
	put_pred_status(Pred, Status, Preds1, Preds).
	



%  source_file_name(+File0, +Relfile, -File)
%  File is the full and proper name of the Prolog source file File0.  Relfile
%  is a directory or file name relative to which relative files will be taken.
%  The current implementation ignores Relfile, because Quintus'
%  absolute_file_name/3 doesn't support file specs relative to another file.

source_file_name(File0, _Relfile, File) :-
	% This is very Quintus-specific
	absolute_file_name(File0,
			   [ignore_underscores(true),
			    file_type(prolog),
			    ignore_version(true),
			    access(read)],
			   File).



%  report_undefined_preds(+Preds0, -Preds)
%  print a report of all undefined predicates in predicate store Preds0.
%  Preds is the same as Preds0, except that perhaps some undefined preds have
%  been resolved.

report_undefined_preds(Preds0, Preds) :-
	predstore_first(Preds0, Cursor),
	report_undefined_preds_1(Cursor, Preds0, Preds).


report_undefined_preds_1(Cursor0, Preds0, Preds) :-
	(   predstore_next(Preds0, Cursor0, Predref, Cursor1) ->
		(   get_pred_code(Predref, Preds0, undefined) ->
			get_old_pred_ref(Spec, Preds0, Predref),
			Preds3 = Preds0,
			format('! Undefined predicate:  ~q~n', [Spec])
		;   Preds3 = Preds0
		),
		report_undefined_preds_1(Cursor1, Preds3, Preds)
	;   Preds = Preds0
	).
		


/****************************************************************

			     Clause Normalization

This code handles normalizing clauses.  Our representation of a predicate's
code is described above.  Note that it is a ground representation, with
variables represented by integers.

We form the Clark completion of each predicate one clause at a time by
moving head unification into the body of each clause, and numbering the
argument variables from 1 through the predicate's arity.

****************************************************************/

%  simplify_clause(+Clause, +Defmod, +Preds0, -Preds, -Predspec,
%		-Simples)
%  Simples is a simplified version of clause Clause, with its head removed,
%  its arguments all turned into '$ VAR $'/1 terms numbered as described
%  above, and all its other variables numbered from Arity + 1 up.  Defmod is
%  the default module in which Clause appears, and Predspec is a predicate
%  specification of the predicate defined by Clause.  Other args are as above.

simplify_clause(Module:Clause, _, Preds0, Preds, Predspec,
		Simples) :- 
	!,
	simplify_clause(Clause, Module, Preds0, Preds, Predspec,
			Simples).  
simplify_clause((Head:-Body), Defmod, Preds0, Preds, Predspec,
		Simples) :- 
	!,
	simplify_compound_clause(Head, Body, Defmod, Defmod, Preds0, Preds, Predspec, Simples).
simplify_clause(Unit, Module, Preds, Preds, Module:Name/Arity,
		Simples) :-
	functor(Unit, Name, Arity),
	V0 is Arity + 1,
	simplify_head_args(Arity, Unit, V0, Simples, []).


simplify_compound_clause(Mod:Head, Body, _, Bodmod, Preds0, Preds,
		Predspec, Simples) :-
	!,
	simplify_compound_clause(Head, Body, Mod, Bodmod, Preds0, Preds, Predspec, Simples).
simplify_compound_clause(Head, Body, Mod, Bodmod, Preds0, Preds,
		Mod:Name/Arity, Simples) :-
	functor(Head, Name, Arity),
	V0 is Arity + 1,
	prenumber_head_args(Arity, Head),
	simplify_goal_1(Body, Bodmod, V0, V0, V1, Preds0, Preds, Simples0,
			[]),
	% It's a nuissance, but we have to do this last, to get the variable
	% numbering right:
	simplify_head_args(Arity, Head, V1, Simples, Simples0).


%  prenumber_head_args(+N, +-Head)
%  Not terribly declarative.  Binds all unbound argument *i* (1 =< i =< N) of
%  Head to a '$ VAR $' term for variable *i*.  This avoids having these
%  variables assigned other variable numbers.

prenumber_head_args(N, Head) :-
	(   N =< 0 ->
		true
	;   arg(N, Head, Arg),
	    (	var(Arg) ->
		    object_var(Arg, N)
	    ;	true
	    ),
	    N1 is N - 1,
	    prenumber_head_args(N1, Head)
	).


%  simplify_head_args(+Arity, +-Head, -Conj, +Tail)
%  Conj is a conjunction term describing the unification of the clause head
%  Head with a goal whose arguments are variables 1 through Arity, followed by
%  the goal terms on the list Tail.  V0 is the first unused varaible number.

simplify_head_args(Arity, Head, V0, conj(Simples), Tail) :-
	simplify_head_args_1(Arity, Head, V0, _, Simples, Tail).


%  simplify_head_args_1(+N, +-Head, +V0, -V, -Simples, +Simples0)
%  Simples is a list of goals unifying arguments *i* from 1 through N of Head
%  with '$ VAR $'(*i*) terms, followed by Simples0.  Where an argument *i* is a
%  variable, it is simply unified with '$ VAR $'(*i*).  Variables within head
%  arguments are numbered from V0 through V-1.

simplify_head_args_1(N, Head, V0, V, Simples, Simples0) :-
	(   N < 1 ->
		Simples = Simples0,
		V = V0
	;   arg(N, Head, Arg),
	    (   object_var(Arg, N) ->
		    Simples1 = Simples0,
		    V1 = V0
	    ;   Simples1 = [equal(N,Descr,Vars,_,V0)|Simples0],
		term_descr(Arg, V0, V1, Descr, Vars)
	    ),
	    N1 is N - 1,
	    simplify_head_args_1(N1, Head, V1, V, Simples, Simples1)
	).


%  simplify_goal(+Goal, +Mod, +V0, -V, +Preds0, -Preds, -Simple)
%  simplify_goal(+Goal, +Mod, +R, +V0, -V, +Preds0, -Preds, -Simple)

simplify_goal(Goal, Mod, V0, V, Preds0, Preds, Simple) :-
	simplify_goal(Goal, Mod, V0, V0, V, Preds0, Preds, Simple).

simplify_goal(Goal, Mod, R, V0, V, Preds0, Preds, Simple) :-
	simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples, []),
	(   Simples = [Simple] ->
		true
	;   Simple = conj(Simples)
	).


%  simplify_goal_1(+Goal, +Mod, +R, +V0, -V, +Preds0, -Preds, -Simples,
%		   +Simples0)

simplify_goal_1(Var, Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	var(Var),
	!,
	simplify_goal_1(call(Var), Mod, R, V0, V, Preds0, Preds, Simples,
			Simples0).
simplify_goal_1((G1,G2), Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	!,
	% Note that we number G2 before G1, but the simplified version of G1
	% is put before that of G2 in Simples.  This is because we need
	% variables last appearing later in the clause to be numbered lower
	% than variables last appearing earlier.
	simplify_goal_1(G2, Mod, R, V0, V1, Preds0, Preds1, Simples1,
			Simples0),
	simplify_goal_1(G1, Mod, V1, V1, V, Preds1, Preds, Simples, Simples1). 
simplify_goal_1((G1;G2), Mod, R, V0, V, Preds0, Preds, [Simple|Simples0],
		Simples0) :-
	!,
	simplify_disjunction(G1, G2, Mod, R, V0, V, Preds0, Preds, Simple).
simplify_goal_1((\+G), Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	!,
	simplify_goal_1((G->fail;true), Mod, R, V0, V, Preds0, Preds,
		      Simples, Simples0).
simplify_goal_1((G1->G2), Mod, R, V0, V, Preds0, Preds, Simples,
		Simples0) :-
	!,
	simplify_goal_1((G1->G2;fail), Mod, R, V0, V, Preds0, Preds,
		      Simples, Simples0).
simplify_goal_1((X=Y), _, R, V0, V, Preds, Preds, Simples, Simples0) :-
	!,
	simplify_unification(X, Y, R, V0, V, Simples, Simples0).
simplify_goal_1(is(X,Y), _, R, V0, V, Preds, Preds, Simples, Simples0) :-
	!,
	simplify_eval(X, Y, R, V0, V, Simples, Simples0).
simplify_goal_1(!, _, _, V, V, Preds, Preds, [!|Simples], Simples) :-
	!.
simplify_goal_1(Mod:Goal, _, R, V0, V, Preds0, Preds, Simples,
		Simples0) :-
	!,
	simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples,
		      Simples0).
simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	functor(Goal, Name, Arity),
	functor(Gterm, Name, Arity),
	simplify_goal_args(1, Arity, Goal, Gterm, V, V0, V, [], Simples,
			   Simples1),
	(   builtin(Name, Arity) ->
		Simples1 = [builtin(Gterm,_,R)|Simples0],
		Preds = Preds0
	;   Simples1 = [call(Pnum,Gterm,_,R)|Simples0],
	    get_pred_ref(Mod:Name/Arity, Preds0, Pnum, Preds)
	).


%  simplify_disjunction(+G1, +G2, +Mod, +R, +V0, -V, +Preds0, -Preds,
%		-Num, -Simple)
%  This is a bit tricky.  When simplifying a conjunction X,Y we simplify Y
%  first, and then X, so that earlier goals will get higher variable numbers.
%  For a disjunction X;Y it doesn't matter in what order we number them, because
%  we restrict each disjunct to the same threshold:  namely, the variable
%  number at the beginning of the whole disjunction.  In order for this to
%  work, though, we must be sure to number goals conjoined both before and
%  after the disjunction before numbering the disjunction itself.  This is
%  taken care of by simplify_goal/10.

simplify_disjunction((If->Then), Else, Mod, R, V0, V, Preds0, Preds,
		if_then_else(Sif,Sthen,Selse)) :-
	!,
	simplify_goal(Then, Mod, R, V0, V1, Preds0, Preds1, Sthen),
	simplify_goal(If, Mod, V1, V1, V2, Preds1, Preds2, Sif),
	(   Else = (G1;G2) ->
		simplify_disjunction(G1, G2, Mod, R, V2, V, Preds2, Preds,
				     Selse)
	;   simplify_goal(Else, Mod, R, V2, V, Preds2, Preds, Selse)
	).
simplify_disjunction(G1, G2, Mod, R, V0, V, Preds0, Preds, disj(Sgs)) :-
	!,
	simplify_disjunct(G1, Mod, R, V0, V1, Preds0, Preds1, Sgs, Sgs1),
	simplify_disjunct(G2, Mod, R, V1, V, Preds1, Preds, Sgs1, []).


%  simplify_disjunct(+G2, +Mod, +R, +V0, -V, +Preds0, -Preds, -Sgs, +Sgs0)

simplify_disjunct((G1;G2), Mod, R, V0, V, Preds0, Preds, Sgs, Sgs0) :-
	!,
	simplify_disjunct(G1, Mod, R, V0, V1, Preds0, Preds1, Sgs, Sgs1),
	simplify_disjunct(G2, Mod, R, V1, V, Preds1, Preds, Sgs1, Sgs0).
simplify_disjunct(G, Mod, R, V0, V, Preds0, Preds, [Simp|Sgs], Sgs) :-
	simplify_goal(G, Mod, R, V0, V, Preds0, Preds, Simp).


%  simplify_goal_args(+N, +Arity, +Goal, -Gterm, +R, +V0, -V, +Vs, -Simps,
%		      +Simps0)

simplify_goal_args(N, Arity, Goal, Gterm, R, V0, V, Vs, Simples, Simples0) :-
	(   N > Arity ->
		V = V0,
		Simples = Simples0
	;   arg(N, Goal, Garg),
	    arg(N, Gterm, Targ),
	    simplify_1_goal_arg(Garg, Targ, R, V0, V1, Vs, Simples, Simples1),
	    N1 is N + 1,
	    simplify_goal_args(N1, Arity, Goal, Gterm, R, V1, V, [Targ|Vs],
			       Simples1, Simples0)
	).


simplify_1_goal_arg(Garg, Targ, R, V0, V, Vs, Simples, Simples0) :-
	(   var(Garg) ->
		Targ = V0,
		Simples = Simples0,
		numbered_object_var(Garg, V0, V)
	;   object_var(Garg, N) ->
		(   member(N, Vs) ->
			% this variable has already been used as an arg in this
			% goal, so we'll have to create a new variable for it
			Targ = V0,
			numbered_object_var(Newvar, V0, V1),
			simplify_unification(Newvar, Garg, R, V1, V, Simples,
					     Simples0)
		;   Targ = N,
		    V = V0,
		    Simples = Simples0
		)
	;   Targ = V0,
	    numbered_object_var(Newvar, V0, V1),
	    simplify_unification(Newvar, Garg, R, V1, V, Simples, Simples0)
	).


%  simplify_unification(+X, +Y, +R, +V0, -V, -Simples, +Simples0)

simplify_unification(X, Y, R, V0, V, Simples, Simples0) :-
	(   var(X) ->
		Simples = [equal(V0,T,Vs,_,R)|Simples0],
		numbered_object_var(X, V0, V1),
		term_descr(Y, V1, V, T, Vs)
	;   object_var(X, N) ->
		Simples = [equal(N,T,Vs,_,R)|Simples0],
		term_descr(Y, V0, V, T, Vs)
	;   var(Y) ->
		Simples = [equal(V0,T,Vs,_,R)|Simples0],
		numbered_object_var(Y, V0, V1),
		term_descr(X, V1, V, T, Vs)
	;   object_var(Y, N) ->
		Simples = [equal(V0,T,Vs,_,R)|Simples0],
		term_descr(X, V0, V, T, Vs)
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity) ->
		simplify_unifications(1, Arity, X, Y, R, V0, V, Simples,
				      Simples0)
	;   Simples = [builtin(fail,_,V0)],
	    write('! the unification '),
	    write(X=Y),
	    write(' is sure to fail'),
	    nl,
	    V = V0
	).


simplify_unifications(N, Arity, X, Y, R, V0, V, Simples, Simples0) :-
	(   N > Arity ->
		V = V0,
		Simples = Simples0
	;   arg(N, X, Xn),
	    arg(N, Y, Yn),
	    simplify_unification(Xn, Yn, R, V0, V1, Simples, Simples1),
	    N1 is N + 1,
	    simplify_unifications(N1, Arity, X, Y, V1, V1, V, Simples, Simples1)
	).
	    


%  simplify_eval(+X, +Y, +V0, -V, -Simples, +Simples0)

simplify_eval(X, Y, R, V0, V, Simples, Simples0) :-
	(   var(X) ->
		Simples = [eval(V0,T,Vs,_,R)|Simples0],
		numbered_object_var(X, V0, V1),
		term_descr(Y, V1, V, T, Vs)
	;   object_var(X, N) ->
		Simples = [eval(N,T,Vs,_,R)|Simples0],
		term_descr(Y, V0, V, T, Vs)
	;   simplify_unification(Newvar, X, V1, V0, V1, Simples, Simples1),
	    simplify_eval(Newvar, Y, R, V1, V, Simples1, Simples0)
	).
	    


%  term_descr(+Term, +V0, -V, -Descr, -Vars)
%  term_descr(+Term, +V0, -V, -Descr, +Vars0, -Vars)
%  Descr is the term description, as described above, corresponding to Term,
%  and Vars is a sorted list of all the variables appearing in Term.

term_descr(Term, V0, V, Descr, Vars) :-
	term_descr(Term, V0, V, Descr, [], Vars1),
	sort(Vars1, Vars).			% sort and remove duplicates

term_descr(Term, V0, V, Descr, Vars0, Vars) :-
	(   var(Term) ->
		Descr = var(V0),
		numbered_object_var(Term, V0, V),
		Vars = [V0|Vars0]
	;   object_var(Term, N) ->
		Descr = var(N),
		V = V0,
		Vars = [N|Vars0]
	;   atom(Term) ->
		Descr = atom(Term),
		V = V0,
		Vars = Vars0
	;   integer(Term) ->
		Descr = integer(Term),
		V = V0,
		Vars = Vars0
	;   float(Term) ->
		Descr = float(Term),
		V = V0,
		Vars = Vars0
	;   Descr = compound(F,A,Args),
	    functor(Term, F, A),
	    arg_descrs(Term, 1, A, V0, V, Args, Vars0, Vars)
	).

%  arg_descrs(+Term, +N, +A, +V0, -V, -Args, +Vars0, -Vars)
%  Args is a list of the argument descriptions corresponding to arguments N
%  through A of Term, and Vars is a list of the variables appearing in those
%  args, appended to Vars0.

arg_descrs(Term, N, A, V0, V, Args, Vars0, Vars) :-
	(   N > A ->
		V = V0,
		Args = [],
		Vars = Vars0
	;   Args = [Descr|Args1],
	    arg(N, Term, Arg),
	    term_descr(Arg, V0, V1, Descr, Vars0, Vars1),
	    N1 is N + 1,
	    arg_descrs(Term, N1, A, V1, V, Args1, Vars1, Vars)
	).
	


object_var(Var, V) :-
	functor(Var, '$ VAR $', 1),		% Don't just write '$ VAR $'/1
	arg(1, Var, V).				% term since this program then
						% this program wouldn't be
						% self-applicable

numbered_object_var(Var, V0, V) :-
	V is V0 + 1,
	object_var(Var, V0).




/************************************************************************

			Handling the Foreign Interface

************************************************************************/

%  process_foreign_decl(+Spec, +Defmod, +Preds0, -Preds)

process_foreign_decl(Spec, Defmod, Preds0, Preds) :-
	functor(Spec, Name, Arity),
	set_property(Name/Arity, Defmod, Pred, foreign, Preds0, Preds1),
	put_pred_code(Pred, foreign(Spec), Preds1, Preds).

%   Package: logarr
%   Authors: Mostly David H.D. Warren, some changes by Fernando C. N.  Pereira
%   Updated: 09 Jan 1990
%   Purpose: Extendable arrays with logarithmic access time.

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%   Modified 23 Jan 95 by Peter Schachte to use arity 16 terms rather than 4.

/*  LOGARITHMIC ARRAYS.

    An array extends from 0 to (2^Size)-1, where Size is divisible by 4.
    Note that 2^Size = 1<<Size.

External interface:

    new_array(A)
	returns a new empty array A.

    is_array(A)
	checks whether A is an array.

    aref(Index, Array, Element)
	unifies Element with Array[Index], or fails if Array[Index]
	has not been set.

    arefa(Index, Array, Element)
	is as aref/3, except that it unifies Element with a new array if
	Array[Index] is undefined.  This is useful for multidimensional
	arrays implemented as arrays of arrays.

    arefl(Index, Array, Element)
	is as aref/3, except that Element appears as '[]' for
	undefined cells.

    aset(Index, Array, Element, NewArray)
	unifies NewArray with the result of setting Array[Index]
	to Element.

    alist(Array, List)
	returns a list of pairs Index-Element of all the elements
	of Array that have been set.

    BEWARE: the atom '$' is used to indicate an unset element, and the
    functor '$'/16 is used to indicate a subtree.  In general, array
    elements whose principal function symbol is '$' will not work.
*/

/*
:- module(logarr, [
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4
%	alist/2
   ]).
%:- use_module(library(types), [
%	must_be_nonneg/3
%   ]).
:- mode
	aref(+, +, ?),
	arefa(+, +, ?),
	arefl(+, +, ?),
%	alist(+, -),
	aset(+, +, +, -),
	array_item(+, +, +, +, ?),
	is_array(+),
	new_array(-),
	not_undef(+),
	subarray(+, +, ?),
	subarray_to_list(+, +, +, +, ?, ?),
	update_subarray(+, +, ?, ?, -).
*/

sccs_id('"@(#)90/01/09 logarr.pl	36.2"').



new_array(array($($,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$),4)).


is_array(array(_,_)).


% alist(array($(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AA,AB,AC,AD,AE,AF),Size), L0) :-
% 	N is Size-4,
% 	subarray_to_list(0,  N, 0, A0, L0, L1),
% 	subarray_to_list(1,  N, 0, A1, L1, L2),
% 	subarray_to_list(2,  N, 0, A2, L2, L3),
% 	subarray_to_list(3,  N, 0, A3, L3, L4),
% 	subarray_to_list(4,  N, 0, A4, L4, L5),
% 	subarray_to_list(5,  N, 0, A5, L5, L6),
% 	subarray_to_list(6,  N, 0, A6, L6, L7),
% 	subarray_to_list(7,  N, 0, A7, L7, L8),
% 	subarray_to_list(8,  N, 0, A8, L8, L9),
% 	subarray_to_list(9,  N, 0, A9, L9, LA),
% 	subarray_to_list(10, N, 0, AA, LA, LB),
% 	subarray_to_list(11, N, 0, AB, LB, LC),
% 	subarray_to_list(12, N, 0, AC, LC, LD),
% 	subarray_to_list(13, N, 0, AD, LD, LE),
% 	subarray_to_list(14, N, 0, AE, LE, LF),
% 	subarray_to_list(15, N, 0, AF, LF, []).

/*RB: dead code

subarray_to_list(K, 0, M, Item, [N-Item|L], L) :-
	not_undef(Item), !,
	N is K+M.
subarray_to_list(K, N, M, $(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AA,AB,AC,AD,AE,AF),
		L0, L) :-
	N > 0, !,
	N1 is N-4,
	M1 is (K+M) << 4,
	subarray_to_list(0,  N1, M1, A0, L0, L1),
	subarray_to_list(1,  N1, M1, A1, L1, L2),
	subarray_to_list(2,  N1, M1, A2, L2, L3),
	subarray_to_list(3,  N1, M1, A3, L3, L4),
	subarray_to_list(4,  N1, M1, A4, L4, L5),
	subarray_to_list(5,  N1, M1, A5, L5, L6),
	subarray_to_list(6,  N1, M1, A6, L6, L7),
	subarray_to_list(7,  N1, M1, A7, L7, L8),
	subarray_to_list(8,  N1, M1, A8, L8, L9),
	subarray_to_list(9,  N1, M1, A9, L9, LA),
	subarray_to_list(10, N1, M1, AA, LA, LB),
	subarray_to_list(11, N1, M1, AB, LB, LC),
	subarray_to_list(12, N1, M1, AC, LC, LD),
	subarray_to_list(13, N1, M1, AD, LD, LE),
	subarray_to_list(14, N1, M1, AE, LE, LF),
	subarray_to_list(15, N1, M1, AF, LF, L ).
subarray_to_list(_, _, _, _, L, L).

*/

aerr(Index, Array, Goal) :-
	must_be_nonneg(Index, 1, Goal),
	should_be(array, Array, 2, Goal).



aref(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	Index < 1<<Size,
	N is Size-4,
	Subindex is Index>>N /\ 15,
	array_item(Subindex, N, Index, Array, Item).
aref(Index, Array, Item) :-
	aerr(Index, Array, aref(Index,Array,Item)).


arefa(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-4,
	    Subindex is Index>>N /\ 15,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   new_array(Item)
	).
arefa(Index, Array, Item) :-
	aerr(Index, Array, arefa(Index,Array,Item)).


arefl(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-4,
	    Subindex is Index>>N /\ 15,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   Item = []
	).
arefl(Index, Array, Item) :-
	aerr(Index, Array, arefl(Index,Array,Item)).



aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-
	integer(Size0),
	integer(Index), 0 =< Index,
	!,
	enlarge_array(Index, Size0, Array0, Size, Array1),
	update_array_item(Size, Index, Array1, Item, Array).
arefl(Index, Array0, Item, Array) :-
	aerr(Index, Array0, aset(Index,Array0,Item,Array)).



% Guts

enlarge_array(I, Size0, Array0, Size, Array) :-
    (	I < 1<<Size0 ->
	Size = Size0, Array = Array0
    ;	Size1 is Size0+4,
	enlarge_array(I, Size1, $(Array0,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$), Size,
		      Array)
    ).


array_item(Subindex, N, Index, Array, Value) :-
	Sub1 is Subindex+1,
	arg(Sub1, Array, Elt),
	(   N =:= 0 ->
		Elt \== $,
		Value = Elt
	;   N1 is N-4,
	    Subindex1 is Index >> N1 /\ 15,
	    array_item(Subindex1, N1, Index, Elt, Value)
	).


% array_item(0, 0,_Index, $(Item,_,_,_), Item) :- !,
% 	not_undef(Item).
% array_item(0, N, Index, $(Array,_,_,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(1, 0,_Index, $(_,Item,_,_), Item) :- !,
% 	not_undef(Item).
% array_item(1, N, Index, $(_,Array,_,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(2, 0,_Index, $(_,_,Item,_), Item) :- !,
% 	not_undef(Item).
% array_item(2, N, Index, $(_,_,Array,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(3, 0,_Index, $(_,_,_,Item), Item) :- !,
% 	not_undef(Item).
% array_item(3, N, Index, $(_,_,_,Array), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).


not_undef($) :- !,
	fail.
not_undef(_).


update_array_item(0,_Index,_Item, NewItem, NewItem) :- !.
update_array_item(N, Index, Array, NewItem, NewArray) :-
	N1 is N-4,
	Subindex is Index >> N1 /\ 15,
	(   Array == $ ->
		update_subarray(Subindex, $($,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$),
				Array1, NewArray1, NewArray)
	;   update_subarray(Subindex, Array, Array1, NewArray1, NewArray)
	),
	update_array_item(N1, Index, Array1, NewItem, NewArray1).


update_subarray(0,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), A, A1,
		$(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(1,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), B, B1,
		$(A,B1,C,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(2,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), C, C1,
		$(A,B,C1,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(3,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), D, D1,
		$(A,B,C,D1,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(4,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), E, E1,
		$(A,B,C,D,E1,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(5,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), F, F1,
		$(A,B,C,D,E,F1,G,H,I,J,K,L,M,N,O,P)).
update_subarray(6,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), G, G1,
		$(A,B,C,D,E,F,G1,H,I,J,K,L,M,N,O,P)).
update_subarray(7,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), H, H1,
		$(A,B,C,D,E,F,G,H1,I,J,K,L,M,N,O,P)).
update_subarray(8,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), I, I1,
		$(A,B,C,D,E,F,G,H,I1,J,K,L,M,N,O,P)).
update_subarray(9,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), J, J1,
		$(A,B,C,D,E,F,G,H,I,J1,K,L,M,N,O,P)).
update_subarray(10, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), K, K1,
		$(A,B,C,D,E,F,G,H,I,J,K1,L,M,N,O,P)).
update_subarray(11, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), L, L1,
		$(A,B,C,D,E,F,G,H,I,J,K,L1,M,N,O,P)).
update_subarray(12, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), M, M1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M1,N,O,P)).
update_subarray(13, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), N, N1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N1,O,P)).
update_subarray(14, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), O, O1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O1,P)).
update_subarray(15, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), P, P1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P1)).
%  file    : misc
%  Authors : Peter Schachte
%  Purpose : misc support code for Prolog groundness analyzer

:- module(misc, [
	max/3,
	reverse/2
   ]).

%  max(+X, +Y, -Z)
%  Z is the greater of X and Y.

max(X, Y, Z) :-
	(   X >= Y ->
		Z = X
	;   Z = Y
	).


%  reverse(+L0, -L)
%  reverse(+L0, -L, +Tail)
%  List L has the same elements as L0, but in reverse order.  If Tail is
%  supplied, L is L0 reversed followed by Tail.

reverse(L0, L) :-
	reverse(L0, L, []).

reverse([], L, L).
reverse([U|Us], L, Tail) :-
	reverse(Us, L, [U|Tail]).


%  file    : mymaps
%  Authors : Peter Schachte
%  Purpose : maintain a set of key-value associations

%				   Abstract
%
%  This code implements a kind of map, also known as a finite function, a
%  dictionary.  A map is a set of *Key* -> *Value* associations.  Operations
%  supplied by this code allow creating a new map, looking up a *Value* given
%  a *Key*, associating a new *Value* with a specified *Key*, and traversing
%  all the associations in a map.

:- module(mymaps, [
	map_empty/1,
	map_store/4,
	map_fetch/3,
	map_member/3,
	map_first/2,
	map_next/5
   ]).

%:- use_module(library(avl)).			% use map code from QP library


/*****************************************************************
			       Basic Operations
*****************************************************************/

%  map_empty(-Empty)
%  Checks if Empty is an empty map, or binds Empty to the empty map.

map_empty(Empty) :-
	empty_avl(Empty).


%  map_store(+Key, +Value, +Map0, -Map)
%  Map is the same as Map0, except that in Map, key Key maps to Value.

map_store(Key, Value, Map0, Map) :-
	avl_store(Key, Map0, Value, Map).


%  map_fetch(+Key, +Map, -Value)
%  Value is the value associated with key Key in map Map.

map_fetch(Key, Map, Value) :-
	avl_fetch(Key, Map, Value).


/*****************************************************************

			       Traversing a Map

We support two kinds of traversal:  backtracking and forward.  Backtracking,
implemented by map_member/3 is certainly the simplest.

Forward traversal requires the introduction of a new data structure:  the
cursor.  A map may be traversed by obtaining a cursor from a map, and then
repeatedly supplying a map and a cursor to get a key, and value, and a new
cursor.

Note that the current implementation of these traversal predicates finds the
keys in alphabetical order (actually @< order), but you should not count on
that for future versions.

*****************************************************************/

%  map_member(+Map, *Key, *Value)
%  Like map_fetch, except it is meant to backtrack over all the Key-Value
%  pairs in the map.

map_member(Map, Key, Value) :-
	avl_member(Key, Map, Value).


%  map_first(+Map, -Cursor)
%  Cursor is a cursor that may be used for traversing Map.

map_first(Map, Cursor) :-
	(   avl_min(Map, Key, Val) ->
		Cursor = Key-Val
	;   Cursor = end
	).


%  map_next(+Cursor0, +Map, -Key, -Value, -Cursor)
%  Key and Value are the association found in Map at position Cursor0, and
%  Cursor is the cursor to find the next association.

map_next(Key-Value, Map, Key, Value, Cursor) :-
	% fails if Cursor0 = end
	(   avl_next(Key, Map, Knext, Vnext) ->
		Cursor = Knext-Vnext
	;   Cursor = end
	).


/*****************************************************************
				Printing Maps

For debugging, we provide portray/1 hook code to print maps.  We have two
forms, 'short' and 'full'.  In 'short' mode (the default), we only show the
keys, while 'full' mode shows the associated values as well.

This code shamelessy swiped from the AVL library code and modified to my
purposes.  This is a violation of referential transparency, but this is only a
debugging hack, so I can live with that.

*****************************************************************/


:- multifile user_portray/1.

user_portray(empty) :-
	write('MAP{}').
user_portray(node(K,V,B,L,R)) :-
	write('MAP{'),
	portray_avl(L, 0, X0),
	portray_avl(K, V, B, X0),
	portray_avl(R, 1, _),
	put("}").


portray_avl(empty, X, X).
portray_avl(node(K,V,B,L,R), X0, X) :-
	portray_avl(L, X0, X1),
	portray_avl(K, V, B, X1),
	portray_avl(R, 1, X).


portray_avl(K, V, B, X0) :-
	( X0 =:= 0 -> true ; put(0',) ),
	(   showing_maps(full) ->
		nl,
		print(K),
		(   B < 0 -> write('*->')
		;   B > 0 -> write('->*')
		;   write('->')
		),
		print(V)
	;   print(K)
	).

showmaps(X) :-
	(   showing_maps(X) ->
		true
	;   retractall(showing_maps(_)),
	    assert(showing_maps(X))
	).

:- dynamic showing_maps/1.

showing_maps(short).
%  File   : predstore
%  Authors: Peter Schachte
%  Purpose: storage and retrieval of predicate and analysis information
%
%				Abstract
%
%  This module implements an abstract datatype for storing the code of
%  and analysis information for the predicates to be analyzed.

:- module(predstore, [
	empty_predstore/1,
	predstore_first/2,
	predstore_next/4,
	predstore_size/2,
	get_pred_ref/4,
	get_pred_ref/5,
	get_old_pred_ref/3,
	put_pred_alias/4,
	put_pred_code/4,
	get_pred_code/3,
	put_pred_status/4,
	get_pred_status/3,
	property_mask/2,
	put_pred_travnum/4,
	get_pred_travnum/3,
	put_pred_success/4,
	get_pred_success/3,
	put_pred_call/4,
	get_pred_call/3,
	put_pred_prep/4,
	get_pred_prep/3,
	put_pred_spec/4,
	get_pred_spec/3,
	print_goal/2
   ]).


%  :- use_module(library(logarr)).		% use array code from library
%:- use_module(logarr).				% use my array code
%:- use_module(mymaps).				% and my maps code

/****************************************************************
		       Predicate Stores and References

This implementation of a predicate store contains three parts:
++enumerate
    1.  a mapping from predicate spec (Module:Name/Arity term) to predicate
	reference, which is just a number;
    2.  an array of data, one record per predicate; and
    3.  the next predicate reference number to be assigned.
--enumerate
We could just use the predicate spec as a predicate reference, but this
implementation should be more efficient.  It also admits reimplementation
using real arrays and real destructive modification, which should be quite a
bit more efficient.
****************************************************************/

%  empty_predstore(-Empty)
%  Empty is an empty predicate store.

empty_predstore(store(0,Map,Array)) :-
	map_empty(Map),
	new_array(Array).


%  predstore_first(+Store, -Cursor)
%  Cursor is a term that can be used to enumerate all the predicate references
%  in a predicate store.

predstore_first(store(N,_,_), N).


%  predstore_next(+Store, +Cursor0, -Predref, -Cursor)
%  Predref is the predicate reference for a predicate in Store at position
%  Cursor, and Cursor is the position of the following predicate reference.

predstore_next(_, Cursor0, Cursor0, Cursor) :-
	Cursor0 > 0,
	Cursor is Cursor0 - 1.


%  predstore_size(+Store, -Size)
%  Predstore Store contains Size predicates.

predstore_size(store(Size,_,_), Size).


%  get_pred_ref(+Spec, +Store0, -Ref, -Store)
%  get_pred_ref(+Spec, +Defmodule, +Store0, -Ref, -Store)
%  Ref is the predicate ref associated with predicate spec Spec, with default
%  module Defmodule, in Store.  Store is the same as Store0, except that it
%  may have been extended by adding predicate Spec.  If Defmodule is omitted,
%  Spec must be a complete spec of the form Module:Name/Arity.

get_pred_ref(Mod:Spec, _, Store0, Ref, Store) :-
	get_pred_ref(Spec, Mod, Store0, Ref, Store).
get_pred_ref(Name/Arity, Mod, Store0, Ref, Store) :-
	get_pred_ref(Mod:Name/Arity, Store0, Ref, Store).


get_pred_ref(Spec, Store0, N, Store) :-
	(   get_old_pred_ref(Spec, Store0, N) ->
		Store = Store0
	;   Store0 = store(N0,Map0,Array0),
	    Store = store(N,Map,Array),
	    N is N0 + 1,
	    map_store(Spec, N, Map0, Map),
	    aset(N, Array0,
		 pred(undefined,0,unprocessed,0,0,_,Spec), Array)
	).


%  get_old_pred_ref(+Spec, +Store0, -Ref)
%  get_old_pred_ref(-Spec, +Store0, +Ref)
%  get_old_pred_ref(*Spec, +Store0, *Ref)
%  Ref is the predicate ref associated with predicate spec Spec in Store0.
%  This differs from get_pred_ref/4 in that no new pred number will be
%  allocated.

get_old_pred_ref(Spec, Store, Ref) :-
	Store = store(_,Map,_),
	(   ground(Spec) ->
		map_fetch(Spec, Map, Ref)
	;   integer(Ref) ->
		get_pred_spec(Ref, Store, Spec)
	;   map_member(Map, Spec, Ref)		% backtrack over whole store
	).


%  put_pred_alias(+Oldpred, +Alias, +Store0, -Store)
%  Predicate store Store is the same as Store0, except that it defines Alias,
%  a predicate spec, as an alias for predicate Oldpred.

put_pred_alias(Oldpred, Alias, Store0, Store) :-
	get_old_pred_ref(Oldpred, Store0, Ref),
	\+ get_old_pred_ref(Alias, Store0, _),
	Store0 = store(N,Map0,Array),
	Store = store(N,Map,Array),
	map_store(Alias, Ref, Map0, Map).


/*****************************************************************
		       Accessing Predicate Information

The data we store for each predicate are:
++description
    code	The predicate's code.  The code is stored in a ground
		representation, as described below.
    status	The predicate's status bits.  This contains information about
		whether the predicate is static or dynamic, whether it is
		defined over multiple files, etc.
    travnum	Temporary dfs traversal number used when computing SCCs.
		Either 'unprocessed' or a positive integer.
    success	The predicate's success patterns.
    call	The predicate's call patterns.
    prep	The predicate's analysis preparation.  See 'prep.pl' for a
		description.
    spec	The predicate's Module:Name/Arity specification.
--description
*****************************************************************/


%  put_pred_code(+Ref, +Code, +Store0, -Store)
%  Store is the same as Store0, except that the code of the predicate referred
%  to by Ref is Code.

put_pred_code(Ref, Code, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(_,B,C,D,E,F,G)),
	aset(Ref, Array0, pred(Code,B,C,D,E,F,G), Array).


%  get_pred_code(+Ref, +Store, -Code)
%  Code is the code of the predicate referred to by Ref in Store.

get_pred_code(Ref, store(_,_,Array), Code) :-
	aref(Ref, Array, pred(Code,_,_,_,_,_,_)).


%  put_pred_status(+Ref, +Status, +Store0, -Store)
%  Store is the same as Store0, except that the status of the predicate
%  referred to by Ref is Status.

put_pred_status(Ref, Status, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,_,C,D,E,F,G)),
	aset(Ref, Array0, pred(A,Status,C,D,E,F,G), Array).


%  get_pred_status(+Ref, +Store, -Status)
%  Status is the status of the predicate referred to by Ref in Store.

get_pred_status(Ref, store(_,_,Array), Status) :-
	aref(Ref, Array, pred(_,Status,_,_,_,_,_)).


%  property_mask(+Property, -Mask)
%  property_mask(-Property, +Mask)
%  property_mask(*Property, *Mask)
%  Mask is the bitmask associated with predicate property Property.

property_mask(dynamic, 1).
property_mask(foreign, 2).
property_mask(multifile, 4).
property_mask(discontiguous, 8).


%  put_pred_travnum(+Ref, +Travnum, +Store0, -Store)
%  Store is the same as Store0, except that the lowest-numbered predicate
%  reachable from the predicate referred to by Ref is Travnum.

put_pred_travnum(Ref, Travnum, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,_,D,E,F,G)),
	aset(Ref, Array0, pred(A,B,Travnum,D,E,F,G), Array).


%  get_pred_travnum(+Ref, +Store, -Travnum)
%  Travnum is the lowest-numbered predicate reachable from the predicate
%  referred to by Ref in Store.

get_pred_travnum(Ref, store(_,_,Array), Travnum) :-
	aref(Ref, Array, pred(_,_,Travnum,_,_,_,_)).


%  put_pred_success(+Ref, +Success, +Store0, -Store)
%  Store is the same as Store0, except that the success of the
%  predicate referred to by Ref is Success.

put_pred_success(Ref, Success, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,_,E,F,G)),
	aset(Ref, Array0, pred(A,B,C,Success,E,F,G), Array).


%  get_pred_success(+Ref, +Store, -Success)
%  Success is the success of the predicate referred to
%  by Ref in Store.

get_pred_success(Ref, store(_,_,Array), Success) :-
	aref(Ref, Array, pred(_,_,_,Success,_,_,_)).


%  put_pred_call(+Ref, +Call, +Store0, -Store)
%  Store is the same as Store0, except that the call of the
%  predicate referred to by Ref is Call.

put_pred_call(Ref, Call, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,_,F,G)),
	aset(Ref, Array0, pred(A,B,C,D,Call,F,G), Array).


%  get_pred_call(+Ref, +Store, -Call)
%  Call is the call of the predicate referred to by Ref
%  in Store.

get_pred_call(Ref, store(_,_,Array), Call) :-
	aref(Ref, Array, pred(_,_,_,_,Call,_,_)).


%  put_pred_prep(+Ref, +Prep, +Store0, -Store)
%  Store is the same as Store0, except that the prep of the
%  predicate referred to by Ref is Prep.

put_pred_prep(Ref, Prep, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,E,_,G)),
	aset(Ref, Array0, pred(A,B,C,D,E,Prep,G), Array).


%  get_pred_prep(+Ref, +Store, -Prep)
%  Prep is the prep of the predicate referred to by Ref
%  in Store.

get_pred_prep(Ref, store(_,_,Array), Prep) :-
	aref(Ref, Array, pred(_,_,_,_,_,Prep,_)).


%  put_pred_code(+Ref, +Code, +Store0, -Store)
%  Store is the same as Store0, except that the code of the predicate referred
%  to by Ref is Code.

put_pred_spec(Ref, Spec, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,E,F,_)),
	aset(Ref, Array0, pred(A,B,C,D,E,F,Spec), Array).


%  get_pred_spec(+Ref, +Store, -Spec)
%  Spec is the spec of the predicate referred to by Ref in Store.

get_pred_spec(Ref, store(_,_,Array), Spec) :-
	aref(Ref, Array, pred(_,_,_,_,_,_,Spec)).


/*****************************************************************
				Printing Code

This code pretty-prints the ground representation we use for predicate code.

A goal specification is one of:
++description
    'conj'(*Gs*)
	specifying a conjunction of goals, where *Gs* is a list of goal specs.
    'equal'(*V,T,Vs,A,R*)
	specifying an explicit or implicit unification, where *V* is a
	variable number, *T* is a term description, *Vs* is a list of the
	variable numbers appearing in *T*, *A* is an analysis term containing
	analysis information for the point of the call (before the call), and
	*R* is the smallest variable number that does not appear later in the
	clause;
    'eval'(*V,E,Vs,A,R*)
	 specifying an explicit or implicit numeric calculation, where *V* is
	a variable number, *E* is a term description of the expression to
	evaluate, *Vs* is a list of the variable numbers appearing in *E*, and
	*A* and *R* are as for 'equal'/5;
    'builtin'(*T,A,R*)
	specifying a call to a Prolog builtin, where *T* is a term
	representing the goal and *T*'s arguments are all distinct variable
	numbers, and *A* and *R* are as for 'equal'/5;
    'call'(*P,T,A,R*)
	specifying a call to a user predicate, where *P* is the predicate
	number and *T*, *A*, and *R* are as for 'builtin'/3;
    '!'	(ordinary cut, cuts to outermost disjunction);
    'disj'(*Gs*)
	specifying a disjunction of goals, where *Gs* is a list of goal specs.
    'if_then_else'(*G1,G2,G3*)
	representing a Prolog if->then;else construct, where *G1*, *G2*, and
	*G3* are goal specs.
--description
We store analysis information for unifications, evaluations, and
builtin calls because it may prove useful in compiling these
operations, and it takes no time, only a little space.

A term description is one of:
++enumerate
    1.  'var'(*N*), where *N* is a variable number;
    2.  'atom'(*C*), where *C* is an atom;
    3.  'integer'(*C*), where *C* is an integer;
    4.  'float'(*C*), where *C* is a float;
    5.  'compound'(*F, A, Args*), where *F* is ther term's principal functor,
	*A* is its arity, and *Args* is a list of term descriptions.
--enumerate

*****************************************************************/


%  print_code(+Code, +Indent)
%  Print out Code at indentation level Indent.

print_code(Var, Indent) :-
	var(Var),
	!,
	indent(Indent),
	write('UNBOUND!'),
	nl.
print_code(undefined, Indent) :-
	!,
	indent(Indent),
	write('UNDEFINED!'),
	nl.
print_code(Code, Indent) :-
	indent(Indent),
	print_goal(Code, Indent),
	write('.'),
	nl.


print_body(Var, _) :-
	var(Var),
	!,
	write(Var).
print_body([], _) :-
	write('true').
print_body([G1|Gs], Indent) :-
	print_goal(G1, Indent),
	print_body_tail(Gs, Indent).


print_body_tail(Var, Indent) :-
	var(Var),
	!,
	write(' |'),
	nl,
	indent(Indent),
	write(Var).
print_body_tail([], _).
print_body_tail([G1|Gs], Indent) :-
	write(','),
	nl,
	indent(Indent),
	print_goal(G1, Indent),
	print_body_tail(Gs, Indent).
	


print_goal(Var, _) :-
	var(Var),
	!,
	write(Var).
print_goal(conj(Specs), Indent) :-
	print_body(Specs, Indent).
print_goal(equal(V,T,_,_,R), _) :-
	print_var(V),
	write(' = '),
	print_term(T),
	format(' /* ~w */', [R]).
print_goal(eval(V,E,_,_,R), _) :-
	print_var(V),
	write(' is '),
	print_term(E),
	format('/* ~w */', [R]).
print_goal(builtin(T,_,R), _) :-
	print_simple_goal(T),
	format('/* ~w */', [R]).
print_goal(call(_,T,_,R), _) :-
	print_simple_goal(T),
	format('/* ~w */', [R]).
print_goal(!, _) :-
	write(!).
print_goal(disj([G1|Gs]), Indent) :-
	write('(   '),
	I1 is Indent + 4,
	print_goal(G1, I1),
	print_disjunction(Gs, Indent, I1),
	nl,
	indent(Indent),
	write(')').
print_goal(if_then_else(G1,G2,G3), Indent) :-
	write('(   '),
	I1 is Indent + 4,
	I2 is Indent + 8,
	print_if_then_else(G1, G2, G3, Indent, I1, I2),
	nl,
	indent(Indent),
	write(')').


print_simple_goal(T) :-
	functor(T, Name, Arity),
	write(Name),
	(   Arity > 0 ->
		write('('),
		arg(1, T, V1),
		print_var(V1),
		print_simple_goal_args(2, Arity, T),
		write(')')
	;   true
	).


print_simple_goal_args(N, Arity, T) :-
	(   N =< Arity ->
		arg(N, T, Vn),
		write(', '),
		print_var(Vn),
		N1 is N + 1,
		print_simple_goal_args(N1, Arity, T)
	;   true
	).


print_if_then_else(G1, G2, G3, Indent, I1, I2) :-
	print_goal(G1, I1),
	write(' ->'),
	nl,
	indent(I2),
	print_goal(G2, I2),
	nl,
	indent(Indent),
	write(';   '),
	print_else(G3, Indent, I1, I2).


print_else(Var, _, _, _) :-
	var(Var),
	!,
	write(Var).
print_else([if_then_else(G1,G2,G3)], Indent, I1, I2) :-
	!,
	print_if_then_else(G1, G2, G3, Indent, I1, I2).
print_else(G3, _, I1, _) :-
	print_goal(G3, I1).



print_disjunction(Var, Indent, _) :-
	var(Var),
	!,
	nl,
	indent(Indent),
	write('|   '),
	write(Var).
print_disjunction([], _, _).
print_disjunction([D|Ds], Indent, I1) :-
	nl,
	indent(Indent),
	write(';   '),
	print_goal(D, I1),
	print_disjunction(Ds, Indent, I1).


print_var(N) :-
	write('V'),
	write(N).


print_term(Var) :-
	var(Var),
	!,
	write(Var).
print_term(var(N)) :-
	print_var(N).
print_term(atom(C)) :-
	write(C).
print_term(integer(I)) :-
	write(I).
print_term(float(F)) :-
	write(F).
print_term(compound(Name,0,[])) :-
	!,
	write(Name).
print_term(compound(.,2,[H,T])) :-
	!,
	write('['),
	print_var(H),
	write('|'),
	print_var(T),
	write(']').
print_term(compound(Name,_,[Arg|Args])) :-
	write(Name),
	write('('),
	print_var(Arg),
	print_term_args(Args),
	write(')').

print_term_args([]).
print_term_args([Arg|Args]) :-
	write(','),
	print_var(Arg),
	print_term_args(Args).


indent(N) :-
	(   N =:= 0 ->
		true
	;   N >= 8 ->
		put(0'	),
		N1 is N - 8,
		indent(N1)
	;   put(0' ),
	    N1 is N - 1,
	    indent(N1)
	).


/*****************************************************************
			  Printing Predicate Stores

For debugging, we provide portray/1 hook code to print stores.

*****************************************************************/

print_store(X, Map, Cursor0, Store) :-
	(   map_next(Cursor0, Map, Pred, Predref, Cursor1) ->
		print_store_separator(X),
		print_store_pred(X, Pred, Predref, Store),
		print_store(X, Map, Cursor1, Store)
	;   true
	).


print_store_pred(none, _, _, _) :-
	!.
print_store_pred(X, Pred, Predref, Store) :-
	format('~w=~q', [Predref,Pred]),
	print_store_pred_suffix(X),
	print_store_pred1(X, Predref, Store).


print_store_pred_suffix(none).
print_store_pred_suffix(short).
print_store_pred_suffix(full) :-
	print_store_pred_suffix(code).
print_store_pred_suffix(groundness) :-
	format(':  ', []).
print_store_pred_suffix(code) :-
	format(' :-~n', []).


print_store_prefix(none) :-
	write('...').
print_store_prefix(short).
print_store_prefix(full) :-
	print_store_prefix(code).
print_store_prefix(code) :-
	nl.
print_store_prefix(groundness) :-
	nl.


print_store_separator(none).
print_store_separator(short) :-
	write(', ').
print_store_separator(full) :-
	nl.
print_store_separator(groundness) :-
	nl.
print_store_separator(calls) :-
	nl.

%  unneeded:  print_store_pred1(none, _, _).
print_store_pred1(short, _, _).
print_store_pred1(full, Predref, Store) :-
	print_store_pred1(code, Predref, Store),
	format('    groundness --~n', []),
	print_store_pred1(groundness, Predref, Store).
print_store_pred1(code, Predref, Store) :-
	get_pred_code(Predref, Store, Code),
	print_code(Code, 8).
print_store_pred1(groundness, Predref, Store) :-
	get_pred_call(Predref, Store, Call),
	get_pred_success(Predref, Store, Success),
	write('	'),
	print_boolfn(Call),
	format(' => ', []),
	print_boolfn(Success),
	nl.


print_boolfn(X) :-
	print(X).


:- dynamic showing_stores/1.

showing_stores(short).

user_show_stores(X) :-
	(   showing_stores(X) ->
		true
	;   \+ valid_store_show_state(X) ->
		format('invalid state for showstores/1:  ~q~n', [X]),
		fail
	;   retractall(showing_stores(_)),
	    assert(showing_stores(X))
	).

valid_store_show_state(none).
valid_store_show_state(short).
valid_store_show_state(full).
valid_store_show_state(code).
valid_store_show_state(groundness).

:- multifile user_portray/1.

user_portray(store(N,Map,Array)) :-
	integer(N),
	nonvar(Map),
	nonvar(Array),
	!,
	write('STORE{'),
	map_first(Map, Cursor0),
	showing_stores(X),
	(   map_next(Cursor0, Map, Pred, Predref, Cursor1) ->
		Store = store(N,Map,Array),
		print_store_prefix(X),
		print_store_pred(X, Pred, Predref, Store),
		print_store(X, Map, Cursor1, Store)
	;   true
	),
	write('}').
user_portray(conj(L)) :-
	print_goal(conj(L), 8).
user_portray(disj(L)) :-
	print_goal(disj(L), 8).
user_portray(if_then_else(G1,G2,G3)) :-
	print_goal(if_then_else(G1,G2,G3), 8).
user_portray(equal(A,B,C,D,E)) :-
	print_goal(equal(A,B,C,D,E), 8).
user_portray(eval(A,B,C,D,E)) :-
	print_goal(eval(A,B,C,D,E), 8).
user_portray(builtin(A,B,C)) :-
	print_goal(builtin(A,B,C), 8).
user_portray(call(A,B,C,D)) :-
	print_goal(call(A,B,C,D), 8).



%  file    : prep.pl
%  Authors : Peter Schachte
%  Purpose : Prepare Prolog code for groundness analysis
%
%				Abstract
%
%  Abstract Interpretation is based on fixpoint iteration.  In order to
%  minimize the time, we do as much of the work as possible in an initial
%  preparation phase, outside the loop.  This code does the fixed part of the
%  analysis, that part that does not require fixpoint iteration.  For
%  bottom-up analysis, this means computing the glb of the analyses of the
%  calls to predicates in other strongly-connected components for each clause,
%  and also computing the lub of these fixed parts for all the clauses that
%  have no calls to the same strongly-connected component.  For top-down
%  analysis, this means computing for each call in each clause, the glb of the
%  analyses of all the inter-scc (non-recursive) calls up to that call.  We do
%  both here.

:- module(prep, [
	prepare_scc/3,
	prepare_code/5
   ]).

%:- use_module(library(basics)).
%:- use_module(predstore).
%:- use_module(analysis).
%:- use_module(misc).


%  TODO:  use 0 instead of 'none'.  Should work but needs careful testing.
%  This would allow me to use max/3 instead of choose_restriction/3,
%  choose_restriction_limit/3, and rlimit_max/3.


/*****************************************************************

			       Code Preparation

This code prepares predicate code for analysis.  This means that we
create for each predicate in a given strongly-connected component a
data structure which contains all and only the information we need to
consider in doing the fixpoint computation for both the bottom-up and
top-down analysis.  We call this data structure a prep term, which is
either:
++description
    'fixed'(*A*)	where *A* is the predicate's complete success
			pattern analysis (we get this for
			non-recursive predicates); or
    a compound prep	which we get for recursive or mutually
			recursive predicates.
--description
A compound prep is one of:
++description
    'conj_prep'(*A,V*)	which describes a conjunction, where *A* is
			the non-recursive part of the analysis and *V*
			is the recursive part; or
    'disj_prep'(*A,V,R*)
			which describes a disjjunction, where *A* is
			the non-recursive part of the analysis, *V* is the
			recursive part, and *R* is the restriction threshold
			(i.e., the lowest numbered variable not appearing
			later in the code); or
    'call_prep'(*P,T,A,R*)
			which describes an individual goal, where *P*
			is the predicate ref of the predicate being
			called, *T* is the call term, *A* is the
			analysis of the calls up to this one, and *R*
			is the restriction threshold.
--description

The *Restriction* terms above apply only *after* conjunction with the left
context of the prep term, i.e. the glb of the analyses of the preceding prep
terms.  For example, in the evaluation of a 'disj_prep' term, the lub of *A*
and the analyses of the elements of *V* are first meeted with the the left
context, and then variables numbered greater or equal to *R* are restricted
away.

****************************************************************/

%  prepare_scc(+SCC, +Preds0, -Preds)
%  prepare_scc(+SCC, +Preds0, -Preds, +WholeSCC)
%  Preds is a predicate store identical to Preds0 except that all the
%  predicates specified on SCC have been "prepared."  SCC is a list of
%  predicate references.  WholeSCC is the whole list of predicates in the
%  scc.

prepare_scc(SCC, Preds0, Preds) :-
	prepare_scc(SCC, Preds0, Preds, SCC).

prepare_scc([], Preds, Preds, _).
prepare_scc([Ref|Refs], Preds0, Preds, SCC) :-
	get_old_pred_ref(_:_/Arity, Preds0, Ref),
	Restriction is Arity + 1,
	get_pred_code(Ref, Preds0, Code),
	prepare_code(Code, Restriction, Preds0, SCC, Prep),
	put_pred_prep(Ref, Prep, Preds0, Preds1),
	prep_fixed_part(Prep, Restriction, Approx1),
	put_pred_success(Ref, Approx1, Preds1, Preds2),
	prepare_scc(Refs, Preds2, Preds, SCC).
	

%  prepare_code(+-Code, +Arity, +Preds, +SCC, -Prep)
%  Prep is the prepared version of Code, given that SCC is a list of the
%  references of the predicates in the same SCC as Code, and Arity is the
%  arity of the predicate whose code is Code. Preds is a predstore.

prepare_code(undefined, _, _, _, fixed(1)) :-
	!.
prepare_code(foreign(Foreignspec), _, _, _, fixed(Fixed)) :-
	!,
	analyze_foreign(Foreignspec, Fixed).
prepare_code(Code, Arity, Preds, SCC, Prep) :-
	anal_top(Top),
	anal_bottom(Bottom),
	% could just call prepare_1_conj, but that will usually get around to
	% making the following call, anyway:
	prepare_1_disj(Code, Preds, SCC, none, _, 999999, _, Arity, Top,
		       Bottom, Fixed, Var, []),
	(   Var == [] ->
		Prep = fixed(Fixed)
	% This won't handle all cases, as anal_top isn't meant to test if an
	% analysis is top, only to generate *an instance* of top.  Still, it
	% will often work, and is better than nothing.
 	;   Var = [Single],
 	    anal_bottom(Fixed) ->
 		Prep = Single
	;   Var = [Elt|_],
	    prep_restriction(Elt, Restriction),
	    Prep = disj_prep(Fixed,Var,Restriction)
	).


%  prepare_conj(+Gs, +Preds, +SCC, +Rlim0, -Rlim, +Rlast0, -Rlast, -Rnext0,
%		+Rnext, +Fixed0, -Fixed, -Var, +Var0)
%  Fixed is the fixed (non-recursive) part of the analysis of the conjunction
%  of the goals on the list Gs, meeted with Fixed0.  Var is a list of the
%  variable (recursive) goals on Gs, followed by Var0.  Preds is the predstore
%  containing all the predicates in the program, and SCC is a list of the
%  predicate references of all the predicates in the same SCC as the predicate
%  being examined.  Var is basically a list of the recursive goals on Gs, and
%  Fixed is the glb of analyses of all the rest.  Rlim is Rlim0 unless Rlim0
%  is 'none', in which case it is the restriction threshold for the last goal
%  in Gs before the first recursive call, or 'none' if there are no recursive
%  calls.  Rlast is restriction threshold of the last goal in Gs and Rlast0 is
%  the threshold for the last goal before Gs.  Rnext0 is the threshold for the
%  last goal in Gs before the first recursive goal, or Rnext if there are no
%  recursive goals, and Rnext is the threshold to be used for the prep term
%  for the last recursive call in Gs, which should be the last threshold
%  before the first recursive call *following* Gs, or 1 + the arity of the
%  predicate if there are no following recursive calls; this pair is used for
%  backward information flow.

prepare_conj([], _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext, Fixed, Fixed,
		Var, Var).
prepare_conj([G|Gs], Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0, Rnext,
		Fixed0, Fixed, Var, Var0) :-
	prepare_1_conj(G, Preds, SCC, Rlim0, Rlim1, Rlast0, Rlast1, Rnext0,
		       Rnext1, Fixed0, Fixed1, Var, Var1),
	prepare_conj(Gs, Preds, SCC, Rlim1, Rlim, Rlast1, Rlast, Rnext1,
		     Rnext, Fixed1, Fixed, Var1, Var0).


%  prepare_1_conj(+Goal, +Preds, +SCC, +Rlimit0, -Rlimit, +Rlast0, -Rlast,
%		  -Rnext0, +Rnext, +Fixed0, -Fixed, +Var0, -Var)
%  Same as prepare_conj, except that Goal is a single goal.

% prepare_1_conj(undefined, _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext,
% 		Fixed, Fixed, Var, Var).
prepare_1_conj(conj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		Rnext, Fixed0, Fixed, Var, Var0) :-
	prepare_conj(Goals, Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		     Rnext, Fixed0, Fixed, Var, Var0).
prepare_1_conj(equal(V,T,Vs,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_unif(V, T, Vs, Restriction, Fixed0, Fixed).
prepare_1_conj(eval(V,T,Vs,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_eval(V, T, Vs, Restriction, Fixed0, Fixed).
prepare_1_conj(builtin(T,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_builtin(T, Restriction, Fixed0, Fixed).
prepare_1_conj(call(P,T,Fixed0,R), Preds, SCC, Rlim0, Rlim, Rlast0, R,
		Rnext0, Rnext, Fixed0, Fixed, Var, Var0) :-
	(   member(P, SCC) ->			% it's a recursive call
		Var = [call_prep(P,T,Fixed0,Rnext)|Var0],
		Fixed = Fixed0,
		Rnext0 = Rlast0,
		choose_restriction_limit(Rlim0, Rlast0, Rlim)
	;   Rlim = Rlim0,			% non-recursive call
	    Var = Var0,
	    Rnext = Rnext0,
	    choose_restriction(Rlim, R, Restriction),
	    analyze_call(P, Preds, T, Restriction, Fixed0, Fixed)
	).
prepare_1_conj(!, _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext, Fixed, Fixed,
		Var, Var).
prepare_1_conj(disj(Ds), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		Rnext, Fixed0, Fixed, Var, Var0) :-
	anal_bottom(Bottom),
	choose_restriction(Rlim0, Rlast0, Rnext1),
	prepare_disj(Ds, Preds, SCC, Rlim0, Rlim0, Rlim, Rlast0, 0, Rlast,
		     Rnext1, Fixed0, Bottom, Fixed1, Var1, []),
	(   Var1 == [] ->
		Var = Var0,
		Rnext = Rnext0,
		Fixed = Fixed1
% I don't think this code is right!!!!
% 	;   Var1 = [conj_prep(Fixed2,Var2)] ->
% 		anal_meet(Fixed0, Fixed1, Fixed1a),
% 		anal_meet(Fixed1a, Fixed2, Fixed),
% 		append(Var2, Var0, Var)
	;   Fixed = Fixed0,
	    Rnext0 = Rlast0,
	    Var = [disj_prep(Fixed1,Var1,Rnext)|Var0]
	).
prepare_1_conj(if_then_else(G1,G2,G3), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast,
		Rnext0, Rnext, Fixed0, Fixed, Var, Var0) :- 
	prepare_1_conj(disj([conj([G1,G2]),G3]), Preds, SCC, Rlim0, Rlim,
		       Rlast0, Rlast, Rnext0, Rnext, Fixed0, Fixed, Var,
		       Var0).


%  prepare_disj(+Gs, +Preds, +SCC, +Rlimctxt, +Rlim0, -Rlim, +Rlastctxt,
%  		+Rlast0, -Rlast, +Rnext, +Context, +Fixed0, -Fixed, -Var,
%  		+Var0)
%  Fixed is the fixed (non-recursive) part of the analysis of the disjunction
%  of the goals on the list Gs, joined with Fixed0.  Var is a list of the
%  variable (recursive) goals on Gs, followed by Var0.  Context is the fixed
%  part of the analysis of the goals conjoined to the left of the disjunction
%  Gs.  Preds is the predstore containing all the predicates in the program,
%  and SCC is a list of the predicate references of all the predicates in the
%  same SCC as the predicate being examined.  Var is basically a list of the
%  recursive goals on Gs, and Fixed is the lub of analyses of all the rest.
%  Rlim is the maximum of Rlim0 and the restriction thresholds for all the
%  goals in Gs, or 'none' if there are no recursive calls.  Rlimctxt is the
%  restriction limit to apply to each disjunct.  Rlast is restriction
%  threshold of the last goal in each disjunct in Gs and Rlastctst is the
%  threshold for the last goal before Gs.  The Rlast for most disjuncts will
%  be the same, but a disjunct with only a cut won't get it right, so we take
%  the max of Rlast0 and the Rlasts of the disjuncts.  Rnext is the threshold
%  to be used for the prep term of the last recursive call in each disjunct in
%  Gs, which should be the last threshold before the first recursive call
%  *following* Gs, or the arity of the predicate if there are no following
%  recursive calls.

prepare_disj([], _, _, _, Rlim, Rlim, _, Rlast, Rlast, _, _, Fixed, Fixed,
		Var, Var).
prepare_disj([D|Ds], Preds, SCC, Rlimctxt, Rlim0, Rlim, Rlastctxt, Rlast0,
		Rlast, Rnext, Context, Fixed0, Fixed, Var, Var0) :-
	prepare_1_disj(D, Preds, SCC, Rlimctxt, Rlim1, Rlastctxt, Rlast1,
		       Rnext, Context, Fixed0, Fixed1, Var, Var1),
	rlimit_max(Rlim0, Rlim1, Rlim2),
	max(Rlast0, Rlast1, Rlast2),		% most Rlasts are the same
	prepare_disj(Ds, Preds, SCC, Rlimctxt, Rlim2, Rlim, Rlastctxt, Rlast2,
		     Rlast, Rnext, Context, Fixed1, Fixed, Var1, Var0).


%  prepare_1_disj(+G, +Preds, +SCC, +Rlim0, -Rlim, +Rlast0, -Rlast,
%		       +Rnext, +Context, +Fixed0, -Fixed, -Var, +Var0)

% prepare_1_disj(undefined, _, _, _, Rlim, Rlim, Rlast, Rlast, _, _, Fixed,
% 		Fixed, Var, Var).
prepare_1_disj(conj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext,
		Context, Fixed0, Fixed, Var, Var0) :-
	prepare_conj(Goals, Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, _, Rnext,
		     Context, Fixed1, Var1, []),
	(   Var1 == [] ->
		anal_join(Fixed0, Fixed1, Fixed),
		Var = Var0
% I don't think this code is right!!!!
% 	;   Var1 = [disj_prep(Fixed2,Var2,_)] ->
% 		anal_join(Fixed0, Fixed1, Fixed1a),
% 		anal_join(Fixed1a, Fixed2, Fixed),
% 		append(Var2, Var0, Var)
	;   Fixed = Fixed0,			% has a variable part, so
						% fixed part doesn't
						% contribute to first approx.
	    Var = [conj_prep(Fixed1,Var1)|Var0]
	).
prepare_1_disj(equal(V,T,Vs,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_unif(V, T, Vs, Restriction, Context, Fixed1),
	anal_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(eval(V,T,Vs,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_eval(V, T, Vs, Restriction, Context, Fixed1),
	anal_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(builtin(T,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_restriction(Rlim, R, Restriction),
	analyze_builtin(T, Restriction, Context, Fixed1),
	anal_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(call(P,T,Context,R), Preds, SCC, Rlim0, Rlim, Rlast0, R, _,
		Context, Fixed0, Fixed, Var, Var0) :-
	(   member(P, SCC) ->			% it's a recursive call
		Var = [call_prep(P,T,Context,Rlim)|Var0],
		Fixed = Fixed0,
		choose_restriction_limit(Rlim0, Rlast0, Rlim)
	;   Rlim = Rlim0,
	    Var = Var0,
	    choose_restriction(Rlim, R, Restriction),
	    analyze_call(P, Preds, T, Restriction, Context, Fixed1),
	    anal_join(Fixed0, Fixed1, Fixed)
	).
prepare_1_disj(!, _, _, Rlim, Rlim, Rlast, Rlast, _, _, Fixed, Fixed, Var,
		Var).
prepare_1_disj(disj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext,
		Context, Fixed0, Fixed, Var, Var0) :-
	prepare_disj(Goals, Preds, SCC, Rlim0, Rlim0, Rlim, Rlast0, Rlast0,
		     Rlast, Rnext, Context, Fixed0, Fixed, Var, Var0).
prepare_1_disj(if_then_else(G1,G2,G3), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast,
		Rnext, Context, Fixed0, Fixed, Var, Var0) :-
	prepare_1_disj(conj([G1,G2]), Preds, SCC, Rlim0, Rlim1, Rlast0,
		       Rlast1, Rnext, Context, Fixed0, Fixed1, Var, Var1),
	prepare_1_disj(G3, Preds, SCC, Rlim0, Rlim2, Rlast0, Rlast2, Rnext,
		       Context, Fixed1, Fixed, Var1, Var0),
	rlimit_max(Rlim1, Rlim2, Rlim),
	max(Rlast1, Rlast2, Rlast).


%  choose_restriction(+Rlimit0, +R, -Restriction)

choose_restriction(none, Restriction, Restriction) :-
	!.
choose_restriction(Restriction, _, Restriction).
	

%  choose_restriction_limit(+Rlim0, +Rlast, -Rlim)

choose_restriction_limit(none, Rlast, Rlast) :-
	!.
choose_restriction_limit(Rlim, _, Rlim).


%  rlimit_max(+Limit0, +Limit1, -Limit)
%  Limit is the maximum restriction limit of Limit0 and Limit1.  Note that
%  either or both of these may be 'none', which we treat like -infinity.

rlimit_max(none, Limit, Limit) :-
	!.
rlimit_max(Limit0, Limit1, Limit) :-
	(   Limit1 == none ->
		Limit = Limit0
	;   max(Limit0, Limit1, Limit)
	).


%  prep_fixed_part(+Prep, +Restriction, -Fixed)
%  Fixed is the fixed part of Prep, a prep term, restricted to variables less
%  than Restriction.

prep_fixed_part(fixed(Fixed), _, Fixed).
prep_fixed_part(conj_prep(Fixed0,Var), Restriction, Fixed) :-
	prep_list_fixed_part(Var, Fixed0, Fixed1),
	anal_restrict(Fixed1, Restriction, Fixed).
prep_fixed_part(disj_prep(Fixed,_,_), _, Fixed).
prep_fixed_part(call_prep(_,_,_,_), _, Bottom) :-
	anal_bottom(Bottom).


prep_list_fixed_part([], Fixed, Fixed).
prep_list_fixed_part([Prep|Preps], Fixed0, Fixed) :-
	prep_fixed_part(Prep, 999999, Fixed1),
	(   anal_bottom(Fixed1) ->		% no point going on
		anal_bottom(Fixed)
	;   anal_meet(Fixed0, Fixed1, Fixed2),
	    prep_list_fixed_part(Preps, Fixed2, Fixed)
	).


%  prep_restriction(+Prep, -Restriction)
%  Restriction is the restriction threshold for prep term Prep.

prep_restriction(conj_prep(_,Var), Restriction) :-
	last(Var, Last),
	prep_restriction(Last, Restriction).
prep_restriction(disj_prep(_,_,Restriction), Restriction).
prep_restriction(call_prep(_,_,_,Restriction), Restriction).


%  last(+List, -Last)
%  List is a non-empty list, and Last is its last element.

last([H|T], Last) :-
	last(T, H, Last).

last([], Last, Last).
last([H|T], _, Last) :-
	last(T, H, Last).
	
%  File   : scc
%  Authors: Peter Schachte
%  Purpose: Find Strongly Connected Components in a program call graph
%
%				Abstract
%
%  Find strongly-connected components in a program call graph using Tarjan's
%  algorithm, as described in Sedgewick's *Algorithms* book.  This algorithm
%  also topologically sorts the strongly-connected components.


:- module(scc, [
	sccs/3,
	sccs/4
   ]).

%:- ensure_loaded(predstore).

/*************************************************************************

		   Condensing Strongly-Connected Components

The choice of the word *condense* to describe this may be a bit confusing,
since the word "condensing" is used to describe Abstract Interpretation
domains.  In traditional program analysis literature, however, "condensing" is
used to refer to creating an acycylic graph from a cyclic one by combining
cyclic subgraphs into single nodes, which is exactly what we're doing here.

*************************************************************************/

/*================================================================

				  Algorithm

We use a declarative adaption of Tarjan's SCC algorithm.  For this, we must
give each predicate a traversal number.  We can't use the predicate
numbers we've already assigned because it's important that a predicate
that's already been visited in the traversal must have a lower number.
The algorithm basically works like this:

to process a pred P:
    If we've already processed P, do nothing more.  P's lowlink is
	just P's traversal number.
    Otherwise, give P the next traversal number, push P on the stack, and
	process all of P's immediate successors, collecting the minimum of
	their lowlinks and P's traversal number, which will be P's lowlink.
	If P's lowlink = P's traversal number, pop nodes off the stack down to
	and including P, marking them as completed by setting their traversal
	numbers to an impossibly large number.  This is a new strong
	component.

================================================================*/

%  sccs(+Preds0, -Preds, -SCCs)
%  sccs(+Preds0, -Preds, +Goal, -SCCs)
%  sccs(+Preds, +Cursor, +Id0, -Id, +Stack0, -Stack, +SCCs0, -SCCs)
%  SCCs is a list of the strongly-connected components on Preds, a predicate
%  store.  Each strong component is represented as a list of predicate
%  references.  If Goal is supplied, then SCCs includes only sccs reachable
%  from Goal, which is a (possibly compound) goal in the form of a clause
%  body.
%
%  Cursor is a cursor for enumerating the predicates in Preds.  Id0 is the
%  traversal number to give to the next predicate we need one for, and Id is
%  the next one available after processing Preds.  Stack0 is a list of
%  predicates being processed, simulating Tarjan's stack.  Stack is the stack
%  after processing Preds.

sccs(Preds0, Preds, Goal, SCCs) :-
	handle_calls(Goal, Preds0, Preds, 0, _, 0, _, [], [], [], SCCs).

sccs(Preds0, Preds, SCCs) :-
	predstore_first(Preds0, Cursor),
	sccs(Preds0, Preds, Cursor, 0, _, [], [], [], SCCs).

sccs(Preds0, Preds, Cursor, Id0, Id, Stack0, Stack, SCCs0, SCCs) :-
	(   predstore_next(Preds0, Cursor, Pred, Cursor1) ->
		handle_pred(Pred, Preds0, Preds1, Id0, Id1, Stack0, Stack1,
			    SCCs1, SCCs, _),
		sccs(Preds1, Preds, Cursor1, Id1, Id, Stack1, Stack, SCCs0,
		     SCCs1)
	;   Id = Id0,
	    Stack = Stack0,
	    SCCs = SCCs0,
	    Preds = Preds0
	).


%  handle_pred(+Pred, +Preds0, -Preds, +Id0, -Id, +Stack0, -Stack, +SCCs0, 
%		-SCCs, -Lowlink)
%  Recursively handle the predicates called by a single predicate, and if this
%  predicate's strongly-connected component isn't already in SCCs, put it
%  there.  Most arguments are as for sccs/9.  Pred is this predicate's
%  reference.  Lowlink is the lowest id number of all the predicates reached
%  (recursively called) from the predicate defined by Predterm.

handle_pred(Pred, Preds0, Preds, Id0, Id, Stack0, Stack, SCCs0, SCCs, Low) :-
	get_pred_code(Pred, Preds0, Code),
	get_pred_travnum(Pred, Preds0, Travnum),
	(   Travnum == unprocessed ->
		put_pred_travnum(Pred, Id0, Preds0, Preds1),
		Id1 is Id0 + 1,
		handle_calls(Code, Preds1, Preds2, Id1, Id, Id0, Low,
			     [Pred|Stack0], Stack1, SCCs1, SCCs),
		(   Id0 == Low ->
			SCCs1 = [SCC|SCCs0],
			pop_stack(Stack1, Stack, Pred, Preds2, Preds, SCC)
		;   SCCs1 = SCCs0,
		    Stack = Stack1,
		    Preds = Preds2
		)
	;   Preds = Preds0,
	    Id = Id0,
	    Stack = Stack0,
	    SCCs = SCCs0,
	    Low = Travnum
	).



%  handle_calls(+Code, +Preds0, -Preds, +Id0, -Id, +Low0, -Low, +Stack0,
%		-Stack, +SCCs0, -SCCs)
%  Code is code of a predicate.  Low is the lowest traversal number (the
%  lowlink) of any predicate called from Bodies, or Low0, whichever is
%  smaller.  Other args are as above.

handle_calls(undefined, Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs).				% report undefineds elsewhere
handle_calls(foreign(_), Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs).				% Foreigns call nothing
handle_calls(conj(Gs), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_calls(Gs, Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		     SCCs0, SCCs).
handle_calls([], Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs, SCCs).
handle_calls([G|Gs], Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack, SCCs0,
		SCCs) :-
	handle_calls(G, Preds0, Preds1, Id0, Id1, Low0, Low1, Stack0, Stack1,
		     SCCs1, SCCs),
	handle_calls(Gs, Preds1, Preds, Id1, Id, Low1, Low, Stack1, Stack,
		     SCCs0, SCCs1).
handle_calls(equal(_,_,_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack,
		SCCs, SCCs).
handle_calls(eval(_,_,_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack,
		SCCs, SCCs).
handle_calls(builtin(_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs). 
handle_calls(call(Pred,_,_,_), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_pred(Pred, Preds0, Preds, Id0, Id, Stack0, Stack, SCCs0, SCCs,
		    Low1),
	(   Low0 < Low1 ->			% Low is the minimum of Low0
		Low = Low0			% and Low1
	;   Low = Low1
	).
handle_calls(!, Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs, SCCs).
handle_calls(disj(Gs), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_calls(Gs, Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		     SCCs0, SCCs).
handle_calls(if_then_else(G1,G2,G3), Preds0, Preds, Id0, Id, Low0, Low,
		Stack0, Stack, SCCs0, SCCs) :-
	handle_calls(G1, Preds0, Preds1, Id0, Id1, Low0, Low1, Stack0, Stack1,
		     SCCs1, SCCs),
	handle_calls(G2, Preds1, Preds2, Id1, Id2, Low1, Low2, Stack1, Stack2,
		     SCCs2, SCCs1),
	handle_calls(G3, Preds2, Preds, Id2, Id, Low2, Low, Stack2, Stack,
		     SCCs0, SCCs2).


%  pop_stack(+Stack0, -Stack, +Pred, +Preds0, -Preds, -SCC)
%  SCC is the strongly-connected component currently constructed on Stack0,
%  and Stack is what is left when the elements of SCC have been popped off
%  Stack0.  Pred is the sentinal predicate which is the last one to pop.

pop_stack([Pred0|Stack0], Stack, Pred, Preds0, Preds, [Pred0|SCC]) :-
	put_pred_travnum(Pred0, 999999, Preds0, Preds1),
	(   Pred0 == Pred ->
		SCC = [],
		Stack = Stack0,
		Preds = Preds1
	;   pop_stack(Stack0, Stack, Pred, Preds1, Preds, SCC)
	).



/*************************************************************************

				 Testing Code

Read a file (defined in 'calls.pl'), computes the strongly-connected
components, and print the results, one component per line.

*************************************************************************/

test(File) :-
	file_contents(File, Preds),
	sccs(Preds, _, SCCs),
	write_sccs(SCCs, Preds).


write_sccs([], _) :-
	nl.
write_sccs([SCC|SCCs], Preds) :-
	write('    '),
	write_predspecs(SCC, Preds),
	nl,
	write_sccs(SCCs, Preds).


write_predspecs([], _).
write_predspecs([P|Ps], Preds) :-
	write_predspec(P, Preds),
	write_predspecs_rest(Ps, Preds).


write_predspecs_rest([], _).
write_predspecs_rest([P|Ps], Preds) :-
	write(', '),
	write_predspec(P, Preds),
	write_predspecs_rest(Ps, Preds).


write_predspec(Ref, Preds) :-
	get_old_pred_ref(Spec, Preds, Ref),
	write(Spec).
%  file    : topdown
%  Authors : Peter Schachte
%  Purpose : Top down part of Prolog groundness analysis program
%
%				Abstract
%
%  This is the top down part of a Prolog groundness analyzer.  When this is
%  called, we have already performed a bottom-up analysis, so we have
%  success patterns for all predicates.  This program finds predicate call
%  patterns.


:- module(topdown, [
	analyze_top_down/3,
	analyze_single_goal/3
   ]).


%:- use_module(analysis).
%:- use_module(predstore).
%  :- use_module(calls).				% for simplify_goal/7
%:- use_module(prep).
%:- use_module(library(basics)).



/*****************************************************************

			 Top-down Groundness Analysis

While one can think of the bottom-up part of the analysis as
collecting the analysis of lower parts of the call graph to produce
the analysis of the higher parts of the graph, top-down part of the
analysis can be thought of as distributing the analysis back down the
tree.  The conditions on completion of a call to a predicate, relative
to the call conditions determine, in part, the relative completion
conditions, i.e. the success pattern, of the predicates that call it.
Similarly, the call conditions for a predicate, together with the
success patterns of the predicates it calls, determine the call
patterns of the predicates it calls.

The top-down phase of groundness analysis, like the bottom-up phase, works on
one strongly-connected component at a time.  Of course, the SCCs are
processed in the opposite order.  For the top-down analysis, we also have
access to the work performed during the bottom-up analysis.

Top-down analysis of each SCC is divided into two phases.  First, we find the
correct call patterns for the predicates in that SCC.  This is done using the
the prep term resulting from the preparation phase of the bottom-up analysis.
Thus the first phase computes call patters for only the recursive calls.
Once a fixpoint is reached, we have the final call pattern for all the
predicates in that SCC.

The bottom-up preparation has already associated with each goal the glb of
the success analyses of the goals up to (but not including) that goal.  In
the second phase, we use this, along with the final call patterns for the
predicates in the same SCC, to compute the correct call patterns for all the
calls in that SCC.

Note: this code doesn't get the most precise possible results.  We find a
separate call pattern for each call, rather than only for each predicate,
which gives us strong results.  But that isn't enough: we must have a call
pattern for each predicate so that we can determine the call patterns for that
predicate's calls.  What we should do is do this computation separately for
each groundness pattern that predicate is called with.  Instead, we lub
together all the different call patterns to produce a single call pattern for
the predicate, using the call patterns for that predicate's calls.  This is
less than ideal.

In order to fix this, we should really do two things: adopt a representation
for boolean functions for which equality checking is very cheap (e.g., Bryant
graphs).  This would help because what we would need to do for each call is
check to see if the new call pattern is already on the list of call patterns
for that predicate, and only add it if not.  The second, more important thing,
is to compute functional dependencies for the predicates before the top-down
pass.  This would allow us to recognize call patterns that are "essentially"
the same.  For example, a call to functor/3 with all arguments ground is
essentially the same as one with only the first ground, because it can be
compiled as a call with the first argument supplied and the other two
temporaries, which are then unified (equality checked) against the two ground
arguments as supplied.  This also applies to user-supplied predicates.  This
is Somogyi's concept of implied modes.  It's not as simple as that, though;
for example, the first two arguments to append/3 imply the third, but one
wouldn't want to compile a call to append/3 with all arguments ground as a
call with a temporary third argument, which would then have to be
general-unified with the supplied argument.  It seems types as well as
functional dependencies are needed to do this properly.  The alternative is to
really generate different versions of code for each distinct call pattern,
which seems a bit much.

So for now I don't worry about this.

*****************************************************************/


%  analyze_single_goal(+Goal, +Preds0, -Preds)
%  Preds is the same as Preds0, except that it accounts for Goal as a possible
%  call pattern.  This is intended to be used for the top-level goal, before
%  the top-down analysis proper.

analyze_single_goal(Goal, Preds0, Preds) :-
%	simplify_goal(Goal, user, 1, _, Preds0, Preds1, Code),
	prepare_code(Goal, 0, Preds0, [], _),
	anal_top(Top),
	propagate_goal_calls(Goal, Top, _, [], Preds0, Preds).



%  analyze_top_down(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed top-down for call conditions.
%  SCCs is a list of strongly-connected components in the program call graph,
%  topologically sorted in top-down order.  Each SCC is represented as a list
%  of predicate references.

analyze_top_down([], Preds, Preds).
analyze_top_down([SCC|SCCs], Preds0, Preds) :-
	analyze_to_fixpoint(SCC, Preds0, Preds1),
	propagate_call_patterns(SCC, SCC, Preds1, Preds2),
	analyze_top_down(SCCs, Preds2, Preds).



/*======================================================================

			    The Fixpoint Iteration

Here we perform the fixpoint iteration for an SCC.  We do this by meeting the
fixed part of the analysis of each clause and the glb of current best
approximation of the call patterns of the variable parts.  We join this result
for each call with the current approximation for that predicate to determine
its next approximation.  If the previous and next approximations for each
predicate called from the SCC are the same, then we're done, otherwise we must
try again.

  ======================================================================*/


%  analyze_to_fixpoint(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed top-down for call
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.

analyze_to_fixpoint(SCC, Preds0, Preds) :-
	analyze_once(SCC, Preds0, Preds1, false, Changed),
	(   Changed == true ->
		analyze_to_fixpoint(SCC, Preds1, Preds)
	;   Preds = Preds1
	).


%  analyze_once(+SCC, +Preds0, -Preds, +Changed0, -Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCC have been analyzed once top-down for call
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.


analyze_once([], Preds, Preds, Changed, Changed).
analyze_once([Pred|SCC], Preds0, Preds, Changed0, Changed) :-
	get_pred_prep(Pred, Preds0, Prep),
	(   Prep = fixed(_) ->			% nonrecursive:  do no more!
		Changed = Changed0,
		Preds = Preds0
	;   get_pred_call(Pred, Preds0, Callpat),
	    analyze_prep(Prep, Callpat, _, Preds0, Preds1, Changed0,
			 Changed1),
	    analyze_once(SCC, Preds1, Preds, Changed1, Changed)
	).


%  analyze_prep(+Prep, +Analysis0, -Analysis, +Preds0, -Preds, +Changed0,
%		-Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Prep, a prep term, have had their call patterns
%  updated corresponding to their call(s) in Prep.  Analysis0 is the (current
%  approximation of) the pattern in which the Prep term would be used, and
%  Analysis is the pattern in which the next goal following Prep would be
%  called.  Changed is true if  Changed0 is, or if one of the predicates
%  specified on Prep gets a new call pattern due to this analysis.

analyze_prep(conj_prep(_,Var), Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	analyze_conj(Var, Anal0, Anal, Preds0, Preds, Ch0, Ch).
analyze_prep(disj_prep(_,Var,R), Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	anal_bottom(Bottom),
	analyze_disj(Var, R, Anal0, Bottom, Anal, Preds0, Preds, Ch0, Ch).
analyze_prep(call_prep(Pred,Call,Anal2,Restriction), Anal0, Anal, Preds0,
		Preds, Ch0, Ch) :-
	anal_meet(Anal0, Anal2, Anal3),
	analyze_call_pattern(Call, Anal3, Cpat),
	update_call_pattern(Pred, Cpat, Preds0, Preds, Ch0, Ch),
	% This is where we rely on our meet operation being commutitive.
	analyze_call(Pred, Preds, Call, Restriction, Anal0, Anal).


%  analyze_conj(+Preps, +Analysis0, -Analysis, +Preds0, -Preds, +Changed0,
%		-Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Preps, a list of prep terms, have had their call
%  patterns updated corresponding to their call(s) in Preps.  Analysis0 is the
%  (current approximation of) the pattern in which the Preps term would be
%  used, and Analysis is the pattern in which the next goal following Preps
%  would be called.  Changed is true if Changed0 is, or if one of the
%  predicates specified on Preps gets a new call pattern due to this analysis.

analyze_conj([], Anal, Anal, Preds, Preds, Ch, Ch).
analyze_conj([Prep|Preps], Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	analyze_prep(Prep, Anal0, Anal1, Preds0, Preds1, Ch0, Ch1),
	analyze_conj(Preps, Anal1, Anal, Preds1, Preds, Ch1, Ch).


%  analyze_disj(+Preps, +Restriction, +Context, +Analysis0, -Analysis,
%		+Preds0, -Preds, +Changed0, -Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Preps, a list of prep terms, have had their call
%  patterns updated corresponding to their call(s) in Preps.  Context is the
%  (current approximation of) the pattern in which the Preps term would be
%  used.  Analysis is Analysis0 joined with the lub of the analysis of Preps.
%  Changed is true if Changed0 is, or if one of the predicates specified on
%  Preps gets a new call pattern due to this analysis.

analyze_disj([], R, _, Anal0, Anal, Preds, Preds, Ch, Ch) :-
	anal_restrict(Anal0, R, Anal).
analyze_disj([Prep|Preps], R, Context, Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	analyze_prep(Prep, Context, Anal1, Preds0, Preds1, Ch0, Ch1),
	(   Preps == [] ->
		Preds = Preds1,
		Ch = Ch1,
		anal_join(Anal0, Anal1, R, Anal)
	;   anal_join(Anal0, Anal1, Anal2),
	    analyze_disj(Preps, R, Context, Anal2, Anal, Preds1, Preds, Ch1,
			 Ch)
	).



%  update_call_pattern(Pred, Callpat, Preds0, Preds, Changed0, Changed)
%  Preds is the same as Preds0, except that it now notes that predicate Pred
%  may be called with call pattern Callpat.  Changed is true Changed0 is, or
%  if Preds is different than Preds0.

update_call_pattern(Pred, Callpat, Preds0, Preds, Changed0, Changed) :-
	get_pred_call(Pred, Preds0, Callpat0),
	anal_join(Callpat0, Callpat, Callpat2),
	(   Changed0 == true ->
		Changed = Changed0,
		put_pred_call(Pred, Callpat2, Preds0, Preds)
	;   Callpat0 = Callpat2 ->
		Changed = Changed0,
		Preds = Preds0
	;   Changed = true,
	    put_pred_call(Pred, Callpat2, Preds0, Preds)
	).



/*======================================================================

			     Analysis Propagation

Once we've completed the call pattern analysis of an SCC, we must
propagate the call pattern information to all the predicates called
from the SCC.  We can ignore calls to predicates in the SCC itself,
since we've handled those calls in the fixpoint computation.  This
part is the complement of the fixpoint analysis:  there we were
interested only in recursive calls, here we only handle nonrecursive
calls.  This can be viewed as the dual of the preparation part of the
bottom-up analysis phase.

Here we again make use of the information computed during the
preparation part of the bottom-up analysis phase.  During that part of
the analysis, we computed a partial call pattern for each call, which
excluded the call pattern for the predicate being analyzed and the
success pattern for the recursive calls appearing earlier in the
clause, since that information was not available at that stage.  Now
it is.  Thus all we need to do at this point is to collect the missing
information and combine it with the information already stored to give
the final answer.

This approach isn't as optimal as it may seem, since we still have to
perform a meet for each call.  Still it's a saving, because had we not
stored that partial information, we'd have to perform a meet for each
unification and builtin, too.

  ======================================================================*/

%  propagate_call_patterns(+List, +SCC, +Preds0, -Preds)
%  Preds is the same predstore as Preds0, except that all predicates not on
%  the list SCC and called by predicates whose pred refs are on List have had
%  their call patterns updated to reflect those calls.

propagate_call_patterns([], _, Preds, Preds).
propagate_call_patterns([Pred|List], SCC, Preds0, Preds) :-
	get_pred_call(Pred, Preds0, Callpat),
	get_pred_code(Pred, Preds0, Code),
	propagate_goal_calls(Code, Callpat, _, SCC, Preds0, Preds1),
	propagate_call_patterns(List, SCC, Preds1, Preds).


%  propagate_goal_calls(+Code, +Callpat, -Exitpat, +SCC, +Preds0, -Preds)

propagate_goal_calls(undefined, Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(foreign(_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(conj(Goals), Callpat, Exitpat, SCC, Preds0, Preds) :-
	propagate_conj_calls(Goals, Callpat, Exitpat, SCC, Preds0, Preds).
propagate_goal_calls(equal(_,_,_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(eval(_,_,_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(builtin(_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(call(P,T,A,R), Callpat, Exitpat, SCC, Preds0, Preds) :-
	(   member(P, SCC) ->
		% recursive call:  add success pattern to call pattern to find
		% the next goal's call pattern
		Preds = Preds0,
		analyze_call(P, Preds, T, R, Callpat, Exitpat)
	;   % non-recursive call:  meet current call pattern with existing
	    % analysis to determine actual call pattern, and update Preds.
	    % Note:  here we rely on meet being commutative.
	    Callpat = Exitpat,
	    anal_meet(Callpat, A, Context),
	    analyze_call_pattern(T, Context, Actualcall),
	    simple_update_call_pattern(P, Actualcall, Preds0, Preds)
	).
propagate_goal_calls(!, Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(disj(Goals), Callpat, Exitpat, SCC, Preds0, Preds) :-
	anal_bottom(Bottom),
	propagate_disj_calls(Goals, Callpat, Bottom, Exitpat, SCC, Preds0,
			     Preds).
propagate_goal_calls(if_then_else(G1,G2,G3), Callpat, Exitpat, SCC, Preds0,
		Preds) :-
	anal_bottom(Bottom),
	propagate_disj_calls([conj([G1,G2]),G3],Callpat, Bottom, Exitpat, SCC,
			     Preds0, Preds).


%  propagate_conj_calls(+Goals, +Callpat, -Exitpat, +SCC, +Preds0, -Preds)

propagate_conj_calls([], Exitpat, Exitpat, _, Preds, Preds).
propagate_conj_calls([Goal|Goals], Callpat, Exitpat, SCC, Preds0, Preds) :-
	propagate_goal_calls(Goal, Callpat, Callpat1, SCC, Preds0, Preds1),
	propagate_conj_calls(Goals, Callpat1, Exitpat, SCC, Preds1, Preds).


%  propagate_disj_calls(+Goals, +Context, +Callpat, -Exitpat, +SCC, +Preds0,
%		-Preds)

propagate_disj_calls([], _, Exitpat, Exitpat, _, Preds, Preds).
propagate_disj_calls([Goal|Goals], Context, Callpat, Exitpat, SCC, Preds0,
		Preds) :-
	propagate_goal_calls(Goal, Context, Exitpat1, SCC, Preds0, Preds1),
	anal_join(Callpat, Exitpat1, Callpat1),
	propagate_disj_calls(Goals, Context, Callpat1, Exitpat, SCC, Preds1,
			     Preds).



%  simple_update_call_pattern(Pred, Callpat, Preds0, Preds)
%  Preds is `<the same as Preds0, except that it now notes that predicate Pred
%  may be called with call pattern Callpat.

simple_update_call_pattern(Pred, Callpat, Preds0, Preds) :-
	get_pred_call(Pred, Preds0, Callpat0),
	anal_join(Callpat0, Callpat, Callpat2),
	put_pred_call(Pred, Callpat2, Preds0, Preds).
