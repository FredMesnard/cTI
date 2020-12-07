:- module(trad_cd,[trad_cd/2,trad_cd1/2]).

:- use_module(library(lists),[append/3]).
:- use_module(library(terms)).
	      
:- use_module(num_op,[]).
:- use_module(bool_op,[]).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trad

trad_cd(C,_) :- var(C),!,throw(cti_exception(prolog_trad_cd,variables1)).
trad_cd((C,D),(Ct,Dt)) :- !,trad_cd1(C,Ct),trad_cd(D,Dt).
trad_cd(C,Ct) :- trad_cd1(C,Ct),!.
trad_cd(C,_) :- throw(cti_exception(prolog_trad_cd,msg(C,' cannot be parsed'))).

%%%
trad_cd1(A = B,'$num'(C2)) :-
	num(A,A2),num(B,B2),
	term_variables(A-B,Vars),tous_pos(Vars,Cpos),
	(num_op:project(Vars,[A2=B2|Cpos],Vars,C) -> C2=C ; num_op:false(C2)).
trad_cd1(A >= B,'$num'(C2)) :-
	num(A,A2),num(B,B2),
	term_variables(A-B,Vars),tous_pos(Vars,Cpos),
	(num_op:project(Vars,[A2 >= B2|Cpos],Vars,C) -> C2=C ; num_op:false(C2)).
trad_cd1(A > B,'$num'(C2)) :-
	num(A,A2),num(B,B2),
	term_variables(A-B,Vars),tous_pos(Vars,Cpos),
	(num_op:project(Vars,[A2 >= B2+1|Cpos],Vars,C) -> C2=C ; num_op:false(C2)).
trad_cd1(A =< B,'$num'(C2)) :-
	num(A,A2),num(B,B2),
	term_variables(A-B,Vars),tous_pos(Vars,Cpos),
	(num_op:project(Vars,[A2 =< B2|Cpos],Vars,C) -> C2=C ; num_op:false(C2)).
trad_cd1(A < B,'$num'(C2)) :-
	num(A,A2),num(B,B2),
	term_variables(A-B,Vars),tous_pos(Vars,Cpos),
	(num_op:project(Vars,[A2+1 =< B2|Cpos],Vars,C) -> C2=C ; num_op:false(C2)).

%%%
trad_cd1(b(X),'$bool'(X)).
trad_cd1((A,B),'$bool'(A1*B1)) :- bool(A,A1),bool(B,B1).
trad_cd1((A;B),'$bool'(A1+B1)) :- bool(A,A1),bool(B,B1).
trad_cd1((A -> B),'$bool'(A1 =< B1)) :- bool(A,A1),bool(B,B1).

%%%
trad_cd1(tc(TC),'$term_cond'(TC)) :- tc(TC).

%%%
num(X,X) :- var(X),!.
num(N,N) :- number(N),!.
num(n(X),X).
num(-E,-X) :- num(E,X).
num(A+B,X+Y) :- num(A,X),num(B,Y).
num(A-B,X-Y) :- num(A,X),num(B,Y).
num(A*B,X*Y) :- num(A,X),num(B,Y).

%%%
bool(b(X),X).
bool((B1,B2),(X*Y)) :- bool(B1,X),bool(B2,Y).
bool((B1;B2),(X+Y)) :- bool(B1,X),bool(B2,Y).
bool((B1 -> B2),(X =< Y)) :- bool(B1,X),bool(B2,Y).

%%%
tc(X) :- var(X),!.
tc(0).
tc(1).
tc(S+B) :- tc(S),tc1(B).
tc(P1*P2) :- tc1(P1*P2).

tc1(X) :- var(X),!.
tc1(0).
tc1(1).
tc1(P*X) :-var(X),tc1(P).
