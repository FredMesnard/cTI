% Computational Intelligence: a logical approach. 
% Prolog Code. The natural language interface of Figure 3.9.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    modifiers(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    pp(T3,T4,Obj,C3,C4).

det(T,T,_,C,C).
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).

modifiers(T,T,_,C,C).
modifiers(T0,T2,Obj,C0,C2) :-
    adjective(T0,T1,Obj,C0,C1),
    modifiers(T1,T2,Obj,C1,C2).

adjective([computer, science | T],T,Obj,C,[dept(Obj,comp_sci)|C]).
adjective([female | T],T,Obj,C,[female(Obj)|C]).

noun([course | T],T,Obj,C,[course(Obj)|C]).
noun([student | T],T,Obj,C,[student(Obj)|C]).
noun([X | T],T,X,C,C) :- course(X); student(X).
noun([cs312 | T],T,312,C,C).

pp(T,T,_,C,C).
pp(T1,T2,O,C0,C1) :-
   prep_phrase(T1,T2,O,C0,C1).

prep_phrase(T0,T2,O1,C0,C2) :-
    prep(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).

prep([enrolled, in | T],T,O1,O2,C,[enrolled_in(O1,O2)|C]).

question([is | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    prep_phrase(T1,T2,Obj,C1,C2).
question([who,is | T0],T1,Obj,C0,C1) :-
    prep_phrase(T0,T1,Obj,C0,C1).
question([who,is | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).

ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

prove_all([]).
prove_all([H|T]) :-
    H,
    prove_all(T).


course(312).
course(322).
course(315).

dept(312,comp_sci).
dept(322,comp_sci).
dept(315,math).

enrolled_in(john,312).
enrolled_in(mary,312).
enrolled_in(jane,315).
enrolled_in(sally,322).

female(mary).
female(jane).
female(sally).

student(mary).
student(jane).
student(sally).
student(john).


/* Try the following queries
| ?- ask([is,john,enrolled,in,cs312],_).
| ?- ask([who,is,a,student],A).
| ?- ask([is,john,enrolled,in,a,computer,science,course],_).
| ?- ask([who,is,enrolled,in,a,computer,science,course],A).
| ?- ask([who,is,a,female,student,enrolled,in,a,computer,science,course],A).
*/
