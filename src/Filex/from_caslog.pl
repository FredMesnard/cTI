unary_constraint(V,V,1,0).
unary_constraint(C,_,0,C) :- number(C).
/*
unary_constraint(-C,V,NTerm,NConst) :-
	unary_constraint(C,V,Term,Const),
	minus(Term,NTerm),
	minus(Const,NConst).*/
	
unary_constraint(C1+C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	addition(Term1,Term2,Term),
	addition(Const1,Const2,Const).
/*
unary_constraint(C1-C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	subtraction(Term1,Term2,Term),
	subtraction(Const1,Const2,Const).
unary_constraint(C1*C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	multiply(Term1,Const2,M1),
	multiply(Term2,Const1,M2),
	addition(M1,M2,Term),
	multiply(Const1,Const2,Const).
unary_constraint(C,V) :-
	variable(C), C \== V, fail.
*/

minus(bot,bot).
minus(inf,bot).
minus(X,Y) :-
	number(X),!,
	(X =:= 0 ->
		Y = 0;
		Y is -X).
minus(X,-X) :-
	/* not number(X), */
	X \== bot,
	X \== inf.

addition(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1+A2.
addition(bot,bot,bot).
addition(A1,bot,bot) :-
	A1 \== bot.
addition(bot,A2,bot) :-
	A2 \== bot.
addition(inf,inf,inf).
addition(A1,inf,inf) :-
	A1 \== inf,
	A1 \== bot.
addition(inf,A2,inf) :-
	A2 \== inf,
	A2 \== bot.
addition(A1,A2,A1) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
addition(A1,A2,A2) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
addition(A1,A2,A1+A2) :-
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1),
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
addition(A1,A2,A1+A2) :-
	number(A1),
	A1 =\= 0,
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
addition(A1,A2,A1+A2) :-
	number(A2),
	A2 =\= 0,
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1).


subtraction(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1-A2.
subtraction(bot,bot,bot).
subtraction(A1,bot,bot) :-
	A1 \== bot.
subtraction(bot,A2,bot) :-
	A2 \== bot.
subtraction(A1,A2,A1) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== bot.
subtraction(A1,A2,-A2) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	nonnumber(A1),
	nonnumber(A2),
	A1 \== bot,
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	number(A1),
	A1 =\= 0,
	nonnumber(A2),
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	nonnumber(A1),
	A1 \== bot,
	number(A2),
	A2 =\= 0.

multiply(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1*A2.
multiply(bot,bot,bot).
multiply(A1,bot,bot) :-
	A1 \== bot.
multiply(bot,A2,bot) :-
	A2 \== bot.
multiply(inf,inf,inf).
multiply(A1,inf,inf) :-
	A1 \== inf,
	A1 \== bot.
multiply(inf,A2,inf) :-
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A2) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
multiply(A1,A2,A1) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A1) :-
	number(A2),
	A2 =:= 1,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
multiply(A1,A2,A2) :-
	number(A1),
	A1 =:= 1,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A1*A2) :-
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1),
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
multiply(A1,A2,A1*A2) :-
	number(A1),
	A1 =\= 0,
	A1 =\= 1,
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
multiply(A1,A2,A1*A2) :-
	number(A2),
	A2 =\= 0,
	A2 =\= 1,
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1).

nonnumber(Term) :-
	var(Term).
nonnumber(Term) :-
	compound(Term).
nonnumber(Term) :-
	atom(Term).

variable($(_)).
variable($(_,_)).
