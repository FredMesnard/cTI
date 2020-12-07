unary_constraint(C,_,0,C) :- number(C).
unary_constraint(C1+C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	addition(Term1,Term2,Term),
	addition(Const1,Const2,Const).


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


nonnumber(Term) :-
	var(Term).

