% This version of the blocs program ("Kowalski's formulation") is taken from 
% [Nils 80].

poss(s0).
holds(on(a,d),s0).
holds(on(b,e),s0).
holds(on(c,f),s0).
holds(clear(a),s0).
holds(clear(b),s0).
holds(clear(c),s0).
holds(clear(g),s0).

holds(clear(X),do(trans(X,Y,Z),S)).
holds(clear(Y),do(trans(X,Y,Z),S)) .
holds(on(X,Z),do(trans(X,Y,Z),S))  .

pact(trans(X,Y,Z),S) :-	
	holds(clear(X),S),
	holds(clear(Z),S),
	holds(on(X,Y),S),
	diff(X,Z).

poss(do(U,S)) :-
	poss(S),
	pact(U,S).

holds(V,do(trans(X,Y,Z),S)) :-
	holds(V,S),
	diff(V,clear(Z)),
	diff(V,on(X,Y)).

tower(X,Y,Z,S) :- poss(S), 
		  holds(on(Y,Z),S),
		  holds(on(X,Y),S).

eq(X,X).
diff(X,Y) :- \+eq(X,Y).


                 
