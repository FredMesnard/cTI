even(s(s(X))) :- even(X).
even(0).
lte(s(X),s(Y)) :- lte(X,Y).
lte(0,Y).
goal :- lte(X,s(s(s(s(0))))), even(X).
