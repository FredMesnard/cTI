%elem(X,f(Y,_)) :- elem(X,Y).
elem(X,f(_,C)) :- !, elem(X,C).
elem(X,X).
