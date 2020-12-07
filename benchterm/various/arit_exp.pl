e(X+Y) :- f(X),e(Y).
e(X) :- f(X).
f(X*Y) :- g(X),f(Y).
f(X) :- g(X).
g(-(X)) :- e(X).
g(X) :- integer(X).
