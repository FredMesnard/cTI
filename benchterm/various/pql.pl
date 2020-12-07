l(X,s(X)).
p(X,s(X)).
q(X,Y) :- l(X,Y), p(X,Y).
p(X,Y) :- q(W,X).

