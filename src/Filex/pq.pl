p:-q.
q.
/*
p(X,Y) :- q(X,Y).
q(s(X),s(Y)) :- p(X,Y).
p(0,0).

r(s(X)):- r(X).

s(s(X),s(Y)):-s(X,Y).

t(_).

u :- fail.

v(X) :- v(X).
*/
w(X):-x(X).