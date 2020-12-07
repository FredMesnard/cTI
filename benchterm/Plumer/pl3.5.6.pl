p(X) :- l(X), q(X).
q([A]).
r(1).
l([]).
l([H|T]) :- r(H),l(T).
