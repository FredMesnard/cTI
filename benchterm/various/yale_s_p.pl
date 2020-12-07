%from K. R. Apt and M. Bezem, Acyclic Programs
%New Generation Computing 9(1991), pp.335-363

holds(alive,[]).
holds(loaded,[load|X]).
holds(dead,[shoot|X]) :- holds(loaded,X).
holds(X,[Y|Z]) :- \+ab(X,Y,Z),holds(X,Z).

ab(alive,shoot,X) :- holds(loaded,X).
