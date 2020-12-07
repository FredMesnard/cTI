%:- import(prolog_types).

%:- pred append(list(T),list(T),list(T)).

append([H|X],Y,[H|Z]) :- append(X,Y,Z).
append([],Y,Y).

