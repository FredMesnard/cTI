%:- import(prolog_types).

%:- pred duplicate(list(T),list(T)).


duplicate([],[]).
duplicate([X|Y],[X,X|Z]) :- duplicate(Y,Z).
