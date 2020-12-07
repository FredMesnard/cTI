perm([],[]).
perm(L,[H|T]) :- append(V,[H|U],L),
                 append(V,U,W),
                 perm(W,T).
append([],Y,Y).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
