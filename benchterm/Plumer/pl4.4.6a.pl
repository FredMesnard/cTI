perm([],[]).
perm([X|L],Z) :- perm(L,Y), insert(X,Y,Z).
insert(X,[],[X]).
insert(X,L,[X|L]).
insert(X,[H|L1],[H|L2]) :- insert(X,L1,L2).
