reach(X,Y,Edges) :- member([X,Y],Edges).
reach(X,Z,Edges) :- member([X,Y],Edges),reach(Y,Z,Edges).
member(H,[H|L]).
member(X,[H|L]) :- member(X,L).
