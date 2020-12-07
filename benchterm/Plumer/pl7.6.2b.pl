reach(X,Y,Edges,Visited) :- member([X,Y],Edges).
reach(X,Z,Edges,Visited) :- member([X,Y],Edges),
                            \+member(Y,Visited),
                            reach(Y,Z,Edges,[Y|Visited]).
member(H,[H|L]).
member(X,[H|L]) :- member(X,L).
