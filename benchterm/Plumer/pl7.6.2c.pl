reach(X,Y,Edges,Not_Visited) :- member([X,Y],Edges).
reach(X,Z,Edges,Not_Visited) :- member([X,Y],Edges),
                                member(Y,Not_Visited),
                                delete(Y,Not_Visited,V1),
                                reach(Y,Z,Edges,V1).

member(H,[H|L]).
member(X,[H|L]) :- member(X,L).

delete(X,[X|Y],Y).
delete(X,[H|T1],[H|T2]) :- delete(X,T1,T2).
