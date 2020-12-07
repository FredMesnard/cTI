append([],X,X).
append([U|Y],V,[U|Z]) :- append(Y,V,Z).

sublist(X1,X2) :- append(X3,X1,X4),append(X4,X5,X2).

goal1(Y1,Y2) :- sublist([a|Y1],[Y2]).

goal2(Y1,Y2) :- sublist3([a|Y1],[Y2]).

sublist2(X,Y) :- append(X2,X3,Y),append(X1,X,X2).
sublist3(X,Y) :- append(X,X1,X2), append(X3,X2,Y).

