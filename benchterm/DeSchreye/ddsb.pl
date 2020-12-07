%:- import(prolog_types).
%:- import(prolog_arith).
%:- import(prolog_meta).

%:- pred append(list(T),list(T),list(T)).

append([A|X],Y,[A|Z]) :- append(X,Y,Z).
append([],Y,Y).

%sublist(X1,X2) :- append(X3,X1,X4),append(X4,X5,X2).

%goal(Y1,Y2) :- sublist([a|Y1],[Y2]).
%goal(Y1,Y2) :- sublist3([a|Y1],[Y2]).

%:- pred sublist2(list(atom),list(atom)).

sublist2(X,Y) :- append(X2,X3,Y),append(X1,X,X2).
sublist3(X,Y) :- append(X,X1,X2), append(X3,X2,Y).

