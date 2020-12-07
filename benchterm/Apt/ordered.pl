ordered([]).
ordered([X]).
ordered([X,Y|Xs]) :- X=<Y, ordered([Y|Xs]).
