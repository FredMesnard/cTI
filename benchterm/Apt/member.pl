member(X,[Y|Xs]) :- member(X,Xs).
member(X,[X|Xs]).
