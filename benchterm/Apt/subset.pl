member(X,[Y|Xs]) :- member(X,Xs).
member(X,[X|Xs]).
subset([X|Xs],Ys) :- member(X,Ys), subset(Xs,Ys).
subset([],Ys).
