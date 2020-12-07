overlap(Xs,Ys) :- member(X,Xs),member(X,Ys).
has_a_or_b(Xs) :- overlap(Xs,[a,b]).

member(X,[Y|Xs]) :- member(X,Xs).
member(X,[X|Xs]).
