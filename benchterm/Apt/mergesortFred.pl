mergesort([],[]).
mergesort([X],[X]):-number(X).
mergesort([X,Y|Xs],Ys) :- split([X,Y|Xs],X1s,X2s),
              mergesort(X1s,Y1s), mergesort(X2s,Y2s), merge(Y1s,Y2s,Ys).

split([],[],[]).
split([X|Xs],[X|Ys],Zs) :- split(Xs,Zs,Ys).

merge([],Xs,Xs):-list(Xs).
merge(Xs,[],Xs):-list(Xs).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y, merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X>Y, merge([X|Xs],Ys,Zs).

list([]).
list([X|Xs]):-number(X),list(Xs).
