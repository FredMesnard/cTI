mergesort([],[]).
mergesort([X],[X]).
mergesort([X,Y|Xs],Ys) :- split2([X,Y|Xs],X1s,X2s),
                          mergesort(X1s,Y1s),
                          mergesort(X2s,Y2s),
                          merge(Y1s,Y2s,Ys).

split(Xs,Ys,Zs) :- split0(Xs,Ys,Zs).
split(Xs,Ys,Zs) :- split1(Xs,Ys,Zs).
split(Xs,Ys,Zs) :- split2(Xs,Ys,Zs).

split0([],[],[]).
split1([X],[X],[]).
split2([X,Y|Xs],[X|Ys],[Y|Zs]) :- split(Xs,Ys,Zs).

merge([],Xs,Xs).
merge(Xs,[],Xs).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y, merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X>Y, merge([X|Xs],Ys,Zs).

