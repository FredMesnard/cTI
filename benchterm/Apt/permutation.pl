app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).
app([],Ys,Ys).

perm(Xs,[X|Ys]) :- app(X1s,[X|X2s],Xs),
                   app(X1s,X2s,Zs),
                   perm(Zs,Ys).
perm([],[]).
