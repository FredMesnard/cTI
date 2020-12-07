app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).
app([],Ys,Ys).
