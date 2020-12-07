app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).
app([],Ys,Ys).

reverse([X|Xs],Ys) :- reverse(Xs,Zs), app(Zs,[X],Ys).
reverse([],[]).
