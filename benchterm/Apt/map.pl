p(val_i,val_j).
map([X|Xs],[Y|Ys]) :- p(X,Y),map(Xs,Ys).
map([],[]).
