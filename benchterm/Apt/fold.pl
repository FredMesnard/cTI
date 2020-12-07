fold(X,[Y|Ys],Z) :- op2(X,Y,V), fold(V,Ys,Z).
fold(X,[],X).
op2(a,b,c).
