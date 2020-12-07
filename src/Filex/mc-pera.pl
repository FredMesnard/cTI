p([],[]).
p(Xs,Ys):-%cti:{n(Zs)=n(Ms)},
	eq(Xs,Ys),rev(Ys,[Z|Zs]),p(Zs,Ms).

eq(A,A).

rev([],[]).
rev([X|Xs],Ys):-rev(Xs,Ts),app(Ts,[X],Ys).

app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]):-app(Xs,Ys,Zs).