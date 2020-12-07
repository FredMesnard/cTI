trans([],[]).
trans([Xs|Xss],[Ys|Yss]):-
	%cti:{n(Xs)+n(Xss)=n(Ys)+n(Yss)},
	ht([Xs|Xss],Ys,Xss1),
	trans(Xss1,Yss).

ht([],[],[]).
ht([[X]|Xss],[X|Ys],Zss):-
	ht(Xss,Ys,Zss).
ht([[X,X1|Xs]|Xss],[X|Ys],[[X1|Xs]|Zss]):-
	ht(Xss,Ys,Zss).

