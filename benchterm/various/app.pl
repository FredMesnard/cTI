append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-append(Xs,Ys,Zs).

member(X,Xs):-append(_,[X|_],Xs).

subset([],_).
subset([X|Xs],Ys):-member(X,Ys),subset(Xs,Ys).

s1(Xs,Ys,Zs):-
	subset(A,Xs),
	subset(B,Ys),
	append(A,B,Zs).

s2(Xs,Ys,Zs):-
	append(Xs,Ys,T),
	subset(T,Zs).

test(X):-X is 2*3.
