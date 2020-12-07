
sequence(L):-L=[x,x,x,x,x,x,x,x,x,
		x,x,x,x,x,x,x,x,x,
		x,x,x,x,x,x,x,x,x].

q(S):-sequence(S),
	sublist([1,x,1,x,1],S),
	sublist([2,x,x,2,x,x,2],S),
	sublist([3,x,x,x,3,x,x,x,3],S),
	sublist([4,x,x,x,x,4,x,x,x,x,4],S),
	sublist([5,x,x,x,x,x,5,x,x,x,x,x,5],S),
	sublist([6,x,x,x,x,x,x,6,x,x,x,x,x,x,6],S),
	sublist([7,x,x,x,x,x,x,x,7,x,x,x,x,x,x,x,7],S),
	sublist([8,x,x,x,x,x,x,x,x,8,x,x,x,x,x,x,x,x,8],S),
	sublist([9,x,x,x,x,x,x,x,x,x,9,x,x,x,x,x,x,x,x,x,9],S).

sublist(Xs,Ys):-append(_,Zs,Ys),append(Xs,_,Zs).

append([],Ys,Ys).
append([x|Xs],Ys,[x|Zs]):-append(Xs,Ys,Zs).

