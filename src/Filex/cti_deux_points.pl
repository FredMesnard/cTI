p(N) :- cti:{N>0,A=0}.
p(N) :- cti:{ M = N-1}, p(M).
p(N) :- cti:{N > 1 , 2*A =< N, N =< 2*A+1, Z = N-A}, p(A), p(Z).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-append(Xs,Ys,Zs).

top :- cti:{n(L1) < 10},append(L1,Zs,L2),cti:{b(L2)},append(Xs,Ys,L2).
