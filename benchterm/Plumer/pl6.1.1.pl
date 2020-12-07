qsort([],[]).
qsort([H|L],S) :- split(L,H,A,B), qsort(A,A1),qsort(B,B1),append(A1,[H|B1],S).

split([],Y,[],[]).
split([X|Xs],Y,[X|Ls],Bs) :- X =< Y, split(Xs,Y,Ls,Bs).
split([X|Xs],Y,Ls,[X|Bs]) :- X > Y, split(Xs,Y,Ls,Bs).

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).
