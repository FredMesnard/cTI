minsort([],[]).
minsort(L,[X|L1]) :- min1(X,L), remove(X,L,L2), minsort(L2,L1).
min1(M,[X|L]) :- min2(X,M,L).
min2(X,X,[]).
min2(X,A,[M|L]) :- min(X,M,B),min2(B,A,L).
min(X,Y,X) :- X=<Y.
min(X,Y,Y) :- X>Y.
%remove(N,[],[]).   (this case cannot occur in our program)
remove(N,[N|L],L).
remove(N,[M|L],[M|L1]) :- N=\=M,remove(N,L,L1).

