member(X,[X|_]).
member(X,[_|Xs]):-member(X,Xs).

rq(L):-setof(X,member(X,[1,2,3]),L).

t(L) :- rq(L),member(_,L).