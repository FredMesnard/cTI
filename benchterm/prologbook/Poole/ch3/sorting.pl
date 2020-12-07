% Computational Intelligence: a logical approach. 
% Prolog Code. Sorting code.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% MERGE SORT
msort([],[]).
msort([X],[X]).
msort(L,LS) :-
   L = [_,_|_],
   split(L,L1,L2),
   msort(L1,S1),
   msort(L2,S2),
   merge(S1,S2,LS).

split([],[],[]).
split([H],[H],[]).
split([H1,H2|T],[H1|T1],[H2|T2]) :-
   split(T,T1,T2).

merge([],L,L).
merge(L,[],L).
merge([A1|L1], [A2|L2], [A1|L3]) :-
   A1 =< A2,
   merge(L1, [A2|L2], L3).
merge([A1|L1], [A2|L2], [A2|L3]) :-
   A1 > A2,
   merge([A1|L1], L2,L3).

% QUICK SORT
qsort([],[]).
qsort([H|T],S) :-
   partn(H,T,L1,L2),
   qsort(L1,S1),
   qsort(L2,S2),
   append(S1,[H|S2],S).

append([],L,L).
append([A|X],Y,[A|Z]) :-
   append(X,Y,Z).

partn(_,[],[],[]).
partn(P,[A|L],[A|L1],L2) :-
   A < P,
   partn(P,L,L1,L2).
partn(P,[A|L],L1,[A|L2]) :-
   A >= P,
   partn(P,L,L1,L2).
