% These programs are written to be suitable for meta-interpretation.
% They are not Prolog programs, but are at the object level.

% <- is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% MERGE SORT
msort([],[]) <- true.
msort([X],[X]) <- true.
msort(L,LS) <-
   L = [_,_|_] &       % L has at least two elements
   split(L,L1,L2) &
   msort(L1,S1) &
   msort(L2,S2) &
   merge(S1,S2,LS).

split([],[],[]) <- true.
split([H],[H],[]) <- true.
split([H1,H2|T],[H1|T1],[H2|T2]) <-
   split(T,T1,T2).

merge([],L,L) <- true.
merge(L,[],L) <- true.
merge([A1|L1], [A2|L2], [A1|L3]) <-
   A1 =< A2 &
   merge(L1, [A2|L2], L3).
merge([A1|L1], [A2|L2], [A2|L3]) <-
   A1 > A2 &
   merge([A1|L1], L2,L3).

% QUICK SORT
qsort([],[]) <- true.
qsort([H|T],S) <-
   partn(H,T,L1,L2) &
   qsort(L1,S1) &
   qsort(L2,S2) &
   append(S1,[H|S2],S).

append([],L,L)  <- true.
append([A|X],Y,[A|Z]) <-
   append(X,Y,Z).

partn(_,[],[],[]) <- true.
partn(P,[A|L],[A|L1],L2) <-
   A < P &
   partn(P,L,L1,L2).
partn(P,[A|L],L1,[A|L2]) <-
   A >= P &
   partn(P,L,L1,L2).
