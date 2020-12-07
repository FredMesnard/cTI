% Computational Intelligence: a logical approach. 
% Prolog Code. Section 3.4.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% append(X,Y,Z) is true when X, Y and Z are lists
% and Z contains the elements of X (in order)
% followed by the elements of Y (in order)
append([],Z,Z).
append([A|X],Y,[A|Z]) :-
   append(X,Y,Z).

% member(X,L) is true if X is an element of list L
member(X,[X|L]).
member(X,[H|R]):-
   member(X,R).

% notin(X,L) is true if X is not an element of list
% L --- or that X is different from every member of L.
notin(A,[]).
notin(A,[H|T]):-
   different(A, H),
   notin(A,T).

% rev(L,R) that is true if list R contains the same
% elements as list L, but in reverse order.
rev([],[]).
rev([H|T],R):-
   rev(T,RT),
   append(RT,[H],R).

% reverse(L,R) is true if R contains the lements of
% L, but in reverse order.
reverse(L,R):-
   rev3(L,[],R).

% rev3(L,A,R) is true if R contains the elements of
% L in reverse order followed by the elements of A.
rev3([],L,L).
rev3([H|T],A,R):-
   rev3(T,[H|A],R).

% different(X,Y) is true if X and Y denote different objects
different(a,b).
different(a,c).
different(b,a).
different(b,c).
different(c,a).
different(c,b).
