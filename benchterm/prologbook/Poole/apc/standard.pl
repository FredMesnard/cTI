% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Standard BUILT-INS (from Appendix B) assumed in other code
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% notin(X,L)
notin(_,[]).
notin(A,[H|T]) :-
   \+ A = H,
   notin(A,T).

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) :-
   member(A,L),
   subset(B,L).

% append(A,B,R) is true if R is the list containing the 
% elements of A followed by the elements of B
append([],R,R).
append([H|T],L,[H|R]) :-
   append(T,L,R).

% nth(N,L,E) is true if E is the Nth element of list L
% this requires N to be bound.
nth(1,[E|_],E) :- !.
nth(N,[_|R],E) :-
   N1 is N-1,
   nth(N1,R,E).

% remove(E,L,R) is true if E is an element of L and R is the remaining
% elements after E is removed.
remove(E,[E|R],R).
remove(E,[A|L],[A|R]) :-
   remove(E,L,R).

% min(A,B,M) is true if M is the minimum of A and M
min(A,B,A) :- 
   A < B,!.
min(A,B,B) :- 
   A >= B.

% insert(E,L,L1) inserts E into L producing L1 
% E is not added it is already there.
% this assumes that E and L are ground
insert(X,[],[X]).
insert(A,[A|R],[A|R]) :- !.
insert(A,[B|R],[B|R1]) :-
%   A \== B,
   insert(A,R,R1).

% writel(L) is true if L is a list of items 
% to be written on a line.
writel([]).
writel([H|T]) :- write(H), writel(T).

% writeln(L) is true if L is a list of items 
% to be written on a line, followed by a newline.
writeln(L) :- 
   writel(L),
   nl.

:- dynamic previndex/1.
previndex(0).

newindex(I1) :-
   retract(previndex(I)),
   I1 is I+1,
   assert(previndex(I1)).

% accumulate(Goal,Init,Prev,Acc,Next,Result)
% accumulates information for each success of Goal.
% Init is the start. Acc is a predicate that shows
% how to derive Next accumulation from Prev
% accumulation. Result is the final accumulation.

accumulate(G,I,P,CN,N,Res) :-
   newindex(Ind),
   accumulate(Ind,G,I,P,CN,N,Res).

accumulate(Ind,G,I,P,CN,N,_) :-
   assert(result(Ind,I)),
   call(G),
   retract(result(Ind,P)),
   call(CN),
   assert(result(Ind,N)),
   fail.
accumulate(Ind,_,_,_,_,_,Res) :-
   retract(result(Ind,Res)).

% allof(X,G,Res) is true if Res is the list of all X such that goal G is true
allof(X,G,Res) :-
   accumulate(G,L-L,P1-[X|P],true,P1-P,Res-[]).
