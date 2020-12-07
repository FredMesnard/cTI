% Computational Intelligence: a logical approach. 
% Prolog Code.
% Iterative deepening A* search, based on Prolog doing the searching.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% idsearch(N,P) is true if path P is a path from
% node N to a goal, found using iterative deepening search
% Example query: idsearch2(o103,P).
idsearch2(N,P) :-
   h(N,HN),
   idsearch(N,HN,P).

% failed(naturally) is true if the depth bound
% was not reached in the current iteration.
% failed(Ex) where Ex is a number, is true if the
% search failed because the search hit the
% depth-bound.  Ex specifies how much the
% smallest f-value exceeded the previous bound.
:- dynamic failed/2.    % this is not actually needed!

% idsearch(N,DB,P) is true if there is a path from
% N to a goal with path length greater than or
% equal to DB
idsearch(N,DB,P) :-
   writeln(['Trying Depth Bound: ',DB]),
   assert(failed(natural)),
   dbsearch(N,DB,P).

idsearch(N,DB,P) :-
   retract(failed(Ex)),
   number(Ex),
   NDB is DB+Ex,
   idsearch(N,NDB,P).

% dbsearch(N,DB,P) is true if path P is a path of
% length DB from N to a goal.
dbsearch(N,DB,[N]) :-
   is_goal(N),
   h(N,DB).
dbsearch(N,DB,_) :-
   h(N,HN),
   DB < HN,
   retract(failed(How)),
   min1(HN-DB,How,Ex),
   assert(failed(Ex)),
   fail.
dbsearch(N,DB,[N|P]) :-
   h(N,HN),
   DB>=HN,
   neighbours(N,NNs),
   member(NN,NNs),
   cost(N,NN,AC),
   DB1 is DB-AC,
   dbsearch(NN,DB1,P).

min1(E,natural,V) :- !, V is E.
min1(E,V,V) :- V =< E.
min1(E,V,V1) :- V > E, V1 is E.

% **************************************************
% writeln(L) is true if L is a list of items to
% be written on a line, followed by a newline.
writeln([]) :- nl.
writeln([H|T]) :- write(H), writeln(T).

% member(E,L) is true if E is a member of list L
member(E,[E|_]).
member(E,[_|L]) :-
   member(E,L).
