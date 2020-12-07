% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Meta-interpreter that lets you ask how a proof was found (Figure 6.9)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% This file contains simple code for creating and traversing a proof tree.
% This is useful for understanding and debugging programs that succeed.
% As an example load the file [sorting] and issue a query:
%  | ?- show(msort([4,2,8,6,1,3,4,2,9,6],S)).
% After this, give numbers that represent the branch you want to follow.
% Follow each number by a period.

% The file trace.pl is simpler and easier to understand.
% The file trace2.pl is more complex and more user friendly.
% If you want to actually use one of these, use trace2.pl.
% If you want to understand what is going on, try to understand trace.pl first.

% <- is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).


% show(G) means to prove goal G and then show us a proof tree
show(G) :-
   solve(G,T),
   traverse(T).

% solve(Goal,Tree) is true if Tree is a proof tree for Goal
solve(true,true).
solve((A&B),(AT&BT)) :-
   solve(A,AT),
   solve(B,BT).
solve(H,if(H,builtin)) :-
   builtin(H),
   call(H).
solve(H,if(H,BT)) :-
%   clause(H,B),
   ( H <- B ),
   solve(B,BT).

% builtin(G) is true if goal G is defined in the system (as opposed to 
% being defined in Prolog clauses.
builtin((_ =< _)).
builtin((_ >= _)).
builtin((_ = _)).
builtin((_ < _)).
builtin((_ > _)).

% traverse(T) true if T is a tree being traversed
traverse(if(H,true)) :-
    writeln([H,' is a fact']).
traverse(if(H,builtin)) :-
    writeln([H,' is built-in.']).
traverse(if(H,B)) :-
    B \== true,
    B \== builtin,
    writeln([H,' :-']),
    printbody(B,1,_),
    read(Comm),
    interpretcommand(Comm,B).

% printbody(B,N1,N2) is true if B is a body to be printed and N1 is the 
% count of atoms before B was called and N2 is the count after
printbody(true,N,N).
printbody((A&B),N1,N3) :-
   printbody(A,N1,N2),
   printbody(B,N2,N3).
printbody(if(H,_),N,N1) :-
   writeln(['   ',N,': ',H]),
   N1 is N+1.

% interpretcommand(Comm,B) interprets the command Comm on body B
interpretcommand(N,B) :-
   integer(N),
   nth(B,N,E),
   traverse(E).

% nth(S,N,E) is true if E is the N-th element of structure S
nth(A,1,A) :-
   \+ (A = (_,_)).
nth((A&_),1,A).
nth((_&B),N,E) :-
   N>1,
   N1 is N-1,
   nth(B,N1,E).

% writeln(L) writes each element of list L
writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).



