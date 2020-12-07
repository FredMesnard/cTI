% Computational Intelligence: a logical approach. 
% Prolog Code. 
% ASK THE USER INTERPRETER (Figure 6.8)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% N.B. this uses assert to maintain the knowledge base as queries are asked.

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% dprove(G) is true if G can be proven perhaps by asking the user.


aprove(true).
aprove((A & B)) :-
   aprove(A),
   aprove(B).
aprove(G) :-
   askable(G),
   answered(G,yes).
aprove(G) :-
   askable(G),
   unanswered(G),
   ask(G,Ans),
   assert(answered(G,Ans)),
   Ans=yes.
aprove(H) :-
   (H <- B),
   aprove(B).

% answered(G,Ans) is dynamically added to the database.
:- dynamic answered/2.

% unanswered(G) is true if G has not be answered
unanswered(G) :- \+ answered(G).   % negation as failure
answered(G) :- answered(G,_).

% ask(G,Ans) asks the user G, and the user relies with Ans
ask(G,Ans) :-
   writeln(['Is ',G,' true? ']),
   read(Ans).

% writeln(L) writes each element of list L
writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).
