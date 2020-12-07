% Computational Intelligence: a logical approach. 
% Prolog Code. 
% ASK THE USER INTERPRETER for Why questions (based on Figures 6.8 and 6.10)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% N.B. this uses assert to maintain the knowledge base as queries are asked.


% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% aprove(G) is true if G can be proven perhaps by asking the user.

aprove(G) :-
   wprove(G,[]).

% wprove(G,Anc) is true if G can be proven with ancestor list Anc.
% This also asks the user and lets the user ask "why".
wprove(true,_).
wprove((A & B), Anc) :-
   wprove(A,Anc),
   wprove(B,Anc).
wprove(G,_) :-
   askable(G),
   answered(G,yes).
wprove(G,Anc) :-
   askable(G),
   unanswered(G),
   ask(G,Ans,Anc),
   assert(answered(G,Ans)),
   Ans=yes.
wprove(H,Anc) :-
   (H <- B),
   wprove(B,[(H <- B)|Anc]).

% answered(G,Ans) is dynamically added to the database.
:- dynamic answered/2.

% unanswered(G) is true if G has not be answered
unanswered(G) :- \+ answered(G).   % negation as failure
answered(G) :- answered(G,_).

% ask(G,Ans) asks the user G, and the user relies with Ans
ask(G,Ans,Anc) :-
   writeln(['Is ',G,' true? ']),
   read(Reply),
   interpret(G,Ans,Reply,Anc).

% interpret(G,Ans,Reply,Anc) means that we should interpret Reply to question
%   G as answer Ans in the context of ancesort list Anc
interpret(_,Reply,Reply,_) :-
   Reply \== why.
interpret(G,Ans,why,[Rule|T]):-
   writeln(['I used the rule: ',Rule,'.']),
   ask(G,Ans,T).
interpret(G,Ans,why,[]):-
   writeln(['Because that is what you asked me!']),
   ask(G,Ans,[]).


% writeln(L) writes each element of list L
writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).

% To implement Example 6.12
% | ?- aprove(lit(L)).
