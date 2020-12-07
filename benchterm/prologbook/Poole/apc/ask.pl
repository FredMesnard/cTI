% Computational Intelligence: a logical approach. 
% Prolog Code.
% ASK THE USER INTERPRETER - assumes ground questions.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% It only uses rules for an askable goal if the user
% doesn't know the answer.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).
% answered(G,Ans) is dynamically added to the database
% when the user answers Ans to question 'Is G true?'.
:- dynamic(answered/2).

% aprove(G) is true if G can be proven perhaps by
% asking the user.
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
   asserta(answered(G,Ans)),
   Ans=yes.
aprove(H) :-
   \+ answered(H,yes),
   \+ answered(H,no),
   (H <- B),
   aprove(B).

% unanswered(G) is true if G has not been answered
unanswered(G) :- \+ been_answered(G).
been_answered(G) :- answered(G,_).

% ask(G,Ans) asks the user G, and the user 
% replies with Ans
ask(G,Ans) :-
   writel(['Is ',G,' true? [yes, no, unknown]: ']),
   read(Ans).
