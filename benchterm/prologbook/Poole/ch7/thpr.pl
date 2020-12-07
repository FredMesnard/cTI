% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Meta-interpreter for general clauses (Figure 7.7)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).
% `~' is object-level negation
:- op(900,fy,~).

% gprove(G,A) is true if G <- A follows from KB
gprove(true,_).
gprove((G & H),A):- 
   gprove(G,A),
   gprove(H,A).
gprove(G,A):- 
   member(G,A).
gprove(G,A):- 
  (G <- B),
   neg(G,NG),
   gprove(B,[NG|A]). 

neg((~ X),X).
neg(X,(~ X)) :- \+ negation(X).

negation((~ _)).

% to prove a theorem do
% gprove(yes,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Standard Definitions

% member(E,L) is true if E is a member of list L
member(E,[E|_]).
member(E,[_|L]) :-
   member(E,L).
