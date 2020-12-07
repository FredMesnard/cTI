% Computational Intelligence: a logical approach. 
% Prolog Code. 
% DELAYING META-INTERPRETER (Figure 6.7)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% dprove(G,D0,D1) is true if G can be proven from the delayables in D1-D0
%   where D0 is a tail of D1.

dprove(true,D,D).
dprove((A & B),D1,D3) :-
   dprove(A,D1,D2),
   dprove(B,D2,D3).
dprove(G,D,[G|D]) :-
   delay(G).
dprove(H,D1,D2) :-
   (H <- B),
   dprove(B,D1,D2).
