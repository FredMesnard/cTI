% Computational Intelligence: a logical approach. 
% Prolog Code. 
% DEPTH-BOUNDED META-INTERPRETER from Figure 6.6
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% bprove(G,D) is true if G can be proven with depth no more than D

bprove(true,D).
bprove((A & B),D) :-
   bprove(A,D),
   bprove(B,D).
bprove(H,D) :-
   D >= 0,
   D1 is D-1,
   (H <- B),
   bprove(B,D1).
