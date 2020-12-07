% Computational Intelligence: a logical approach. 
% Prolog Code.
% Iterative-deepening Definite Clause Interpreter (Appendix C.2, page 502)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
:- op(950,xfy, &).

% iprove(G) is true if we can prove G using
% iterative deepening search
iprove(G) :-
   iprove_from(G,1).

% bprove(G,Bound,CL1,CL2) is true if we can prove G
% with a proof tree of depth less than Bound where
% CL1 is the closest that we came to the bound before
% proving G, and CL2 is the closest we came to the
% bound after proving G.
bprove(true,Bnd,In,Out) :- !,
   min(Bnd,In,Out).

bprove((A&B),Bnd,C1,C3) :- !,
   bprove(A,Bnd,C1,C2),
   bprove(B,Bnd,C2,C3).

bprove(G,Bnd,C1,C2) :-
   Bnd > 1, !,
   B1 is Bnd-1,
   (G <- Body),
   bprove(Body,B1,C1,C2).

bprove(_,1,_,_) :-
   retract(failed(naturally)),
   assert(failed(unnaturally)),
   fail.

% iprove_from(G,Bnd) is true if we can prove G using
% an iterative deepening search, starting from depth
% Bnd. We succeed if there is a number, such that
% iprove previously failed  unnaturally, such that
% bprove can prove G, getting within one of the depth
% of the previous search (so it must have failed for
% the previous iteration).

iprove_from(G,Bnd) :-
   retractall(failed(_)),
   assert(failed(naturally)),
   bprove(G,Bnd,Bnd,1).

iprove_from(G,B) :-
   failed(unnaturally),
   B1 is B+1,
   iprove_from(G,B1).

% cleanup needs to be done between proofs so the
% proofs do not interfere with each other.
cleanup :-
   retractall(failed(_)).
