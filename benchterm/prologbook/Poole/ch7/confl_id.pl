% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Finding minimal Conflicts by iterative deepening
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% conflict(C) is true if C is a minimal conflict.
conflict(C) :-
   iprove(C,0).

% iprove(C,Bnd) is true if we can prove C is a minimal
% conflict using an iterative deepening search,
% starting from bound Bnd. The bound is the number of
% assumptions we can have in an explanation.
:- dynamic(failed/1).
% failed(How) means that the goal failed How the 
% previous iteration. How = naturally or unnaturally

iprove(C,Bnd) :-
   retractall(failed(_)),
   asserta(failed(naturally)),
   bhprove(false,Bnd,[],C),
   \+ found(C),
   assert(confl(C)).

iprove(G,B) :-
   failed(unnaturally),
   B1 is B+1,
   iprove(G,B1).

% bhprove(G,Bound,C1,C2) is true if we can prove
% G given the assumables in C1 resulting in the
% assumables in C2, and never exceeding the Bound
% on the number of assumables.

bhprove(true,_,D,D) :- !.

bhprove((A & B),Bnd,C1,C3) :- !,
   bhprove(A,Bnd,C1,C2),
   bhprove(B,Bnd,C2,C3).

bhprove(G,Bnd,C1,C2) :-
   (G <- Body),
   bhprove(Body,Bnd,C1,C2).

bhprove(G,_,C,C) :-
   member(G,C).

bhprove(G,Bnd,C,[G|C]) :-
   \+ member(G,C),
   assumable(G),
   length(C,LC),
   LC < Bnd.

bhprove(G,Bnd,C,_) :-
   \+ member(G,C),
   assumable(G),
   length(C,LC),
   LC >= Bnd,
   retract(failed(_)),
   assert(failed(unnaturally)),
   fail.

% found(C) is true if C has already been found as
% a conflict. It uses the dynamic relation confl(C1)
% to record what conflicts have been found

:- dynamic(confl/1).
found(C) :-
   confl(C1),
   subset(C1,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The following are standard definitions

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) :-
   member(A,L),
   subset(B,L).
