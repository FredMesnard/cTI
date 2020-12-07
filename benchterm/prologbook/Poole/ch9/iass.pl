% Computational Intelligence: a logical approach. 
% Prolog Code.
% Iterative Deepening Proof for Minimal Explanations in Horn Theorist
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%  Note that this asserts intermediate clauses and so may need to be reloaded
%  between queries.

% `<-' is the object-level `if' 
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).


% explain(G,A) is true if C is a minimal explanation of G.
% an iterative deepening search,
explain(G,A) :-
   iprove(G,A,0).

% bhprove(G,Bound,C1,C2) is true if we can prove G given the assumables in C1
% resulting in the assumables in C2, and never exceeding the Bound on the 
% number of assumables.

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

% iprove(G,A,Bnd) is true if we can prove A is an explanation of G using an 
% iterative deepening search, starting from depth Bnd. At each depth we first
% look for nogoods at this depth and then for explanations of G. 
iprove(_,_,Bnd) :-
   retractall(failed(_)),
   assert(failed(naturally)),
   bhprove(false,Bnd,[],C),
   \+ found(C),
   assert(nogood(C)), fail.

iprove(G,A,Bnd) :-
   bhprove(G,Bnd,[],A),
   \+ found(A),
   \+ explfound(G,A),
   assert(minexpl(G,A)).

iprove(G,A,B) :-
   failed(unnaturally),
   B1 is B+1,
   iprove(G,A,B1).

% found(C) is true if C has already been found as a conflict. It uses the 
%  relation nogood(C1) to record what conflicts have been found
:- dynamic nogood/1.
found(C) :-
   nogood(C1),
   subset(C1,C).

%explfound(G,A) is true if A has already been found as an explanation of G. 
% It uses the relation minexpl(G,A1) to record what conflicts have been found
:- dynamic minexpl/2.
explfound(G,A) :-
   minexpl(G,A1),
   subset(A1,A).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) :-
   member(A,L),
   subset(B,L).

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

