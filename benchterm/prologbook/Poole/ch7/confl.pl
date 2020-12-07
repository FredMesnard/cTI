% Computational Intelligence: a logical approach. 
% Prolog Code. 
% FINDING ALL MINIMAL CONFLICTS using a DELAYING META-INTERPRETER
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% The following code finds all minimal conflicts by first finding 
% all conflicts and then reducing them.

% `<-' is the object-level `if' 
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% conflict(C) is true if C is a conflict (not necessarily minimal)
conflict(C) :-
   dprove(false,[],C).

% dprove(G,D0,D1) is true if G can be proven from the assumables in D1-D0
%   where D0 is a tail of D1.

dprove(true,D,D).
dprove((A & B),D1,D3) :-
   dprove(A,D1,D2),
   dprove(B,D2,D3).
dprove(G,C,C) :-
   member(G,C).
dprove(G,D,[G|D]) :-
   assumable(G).
dprove(H,D1,D2) :-
   (H <- B),
   dprove(B,D1,D2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The following is more advanced and assumes that 
% we can collect all answers

% conflicts(MCs) is true if MCs is the list of all
% minimal conflicts.

conflicts(MCs) :-
   bagof(Con,conflict(Con),Cons),
   reduce(Cons,[],MCs).

% reduce(Cs,Fnd,MCs) is true if Cs is a set of
% conflicts, Fnd is a set of minimal conflicts so far,
% and MCs is a set of minimal conflicts.

reduce([],MCs,MCs).
reduce([C|Cs],Fnd,MCs) :-
   member(F,Fnd),
   subset(F,C), !,         % C has been found
   reduce(Cs,Fnd,MCs).
reduce([C|Cs],Fnd,MCs) :-
   remsub(C,Fnd,Fnd1),
   reduce(Cs,[C|Fnd1],MCs).

% remsub(C,MC1,MC2) is true if MC2 is the set of
% conflicts in MC1 that are not subsumed by C.

remsub(_,[],[]).
remsub(C,[C1|MC1],MC2) :-
   subset(C,C1),!,
   remsub(C,MC1,MC2).
remsub(C,[C1|MC1],[C1|MC2]) :-
   remsub(C,MC1,MC2).

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
