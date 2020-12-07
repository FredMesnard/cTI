%   Explanation-Based Learning Meta-Interpreter
%   Copyright 1995, David Poole
%   This version collects a conjunction as the body of the learned list

% ebl(G,H,true,B) means that H <- B is the learned rule from proving G

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).
:- op(1160, xfx, :: ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,yfx, &).

% We assume that the object-level theory is divided into:
%   facts about a particular case, written as
%      fact(F).
%   general rules are wrtten as
%      N :: H <- B.
%   where N is a unique index for the rule (index is an unstructured atom). 
%   This index lets us access the rule, without any binding.
%
%   built-in rules that are delayed using
%      built_in(P)

% ebl(G,H,B0,B1) is true if H is an instance of G, and B0 is a tail of B1
%   and B0 is a list of fact relations and built-ins that imply H

ebl(true,true,B,B).
ebl((A&B),(A1&B1),D0,D2) :-
   ebl(A,A1,D0,D1),
   ebl(B,B1,D1,D2).
ebl(G,G1,D0,(D0&G1)) :-
   fact(G).
ebl(G,G1,D0,(D0&G1)) :-
   built_in(G),
   call(G).
ebl(G,G1,D0,D1) :-
   (N :: G <- B),
   (N :: G1 <- B1),
   ebl(B,B1,D0,D1).

% Example query (for course example):
% ? ebl(fulfilled_electives(david,cs),H,true,B).

% Example query (for Mitchell's example):
% ? ebl(safe_to_stack(obj1,obj2),Head,true,Body).
