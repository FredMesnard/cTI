%   Explanation-Based Learning Meta-Interpreter
%   Copyright 1995, David Poole

% ebl(G,H,[],B) means that H <- B is the learned rule from proving G

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% We assume that the object-level theory is divided into:
%   facts about a particular case, written as
%      fact(F).
%   general rules are wrtten as
%      rule_copy((H <- B), (H1 <- B1))
%      where H1 <- B1 is a syntactic variant (variables are renamed) of H <- B
%   built-in rules that are delayed using
%      built_in(P)

% ebl(G,H,B0,B1) is true if H is an instance of G, and B0 is a tail of B1
%   and B0 is a list of fact relations and built-ins that imply H

ebl(true,true,B,B).
ebl((A&B),(A1&B1),D0,D2) :-
   ebl(A,A1,D1,D2),
   ebl(B,B1,D0,D1).
ebl(G,G1,D0,[G1|D0]) :-
   fact(G).
ebl(G,G1,D0,[G1|D0]) :-
   built_in(G),
   call(G).
ebl(G,G1,D0,D1) :-
   rule_copy((G <- B),(G1 <- B1)),
   ebl(B,B1,D0,D1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Here is the example from Mitchell's book

% Training example
fact(on(obj1,obj2)).
fact(isa(obj1,box)).
fact(isa(obj2,endtable)).
fact(color(obj1,red)).
fact(color(obj2,blue)).
fact(volume(obj1,0.1)).
fact(density(obj1,0.1)).

% domain theory
rule_copy((safe_to_stack(X,Y) <- unfragile(Y)),
          (safe_to_stack(X1,Y1) <- unfragile(Y1))).
rule_copy((safe_to_stack(X,Y) <- lighter(X,Y)),
          (safe_to_stack(X1,Y1) <- lighter(X1,Y1))).
rule_copy((lighter(X,Y) <- weight(X,WX) & weight(Y,WY) & WX<WY),
          (lighter(X1,Y1) <- weight(X1,WX1) & weight(Y1,WY1) & WX1<WY1)).
rule_copy((weight(X,V*D) <- volume(X,V) & density(X,D)),
          (weight(X1,V1*D1) <- volume(X1,V1) & density(X1,D1))).
rule_copy((weight(X,5) <- isa(X,endtable)),
          (weight(X1,5) <- isa(X1,endtable))).

built_in((_<_)).

% Example query:
% ? ebl(safe_to_stack(obj1,obj2),safe_to_stack(X,Y),[],B).
