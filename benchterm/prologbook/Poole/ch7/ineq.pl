% Computational Intelligence: a logical approach. 
% Prolog Code. 
% SOUND AND COMPLETE SOLVER FOR REASONING ABOUT INEQUALITY
% Delays those inequalities that cannot be resolved 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).
% `\=' is the object level not equal.
:- op(700,xfx, \=).

% neprove(G) is true if G is a logical consequence of the knowledge base
neprove(G) :-
   neprove(G,[],DIC),
   all_constraints_satisfied(DIC).

% neprove(G,DIC0,DIC1) is true if G can be proven where
%  DIC0 is a list of delayed inequality constraints that
% must hold before G and DIC are those that must hold after.
neprove(true,D,D).
neprove((A & B),D0,D2) :-
   neprove(A,D0,D1),
   neprove(B,D1,D2).
neprove((X \= Y),D0,D1) :-
   add_inequality_constraint((X\= Y),D0,D1).
neprove(G,D0,D1) :-
   (G <- B),
  neprove(B,D0,D1).

% =============================================================================
%   INEQUALITY CONSTRAINTS
/* In this meta-interpreter, all of the inequality constraints are
checked whenever a new one is added. A correct implementation does
not depend on when the contraints are checked. At one extreme, the
inequality constraints can be checked after every unification (to
implement this the last clause to neprove can be modified); at the
other extreme they need only be checked at the end. The problem with
only checking at the end is that we may be able to prune the search
much earlier if we had checked. A better implemenation would be to
notice when a variable is bound and check the inequality constraints
on that variable then, but that would require a different level of
meta-interpreter that we assume here. */

% check_inequality_constraints(DIC0,DIC1).
% is true if the none of inequality constraints in DIC0 are violated, and
% DIC1 is the reamining constraints that much be checked to ensure that none
% of the constraints are satisfied in the future.
check_inequality_constraints([],[]).
check_inequality_constraints([(X \= Y)|DIC0],DIC1) :-
   add_inequality_constraint((X \= Y),DIC0,DIC1).

% add_inequality_constraint((X \= Y),DIC0,DIC1).
% adds the inequality constraint (X \= Y) to DIC0, where
% DIC1 is the reamining constraints that must be satisfied.
% this fails of any of the constraints are violated.
add_inequality_constraint((X \= Y),_,_) :-
   (X==Y),!,fail.
add_inequality_constraint((X \= Y),DIC0,DIC1) :-
   \+(X=Y),!,
   check_inequality_constraints(DIC0,DIC1).
add_inequality_constraint((X \= Y),DIC0,[(X\= Y)|DIC1]) :-
   check_inequality_constraints(DIC0,DIC1).

% all_constraints_satisfied(DIC) is true if all of the constraints in DIC
%are satisfied.

all_constraints_satisfied([]).
all_constraints_satisfied([(X \= Y)|DIC]):-
    X \== Y,
    all_constraints_satisfied(DIC).
