% Computational Intelligence: a logical approach. 
% Prolog Code. Bottom-up assumable interpreter (Appendix C.1)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% An environment is a term of the form env(G,E) where
% E is a list of assumables such that G<-E logically
% follows from the clauses.  Environment E1 subsumes
% environment E2 if the implication represented by E1
% implies the implication represented by E2.

% An explanation of G is a set E of assumables that,
% together with the given rules implies G and is
% consistent (doesn't imply false).  An explanation
% is represented by the corresponding environment
% env(G,E).  A minimal explanation is an explanation
% such that no subset is also an explanation.

% explanations(Ans) is true if Ans is a list of
% minimal explanations.
explanations(Ans) :-
   allof( env(A,[A]),assumable(A),C),
   writeln(['Initial assumables: ',C]),
   fc_ab(C, Ans).

% fc_ab(C,R) means you can forward chain from C
% getting R.  This chooses one rule at a time to
% forward chain on, producing one candidate
% environment. This environment must not already be
% subsumed by an already derived environment.
fc_ab(C,R) :-
   ( A <- B ),
   derived(B,[],H,C),
   \+ subsumed(env(A,H),C), 
   !,                   % commit to this selection
   writeln(['Derived: ',env(A,H)]),
   add_n_prune(env(A,H),C,C1),
   fc_ab(C1,R).
fc_ab(C,C).

% derived(B,H0,H,C) is true if body B can be
% directly explained starting from the assumptions
% H0, resulting in the assumptions H1 given list
% of environments C
derived(true,H,H,_) :-!.
derived((A & B),H0,H2,C) :-
   !,
   derived(A,H0,H1,C),
   derived(B,H1,H2,C).
derived(A,H0,H1,C) :-
   member(env(A,H),C),
   insert_each(H,H0,H1).

insert_each([],L,L).
insert_each([A|R],L0,L1) :-
   member(A,L0),
   !,
   insert_each(R,L0,L1).
insert_each([A|R],L0,L1) :-
   insert_each(R,[A|L0],L1).

% subsumed(E,C) is true if environment E is subsumed
% by an element of C.
subsumed(E,C) :- 
   member(E1,C),
   subsumes(E1,E).

% subsumes(E1,E2) is true if environment E1 subsumes
% environment E2
subsumes(env(A,H1),env(A,H2)) :-
   subset(H1,H2),!.
subsumes(env(false,H1),env(_,H2)) :-
   subset(H1,H2),!.

% add_n_prune(E,L,R) is true if R is the resulting
% list of minimal environments obtained by adding
% environment E to L.  We need to remove all
% environments in L that are subsumed by E.
add_n_prune(E,[],[E]).
add_n_prune(E,[E1|R],C) :-
   subsumes(E,E1),
   add_n_prune(E,R,C).
add_n_prune(E,[E1|R],[E1|C]) :-
   \+ subsumes(E,E1),
   add_n_prune(E,R,C).
