% Computational Intelligence: a logical approach. 
% Prolog Code. Bottom-up negation-as-failure interpreter (Appendix C.1)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% "<-" is the object-level "if"
:- op(1150, xfx, <- ).
% "&" is the object level conjunction.
:- op(950,xfy, &).
% "~" is the object level negation.
:- op(900,fy, ~).

% fc_nf(C,R) is true if you can forward chain from 
% atoms in C resulting in R.
fc_nf(C,R) :-
   ( A <- B ),
   derived(B,C),
   notin(A,C), 
   !,
   fc_nf([A|C],R).
fc_nf(C,R) :-
   atomic_symbol(A),
   notin((~ A),C),
   allof(B,(A <- B),Bds),
   allfail(Bds,C),
   !,
   fc_nf([(~ A)|C],R).
fc_nf(C,C).

% derived(B,C) is true if body B can be directly 
% derived from atoms in C
derived(true,_).
derived((A & B),C) :-
   derived(A,C),
   derived(B,C).
derived(A,C) :- 
   member(A,C).

% allfail(Bs,C) is true if all of the bodies in 
% list Bs are false given the truths in list C.
allfail([],_).
allfail([H|T],C) :-
   fails(H,C),
   allfail(T,C).

% fails(B,C) is true if body B is false given C
fails((A&_),C) :- fails(A,C), !.
fails((_&B),C) :- !,fails(B,C).
fails((~ A),C) :- !,member(A,C).
fails(A,C) :- member((~ A),C),!.
