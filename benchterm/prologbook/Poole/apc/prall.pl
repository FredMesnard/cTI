% Computational Intelligence: a logical approach. 
% Prolog Code.
% META INTERPRETER WITH SEARCH (Section C.2, page 505)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% proveall(Qs,As) is true if Qs is a list of
% generalized answer clauses, and As is the list of
% all answers to the queries in Qs.
proveall([],[]).
proveall(Qs,[A|Ans]) :-
   select_query(goal(A,[]),Qs,RQs),
   proveall(RQs,Ans).
proveall(Qs,Ans) :-
   select_query(goal(A,C),Qs,RQs),
   select_atom(G,C,C1),
   allof(goal(A,NC),((G <- B), add_body(B,C1,NC)),S),
   add_to_frontier(S,RQs,ND),
   proveall(ND,Ans).

% select_query(Q,Qs,RQs) is true if generalized
% answer clause Q is selected from list Qs, and RQs are
% the remaining elements of Qs.
select_query(Q,[Q|Qs],Qs).

% select_atom(A,As,RAs) is true if selecting atom
% A from list As leaves RAs
select_atom(A,[A|Gs],Gs).

% add_to_frontier(NN,F0,F1) is true if adding
% elements NN to frontier F0 produces frontier F1
add_to_frontier(NN,F0,F1) :-
   append(NN,F0,F1).

% add_body(B,C1,C2) adds the elements of body B to
% the conjuncts in C1 producing the conjuncts in C2
add_body(true,Con,Con) :- !.
add_body((A & B),Con1,Con3) :- !,
   add_body(B,Con1,Con2),
   add_body(A,Con2,Con3).
add_body(A,Con,[A|Con]).
