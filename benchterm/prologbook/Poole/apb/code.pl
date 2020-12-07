% Computational Intelligence: a logical approach. 
% Prolog Code from Appexdix B.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) :-
   member(A,L),
   subset(B,L).

% append(A,B,R) is true if R is the list
% containing the elements of A followed by the
% elements of B
append([],R,R).
append([H|T],L,[H|R]) :-
   append(T,L,R).

% odd(N) is true if N evaluates to an odd integer
odd(N) :-
   1 is abs(N) mod 2.
% even(N) is true if N evaluates to an even integer
even(N) :-
   0 is abs(N) mod 2.
% ? odd(77*3).

% power(X,N,P) is true if N is a postive integer,
% and P is X multiplies by itself N times.
power(X,1,X).
power(X,N,V) :- 
   even(N),
   N>1,
   X2 is X*X,
   N2 is N //2,
   power(X2,N2,V).
power(X,N,V) :-
   odd(N),
   N>1,
   N1 is N-1,
   power(X,N1,V1),
   V is V1 * X.

% min(A,B,C) is true if C is the minimum of A and B.
min(A,B,A) :- A =< B.
min(A,B,B) :- A > B.
% ? min(2*22,7*3,Y).

%-------------
% `<-' is the object-level `if' 
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% here is an object-level definition of append:
append([],R,R) <- true.
append([H|T],L,[H|R]) <-
   append(T,L,R).

% prove(G) is true if G can be proven
prove(true).
prove((A & B)) :- prove(A), prove(B).
prove(G) :- (G <- B), prove(B).

% ? prove(append([a,X],[d,e,f],[Y,b|W])).
% ? prove(append(X,[Y,Z|W],[a,b,c,d,e])).

% bassert(C) asserts clause C to the rule base,
% and undoes the assertion on backtracking
bassert(C) :-
   asserta(C).
bassert(C) :-
   retract(C),!, fail.

% bretract(C) retracts clause C from the rule
% base, and undoes the retraction on backtracking
bretract(C) :-
   retract(C) , assertonfailure(C).
assertonfailure(_).
assertonfailure(C) :- asserta(C), fail.

%---------------
:- dynamic(previndex/1).
previndex(0).

newindex(I1) :-
   retract(previndex(I)),
   I1 is I+1,
   asserta(previndex(I1)).

% accumulate(Goal,Init,Prev,Acc,Next,Result)
% accumulates information for each success of
% Goal. Init is the start. Acc is a predicate that
% shows how to derive Next accumulation from Prev
% accumulation. Result is the final accumulation.

accumulate(G,I,P,CN,N,Res) :-
   newindex(Ind),
   iaccumulate(Ind,G,I,P,CN,N,Res).

:- dynamic(result/2).
iaccumulate(Ind,G,I,P,CN,N,_) :-
   asserta(result(Ind,I)),
   G,
   retract(result(Ind,P)),
   CN,
   asserta(result(Ind,N)),
   fail.
iaccumulate(Ind,_,_,_,_,_,Res) :-
   retract(result(Ind,Res)).

:- dynamic p/1.
p(3).
p(5).
p(7).
p(1).

% ? accumulate(p(X),0,P,N is P+X,N,Res).
% ? accumulate(p(X),1,P,N is P*X,N,Res).
% ? accumulate(p(X),[],P,N=[X|P],N,Res).
% ? accumulate(p(X),L-L,P1-P2,P2=[X|P],P1-P,Res-[]).

allof(X,G,Res) :-
   accumulate(G,L-L,P1-[X|P],true,P1-P,Res-[]).
%? allof(X,p(X),L).
%? allof(together(X,Y),append(_,[X,Y|_],[a,b,c,d]),L).
%---------

% writeln(L) is true if L is a list of items to
% be written on a line, followed by a newline.
writeln([]) :- nl.
writeln([H|T]) :- write(H), writeln(T).

% not1,not2 and not3 are different implementations of 
% (non delaying) negation as failure.
not1(G) :- allof(true,G,[]).

not2(G) :- (G -> fail; true).

not3(G) :- G, !, fail.
not3(_).
