% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A CSP solver using generate and test. 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.
                                    
% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
% Domains represented as list of 
% [dom(V,[c1,...,cn]),...]
% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r
csp(Doms,Relns) :-
   generate_and_test(Doms,Relns).

% generate_and_test(Doms,Relns) is true if we can
% find a value for each variable that satisfies
% Relns by generating and testing.
generate_and_test(Doms,Relns) :-
   generate(Doms),
   test(Relns).

generate([]).
generate([dom(X,D)|Ds]) :-
   member(X,D),
   generate(Ds).

test([]).
test([rel(_,R)|Rs]) :-
   call(R),
   test(Rs).

member(A,[A|_]).
member(A,[_|L]) :-
   member(A,L).
