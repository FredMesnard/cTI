% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Random number generater (Example 2.9 page 485)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

:- dynamic seed/1.

seed(447563523).

% rand(R) generates random real number R in the range [0,1)
rand(R) :-
   retract(seed(S)),
   N is (S * 314159262 + 453816693) mod 2147483647,
   assert(seed(N)),
   R is N / 2147483647.0 .

% ramdom(R,M) generates random integer R in the range 0..M-1
random(R,M) :-
    rand(RR),
    R is integer(M * RR).

% random_list(N,L) creates list L of length N of random numbers in range [0,1)
random_list(0,[]).
random_list(N,[R|L]) :-
    N>0,
    rand(R),
    N1 is N-1,
    random_list(N1,L).

% random_elt(E,L) selects a random element E of list L
random_elt(E,L) :-
   length(L,N),
   random(R,N),
   N1 is R+1,
   nth(N1,L,E).

% random_rem(E,L,R) selects a random element E of
% list L where list R is contains the other elements.
random_rem(E,L,R) :-
   length(L,N),
   random(Rand,N),
   N1 is Rand+1,
   nth(N1,L,E,R).

% nth(N,L,E,R) is true if E is the Nth element of
% list L, and R is the remianing elements. We start
% counting positions from 1.
nth(1,[E|R],E,R).
nth(N,[H|T],E,[H|R]) :-
   N>1,
   N1 is N-1,
   nth(N1,T,E,R).

