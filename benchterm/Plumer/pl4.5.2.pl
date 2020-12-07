s(A+(B+C),D) :- s((A+B)+C,D).
s(A+B,C) :- s(B+A,C).
s(X+0,X).
s(X+Y,Z) :- s(X,A),s(Y,B),s(A+B,Z).
s(A+B,C) :- number(A),number(B), C is A+B.
