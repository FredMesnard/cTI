% Computational Intelligence: a logical approach. 
% Prolog Code. Section 3.2.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% lit(L) is true if light L is lit.
lit(L) :-
   light(L),
   ok(L),
   live(L).

% live(W) is true if W is live (i.e., current will flow through it if grounded)
live(W) :-
   connectedto(W,W1),
   live(W1).

live(outside).

% connectedto(W0,W1) is true if W0 is connnected to W1 such that current will
% flow from W1 to W0.

connectedto(l1,w0).
connectedto(w0,w1) :- up(s2).
connectedto(w0,w2) :- down(s2).
connectedto(w1,w3) :- up(s1).
connectedto(w2,w3) :- down(s1).
connectedto(l2,w4).
connectedto(w4,w3) :- up(s3).
connectedto(p1,w3).
connectedto(w3,w5) :- ok(cb1).
connectedto(p2,w6).
connectedto(w6,w5) :- ok(cb2).
connectedto(w5,outside).

% light(L) is true if L is a light
light(l1).
light(l2).

% up(S) is true if switch S is up
% down(S) is true if switch S is down

up(s2).
down(s1).
up(s3).

% ok(X) is true if circuit breaker or light X is working (not blown)
ok(l1).
ok(l2).
ok(cb1).
ok(cb2).
