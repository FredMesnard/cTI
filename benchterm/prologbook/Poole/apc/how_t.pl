% Computational Intelligence: a logical approach. 
% Prolog Code.
% OBJECT-LEVEL DEFINITION OF THE ELECTRICAL DOMAIN
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% Load how.pl first

% Here "<-" is the object-level "if" and is a meta-level predicate.
% "&" is the object-level conjunction and is a meta-level function symbol.

% lit(L) is true if light L is lit.
lit(L) <-
   light(L) &
   ok(L) &
   live(L).

% live(W) is true if W is live (i.e., current will flow through it if grounded)
live(W) <-
   connected_to(W,W1) &
   live(W1).

live(outside) <- true.

% connected_to(W0,W1) is true if W0 is connected to W1 such that current will
% flow from W1 to W0.

connected_to(l1,w0) <- true.
connected_to(w0,w1) <- up(s2).
connected_to(w0,w2) <- down(s2).
connected_to(w1,w3) <- up(s1).
connected_to(w2,w3) <- down(s1).
connected_to(l2,w4) <- true.
connected_to(w4,w3) <- up(s3).
connected_to(p1,w3) <- true.
connected_to(w3,w5) <- ok(cb1).
connected_to(p2,w6) <- true.
connected_to(w6,w5) <- ok(cb2).
connected_to(w5,outside) <- true.

% light(L) is true if L is a light
light(l1) <- true.
light(l2) <- true.

% up(S) is true if switch S is up
% down(S) is true if switch S is down
up(s2) <- true.
down(s1) <- true.
up(s3) <- true.


% ok(G) is true if G is working
ok(l1) <- true.
ok(l2) <- true.
ok(cb1) <- true.
ok(cb2) <- true.


% Example Queries:
% ? show(lit(L)).
