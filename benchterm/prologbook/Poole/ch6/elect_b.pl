% Computational Intelligence: a logical approach. 
% Prolog Code. 
% BUGGY OBJECT-LEVEL DEFINITION OF THE ELECTRICAL DOMAIN
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%  The goal lit(l1) succeeds when it should fail. Try ? show(lit(l1)).

% Load trace.pl or trace2.pl first

% Here "<-" is the object-level "if" and is a meta-level predicate.
% "&" is the object-level conjunction and is a meta-level function symbol.

% lit(L) is true if light L is lit.
lit(L) <-
   light(L) &
   ok(L) &
   live(L).

% live(W) is true if W is live (i.e., current will flow through it if grounded)
live(W) <-
   connectedto(W,W1) &
   live(W1).

live(outside) <- true.

% connectedto(W0,W1) is true if W0 is connnected to W1 such that current will
% flow from W1 to W0.

connectedto(l1,w0) <- true.
connectedto(w0,w1) <- up(s2).
connectedto(w0,w2) <- down(s2).
connectedto(w1,w3) <- up(s3).
connectedto(w2,w3) <- down(s1).
connectedto(l2,w4) <- true.
connectedto(w4,w3) <- up(s3).
connectedto(p1,w3) <- true.
connectedto(w3,w5) <- ok(cb1).
connectedto(p2,w6) <- true.
connectedto(w6,w5) <- ok(cb2).
connectedto(w5,outside) <- true.

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

