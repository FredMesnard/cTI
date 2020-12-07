% Computational Intelligence: a logical approach. 
% Prolog Code.
% More test code for the meta-interpreter for general clauses (Example 7.32)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% a ; ~b <- c.
a <- b & c.
~b <- ~a & c.
~c <- b & ~a.

% ~e <- ~c.
~e <- ~c.
c <- e.

% b; d.
b <- ~d.
d <- ~b.

% a; b<- d.
a <- ~b & d.
b <- ~a & d.
~d <- ~a & ~b.

% e.
e <- true.

% ? a.
yes <- a.
~a <- ~ yes.
