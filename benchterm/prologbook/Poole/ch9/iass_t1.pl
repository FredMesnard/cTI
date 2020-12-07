% Computational Intelligence: a logical approach. 
% Prolog Code.
% This is a simple example with conflicting assumables
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% load iass.pl first

g <- a & b.
g <- c.
false <- f & h.
a <- s.
a <- u.
b <- t.
b <- v.
c <- u.
c <- w.
f <- s.
f <- w.
h <- t.
assumable(s).
assumable(t).
assumable(u).
assumable(v).
assumable(w).
