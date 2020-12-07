% Computational Intelligence: a logical approach. 
% Prolog Code.
% Test code for bottom-up abduction interpreter (from page 498)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

a <- b & c.
a <- e & t.
b <- d.
b <- e.
c <- w.
c <- e.
f <- e.
h <- i.
false <- f.

assumable(e).
assumable(d).
assumable(g).
assumable(w).
assumable(t).
