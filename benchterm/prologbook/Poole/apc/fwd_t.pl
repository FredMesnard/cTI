% Computational Intelligence: a logical approach. 
% Prolog Code.
% Test code for bottom-up definite clause interpreter (Example 2.14)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

a <- b & c.
b <- d & e.
b <- g & e.
c <- e.
d <- true.
e <- true.
f <- a & g.
