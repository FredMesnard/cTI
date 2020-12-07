% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Example code for the conflict meta-interpreter
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% Load confl.pl or confl_id.pl first

false <- a & b.
false <- c.
c <- d.
a <- d.
a <- h.
a <- e.
b <- e.
b <- s.
assumable(d).
assumable(e).
assumable(h).
assumable(s).
