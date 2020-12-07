% Computational Intelligence: a logical approach. 
% Prolog Code.
% Test code for the meta-interpreter for general clauses (Example 7.29)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% a or b
a <- ~b.
b <- ~a.

% c or not a
c <- a.
~a <- ~c.

% c or not b
c <- b.
~b <- ~c.

% ?c.
yes <- c.
~c <- ~yes.
