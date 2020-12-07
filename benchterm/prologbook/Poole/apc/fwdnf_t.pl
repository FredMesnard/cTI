% Computational Intelligence: a logical approach. 
% Prolog Code.
% Test code for bottom-up negation-as-faulure interpreter (Example 7.22)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

p <- q &  ~ r.
p <- s.
q <-  ~ s.
r <-  ~ t.
t <- true.
s <- w.

atomic_symbol(p).
atomic_symbol(q).
atomic_symbol(r).
atomic_symbol(s).
atomic_symbol(t).
atomic_symbol(u).
atomic_symbol(v).
atomic_symbol(w).
