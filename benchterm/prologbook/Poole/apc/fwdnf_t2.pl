% test code for the forward negation as failure prover.
a <- b & c.
a <- e & ~t.
b <- d.
b <- e.
c <- w.
f <- ~g.
e <- true.
t <- true.

atomic_symbol(a).
atomic_symbol(b).
atomic_symbol(c).
atomic_symbol(d).
atomic_symbol(e).
atomic_symbol(f).
atomic_symbol(g).
atomic_symbol(h).
atomic_symbol(t).
atomic_symbol(w).
