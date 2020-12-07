e(L,T) :- t(L,T).
e(L,T) :- t(L,['+'|C]),e(C,T).
t(L,T) :- n(L,T).
t(L,T) :- n(L,['*'|C]),t(C,T).
n([L|T],T) :- z(L).
n(['('|A],B) :- e(A,[')'|B]).
z(a).
z(b).
z(c).

