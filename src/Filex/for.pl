for(N,N).
for(I,N):-less(I,N),for(s(I),N).

less(0,s(_X)).
less(s(X),s(Y)):-less(X,Y).