add(0,X,X).
add(s(X),Y,s(Z)):-add(X,Y,Z).

%fib(0,0).
%fib(s(0),s(0)).
%fib(s(s(N)),F):-fib(s(N),F1),fib(N,F2),add(F1,F2,F).

fib2(0,s(0)).
fib2(s(0),s(0)).
fib2(s(s(N)),F):-fib2(s(N),F1),fib2(N,F2),add(F1,F2,F).