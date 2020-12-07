p(X) --> q(X).
p(X) --> q(X),r(X,Y),s(Y).
p(X) --> [go,to],q(X),[stop].
p(X) --> [X],{integer(X),X>0},q(X).

q(1) --> [].
r(1,2) --> [].
s(2) --> [].

bin --> [0],bin.
bin --> [1],bin.
bin --> []. 