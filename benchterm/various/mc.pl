m(X,Y):-X>100, Y is X-10.
m(X,Y):-X=<100, Z is X+11, m(Z,Z1),m(Z1,Y).