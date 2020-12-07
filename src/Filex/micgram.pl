ident(A, B) :-  A=[97|B].

p(X,X).
p(2,Y):-ident(4,Y).
p(3,Y):-ident(5,Z),ident(6,Y).
p(4,Y):-X=Y,ident(X,X).
q(A,B,A,B,C).
