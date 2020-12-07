p(0,_).
p(s(X),_):-q(X,_).
q(X,_):-r(X).
r(X):-p(X,_).

%a(X):-toto(X).

