
inf2(0,s(Y)).
inf2(s(X),s(Y)):-inf2(X,Y).

inf4(f(0,X2),f(0,Y2)):-inf2(X2,Y2).
inf4(f(0,X2),f(s(Y1),Y2)).
%inf4(s(X1),X2,s(X1),Y2):-inf2(X2,Y2).
inf4(f(s(X1),X2),f(s(Y1),Y2)):-inf4(f(X1,X2),f(Y1,Y2)).


add(0,Y,Y).
add(s(X),Y,s(Z)):-add(X,Y,Z).

p(s(X),Y):-%add(s(Y),s(Y),S),
	inf2(Z,s(Y)),p(X,Z).
p(X,s(Y)):-p(X,Y).
