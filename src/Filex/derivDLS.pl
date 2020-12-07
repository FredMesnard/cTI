deriv(d(t),t,1).
deriv(d(N),t,0):-atom(N).
deriv(d(X+Y),t,L+M):-deriv(d(X),t,L),deriv(d(Y),t,M).
deriv(d(X*Y),t,X*L+Y*M):-deriv(d(X),t,M),deriv(d(Y),t,L).
% avec la clause ci-dessous, ct=0
% d'apres Dershowitz, Lindenstrauss, on ne peut pas
% montrer la term avec une norme lineaire -> omega
deriv(d(d(X)),t,L):-deriv(d(X),t,M),deriv(d(M),t,L).
