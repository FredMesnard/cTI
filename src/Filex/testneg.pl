p(X) :- \+ X=0.

% q(X) :- call(X=0).

r(X) :- X=0 ; X=1.

% s(X) :- repeat.

t(X) :- true.
t(X) :- fail.

%s(X):- findall(X,p(X),L).

u(X) :- not(true).