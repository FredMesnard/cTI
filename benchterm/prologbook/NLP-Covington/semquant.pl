% File SEMQUANT.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 7, Section 7.3.4, and Chapter 8, Section 8.2.2


% Semantics of sentences with quantifiers on NPs.

% To test:  ?- s(Sem,[every,dog,chased,some,cat],[]).


s(Sem) --> np((X^Sco)^Sem), vp(X^Sco).

np(Sem) --> d((X^Res)^Sem), n(X^Res).

vp(Sem) --> v(Sem).
vp(X^Pred) --> v(Y^X^Sco), np((Y^Sco)^Pred).

d((X^Res) ^ (X^Sco) ^ all(X,Res,Sco))  --> [every].
d((X^Res) ^ (X^Sco) ^ some(X,Res,Sco)) --> [a];[some].

n(X^dog(X)) --> [dog].
n(X^cat(X)) --> [cat].


v(X^meowed(X))     --> [meowed].
v(Y^X^chased(X,Y)) --> [chased].
v(Y^X^saw(X,Y))    --> [saw].


