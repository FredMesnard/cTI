% File TRANSLAT.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 8, Section 8.2.4

% English-Latin translation via semantic representation

% To test: ?- english_latin([a,cat,meowed],What).


% English grammar (from SEMQUANT.PL)

s(Sem) --> np((X^Sco)^Sem), vp(X^Sco).

vp(Sem) --> v(Sem).
vp(X^Pred) --> v(Y^X^Sco), np((Y^Sco)^Pred).

np(Sem) --> d((X^Res)^Sem), n(X^Res).

d((X^Res) ^ (X^Sco) ^ all(X,Res,Sco))  --> [every].
d((X^Res) ^ (X^Sco) ^ some(X,Res,Sco)) --> [a];[some].

n(X^dog(X)) --> [dog].
n(X^cat(X)) --> [cat].

v(X^meowed(X))     --> [meowed].
v(Y^X^chased(X,Y)) --> [chased].
v(Y^X^saw(X,Y))    --> [saw].


% Latin grammar
% (to avoid conflicts, all node labels begin with x).

xs(Sem) --> xnp(_,nom,(X^Sco)^Sem), xvp(X^Sco).

xvp(Sem) --> xv(Sem).
xvp(X^Pred) --> xnp(_,acc,(Y^Sco)^Pred), xv(Y^X^Sco).

xnp(Gender,Case,Sem) --> xd(Gender,Case,(X^Res)^Sem), xn(Gender,Case,X^Res).
xnp(Gender,Case,Sem) --> xn(Gender,Case,X^Res),xd2(Gender,Case,(X^Res)^Sem).

xd(_,_,(X^Res)   ^ (X^Sco) ^ some(X,Res,Sco))  --> [].
xd(_,nom,(X^Res) ^ (X^Sco) ^ all(X,Res,Sco))   --> [omnis].
xd(_,acc,(X^Res) ^ (X^Sco) ^ all(X,Res,Sco))   --> [omnem].

xd2(masc,nom,(X^Res) ^ (X^Sco) ^ some(X,Res,Sco)) --> [quidam].
xd2(masc,acc,(X^Res) ^ (X^Sco) ^ some(X,Res,Sco)) --> [quendam].
xd2(fem,nom,(X^Res) ^ (X^Sco) ^ some(X,Res,Sco))  --> [quaedam].
xd2(fem,acc,(X^Res) ^ (X^Sco) ^ some(X,Res,Sco))  --> [quandam].

xn(masc,nom,X^dog(X))  --> [canis].
xn(masc,acc,X^dog(X))  --> [canem].
xn(fem,nom,X^cat(X))   --> [felis].
xn(fem,acc,X^cat(X))   --> [felem].

xv(X^meowed(X))     --> [ululavit].
xv(Y^X^chased(X,Y)) --> [agitavit].
xv(Y^X^saw(X,Y))    --> [vidit].


% english_latin(?E,?L)
%  English sentence E = Latin sentence L.
%  More efficient when E is instantiated.

english_latin(E,L) :- s(Sem,E,[]), xs(Sem,L,[]).

