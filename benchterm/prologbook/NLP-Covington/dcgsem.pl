% File DCGSEM.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.5.1

% Definite-clause grammar that builds a semantic representation

% NOTE: In Arity Prolog, change '^(' to '^ (' throughout.

np(fido)   --> [fido].
np(felix)  --> [felix].

v(X^slept(X))        --> [slept].
v(Y^(X^chased(X,Y))) --> [chased].

s(Pred) --> np(Subj), vp(Subj^Pred).

vp(Subj^Pred) --> v(Subj^Pred).
vp(Subj^Pred) --> v(Obj^(Subj^Pred)), np(Obj).

% To test:
%  ?- s(Semantics,[fido,chased,felix],[]).
%  Semantics = chased(fido,felix)
%
%  ?- s(Semantics,[felix,slept],[]).
%  Semantics = slept(felix)
