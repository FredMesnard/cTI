% File SEMN1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 7, Section 7.2.3


% Semantics of the N1 constituent
%   (common noun plus adjectives)

% To test:  ?- n1(Sem,[big,brown,dog],[]).


n1(X ^ (P,Q)) --> adj(X^P), n1(X^Q).

n1(Sem) --> n(Sem).

adj(X^big(X))    --> [big].
adj(X^brown(X))  --> [brown].
adj(X^little(X)) --> [little].
adj(X^green(X))  --> [green].

n(X^dog(X)) --> [dog].
n(X^cat(X)) --> [cat].
