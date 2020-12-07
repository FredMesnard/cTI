% File DCG2.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.3.1

% Sample parser in DCG notation

s  --> np, vp.
np --> d,  n.
vp --> v,  np.
d  --> [the];[a].
n  --> [dog];[cat];[gardener];[policeman];[butler].
v  --> [chased];[saw].

% To test:
%  ?- s([the,gardener,saw,a,policeman],[]).
%  yes
