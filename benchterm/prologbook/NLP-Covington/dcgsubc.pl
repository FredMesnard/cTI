% File DCGSUBC.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.4

% Parser that enforces subcategorization

s  --> np, vp.
np --> d,  n.
d  --> [the];[a].
n  --> [dog];[cat];[gardener];[policeman];[butler].

vp --> v(1).
vp --> v(2), np.
vp --> v(3), np, np.
vp --> v(4), s.

v(1) --> [barked];[slept].
v(2) --> [chased];[saw].
v(3) --> [gave];[sold].
v(4) --> [said];[thought].

% To test:  ?- s([the,gardener,thought,the,dog,chased,a,cat],[]).  (succeeds)
%  versus:  ?- s([the,gardener,slept,the,dog,chased,a,cat],[]).    (fails)
