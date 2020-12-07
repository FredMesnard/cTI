% File DCG1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.2.2

% First top-down parser, Chapter 3
% (not written in DCG notation)

s(L1,L)  :- np(L1,L2), vp(L2,L).
np(L1,L) :- d(L1,L2),  n(L2,L).
vp(L1,L) :- v(L1,L2),  np(L2,L).
d([the|L],L).
d([a|L],L).
n([dog|L],L).
n([cat|L],L).
n([gardener|L],L).
n([policeman|L],L).
n([butler|L],L).
v([chased|L],L).
v([saw|L],L).

% To test:
%  ?- s([the,gardener,saw,a,policeman],[]).
%  yes
