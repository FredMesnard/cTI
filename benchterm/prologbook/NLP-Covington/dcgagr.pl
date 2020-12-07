% File DCGAGR.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.2

% Parser that enforces subject-verb agreement

n(singular) --> [dog];[cat];[mouse].
n(plural)   --> [dogs];[cats];[mice].

v(singular) --> [chases];[sees].
v(plural)   --> [chase];[see].

d --> [the].

np(Number) --> d, n(Number).
vp(Number) --> v(Number), np(_).

s --> np(Number), vp(Number).

% To test:  ?- s([the,dog,chases,the,cats],[]).  (succeeds)
%  versus:  ?- s([the,dog,chase,the,cats],[]).   (fails)



