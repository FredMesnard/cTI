% File DCGCASE.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.3

% Parser with case marking as well as agreement

n(singular) --> [dog];[cat];[mouse].
n(plural)   --> [dogs];[cats];[mice].

v(singular) --> [chases];[sees].
v(plural)   --> [chase];[see].

pronoun(singular,nominative) --> [he]; [she].
pronoun(singular,accusative) --> [him];[her].
pronoun(plural,nominative)   --> [they].
pronoun(plural,accusative)   --> [them].

d --> [the].

np(Number,Case)  -->  pronoun(Number,Case).
np(Number,_)     -->  d, n(Number).

vp(Number) --> v(Number), np(_,accusative).

s  -->  np(Number,nominative), vp(Number).

% To test:  ?- s([the,dog,chases,him],[]). (succeeds)
%  versus:  ?- s([the,dog,chases,he],[]).  (fails)

