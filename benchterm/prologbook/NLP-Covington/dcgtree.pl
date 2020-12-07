% File DCGTREE.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.1

% Parser that builds tree structure representation

s(s(NP,VP))  --> np(NP), vp(VP).
np(np(D,N))  --> d(D), n(N).
vp(vp(V,NP)) --> v(V), np(NP).
d(d(the))    --> [the].
n(n(dog))    --> [dog].
n(n(cat))    --> [cat].
v(v(chased)) --> [chased].
v(v(saw))    --> [saw].

% To test:
%  ?- s(Structure,[the,dog,chased,the,cat],[]).
%  Structure = s(np(d(the),n(dog)),vp(v(chased),np(d(a),n(cat))))
