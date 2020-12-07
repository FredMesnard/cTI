% File BUP.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.4.4

% BUP left-corner parser, without links, without chart

% parse(+C,+S1,-S)
%  Parse a constituent of category C
%  starting with input string S1 and
%  ending up with input string S.

parse(C,S1,S) :-
  word(W,S1,S2),
  P =.. [W,C,S2,S],
  call(P).


% PS-rules and terminating clauses

np(C,S1,S) :- parse(vp,S1,S2), s(C,S2,S).          % S --> NP VP
np(C,S1,S) :- parse(conj,S1,S2),
                      parse(np,S2,S3), np(C,S3,S). % NP --> NP Conj NP
np(np,X,X).

d(C,S1,S) :- parse(n,S1,S2), np(C,S2,S).    % NP --> D N
d(d,X,X).

v(C,S1,S) :- parse(np,S1,S2), vp(C,S2,S).                   % VP --> V NP
v(C,S1,S) :- parse(np,S1,S2), parse(pp,S2,S3), vp(C,S3,S).  % VP --> V NP PP
v(v,X,X).

p(C,S1,S) :- parse(np,S1,S2), pp(C,S2,S).    % PP --> P NP
p(p,X,X).


% Terminating clauses for all other categories

s(s,X,X).
vp(vp,X,X).
pp(pp,X,X).
n(n,X,X).
conj(conj,X,X).

% Lexicon

word(conj,[and|X],X).

word(p,[near|X],X).

word(d,[the|X],X).

word(n,[dog|X],X).       word(n,[dogs|X],X).
word(n,[cat|X],X).       word(n,[cats|X],X).
word(n,[elephant|X],X).  word(n,[elephants|X],X).

word(v,[chase|X],X).     word(v,[chases|X],X).
word(v,[see|X],X).       word(v,[sees|X],X).
word(v,[amuse|X],X).     word(v,[amuses|X],X).

