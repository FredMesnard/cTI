% File LEFTC.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.4.2

% Simple left-corner parser

% parse(+C,+S1,-S)
%  Parse a constituent of category C
%  starting with input string S1 and
%  ending up with input string S.

parse(C,[Word|S2],S) :-
  word(W,Word),
  complete(W,C,S2,S).


% parse_list(+Cs,+S1,-S)
%  Like parse/3, but Cs is a list of
%  categories to be parsed in succession.

parse_list([C|Cs],S1,S) :-
   parse(C,S1,S2),
   parse_list(Cs,S2,S).

parse_list([],S,S).


% complete(+W,+C,+S1,-S)
%  Verifies that W can be the first sub-constituent
%  of C, then left-corner-parses the rest of C.

complete(C,C,S,S).   % if C=W, do nothing.

complete(W,C,S1,S) :-
  rule(P,[W|Rest]),
  parse_list(Rest,S1,S2),
  complete(P,C,S2,S).


% Phrase-structure rules

rule(s,[np,vp]).
rule(np,[d,n]).
rule(np,[np,conj,np]).  % left-corner can parse this, top-down can't
rule(vp,[v,np]).
rule(vp,[v,np,pp]).
rule(pp,[p,np]).
% rule(d,[]).           % not suitable for left-corner parser

% Lexicon

word(conj,and).

word(d,the).

word(p,near).

word(n,dog).       word(n,dogs).
word(n,cat).       word(n,cats).
word(n,elephant).  word(n,elephants).

word(v,chase).     word(v,chases).
word(v,see).       word(v,sees).
word(v,amuse).     word(v,amuses).


