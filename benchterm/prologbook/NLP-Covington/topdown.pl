% File TOPDOWN.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.2

% Recursive-descent top-down parser


% parse(?C,?S1,?S)
%  Parse a constituent of category C
%  starting with input string S1 and
%  ending up with input string S.

parse(C,[Word|S],S) :-
   word(C,Word).

parse(C,S1,S) :-
   rule(C,Cs),
   parse_list(Cs,S1,S).


% parse_list(+Cs,?S1,?S)
%  Like parse/3, but Cs is a list of
%  categories to be parsed in succession.

parse_list([C|Cs],S1,S) :-
   parse(C,S1,S2),
   parse_list(Cs,S2,S).

parse_list([],S,S).



% Phrase-structure rules

rule(s,[np,vp]).
rule(np,[d,n]).
rule(vp,[v,np]).
rule(vp,[v,np,pp]).
rule(pp,[p,np]).
rule(d,[]).

% Lexicon

word(d,the).

word(p,near).

word(n,dog).       word(n,dogs).
word(n,cat).       word(n,cats).
word(n,elephant).  word(n,elephants).

word(v,chase).     word(v,chases).
word(v,see).       word(v,sees).
word(v,amuse).     word(v,amuses).

