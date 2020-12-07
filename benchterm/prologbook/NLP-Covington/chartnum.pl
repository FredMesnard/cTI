% File CHARTNUM.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.5.3

% Simple chart parser (without completeness check)
% representing all positions as numbers

% To test:   ?- clear_chart, parse(s,0,8).


% Following line required in Quintus (LPA) DOS Prolog only,
% to prevent error messages if we try to look at the chart
% before asserting anything into it.
% '?ERROR?'(2,_) :- fail.


% c(?Word,?Position1,?Position2)
%   Contains the sentence to be parsed

c(the,0,1).
c(dog,1,2).
c(sees,2,3).
c(the,3,4).
c(cat,4,5).
c(near,5,6).
c(the,6,7).
c(elephant,7,8).


% clear_chart
%  Utility predicate to be used between
%  runs, to discard the entire chart.

  clear_chart :- abolish(chart/3).  % most Prologs
% clear_chart :- abolish(chart,3).  % ALS Prolog only



% parse(+C,+S1,-S)
%  Parse a constituent of category C
%  starting with input string S1 and
%  ending up with input string S.

parse(C,S1,S) :-
   chart(C,S1,S).

parse(C,S1,S) :-
   c(Word,S1,S),   % this is the only change
   word(C,Word).

parse(C,S1,S) :-
   rule(C,Cs),
   parse_list(Cs,S1,S),
   asserta(chart(C,S1,S)).


% parse_list(+Cs,+S1,-S)
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

