% File CHARTCS.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.5.5


% Chart parser with completeness and subsumption checks
% (using grammar from Section 6.5.5)

:- reconsult('subsumes.pl').  % not needed in Quintus Prolog


% Following line required in Quintus (LPA) DOS Prolog only,
% to prevent error messages if we try to look at the chart
% before asserting anything into it.
% '?ERROR?'(2,_) :- fail.


% clear_chart
%  Utility predicate to be used between
%  runs, to discard the entire chart.

  clear_chart :- abolish(chart/3), abolish(complete/2).  % most Prologs
% clear_chart :- abolish(chart,3), abolish(complete,2).  % ALS Prolog


% parse(+C,+S1,-S)
%  Parse a constituent of category C
%  starting with input string S1 and
%  ending up with input string S.

parse(C,[Word|S],S) :-
   word(C,Word).

parse(C,S1,S) :-
   complete(C0,S1),
   subsumes_chk(C0,C),
   !,
   C0 = C,
   chart(C,S1,S).

parse(C,S1,S) :-
   rule(C,Cs),
   parse_list(Cs,S1,S2),
   asserta(chart(C,S1,S2)),
   S2 = S.

parse(C,S1,_) :-
   asserta(complete(C,S1)),
   fail.

% parse_list(+Cs,+S1,-S)
%  Like parse/3, but Cs is a list of
%  categories to be parsed in succession.

parse_list([C|Cs],S1,S) :-
   parse(C,S1,S2),
   parse_list(Cs,S2,S).

parse_list([],S,S).



% Phrase-structure rules

rule(vp,[verbal(0)]).
rule(vp,[verbal(X),rest_of_vp(X)]).
rule(rest_of_vp(1),[np]).
rule(rest_of_vp(2),[np,np]).
rule(verbal(X),[v(X)]).
rule(np,[d,n]).

% Lexicon

word(d,the).

word(n,dog).       word(n,dogs).
word(n,cat).       word(n,cats).
word(n,elephant).  word(n,elephants).

word(v(0),sleep).
word(v(1),see).
word(v(2),give).

