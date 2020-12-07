% File MINIGUL1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 5, Section 5.4.2

% First version of Mini-GULP translator

:- op(490,xfy,':').
:- op(495,xfy,'..').


% g_translate(+FeatureStructure,-g_(List)) (FIRST VERSION)
%  Translates FeatureStructure to internal representation g_(List).

% Case 1: A single feature-value pair
%
g_translate(F:V,g_(List)) :-
   g_schema(F:V,List).

% Case 2: A series of feature-value pairs
%
g_translate(First..Rest,g_(List)) :-
   g_translate(First,g_(List)),
   g_translate(Rest,g_(List)).



% Some schemas for testing

g_schema(case:X,  [X,_,_,_,_]).
g_schema(person:X,[_,X,_,_,_]).
g_schema(number:X,[_,_,X,_,_]).
g_schema(sem:X,   [_,_,_,X,_]).
g_schema(pred:X,  [_,_,_,_,X]).


% Some test code

test1 :- g_translate(case:nom,What), write(What), nl.
test2 :- g_translate(case:acc..person:3,What), write(What), nl.
test3 :- g_translate(person:3..case:acc,What), write(What), nl.
test4 :- g_translate(number:plural..sem:(pred:chases)..person:2,What),
             write(What), nl.

