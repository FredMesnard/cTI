% File MINIGULP.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 5, Section 5.4.4-5.4.5

% Finished Mini-GULP translator

% To use:
% ?- reconsult('minigulp.pl').
% ?- g_consult(...file of input for GULP...).
% See SAMPLE1.GLP for further information.


% Following 2 lines are needed ONLY in ALS Prolog:
:- reconsult(dcgs).
expand_term(X,Y) :- builtins:dcg_expand(X,Y).


:- op(490,xfy,':').
:- op(495,xfy,'..').


% g_translate(+FeatureStructure,-g_(List)) (SECOND VERSION)
%  Translates FeatureStructure to internal representation g_(List).

% Case 1: A variable or atomic term
%
g_translate(X,X) :-
   (var(X) ; atomic(X)), !.

% Case 2: A single feature-value pair
%
g_translate(F:V,g_(List)) :-
   !,
   g_translate(V,TranslatedV),
   g_schema(F:TranslatedV,List).

% Case 3: A series of feature-value pairs
%
g_translate(First..Rest,g_(List)) :-
   !,
   g_translate(First,g_(List)),
   g_translate(Rest,g_(List)).

% Case 4: A structure
%
g_translate(Structure,Result) :-
   Structure =.. [Functor|Args],
   !,
   g_translate_aux(Args,NewArgs),  % translate all args
   Result =.. [Functor|NewArgs].


g_translate_aux([T|Terms],[NewT|NewTerms]) :-
   g_translate(T,NewT),
   g_translate_aux(Terms,NewTerms).

g_translate_aux([],[]).



% g_consult(+File)
%  Reads clauses from File, translating as appropriate.

g_consult(File) :-
   see(File),
   repeat,
     read(Term),
     g_consult_aux(Term),   % handle it appropriately
   Term == end_of_file,
   !,
   seen.

g_consult(_) :-     % if something went wrong in previous clause
   seen,
   write('g_consult failed.'),
   nl.


g_consult_aux(end_of_file) :- !.

g_consult_aux(g_schema(X,Y)) :-
    !,
    assertz(g_schema(X,Y)).

g_consult_aux((X-->Y)) :-
    !,
    g_translate((X-->Y),Rule),
    expand_term(Rule,NewRule),   % DCG translator
    assertz(NewRule).

g_consult_aux(Term) :-
    g_translate(Term,TranslatedTerm),
    assertz(TranslatedTerm).


% g_write(+g_(List))
%  Produces legible output of a feature structure in internal form.
%  Assumes all necessary schemas are present.
%  Imperfect; limitations are noted in text.

g_write(g_(Features)) :-
   !,
   write('('),
   setof(FV,g_schema(FV,Features),FVList),
   g_write_aux(FVList),
   write(')').

g_write(X) :-
   write(X).


g_write_aux([]) :-
   !.

g_write_aux([F:V|Rest]) :-
   var(V),
   !,
   g_write_aux(Rest).

g_write_aux([F:V|Rest]) :-
   !,
   write(F),
   write(':'),
   g_write(V),
   write('..'),
   g_write_aux(Rest).

g_write_aux(X) :-
   write(X).



