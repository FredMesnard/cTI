% File DISAMB2.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 8, Section 8.3.3

% Word sense disambiguation -- Approach 2
% Choose the readings that activate the fewest contexts.

% To test:  ?- disambiguate([there,are,pigs,in,the,pen],What).


% Word sense disambiguation -- Second program

% disambiguate(+Words,-Readings)
%  Readings is a list of structures of the form
%  reading(NumberOfContexts,ListOfContexts,DisambiguatedWords)
%  listing the readings that activate the fewest contexts first.

disambiguate(Words,Readings) :-
   setof(reading(N,Contexts,DWords),
         (dis1(Words,DWords,[],Contexts),length(Contexts,N)),
         Readings).

% dis1(Words,DWords,Contexts,NewContexts)
%  Find 1 disambiguated reading of Words, placing it in DWords,
%  adding newly activated contexts to Contexts giving NewContexts.

dis1([],[],Contexts,Contexts).    % end of list

dis1([W|Words],[D|DWords],Contexts,NewContexts) :-
  mic(W,D,C),
  member(C,Contexts),     % W means D in a context already active
  dis1(Words,DWords,Contexts,NewContexts).

dis1([W|Words],[D|DWords],Contexts,NewContexts) :-
  mic(W,D,C),
  \+ member(C,Contexts),  % W means D by activating a new context
  dis1(Words,DWords,[C|Contexts],NewContexts).

dis1([W|Words],[W|DWords],Contexts,NewContexts) :-
  \+ mic(W,_,_),          % W is not ambiguous
  dis1(Words,DWords,Contexts,NewContexts).


% member(?E,?L)
%  E is an element of list L

member(E,[E|_]).
member(E,[_|L]) :- member(E,L).


% mic(?Word,?Meaning,?Context)
%  Word has specified Meaning in Context.

mic(farmer,    farmer,            farm).
mic(pigs,      pigs,              farm).
mic(ink,       ink,               writing).
mic(airport,   airport,           aircraft).
mic(carpenter, carpenter,         woodworking).
mic(pen,       pen_for_animals,   farm).
mic(pen,       writing_pen,       writing).
mic(plane,     plane_tool,        woodworking).
mic(plane,     airplane,          aircraft).
mic(terminal,  airport_terminal,  aircraft).
mic(terminal,  computer_terminal, computer).


