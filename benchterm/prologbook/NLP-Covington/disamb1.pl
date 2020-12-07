% File DISAMB1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 8, Section 8.3.2

% Word sense disambiguation -- Approach 1
% Scan sentence for cues, building a list of active contexts.
% Then use active contexts to disambiguate ambiguous words.

% To test:  ?- disambiguate([there,are,pigs,in,the,pen],W,C).


% disambiguate(+Words,-DWords,-Contexts)
%  disambiguates Words creating DWords and Contexts.

disambiguate(Words,DWords,Contexts) :-
  collect_cues(Words,[],Contexts),
  disambiguate_words(Words,DWords,Contexts).


% collect_cues(+Words,+Contexts,-NewContexts)
%  scans Words looking for cues, and adding appropriate
%  contexts to Contexts giving NewContexts.

collect_cues([],Contexts,Contexts).

collect_cues([W|Words],Contexts,NewContexts) :-
  cue(W,C),
  \+ member(C,Contexts),
  !,
  collect_cues(Words,[C|Contexts],NewContexts).

collect_cues([_|Words],Contexts,NewContexts) :-
  collect_cues(Words,Contexts,NewContexts).



% disambiguate_words(+Words,-DWords,+Contexts)
%  goes through Words using Contexts to disambiguate them.

disambiguate_words([],[],_).

disambiguate_words([W|Words],[D|DWords],Contexts) :-
  mic(W,D,C),     % W is ambiguous and means D in context C
  member(C,Contexts),
  disambiguate_words(Words,DWords,Contexts).

disambiguate_words([W|Words],[W|DWords],Contexts) :-
  \+ mic(W,_,_),  % W is not listed as ambiguous
  disambiguate_words(Words,DWords,Contexts).


% member(?E,?L)
%  E is an element of list L

member(E,[E|_]).
member(E,[_|L]) :- member(E,L).

% cue(?Word,?Context)
%  Word is a cue for Context

cue(farmer,    farm).
cue(pigs,      farm).
cue(ink,       writing).
cue(airport,   aircraft).
cue(carpenter, woodworking).

% mic(?Word,?Meaning,?Context)
%  Word has specified Meaning in Context

mic(pen,      pen_for_animals,   farm).
mic(pen,      writing_pen,       writing).
mic(plane,    plane_tool,        woodworking).
mic(plane,    airplane,          aircraft).
mic(terminal, airport_terminal,  aircraft).
mic(terminal, computer_terminal, computer).

