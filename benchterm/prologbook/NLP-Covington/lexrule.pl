% File LEXRULE.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.6

% Implementation of a lexical rule to recognize
% nouns that end in  -ness
% by looking up the corresponding adjective.

n --> [X], { noun(X) }.

noun(dog).
noun(cat).
noun(gardener).

noun(N) :-
   name(N,Nchars),
   append(Achars,"ness",Nchars),
   name(A,Achars),
   adjective(A).

adj --> [X], { adjective(X) }.

adjective(flat).
adjective(green).
adjective(blue).


append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).


% To test:
% ?- n([dog],[]).
% yes
% ?- n([flatness],[]).
% yes
