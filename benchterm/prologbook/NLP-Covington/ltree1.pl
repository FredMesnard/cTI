% File LTREE1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 9, Section 9.3.2

% Lexical storage in a letter tree (first version)

% To test:  ?- mparse([q,u,a,r,r,y],Result).


% mparse(+Word,-Result)
%  Retrieves morphological description of Word (a charlist).

mparse(Word,Result) :-
  ltree(T),
  find_word(Word,T,Result).


% ltree(?Tree)
%  Stores the lexicon as a letter tree.

ltree([ [b, [a, [r, [k, bark]]]],
        [c, [a, [r, [r, [y, carry]]],
                [t, cat,
                    [e, [g, [o, [r, [y, category]]]]]]]],
        [d, [e, [l, [a, [y, delay]]]]],
        [h, [e, [l, [p, help]]],
            [o, [p, hop,
                    [e, hope]]]],
        [q, [u, [a, [r, [r, [y, quarry]]]],
                [i, [z, quiz]],
                [o, [t, [e, quote]]]]]
      ]).


% find_word(+Word,+Tree,-LexEntry) -- First version
%  Finds Word in Tree retrieving LexEntry.

find_word([H|T],Tree,LexEntry) :-
   member([H|Branches],Tree),
   find_word(T,Branches,LexEntry).

find_word([],Tree,LexEntry) :-
   member(LexEntry,Tree),
   \+ (LexEntry = [_|_]).


% member(?E,?L)
%  E is an element of list L

member(E,[E|_]).
member(E,[_|L]) :- member(E,L).

