% File SUFFIX.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 9, Section 9.3.3

% Morphological analyzer with limited ability to recognize suffixes

% To test:  ?- mparse([q,u,a,r,r,y,i,n,g],Result).


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


% suffix(?Chars,?Morpheme)
%  Chars is a suffix, Morpheme is the representation
%  of it (identifying the morpheme).

suffix([s],s).
suffix([e,d],ed).
suffix([e,d],en).    % Ambiguous verb suffix
suffix([i,n,g],ing).


% find_word(+Word,+Tree,-LexEntry)
%  Finds Word in Tree retrieving LexEntry.

find_word([H|T],Tree,LexEntry) :-
   find_branch(H,Tree,SubTree),
   find_word(T,SubTree,LexEntry).

find_word([],[LexEntry|_],LexEntry) :-
   \+ (LexEntry = [_|_]).

find_word(Ending,Tree,Result+Suffix) :-    % Additional rule
   suffix(Ending,Suffix),                  %  for suffixes
   find_word([],Tree,Result).


% find_branch(+Letter,+Tree,-LexEntry)
%  Given a letter and a tree, returns the
%  appropriate (unique) subtree.  Deterministic.

find_branch(Letter,[[Letter|LexEntry]|_],LexEntry) :- !.
  % Found it; there is only one.

find_branch(Letter,[[L|_]|_],_) :- Letter @< L, !, fail.
  % Went past where it should be; don't search any further.

find_branch(Letter,[_|Rest],LexEntry) :-
  find_branch(Letter,Rest,LexEntry).
  % Haven't found it yet, so advance to next branch.



