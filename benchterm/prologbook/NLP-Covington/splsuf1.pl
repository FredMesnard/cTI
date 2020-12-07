% File SPLSUF1.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 9, Section 9.3.4

% Morphological analyzer with enhanced ability to recognize suffixes

% To test:  ?- mparse([q,u,i,z,z,e,s],Result).


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


% vowel(?Char)
%  Char is a vowel.

vowel(a).  vowel(e).  vowel(i).  vowel(o).  vowel(u).  vowel(y).



% find_word(+Word,+Tree,-LexEntry)
%  Finds Word in Tree retrieving LexEntry.

find_word([H|T],Tree,LexEntry) :-
   find_branch(H,Tree,SubTree),
   find_word(T,SubTree,LexEntry).

find_word([],[LexEntry|_],LexEntry) :-
   \+ (LexEntry = [_|_]).

find_word(Chars,Tree,Root+Suffix) :-
   split_suffix(Chars,Stem,Suffix),     % note the change here
   find_word(Stem,Tree,Root).


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


% split_suffix(+Characters,-Root,-Suffix)
%  Splits a word into root and suffix.
%  Fails if there is no suffix.

% Suffixes with doubled consonants in root
split_suffix([V,z,z,e,s],[V,z],s).  % "quizzes", "whizzes"
split_suffix([V,s,s,e,s],[V,s],s).  % "gasses" (verb)
split_suffix([V,C,C,V2|Rest],[V,C],Suffix) :-
   vowel(V), \+ vowel(C), vowel(V2), suffix([V2|Rest],Suffix).

% y changing to i and -s changing to -es simultaneously
split_suffix([C,i,e,s],[C,y],s) :- \+ vowel(C).

% y changes to i after consonant, before suffix beg. w vowel
split_suffix([C,i,X|Rest],[C,y],Suffix) :-
   \+ vowel(C), \+ (X = i), suffix([V|Rest],Suffix).

% -es = -s after s (etc.)
split_suffix([s,h,e,s],[s,h],s).
split_suffix([c,h,e,s],[c,h],s).
split_suffix([s,e,s],[s],s).
split_suffix([z,e,s],[z],s).
split_suffix([x,e,s],[x],s).

% Final e drops out before a suffix beg. with a vowel
split_suffix([C,V|Rest],[C,e],Suffix) :-
   \+ vowel(C), vowel(V), suffix([V|Rest],Suffix).

% Ending of word exactly matches suffix.
split_suffix(Ending,[],Suffix) :- suffix(Ending,Suffix).


