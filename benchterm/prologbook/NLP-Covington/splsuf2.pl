% File SPLSUF2.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 9, Section 9.3.5

% Morphological analyzer with enhanced ability to recognize suffixes
% and with limits on overgeneration

% To test:  ?- mparse([h,o,p,i,n,g],Result).


% mparse(+Word,-Result)
%  Retrieves morphological description of Word (a charlist).

mparse(Word,Result) :-
  ltree(T),
  find_word(Word,T,Result).


% ltree(?Tree)
%  Stores the lexicon as a letter tree.

ltree([ [b, [a, [r, [k, word(bark,verb,1)]]]],
        [c, [a, [r, [r, [y, word(carry,verb,1)]]],
                [t, word(cat,noun,1),
                    [e, [g, [o, [r, [y, word(category,noun,1)]]]]]]]],
        [d, [e, [l, [a, [y, word(delay,verb,1)]]]]],
        [g, [o, word(go,verb,0),
                [n, [e, word(go+en,verb,0)]]]],
        [h, [e, [l, [p, word(help,verb,1)]]],
            [o, [p, word(hop,verb,1),
                    [e, word(hope,verb,1)]]]],
        [q, [u, [a, [r, [r, [y, word(quarry,verb,1)]]]],
                [i, [z, word(quiz,verb,1)]],
                [o, [t, [e, word(quote,verb,1)]]]]],
        [w, [e, [n, [t, word(go+ed,verb,0)]]]]
      ]).


% suffix(?Chars,?Morpheme,?Category)
%  If Chars is a suffix, Morpheme is the description of it,
%  and Category is the syntactic category it attaches to.

suffix([s],s,_).          % let it attach to any category
suffix([e,d],ed,verb).
suffix([e,d],en,verb).    % ambiguous suffix
suffix([i,n,g],ing,verb).


% vowel(?Char)
%  Char is a vowel.

vowel(a).  vowel(e).  vowel(i).  vowel(o).  vowel(u).  vowel(y).



% find_word(+Word,+Tree,-LexEntry)
%  Finds Word in Tree retrieving LexEntry
%  where LexEntry = word(Form,Category,Inflectable).

find_word([],[LexEntry|_],LexEntry) :-
   \+ (LexEntry = [_|_]).

find_word([H|T],Tree,LexEntry) :-
   find_branch(H,Tree,SubTree),
   find_word(T,SubTree,LexEntry).

find_word(Chars,Tree,word(Form+Suffix,Cat,0)) :-     % use split_suffix
   split_suffix(Chars,Stem,Suffix,Cat),
   find_word(Stem,Tree,word(Form,Cat,1)).  % only use inflectable roots


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


% split_suffix(+Characters,-Root,-Suffix,-Category)
%  Splits a word into root and suffix.
%  Fails if there is no suffix.
%  Instantiates Category to the syntactic category
%  to which this suffix attaches.

% Suffixes with doubled consonants in root
split_suffix([V,z,z,e,s],[V,z],s,_).  % "quizzes", "whizzes"
split_suffix([V,s,s,e,s],[V,s],s,_).  % "gasses" (verb)
split_suffix([V,C,C,V2|Rest],[V,C],Suffix,Cat) :-
   vowel(V), \+ vowel(C), vowel(V2), suffix([V2|Rest],Suffix,Cat).

% y changing to i and -s changing to -es simultaneously
split_suffix([C,i,e,s],[C,y],s,_) :- \+ vowel(C).

% y changes to i after consonant, before suffix beg. w vowel
split_suffix([C,i,X|Rest],[C,y],Suffix,Cat) :-
   \+ vowel(C), \+ (X = i), suffix([V|Rest],Suffix,Cat).

% -es = -s after s (etc.)
split_suffix([s,h,e,s],[s,h],s,_).
split_suffix([c,h,e,s],[c,h],s,_).
split_suffix([s,e,s],[s],s,_).
split_suffix([z,e,s],[z],s,_).
split_suffix([x,e,s],[x],s,_).

% Final e drops out before a suffix beg. with a vowel
split_suffix([C,V|Rest],[C,e],Suffix,Cat) :-
   \+ vowel(C), vowel(V), suffix([V|Rest],Suffix,Cat).

% Ending of word exactly matches suffix.
% (Does not apply if Doubling was required to apply.
%  This eliminates spurious ambiguities such as hoped=hop+ed.)
split_suffix([A,B,C|Rest],[A,B],Suffix,Cat) :-
   suffix([C|Rest],Suffix,Cat),
   \+ (vowel(A), \+ vowel(B), vowel(C)).  % Doubling

