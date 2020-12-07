% Computational Intelligence: a logical approach. 
% Prolog Code.
% A natural language parser using context free grammar from Figure 3.5
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% a sentence is a noun phrase followed by a verb phrase.
sentence(T_0,T_2) :- 
   noun_phrase(T_0,T_1), 
   verb_phrase(T_1,T_2).

% a noun phrase is a determiner followed by modifiers followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T_0,T_4) :- 
   det(T_0,T_1), 
   modifiers(T_1,T_2), 
   noun(T_2,T_3), 
   pp(T_3,T_4).

% an optional noun phrase is either nothing or a noun phrase
opt_noun_phrase(T,T).
opt_noun_phrase(T_0,T_1) :-
   noun_phrase(T_0,T_1).

% a verb phrase is a verb followed by a noun phrase and an optional pp
verb_phrase(T_0,T_3) :- 
   verb(T_0,T_1), 
   opt_noun_phrase(T_1,T_2), 
   pp(T_2,T_3).

% an optional prepositional phrase is either
% nothing or a preposition followed by a noun phrase
pp(T,T).
pp(T_0,T_2) :-
   preposition(T_0,T_1),
   noun_phrase(T_1,T_2).

% modifiers is a sequence of adjectives
modifiers(T,T).
modifiers(T0,T2) :-
    adjective(T0,T1),
    modifiers(T1,T2).

% dictionary
det(T,T).
det([a|T],T).
det([the|T],T).
noun([student|T],T).
noun([course|T],T).
noun([computer|T],T).
adjective([practical | T],T).
verb([passed|T],T).
verb([failed|T],T).
preposition([with|T],T).

% Example Query:
%? sentence([the,student,passed,the,practical,course,with,a,computer],R).
