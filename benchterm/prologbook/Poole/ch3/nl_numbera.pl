% Computational Intelligence: a logical approach. 
% Prolog Code. The natural language parser with number agreement of Figure 3.8.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% a sentence is a noun phrase followed by a verb phrase.
sentence(T_0,T_2,Num,s(NP,VP)) :- 
   noun_phrase(T_0,T_1,Num,NP), 
   verb_phrase(T_1,T_2,Num,VP).
% a noun phrase is a determiner followed by modifiers followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T,T,_,nonp).
noun_phrase(T_0,T_4,Num,np(Det,Mods,Noun,PP)) :- 
   det(T_0,T_1,Num,Det), 
   modifiers(T_1,T_2,Mods), 
   noun(T_2,T_3,Num,Noun), 
   pp(T_3,T_4,PP).

% a verb phrase is a verb followed by a noun phrase and an optional pp
verb_phrase(T_0,T_3,Num,vp(V,NP,PP)) :- 
   verb(T_0,T_1,Num,V), 
   noun_phrase(T_1,T_2,N2,NP), 
   pp(T_2,T_3,PP).

% an optional prepositional phrase is either
% nothing or a preposition followed by a noun phrase
pp(T,T,nopp).
pp(T_0,T_2,pp(Prep,NP)) :-
   preposition(T_0,T_1,Prep),
   noun_phrase(T_1,T_2,NP).

% modifiers is a sequence of adjectives
modifiers(T,T,[]).
modifiers(T0,T2,[A|M]) :-
    adjective(T0,T1,A),
    modifiers(T1,T2,M).


det([a|T],T,singular,indefinite).
det([the|T],T,Num,definite).
adjective([nervous | T],T,nervous).
noun([student|T],T,singular,student).
noun([students|T],T,plural,student).
verb([eats|T],T,singular,eat).
verb([eat|T],T,plural,eat).
preposition([with|T],T,with).
