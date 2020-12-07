% Computational Intelligence: a logical approach. 
% Prolog Code. A DCG grammar for outputting canned English from Figure 3.7.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

/* The DCG approach to solving this problem is probably the most
general and straightforward.  It supports operation in both modes:
Prolog -> English, and English -> Prolog.  This implementation is
absolutely rudimentary.

This is used by the query: s(Prolog, English, []). 
try:
| ?- s(scheduled(w92,cs422,pm(1,30),above(cpsc333)),English,[]).
| ?- q(scheduled(w92,cs422,pm(1,30),above(cpsc333)),English,[]).

*/

% s(Prolog, English, []) is true when there is a mapping from the Prolog
%   to the English forms of the sentence.

% these are the kinds of sentences for which the grammar knows what to do:
q(scheduled(S,C,T,R)) -->         % QUERY example
   [is], section(S), [of], course(C), [scheduled, at], time(T), [in], room(R).

s(scheduled(S,C,T,R)) -->         % ASSERTION
   section(S), [of], course(C), [is, scheduled, at], time(T), [in], room(R).

% the keywords are really just special cases of the production rules:
section(w92)   --> [the, spring, 1992, session].

course(cs422)  --> [the, intelligent, systems, course].

time(pm(H,M))  --> [H, :, M, pm].
time(noon)     --> [noon].

room(cpsc333)  --> [the, computer, science, departmental, office].
room(above(R)) --> [the, room, above], room(R).

