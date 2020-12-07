% File BOTTOMUP.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.3.2

% Bottom-up shift-reduce parser

% parse(+S,?Result)
%  parses input string S, where Result
%  is list of categories to which it reduces.

parse(S,Result) :-
  shift_reduce(S,[],Result).


% shift_reduce(+S,+Stack,?Result)
%  parses input string S, where Stack is
%  list of categories parsed so far.

shift_reduce(S,Stack,Result) :-
  shift(Stack,S,NewStack,S1),     % fails if S = []
  reduce(NewStack,ReducedStack),
  shift_reduce(S1,ReducedStack,Result).

shift_reduce([],Result,Result).


% shift(+Stack,+S,-NewStack,-NewS)
%  shifts first element from S onto Stack.

shift(X,[H|Y],[H|X],Y).



% reduce(+Stack,-ReducedStack)
%  repeatedly reduces beginning of Stack
%  to form fewer, larger consituents.

reduce(Stack,ReducedStack) :-
  brule(Stack,Stack2),
  reduce(Stack2,ReducedStack).

reduce(Stack,Stack).




% Phrase structure rules

%  These are stored with constituents backward and
%  with variables as tails, so they can be used
%  directly as schemas for reducing.

%  Here "brule" means "backward rule."

brule([vp,np|X],[s|X]).
brule([n,d|X],[np|X]).
brule([np,v|X],[vp|X]).
brule([pp,np,v|X],[vp|X]).
brule([np,p|X],[pp|X]).

brule([np,conj,np|X],[np|X]).

brule([Word|X],[Cat|X]) :- word(Cat,Word).



% Lexicon

word(conj,and).

word(p,near).

word(d,the).

word(n,dog).       word(n,dogs).
word(n,cat).       word(n,cats).
word(n,elephant).  word(n,elephants).

word(v,chase).     word(v,chases).
word(v,see).       word(v,sees).
word(v,amuse).     word(v,amuses).


