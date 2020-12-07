% File TRANSDUC.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 9, Section 9.4.5


% KIMMO-like finite-state transducer (see text).

% To test: ?- transduce(1,State,[r,a,k,e,+,e,d],Surface).


% state(+Old,-New,?Undl,?Surf)
%  Allows moving from state Old to state New
%  by accepting Undl and Surf characters.

%% NOTE: In ESL Prolog-2 you must give this predicate
%%  a different name, as state/4 is already used internally.

state(1,2,C,C) :- \+ vowel(C), !.
state(1,1,X,X).
state(2,3,e,0) :- !.
state(2,2,C,C) :- \+ vowel(C), !.
state(2,1,X,X).
state(3,4,+,0).
state(4,5,V,V) :- vowel(V).
state(5,1,X,X).


% final_state(?N)
%  N is a state in which the transducer can stop.

final_state(1).
final_state(2).
final_state(5).


% vowel(V)
%  V is a vowel.

vowel(a).  vowel(e).  vowel(i).  vowel(o).  vowel(u).  vowel(y).


% transduce(Start,Finish,UndlString,SurfString)

transduce(Start,Finish,[U|UndlString],[S|SurfString]) :-
  state(Start,Next,U,S),
  transduce(Next,Finish,UndlString,SurfString).

transduce(Start,Start,[],[]) :-
  final_state(Start).
