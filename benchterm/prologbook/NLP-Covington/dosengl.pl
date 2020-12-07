% File DOSENGL.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 2

% DOS-in-English system.
% This version runs in ALS Prolog; comments explain
% how to adapt it to other versions of Prolog.


% :- state(token_class,_,dec10).  % un-comment this in ESL Prolog-2 only

% Requires code from READATOM.PL
:- reconsult('readatom.pl').
% In Arity Prolog, reconsult 'readatom.ari' instead.


% sr(T1,T2)
%  Simplification rules.
%  (Adding more is left as an exercise for the student.)

sr([the|X],X).
sr([is|X],X).
sr([are|X],X).
sr([there|X],X).
sr([any|X],X).
sr([disk,in,drive|X],[drive|X]).
sr([disk,in|X],[drive|X]).
sr([disk|X],[drive|X]).
sr([what,files|X],[files|X]).
sr([what|X],[files|X]).
sr([file|X],[files|X]).
sr([everything|X],[all,files|X]).
sr([every|X],[all|X]).


% simplify(+List,-Result)
%  Applies simplification rules to List giving Result.

simplify(List,Result) :-             % A simp. rule matches
   sr(List,NewList),                 % so apply it and then
   !,                                % try to further simplify
   simplify(NewList,Result).         % the result

simplify([W|Words],[W|NewWords]) :-  % No simp. rule matches
   simplify(Words,NewWords).         % so advance to next word

simplify([],[]).                     % No more words


% tr(?Input,?Result)
%  Translation rules.
%  (Adding more is left as an exercise for the student.)

tr([files,on,drive,X,'?'],['dir ',X,':']).
tr([X,files,on,drive,Y,'?'],['dir ',Y,':*.',X]).
tr([copy,all,files,from,drive,X,to,drive,Y,'.'],
                                    ['copy ',X,':*.* ',Y,':']).
tr([quit],[quit]).


% translate(-Input,+Result)
%  Applies a translation rule, or complains
%  if no translation rule matches the input.

translate(Input,Result) :-
   tr(Input,Result),
   !.

translate(_,[]) :-
   write('I do not understand.'),
   nl.


% pass_to_os(Command)
%  Accepts a command as a list of atoms, concatenates
%  the atoms, and passes the command to the operating system.
%  Ignores [quit] (the quit command) and [] (unrecognized command).

pass_to_os([quit]) :- !.

pass_to_os([]) :- !.

pass_to_os(Command) :-
%
% Un-comment ONE of the following lines to suit your version of Prolog.
%
   make_string(Command,S),system(S).                           % ALS Prolog
%  concat(Command,S),shell(S).                                 % Arity Prolog
%  make_string(Command,S),name(C,S),unix(system(C)).           % Quintus (UNIX)
%  make_string(Command,S),name(C,S),vms(dcl(C)).               % Quintus (VMS)
%  make_string(Command,S),name(C,S),dos(C).                    % LPA (MS-DOS)
%  make_string(Command,S),list(S,C),command(C,32000,display).  % ESL Prolog-2


% make_string+ListOfAtoms,-String)
%  Concatenates a list of atoms giving a string.
%  Example: ['a','b','c'] gives "abc".
%  Not used in Arity Prolog (use built-in concat/2 instead).

make_string[H|T],Result) :-
   name(H,Hstring),
   make_stringT,Tstring),
   append(Hstring,Tstring,Result).

make_string[],[]).


% append(?List1,?List2,?List3)
%  Appends List1 to List2 giving List3.

append([H|T],L,[H|Rest]) :- append(T,L,Rest).
append([],L,L).


% process_commands
%  Repeatedly accepts commands in English,
%  simplifies and translates them,
%  and passes them to the operating system.

process_commands :-
   repeat,
     write('Command: '),
     read_atomics(Words),    % defined in Appendix B
     simplify(Words,SimplifiedWords),
     translate(SimplifiedWords,Command),
     write(Command),nl,      % so you can see what's going on
     pass_to_os(Command),
     Command == [quit],
   !.

