% File SCRIPT.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 8, Section 8.4.3


% Miniature script applier based on McSAM (Cullingford 1981)
% To test:  See tests at end.

% apply_script(+Events,-Result)
%  takes a list of events and applies a script to them,
%  giving a more detailed list of events.

apply_script(Events,Result) :-
   script(Script),
   apply_aux(Script,Events,Result).

apply_aux([E|Script],[E|Events],[E|Result]) :-
   apply_aux(Script,Events,Result).
   % Event in script matches actual event, so use it

apply_aux([S|Script],[E|Events],[S|Result]) :-
   \+ (E = S),
   apply_aux(Script,[E|Events],Result).
   % Event in script matches no actual event,
   % so add it to the list.

apply_aux(Script,[],Script).
   % If events are used up, fill in the rest of the
   % script (which may be empty) and stop.


% Script for buying something at a store

script([ptrans(Actor,Actor,_,Store),     % go to store,
        ptrans(Actor,Item,_,Actor),      % get an item,
        atrans(Actor,Money,Actor,Store), % pay for it,
        atrans(Store,Item,Store,Actor),  % obtain ownership,
        ptrans(Actor,Actor,Store,_)]).   % go away.



test0 :- apply_script([ptrans(john,john,home,macys)],What),
         write(What).
         % "John went to Macy's."

test1 :- apply_script([ptrans(john,scarf,macys,john)],What),
         write(What).
         % "John got a scarf from Macy's."

test2 :- apply_script([ptrans(john,john,home,macys),
                       atrans(john,'$5',john,macys)],What),
         write(What).
         % "John went to Macy's and spent $5."

test3 :- apply_script([ptrans(john,john,home,macys),
                       ptrans(john,scarf,_,john),
                       ptrans(john,john,_,home)],What),
         write(What).
         % "John went to Macy's, got a scarf, and went home."

