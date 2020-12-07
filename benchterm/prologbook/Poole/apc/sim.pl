% Computational Intelligence: a logical approach. 
% Prolog Code.
% ROBOT CONTROLLER (Section C.7)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

:- op(1110,xfx,<-).  % object-level "IF"
:- op(1000,xfy,&).   % object-level "AND"
:- op(950,fy,~).     % object-level "NOT"
:- op(700,xfx,~=).   % object-level "NOT EQUALS"

% assigned(Fl,Val,T) is true if fluent FL was
% assigned value Val at time T. We exploit the fact
% that the simulation runs forward and make sure that
% the first fact in the database always represents
% the latest time that the fluent was assigned a
% value. This means that we can always look up the
% last value assigned to a fluent quickly.
:- dynamic(assigned/3).

% tried(Fl) is true if fluent Fl has been tried to determine
% if it should be assigned a value for the current time.
:- dynamic(tried/1).

% sim(T0,T2,DT) means simulate the system for all
% times in range [T0,T2] in increments of DT.
sim(T0,T2,DT) :-
   T0 =< T2,
   !,
   prove_all_assigns(T0),
   view_all(T0),
   retractall(tried(_)),
   T1 is T0+DT,
   sim(T1,T2,DT).
sim(_,_,_).

% clear clears the database for another simulation
clear :-
   retractall(assigned(_,_,_)).

% prove(G,T) is true if G can be proved at time T, but where
% special care is taken to remember state (assigned
% values) rather than recomputing.
prove(true,_) :- !.
prove((A & B),T) :- !,
   prove(A,T),
   prove(B,T).
prove((A ; B),T) :- !,
   (prove(A,T);
   prove(B,T)).
prove(val(Fl,Val,T),T) :-
   \+ tried(Fl),
   asserta(tried(Fl)),
   prove(assign(Fl,V1,T),T),
   asserta(assigned(Fl,V1,T)),
   !,
   Val=V1.
prove(val(Fl,Val,T),_) :-
      % either tried or can't currently be assigned 
      % look up latest value
   assigned(Fl,V1,T1),
   T1 =< T,
   !,
   Val=V1 .
prove(was(Fl,Val,T1,T),_) :-
   assigned(Fl,V1,T1),
   T1 < T, !,
   Val=V1.
prove((A ~= B),_) :-
   \+ (A = B).
prove((~ G),T) :-!,
   \+ prove(G,T).
prove(G,_) :-
   builtin(G),
   !,
   call(G).
prove(H,T) :-
   (H <- B),
   prove(B,T).

% builtin(G) is true if G is built-in
builtin((_ =< _)).
builtin((_ >= _)).
builtin((_ = _)).
builtin((_ < _)).
builtin((_ > _)).
builtin((_ is _)).

% prove_all_assigns(T) is true if all assignments
% of values to variables are proved and remembered
% for time T
prove_all_assigns(T) :-
   fluent(Fl),
   \+ tried(Fl),
   asserta(tried(Fl)),
   prove(assign(Fl,Val,T),T),
   asserta(assigned(Fl,Val,T)),
   fail.
prove_all_assigns(_).

fluent(Fl) :-
   (assign(Fl,_,_) <- _).

% view_all(T) lets us print out all of the "view"
% variables for time T. This lets us monitor the
% simulation.  view(G,T,P) is true if the elements
% of list P should be printed when G is proved at 
% time T
view_all(T) :-
   view(G,T,P),
   prove(G,T),
   writeln(P),
   fail.
view_all(_).
