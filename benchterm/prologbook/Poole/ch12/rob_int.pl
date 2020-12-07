:- multifile '<-' .
:- op(1110,xfx,<-).
:- op(1000,xfy,&).
:- op(950,fy,~).
:- op(700,xfx,~=).
:- dynamic assigned/3.


% sim(T0,T2,DT) means simulate the system for all times in range [T0,T2] in
% increments of DT.
sim(T0,T2,DT) :-
   T0 =< T2,
   prove_all_assigns(T0),
   view_all(T0),
   T1 is T0+DT,
   sim(T1,T2,DT).

% prove(G) is true if G can be proved, but where special care is taken to
% remember state (assigned values) rather than recomputing.
prove(true) :- !.
prove((A & B)) :- !,
   prove(A),
   prove(B).
prove(val(Fl,Val,T)) :-
   assigned(Fl,V1,T),!,Val=V1.
prove(val(Fl,Val,T)) :-
    prove(assign(Fl,V1,T)),!,Val=V1.
prove(val(Fl,Val,_)) :-
   assigned(Fl,V1,_),!,Val=V1.
prove(was(Fl,Val,T1,T)) :-
   assigned(Fl,V1,T1),
   T1 < T, !,
   Val=V1.
prove((~ G)) :-
   when(ground(G),\+ prove(G)).
prove((A ~= B)) :-
   dif(A,B).
prove((A < B)) :-
   when(ground((A<B)),(A<B)).
prove((A =< B)) :-
   when(ground((A =< B)),(A =< B)).
prove((A > B)) :-
   when(ground((A > B)),(A > B)).
prove((A >= B)) :-
   when(ground((A >= B)),(A >= B)).
prove((A is E)) :-
   when(ground(E),(A is E)).
prove(H) :-
   (H <- B),
   prove(B).

% prove_all_assigns(T) is true if all assignments of values to variables
% are proved and remebered for time T
prove_all_assigns(T) :-
   prove(assign(Fl,Val,T)),
   asserta(assigned(Fl,Val,T)),
   fail.
prove_all_assigns(_).

% view_all(T) lets us print out all of the "view" variables for time T
% this lets us monitor the simulation.
% view(G,T,P) is true if the elements of list P should be printed when G is proved at time T
view_all(T) :-
   view(G,T,P),
   prove(G),
   writeln(P),
   fail.
view_all(_).

% writeln(L) is true if list L is written on a line
writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).
