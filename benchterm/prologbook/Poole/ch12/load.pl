% :- multifile '<-' .
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
prove((~ G)) :-!,
   when(ground(G),\+ prove(G)).
prove((A ~= B)) :-!,
   dif(A,B).
prove((A < B)) :- !,
   when(ground((A<B)),(A<B)).
prove((A =< B)) :- !,
   when(ground((A =< B)),(A =< B)).
prove((A > B)) :- !,
   when(ground((A > B)),(A > B)).
prove((A >= B)) :- !,
   when(ground((A >= B)),(A >= B)).
prove((A is E)) :- !,
   when(ground(E),(A is E)).
prove(H) :-
   (H <- B),
   prove(B).

%when(A,B) :- call(B).
%dif(A, B) :- \+ (A = B).

% prove_all_assigns(T) is true if all assignments of values to variables
% are proved and remebered for time T
prove_all_assigns(T) :-
   prove(assign(Fl,Val,T)),
   asserta(assigned(Fl,Val,T)),
%   writeln(['Assigned ',Fl,' = ',Val,' at ',T]),
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
% ROBOT CONTROLLER for layer
%
%                 arrived         goto
%                    ^              v
%                    |              |
%                    ^              v
%                 ------------------------
%                 |                      |
%                 |                      |
%   goal_pos ---> |                      | ---> goal_pos
%                 |                      |
%                 |                      |
%                 ------------------------
%                    ^              v
%                    |              |
%                    ^              v
%             current_pos         steer
%             compass
%             front_sensor
%             right_sensor

% :- multifile <- .

% INPUT commands
% goto(L,T) is true if the robot should go to location L at time T

% OUTPUT percepts
% arrived(T) is true if the robot has arrived at location L at time T

% INPUT percepts:
% curent_pos(C,T) is true is the robot is at coordinates C at time T
% compass(D,T) the robot is pointing in direction D at time T
% front_sensor(on,T) the front sensor of the robot is on at time T
% right_sensor(on,T) the right sensor of the robot is on at time T

% OUTPUT commands:
% steer(D,T) means steer Dwards at time T. D in {left,right,straight}.

% The following declaration declares what it is that we view during the
% simulation.
view(val(robot_pos,(X,Y),T),T,['X=',X,' Y=',Y,' at T=',T]).
view(arrived(T),T,['arrived at',T]).

%===================================================================

% Local Goals
assign(goal_pos,Coords,T) <-
   goto(Loc,T) &
   at(Loc,Coords).

goto(goal_1,0) <- true.
at(goal_1,(50,10)) <- true.

% announcing completion
arrived(T) <-
   val(goal_pos,Coords,T) &
   val(robot_pos,CCoords,T) &
   close_enough(Coords,CCoords).

% close_enough(C0,C1) is true if coordinates C0 are close enough to C1
close_enough((X0,Y0),(X1,Y1)) <-
   sqrt((X1-X0)*(X1-X0)+(Y1-Y0)*(Y1-Y0)) < 3.0 .

% OUTPUT:
% steer(D,T) means steer Dwards at time T. D in {left,right,straight}.
%    Here is a bang-bang controller to steer towards the right direction

steer(left,T) <-
   goal_is(left,T).
steer(left,T) <-
   goal_is(straight,T) &
   fronsensor(on,T).
steer(left,T) <-
   goal_is(right,T) &
   right_sensor(on,T) &
   front_sensor(on,T).
steer(straight,T) <-
   goal_is(straight,T) &
   ~ fronsensor(on,T).
steer(straight,T) <-
   goal_is(right,T) &
   right_sensor(on,T) &
   ~ front_sensor(on,T).
steer(right,T) <-
   goal_is(right,T) &
   ~right_sensor(on,T).

goal_is(left,T) <-
   goal_direction(G,T) &
   val(compass,C,T) &
   (integer(G-C + 180) mod 360 - 180) > 5.
goal_is(straight,T) <-
   goal_direction(G,T) &
   val(compass,C,T) &
   abs(integer(G-C + 180) mod 360 - 180) =< 5.
goal_is(right,T) <-
   goal_direction(G,T) &
   val(compass,C,T) &
   (integer(G-C + 180) mod 360 - 180) < -5.

goal_direction(G,T) <-
   robot_pos((X0,Y0),T) &
   val(goal_pos,(X1,Y1),T) &
   direction((X0,Y0),(X1,Y1),G).
direction((X0,Y0),(X1,Y1),Dir) <-
   Y0=<Y1 &
   Dir is 180 * acos((X1-X0)/sqrt((X1-X0)*(X1-X0)+(Y1-Y0)*(Y1-Y0))) / 3.141592653589793 .
direction((X0,Y0),(X1,Y1),Dir) <-
   Y0>Y1 &
   Dir is 360 - 180 * acos((X1-X0)/sqrt((X1-X0)*(X1-X0)+(Y1-Y0)*(Y1-Y0))) / 3.141592653589793.



% Continuous maze with memory 
% ENVORONMENT MODULE
%

% INPUT commands
% steer(D,T) means steer Dwards at time T. D in {left,right,straight}.



% OUTPUT percepts
% curent_pos(C,T) is true is the robot is at coordinates C at time T
% compass(D,T) the robot is pointing in direction D at time T
% frontsensor(on,T) the front sensor of the robot is on at time T
% rightsensor(on,T) the right sensor of the robot is on at time T


% :- multifile <- .

%===================================================================


assign(robot_pos,(0,10),0) <- true.
assign(compass,90,0) <- true.

robot_pos(C,T) <- val(robot_pos,C,T).


front_sensor(on,T) <-
    val(compass,D,T) &
    val(robot_pos,(X,Y),T) &
    seeblock(X,Y,D).

right_sensor(on,T) <-
    val(compass,D,T) &
    val(robot_pos,(X,Y),T) &
    seeblock(X,Y,D-80).

% compass((C+DC+360) mod 360,T+DT) <-
assign(compass,C,T) <-
   was(compass,C1,T1,T) &
   compass_deriv(DC,T1) &
   C is integer(C1+DC*(T-T1)+360) mod 360.

% if robot is steering left, DC/DT=10 (i.e., 10 degrees per second).
compass_deriv(10,T) <-
   steer(left,T).

% if robot is steering left, DC/DT=0 
compass_deriv(0,T) <-
   steer(straight,T).

% if robot is steering right, DC/DT=-10 (i.e., -10 degrees per second).
compass_deriv(-10,T) <-
   steer(right,T).

%robotpos(X+DX,Y+DY,T+DT) <-
assign(robot_pos,(X,Y),T) <-
   was(robot_pos,(X1,Y1),T1,T) &
   DT is T-T1 &
   x_deriv(DX,T1) &
   y_deriv(DY,T1) &
   X is X1+DX*DT &
   Y is Y1+DY*DT.

x_deriv(DX,T) <-
   val(compass,D,T) &
   DX is cos(D*3.14159265358979344/180).

y_deriv(DY,T) <-
   val(compass,D,T) &
   DY is sin(D*3.14159265358979344/180).

seeblock(X,Y,D) <-
   inblock(X+12*cos(D*3.14159265358979344/180),Y+12*sin(D*3.14159265358979344/180)).

inblock(X,Y) <-
   X >= 60 & X =< 70 &
   Y >= 0 & Y =< 70.
inblock(X,Y) <-
   X >= 10 & X =< 60 &
   Y >= 60 & Y =< 70.
inblock(X,Y) <-
   X >= 10 & X =< 20 &
   Y >= 40 & Y =< 60 .
inblock(X,Y) <-
   X >= 10 & X =< 40 &
   Y >= 20 & Y =< 40 .
