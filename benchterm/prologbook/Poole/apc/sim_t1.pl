% ROBOT CONTROLLER for layer
%
%                 arrived         goal_pos
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
%               robot_pos         steer
%               compass
%               front_sensor
%               right_sensor

% INPUT commands
% goto(L,T) is true if the robot should go to location L at time T

% OUTPUT percepts
% arrived(T) is true if the robot has arrived at location L at time T

% INPUT percepts:
% current_pos(C,T) is true is the robot is at coordinates C at time T
% compass(D,T) the robot is pointing in direction D at time T
% front_sensor(on,T) the front sensor of the robot is on at time T
% right_sensor(on,T) the right sensor of the robot is on at time T

% OUTPUT commands:
% steer(D,T) means steer Dwards at time T. D in {left,right,straight}.

% The following declaration declares what it is that we view during the
% simulation.
view(val(robot_pos,(X,Y),T),T,[X,' ',Y]).
%view(val(robot_pos,(X,Y),T),T,['X=',X,' Y=',Y,' at T=',T]).
%view(val(compass,V,T),T,['  Compass: ',V,' at ',T]).
%view(steer(D,T),T,[' Steering: ',D,' at ',T]).
%view(arrived(T),T,[' *** arrived at ',T]).

%===================================================================

% announcing completion
arrived(T) <-
   was(goal_pos,Coords,_,T) &
   robot_pos(CCoords,T) &
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
   front_sensor(on,T).
steer(left,T) <-
   goal_is(right,T) &
   right_sensor(on,T) &
   front_sensor(on,T).
steer(straight,T) <-
   goal_is(straight,T) &
   ~ front_sensor(on,T).
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
   (integer(G-C + 540) mod 360 - 180) > 11.
goal_is(straight,T) <-
   goal_direction(G,T) &
   val(compass,C,T) &
   abs(integer(G-C + 540) mod 360 - 180) =< 11.
goal_is(right,T) <-
   goal_direction(G,T) &
   val(compass,C,T) &
   (integer(G-C + 540) mod 360 - 180) < -11.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROBOT CONTROLLER for higher layer
%
%                 ------------------------
%                 |                      |
%                 |                      |
%      to_do ---> |                      | ---> to_do
%                 |                      |
%                 |                      |
%                 ------------------------
%                    ^              v
%                    |              |
%                    ^              v
%                 arrived         goal_pos

% Local Goals
assign(goal_pos,Coords,T) <-
   arrived(T) &
   was(to_do,[goto(Loc)|_],_,T) &
   at(Loc,Coords).

%assign(goal_pos,stop,T) <-
%   arrived(T) &
%   was(to_do,[],_,T).

assign(to_do,R,T) <-
   arrived(T) &
   was(to_do,[_|R],_,T).

at(mail,(0,10)) <- true.
at(o103,(50,10)) <- true.
at(o109,(100,10)) <- true.
at(lng,(100,50)) <- true.

assign(to_do,[goto(o109),goto(lng),goto(o109),goto(o103)],0) <- true.
arrived(1) <- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENVIRONMENT MODULE
%

% INPUT commands
% steer(D,T) means steer Dwards at time T. D in {left,right,straight}.



% OUTPUT percepts
% current_pos(C,T) is true is the robot is at coordinates C at time T
% compass(D,T) the robot is pointing in direction D at time T
% frontsensor(on,T) the front sensor of the robot is on at time T
% rightsensor(on,T) the right sensor of the robot is on at time T


% :- multifile <- .

%===================================================================


assign(robot_pos,(0,5),0) <- true.
assign(compass,90,0) <- true.

robot_pos(C,T) <- val(robot_pos,C,T).


front_sensor(on,T) <-
    val(compass,D,T) &
    val(robot_pos,(X,Y),T) &
    (seeblock(X,Y,9,D); seeblock(X,Y,4,D); seeblock(X,3,D-10); 
     seeblock(X,3,D+10)).

right_sensor(on,T) <-
    val(compass,D,T) &
    val(robot_pos,(X,Y),T) &
    (seeblock(X,Y,3,D-90); seeblock(X,Y,6,D-60); seeblock(X,9,D-30)).

% compass((C+DC+360) mod 360,T+DT) <-
assign(compass,C,T) <-
   was(compass,C1,T1,T) &
   compass_deriv(DC,T1) &
   C is integer(C1+DC*(T-T1)+360) mod 360.

% if robot is steering left, DC/DT=10 (i.e., 10 degrees per second).
compass_deriv(18,T) <-
   steer(left,T).

% if robot is steering left, DC/DT=0 
compass_deriv(0,T) <-
   steer(straight,T).

% if robot is steering right, DC/DT=-10 (i.e., -10 degrees per second).
compass_deriv(-18,T) <-
   steer(right,T).

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

seeblock(X,Y,Dist,Dir) <-
   inblock(X+Dist*cos(Dir*3.14159265358979344/180),
           Y+Dist*sin(Dir*3.14159265358979344/180)).

inblock(X,Y) <-
   X >= 20 & X =< 35 &
   Y >= -20 & Y =< 20.
