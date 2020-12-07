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

:- multifile <- .

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

%===================================================================

% Local Goals
assign(goal_pos,Coords,T) <-
   goto(Loc,T) &
   at(Loc,Coords).

goto((50,10),0) <- true.

% announcing completion
arrived(T) <-
   was(goal_pos,Coords,T) &
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

goal_is(right,T) <-
   goal_direction(G,T) &
   compass(C,T) &
   ((G-C + 180) mod 360 - 180) > 5.
goal_is(straight,T) <-
   goal_direction(G,T) &
   compass(C,T) &
   abs((G-C + 180) mod 360 - 180) =< 5.
goal_is(right,T) <-
   goal_direction(G,T) &
   compass(C,T) &
   ((G-C + 180) mod 360 - 180) < -5.

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


