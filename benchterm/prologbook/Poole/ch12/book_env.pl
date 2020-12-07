
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

% The following declaration declares what it is that we view during the
% simulation.
view(val(robot_pos,(X,Y),T),T,['X=',X,' Y=',Y,' at T=',T]).

:- multifile <- .

%===================================================================


assign(robot_pos,(0,10),0) <- true.

robot_pos(C,T) <- val(robot_pos,C,T).


front_sensor(on,T) <-
    compass(D,T) &
    val(robotpos,(X,Y),T) &
    seeblock(X,Y,D).

right_sensor(on,T) <-
    compass(D,T) &
    val(robot_pos,(X,Y),T) &
    seeblock(X,Y,D-80).

% compass((C+DC+360) mod 360,T+DT) <-
set(compass,C,T) <-
   was(compass,C1,T1,T) &
   compass_deriv(DC,T) &
   C = (C1+DC*(T-T1)+360) mod 360.

% if robot is steering left, DC/DT=10 (i.e., 10 degrees per second).
compass_deriv(DC,T) <-
   steer(left,T) &
   DC = 10.

% if robot is steering left, DC/DT=0 
compass_deriv(0,T) <-
   steer(straight,T).

% if robot is steering right, DC/DT=-10 (i.e., -10 degrees per second).
compass_deriv(DC,T) <-
   steer(right,T) &
   DC = -10.

%robotpos(X+DX,Y+DY,T+DT) <-
assign(robot_pos,(X,Y),T) <-
   was(robot_pos(X1,Y1),T1,T) &
   DT=T-T1 &
   x_deriv(DX,T1) &
   y_deriv(DY,T1) &
   X1 = X+DX*DT &
   Y1 = Y+DY*DT.

x_deriv(DX,T) <-
   compass(D,T) &
   DX=cos(D*3.14159265358979344/180).

y_deriv(DY,T) <-
   compass(D,T) &
   DY=sin(D*3.14159265358979344/180).

seeblock(X,Y,D) <-
   inblock(X+12*cos(D*3.14159265358979344/180),
           Y+12*sin(D*3.14159265358979344/180)).

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
