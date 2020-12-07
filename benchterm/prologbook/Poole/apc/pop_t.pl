% Computational Intelligence: a logical approach. 
% Prolog Code.
% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION (Section C.5, page 517)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% ACTIONS
% move(Ag,Pos,Pos_1) is the action: Ag moves from Pos to Pos_1

poss(move(Ag,Pos,Pos_1)) <-
   autonomous(Ag) &
   adjacent(Pos,Pos_1) &
   sitting_at(Ag,Pos).
achieves(move(Ag,_,Pos_1),sitting_at(Ag,Pos_1)).
deletes(move(Ag,Pos,_),sitting_at(Ag,Pos)).

% pickup(Ag,Obj,Pos) is the action: agent Ag picks up Obj
% when they are both at position Pos.
poss(pickup(Ag,Obj,Pos)) <-
   autonomous(Ag) &
   Ag \= Obj &
   sitting_at(Obj,Pos) &
   at(Ag,Pos).
achieves(pickup(Ag,Obj,Pos), carrying(Ag,Obj)).
deletes(pickup(Ag,Obj,Pos), sitting_at(Obj,Pos)).

% putdown(Ag,Obj,Pos) is the action: Ag puts down Obj at Pos
poss(putdown(Ag,Obj,Pos)) <- 
   autonomous(Ag) &
   Ag \= Obj &
   at(Ag,Pos) &
   carrying(Ag,Obj).
achieves(putdown(Ag,Obj,Pos),sitting_at(Obj,Pos)).
deletes(putdown(Ag,Obj,Pos),carrying(Ag,Obj)).

% unlock(Ag,Door) is the action: agent Ag unlocks Door
poss(unlock(Ag,Door)) <-
   autonomous(Ag) &
   blocks(Door,P_1,_) &
   opens(Key,Door) &
   carrying(Ag,Key) &
   at(Ag,P_1).
achieves(unlock(Ag,Door),unlocked(Door)).

% PRIMITIVE RELATIONS
primitive(carrying(_,_)).
primitive(sitting_at(_,_)).
primitive(unlocked(_)).

% DERIVED RELATIONS

at(Obj,Pos) <-
   sitting_at(Obj,Pos).
at(Obj,Pos) <-
   autonomous(Ag) &
   Ag \= Obj &
   carrying(Ag,Obj) &
   at(Ag,Pos).

adjacent(o109,o103) <- true.
adjacent(o103,o109) <- true.
adjacent(o109,storage) <- true.
adjacent(storage,o109) <- true.
adjacent(o109,o111) <- true.
adjacent(o111,o109) <- true.
adjacent(o103,mail) <- true.
adjacent(mail,o103) <- true.
adjacent(lab2,o109) <- true.
adjacent(P_1,P_2) <-
   blocks(Door,P_1,P_2) &
   unlocked(Door).
blocks(door1,o103,lab2) <- true.
opens(k1,door1) <- true.
autonomous(rob) <- true.

% INITIAL SITUATION
holds(sitting_at(rob,o109),init).
holds(sitting_at(parcel,storage),init).
holds(sitting_at(k1,mail),init).

achieves(init,X) :-
   holds(X,init).
   
