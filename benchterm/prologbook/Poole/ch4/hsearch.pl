% Computational Intelligence: a logical approach. 
% Prolog Code. A graph searcher for heuristic search..
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% hsearch is like psearch, but elements of the fringe are of the form: 
% node(Node,Path,Pathcost,Nodecost)
%   where Node is the current node, Path is the path found to Node,
%   Pathcost is the cost of the path and Nodecost is the `value' of the node,
%     for which ever search strategy we are using.

% hsearch(M,F,S) is method M from fringe F results in path S to goal.
%   This works for methods in {breadth,depth,astar,best,hdepth,shortest}.

% ? hsearch(best,[node(s,[],0,0)],P).
hsearch(M,F,P) :-
   writeln(['Fringe: ',F]),
   choose(M,node(N,P,_,_),F,_),
   is_goal(N).
hsearch(M,F,S) :-
   choose(M,node(N,P,PC,_),F,F1),
   neighbours(N,NN),
   add_paths2(M,N,NN,[N|P],PC,NN2),
   hadd_to_fringe(M,NN2,F1,F2),
   hsearch(M,F2,S).

add_paths2(_,_,[],_,_,[]).
add_paths2(M,N,[NN|R],P,PC,[node(NN,P,NPC,NNC)|PR]) :-
   cost(N,NN,AC),
   NPC is PC + AC,
   value(M,NPC,NN,NNC),
   add_paths2(M,N,R,P,PC,PR).

value(astar,NPC,NN,NNC) :-
   h(NN,HNN),
   NNC is NPC+HNN.
value(best,_,NN,HNN) :-
   h(NN,HNN).
value(hdepth,_,NN,HNN) :-
   h(NN,HNN).
value(shortest,NPC,_,NPC).
value(breadth,_,_,0).
value(depth,_,_,0).


% choose(M,E,F,NF) is true if E is an element of fringe F and NF is
%   the remaining fringe after E is removed. M is the search method used.
% In each of these the fringe is the list of elements in order they
%   are to be chosen.

choose(_,N,[N|F],F).

% hadd_to_fringe(M,N,F1,F2) is true if when using search method M, when
%   nodes N are added to fringe F1, the resulting fringe is list F2.

hadd_to_fringe(depth,N,F1,F2) :- !,
   append(N,F1,F2).

hadd_to_fringe(breadth,N,F1,F2) :- !,
   append(F1,N,F2).

hadd_to_fringe(hdepth,N,F1,F2) :- !,
   mergeinto(N,[],NF),
   append(NF,F1,F2).

hadd_to_fringe(_,N,F1,F2) :-
   mergeinto(N,F1,F2).

mergeinto([],L,L).
mergeinto([H|T],L1,L3) :-
   insertinto(H,L1,L2),
   mergeinto(T,L2,L3).

insertinto(E,[],[E]).
insertinto(node(N,P,PC,NC),[node(N1,P1,PC1,NC1)|R],
              [node(N,P,PC,NC),node(N1,P1,PC1,NC1)|R]) :-
   NC =< NC1.
insertinto(node(N,P,PC,NC),[node(N1,P1,PC1,NC1)|R],
              [node(N1,P1,PC1,NC1)|R1]) :-
   NC > NC1,
   insertinto(node(N,P,PC,NC),R,R1).

% **************************************************
% Auxiliary definitions

% append(A,B,R) is true if R is the list containing the elements of A followed by the elements of B
append([],R,R).
append([H|T],L,[H|R]) :-
   append(T,L,R).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln([]) :- nl.
writeln([H|T]) :- write(H), writeln(T).
