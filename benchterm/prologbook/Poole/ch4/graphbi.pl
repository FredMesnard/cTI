% ***************************************************************
% The following defines the graph of Section 4.2.3 in the book

% neighbours(N,NN) is true if NN is the list of neighbours of node N
neighbours(o103,[o109,l2d3]).
neighbours(o109,[o103,o119]).
neighbours(o119,[o109,o123]).
neighbours(o123,[o119,r123,l3d1]).
neighbours(l2d3,[l2d1,l2d4]).
neighbours(l2d1,[l2d3,l2d2,l3d2]).
neighbours(l2d2,[l2d1,l2d4]).
neighbours(l2d4,[l2d2,l2d3,o109]).
neighbours(l3d2,[l3d3,l3d1]).
neighbours(l3d1,[l3d3,l3d2]).
neighbours(l3d3,[l3d2,l3d1,l2d2]).
neighbours(r123,[]).


% is_goal(N) is true if N is a goal node.
is_goal(r123).

% cost(N,M,C) is true if C is the arc cost for the arc from node N to node M
cost(N,M,C) :-
   position(N,NX,NY),
   position(M,MX,MY),
   C is abs(NX-MX)+abs(NY-MY).

% h(N,C) is true if C is the heuristic cost of node N
h(N,C) :-
   position(N,NX,NY),
   is_goal(G),
   position(G,GX,GY),
   C is abs(NX-GX)+abs(NY-GY).

% position(N,X,Y) is true if node X is at position (X,Y)

position(o103,31,43).
position(o109,43,43).
position(o119,42,58).
position(o123,33,58).
position(r123,33,62).
position(l2d1,32,43).
position(l2d2,39,49).
position(l2d3,34,46).
position(l2d4,39,46).
position(l3d1,34,55).
position(l3d2,33,52).
position(l3d3,39,52).
