% Computational Intelligence: a logical approach. 
% Prolog Code.
% Example for META INTERPRETER WITH SEARCH (Section C.2, page 507)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% app(A,B,C) means appending A to B results in C
app([],L,L) <- true.
app([A|X],Y,[A|Z]) <- app(X,Y,Z).

% sublist(S,C) is true if list S is in the middle of list C
sublist(S,L) <- app(_,B,L) & app(S,_,B).

% Example query:
% ?proveall([goal(yes(X,Y),[sublist([X,Y],[a,b,c,d,e])])],A).
