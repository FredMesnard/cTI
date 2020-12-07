% Computational Intelligence: a logical approach. 
% Prolog Code.
% Neural-network learning algorithm (Section C.4)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% Neural-net style learning for parameterized logic
% programs.  Given a set of examples, a parameterized
% logic program and a measure of error for each
% example, this program used back-propagation to tune
% the parameters. Derivatives are estimated numerically.

% nnlearn(N,DX,LC,Exs,P0,P1)
% N is the number of iterations to do
% DX is the increment to evaluate derivatives
% LC is the learning constant for gradient descent
% Exs is a list of all of the examples
% P0 is the list of parameter settings before the learning
% P1 is the list of parameter settings after the learning
nnlearn(0,_,_,Exs,P0,P0) :-
   total_error(Exs,P0,0,Err0),
   writeln(['Error = ', Err0]).
nnlearn(N,DX,LC,Exs,P0,P2) :-
   update_parms(DX,LC,Exs,P0,P1),
   N1 is N-1,
   nnlearn(N1,DX,LC,Exs,P1,P2).

% update_parms(DX,LC,Exs,P0,P1).
% updates all of parameter in P0 to P1
update_parms(DX,LC,Exs,P0,P1) :-
   total_error(Exs,P0,0,Err0),
   writeln(['Error = ', Err0]),
   update_each(P0,P0,Err0,Exs,DX,LC,P1).

% update_each(PR,P0,Err0,Exs,DX,LC,P1)
% updates each parameter in PR.
% P0 is the initial parameter setting with error Err0
% Exs is a list of all of the examples
% DX is the increment to evaluate derivatives
% LC is the learning constant for gradient descent
% P1 is the updated parameter settings
update_each([],_,_,_,_,_,[]).
update_each([val(P,V)|RPs],P0,Err0,Exs,DX,LC,
                               [val(P,NV)|NPs]) :-
   V1 is V+DX,
   total_error(Exs,[val(P,V1)|P0],0,Nerr),
   NV is V+LC*(Err0-Nerr)/DX,
   update_each(RPs,P0,Err0,Exs,DX,LC,NPs).

% total_error(Exs,P,Err0,Err1)
% P is a list of parameter settings
% Exs is a list of examples
% Err1 is Err0 plus the sum of the errors on examples
total_error([],_,Err,Err).
total_error([Ex|Exs],P,PErr,TErr) :-
   pprove(error(Ex,Err),P),!,
   NErr is PErr+Err,
   total_error(Exs,P,NErr,TErr).


% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% pprove(G,P) means prove G with parameter settings P.
% P is a list of the value assignments of the
% form val(P,V)

pprove(true,_) :- !.
pprove((A & B),P) :-
   !,
   pprove(A,P),
   pprove(B,P).
pprove((A is E),P) :-
   !,
   eval(E,A,P).
pprove(H,P) :-
   (H <- B),
   pprove(B,P).

% eval(E,V,P) true if expression E has value V
% given parameter settings P
eval((A+B),S,P) :-
   !,
   eval(A,VA,P),
   eval(B,VB,P),
   S is VA+VB.
eval((A*B),S,P) :-
   !,
   eval(A,VA,P),
   eval(B,VB,P),
   S is VA*VB.
eval((A-B),S,P) :-
   !,
   eval(A,VA,P),
   eval(B,VB,P),
   S is VA-VB.
eval((A/B),S,P) :-
   !,
   eval(A,VA,P),
   eval(B,VB,P),
   S is VA/VB.
eval(sigmoid(E),V,P) :-
   !,
   eval(E,EV,P),
   V is 1/(1+exp(-EV)).
eval(N,N,_) :-
   number(N),!.
eval(N,V,P) :-
   member(val(N,V1),P),!,V=V1.
      % it only sees the first value for the parameter
eval(N,V,_) :-
   writeln(['Arithmetic Error: ',V,' is ',N]),fail.

