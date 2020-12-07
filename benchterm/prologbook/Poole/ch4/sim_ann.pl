% Authors: David Poole and Alan Mackworth
% A CSP solver using Simulated Annealing. 
% Last Modified: 7/June/96
                                    
% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
% Domains represented as list of 
% [dom(V,[c1,...,cn]),...]
% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r
csp(Doms,Relns) :-
   sim_ann(Doms,Relns).

% gsat(Doms,Relns,N) is true if we can
% find a value for each variable that satisfies
% Relns using GSAT. N is the maxumum number of restarts.
sim_ann(Doms,Relns,_) :-
   random_assign(Doms,Vals),
   initial_temp(T0),
   improve(Vals,T0).

random_assign([],[]).
random_assign([dom(X,D)|Ds],[val(X,E)|Vs]) :-
   random_elt(E,D),
   random_assign(Ds,Vs).

improve(N,T) :-
   select_neighbor(N,NN),
   rand(R),
   prob_select(N,NN,R,T).

prob_select(N,NN,R,T) :-
   R =< exp((h(NN)-h(N))/T),
   update_temp(T,T1),
   improve(NN,T1).

prob_select(N,NN,R,T) :-
   R > exp((h(NN)-h(N))/T),
   update_temp(T,T1),
   improve(N,T1).


   