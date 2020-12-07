% Computational Intelligence: a logical approach. 
% Prolog Code.
% BELIEF NETWORK for the Aching limbs network
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% list of all variables, such that parents of a node are before the node
variables([ar,te,ae,dh,ah]).

% Structure of the graph
parents(ar,[]).
parents(te,[ar]).
parents(ae,[ar,te]).
parents(dh,[]).
parents(ah,[ar,dh]).

% values for variables
values(_,[present,absent]).

% conditional probabilities
pr(ar,[],[0.001,0.999]).
pr(te,[ar=present],[0.0001,0.9999]).
pr(te,[ar=absent], [0.01,0.99]).
pr(ae,[ar=present,te=present],[0.1,0.9]).
pr(ae,[ar=present,te=absent], [0.99,0.01]).
pr(ae,[ar=absent, te=present],[0.99,0.01]).
pr(ae,[ar=absent, te=absent], [0.00001,0.99999]).
pr(dh,[],[0.01,0.99]).
pr(ah,[ar=present,dh=present],[0.1,0.9]).
pr(ah,[ar=present,dh=absent], [0.99,0.01]).
pr(ah,[ar=absent, dh=present],[0.99,0.01]).
pr(ah,[ar=absent, dh=absent], [0.00001,0.99999]).

% ? p(ar,[],P).
% ? p(ar,[ae=present],P).
% ? p(ar,[ah=present],P).
% ? p(ar,[ae=present,ah=present],P).

% ? p(te,[],P).
% ? p(te,[ae=present],P).
% ? p(te,[ah=present],P).
% ? p(te,[ae=present,ah=present],P).
