% list of all variables, such that parents of a node are before the node
variables([tampering, fire, smoke, alarm, leaving, report, 
           check_smoke, see_smoke, call_brigade, utility]).

% Structure of the graph
parents(tampering,[]).
parents(fire,[]).
parents(smoke,[fire]).
parents(alarm,[tampering,fire]).
parents(leaving,[alarm]).
parents(report,[leaving]).
parents(check_smoke,[report]).
parents(see_smoke,[smoke,check_smoke]).
parents(call_brigade,[report, check_smoke, see_smoke]).
parents(utility,[fire, check_smoke, call_brigade]).

% values for variables
values(tampering,[yes,no]).
values(fire,[yes,no]).
values(smoke,[yes,no]).
values(alarm,[yes,no]).
values(leaving,[yes,no]).
values(report,[yes,no]).
values(check_smoke,[yes,no]).
values(see_smoke,[yes,no]).
values(call_brigade,[yes,no]).
values(utility,[min,max]).
     % every utility is equivalent to a lottery
     % between the min and max utilities

% conditional probabilities
pr(tampering,[],[0.02,0.98]).

pr(fire,[],[0.01,0.99]).

pr(smoke,[fire=yes],[0.9,0.1]).
pr(smoke,[fire=no], [0.01,0.99]).

pr(alarm,[tampering=yes,fire=yes],[0.5,0.5]).
pr(alarm,[tampering=yes,fire=no], [0.85,0.15]).
pr(alarm,[tampering=no, fire=yes],[0.99,0.01]).
pr(alarm,[tampering=no, fire=no], [0.0001,0.9999]).

pr(leaving,[alarm=yes],[0.88,0.12]).
pr(leaving,[alarm=no],[0.001,0.999]).

pr(report,[leaving=yes],[0.75,0.25]).
pr(report,[leaving=no],[0.01,0.99]).

pr(see_smoke,[smoke=yes,check_smoke=yes],[1,0]).
pr(see_smoke,[smoke=yes,check_smoke=no ],[0,1]).
pr(see_smoke,[smoke=no, check_smoke=yes],[0,1]).
pr(see_smoke,[smoke=no, check_smoke=no ],[0,1]).

%  utility = 1+value/5010.
pr(utility,[fire=yes,check_smoke=yes,call_brigade=yes],[0.958,0.042]). % -210
pr(utility,[fire=yes,check_smoke=yes,call_brigade=no ],[0.0  ,1.0  ]). % -5010
pr(utility,[fire=yes,check_smoke=no, call_brigade=yes],[0.960,0.040]). % -200
pr(utility,[fire=yes,check_smoke=no, call_brigade=no ],[0.002,0.998]). % -5000
pr(utility,[fire=no, check_smoke=yes,call_brigade=yes],[0.958,0.042]). % -210
pr(utility,[fire=no, check_smoke=yes,call_brigade=no ],[0.998,0.002]). % -10
pr(utility,[fire=no, check_smoke=no, call_brigade=yes],[0.960,0.040]). % -200
pr(utility,[fire=no, check_smoke=no, call_brigade=no ],[1.0  ,0.0  ]). % 0

% The following represent one particular strategy:
pr(check_smoke,[report=yes],[1,0]).
pr(check_smoke,[report=no ],[0,1]).

pr(call_brigade,[report=yes, check_smoke=yes, see_smoke=yes],[1,0]).
pr(call_brigade,[report=yes, check_smoke=yes, see_smoke=no],[0,1]).
pr(call_brigade,[report=yes, check_smoke=no, see_smoke=yes],[1,0]).
pr(call_brigade,[report=yes, check_smoke=no, see_smoke=no],[0,1]).
pr(call_brigade,[report=no, check_smoke=yes, see_smoke=yes],[0,1]).
pr(call_brigade,[report=no, check_smoke=yes, see_smoke=no],[0,1]).
pr(call_brigade,[report=no, check_smoke=no, see_smoke=yes],[0,1]).
pr(call_brigade,[report=no, check_smoke=no, see_smoke=no],[0,1]).


% ? p(utility,[],[U,_]), Value is (U-1)*5010.
% ? p(utility,[leaving=yes],[U,_]), Value is (U-1)*5010.
% ? p(utility,[tampering=yes],[U,_]), Value is (U-1)*5010.
% ? p(utility,[tampering=no],[U,_]), Value is (U-1)*5010.     % EXPLAIN!
% ? p(utility,[tampering=yes,leaving=yes],[U,_]), Value is (U-1)*5010.
% ? p(call_brigade,[],P).
% ? p(call_brigade,[fire=yes],P).
% ? p(call_brigade,[leaving=yes],P).
% ? p(call_brigade,[report=yes],P).
% ? p(call_brigade,[smoke=yes],P).
