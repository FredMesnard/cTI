% Structure of the graph
parents(report,[leaving]).
parents(leaving,[alarm]).
parents(alarm,[tampering,fire]).
parents(smoke,[fire]).
parents(tampering,[]).
parents(fire,[]).

% values for variables
values(report,[yes,no]).
values(leaving,[yes,no]).
values(alarm,[yes,no]).
values(tampering,[yes,no]).
values(fire,[yes,no]).
values(smoke,[yes,no]).

% conditional probabilities
pr(report,[leaving=yes],[0.75,0.25]).
pr(report,[leaving=no],[0.01,0.99]).

pr(leaving,[alarm=yes],[0.88,0.12]).
pr(leaving,[alarm=no],[0.001,0.999]).

pr(alarm,[tampering=yes,fire=yes],[0.5,0.5]).
pr(alarm,[tampering=yes,fire=no], [0.85,0.15]).
pr(alarm,[tampering=no, fire=yes],[0.99,0.01]).
pr(alarm,[tampering=no, fire=no], [0.0001,0.9999]).

pr(smoke,[fire=yes],[0.9,0.1]).
pr(smoke,[fire=no], [0.01,0.99]).

pr(tampering,[],[0.02,0.98]).

pr(fire,[],[0.01,0.99]).
