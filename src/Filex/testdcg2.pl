:- include_predef('predef_for_compatibility.pl').


expr(X) --> term(Y),[+],expr(Z),{X is Y+Z}.
expr(X) --> term(Y),[-],expr(Z),{X is Y-Z}.
expr(X) --> term(X).
term(X) --> factor(Y),[*],term(Z),{X is Y*Z}.
term(X) --> factor(Y),[/],term(Z),{X is Y/Z}.
term(X) --> factor(X).
factor(X) --> [X],{integer(X)}.
