% Computational Intelligence: a logical approach. 
% Prolog Code.
% A simple example using the CSP solver (Section C.3, page 510)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

test_csp(X,Y,Z) :-
    csp([dom(X,[1,2,3,4]),
         dom(Y,[1,2,3,4]),
         dom(Z,[1,2,3,4])],
        [rel([X,Y],X<Y),rel([Y,Z],Y<Z)]).

% | ?- test_csp(X,Y,Z).
