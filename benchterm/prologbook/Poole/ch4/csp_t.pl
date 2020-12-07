% Here is a simple example using the CSP solver
test_csp(X,Y,Z) :-
    csp([dom(X,[1,2,3,4]),
         dom(Y,[1,2,3,4]),
         dom(Z,[1,2,3,4])],
        [rel([X,Y],X<Y),rel([Y,Z],Y<Z)]).

% | ?- test_csp(X,Y,Z).

% Here's the scheduling problem from Chapter 4:
schedule(A,B,C,D,E) :-
    csp([dom(A,[1,2,3,4]),
         dom(B,[1,2,4]),
         dom(C,[1,3,4]),
         dom(D,[1,2,3,4]),
         dom(E,[1,2,3,4])],
        [rel([A,B], A =\= B),
         rel([B,C], B =\= C),
         rel([C,D], C < D),         
         rel([B,D], B =\= D),
         rel([A,D], A =:= D),
         rel([A,E], A > E),
         rel([B,E], B > E),
         rel([C,E], C > E),
         rel([D,E], D > E)]).

    
% A crossword puzzle example (described in Chapter 4).
crossword(A1,A2,A3,D1,D2,D3) :-     
    cross(A1,A2,A3,D1,D2,D3,
          [[a,d,d], [a,d,o], [a,g,e], [a,g,o], [a,i,d],
           [a,i,l], [a,i,m], [a,i,r], [a,n,d], [a,n,y],
           [a,p,e], [a,p,t], [a,r,c], [a,r,e], [a,r,k],
           [a,r,m], [a,r,t], [a,s,h], [a,s,k], [a,u,k],
           [a,w,e], [a,w,l], [a,y,e], [b,a,d], [b,a,g],
           [b,a,n], [b,a,t], [b,e,e], [b,o,a], [e,a,r],
           [e,e,l], [e,f,t], [f,a,r], [f,a,t], [f,i,t],
           [l,e,e], [o,a,f], [r,a,t], [t,a,r], [t,i,e]
          ]).
           

cross(A1,A2,A3,D1,D2,D3,Words) :-
    csp([dom(A1,Words),
         dom(A2,Words),
         dom(A3,Words),
         dom(D1,Words),
         dom(D2,Words),
         dom(D3,Words)],
        [rel([A1,D1],compatible(A1,1,D1,1)),
         rel([A1,D2],compatible(A1,2,D2,1)),
         rel([A1,D3],compatible(A1,3,D3,1)),
         rel([A2,D1],compatible(A2,1,D1,2)),
         rel([A2,D2],compatible(A2,2,D2,2)),
         rel([A2,D3],compatible(A2,3,D3,2)),
         rel([A3,D1],compatible(A3,1,D1,3)),
         rel([A3,D2],compatible(A3,2,D2,3)),
         rel([A3,D3],compatible(A3,3,D3,3))]).

compatible(Word1,I1,Word2,I2) :- 
    common_letter(Word1,I1,Word2,I2).

common_letter(Word1,I1,Word2,I2) :-
    letter(Word1,I1,Letter),
    letter(Word2,I2,Letter).

letter(Word,I,Letter) :- nth(I,Word,Letter).

% | ?- crossword(A,B,C,D,E,F).
