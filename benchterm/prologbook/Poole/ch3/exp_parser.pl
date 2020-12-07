% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% this implements a left-associative parser for arithmetic expressions
% with * and / having precedence over + and -

% exp(T0,T2,V) is true if T1-T2 is an expression with value V
exp(T0,T2,V) :-
   term(T0,T1,VT),
   rexp(T1,T2,VT,V).

% rexp(T0,T1,V0,V) is true if an expression with value V0 combined with T0-T1
% has value V
rexp(T,T,V,V).
rexp([+|T1],T3,L,V) :-
   term(T1,T2,R),
   rexp(T2,T3,plus(L,R),V).
rexp([-|T1],T3,L,V) :-
   term(T1,T2,R),
   rexp(T2,T3,minus(L,R),V).

% term(T0,T2,V) is true if T1-T2 is a term with value V
term(T0,T2,V) :-
   factor(T0,T1,VT),
   rterm(T1,T2,VT,V).

% rterm(T0,T1,V0,V) is true if a term with value V0 combined with T0-T1
% has value V
rterm(T,T,V,V).
rterm([*|T1],T3,L,V) :-
   factor(T1,T2,R),
   rterm(T2,T3,times(L,R),V).
rterm([/|T1],T3,L,V) :-
   factor(T1,T2,R),
   rterm(T2,T3,divide(L,R),V).

% factor(T0,T2,V) is true if T1-T2 is a factor with value V
factor([X|T],T,X) :- atom(X).
factor(['('|T1],T3,V) :-
   exp(T1,[')'|T3],V).
factor([D|T0],T1,V) :-
   digit(D),
   number(T0,T1,D,V).

% number(T0,T1,V0,V1) is true if T0-T1 forms a number whose first part is V0
% and whose value is V
number(T,T,V,V).
number([D|T0],T1,L,V) :-
   digit(D),
   VT is 10*L+D,
   number(T0,T1,VT,V).

% digit(D) is true if D is a digit
digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

% Example queries
% exp([x,*,y,+,3,4],[],A).
% exp([x,+,3,2,5,1,-,a,+,1],[],A).
% exp([x,*,y,+,4,3,/,z,*,3,3,-,5,6],[],A).
% exp([x,*,'(',3,4,+,y,')',+,5,6],[],A).

