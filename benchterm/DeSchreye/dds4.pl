%:- import(prolog_types).
%:- import(prolog_arith).

%:- pred sum(list(num),list(num),list(num)).

sum([],[],[]).
sum([X1|Y1],[X2|Y2],[X3|Y3]) :- X3 is X1+X2, sum(Y1,Y2,Y3).
