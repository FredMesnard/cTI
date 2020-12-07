/*
:- import(prolog_types).
:- import(prolog_arith).

:- pred sum(list(num),list(num),list(num)).
*/

sum3([],Ys,Ys).
sum3([a|Xs],[],[a|Xs]).
sum3([a|Y1],[a|Y2],[a|Y3]) :-  sum3(Y1,Y2,Y3).

%sum41(As,Bs,Cs,Ds):-sum3(As,Bs,Es),sum3(Es,Cs,Ds).

%sum42(As,Bs,Cs,Ds):-sum3(Es,Cs,Ds),sum3(As,Bs,Es).
