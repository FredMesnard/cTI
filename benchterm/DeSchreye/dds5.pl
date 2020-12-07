%:- import(prolog_types).
%:- import(prolog_arith).

%:- pred merge(list(num),list(num),list(num)).
merge([],X,X).
merge(X,[],X).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y,merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- Y<X,merge([X|Xs],Ys,Zs).
