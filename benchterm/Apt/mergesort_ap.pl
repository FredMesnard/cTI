% this is a version of mergesort that appears in
% K. A. Apt and D. Pedreschi, Modular Termination Proofs for Logic and Pure
% Prolog Programs, Dipartimento di Informatica, Universita di Pisa, 1993
% mergesort(Xs,Ys,Xs) if Ys is an ordered permutation of the list Xs

mergesort([],[],Ls).
mergesort([X],[X],Ls).
mergesort([X,Y|Xs],Ys,[H|Ls]) :- split([X,Y|Xs],X1s,X2s,[H|Ls]),
              mergesort(X1s,Y1s,Ls), mergesort(X2s,Y2s,Ls), 
              merge(Y1s,Y2s,Ys,[H|Ls]).

split([],[],[],Ls).
split([X|Xs],[X|Ys],Zs,[H|Ls]) :- split(Xs,Zs,Ys,Ls).

merge([],Xs,Xs,Ls).
merge(Xs,[],Xs,Ls).
merge([X|Xs],[Y|Ys],[X|Zs],[H|Ls]) :- X=<Y, merge(Xs,[Y|Ys],Zs,Ls).
merge([X|Xs],[Y|Ys],[Y|Zs],[H|Ls]) :- X>Y, merge([X|Xs],Ys,Zs,Ls).
