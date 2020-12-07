mergesort([],[]).
mergesort([x],[x]).
mergesort(Xs,Ys) :-
        Xs=[x,y|Zs],
	split(Xs,X1s,X2s),
        mergesort(X1s,Y1s).
%	mergesort(X2s,Y2s),
%	merge(Y1s,Y2s,Ys).

split([],[],[]).
split([x|Xs],[x|Ys],Zs) :- split(Xs,Zs,Ys).

%merge([],Xs,Xs).
%merge(Xs,[],Xs).
%merge([x|Xs],[y|Ys],[x|Zs]) :- x=<y, merge(Xs,[y|Ys],Zs).
%merge([x|Xs],[y|Ys],[y|Zs]) :- x>y, merge([y|Xs],Ys,Zs).
