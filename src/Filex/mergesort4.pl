mergesort([],[]).
mergesort([x],[x]).
mergesort(Xs,Ys) :-
    Xs=[x,y|Zs],
	split(Xs,X1s,X2s),
	mergesort(X2s,Y2s).
%	mergesort(X1s,Y1s).
%	merge(Y1s,Y2s,Ys).

split([],[],[]).
split([x|Xs],[x|Ys],Zs) :- split(Xs,Zs,Ys).

%merge([],Xs,Xs).
%merge(Xs,[],Xs).
%merge([x|Xs],[y|Ys],[x|Zs]) :- x=<y, merge(Xs,[y|Ys],Zs).
%merge([x|Xs],[y|Ys],[y|Zs]) :- x>y, merge([y|Xs],Ys,Zs).


mergesort2([],[]).
mergesort2([X],[X]).
mergesort2(Xs,Ys) :-
    Xs=[X,Y|Zs],
	split2(Xs,X1s,X2s),
	mergesort2(X2s,Y2s).
%	mergesort(X1s,Y1s).
%	merge(Y1s,Y2s,Ys).

split2([],[],[]).
split2([X|Xs],[X|Ys],Zs) :- split2(Xs,Zs,Ys).
