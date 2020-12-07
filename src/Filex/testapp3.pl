

% app3(Xs, Ys, Zs, Us) :- Us is the result of concatentating the lists Xs, Ys and Zs
app3(Xs, Ys, Zs, Us) :- app(Xs, Ys, Vs), app(Vs, Zs, Us).

% another possibility
app3bis(Xs, Ys, Zs, Us):- app(Vs,Zs,Us),app(Xs,Ys,Vs).

% augmented by the APPEND program
app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).

rev([],[]).
rev([X|Xs],Zs):-rev(Xs,Ys),app(Ys,[X],Zs).