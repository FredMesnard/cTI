
% app3(Xs, Ys, Zs, Us) iff Us is the result of concatentating the lists Xs, Ys and Zs
app3(Xs,Ys,Zs,Us) :- app(Xs,Ys,Vs), app(Vs,Zs,Us).

% idem but note that the atoms in the body have been swapped
app3bis(Xs,Ys,Zs,Us) :- app(Vs,Zs,Us), app(Xs,Ys,Vs).

% app(Xs,Ys,Zs) iff Zs is the concatenation of Xs and Ys 
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).
	
% rev(Xs,Ys) iff Ys is Xs in the reverse order
rev([],[]).
rev([X|Xs],Zs) :- rev(Xs,Ys), app(Ys,[X],Zs).


/*
% ./cTI Filex/testapp3.pl    
...
predicate_term_condition(rev(A,B),A).
predicate_term_condition(app(A,B,C),C+A).
predicate_term_condition(app3(A,B,C,D),A*D+A*B).
predicate_term_condition(app3bis(A,B,C,D),D).
*/
