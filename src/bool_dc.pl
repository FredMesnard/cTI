:- module(dc,[dc/3]).
		
:- use_module(library(clpb)).
:- use_module(bool_op,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  dc(Vars, Tb, Tb_Dc)
%%    Tb_Dc est la "+ grande"  disj de conj de vars  =>  Tb
%%    ie    est la partie >=0

dc(_,Tb,1) :- \+ (\+ taut(Tb =:= 1,1)), !.
dc(_,Tb,0) :- \+ (\+ taut(Tb =:= 0,1)), !.
dc([V|Vars],Tb,Tb_Dc) :-
	length([V|Vars],L),
    generer(1,L,[V|Vars],Tb,0,Tb_Dc).
	
generer(I,N,Vars,Tb,D1s,D2s) :-
	I =< N, !, J is I+1,
	findall(Vars-S,(length(L,I),sous_liste(L,Vars),pdt(L,S),\+ (\+ taut((S =< Tb),1))),Disjs),
	filtrer(Disjs,Vars,D1s,D3s),
	generer(J,N,Vars,Tb,D3s,D2s).
generer(I,N,_,_,Ds,Ds):-I is N+1.

pdt([X],X) :- !.
pdt([X|Xs],X*S) :- pdt(Xs,S).

filtrer([],_Vars,D,D).
filtrer([Vars-D|Ds],Vars,Doks,Dnvx) :-
	(   bool_op:satisfiable(Doks) 
    ->
	    (   \+ ( \+ taut((D =< Doks),1)) 
        ->
		    filtrer(Ds,Vars,Doks,Dnvx)
	    ;
		    filtrer(Ds,Vars,Doks+D,Dnvx)
        )
	;
	    filtrer(Ds,Vars,D,Dnvx)
    ).
	    
sous_liste([],[]).
sous_liste(Xs,[_|Ys]) :- sous_liste(Xs,Ys).
sous_liste([X|Xs],[X|Ys]) :- sous_liste(Xs,Ys).



