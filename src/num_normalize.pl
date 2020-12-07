:- use_module(library(clpq)).
:- use_module(library(terms)).

eqnum_termbool(L = R,G =:= D) :-
	normalize(L,R,VC),
	dispatch(VC,G,D).
eqnum_termbool(L >= R,G =< D) :-
	normalize(L,R,VC),
	dispatch(VC,G,D).
eqnum_termbool(L =< R,G >= D) :-
	normalize(L,R,VC),
	dispatch(VC,G,D).

%% trad(nat,bool) : trad(=, =:=)	trad(>=,=<)	trad(=<,>=)
% normalize(+Exp1,+Exp2,-VarCoeffs)
normalize(E1,E2,VarCoeffs) :-
	term_variables(E1-E2,Vars),
	init_vc(Vars,VarCoeffs0),
	nexp(E1,1,VarCoeffs0,VarCoeffs1),
	nexp(E2,-1,VarCoeffs1,VarCoeffs).

init_vc([],[]).
init_vc([X|Xs],[X-0|Ys]) :- init_vc(Xs,Ys).

add_vc([X-Cx|Xs],Y,LR,N,VCs) :- X==Y,!,{Cx2 = Cx+LR*N},VCs=[X-Cx2|Xs].
add_vc([VC|Xs],Y,LR,N,[VC|Zs]) :- add_vc(Xs,Y,LR,N,Zs).

nexp(X,LR,VC0,VC) :- var(X),!,add_vc(VC0,X,LR,1,VC).
nexp(rat(_,_),_LR,VC,VC) :- !.
nexp(N,_LR,VC,VC) :- integer(N),!.
nexp(rat(N,D)*X,LR,VC0,VC) :- var(X),!,add_vc(VC0,X,LR,rat(N,D),VC).
nexp(N*X,LR,VC0,VC) :- integer(N),var(X),!,add_vc(VC0,X,LR,N,VC).
nexp(E1+E2,LR,VC0,VC) :- nexp(E1,LR,VC0,VC1),nexp(E2,LR,VC1,VC).
nexp(E1-E2,LR,VC0,VC) :- nexp(E1,LR,VC0,VC1),nexp(E2,-LR,VC1,VC).


dispatch([],1,1).
dispatch([X-Cx|Vc],X*G,D) :- {Cx>0},!,dispatch(Vc,G,D).
dispatch([_X-Cx|Vc],G,D)  :- {Cx=0},!,dispatch(Vc,G,D).
dispatch([X-Cx|Vc],G,X*D) :- {Cx<0},!,dispatch(Vc,G,D).
