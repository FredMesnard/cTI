:- module(nat_bool,[nat_bool/2]).
	
:- use_module(library(clpq)).
:- use_module(num_op,[]).
:- use_module(utils).
:- use_module(predef).

:- dynamic('$copy'/2).

nat_bool(SccsClsN,SccsClsB) :-
	%trace,
	nat_bool0(SccsClsN,SccsClsB).

nat_bool0([],[]).
nat_bool0([Ps|Pss],[Pbs|Pbss]) :-
	nat_bool1(Ps,Pbs),
	nat_bool0(Pss,Pbss).

nat_bool1([],[]).
nat_bool1([P/N-Rs|LPRs],[P/N-R2s|LPRs2]) :-
	nat_bool2(Rs,R2s),
	nat_bool1(LPRs,LPRs2).
	
nat_bool2([],[]).
nat_bool2([Ref|Rs],[Ref2|R2s]) :-
	clause_head(Ref,Head),
	clause_body(Ref,Bs),
	nat_bool3(Bs,_Bs2,Bs3),
	build_clause(Head,Bs3,Ref2),
	nat_bool2(Rs,R2s).

nat_bool3([],_,[]).
nat_bool3(['$constraint'(Cs)|_Bs],_,['$constraint'(0)]) :-
    \+ (num_op:satisfiable(Cs)), !.
nat_bool3(['$constraint'(Cs)|Bs],['$constraint'(_Cs2)|Bs2],['$constraint'(Cbs)|Es]) :-
	%FRED num_op:tell_cs(Cs2), pour pdt crois'e
	!,nb_eqs(Cs,Cbs),
	%bool_op:satisfiable(Ds),
	%bool_op:project(Vars,Ds,Vars,Cbs),
	nat_bool3(Bs,Bs2,Es).
%nat_bool3(['$constraint'(_Cs)|_Bs],_,['$constraint'(0)]) :-
%	!. % As et Cs insatisfiables
nat_bool3(['$predef'(A)|Bs],[_|Bs2],['$predef'(A),'$constraint'(Ma)|Ds]) :-
	!,predef_nat_bool_tc(A,_,Ma,_),nat_bool3(Bs,Bs2,Ds).
nat_bool3([B|Bs],[_|Bs2],[B|Ds]) :-
	nat_bool3(Bs,Bs2,Ds).

nb_eqs([],1).
nb_eqs([E|Eqs],C*Cs) :-
	(eqnum_termbool(E,C) -> true;throw(cti_exception(bool_nat_bool,E))),
	%write_list([E-C]),
	nb_eqs(Eqs,Cs).

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
nexp(-(X),LR,VC0,VC) :- var(X),!,add_vc(VC0,X,LR,-1,VC).
nexp(N,_LR,VC,VC) :- number(N),!.
nexp(N*X,LR,VC0,VC) :- number(N),var(X),!,add_vc(VC0,X,LR,N,VC).
nexp(E1+E2,LR,VC0,VC) :- nexp(E1,LR,VC0,VC1),nexp(E2,LR,VC1,VC).
nexp(E1-E2,LR,VC0,VC) :- nexp(E1,LR,VC0,VC1),nexp(E2,-LR,VC1,VC).

dispatch([],1,1).
dispatch([X-Cx|Vc],X*G,D) :- {Cx>0},!,dispatch(Vc,G,D).
dispatch([_X-Cx|Vc],G,D)  :- {Cx=0},!,dispatch(Vc,G,D).
dispatch([X-Cx|Vc],G,X*D) :- {Cx<0},!,dispatch(Vc,G,D).

%% trad(nat,bool) : trad(=, =:=)	trad(>=,=<)	trad(=<,>=)

/*
%%
%% 		E+ >= rat(N,D)	E+ commence par un monome positif (une var) 
%%				et ne contient pas de cste
%%
%% 		E+ =< rat(N,D)	E commence par un monome positif (une var)
%%				et ne contient pas de cste
%%
%% 		X = rat(N,D)	
%%	
%% 		X = rat(N,D)+E	o'u X n'apparait pas dans E, 
%%				E ne contient pas de cste, N>0 ou N<0
%%
%%		X = E		E ne contient pas de cste (evt E var)	

nb_eq0(E,C) :-
	term_variables(E,V),
	(num_op:project(V,[E],V,[Ep]) ->
	    nb_eq(Ep,C)
	;
	    C=0).
	%(taut(C,1) -> Cb=1; Cb=C),
	%write(E-Ep-C),nl.


	
nb_eq(X = Y,X =:= Y) :-
	var(X),var(Y),!.
nb_eq(X = rat(_,_),X =:= 1) :-
	var(X),!.
nb_eq(X = Rat+E,(X*Em1) =:= Ep1) :-
	var(X),Rat==rat(_,_),!,
	sep_p_m(E,Ep,Em),
	((ground(Ep),Ep =:= 0) -> Ep1=1 ; Ep1=Ep),
	((ground(Em),Em =:= 0) -> Em1=1 ; Em1=Em).
nb_eq(X = E,(X*Em1) =:= Ep1) :-
	var(X),!,
	sep_p_m(E,Ep,Em),
	((ground(Ep),Ep =:= 0) -> Ep1=1 ; Ep1=Ep),
	((ground(Em),Em =:= 0) -> Em1=1 ; Em1=Em).	
nb_eq(E >= rat(_,_),Ep1 =< Em1) :-
	!,sep_p_m(E,Ep,Em),
	((ground(Ep),Ep =:= 0) -> Ep1=1 ; Ep1=Ep),
	((ground(Em),Em =:= 0) -> Em1=1 ; Em1=Em).
nb_eq(E =< rat(_,_),Ep1 >= Em1) :-
	!,sep_p_m(E,Ep,Em),
	((ground(Ep),Ep =:= 0) -> Ep1=1 ; Ep1=Ep),
	((ground(Em),Em =:= 0) -> Em1=1 ; Em1=Em).
nb_eq(rat(0,1)=rat(1,1),0 =:= 1) :-!.
nb_eq(E,_) :-throw(cti_exception(bool_nat_bool,nb_eq(E))).
	
sep_p_m(X,X,1) :-var(X),!.
sep_p_m(-(X),1,X) :-var(X),!.
sep_p_m(rat(N,_)*X,X,1) :-N>0,var(X),!.
sep_p_m(rat(N,_)*X,1,X) :-N<0,var(X),!.
sep_p_m(X+E,X*Ep,En) :-var(X),!,sep_p_m(E,Ep,En).
sep_p_m(X-E,X*En,Ep) :-var(X),!,sep_p_m(E,Ep,En).
sep_p_m(rat(_,_)+E,Ep,En) :-!,sep_p_m(E,Ep,En).
sep_p_m(rat(_,_)-E,En,Ep) :-!,sep_p_m(E,Ep,En).
sep_p_m(E1+E2,Ep1*Ep2,En1*En2) :-!,sep_p_m(E1,Ep1,En1),sep_p_m(E2,Ep2,En2).
sep_p_m(E1-E2,Ep1*En2,En1*Ep2) :-!,sep_p_m(E1,Ep1,En1),sep_p_m(E2,Ep2,En2).
%sep_p_m(I,1,0) :-number(I),!,I>=0.
sep_p_m(E,_,_) :-throw(cti_exception(bool_nat_bool,sep_p_m(E,_,_))).
*/











