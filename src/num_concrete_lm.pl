:-module(concrete_lm,[concrete_lm/3]).

:-use_module(library(clpb)).
:-use_module(library(clpq)).
:-use_module(library(lists),[append/3,member/2]).
:-use_module(library(terms)).
%:-use_module(library(timeout)).
:- use_module(compat_swi,[cti_time_out/3]).

:-use_module(num_op,[]).
:-use_module(utils).

/*
Prg:= p(X) :- q(X). q(s(X)) :- p(X). r(s(X)):- r(X).
CLM:=[[r/1-[A,B]]-[A=0,B>=1],[p/1-[A,B],q/1-[C,D]]-[A-C>=1,B=D,A-C-D=< -1,C>=0]]

LMs:=[[p/2,q/2]-[[p/2-[1,0,2],q/2-[0,0,2]],[p/2-[0,2,0],q/2-[1,2,0]]],...]

CLM:=[[s/2-[A,B,C]]-[A=0,B+C>=1]]
LMs:=[[s/2]-[[s/2-[0,1,0]],[s/2-[0,0,1]]],...]
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
concrete_lm(CLM,LM,T0) :-
	%trace,
	concrete_lm2(CLM,LM,T0).

concrete_lm2([],[],_).
concrete_lm2([SccV-CLM|CLMs],[Scc-LM|LMs],T0) :-
	concrete_lm2_aux(CLM,SccV,Scc,LM,T0),
	concrete_lm2(CLMs,LMs,T0).

concrete_lm2_aux(false,SccV,Scc,false,_) :-
	!,sccv_scc_varexcst(SccV,Scc,_).
concrete_lm2_aux(CLM,SccV,Scc,LMs,T0) :-
	sccv_scc_varexcst(SccV,Scc,Vars),
	cti_time_out(concrete_lm:concrete_lm3(Scc,SccV,CLM,Vars,LMs),
		 T0,Res),
	(Res=time_out ->
	    write('% concrete level mapping: '),
	    write(Scc),write(' time_out! '),nl,flush_output,
	    LMs=false
	;
	    true).

sccv_scc_varexcst([],[],[]).
sccv_scc_varexcst([P/N-[_|Varp]|SccVs],[P/N|Sccs],Vars):-
	append(Varp,Vars1,Vars),
	sccv_scc_varexcst(SccVs,Sccs,Vars1).

%%%%%%%%%%%%%%%%%%%%%%%%
%% cycle unitaire : ok
concrete_lm3([_],SccV,Cons,Vars,SccLMs):-
	!,  % [P/N-GLM|...] = SccV
	generer(Vars,Cons,SccV,SccLMs). 
concrete_lm3(Ps,SccV,Cons,Vars,SccLMs):-
	Ps=[_,_|_],
	gm4(Ps,SccV,Cons,Vars,Mess),
	% assert(Mod:'$mes_max'(Ps,Mess))  pb : pas certain que c'est correct, d'ou la verif:
	verif_mes(Mess,Ps,SccV,Cons,MessOk),
	% MessOk=[[P/N-Mesp,Q/M-Mesq,...]|...]
	(MessOk=[_|_] ->
	    (   donner_val_cte(MessOk,SccV,Cons,MessOk2),
		%write('    '),printnl(mes_max(Ps,MessOk2)),writenl(' ok'),
		SccLMs=MessOk2)
	;
	    (   %write('    '),printnl(mes_max(Ps,Mess)),writenl(' oops ... adjustment :'),
		ajust_mes(Mess,Ps,SccV,Cons,MessOk2),
		(MessOk2=[_|_] ->
		    (%writenl('ok ? '),
		     %printnl(mes_max(Ps,MessOk2)),writenl('semble ok ; '),
		     %assert(Mod:'$mes_max'(Ps,MessOk2)),
		     verif_mes(MessOk2,Ps,SccV,Cons,MessOk3),
		     (MessOk3=[_|_] ->
			 (%writenl('confirmer.'),
			  SccLMs=MessOk3)
		     ;
			 (%writenl('infirmer.'),
			  SccLMs=false)))
		;
		    (%writenl('et non !'),writenl(mes_max(Ps,false)),
		     SccLMs=false)))).

donner_val_cte([],_P_Ms,_Cons,[]).
donner_val_cte([Mess|Messs],P_Ms,Cons,[Messc|Messs2]):-
	copy_term((Mess,P_Ms,Cons),(Messc,Messc,Consc)),
	term_variables(Messc,Vars),
	num_op:tell_cs(Consc),
	(donner_val2(Vars) ->
	    true
	;
	    true),
	    %write('Pb valeur pour cste : '),printnl(Mess-Cons))),
	donner_val_cte(Messs,P_Ms,Cons,Messs2).
	
	
gm4([],_,_,_,[]).
gm4([P/N|Ps],P_Ms,Cons,_,MM):-
	member(P/N-Vp,P_Ms),!,
	%num_op:project(Vp,Cons,Vp,Consp),
	Vp=[Cte|V],
	num_op:project(V,Cons,V,Consp),
	generer(0,N,V,Consp,[P/N-Vp],[],Mesp),
	reintroduction(Mesp,Cte,Mesp2),
	gm4(Ps,P_Ms,Cons,_,MM2),
	croiser(Mesp2,MM2,MM).

reintroduction([],_,[]).
reintroduction([[P/N-[_|Coeff]]|Mes],C,[[P/N-[C|Coeff]]|Mes2]):-
	reintroduction(Mes,C,Mes2).

croiser(MM,[],MM):-!.
croiser([],_MM,[]).
croiser([[P/N-Mes]|Mesp],MM2,MM5 ):-
	mdevant(MM2,P/N-Mes,MM3),
	croiser(Mesp,MM2,MM4),
	append(MM4,MM3,MM5).

mdevant([A],P/N-Mes,[[P/N-Mes|A]]):-!.
mdevant([A|As],P/N-Mes,[[P/N-Mes|A]|MM]):-
	mdevant(As,P/N-Mes,MM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     [P/N-Mes|Ps] = P_Ms
%%     mes([p/2-[A,B,C],q/2-[D,E,F]],[A-D-F=< -1,E>=0,B=E,D>=0,A-D+E>=1,A>=0,C=F,F>=0])
%%     mes_max([p/2,q/2],[[p/2-[1,0,2],q/2-[0,0,2]],[p/2-[0,2,0],q/2-[1,2,0]]])

%%     verif_mes(Mess,Ps,P_Ms,Cons)
verif_mes([],_Ps,_P_Ms,_Cons,[]).
verif_mes([Mes|Mess],Ps,P_Ms,Cons,[Mes|Mess2]):-
	copy_term(P_Ms-Cons,P_Msc-Consc),
	vm2(Mes,P_Msc,Consc),!,
	verif_mes(Mess,Ps,P_Ms,Cons,Mess2).
verif_mes([_|Mess],Ps,P_Ms,Cons,Mess2):-verif_mes(Mess,Ps,P_Ms,Cons,Mess2).
	
vm2([],_P_Ms,Consc):- num_op:satisfiable(Consc).
vm2([P/N-Coeffs|Mes],P_Ms,Cons):-
	member(P/N-Coeffs,P_Ms),!,
	vm2(Mes,P_Ms,Cons).

%%     ajust_mes(Mess,Ps,P_Ms,Cons)
ajust_mes([],_Ps,_P_Ms,_Cons,[]).
ajust_mes([Mes|Mess],Ps,P_Ms,Cons,[P_Mss|Mess2]):-
	copy_term(P_Ms-Cons,P_Msc-Consc),
	am2(Mes,P_Msc,Consc),!,
	simplifier(P_Msc,P_Mss),
	ajust_mes(Mess,Ps,P_Ms,Cons,Mess2).
ajust_mes([_|Mess],Ps,P_Ms,Cons,Mess2):-ajust_mes(Mess,Ps,P_Ms,Cons,Mess2).
	
am2([],_P_Ms,Consc):- minimiser(Consc).
am2([P/N-[_Cte|Coeffs]|Mes],P_Ms,Cons):-
	multiplier(Coeffs,_K1,Coeffs2),member(P/N-[_K2|Coeffs2],P_Ms),!,am2(Mes,P_Ms,Cons).

multiplier([],_,[]).
multiplier([Coeff|Coeffs],K,[K*Coeff|KCoeffs]):-multiplier(Coeffs,K,KCoeffs).

minimiser(Consc):-
	term_variables(Consc,Vars),tous_pos(Vars,Ds),append(Ds,Consc,Es),post(Es),min(Vars).

post([]).
post([C|Cs]):-{C},post(Cs).

min([]).
min([K|Ks]):-minimize(K),min(Ks).

simplifier([],[]).
simplifier([P/N-Coeffs|Mes],[P/N-Coeffss|Mess]):- simp2(Coeffs,Coeffss),simplifier(Mes,Mess).

simp2([],[]).
simp2([C|Cs],[D|Ds]):-{D=C},simp2(Cs,Ds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generer(Vars,Cons,SccGLM,SccLMs) :-
	length(Vars,N),
	generer(0,N,Vars,Cons,SccGLM,[],SccLMs).

generer(I,N,Vars,Cons,P_Ms,M1s,M2s):-
	I =< N, !, J is I+1,
	findall(Vars,(sat(card([I],Vars)),labeling(Vars)),M_Bool_Pots),
	recup_coeff_arg(M1s,Moks),
	filtrer(M_Bool_Pots,Moks,M3s),
	instancier(M3s,Vars,Cons,P_Ms,M4s),
	append(M4s,M1s,M5s),
	generer(J,N,Vars,Cons,P_Ms,M5s,M2s).
generer(I,N,_,_,_,Ms,Ms):-
	I is N+1.
	
recup_coeff_arg([],[]).
recup_coeff_arg([P_Ms|Ps],[V1|Moks]):-
	rca(P_Ms,V1),
	recup_coeff_arg(Ps,Moks).

rca([],[]).
rca([_-[_|Coeffs]|Ps],C2):-
	append(Coeffs,C1,C2),
	rca(Ps,C1).
	
filtrer([],_,[]).
filtrer([M|Ms],Moks,Mtests):-
	couvrent(Moks,M), !,
	filtrer(Ms,Moks,Mtests).
filtrer([M|Ms],Moks,[M|Mtests]):-
	filtrer(Ms,Moks,Mtests).

couvrent([M|_],Mpot):-couvre(M,Mpot),!.
couvrent([_|Ms],Mpot):-couvrent(Ms,Mpot).

couvre([X|Xs],[0|Ys]):- !,{X=0},couvre(Xs,Ys).
couvre([_|Xs],[_|Ys]):- couvre(Xs,Ys).
couvre([],[]).

instancier([],_,_,_,[]).
instancier([M|Ms],V,C,P_Ms,[P_Ms_copy|Moks]):-
	copy_term(cp(V,P_Ms,C),cp(V_copy,P_Ms_copy,Cons)),
	num_op:tell_cs(Cons),
	poser_constr(M,V_copy),
	donner_val(P_Ms_copy),!,
	instancier(Ms,V,C,P_Ms,Moks).
instancier([_|Ms],V,C,P_Ms,Moks):-
	instancier(Ms,V,C,P_Ms,Moks).
	
poser_constr([],[]).
poser_constr([0|Xs],[Y|Ys]):-{Y=0},!,poser_constr(Xs,Ys).
poser_constr([1|Xs],[Y|Ys]):-{Y>=1},poser_constr(Xs,Ys).
poser_constr([1|Xs],[Y|Ys]):-{-1>=Y},poser_constr(Xs,Ys).

donner_val([]).
donner_val([_-Mes|P_Ms]):-
	donner_val2(Mes),
	donner_val(P_Ms).
	
donner_val2([]).
donner_val2([X|Xs]):-inf(X,X),donner_val2(Xs).
donner_val2([X|Xs]):-sup(X,X),donner_val2(Xs).
donner_val2([X|Xs]):-{X=0},donner_val2(Xs).


