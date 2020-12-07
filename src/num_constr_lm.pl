:- module(constr_lm,[constr_lm/3]).

:- use_module(library(lists),[member/2,append/3,reverse/2]).
:- use_module(library(terms)).

:- use_module(compat_swi,[cti_time_out/3]).
:- use_module(num_dual,[dual/5]).
:- use_module(num_cmc,[calcul_mat_col/4]).
:- use_module(utils).		

constr_lm(SccsClsNSpec,SccsLMs,TimeOut) :-
	%trace,
	constr_lm2(SccsClsNSpec,SccsLMs,TimeOut).
	
%%%
constr_lm2([],[],_TO).
constr_lm2([Classe|Classes],[LM2-ConsLM2|LMs],TO) :-
	init_lm(Classe,LM),
	catch(constr_lm3(Classe,LM,ConsLM,TO),dual_faux_sur(_H),ConsLM=false),
	homogenize(LM,ConsLM,LM2,ConsLM2),
	constr_lm2(Classes,LMs,TO).

  homogenize([P/N-Vs],Cs,[P/N-Ws],Ds) :- !,homogenize_aux(Cs,Vs,Ds,Ws).
  homogenize(LM,ConsLM,LM,ConsLM) :- LM=[_,_|_].
	   
  homogenize_aux(false,Vs,false,[_|Vs]).
  homogenize_aux([],Vs,[Cst=0],[Cst|Vs]).
  homogenize_aux([C|Cs],Vs,[Cst=0,C|Cs],[Cst|Vs]).

%%%
init_lm([P/N-_],[P/N-LMpn]) :- !,length(LMpn,N).	% if |SCC|=1 then cst=0
init_lm([PS1-_,PS2-_|PSs],PSLMs) :- init_lms([PS1-_,PS2-_|PSs],PSLMs).
	
init_lms([],[]).
init_lms([P/N-_|Ps],[P/N-LMpn|Lms]) :- J is N+1,length(LMpn,J),init_lms(Ps,Lms).

%%%
constr_lm3(Classe,LM,ConsLM,TO) :-
	cti_time_out(constr_lm:constr_lm4(Classe,LM,ConsLM),TO,Res),
	(Res=time_out ->
	    num_itp:virer_ref(Classe,PIs),
	    write('% constraint level mapping: '),
	    write(PIs),write(' time_out! '),nl,flush_output,ConsLM=false
	;
	    true).

%%%
constr_lm4(Classe,LM,ConsLM) :-
    %trace,
	get_clauses(Classe,Clauses),
	parcours_ref(Clauses,Classe,LM,ConsLM).
	
%%%
get_clauses([],[]).
get_clauses([_-Cs1|Ps],Cs3) :-
	append(Cs1,Cs2,Cs3),
	get_clauses(Ps,Cs2).

%%%
parcours_ref(Refs,Classe,Lm,ConsLm) :-
	parcours_ref(Refs,Classe,Lm,[],ConsLm).
	
parcours_ref([],_Classe,_Lm,Cs,Cs).
parcours_ref([R|Rs],Classe,Lm,C1s,Cs) :-
	clause_head(R,H),
	clause_body(R,Body),
	mut_rec(Body,Classe),!,
	% Body contient un atome de Classe
	process_cl(Body,Classe,B3),
	generer_constr(Classe,B3,H,Lm,C1s,C2s),
	parcours_ref(Rs,Classe,Lm,C2s,Cs).
parcours_ref([_|Rs],Classe,Lm,C1s,Cs):-
	% si non mut_rec, on passe aux clauses suivantes
	parcours_ref(Rs,Classe,Lm,C1s,Cs).

mut_rec([At|_],Classe) :-
	functor(At,P,N),P\=='$constraint',P\=='$predef',member(P/N-_,Classe),!.
mut_rec([_|Ats],Classe) :- mut_rec(Ats,Classe).

%%%
process_cl([],_Classe,[]).
process_cl(['$constraint'(Cons)|As],Classe,['$constraint'(Cons)|Ps]) :-
	!,process_cl(As,Classe,Ps).
process_cl(['$predef'(_)|As],Classe,Ps) :-
	!,process_cl(As,Classe,Ps).
process_cl([A|As],Classe,Ps) :-
	functor(A,Q,M),
	(member(Q/M-_,Classe) -> Ps=[A|P2s];Ps=P2s),
	process_cl(As,Classe,P2s).

%%%
generer_constr([_],B3,H,Lm,C1s,C2s) :-
	%trace,
	!,generer_constr_1(B3,[],H,Lm,C1s,C2s).
generer_constr([_,_|_],B3,H,Lm,C1s,C2s) :-
	%trace,
	generer_constr_n(B3,[],H,Lm,C1s,C2s).

% generer_constr_1
generer_constr_1([],_,_,_,Cs,Cs).
generer_constr_1(['$constraint'(Cs)|Bs],Eqs,Head,Lm,Cin,Cout) :-
	append(Cs,Eqs,Eqs2),
	num_op:satisfiable(Eqs2),!,
	generer_constr_1(Bs,Eqs2,Head,Lm,Cin,Cout).
generer_constr_1(['$constraint'(_Cs)|_],_Eqs,_Head,_Lm,Cin,Cout) :-
	!,Cout=Cin.
generer_constr_1([B|Bs],Eqs,Head,Lm,Cin,Cout) :-
	Head=..[_|V1],B=..[_|V2],
	% !!! pas de  terme constant pour les lms 
	append(V1,V2,V3),
	num_op:project(V3,Eqs,V4,C4),
	calcul_mat_col(C4,V4,Mat1,Col1),
	num_op:project(V2,Eqs,V5,C5),
	calcul_mat_col(C5,V5,Mat2,Col2),
	% a partir de Head, B et Lm, exprimer Param1 et Param2
		calcul_param1(Head,B,Lm,Param1,C1s),	% mu(H-B)
		calcul_param2(B,Lm,Param2),		% mu(B)
		dual(Param1,Mat1,Col1,1,C2s),		% mu(H-B) >= 1
		dual(Param2,Mat2,Col2,0,C3s),		% mu(B)	  >= 0
	append(C1s,C2s,C4s),
	append(C3s,C4s,C5s),
	num_op:conjunction(C5s,Cin,Cin2),
	term_variables(Lm,VarLm2),
	(num_op:project(VarLm2,Cin2,VarLm2,Cin3) ->
	    true
	;
	    raise_exception(dual_faux_sur(Head))),
	generer_constr_1(Bs,Eqs,Head,Lm,Cin3,Cout).

% generer_constr_n
generer_constr_n([],_,_,_,Cin,Cin).
generer_constr_n(['$constraint'(Cs)|Bs],Eqs,Head,Lm,Cin,Cout):-
	append(Cs,Eqs,Eqs2),
	num_op:satisfiable(Eqs2),!,
	generer_constr_n(Bs,Eqs2,Head,Lm,Cin,Cout).
generer_constr_n(['$constraint'(_Cs)|_Bs],_Eqs,_Head,_Lm,Cin,Cout):-
	!,Cout=Cin.
generer_constr_n([B|Bs],Eqs,Head,Lm,Cin,Cout):-
	Head=..[_|V1],B=..[_|V2],
	% !!! gerer terme constant des lmures : Xh=1 & Xb=1
	append([Xh|V1],[Xb|V2],V3),
	num_op:project(V3,[Xh=1,Xb=1|Eqs],V4,C4),
	calcul_mat_col(C4,V4,Mat1,Col1),
	num_op:project([Xb|V2],[Xb=1|Eqs],V5,C5),
	calcul_mat_col(C5,V5,Mat2,Col2),
	% a partir de Head, B et Lm, exprimer Param1 et Param2	
		calcul_param1(Head,B,Lm,Param1,C1s),	% mu(H-B)
		calcul_param2(B,Lm,Param2),		% mu(B)
		dual(Param1,Mat1,Col1,1,C2s),		% mu(H-B) >= 1
		dual(Param2,Mat2,Col2,0,C3s),		% mu(B)	  >= 0		
	append(C1s,C2s,C4s),
	append(C4s,C3s,C5s),
	num_op:conjunction(C5s,Cin,Cin2),
	term_variables(Lm,VarLm2),
	(num_op:project(VarLm2,Cin2,VarLm2,Cin3) ->
	    true
	;
	    raise_exception(dual_faux_sur(Head))),
	generer_constr_n(Bs,Eqs,Head,Lm,Cin3,Cout).
	
%%%
calcul_param1(H,B,Lm,Param1,Cs):-
	functor(H,P,N),member(P/N-Mu_H,Lm),
	functor(B,Q,M),member(Q/M-Mu_B,Lm),!,
	opposes(Mu_B,Moins_Mu_B,Cs),
	append(Mu_H,Moins_Mu_B,Param1).

calcul_param2(B,Lm,Param2):-
	functor(B,P,N),member(P/N-Param2,Lm),!.

opposes([],[],[]).
opposes([X|Xs],[Y|Ys],[Y= -X|Cs]):-opposes(Xs,Ys,Cs).
