:- module(term_cond,[term_cond/4]).

:- use_module(library(lists),[member/2]).
:- use_module(library(clpb)).

:- use_module(predef).
:- use_module(bool_op,[]).
:- use_module(bool_dc).
:- use_module(utils).
:- use_module(db).

:- use_module(compat_swi,[cti_time_out/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% term_cond(+SccsClsB,+DbIn,-DbOut,+TimeOut)

term_cond(SccsClsB,Db0,Db,TimeOut) :-
	%trace,
	tc1(SccsClsB,Db0,Db,TimeOut).	

%% tc1(SccsClsB,Db0,Db,TimeOut)
tc1([],Db,Db,_).
tc1([Ps|Pss],Db0,Db,TimeOut) :-
	associer_pred_vars_tc(Ps,P_V_Ts,Blms,Db0),
	tc2(Ps,Blms,P_V_Ts,Db0,Db1,TimeOut),
	tc1(Pss,Db1,Db,TimeOut).
	
    associer_pred_vars_tc([],[],[],_).
    associer_pred_vars_tc([P/N-_|Ps],[P/N-Vars-Tcp|P_V_Ts],[P/N-VarsP-Blm|Blms],Db) :-
        length(Vars,N),
        %var(Tcp),
        Tcp=tc(P,N,Vars),
	    get_db(P,N,blm,Db,VarsP-Blm),
	    associer_pred_vars_tc(Ps,P_V_Ts,Blms,Db).

tc2(Ps,MesB,P_V_Ts,Db0,Db,TimeOut) :-
    virer_ref(Ps,PIs),
    %(PIs==['$initialization'/0] -> trace ; true),
    cti_time_out(term_cond:tc3(Ps,MesB,P_V_Ts,Db0,Bfinal,TimeOut),TimeOut,Result),
	(Result=time_out ->
	    write('% bool term cond: '),
		write(PIs),
		writeln(' time_out!'),
		pvts_timeout(Ps,Db0,Db)
	;
	    %write('% bool term cond: '),
        %writeln(PIs),
        store_gfp(Bfinal,Db0,Db)).


    pvts_timeout([],Db,Db).
    pvts_timeout([P/N-_|Ps],Db0,Db) :-
	    length(Vars,N),
	    put_db(P,N,termcond,Db0,Vars-0,Db1),
        put_db(P,N,pos_termcond,Db1,Vars-0,Db),
	    pvts_timeout(Ps,Db1,Db).

tc3(Ps,MesB,P_V_Ts,Db0,Bfinal,TO) :-
    %trace,
	tc4(Ps,MesB,P_V_Ts,Db0,System),
    %
    %\+ \+ ( numbervars(System,0,_),	write(System), nl),
    %trace,
    virer_ref(Ps,PIs),
    greatest_fxpt(PIs,System,Bfinal,TO).    
	%store_gfp(Bfinal,Db0,Db).

    aff_syst([]) :- nl.
    aff_syst([P/N-Syst|S]) :- writeln(P/N-Syst), aff_syst(S).

    store_gfp([],Db,Db).
    store_gfp([P/N-Vars-T|P_V_Ts],Db0,Db) :-
        put_db(P,N,termcond,Db0,Vars-T,Db1),
        dc(Vars,T,PosT),
        put_db(P,N,pos_termcond,Db1,Vars-PosT,Db2),
        store_gfp(P_V_Ts,Db2,Db).
    
    greatest_fxpt(PIs,System,Bfinal,TO) :-
        base_init_all_true(PIs,Binit),
        compute_greatest_fxpt(System,Binit,Bfinal,TO).
        
        base_init_all_true([],[]).
        base_init_all_true([P/N|Ps],[P/N-Vars-1|Binit]) :- length(Vars,N), base_init_all_true(Ps,Binit).
        
        %compute_fxpt(_S,B,B) :- !.
        compute_greatest_fxpt(System,B0,Bf,TO) :-
            one_iteration(System,B0,B0,B1,TO),
            (is_fxpt(B0,B1) -> Bf=B0 ; compute_greatest_fxpt(System,B1,Bf,TO)).
        
            is_fxpt([],[]).
            is_fxpt([P/N-Vars0-Tc0|B0],[P/N-Vars1-Tc1|B1]) :- bool_op:equivalent(Vars0,Tc0,Vars1,Tc1), is_fxpt(B0,B1).
            
            one_iteration([],_B,Binter,Binter,_TO).
            one_iteration([P/N-nu(tc(P,N,Vars),Expr)|Syst],B,Binter,Bfinal,TO) :-
                substitute_tc_value(Expr,B,Nexpr),
                simplify_value(Nexpr,Vars,SNexpr),
                %
                bool_op:simplify_constraint_else_false(SNexpr,SSNexpr,TO),
                %
                update(Binter,P,N,Vars,SSNexpr,Binter1),
                one_iteration(Syst,B,Binter1,Bfinal,TO).
            
                update([P/N-Vars-Tc|B],P,N,Vars2,Tc2,[P/N-Vars-Tc*Tc3|B]) :- !, copy_term(Vars2-Tc2,Vars-Tc3).
                update([P/N-Vars-Tc|B],Q,M,Vars2,Tc2,[P/N-Vars-Tc|B2]) :- P/N \= Q/M, update(B,Q,M,Vars2,Tc2,B2).
            
                %substitute_tc_value(Expr,[P/N-Vars-Tc|B],Nexpr)
                substitute_tc_value(X,_B,X) :- var(X), !.
                substitute_tc_value(X,_B,X) :- atomic(X), !.
                substitute_tc_value(X^T,B,X^U) :- !, substitute_tc_value(T,B,U). 
                substitute_tc_value(forall(X,T),B,forall(X,U)) :- !, substitute_tc_value(T,B,U). 
                substitute_tc_value(~X,B,~Y) :- !, substitute_tc_value(X,B,Y).
                substitute_tc_value(tc(P,N,Vars),B,TC) :- !,get_value(B,P,N,Vars,TC).
                substitute_tc_value(T,B,U) :- T=..[Op,T1,T2], U=..[Op,S1,S2],substitute_tc_value(T1,B,S1),substitute_tc_value(T2,B,S2).
            
                    get_value([P/N-Vars-TC|_],P,N,Vars2,TC2) :- !, copy_term(Vars-TC,Vars2-TC2).
                    get_value([Q/M-_Vars-_TC|B],P,N,Vars2,TC2) :- P/N \= Q/M, get_value(B,P,N,Vars2,TC2).
            
                simplify_value(X,_Vars,X) :- var(X), !.
                simplify_value(X,_Vars,X) :- atomic(X), !.
                simplify_value(X^T,Vars,X^U) :- !, simplify_value(T,Vars,U).
                simplify_value(forall(X,T),Vars,~(X^(~ U))) :- !, simplify_value(T,Vars,U).
                simplify_value(~X,Vars,~Y) :- !,simplify_value(X,Vars,Y).
                simplify_value(T,Vars,U) :- T=..[Op,T1,T2], U=..[Op,U1,U2], simplify_value(T1,Vars,U1),simplify_value(T2,Vars,U2).
                
        
                 


tc4([],_,_P_V_Ts,_Db,[]).
tc4([P/N-Refs|Ps],MesB,P_V_Ts,Db,[P/N-Syst|Systs]) :-
	member(P/N-Vars-Mbpn,MesB),
	member(P/N-Vars-Tcpn,P_V_Ts),!,
	tc5(Refs,P/N,Vars,Mbpn,Syst_Bool,P_V_Ts,Db),
	Syst=nu(Tcpn,Syst_Bool),
	tc4(Ps,MesB,P_V_Ts,Db,Systs).


% tc5(Refs,P/N,Vars,Mbpn,Syst_Bool,P_V_Ts,Db),	
tc5([],_,_,Syst_Bool,Syst_Bool,_,_).
tc5([R|Refs],P/N,Vars,S0,S,P_V_Ts,Db) :-
	clause_head(R,H),clause_body(R,Body),
	bool_op:true(Cinit), 
    H=..[P|Vars],
    %
    %trace,
	tc6(Body,Cinit,Vars,S0,S1,P_V_Ts,Db),
	tc5(Refs,P/N,Vars,S1,S,P_V_Ts,Db).

% tc6(Body,Cb,Vars,S1,S2,P_V_Ts,Db)
tc6([],_,_,Syst_Bool,Syst_Bool,_,_).
tc6(['$constraint'(C)|Bs],Cb,Vars,S0,S,P_V_Ts,Db) :-
	!, tc6(Bs,C*Cb,Vars,S0,S,P_V_Ts,Db).
tc6(['$predef'(A)|Bs],Cb,Vars,S0,S,P_V_Ts,Db) :-
	!, predef_nat_bool_tc(A,_,Model,TcA),
	Formula = (Cb =< TcA), %%
	free_vars(Formula,Vars,FreeVars),
	quantif_univ(FreeVars,Formula,S1),
	tc6(Bs,Cb*Model,Vars,S0*S1,S,P_V_Ts,Db).
tc6([B|Bs],Cb,Vars,S0,S,P_V_Ts,Db) :-
	functor(B,Q,M), member(Q/M-_-Tcqm0,P_V_Ts),!,    % B est dans le Scc
	copy_term(Tcqm0,Tcqm),
    B=..[Q|VarsQ], 
    Tcqm=tc(Q,M,VarsQ),
	Formula = (Cb =< Tcqm),
	free_vars(Formula,Vars,FreeVars),
	quantif_univ(FreeVars,Formula,S1),
    % pas necessaire d'ajouter le modele
    % car le prog bool est deja specialise
	%get_db(Q,M,modelb,Db,VarsQ-Modbq-_-_),
	%tc6(Bs,Cb*Modbq,Vars,S0*S1,S,P_V_Ts,Db).
	tc6(Bs,Cb,Vars,S0*S1,S,P_V_Ts,Db).    
tc6([B|Bs],Cb,Vars,S0,S,P_V_Ts,Db) :-
	functor(B,Q,M),                                 % B n'est pas dans le Scc
	get_db(Q,M,termcond,Db,VarsQ-TcQ),
	%bool_op:satisfiable(TcQ),!,
	B=..[_Q|VarsQ],
	Formula = (Cb =< TcQ),
	free_vars(Formula,Vars,FreeVars),
	quantif_univ(FreeVars,Formula,S1),
	%get_db(Q,M,modelb,Db,VarsQ-ModbQ-_-_),
	%tc6(Bs,Cb*ModbQ,Vars,S0*S1,S,P_V_Ts,Db).
    tc6(Bs,Cb,Vars,S0*S1,S,P_V_Ts,Db).
tc6([_B|_Bs],_Cb,_Vars,_S,0,_P_V_Ts,_Db).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% quantif_univ(FreeVars,Tb,For_All_Tb)

quantif_univ([],Tb,Tb).
quantif_univ([X|Xs],Tb1,Tb2) :- quantif_univ(Xs,forall(X,Tb1),Tb2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% free_vars(Term,Vars,FreeVars)
%% 	FreeVars est l'ensemble des var libres de Term
%% 	qui n'apparaissent pas dans Vars

free_vars(Term,Vars,FreeVars) :- fv(Term,Vars,[],FreeVars).

fv(X,Vars,FV,FV) :- var(X), (member_var(Vars,X) ; member_var(FV,X)),!.
fv(X,_,FV,[X|FV]) :- var(X),!.
fv(X,_,FV,FV) :- ground(X),!.
fv(~T,Vars,FV1,FV2) :- !, fv(T,Vars,FV1,FV2).
fv(X^T,Vars,FV1,FV2) :- !, fv(T,[X|Vars],FV1,FV2).
fv(eval(_,Params),Vars,FV1,FV2) :- !, fv(Params,Vars,FV1,FV2).
fv(tc(_P,_N,Params),Vars,FV1,FV2) :- !, fv(Params,Vars,FV1,FV2).
fv(T,Vars,FV1,FV2) :- T=..[_,T1,T2], fv(T1,Vars,FV1,FV3), fv(T2,Vars,FV3,FV2).
	
member_var([Y|_],X) :- X==Y,!.
member_var([_|Ys],X) :- member_var(Ys,X).















