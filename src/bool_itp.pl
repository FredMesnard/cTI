:- module(bool_itp,[tp/6]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(utils).
:- use_module(compat_swi,[cti_time_out/3]).


% SccRs = [[P1/N1-R1s ...],[P2/N2-R2s ...],...]
tp(SccRs,Module,Precision,Bfinal,Controle,TimeOut):-
	%trace,
	mod_modprop(Module,ModuleProp),
	base_initiale(ModuleProp,Binit),
	tp(SccRs,Module,ModuleProp,Precision,Binit,Bprefinal,Controle,TimeOut),
	majfinal(SccRs,Module,Bprefinal,Bfinal).

mod_modprop(num_op,pronatres).
mod_modprop(bool_op,proboolres).

tp([],_Module,_ModuleProp,_Precision,B,B,_Controle,_TimeOut).
tp([SccR|SccRs],Module,ModuleProp,Precision,Bglobal,Bfinal,Controle,TimeOut):-
	%trace,
	precision(Precision,SccR,M),
	cti_time_out(
        (bool_itp:base_depart(SccR,Module,Bglobal,B0,SccR2),
		 bool_itp:iterate_tp(SccR2,Module,Bglobal,B0,0,M,Prof,Blocal,Controle,TimeOut)),
		 TimeOut,
		 Result),
	(Result=success ->
	    true
	;
	    ( virer_ref(SccR,Scc),
	      base_depart_timeout(Scc,t,Blocal,Module),
	      %nl,write('% '),
	      write('% bool model: '),write(Scc),write(' time_out! '),
	      nl,flush_output,
	      Prof=time_out)),
	maj_prof(Blocal,M,Prof,Bglobal,Bglobal2),
	tp(SccRs,Module,ModuleProp,Precision,Bglobal2,Bfinal,Controle,TimeOut).

precision(length+N,SccR,M):-integer(N),!,length(SccR,L),M is L+N.
precision(no_widening,_SccR,1000000):-!.
precision(Precision,_SccR,Precision):-integer(Precision).

iterate_tp(SccR,Module,Bglobal,B0,J,K,Prof,B1,Controle,_TimeOut):-
	iterate_tp0(SccR,Module,Bglobal,B0,J,K,Prof,B1,Controle).

iterate_tp0(SccR,Module,Bglobal,B0,J,K,Prof,B1,Controle) :-
	I is J+1,
	itp(SccR,Module,Bglobal,B0,B0,B4),
	maj_base(I,K,B0,B4,B2,Module,Controle,Controle2),
	(base_incluse(B2,B0,Module) ->
	    (B1=B2,Prof=I)
	;   
	    iterate_tp0(SccR,Module,Bglobal,B2,I,K,Prof,B1,Controle2)).

maj_prof(t,_Precision,_Prof,Bg,Bg).
maj_prof(t(A1,A2,A3,A4,A5),Precision,Prof,Bg1,Bg3):-
	del_min_assoc(t(A1,A2,A3,A4,A5),K,Val,B2),
	put_assoc(K,Bg1,Val-(precision=Precision)-(nb_ite=Prof),Bg2),
	maj_prof(B2,Precision,Prof,Bg2,Bg3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% itp(+SccR,+Module,+Bglobal,+B0,-B1)
itp([],_Module,_Bglobal,_B0,B1,B1).
itp([P/N-Refs|SccR],Module,Bglobal,B0,B1,B3):-
	itp2(Refs,Module,P,N,Bglobal,B0,B1,B2),
	itp(SccR,Module,Bglobal,B0,B2,B3).

itp2([],_Module,_,_,_Bglobal,_,B,B).
itp2([R|Rs],Module,P,N,Bglobal,B0,B1,B3):-
	R=(Head:-body(['$constraint'(Cs)|Body])),
	Head=..[P|Vars],functor(Head,_,N),
	((proves(Body,Module,Cs,Bglobal,B0,ConsOut),Module:project(Vars,ConsOut,Vars,Cons)) ->
	    ( get_assoc(P/N,B1,Vars2-C2) ->
		( Module:entail(Vars,Cons,Vars2,C2)  ->
		    B2=B1
		;   
		    (	Module:union(Vars2,C2,Vars,Cons,Vars3,C3),
			put_assoc(P/N,B1,Vars3-C3,B2)))
	    ;	
		put_assoc(P/N,B1,Vars-Cons,B2))
	;   
	    B2=B1),
	itp2(Rs,Module,P,N,Bglobal,B0,B2,B3).
	
proves([],_Module,Cs,_Bglobal,_B,Cs).
proves(['$predef'(_)|Bs],Module,Cs,Bglobal,Base,CsOut):-
	!,proves(Bs,Module,Cs,Bglobal,Base,CsOut).
proves(['$constraint'(Ds)|Bs],Module,Cs,Bglobal,Base,CsOut):-
	!,Module:conjunction(Ds,Cs,Es),
	proves(Bs,Module,Es,Bglobal,Base,CsOut).
proves([B|Bs],Module,Cs,Bglobal,Base,C3):-
	B=..[Pb|Varsb],functor(B,_Pb,Nb),
	(get_assoc(Pb/Nb,Base,Varsb2-C22);get_assoc(Pb/Nb,Bglobal,Varsb2-C22-_-_)),
	!, copy_term(Varsb2-C22,Varsb-C2),
	Module:conjunction(C2,Cs,C5),
	proves(Bs,Module,C5,Bglobal,Base,C3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maj_base(I,K,_B0,B1,B1,_Mod,C,C):-
	I =< K,!,
	%write(I),write(' ').
	true.
maj_base(I,K,B0,B1,B3,Mod,C1,C2):-
	I > K,
	%write(I),write(' '),
	oprtr(C1,OprtrW,C2),
	%write(OprtrW),write(' '),
	maj_base2(B0,B1,B3,Mod,OprtrW).

maj_base2(t,B1,B1,_Mod,_OpW).
maj_base2(t(A1,A2,A3,A4,A5),B1,B4,Mod,OpW):-
	empty_assoc(B2),
	widen_base(t(A1,A2,A3,A4,A5),B1,B2,B1bis,B3,Mod,OpW),
        maj_base3(B1bis,B3,B4).

widen_base(t,B1,B2,B1,B2,_Mod,_OpW).
widen_base(t(A1,A2,A3,A4,A5),B1,B2,B1ter,B3,Mod,OpW):-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,V-C,B),
	del_assoc(P/N,B1,V2-C2,B1bis),
	Mod:widening(V,C,V2,C2,V3,C3), 
	put_assoc(P/N,B2,V3-C3,B4),
	widen_base(B,B1bis,B4,B1ter,B3,Mod,OpW).

maj_base3(t,B,B).
maj_base3(t(A1,A2,A3,A4,A5),B1,B2):-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,V-C,B),
	put_assoc(P/N,B1,V-C,B3),
	maj_base3(B,B3,B2).

%oprtr(A,O,B):-oprtr2(A,O,B),write(O),write(' '),flush_output.
oprtr([star-Op],Op,[star-Op]):-!.
oprtr([N-Op|Oprtrs],Op,[M-Op|Oprtrs]):-N>0,!,M is N-1.
oprtr([0-_Op|Oprtrs],Op,Ops):-oprtr(Oprtrs,Op,Ops).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_incluse(B4,B2) ssi B4 est incluse dans B2

base_incluse(t,_B2,_Mod).
base_incluse(t(A1,A2,A3,A4,A5),B2,Mod):-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,V1-C1,B0),
	(get_assoc(P/N,B2,V2-C2) -> Mod:entail(V1,C1,V2,C2)),
	base_incluse(B0,B2,Mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_initiale(SccRs,Module,Binit)

base_initiale(_Module,Binit):-
	empty_assoc(Binit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% majfinal(Sccrs,M,B1,Bfinal)
majfinal([],_M,B,B).
majfinal([Scc|Sccs],M,B0,B1):-
	majfinal2(Scc,M,B0,B2),
	majfinal(Sccs,M,B2,B1).

majfinal2([],_M,B,B).
majfinal2([P/N-_|Ps],M,B0,B1):-
	(get_assoc(P/N,B0,_) ->
	    B2=B0
	;
	    (length(Vars,N),
	     M:false(False),
	     put_assoc(P/N,B0,Vars-False-(precision=0)-(nb_ite=0),B2))),
	majfinal2(Ps,M,B2,B1).

% base_depart(SccR,Module,B0,B1,SccR2)

base_depart(SccR,Module,Bglobal,Bdepart,SccR2):-
	empty_assoc(Binit),
	bdep1(SccR,Module,Bglobal,Binit,Bdepart,SccR2).
	
bdep1([],_Module,_Bg,B,B,[]).
bdep1([P/N-Rs|SccRs],M,Bg,B1,B2,[P/N-R2s|SccR2s]):-
	bdep2(Rs,P/N,M,_,Bg,B1,B3,R2s),
	bdep1(SccRs,M,Bg,B3,B2,SccR2s).


bdep2([],_PN,_M,_MP,_Bg,B,B,[]).
bdep2([R|Rs],PS,Module,MP,Bg,B1,B,R2s):-
	clause_head(R,H),
	clause_body(R,Body),
	Module:true(Cs),
	partition(Body,Bg,Module,Cs,Csf,[],Bodyf),
	simplify(Module,Csf,H-Bodyf,CsSimp,Flag),
	(Flag=sat ->
	    bdep3(Bodyf,PS,H,CsSimp,Module,MP,Bg,B1,B2,Ref)
	;
	    (Ref=nil,B2=B1)),
	(Ref=nil -> R2s=R3s ; R2s=[Ref|R3s]),
	bdep2(Rs,PS,Module,MP,Bg,B2,B,R3s).

bdep3([],PS,H,Cs,Module,_MP,_Bg,B1,B2,nil):-
	H=..[_|Vars],
	(get_assoc(PS,B1,Vars2-C2) ->
	    ( Module:entail(Vars,Cs,Vars2,C2)  ->
		B2=B1
	    ;
		(   Module:union(Vars2,C2,Vars,Cs,Vars3,C3),
		    put_assoc(PS,B1,Vars3-C3,B2)))
	;
	    put_assoc(PS,B1,Vars-Cs,B2)).
bdep3([A|As],_PS,H,Cs,_Module,_MP,_Bg,B1,B1,(H:-body(['$constraint'(Cs),A|As]))).

partition([],_Bg,_M,Cs,Cs,Bs,Bs).
partition(['$predef'(_A)|Bs],Bg,M,Cs1,Cs2,Bs1,Bs2):-
	!,partition(Bs,Bg,M,Cs1,Cs2,Bs1,Bs2).
partition(['$constraint'(Cs)|Bs],Bg,M,Cs1,Cs2,Bs1,Bs2):-
	!,M:conjunction(Cs,Cs1,Cs3),partition(Bs,Bg,M,Cs3,Cs2,Bs1,Bs2).
partition([B|Bs],Bg,M,Cs1,Cs2,Bs1,Bs2):-
        %partition(Bs,Bg,M,Cs1,Cs2,[B|Bs1],Bs2),     % ok mais ce qui suit est legerement + efficace
	functor(B,F,N),B=..[_|Vars],
	(get_assoc(F/N,Bg,Var2s-Conts-_-_) ->
	    ( copy_term(Var2s-Conts,Vars-Cs), % car B peut apparaitre +sieurs fois (modulo Vars)
		M:conjunction(Cs,Cs1,Cs3),
		Bs3=Bs1)
	;
	    ( Bs3=[B|Bs1],
		Cs3=Cs1)),
        partition(Bs,Bg,M,Cs3,Cs2,Bs3,Bs2).

simplify(M,Cs,Term,Ds,F):-
	term_variables(Term,Vars),
	( M:satisfiable(Cs) ->
	    (  M:project(Vars,Cs,Vars,Ds),
		F=sat)
	;   (  M:false(Ds),
		F=nonsat)).

/* version sans simplification:
bdep2([],_PN,_M,_Bg,B,B,[]).
bdep2([R|Rs],PS,Module,Bg,B1,B,[(H:-Body)|R2s]):-
	clause(_:H,Body,R),
	bdep2(Rs,PS,Module,Bg,B1,B,R2s).
*/

%%%%%%%%%%%%%%
base_depart_timeout([],B,B,_Mod).
base_depart_timeout([P/N|Ps],B1,B2,Mod):-
	length(Vars,N),
	Mod:true(True),
	put_assoc(P/N,B1,Vars-True,B3),
	base_depart_timeout(Ps,B3,B2,Mod).
