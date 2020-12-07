%% Fred - Roberto
%% juillet 99 - avril 2000
%% mai 2002 - octobre 2002

:- module(num_itp,[ntp/6]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(utils).
:- use_module(compat_swi).

% SccRs = [[P1/N1-R1s ...],[P2/N2-R2s ...],...]
ntp(SccRs,Module,Precision,Bfinal,Controle,TimeOut) :-
	%trace,
	base_initiale(Module,Binit),
	tp(SccRs,Module,Precision,Binit,Bprefinal,Controle,TimeOut),
	majfinal(SccRs,Module,Bprefinal,Bfinal).

tp([],_Module,_Precision,B,B,_Controle,_TimeOut).
tp([SccR|SccRs],Module,Precision,Bglobal,Bfinal,Controle,TimeOut) :-
	%trace,
	%write(SccR),write(' '),nl,
	%(SccR=[possessive/14-_] -> trace;true),
	cti_time_out(
		 (num_itp:base_depart(SccR,Module,Bglobal,B0,SccNonRec,SccRec),
		  num_itp:iterate_tp(SccNonRec,SccRec,Module,Bglobal,B0,Precision,Prof,
			     Blocal,Controle,TimeOut)),
		 TimeOut,
		 Result),
	(Result=success ->
	    true
	;
	    ( virer_ref(SccR,Scc),
	      base_depart_timeout(Scc,t,Blocal,Module),
	      write('% nat model: '),
	      write(Scc),write(' time_out!'),nl,flush_output,
	      Prof=time_out)),
	maj_prof(Blocal,Precision,Prof,Bglobal,Bglobal2),
	tp(SccRs,Module,Precision,Bglobal2,Bfinal,Controle,TimeOut).


iterate_tp(SccNonRec,SccRec,Module,Bglobal,B0,K,Prof,B1,Controle,_TimeOut) :-
	empty_assoc(Empty),
	maj_base3(B0,Module,Empty,B0copy),
	%trace,
	itp(SccNonRec,Module,Bglobal,B0,B0copy,B0ter),
 	(SccRec=[_-[]] ->  % pas de clause recursive 
 	    B1=B0ter,
 	    Prof=1
 	;
 	    iterate_tp0(SccRec,Module,Bglobal,B0ter,1,K,Prof,B1,Controle)).
%%% 	 iterate_tp0(SccRec,Module,Bglobal,B0ter,1,K,Prof,B1,Controle).


iterate_tp0(SccR,Module,Bglobal,B0,J,K,Prof,B1,Controle) :-
	%write('--------- debut iteration : '),write(J),nl,
	%((J=33) -> trace;true),
%	(get_assoc(p/1,B0,XXXX) -> poly_ppl:pp('poly : ',XXXX) ; true),
%	write('--------------------------------'),nl,
	I is J+1,
	% copie explicite
	empty_assoc(Empty),maj_base3(B0,Module,Empty,B0copy),
%	(get_assoc(p/1,B0,XXXX1) -> poly_ppl:pp('poly apres maj base3 : ',XXXX1) ; true),
	%
	itp(SccR,Module,Bglobal,B0,B0copy,B4),
%	(get_assoc(p/1,B0,YYYY2) -> poly_ppl:pp('poly apres itp: ',YYYY2) ; true),
	maj_base(I,K,B0,B4,B2,Module,Controle,Controle2),
%	write('============ base_incluse ?'),nl,
%	write('--- B0'),nl,
%	aff_base(B0,Module),
%	write('--- B2'),nl,
%	aff_base(B2,Module),
%	write('================================'),nl,
	(base_incluse(B2,B0,Module) ->
%	    ( write('--- B2 included in B0'),nl,
	    dispose_base(B0,Module),
	    B1=B2,Prof=I
%	)
	;
%	    write('--- B2 NOT included in B0'),nl,
	    dispose_base(B0,Module),
	    iterate_tp0(SccR,Module,Bglobal,B2,I,K,Prof,B1,Controle2)).

maj_prof(t,_Precision,_Prof,Bg,Bg).
maj_prof(t(A1,A2,A3,A4,A5),Precision,Prof,Bg1,Bg3) :-
	del_min_assoc(t(A1,A2,A3,A4,A5),K,Val,B2),
	put_assoc(K,Bg1,Val-(precision=Precision)-(nb_ite=Prof),Bg2),
	maj_prof(B2,Precision,Prof,Bg2,Bg3).

aff_base(B,_M) :-
	empty_assoc(B),!.
aff_base(B,Module) :-
	del_min_assoc(B,P/N,Poly,B0),
	write(P/N),write(': '),
        Module:poly_print(Poly),
	aff_base(B0,Module).

dispose_base(B,_M) :-
	empty_assoc(B),!.
dispose_base(B,Module) :-
	del_min_assoc(B,_P/_N,Poly,B0),
        Module:dispose(Poly),
	dispose_base(B0,Module).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% itp(+SccR,+Module,+Bglobal,+B0,+B1,-B2)
itp([],_Module,_Bglobal,_B0,B1,B1).
itp([P/N-Refs|SccR],Module,Bglobal,B0,B1,B3) :-
	itp2(Refs,Module,P,N,Bglobal,B0,B1,B2),
	itp(SccR,Module,Bglobal,B0,B2,B3).

itp2([],_Module,_,_,_Bglobal,_,B,B).
itp2([R|Rs],Module,P,N,Bglobal,B0,B1,B3) :-
	R=(Head :- body(Body)),
	functor(Head,P,N),
	Head=..[_|Vars],
	Module:create_dont_know(Vars,PolyIn),
	((proves(Body,Head,Module,PolyIn,Bglobal,B0,PolyOut),
	  Module:restrict(N,PolyOut,PolyRestrict)) ->
	    (get_assoc(P/N,B1,PolyOld) ->
		Module:join(PolyOld,PolyRestrict,PolyNew),
		put_assoc(P/N,B1,PolyNew,B2)
	    ;	
		put_assoc(P/N,B1,PolyRestrict,B2))
	;   
	    B2=B1
	),
	itp2(Rs,Module,P,N,Bglobal,B0,B2,B3).
	

proves(['$constraint'(Ds)|Bs],H,Module,Poly,Bglobal,Base,PolyOut) :-
	!,Module:extend_and_add_constraints(Poly,Ds,PolyExt),
	proves2(Bs,H,Module,PolyExt,Bglobal,Base,PolyOut).
proves2([],_H,_Module,Poly,_Bglobal,_B,Poly).
proves2([B|Bs],H,Module,Poly,Bglobal,Base,PolyOut) :-
	B=..[Pb|Varsb],functor(B,_Pb,Nb),
	(
	  get_assoc(Pb/Nb,Base,PolyB2)
% 	  poly_ppl:pp(' from assoc local 2 ', PolyB2)
	;
	    get_assoc(Pb/Nb,Bglobal,PolyB2-_-_)
%  	    poly_ppl:pp(' from assoc global 1 ', PolyB2)
	),
	!,
	%% copy_term(PolyB2,PolyB),
	Module:rename(PolyB2,Varsb,PolyB),
	Module:extend_and_meet(Poly,PolyB,NewPoly),
	proves2(Bs,H,Module,NewPoly,Bglobal,Base,PolyOut).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maj_base(I,K,_B0,B1,B1c,Mod,C,C) :-
	I =< K,!,
	%write(I),
	%(I==2 -> (write(tracer),nl,trace);true),
	%write(' '),
	empty_assoc(Binit),
	maj_base3(B1,Mod,Binit,B1c).

maj_base(I,K,B0,B1,B3,Mod,C1,C2) :-
	I > K,
	%write(I),
	%(I==2 -> (write(tracer),nl,trace);true),
	%write(' '),
	oprtr(C1,OprtrW,C2),
	%write(OprtrW),write(' '),
	maj_base2(B0,B1,B3,Mod,OprtrW).

maj_base2(t,B1,B1,_Mod,_OpW).
maj_base2(t(A1,A2,A3,A4,A5),B1,B4,Mod,OpW) :-
	empty_assoc(B2),
	widen_base(t(A1,A2,A3,A4,A5),B1,B2,B1bis,B3,Mod,OpW),
        maj_base3(B1bis,Mod,B3,B4).

widen_base(t,B1,B2,B1,B2,_Mod,_OpW).
widen_base(t(A1,A2,A3,A4,A5),B1,B2,B1ter,B3,Mod,OpW) :-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,Poly1,B),
	del_assoc(P/N,B1,Poly2,B1bis),
	Mod:widening(Poly2,Poly1,Poly3),
	put_assoc(P/N,B2,Poly3,B4),
	widen_base(B,B1bis,B4,B1ter,B3,Mod,OpW).

maj_base3(t,_Mod,B,B).
maj_base3(t(A1,A2,A3,A4,A5),Mod,B1,B2) :-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,Poly,B),
	Mod:copy(Poly,Polyc),
	put_assoc(P/N,B1,Polyc,B3),
	maj_base3(B,Mod,B3,B2).

oprtr([star-Op],Op,[star-Op]) :-!.
oprtr([N-Op|Oprtrs],Op,[M-Op|Oprtrs]) :-N>0,!,M is N-1.
oprtr([0-_Op|Oprtrs],Op,Ops) :-oprtr(Oprtrs,Op,Ops).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_incluse(B4,B2) ssi B4 est incluse dans B2

base_incluse(t,_B2,_Mod).
base_incluse(t(A1,A2,A3,A4,A5),B2,Mod) :-
	del_min_assoc(t(A1,A2,A3,A4,A5),P/N,Poly1,B0),
	(get_assoc(P/N,B2,Poly2) ->
%	    poly_ppl:pp(' from assoc local 3 ', Poly2),
	    Mod:more_precise_or_equal(Poly1,Poly2)),
	base_incluse(B0,B2,Mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_initiale(SccRs,Module,Binit)

base_initiale(_Module,Binit) :-
	empty_assoc(Binit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% majfinal(Sccrs,M,B1,Bfinal)
majfinal([],_M,B,B).
majfinal([Scc|Sccs],M,B0,B1) :-
	majfinal2(Scc,M,B0,B2),
	majfinal(Sccs,M,B2,B1).

majfinal2([],_M,B,B).
majfinal2([P/N-_|Ps],M,B0,B1) :-
	(get_assoc(P/N,B0,Poly-Prec-Ite) ->
	    (M:get_constraints(Poly,Cs),
	     M:get_variables(Poly,Vs),
	     M:dispose(Poly),	
	     put_assoc(P/N,B0,Vs-Cs-Prec-Ite,B2))
	;
	    (M:create_impossible(N,Poly),
	     M:get_constraints(Poly,Cs),
	     M:get_variables(Poly,Vs),
             M:dispose(Poly),
	    put_assoc(P/N,B0,Vs-Cs-(precision=0)-(nb_ite=0),B2))),
	majfinal2(Ps,M,B2,B1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_depart(SccR,Module,B0,B1,SccR2)

base_depart(SccR,Module,Bglobal,Bdepart,SccNonRec,SccRec) :-
	empty_assoc(Binit),
	virer_ref(SccR,Scc),
	bdep1(SccR,Scc,Module,Bglobal,Binit,Bdepart,SccNonRec,SccRec).
	
bdep1([],_,_Module,_Bg,B,B,[],[]).
bdep1([P/N-Rs|SccRs],SccR,M,Bg,B1,B2,[P/N-NRecs|SccR2s],[P/N-Recs|SccR3s]) :-
	bdep2(Rs,SccR,P/N,M,Bg,B1,B3,NRecs,Recs),
	bdep1(SccRs,SccR,M,Bg,B3,B2,SccR2s,SccR3s).

bdep2([],_,_PN,_M,_Bg,B,B,[],[]).
%bdep2([R|Rs],Scc,PS,Module,Bg,B1,B,[(H:-Body2)|NonRecs],[(H:-Body2)|Recs]) :-
bdep2([R|Rs],Scc,PS,Module,Bg,B1,B,NonRecs,[(H:-Body2)|Recs]) :-
    clause_head(R,H),clause_body(R,Bs),
	is_rec(Bs,Scc),!,
	process(H,Bs,Body2),
	bdep2(Rs,Scc,PS,Module,Bg,B1,B,NonRecs,Recs).
%bdep2([R|Rs],Scc,PS,Module,Bg,B1,B,[(H:-Body2)|NonRecs],[(H:-Body2)|Recs]) :-
bdep2([R|Rs],Scc,PS,Module,Bg,B1,B,[(H:-Body2)|NonRecs],Recs) :-
    clause_head(R,H),clause_body(R,Bs),
	process(H,Bs,Body2),
	bdep2(Rs,Scc,PS,Module,Bg,B1,B,NonRecs,Recs).

process(_H,Bs,Body2) :-
	%term_variables(H-Bs,Vars),
	%tous_pos(Vars,CPos), %CPos=[],
	rassemble_constraintes(Bs,Cs,Atoms),
	%append(CPos,Cs,CsFinal),
	% F
    % remove_duplicates(Cs,CsFinalp),
    CsFinalp = Cs,
	%simplify(Vars,CsFinal,CsFinalp),
	Body2=body(['$constraint'(CsFinalp)|Atoms]).
	%\+ (\+ (numbervars(H-Body2,0,_),write((H :- Body2)),nl)),

is_rec([B|_Bs],Scc) :-functor(B,P,N),member(P/N,Scc),!.
is_rec([_B|Bs],Scc) :-is_rec(Bs,Scc).

rassemble_constraintes([],[],[]).
rassemble_constraintes(['$predef'(_)|Bs],Cs,Atoms) :-
	!,rassemble_constraintes(Bs,Cs,Atoms).
rassemble_constraintes(['$constraint'(Cs)|Bs],Ds,Atoms) :-
	!,append(Cs,Cs2,Ds),
	rassemble_constraintes(Bs,Cs2,Atoms).
rassemble_constraintes([B|Bs],Cs,[B|Atoms]) :-
	rassemble_constraintes(Bs,Cs,Atoms).

%simplify(Vars,Cs,CsP) :-
%	(num_op:project(Vars,Cs,Vars,CsP) -> true ; CsP=[rat(0,1)=rat(1,1)]).

%%%%%%%%%%%%%%
base_depart_timeout([],B,B,_Mod).
base_depart_timeout([P/N|Ps],B1,B2,Mod) :-
	length(Vars,N),
	tous_pos(Vars,Cs),
	Mod:create_dont_know(Vars,Poly),
	Mod:extend_and_add_constraints(Poly,Cs,PolyExt),
	Mod:restrict(N,PolyExt,Poly_TimeOut),
	%Mod:true(True),
	put_assoc(P/N,B1,Poly_TimeOut,B3),
	base_depart_timeout(Ps,B3,B2,Mod).
