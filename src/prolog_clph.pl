:- module(prolog_clph,[flatprolog_clph/2]).

:- use_module(library(lists),[append/3]).
:- use_module(utils,[not_member/2,suppress_empty_constraint/2,clause_head/2,clause_body/2,build_clause/3]).

flatprolog_clph(FPCl,ObjCl) :-
    %
    %trace,
	clause_head(FPCl,H),
	normalise_atome(H,Hn,Cs),
	clause_body(FPCl,Bs),
	normalise_corps(Bs,Ds),
	suppress_empty_constraint(['$constraint'(Cs)|Ds],Es),
	build_clause(Hn,Es,ObjCl).

normalise_atome(H,Hn,Cs) :-
	functor(H,P,N),
	functor(Hn,P,N),
	normalise_args(N,H,Hn,[],Cs,[]).

normalise_args(0,_H,_Hn,Cs,Cs,_) :- !.
normalise_args(N,H,Hn,Cs,Ds,Vars) :-
	N>0,M is N-1,
	arg(N,H,An),
	arg(N,Hn,Xn),
	%% attn : Hn doit etre normalise ex: p(A,A) non, p(AB) ok
        (var_premiereOcc(An,Vars) ->
            (Xn=An,C2s=Cs,Var2s=[An|Vars])
	;
	    (C2s=[Xn=An|Cs],Var2s=Vars)),
	%%% Xn=An,C2s=Cs,Var2s=[An|Vars],
	normalise_args(M,H,Hn,C2s,Ds,Var2s).

var_premiereOcc(X,Xs) :- var(X),not_member(X,Xs).

normalise_corps([],[]).
normalise_corps([A|As],Cns) :-
	normalise_elt(A,Ans),
	append(Ans,Bns,Cns),
	normalise_corps(As,Bns).

normalise_elt('$predef'('$bool'(A)),['$predef'('$bool'(A))]) :-!.
normalise_elt('$predef'('$num'(A)), ['$predef'('$num'(A))]) :-!.
normalise_elt('$predef'('$term_cond'(A)), ['$predef'('$term_cond'(A))]) :-!.
normalise_elt('$predef'(A),['$constraint'(Cs),'$predef'(An)]) :-
	!,normalise_atome(A,An,Cs).
normalise_elt(A,['$constraint'(Cs),An]) :-
	normalise_atome(A,An,Cs).
