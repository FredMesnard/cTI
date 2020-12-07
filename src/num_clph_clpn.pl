:- module(num_clph_clpn,[clph_clpn/2]).

:- use_module(library(lists),[append/3]).
:- use_module(predef,[predef_nat_bool_tc/4]).
:- use_module(utils,[tous_pos/2,suppress_empty_constraint/2,clause_head/2,clause_body/2,build_clause/3]).
:- use_module(cti_term_size,[cti_term_size/2]).
:- use_module(num_op).

clph_clpn(ClpHCl,ClpNCl) :-
    %
    %trace,
	clause_head(ClpHCl,Hh),
	clause_body(ClpHCl,Bh),
	term_variables(Hh,VarsH),
	tous_pos(VarsH,Cs),
	tradnat_corps(Bh,Bn),
	suppress_empty_constraint(['$constraint'(Cs)|Bn],Body),
    simplify_body(Body,BodySimp),
	build_clause(Hh,BodySimp,ClpNCl).
    
simplify_body([],[]).
simplify_body(['$constraint'(Cs)|Body],['$constraint'(Ds)|BodySimp]) :- !, simplify_constraint(Cs,Ds),simplify_body(Body,BodySimp).
simplify_body([B|Body],[B|BodySimp]) :- simplify_body(Body,BodySimp).

    simplify_constraint(Cs,Cs) :- !.
    simplify_constraint(Cs,Ds) :- term_variables(Cs,Vars), project(Vars,Cs,Vars,Ds).
    
tradnat_corps([],[]).
tradnat_corps([A|As],Ds) :-
	tradnat_elt(A,Bs),
	append(Bs,Cs,Ds),
	tradnat_corps(As,Cs).

tradnat_elt('$predef'('$num'([A])),['$constraint'([A|Cs])]):-
	!,predef_nat_bool_tc(A,Cs,_,_).
tradnat_elt('$predef'(A),['$predef'(A),'$constraint'(Cs)]):-
	!,predef_nat_bool_tc(A,Cs,_,_). %% '$bool' ???
tradnat_elt('$constraint'(Cpro),['$constraint'(Cnat)]):-
	!,tradnat_constraint(Cpro,Cnat).
tradnat_elt(A,['$constraint'(Cs),A]):-
	term_variables(A,Vs),
	tous_pos(Vs,Cs).

tradnat_constraint([],[]).
tradnat_constraint([Cp|Cps],Dns) :-
	tc(Cp,Cn),
	append(Cn,Cns,Dns),
	tradnat_constraint(Cps,Cns).

tc(X=Terme,[X=NormTerme|Cs]) :-
	cti_term_size(Terme,NormTerme),
	term_variables2(NormTerme,X,Vars),
	tous_pos(Vars,Cs).

term_variables2(Z,_X,[]) :- Z==0,!.
term_variables2(NormTerme,X,[X|Vars]) :-
	term_variables(NormTerme,Vars).
