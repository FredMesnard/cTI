:- module(num_prolog_clpn,[prologs_clpns/2]).

:- use_module(prolog_flatprolog,[prolog_flatprologs/2]).	
:- use_module(prolog_clph,[flatprolog_clph/2]).
:- use_module(num_clph_clpn,[clph_clpn/2]).
:- use_module(library(lists),[append/3]).

prologs_clpns([],[]).
prologs_clpns([Cl|Cls],ClpNCls3) :-
	%write(Cl),nl,
	prolog_flatprologs(Cl,FPCls),
	fps_clpns(FPCls,ClpNCls1),
	append(ClpNCls1,ClpNCls2,ClpNCls3),
	prologs_clpns(Cls,ClpNCls2).


fps_clpns([],[]).
fps_clpns([Cl|Cls],[Cln|Clns]) :-
	flatprolog_clph(Cl,Clh),
	clph_clpn(Clh,Cln),
	fps_clpns(Cls,Clns).
