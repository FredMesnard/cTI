:- module(db,[
	      empty_db/1,
	      get_db/5,
	      put_db/6,
	      %add_db/6,
	      print_db/1,
	      add_prop_domain_dbs/4,
	      add_cls_domain_dbs/4,
	      get_sccs_domain_db_props/4
	     ]).

:- use_module(library(assoc)).
:- use_module(utils,[clause_head/2,write_list/1]).
:- use_module(library(lists),[append/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_db(t). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_db(P,N,Id,M,V) :-
	mget_assoc(P,M,M1),
	mget_assoc(N,M1,M2),
	mget_assoc(Id,M2,Val),
	(Val==t ->
	    V=[]
	;
	    copy_term(Val,V)).

mget_assoc(Key,Assoc,Val) :-
	(get_assoc(Key,Assoc,V) ->
	    Val=V
	;
	    Val=t).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
put_db(P,N,Id,OldM,V,NewM) :-
	mget_assoc(P,OldM,M1),
	mget_assoc(N,M1,M2),
	put_assoc(Id,M2,V,M3),
	put_assoc(N,M1,M3,M4),
	put_assoc(P,OldM,M4,NewM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_db(P,N,Id,OldM,V,NewM) :-
	mget_assoc(P,OldM,M1),
	mget_assoc(N,M1,M2),
	mget_assoc(Id,M2,W),
	(W==t ->
	    put_assoc(Id,M2,[V],M3)
	;
	    put_assoc(Id,M2,[V|W],M3)),
	put_assoc(N,M1,M3,M4),
	put_assoc(P,OldM,M4,NewM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_db(t).
print_db(t(T1,T2,T3,T4,T5)) :-
	del_min_assoc(t(T1,T2,T3,T4,T5),P,M1,M2),
	print_db2(M1,P),
	print_db(M2).

print_db2(t,_). 
print_db2(t(T1,T2,T3,T4,T5),P) :-
	del_min_assoc(t(T1,T2,T3,T4,T5),N,M1,M2),
	nl,print_db3(M1,P,N),
	print_db2(M2,P).

print_db3(t,_,_).
print_db3(t(T1,T2,T3,T4,T5),P,N) :-
	del_min_assoc(t(T1,T2,T3,T4,T5),Id,V,M1),
	print_db4(Id,P,N,V),
	print_db3(M1,P,N).

print_db4(Id,P,N,V) :-
	is_list(V),!,
	write(P),write('/'),write(N),write(' '),write(Id),write(':'),nl,
	write_list(V).
print_db4(Id,P,N,V) :-
	write(P),write('/'),write(N),write(' '),write(Id),write(':'),nl,
	write_list([V]).

is_list([]).
is_list([_|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_prop_domain_dbs([],_,A,A).
add_prop_domain_dbs([P/N-Prop|Ps],Id,A0,A) :-
	put_db(P,N,Id,A0,Prop,A1),
	add_prop_domain_dbs(Ps,Id,A1,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_cls_domain_dbs([],_D,M,M).
add_cls_domain_dbs([Cl|Cls],Domain,M0,M) :-
	clause_head(Cl,H),
	functor(H,P,N),
	add_db(P,N,Domain,M0,Cl,M1),
	add_cls_domain_dbs(Cls,Domain,M1,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_sccs_domain_db_props([],_,_,[]).
get_sccs_domain_db_props([Scc|Sccs],Domain,Db,Props) :-
	get_sccs_domain_db2(Scc,Domain,Db,Props0),
	append(Props0,Props1,Props),
	get_sccs_domain_db_props(Sccs,Domain,Db,Props1).

get_sccs_domain_db2([],_,_,[]).
get_sccs_domain_db2([P/N|PIs],Domain,Db,[predicate_term_condition(Atom,Tc)|Props]) :-
	get_db(P,N,Domain,Db,Vars-Tc),Atom=..[P|Vars],
	get_sccs_domain_db2(PIs,Domain,Db,Props).
