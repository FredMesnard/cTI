current_cti_flag(cTI_version,'2.0').
current_cti_flag(process_include_ensure_loaded,no).
current_cti_flag(time_out,10).  % in seconds
current_cti_flag(nb_ite_clpn,4).
current_cti_flag(print_info,yes).
current_cti_flag(poly_lib,poly_clpq).
current_cti_flag(known_predicate,PI) :-
	(var(PI) ->
	    predef(Atom),
	    functor(Atom,P,N),
	    PI=P/N
	;
	    (ground(PI) ->
		PI=P/N,
		functor(Atom,P,N),
		predef(Atom))).

set_cti_flag(X,_) :- var(X),!,fail.
set_cti_flag(time_out,V) :-
	integer(V), V >= 10, !, % min for timeout
	retractall(current_cti_flag(time_out,_)),
	asserta(current_cti_flag(time_out,V)).
set_cti_flag(nb_ite_clpn,V) :-
	integer(V), V >= 1, !,
	retractall(current_cti_flag(nb_ite_clpn,_)),
	asserta(current_cti_flag(nb_ite_clpn,V)).
set_cti_flag(process_include_ensure_loaded,V) :-
	(V==yes ; V==no),!,
	retractall(current_cti_flag(process_include_ensure_loaded,_)),
	asserta(current_cti_flag(process_include_ensure_loaded,V)).
set_cti_flag(print_info,V) :-
	(V==yes ; V==no),!,
	retractall(current_cti_flag(print_info,_)),
	asserta(current_cti_flag(print_info,V)).

