:- use_module(main).
:- use_module(library(system)).

%%% for the saved program state cTI:
%%% make cTI
runtime_entry(start2) :-
	runtime_entry(start),
	halt.

%%% for the executable cti:
%%% make 
runtime_entry(start) :-
	prolog_flag(argv, Args),
	(process_args(Args,_Mode) ->
	    true
	;
	    write('% Usage: cti [OPTION] FILE'), nl,
	    write_options).

write_options :-
      write('% Options: '), nl,
      % n N, p F, q, r, t T
      write('%  -n N'),nl,
      write('%      N (>= 1) clp(n) iterations before widening'),nl,
      write('%  -p F'),nl,
      write('%      read the predefined predicates in file F before analysis'),nl,
%      write('  -q'),nl,
%      write('      stick to the clp(q) library everywhere'),nl,
      write('%  -r'),nl,
      write('%      for a recursive analysis of included and ensure_loaded files'),nl,
      write('%  -t T'),nl,
      write('%      set the timeout to T (>= 100) ms'),nl,
      write(error),write('.'),nl.


process_args([F],iso) :-
	file_exists(F),
	!,
	write('% File: '),write(F),nl,
	banner,
	catch(file_termconds(F,L),_,(write('% an error occured'),nl)),
	numbervars(L,0,_),
	write(L),write('.'),
	nl.
process_args([F],with_predef) :-
	file_exists(F),
	!,
	write('% File: '),write(F),nl,
	banner,
	catch(file_expand_termconds(F,L),_,(write('% an error occured'),nl)),
	numbervars(L,0,_),
	write(L),write('.'),
	nl.
process_args(['-t',T|Args],Mode) :-
	!,
	name(T,L),name(N,L),
	set_cti_flag(time_out,N),
	process_args(Args,Mode).
process_args(['-n',N|Args],Mode) :-
	!,
	name(N,L),name(N1,L),
	set_cti_flag(nb_ite_clpn,N1),
	process_args(Args,Mode).
process_args(['-c'|Args],Mode) :-
	!,
	set_cti_flag(compare_ppl_clpq,on),
	process_args(Args,Mode).
process_args(['-r'|Args],Mode) :-
	!,
	set_cti_flag(process_include_ensure_loaded,on),
	process_args(Args,Mode).
%process_args(['-q'|Args],Mode) :-
%	!,
%	set_cti_flag(poly_lib,poly_clpq),
%	process_args(Args,Mode).
process_args(['-p',F|Args],_Mode) :-
	!,
	file_exists(F),
	main:include_predef(F),
	process_args(Args,with_predef).



banner :-
	current_cti_flag(cTI_version,Vers),
	current_cti_flag(poly_lib,Lib),
	current_cti_flag(time_out,T),
	current_cti_flag(nb_ite_clpn,N),
	write('% cTI version '),write(Vers),
	write(', '),write(Lib),
	write(', '),write(T),
	write('ms, '),write(N),write(' ite'),
	nl.
