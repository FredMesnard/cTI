:- module(main,[
        current_cti_flag/2,
		set_cti_flag/2,
		program_termination_conditions/2,
		file_termconds/2,
		file_expand_termconds/2]).

:- dynamic(current_cti_flag/2).

%----------- libraries --------------------------------------------------------
% all libraries we need are referenced below:
%------------------------------------------------------------------------------
:- use_module(library(ugraphs)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(terms)).
:- use_module(library(clpb)).
:- use_module(library(clpq)).
:- use_module(library(time)).
:- use_module(library(system)).
%----------- cTI files ----------------------------------------------------------------
% all files we need are referenced below:
%--------------------------------------------------------------------------------------
:- use_module(predef).		                % ISO-Prolog predefs
:- use_module(utils).       		        % utilities
:- use_module(db).                          % db management
:- use_module(cti_term_size).               % term_size definition
:- use_module(compat_swi).                  % added definitions for the switch SP -> SWI
:- use_module(cti_time).                    % timings utilities
:- use_module(clpq_clpn).                   % clp(Q) constraints to clp(N) constraints
%--------------------------------------------------------------------------------------
:- use_module(prolog_flatprolog).           % ISO-Prolog to FlatPrologh
:- use_module(prolog_clph).                 % FlatProlog to clp(H)
:- use_module(prolog_trad_cd).              % cTI directives 
%---------------------------------------------------------------------------------------
:- use_module(num_itp).                     % fix-point poly_ppl/poly_clpq
:- use_module(num_clph_clpn).	            % clp(H) to clp(N)
:- use_module(num_prolog_clpn).	            % ISO-Prolog to clp(N)
:- use_module(num_op,[]).                   % meta ops for clp(Q)
:- use_module(num_dual).                    % dual
:- use_module(num_cmc).                     % compute matrix/column
:- use_module(num_constr_lm).               % compute all level mappings
:- use_module(num_concrete_lm).             % generate max level mappings
%---------------------------------------------------------------------------------------
:- use_module(add_model_al).                % adding prop (N or B) to prog 
%---------------------------------------------------------------------------------------
:- use_module(bool_itp).                    % fix-point clp(B)
:- use_module(bool_op,[]).    		        % meta ops for clp(B)
:- use_module(bool_nat_bool).               % clp(N) to clp(B)
:- use_module(bool_lm_nat_bool).            % lm max clp(N) to  lm bool
:- use_module(bool_dc).                     % transformation Bool -> FPos
:- use_module(bool_term_cond).	    	    % compute termination conditions
%---------------------------------------------------------------------------------------
:- use_module(quantify).                    % quantify precision for termination analysis
%---------------------------------------------------------------------------------------
:- include(flag).                           % current_cti_flag/2 and set_cti_flag/2
:- include(file_termconds).                 % reading files and processing directives
%---------------------------------------------------------------------------------------
:- use_module(poly_clpq).                   % bridge for clp(Q)/PPL, with Roberto Bagnara
%---------------------------------------------------------------------------------------


program_termination_conditions(ListClausesProlog,ListTermConds) :-
    %write_list(ListClausesProlog),
	stat_init(Stat0),
    prologs_clpns(ListClausesProlog,ListClausesClpN),
    %write_list(ListClausesClpN),
	add_stat(Stat0,isopl_clpn,Stat1),
	current_cti_flag(nb_ite_clpn,NbIteCLPN),
	current_cti_flag(poly_lib,PolyLib), 
    current_cti_flag(time_out,TimeOut),
	Options=[nb=NbIteCLPN,poly=PolyLib,to=TimeOut],
	clpn_tc_(ListClausesClpN,Options,ListTermConds0,UP,_RN,_LM,_RB,Stat1,Stat2),
	(current_cti_flag(print_info,yes) -> stat_final(Stat2) ; true),
	sort(ListTermConds0,ListTermConds),
	(current_cti_flag(print_info,yes) -> quantify(ListTermConds) ; true),
	(current_cti_flag(print_info,yes) -> (write('% Undefined predicates assumed to fail:'),writeln(UP)) ; true).


clpn_tc_(ListClausesClpN,Options,ListTermConds,UndefPreds,RelsInterArgsN,LevelMappings,RelsInterArgsBool,Stat1,Stat) :-
	Options=[nb=NbIteCLPN,poly=PolyLib,to=TimeOut],
	call_graph(ListClausesClpN,CallGraph),
	graph_sccs(CallGraph,Sccs), 
    %write(CallGraph),nl,write(Sccs),nl,
	length(Sccs,LengthSccs),
    (current_cti_flag(print_info,yes) -> (write('% |Sccs|='),writeln(LengthSccs)) ; true),
	
	empty_db(Db0),
	add_cls_domain_dbs(ListClausesClpN,clpn,Db0,Db1),
	cg_db_domain_undefpred(CallGraph,Db1,clpn,UndefPreds),
    %(UndefPreds==[] -> true; write_list(UndefPreds),nl),
	sccs_db_domain_sccscls(Sccs,Db1,clpn,SccsClsN), 
    %
    %write_list(SccsClsN),
	
	(current_cti_flag(print_info,yes) -> writeln('% n-model ...'); true),
	ntp(SccsClsN,PolyLib,NbIteCLPN,BaseN,[star-widening],TimeOut),
    !,
	add_stat(Stat1,model_clpn,Stat2),
	assoc_to_list(BaseN,RelsInterArgsN), 
    %
    %write_list(RelsInterArgsN),
	add_prop_domain_dbs(RelsInterArgsN,modeln,Db1,Db1bis), 
    %print_db(Db1bis),

	add_model_after_litt(SccsClsN,Db1bis,modeln,SccsClsNSpec,TimeOut), 
    %
    %write_list_list(SccsClsNSpec),
    sccs_list(SccsClsNSpec,ClsNSpec),
	add_cls_domain_dbs(ClsNSpec,clpn_with_model,Db1bis,Db2),
    %print_db(Db2),

	(current_cti_flag(print_info,yes) -> writeln('% constr-lm ...') ; true),
    constr_lm(SccsClsNSpec,ConsLevelMappings,TimeOut), 
    %
    %write_list(ConsLevelMappings),
	add_stat(Stat2,constr__lm,Stat3),
	(current_cti_flag(print_info,yes) -> writeln('% concr-lm ...') ; true),
	concrete_lm(ConsLevelMappings,LevelMappings,TimeOut),
    %
    %write_list(LevelMappings),
	add_stat(Stat3,concret_lm,Stat4),
	lm_nat_bool(LevelMappings,BoolLevelMappings),
    %
    %write_list(BoolLevelMappings),
	add_prop_domain_dbs(BoolLevelMappings,blm,Db2,Db3),

	%nat_bool(SccsClsN,SccsClsB), 
	nat_bool(SccsClsNSpec,SccsClsB), 
    %
    %nl,write_list(SccsClsNSpec),nl,write_list(SccsClsB),
    %
	add_stat(Stat4,pgnat_bool,Stat5),
    sccs_list(SccsClsB,ClsB),
	add_cls_domain_dbs(ClsB,clpb,Db3,Db3bis), %print_db(Db3bis),
	(current_cti_flag(print_info,yes) -> writeln('% b-model ...') ; true),
	tp(SccsClsB,bool_op,no_widening,BaseBool,[star-union],TimeOut),
    !,
	add_stat(Stat5,model_bool,Stat6),
	assoc_to_list(BaseBool,RelsInterArgsBool), 
    %
    %write_list(RelsInterArgsBool),
	add_prop_domain_dbs(RelsInterArgsBool,modelb,Db3bis,Db4),  %print_db(Db4),
    %
	add_model_after_litt(SccsClsB,Db4,modelb,SccsClsBSpec,TimeOut), 
    %
    %write_list_list(SccsClsBSpec),
    sccs_list(SccsClsBSpec,ClsBSpec),
	add_cls_domain_dbs(ClsBSpec,clpb_with_model,Db4,Db4bis),
    %print_db(Db4bis),
    
 	(current_cti_flag(print_info,yes) -> writeln('% term-cond ...') ; true),
	term_cond(SccsClsBSpec,Db4bis,Db5,TimeOut),%print_db(Db5),
    !,
	add_stat(Stat6,term_conds,Stat),
    %ListTermConds=[].
	get_sccs_domain_db_props(Sccs,pos_termcond,Db5,ListTermConds).
    
    
    