
:- op(1150, fx, dynamic).

absolute_file_name(X,Y,Z,W) :- '$top'(X,Y,Z,W).
absolute_file_name(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
absolute_file_name(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).

gensym_counter(X,Y) :- '$top'(X,Y).

phrase(X,Y,Z) :- '$top'(X,Y,Z).
lgi_expand_file_names(X,Y,Z) :- '$top'(X,Y,Z).
lg_control_files(X) :- '$top'(X).
lgi_description_files(X,Y) :- '$top'(X,Y).
lg_description_files(X) :- '$top'(X).
grammar_files(X,Y) :- '$top'(X,Y).
control_files(X,Y) :- '$top'(X,Y).
collect_text_tree(X,Y) :- '$top'(X,Y).
lgi_test_file_prefix(X) :- '$top'(X).
lg_module(X,Y,Z,W) :- '$top'(X,Y,Z,W).
abbreviated_name(X,Y) :- '$top'(X,Y).
lgi_tcl_output(X,Y,Z) :- '$top'(X,Y,Z).
visit_text_item(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
visit_text_item(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
visit_text_item(X,Y,Z,W,V,U,T) :- '$top'(X,Y,Z,W,V,U,T).
mk_text_item_fs(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
mk_text_item_fs(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
input_text(X,Y) :- '$top'(X,Y).
display_string(X) :- '$top'(X).
displ(X) :- '$top'(X).
'POLY'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
resolve(X,Y,Z,W,V,U,T,S,R) :- '$top'(X,Y,Z,W,V,U,T,S,R).
print_head(X,Y,Z,W) :- '$top'(X,Y,Z,W).
print_body(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
current_spypoint(X) :- '$top'(X).
folder(X,Y,Z) :- '$top'(X,Y,Z).
search_tree_link(X,Y,Z) :- '$top'(X,Y,Z).
foreign_result(X,Y,Z,W) :- '$top'(X,Y,Z,W).
ieSendTraceMsg(X) :- '$top'(X).
restrict(X,Y) :- '$top'(X,Y).
environ(X,Y) :- '$top'(X,Y).
cuf_env(X,Y) :- '$top'(X,Y).
cuf_not_eq(X,Y) :- '$top'(X,Y).
cuf_dbg_window_selection(X,Y,Z) :- '$top'(X,Y,Z).
initialization.

%%RB:
% cuf:'PARSER OUTPUT' changed to 'PARSER OUTPUT'
%%%%%%%%%%%%%%%%%%%%%%%

%%RB: dynamic predicates
%%
'LGflag'(X,Y) :- '$top'(X,Y).
'LGcounter'(X) :- '$top'(X).
ci(X,Y) :- '$top'(X,Y).
gt0(X,Y) :- '$top'(X,Y).
gt(X,Y) :- '$top'(X,Y).
ht0(X,Y,Z,W,V,U,T) :- '$top'(X,Y,Z,W,V,U,T).
hpt0(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
hpt(X,Y) :- '$top'(X,Y).
lgi_input_string(X,Y) :- '$top'(X,Y).
'LGl0'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'LGhl0'(X,Y,Z) :- '$top'(X,Y,Z).
'LGhl'(X,Y) :- '$top'(X,Y).
'LGhl'(X,Y,Z) :- '$top'(X,Y,Z).
'LGl'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'LGl'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'LGhc'(X,Y) :- '$top'(X,Y).
'LGhc'(X,Y,Z) :- '$top'(X,Y,Z).
'LGg'(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
'hl'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'hc'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
l(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'LGnt'(X) :- '$top'(X).
'LGusednt0'(X,Y) :- '$top'(X,Y).
'LGusednt'(X,Y) :- '$top'(X,Y).
'LGsid'(X) :- '$top'(X).
ci(X,Y) :- '$top'(X,Y).
gt0(X,Y) :- '$top'(X,Y).
gt(X,Y) :- '$top'(X,Y).
'hpt0'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
hpt(X,Y) :- '$top'(X,Y).
lgi_input_string(X,Y) :- '$top'(X,Y).
'LGl0'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'LGhl0'(X,Y,Z) :- '$top'(X,Y,Z).
'LGhl'(X,Y) :- '$top'(X,Y).
'LGhl'(X,Y,Z) :- '$top'(X,Y,Z).
'LGl'(X,Y,Z) :- '$top'(X,Y,Z).
'LGl'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'LGhc'(X,Y) :- '$top'(X,Y).
'LGhc'(X,Y,Z) :- '$top'(X,Y,Z).
'LGg'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'hl'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'hc'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
l(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
'LGnt'(X) :- '$top'(X).
'LGusednt0'(X,Y) :- '$top'(X,Y).
'LGusednt'(X,Y) :- '$top'(X,Y).
cuf_debugging(X) :- '$top'(X).


feat_decl(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
sort_decl(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
file_table(X,Y,Z) :- '$top'(X,Y,Z).
symbol_table(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
type_code(X,Y) :- '$top'(X,Y).
'LENGTH'(X,Y) :- '$top'(X,Y).
'CUF_FLAG'(X,Y) :- '$top'(X,Y).
concat_string(X,Y,Z) :- '$top'(X,Y,Z).
cuf_tables_initialised.

index_table(X,Y,Z) :- '$top'(X,Y,Z).
cuf_foreign(X,Y,Z) :- '$top'(X,Y,Z).
cuf_task(X,Y) :- '$top'(X,Y).
'FOLDER DEF'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
mono_feat(X,Y,Z) :- '$top'(X,Y,Z).

'A LENGTH'(X,Y) :- '$top'(X,Y).
'index_table_name'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
poly_feat(X,Y) :- '$top'(X,Y).
feat_doms(X,Y) :- '$top'(X,Y).

cuf_grammar_directory(X) :- '$top'(X).

cuf_trace_filter_pred(X,Y) :- '$top'(X,Y).
cuf_trace_depth(X,Y) :- '$top'(X,Y).

current_grammar(X) :- '$top'(X).

resolve_port(X,Y,Z,W,V,U,T) :- '$top'(X,Y,Z,W,V,U,T).
resolve_port_parent_access(X,Y) :- '$top'(X,Y).
'success_node'(X,Y,Z,W) :- '$top'(X,Y,Z,W).

'CONCAT STRING'(X,Y,Z) :- '$top'(X,Y,Z).

'cuf_top_level_goal'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
goal(X,Y) :- '$top'(X,Y).
goal_active(X,Y) :- '$top'(X,Y).
goal_passive(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
active(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
earley_action(X) :- '$top'(X).
restriction(X) :- '$top'(X).
sat_debug.

'STRING_CONST'(X) :- '$top'(X).
'TYPE CONST'(X,Y,Z) :- '$top'(X,Y,Z).
'DEF OCC'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'PREFIXOP'(X,Y,Z,W) :- '$top'(X,Y,Z,W).

%%RB: dynamic predicates, even if not declared as such
%%

'AMBIGOP'(X,Y,Z,W,V,U,T,S) :- '$top'(X,Y,Z,W,V,U,T,S).
'INFIXOP'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'POSTFIXOP'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'CHECK FORMULA'(X,Y,Z) :- '$top'(X,Y,Z).
'CHECK FORMULAS'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'TYPE SUBSUMES'(X,Y,Z,W) :- '$top'(X,Y,Z,W).
'ATOM DOMAINS'(X) :- '$top'(X).
'VOLATILE_TYPE_CODE'(X,Y) :- '$top'(X,Y).
'CUF_ID'(X,Y) :- '$top'(X,Y).
'USED OCC'(X,Y,Z,W,V) :- '$top'(X,Y,Z,W,V).
'CUF clause'(X,Y,Z,W,V,U,T,S,R) :- '$top'(X,Y,Z,W,V,U,T,S,R).
added_axiom(X,Y,Z) :- '$top'(X,Y,Z).
cuf_demo_directory(X) :- '$top'(X).
nthsolution(X) :- '$top'(X).

% Not called

'SYS_OP'(X,Y,Z) :- '$top'(X,Y,Z).
start_incremental_proof(X) :- '$top'(X).
incremental_nthsolution(X) :- '$top'(X).

%%RB: unknown predicates
%%
tree_display_dtr_pred(X,Y) :- '$top'(X,Y).
'COST PREDS'(X,Y,Z) :- '$top'(X,Y,Z).
'MEMO GOAL'(X,Y) :- '$top'(X,Y).
'LAZY GOAL'(X,Y) :- '$top'(X,Y).
'CHECK TRIGGER'(X) :- '$top'(X).
wait_condition(X) :- '$top'(X).
attribute_order(X) :- '$top'(X).
invisible_attributes(X) :- '$top'(X).
delay_pattern(X,Y,Z) :- '$top'(X,Y,Z).

%%RB: foreign predicates
%%
check_n_close_axioms_0(MTerm) :- '$top'(MTerm).
classify_atom(PInteger1,PInteger2) :- '$top'(PInteger1,PInteger2).
unclassify_atom(PInteger) :- integer(PInteger).
set_sat_trace(PInteger) :- integer(PInteger).
set_sat_debug(PInteger) :- integer(PInteger).
init_sat0.
assert_atom_domain(PInteger) :- integer(PInteger).
check_formula_0(PTerm,MInteger) :- '$top'(PTerm), integer(MInteger).
add_axioms_0(PTerm,MInteger) :- '$top'(PTerm), integer(MInteger).
reset_counter.
incr_counter(MInteger) :- integer(MInteger).
get_counter(MInteger) :- integer(MInteger).
cuf_mktemp(PString, MString) :- atom(PString), atom(MString).
cuf_file_mtime(PString, MInteger) :- atom(PString), integer(MInteger).
cuf_newer_file(PString1, PString2, MInteger) :- atom(PString1), atom(PString2), integer(MInteger).
cuf_file_exists(PString, MInteger) :- atom(PString), integer(MInteger).
cuf_type_term_hash(PTerm,MInteger) :- '$top'(PTerm), integer(MInteger).
cuf_type_term_hash2(PTerm1,PTerm2,MInteger) :- '$top'(PTerm1), '$top'(PTerm2), integer(MInteger).

%%RB: END
%%%      File: lgcommon.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: load file for the common kernel of the           %%%
%%%            LexGram interpreters                             %%%
%%%   Created:                                                  %%%
%%%  Modified: Thu Oct 17 11:51:53 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgcommon_files/1
%% Imports:
%%   lgi_expand_file_names/3

lgcommon_files( 
      [
       category,      % category data type
       daccessT       % special types for grammar interpreters
      |R ], R ) :-
   lgi_expand_file_names(
      [
       msgs,               % LexGram messages
       utils,              % aux. predicates
       daccess,            % aux. predicates/parser
       lgi_uif,            % auxiliary routines for user interface 
       cuf_if              % CUF interface - general stuff
      ],
      'lg-sys/common/',     % directory relative to LexGram load file
      ExpandedFileNames),
    ensure_loaded(ExpandedFileNames).


%% --- END OF FILE: lgcommon.pl
%%%      File: daccess.pl                                       %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: data-access sorts for the grammar interpreters   %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Jul 12 13:48:10 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_leftcorner_candidate/4 % leftcorner_candidate/4
%%   lgi_mk_tree/6           
%% Imports:

%% Remarks:
%% - Prolog version of dacess.cuf
%% - for more comments see daccess.cuf
%% - coordinate changes of predicates with their counterparts in daccess.cuf
%%   (because generator and parser share data structure)  !!!    


%% lgi_leftcorner_candidate(+ArgDir,+LCCandIn,+ArgLeftCorner,
%%                          -NewLeftCornerCand)
%% - calculate the new leftcorner candidate
%%
lgi_leftcorner_candidate(left,_LCCandIn,ArgLC,ArgLC).

lgi_leftcorner_candidate(right,LCCandIn,_ArgLC,LCCandIn).




lgi_mk_tree(
   left,RootCat,RootId,HeadTree,ArgTree,
   node(RootCat,RootId,[ArgTree,HeadTree]) ).

lgi_mk_tree(
   right,RootCat,RootId,HeadTree,ArgTree,
   node(RootCat,RootId,[HeadTree,ArgTree]) ).


%% --- END OF FILE: daccess.pl
%%%      File: lgi_uif.pl                                       %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: common user interface of LexGram interpreters    %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Nov  8 16:53:50 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgflags/0
%%   lgflag/2
%%   lgi_file_name/3
%%   lgi_mk_cnf_file/3
%%   lgi_print_runtime/0
%%   lgi_print_total_runtime/0
%%   lgi_reset_runtime/0
%%   lgload/0,/1
%%   show_nt_hierarchy/0
%%   show_tree/1
%% Imports:
%%   concat/3       (CUF system/Quintus-built-in)
%%   cuf_mktemp/2           % CUF system
%%   lg_control_files/1  (local grammar)
%%   lg_description_files/1  (local grammar)
%%   lgi_compile_lexicon/0
%%   lgi_collect_nt_hierarchy/1
%%   lgi_collect_tree/2
%%   lgi_description_files/1  (local LexGram interpreter)
%%   lgi_test_file_prefix/1  (local grammar)
%%
:- dynamic('LGflag'/2).


lgi_header :- 
nl,
write('**************************************************************'),nl,
write('*                                                            *'),nl,
write('*                    LexGram 0.9.3beta       (November 1996) *'),nl,
write('*                                                            *'),nl,
write('*       Joerg Junger, Esther Koenig, Peter Krause            *'),nl,
write('* (C) Institut fuer maschinelle Sprachverarbeitung (IMS)     *'),nl,
write('* Universitaet Stuttgart,Germany,esther@ims.uni-stuttgart.de *'),nl,
write('**************************************************************'),nl,
nl,
nl.



%% ----------------------------------------------------------
%%   flag handling (adapted from CUF)
%% ----------------------------------------------------------

%% lgflag(+Name, +Value)
%% - lgflag value setting
%%
lgflag(Name, Value) :-
   lgi_set_flag(Name,Value)
   -> lgi_tell_flag(Name)
   ;  ( cuf_out(
           message,
           ['LexGram: illegal flag/value combination: ',Name,':',Value] ),
        ( % EITHER a legal name
          lgi_tell_flag(Name)    
          -> true
          ;  % OR enumerate all flag/value lists
             lgi_tell_flag(_) ),  % 
        fail ).


%% lgi_flags/0
%% - shows all flag settings
lgflags :-
   setof( Flag, lgi_get_flag(Flag), Flags ), % make alphabetical order
   member( Flag1, Flags),
   lgi_tell_flag(Flag1),
   fail.
lgflags.


%% lgi_set_flag(?Flag,?Value)
%% - set Value of Flag
%% - REMARK: relational! 
%%   for uninstantiated values, the valid flag values will be taken
%% - failure for nonappriate Flag/Value combinations
%%
lgi_set_flag(Name,Value) :-
   lgi_valid_flag_value(Name,Value,Action),
   retractall('LGflag'(Name, _)),
   assert('LGflag'(Name, Value)),
   call(Action).

   
%% lgi_get_flag(+Flag, ?Value)
%% - get Value of Flag
%% 
lgi_get_flag(Name,Value) :-
   'LGflag'(Name,Value).

lgi_get_flag(Flag) :-
   lgi_get_flag(Flag,_Value).


%% lgi_tell_flag(?Flag)
%% - list current value and value range for Flag
%%
lgi_tell_flag(Flag) :-
   lgi_get_flag(Flag,Value),  % assumes that all flags are properly set
   bagof( Value0, 
          ( lgi_valid_flag_value(Flag,Value0,_Action), 
            ( \+ Value0 = Value ) ),
          Values ),
   cuf_out( 
      message,   
      [ 'LexGram flag:',Flag,'-->',Value, Values ] ).



%% lgi_valid_flag_value(?Flag,?LegalValue,?Action)
%% - list of LexGram flags and their values and actions to be
%%   taken when flag is set to a specific value
%% 
% which chart should be accessible?
lgi_valid_flag_value(chart,sentence,true). % sentence level chart
lgi_valid_flag_value(chart,text,true).     % text level chart
% how to print result structures?
lgi_valid_flag_value(tcl_output,no,true).  % usual CUF output
lgi_valid_flag_value(tcl_output,yes,true). % 'intelligent' Tcl/Tk output 


% carry out default settings
:- lgi_set_flag(chart,sentence),
   lgi_set_flag(tcl_output,no).



%% ----------------------------------------------------------
%%   grammar loading
%% ----------------------------------------------------------


%% lgload/0, /1
%% - LexGram specific gload/0, /1 versions
%%
lgload :-
   gload,
   ( lg_control_files(GrammarControlFiles)
     -> lgi_cloadS(GrammarControlFiles)
     ;  true ),
   lgi_compile_lexicon.


lgload(GrammarName) :-
   lgi_mk_cnf_file(GrammarName,interpreter,CNFFileName),
   load_cnf(CNFFileName),  % load the configuration
   gload(GrammarName),
   lgi_compile_lexicon.


%% load a series of files
lgi_cloadS([]).

lgi_cloadS([File|Files]) :-
   cload(File),
   lgi_cloadS(Files).


%% lgi_mk_cnf_file(+UserModuleName,+InterpreterModuleName,ConfigFileName)
%% - this is a dirty hack in order to outwit the CUF compiler
%%   to load both the LexGram type definitions and the user specific stuff
%% - InterpreterModuleName :== interpreter
%% - GrammarName = UserModuleName
%% - the configuration file is written on the  /tmp  directory
%%
lgi_mk_cnf_file(UserModuleName,InterpreterName,File) :-  
   lgi_description_files(InterpreterName,InterpreterFiles),
   ( current_predicate(lg_description_files,_P1)
     -> lg_description_files(UserDescrFiles)
     ; 
     ( cuf_out(warning,
         ['lg_description_files/1',declaration,is,missing,'$NL$','$NL$']),
       ( UserDescrFiles = [] ) ) ),
   ( current_predicate( lg_control_files, _P2 )
     -> lg_control_files(UserContrFiles)
     ; ( UserContrFiles = [] ) ),
   append(InterpreterFiles,UserDescrFiles,Files),
   cuf_mktemp('/tmp/lgXXXXXX',File),  % create a new file name
   concat(File,'.cnf',ExtName),
   tell(ExtName),
   write( grammar_files(UserModuleName,Files) ), put(46), nl,
   write( control_files(UserModuleName,UserContrFiles) ), put(46), nl,
   told.


%% ----------------------------------------------------------
%%   nonterminal hierarchy inspection
%% ----------------------------------------------------------

%% show_nt_hierarchy/0
%% - works only after a parse has been carried out
%%
show_nt_hierarchy :-
   lgi_collect_nt_hierarchy(Tree),
   lgi_pptree(Tree).


%% lgi_pptree( Tree )
%% - general term pretty printer

lgi_pptree( Tree ) :-
   lgi_pptree( Tree, 0 ).


lgi_pptree( Tree, Tab ) :-  
   ( atomic( Tree )
     -> lgi_tab_and_write( Tab, Tree )
   ) ; 
   ( ( Tree =.. [Functor|Trees] )
     -> ( lgi_tab_and_write( Tab, Functor ),
          Tab1 is Tab + 3,
          lgi_pptrees( Trees, Tab1 ) ) ).

lgi_pptrees( Trees, Tab ) :-
   ( Trees = [Tree|Trees1] ) 
   -> ( lgi_pptree(Tree,Tab),
        lgi_pptrees(Trees1,Tab) )
   ; ( Trees = [] ).


%% ----------------------------------------------------------
%%   chart inspection
%% ----------------------------------------------------------


%% show_tree(ItemId)
%% - collect and pretty print the tree for an item ItemId
%%
show_tree(ItemId) :-
   lgi_get_flag(chart,Value),
   ( ( Value = sentence )
     -> lgi_collect_tree(ItemId,Tree)
     ;  ( ( Value = text )
          -> collect_text_tree(ItemId,Tree) ) ),
   lgi_print_tree(Tree,0).


%% lgi_print_tree(+Tree,+Tab)
%% - special tree pretty printer for syntax trees extracted from the chart
%%
lgi_print_tree( 
      node(CatName,ItemId,Dtrs),
      Tab ) :-
   lgi_tab_and_write( Tab, CatName : ItemId ),
   Tab1 is Tab + 3,
   lgi_print_trees(Dtrs,Tab1).

lgi_print_tree( 
      leaf(LeafName),
      Tab ) :-
   lgi_tab_and_write( Tab, LeafName ).


lgi_print_trees([Tree|Trees],Tab) :-
   lgi_print_tree(Tree,Tab),
   lgi_print_trees(Trees,Tab).

lgi_print_trees([],_Tab).



%% ----------------------------------------------------------
%%   aux. stuff
%% ----------------------------------------------------------

lgi_tab_and_write( Tab, Thing ) :-
   nl, tab( Tab ),
   write( Thing ).



%%
%%  lgi_file_name(+TestNumber,+ReadingNumber,-FileName)
%%  -  create file name according to Prefix, TestNumber, ReadingNumber
%%  - causes a Prolog error message if lgi_test_file_prefix is not defined

lgi_file_name(StringNumber,ReadingNumber,FileName) :-
   lgi_test_file_prefix(Prefix),
   concat('_',ReadingNumber,Postfix),
   concat(Prefix,StringNumber,Prefix1), 
   % inefficient, but first argument of concat cannot be a number - why not?
   concat(Prefix1,Postfix,FileName).


%% --- END OF FILE: lgi_uif.pl 



%%%      File: utils.pl                                         %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: auxiliary predicates                             %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Oct 15 15:47:32 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_in_interval_downward/3
%%   lgi_in_interval/3
%%   lgi_get_counter/2
%%   lgi_inc_counter/2
%%   lgi_set_counter/2
%%   lgi_unify/3
%% Imports:
%%     append/3               % CUF system / Prolog
%%     cuf_convert_type_name/2  % CUF ADT
%%     cuf_prove/6            % CUF ADT
%%     cuf_unify/2            % CUF ADT
:- dynamic('LGcounter'/1).


%% ----------------------------------------------------------
%%   counters (adapted from   CUF utils.pl )
%% ----------------------------------------------------------

%% - one entry per counter Name !
%% - fails if Name is not an atom and Value is not an integer

%% lgi_get_counter(+Name, Value)
%% - get value of global counter Name
%%
lgi_get_counter(Name, Value) :-
    atom(Name),
    clause('LGcounter'(Name, Value), _ ).

%% lgi_inc_counter(+Name, Value)
%% - return counter value Value and increment counter Name by Increment
%%
lgi_inc_counter(Name,ValueNew) :-
    atom(Name),
    retract('LGcounter'(Name,Value)),
    ValueNew is Value + 1,
    assert('LGcounter'(Name,ValueNew)).

%% lgi_set_counter(+Name, +Value)
%% - set global counter Name to value Value
%%
lgi_set_counter(Name, Value) :-
    atom(Name),
    integer(Value),
    retractall('LGcounter'(Name, _)), 
    assert('LGcounter'(Name, Value)).


%% ----------------------------------------------------------
%%   cuf_prove with failure message
%% ----------------------------------------------------------

lgi_cuf_prove(ProveFlag,GoalSIn,GoalSOut,ConstrSIn,ConstrSOut,CL) :-
   cuf_prove(ProveFlag,GoalSIn,GoalSOut,ConstrSIn,ConstrSOut,CL).

lgi_cuf_prove(_ProveFlag,GoalS,GoalS,ConstrS,ConstrS,_CL) :-
   cuf_out(
      message,
      [no,'(',more,')','CUF',proof,for,'$GOAL LIST$'(GoalS,ConstrS)] ),
   !,
   fail.

%% lgi_cuf_clause(+ClauseHeadExt,ResultFS,-GoalS,-ConstraintS)
%% - similar to Prolog clause/2
%%
%lgi_cuf_clause(ClauseHeadExt,ResultFS,GoalS,ConstraintS) :-
%  ( current_predicate( 'CUF clause', _ ),
%     nonvar(ClauseHeadExt) )
%   -> ( ClauseHeadExt =.. [Functor|ArgS],
%        ClauseHeadInt =.. [Functor,ResultFS|ArgS],
%       cuf:'CUF clause'(ClauseHeadInt,_FileId,_EntryId,_ClauseId,_NodeId,
%            GoalS,[],ConstraintS,[]) ). % 'unifies' will be carried otu


%% ----------------------------------------------------------
%%   reduction of CUF descriptions
%% ----------------------------------------------------------

%% lgi_unify(Description1,Description2,UnifiedDescription)
%% - Description:  d(FeatureTerm,GoalS,Constraints)
%% - do the whole constraint resolution for a CUF description,
%%   not just plain term unification
%% - UnifiedDescription: residuated description
%%
lgi_unify(
      d(Term1,GoalS1,ConstrS1),
      d(Term2,GoalS2,ConstrS2),
      d(Term1,GoalSOut,ConstrSOut) ) :-
   cuf_unify(Term1,Term2),   % term unification
   append(GoalS1,GoalS2,GoalSIn),
   append(ConstrS1,ConstrS2,ConstrSIn),
   % wake up goals, check constraints
   cuf_prove(undelayed_only,GoalSIn,GoalSOut,ConstrSIn,ConstrSOut,_ClNumList).


%% ----------------------------------------------------------
%%   intervals
%% ----------------------------------------------------------

%% lgi_in_interval_downward(?Element,+From,+To).
%% - pick an Element from the closed interval [From,To]
%% - or check whether Element in interval [From,To]
%% - searches from To to From
%%
lgi_in_interval_downward(Element,_From,Element).

lgi_in_interval_downward(Element,From,To) :-
   From < To,
   To1 is To - 1,
   lgi_in_interval_downward(Element,From,To1).




%% lgi_in_interval(?Element,+From,+To).
%% - pick an Element from the closed interval [From,To]
%% - or check whether Element in interval [From,To]
%% - searches from From to To
%% - no wellformed-check for the interval itself, i.e.
%%   nonsensical behavior for cases like: lgi_in_interval(1,1,0).

lgi_in_interval(Element,Element,_To).

lgi_in_interval(Element,From,To) :-
   From < To,
   From1 is From + 1,
   lgi_in_interval(Element,From1,To).



%% --- END OF FILE: utils.pl
%%%      File: cuf_if.pl                                        %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: interface to the CUF system                      %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Apr 30 15:48:51 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   cuf_if_cons_list/3
%%   cuf_if_get_callers/2
%%   cuf_if_get_subgoals/3
%%   cuf_if_is_disjunction_name/1
%%   cuf_if_lookup_featdecl/2
%%   cuf_if_lookup_type_kids/2
%% Imports:
%%   'CUF clause'/9          % CUF compiler output
%%   'PARSER OUTPUT'/4       % CUF compiler output
%%   'USED OCC'/5            % CUF compiler output
%%   cuf_convert_type_name/2 % CUF ADT
%%   cuf_put_type/2          % CUF ADT
%%   cuf_put_path/5          % CUF ADT
%%   length/2                % CUF
%%   member/2                % CUF
%%   Types:
%%     list                  % CUF built in

    
%% ----------------------------------------------------------
%%   interface to built-in CUF types
%% ----------------------------------------------------------

%% cuf_if_cons_list( First, Rest, List )
%% - construct or destruct a list
%%
cuf_if_cons_list(First,Rest,List) :-
   cuf_convert_type_name( TypeInt, [[nelist]] ),
   cuf_put_type(TypeInt,List),
   cuf_put_path(List,['F'],First),
   cuf_put_path(List,['R'],Rest).


%% ----------------------------------------------------------
%%   interface to CUF's parser output
%% ----------------------------------------------------------

%% cuf_if_is_disjunction_name(+SortName)
%% - test for 'genDisj' as a prefix of SortName
cuf_if_is_disjunction_name(SortName) :-
   ( name(SortName,SORTNAME),
     % name(genDisj,[103,101,110,68,105,115,106]),
     append([103,101,110,68,105,115,106],_,SORTNAME) )
   -> true.  % no backtracking necessary


%% cuf_if_get_callers(+Sort/Arity,-CallingSortS)
%% - find out all CallingSortS which call Sort/Arity
%% - !!! displays also internal sorts (disjunction names)
%%
cuf_if_get_callers(Sort,CallingSortS) :-
   setof( CallingSort,
          cuf_if_get_caller(Sort,CallingSort),
          CallingSortS )
   ; ( ( \+ cuf_if_get_caller(Sort,CallingSort) )
       -> ( CallingSortS = [] ) ).



cuf_if_get_caller(Sort/Arity,CallerSort/CallerArity) :-
   clause( cuf:'USED OCC'(Sort,Arity,sort,FileId,EntryId), _B1 ),
   clause( cuf:'CUF clause'(CallerSortHead1,FileId,EntryId,_ClauseId,_NodeId,
                _SubGoalS,_R1,_ConstrS,_R2), _B2 ),
   ( ( CallerSortHead1 =.. [CallerSort|ArgS],
       length(ArgS,IntCallerArity),
       CallerArity is IntCallerArity - 1 )
     -> true ).  % avoid backtracking



%% cuf_if_get_subgoals(SortAri,SubGoalS)
%% 
cuf_if_get_subgoals(SortName/Arity,SubGoalS) :-
   Arity1 is Arity + 1,
   length(Args,Arity1),
   ( IntGoal1 =.. [ SortName | Args ] ),
   clause( cuf:'CUF clause'(IntGoal1,_FId,_EId,_ClauseId,_NodeId,
                IntSubGoalS,_R1,_ConstrS,_R2), _B2 ),
   cuf_if_mk_sort_skeletons(IntSubGoalS,SubGoalS).


cuf_if_mk_sort_skeletons(IntGoals,Goals) :-
   ( nonvar(IntGoals)   % avoid loop with open ended list
     -> ( ( IntGoals = [ g(_What1,_What2,IntGoal1,_What3) | IntGoals1 ] ),
          ( IntGoal1 =.. [ SortName | Args ] ),
          length(Args,Arity1),
          Arity is Arity1 - 1,
          ( Goal = SortName/Arity ),
          cuf_if_mk_sort_skeletons(IntGoals1,Goals1),
          Goals = [Goal|Goals1] )
     ;  ( var(IntGoals)
          -> Goals = [] ) ).


%% cuf_if_lookup_featdecl( Type, FeatureValueList ) :-
%%   - lookup of the feature declarations for a Type
%%   - returns the empty list of no feature declaration is available
%%
cuf_if_lookup_featdecl( Type, FeatureValueList ) :-
   bagof( 
      FeatValPair,
      cuf_if_lookup_featdecl1( Type, FeatValPair ),
      FeatureValueList ).

cuf_if_lookup_featdecl( Type, FeatureValueList ) :-
    \+ /*RB cuf:*/'PARSER OUTPUT'(fdecl,_FileId,_EntryId, ( Type, _FeatValS ) )
    -> ( FeatureValueList = [] ).


% get a single feature declaration for a Type
cuf_if_lookup_featdecl1( Type, FeatValuePair ) :-
   /*RB cuf:*/'PARSER OUTPUT'(    
      fdecl,
      _FileId,
      _EntryId,
      ( Type, FeatValS ) ),
   member(FeatValuePair,FeatValS).


%% cuf_if_lookup_type_kids( +Type, -Kids )
%% - determine the firstmost defined set of subtypes for Type
%% - searches only the axioms of shape (i.e. proper type hierarchy):
%%   <Type Axiom> ::= <Type Symbol> = <Disjoints>  
%%   <Disjoints> ::=  <Type Symbol>          % direct equation is not covered
%%                   |   <Type Symbol> '|' <Disjoints>
%%                   |   <Type Symbol> ';' <Disjoints>
%%   <Disjoints> ::= '{' <Constants> '}'         % enumeration type
%% - Kids = [] in all other cases
%%
cuf_if_lookup_type_kids(Type,Kids) :-
   cuf_if_lookup_type_kids1(Type,Kids).

cuf_if_lookup_type_kids(Type,[]) :-
   \+ cuf_if_lookup_type_kids1(Type,_Kids).


% disjoint subtypes
cuf_if_lookup_type_kids1(Type,Kids) :-
   cuf_if_lookup_type_kids2( '='( Type, 'Disjs'(Kids) ), Kids ).

% disjunction of subtypes
cuf_if_lookup_type_kids1(Type,Kids) :-
   cuf_if_lookup_type_kids2( '='( Type, ';'(Kids) ), Kids ).


cuf_if_lookup_type_kids2(TypeAxiom,Kids) :-
   /*RB cuf:*/'PARSER OUTPUT'(type_axiom,_FId,_EId,TypeAxiom),
   cuf_if_list_of_atoms(Kids).


%% cuf_if_list_of_atoms(List)
%% - check whether all elements of List are atomic
%%
cuf_if_list_of_atoms(List) :-
   \+ ( member(Element,List), \+ atomic(Element) ).


%% --- END OF FILE: cuf_if.pl 
%%%      File: msgs.pl                                          %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: LexGram system: messages, warnings, and errors   %%%
%%%   Created: winter 95/96                                     %%%
%%%  Modified: Fri Oct 18 16:59:17 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_error/1
%%   lgi_message/1
%%   lgi_warning/1
%% Imports:
%%   concat/3               % CUF system / Prolog
%%   cuf_out/2              % CUF system
    

%% ----------------------------------------------------------
%%   auxiliary stuff
%% ----------------------------------------------------------

%% lgi_reversed_concat(+ListOfAtoms,+AccuAtom,-ConcatenatedAtom)
%% - inserts in addition a ':' between the elements
%%
lgi_reversed_concat([],CList,CList).

lgi_reversed_concat([Element|List],CAccu,CList) :-
   concat(':',CAccu,CAccu0),
   concat(Element,CAccu0,CAccu1),
   lgi_reversed_concat(List,CAccu1,CList).


lgi_warning1( Message ) :-
   cuf_out( message,
            ['LexGram',warning,':' | Message ] ).


lgi_error1( Message ) :-
   cuf_out( message, 
            ['LexGram',error,':' | Message ]).


%% ----------------------------------------------------------
%%   compiler messages
%% ----------------------------------------------------------

lgi_message(lexicon_compilation) :-
   cuf_out(message,['LexGram:',lexicon,compilation,'$NL$']).

lgi_message(lexicon_count(LexSize)) :-
   cuf_out(message,[lexicon,entries,'(including','traces)',':',
                    LexSize,'$NL$']).

lgi_message( default_feature_value(Path,ValueCUFsource) ) :-
   lgi_reversed_concat(Path,'',ReversedPath), 
   cuf_out(message,
           ['LexGram',assumes,':',ReversedPath,ValueCUFsource]).

%% ----------------------------------------------------------
%%   parser messages
%% ----------------------------------------------------------

lgi_message( runtime(What,RelTime) ) :-
   cuf_out(message,[What,':',RelTime]).


%% ----------------------------------------------------------
%%   compiler warnings
%% ----------------------------------------------------------

lgi_warning( missing_feature( db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_warning1( [no,value,specified,for,path,ReversedPath,
                  in,'$FS$'(CategoryFS),for,word,'<',Word,'>','$NL$'] ).

lgi_warning(not_nt_classifiable( NT, db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_warning1(['$FS$'(NT),cannot,be,classified,in,nonterminal,hierarchy,
                '/',value,of,path,ReversedPath,in,
                    category,'$FS$'(CategoryFS),for,word,
                   '<',Word,'>','$NL$' ] ).

%% ----------------------------------------------------------
%%   compiler errors
%% ----------------------------------------------------------

lgi_error( missing_feature( db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_error1( [no,value,specified,for,path,ReversedPath,
                  in,'$FS$'(CategoryFS),for,word,'<',Word,'>','$NL$'] ).

lgi_error(no_nt_hierarchy) :-
   lgi_error1( ['Nonterminal',hierarchy,does,not,exist,'$NL$'] ).

lgi_error( unexpandable_category( db(Word,_Path,CategoryFS) ) ):-
   lgi_error1( [ category,'$FS$'(CategoryFS),for,word,'<',Word,'>',
                 cannot,be,expanded,'$NL$'] ).

%% ----------------------------------------------------------
%%   interpreter errors (general)
%% ----------------------------------------------------------

%lgi_error(empty_lexicon) :-
%   lgi_error1([no,lexicon,available]).


%% ----------------------------------------------------------
%%   parser errors
%% ----------------------------------------------------------

lgi_error(list_of_afs(ListCUF)) :-
   lgi_error1( ['$FS$'(ListCUF),must,consist,of,afs,'''',s] ).

lgi_error(illformed_input_string(ListCUF)) :-
   lgi_error1( ['ill-formed',input,string,'$FS$'(ListCUF)] ).


%% ----------------------------------------------------------
%%   generator errors
%% ----------------------------------------------------------

lgi_error( illformed_guide(GuideCUF) ) :-
   lgi_error1( ['ill-formed',generation,guide,'$FS$'(GuideCUF)] ).


%% ----------------------------------------------------------
%%   various stuff /  LexGram self-debugging
%% ----------------------------------------------------------

lgi_error( instantiation_error(PredicateName) ) :-
   lgi_error1( [argument,not,instantiated,in,PredicateName] ).


%% --- END OF FILE:  msgs.pl
%%%      File: lgp.pl                                           %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: load file for the parser                         %%%
%%%   Created:                                                  %%%
%%%  Modified: Thu Oct 17 11:51:18 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgp_files/1
%% Imports:
%%   lg_module/1   % LexGram user interface
%%   lgi_expand_file_names/3

    
lgp_files( LGIFiles, Rest ) :-    
   lg_module('lg-sys/common/',lgcommon,LGIFiles,Rest), 
   % no CUF description files specific to the parser
   lgi_expand_file_names(
      [
       chartman,  % chart manager
       indexman,  % premise set manipulation/chart index
       expcat,    % category compiler
       hdparser,  % parser
       lgp_uif,   % user interface for the parser
       lg_chart   % chart browser
      ],
      'lg-sys/parser/', % directory relative to LexGram load file
      ExpandedFileNames ),
   ensure_loaded(ExpandedFileNames).



%% --- END OF FILE: lgp.pl
%%%      File: chartman.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: chart manager for parsing                        %%%
%%%   Created:                                                  %%%
%%%  Modified: Mon Jul 22 17:20:37 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_add_gtable_entry/2
%%   lgi_add_hptable_entry/2
%%   lgi_collect_tree/2
%%   lgi_fitting_gtable/2 
%%   lgi_fitting_hptable/3
%%   lgi_get_active_item/5
%%   lgi_get_passive_item/2
%%   lgi_get_gtable_entry/4
%%   lgi_get_hptable_entry/4
%%   lgi_init_chart/4
%%   lgi_input_string/2
%%   lgi_mk_item_fs/8
%%   lgi_new_gtable/2
%%   lgi_new_hptable/4
%%   lgi_reset_chart/0
%%   lgi_visit_sentence_item/5
%%   lgi_visit_item/8
%% Imports:
%%   predicates:
%%     cuf_extern2intern/4    % CUF ADT
%%     cuf_prolog_term/2      % CUF ADT
%%     cuf_put_path/3         % CUF ADT
%%     'LGl'/5
%%     'LGl'/4
%%     'LGusednt'/2
%%     l/5
%%     lgi_add_hypos/4
%%     lgi_cat2fs/2
%%     lgi_error/1
%%     lgi_expand_goal/6
%%     lgi_get_counter/2
%%     lgi_lookup_current_lex_index/4
%%     lgi_message/1
%%     lgi_mk_index_add/3
%%     lgi_mk_index_at/2
%%     lgi_mk_index_ga/3
%%     lgi_mk_index_gp/3
%%     lgi_mk_index_gt/2
%%     lgi_mk_string_index/3
%%     lgi_inc_counter/2
%%     lgi_reset_chart_index/0
%%     lgi_reset_goal_hypos/0
%%     lgi_set_counter/2
%%     lgi_string2ext/2
%%     lgi_subsumes_cat/2
%%     lgi_unify/2            
%%     lgi_unify_cat/3
%%     lgi_unify_hypos/2
%%     lgi_split_leaves/3
%% Counters:
%%   hcoffset  lexicon size + number of goal triggered traces
%%   l         entry id
%%   lexsize   lexicon size 
%%   t         table id

% ci( ItemId, OwnInfo ) % derived chart item 
:- dynamic(ci/2).  
% gt0( GoalTableId, GoalInfo ) % goal table header
:- dynamic(gt0/2).
% gt( GoalTableId, ItemId ) % goal table: goal + id's of solutions
:- dynamic(gt/2).
% all next head projections table header:
% ht0(HeadId,FrontFrom,FromTo,BackFrom,BackTo,RemHypoS,HeadProjTableId)
:- dynamic(ht0/7).
% head projection table header (for same set of resources):
% hpt0(HeadId,ArgFrom,ArgTo,ArgHypoS,HeadProjTableId).
:- dynamic(hpt0/5).
% hpt( HeadTableId, ItemId ) % goal table: goal + id's of solutions
:- dynamic(hpt/2).
% lgi_input_string(StringListPl,NumberOfWords)
:- dynamic(lgi_input_string/2).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   database initialization
%% ----------------------------------------------------------

lgi_reset_chart :-
   lgi_reset_chart_index,
   lgi_reset_goal_hypos,
   retractall(ci(_,_)),
   retractall(gt0(_,_)),
   retractall(gt(_,_)),
   retractall(hpt0(_,_,_,_,_)),
   retractall(hpt(_,_)),
   % reset entry id counter to lexicon size
   lgi_get_counter(lexsize,LexSize),  
   lgi_set_counter(l,LexSize),
   lgi_set_counter(t,0),        % table id
   retractall( lgi_input_string(_,_) ).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   string preprocessing
%% ----------------------------------------------------------

%% lgi_init_chart(+WordListCUF,-String,+GoalFS,-GoalArgCat)
%% - goal expansion
%% - assert chart items for the lexical entries and their hypos
%% - insert slash_ids and translated root type representation 
%%   into Category's
%%
lgi_init_chart(ListCUF,String,GoalFS,Goal):-
   % goal expansion
   lgi_expand_goal( GoalFS,Goal,[],SlashS, _CNType,
                    db('STARTSYMBOL',[],GoalFS)),
   lgi_add_hypos(SlashS,hc,0,_IdS1),   % goal has id 0
   lgi_get_counter(l,LastEntryId),
   lgi_set_counter(hcoffset,LastEntryId),
   ( lgi_mk_string_index(ListCUF,0,Length,StringListPl)
     -> ( lgi_string2ext( String, s(0,Length) ),
          assert( lgi_input_string(StringListPl,Length) ) )
     ;  ( lgi_error( illformed_input_string(ListCUF) ),
          fail ) ).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   handling of goal tables
%% ----------------------------------------------------------

%% ----------------------------------------------------------
%%   get goal table header
%% ----------------------------------------------------------

%% lgi_fitting_gtable(+Goal,-GTableId)
%% - if there a goal table for a goal Goal1 where Goal1 subsumes Goal,
%%   return the GTableId of that table

lgi_fitting_gtable(
      c(String,HypoS1,GoalCat,GoalGoalS,GoalConstrS),
      GTableId) :-
   ( GoalCat = cat( _CT1, root( RootType, _RFS1), 
                    lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,_PI1) ),
   clause( 'LGusednt'(RootType,UnifS), _ ), % lookup UnifS
   lgi_mk_index_gt(
      ii(UnifS,String,HypoS1,LLength), 
      GTableId ),
   clause(
      gt0( 
         GTableId,
         c(String,_HypoS2,Goal1Cat,Goal1GoalS,Goal1ConstrS) ), _ ),
   lgi_subsumes_cat(
      c1( Goal1Cat, Goal1GoalS, Goal1ConstrS ),
      c1( GoalCat, GoalGoalS, GoalConstrS ) ).

%% ----------------------------------------------------------
%%   get goal table entry
%% ----------------------------------------------------------

%% lgi_get_gtable_entry(+Goal,+GTableId,-Out,-ItemId)
%%
lgi_get_gtable_entry(
      c(_String,HypoS,GoalCat,GoalGoalS,GoalConstrS),
      GTableId,
      cth(GoalSOut,ConstrSOut),
      Id ) :-
   gt(GTableId,Id),
   lgi_get_counter(lexsize,LexOffSet), 
   ( ( Id =< LexOffSet ) % lexical stuff
     -> lgi_get_gtable_entry_lex(HypoS,Cat,GoalS,ConstrS,Id)
     ;  % stuff in chart
        lgi_get_gtable_entry_chart(Cat,HypoS,GoalS,ConstrS,Id) ),
   lgi_unify_cat(
      c1(GoalCat,GoalGoalS,GoalConstrS), % check goal's goals + constraints
      c1(Cat,GoalS,ConstrS),
      c1(_Res,GoalSOut,ConstrSOut) ).


% lookup a word
lgi_get_gtable_entry_lex([],Cat,GoalS,ConstrS,Id) :-
   clause( l(Id,Cat,GoalS,ConstrS,_LicSl), _ ).

% lookup a hypo in the lexicon
lgi_get_gtable_entry_lex([s(Id,Cat1)],Cat2,GoalS,ConstrS,Id) :-
   clause( hl(Id,Cat2,GoalS,ConstrS), _ ),
   % unify chart entry's cat with top-down expected hypo information
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ). 


% lookup a derived item
lgi_get_gtable_entry_chart(Cat,HypoS1,GoalS,ConstrS,Id) :-
   clause( ci(Id,Own), _ ),
   Own = c(_String2,HypoS2,Cat,GoalS,ConstrS),
   lgi_unify_hypos(HypoS1,HypoS2).

% lookup a hypo in the chart
lgi_get_gtable_entry_chart(Cat2,[s(Id,Cat1)],GoalS,ConstrS,Id) :-
   clause( hc(Id,Cat2,GoalS,ConstrS), _ ), 
   % unify chart entry's cat with top-down expected hypo information 
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).


%% ----------------------------------------------------------
%%   add goal table header
%% ----------------------------------------------------------

%% lgi_new_gtable(+Goal,-GTableId)
%%
lgi_new_gtable(Goal,GTableId) :-
   ( Goal =
      c( String, HypoS, 
         cat( _CT1, root( RootType, _RFS1), 
              lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1, _PI1 ),
         _GS1, _CS1 ) ),
   lgi_inc_counter(t,GTableId), 
   lgi_mk_index_at(
      ii(RootType,String,HypoS,LLength), 
      GTableId ),
   assert( gt0(GTableId,Goal) ).


%% ----------------------------------------------------------
%%   add goal table entry
%% ----------------------------------------------------------

%% lgi_add_gtable_entry(+GTableId,+ItemId)
%%
lgi_add_gtable_entry(GTableId,ItemId) :-
   assert( gt(GTableId,ItemId) ).



%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   handling of head projection tables
%% ----------------------------------------------------------

%%%%  head projections for same ressources

%% ----------------------------------------------------------
%%   get head project table header
%% ----------------------------------------------------------

%% lgi_fitting_hptable(+HeadId,+ArgString,+ArgHypoS,-HeadProjTableId)
%% - IF there is a head projection table for HeadId and the argument
%%      ressources ArgString, ArgHypoS
%%   THEN return the HeadProjTableId of that table
%% - OTHERWISE fail

lgi_fitting_hptable(
      HeadId,
      s(ArgFrom,ArgTo),
      ArgHypoS,
      HPTableId) :-
   clause( hpt0(HeadId,ArgFrom,ArgTo,ArgHypoS,HPTableId), _ ).


%% ----------------------------------------------------------
%%   get head projection table entry
%% ----------------------------------------------------------

%% lgi_hptable_entry(+HPTableId,-HeadProj,-RemLeaves,-ItemId)
%% - head projections are always derived items
%%
lgi_get_hptable_entry(HPTableId,HeadProj,RemLeaves,Id) :-
   hpt(HPTableId,Id),
   clause( ci( Id, HeadProj), _ ),
   ( HeadProj =
        c( _S1, _H1, 
           cat( _CT1, _R1, lvs(_LL1,_L1,_RL1,RemLeaves),
                _DCtrs1, _PI1 ),
           _G1, _ConstrS1 ) ).


%% ----------------------------------------------------------
%%   add head projection table header
%% ----------------------------------------------------------

%% lgi_new_hptable(+HeadId,+ArgString,+ArgHypoS,-HPTableId)
%% - ArgInfo: only ressource instantiations count
%%
lgi_new_hptable(
      HeadId,
      s(ArgFrom,ArgTo), 
      ArgHypoS, 
      HPTableId) :-
   lgi_inc_counter(t,HPTableId), 
   assert( hpt0(HeadId,ArgFrom,ArgTo,ArgHypoS,HPTableId) ).


%% ----------------------------------------------------------
%%   add head projection table entry
%% ----------------------------------------------------------

%% lgi_add_hptable_entry(+HPTableId,-ItemId)
%%
lgi_add_hptable_entry(HPTableId,ItemId) :-
   assert( hpt(HPTableId,ItemId) ).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   general item manipulation
%% ----------------------------------------------------------

%% ----------------------------------------------------------
%%   get lexical items
%% ----------------------------------------------------------

%%%%%%
%% lgi_get_passive_item(+GoalInfo,-ItemId)
%% - lookup a passive item 
%%
lgi_get_passive_item(
      Goal,
      Id ) :-
   ( ( ( Goal = c(String,HypoS,GoalCat,_GoalGoalS,_GoalConstrS) ),
       ( GoalCat = cat( _CT1, root( RootType, _RFS1), 
                        lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,_PI1) ),
       clause( 'LGusednt'(RootType,UnifS), _ ) % lookup UnifS
     ) -> true ),   % no backtracking for this part
   lgi_mk_index_gp(
      ii(UnifS,String,HypoS,LLength), 
      Id,
      Flag),
   lgi_get_passive_item0(Flag,Id,HypoS,OwnCat,_OwnGoalS,_OwnConstrS),
   lgi_unify_cat(   % GoalS, ConstrS considered at gtable lookup
      c1(GoalCat,[],[]), 
      c1(OwnCat,[],[] ),
      _Cat1 ).


% lookup a word
lgi_get_passive_item0(word,Id,[],Category,GoalS,ConstrS) :-
   clause( l(Id,Category,GoalS,ConstrS,_LicSl), _ ).

% lookup a hypo in the lexicon
lgi_get_passive_item0( 
      trace(hl),
      Id,
      [ s(Id,Cat1) ],
      Cat2,
      GoalS,
      ConstrS ) :-
   clause( hl(Id,Cat2,GoalS,ConstrS), _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).

% lookup a hypo in the chart
lgi_get_passive_item0(trace(hc),Id,[s(Id,Cat1)],Cat2,GoalS,ConstrS) :-
   clause( hc(Id,Cat2,GoalS,ConstrS), _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).



%%%%%%
%% lgi_get_active_item((+GoalInfo,-HeadInfo,-RestLeaves,-PremiseRest,-ItemId)
%%
lgi_get_active_item(Goal,Head,RestLeaves,In1,Id) :-
   ( ( ( Goal = c( String,HypoS, 
                   cat( _CT1, root(GoalRT,GoalRoot),
                        lvs(GoalLL,_L1,GoalRLeaves,_RemL1),
                        _Ctrs1, _P1 ), 
                   _GoalGoalS, _GoalConstrS ) ),
       clause( 'LGusednt'(GoalRT,UnifS), _ ) % lookup UnifS
     ) -> true ),  % no backtracking for this part
   lgi_mk_index_ga( ii(UnifS,String,HypoS,GoalLL),
                    HeadString, HeadHypoS, In1, Id, Flag ), 
   lgi_get_active_item0(Flag,Id,HeadHypoS,HeadCat,HeadGoalS,HeadConstrS),
   ( HeadCat = cat( _CT3, root(_RT3,Root),
                    lvs(_LL3,_L3,HeadRLeaves,_RemL3), _Ctrs3, _P3 )),
   lgi_unify(   
      d(GoalRoot,[],[]),  % GoalGoalS, GoalConstrS at gtable lookup
      d(Root,HeadGoalS,HeadConstrS),
      d(_Res,OutGoalS,OutConstrS) ),
   lgi_split_leaves(GoalRLeaves,HeadRLeaves,RestLeaves),
   ( Head = c(HeadString,HeadHypoS,HeadCat,OutGoalS,OutConstrS) ).


%% lgi_get_active_item0(Flag,+Id,+HypoS,-Category,-GoalS,-ConstrS)
%% - Category: output variable in case of word
%% - Category: input/output variable in case of hypo
%%
lgi_get_active_item0(word,Id,[],Category,GoalS,ConstrS) :-
   clause( l(Id,Category,GoalS,ConstrS,_LicSl), _ ).

lgi_get_active_item0(trace(Flag),Id,[s(Id,Cat1)],Cat2,GoalS,ConstrS) :-
   Clause =.. [Flag,Id,Cat2,GoalS,ConstrS],
   clause( Clause, _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).


%% ----------------------------------------------------------
%%   add item
%% ----------------------------------------------------------

%% lgi_add_derived_item(+OwnInfo,-ItemId)
%% - assert a derived chart item 
%%
lgi_add_derived_item(Own,ItemId) :-
   lgi_inc_counter(l,ItemId), 
   assert( ci(ItemId,Own) ), 
write(ItemId),put(44).


%% ----------------------------------------------------------
%%   view item
%% ----------------------------------------------------------

%% lgi_visit_sentence_item(From,To,ItemId,CatNameAtom,ConstituentType)
%% lgi_visit_item(From,To,ItemId,CatNameAtom,CatFT,Tree,ConstituentType,HypoS)
%%
%% - ConstituentType needed for chart browser
%% - HypoS needed for output feature structure

lgi_visit_sentence_item(From,To,ItemId,CatNameAtom,CatType) :-
   lgi_visit_item(From,To,ItemId,CatNameAtom,_CatFT,_Tree,CatType,_HypoS).


lgi_visit_item( 
      From, To, Id, CatName, 
      d(CatFS,GoalS,ConstrS),
      Tree, CatType, HypoS ) :-
   lgi_visit_item0(CatType,From,To,Id,Category,GoalS,ConstrS,HypoS),
   lgi_cat2fs(Category,CatFS),
   ( Category = cat(_CT1,_R1,_L1,_Ctrs1,Tree) ),
   ( Tree = node(CatName,Id,_Dtrs) ).


lgi_visit_item0(
      word,
      From, To, Id, Category, GoalS, ConstrS, [] ) :-
   ( var(Id) 
     -> ( lgi_lookup_current_lex_index(word,From,To,Id),
          ( VarFlag = Id ) )
     ;  true ), % var(VarFlag)
   clause( l(Id,Category,GoalS,ConstrS,_SlashIdS1), _ ),
   % 1. restrict view to items of the current parse
   % 2. make sure that From and To are instantiated
   ( var(VarFlag)
     -> lgi_lookup_current_lex_index(word,From,To,Id)
     ;  nonvar(VarFlag) ).

lgi_visit_item0(
      derived_phrase, 
      From, To, Id, Category, GoalS, ConstrS, HypoS ) :-
   lgi_string2ext(String,s(From,To)),
   clause( ci( Id,
               c(String,HypoS,Category,GoalS,ConstrS) ), _ ).

% lexical trace
lgi_visit_item0(
      trace,
      0, 0,    % trivial instantiations of trace positions
      Id, Category, GoalS, ConstrS,
      [s(Id,_Hypo)] ) :-
   ( var(Id) 
     -> lgi_lookup_current_lex_index(trace,_From,_To,Id)
     ;  true ),  % var(VarFlag)
   clause( hl(Id,Category,GoalS,ConstrS), _ ),
   % restrict view to items of the current parse
   ( var(VarFlag)
     -> lgi_lookup_current_lex_index(trace,_From,_To,Id)
     ;  nonvar(VarFlag) ).

% goal trace
lgi_visit_item0(
      trace,
      0, 0,    % trivial instantiations of trace positions
      Id, Category, GoalS, ConstrS,
      [s(Id,_Hypo)] ) :-
   clause( hc(Id,Category,GoalS,ConstrS), _ ).


%% lgi_mk_item_fs(+From,+To,+HypoS,+ItemId,+CatFT,+CatNameAtom,+Tree,-ItemFT)
%%
lgi_mk_item_fs(
       From,To,HypoS,ItemId,
       d(CatFS,GoalS,ConstrS),
       CatNameAtom,
       node(_CatName,_Id,Dtrs),
       d(ItemFS,GoalS,ConstrS) ) :-
    cuf_prolog_term(ItemId,ItemIdCUF),
    cuf_put_path(ItemFS, ['ITEM-ID'], ItemIdCUF),
    lgi_input_string(String,_Length),
    lgi_collect_string(0,From,To,String,SubString),
    cuf_prolog_term( (From-To:SubString), FromToCUF ),
    cuf_put_path(ItemFS, ['FROM-TO'], FromToCUF),
    lgi_extract_hypo_ids(HypoS,HypoIdS),
    cuf_prolog_term(HypoIdS,HypoSCUF),
    cuf_put_path(ItemFS, ['TRACES'], HypoSCUF),
    cuf_prolog_term(CatNameAtom,CatNameAtomCUF),
    cuf_put_path(ItemFS, ['CAT-NAME'], CatNameAtomCUF),
    cuf_put_path(ItemFS, ['CAT'], CatFS),
    cuf_prolog_term(Dtrs,DtrsCUF),
    cuf_put_path(ItemFS, ['DTRS'], DtrsCUF).


%% lgi_collect_string(+StartPos,+From,+To,+String,-SubString)
%% - calculate the SubString between From and To
%%
% search for beginning of substring
lgi_collect_string( StartPos, From, To, [_Word|String], SubString ) :-
   StartPos < From,   
   !,
   NextPos is StartPos + 1,
   lgi_collect_string(NextPos,From,To,String,SubString).

% collect substring
lgi_collect_string( From, From, To, [Word|String], [Word|SubString] ) :-
   From < To,
   !,
   NextPos is From + 1,
   lgi_collect_string(NextPos,NextPos,To,String,SubString).

lgi_collect_string(Pos,Pos,Pos,_String,[]).



%% lgi_extract_hypo_ids(+HypoS,-HypoIdS) 
%%
lgi_extract_hypo_ids( 
      [ s(Id,_Hypo) | RestSlash ], 
      [ Id | RestIdS ] ) :-
   lgi_extract_hypo_ids(RestSlash,RestIdS).

lgi_extract_hypo_ids([],[]).


%% lgi_collect_tree(+ItemId,-TreeOut)
%% - collect the Tree with for ItemId
%%

lgi_collect_tree(Id,Tree) :-
   ( % word
       l( Id,
        cat(_CT1,_R1,_L1,_Ctrs1,Tree),
        _G1, _C1, _LH1 )
     ; % lexical trace
       hl( Id,
           cat(_CT1,_R1,_L1,_Ctrs1,Tree),_G1,_C1)
     ; % goal trace
       hc( Id,
           cat(_CT1,_R1,_L1,_Ctrs1,Tree),_G1,_C1)
     ; % derived item
       ( ci( Id, 
             c( _S1, _H1,
                cat( _CT1, _R1, _L1, _Ctrs1, node(CatName,Id,DtrIds) ),
                _G1, _C1 ) ),
         lgi_collect_trees(DtrIds,Dtrs),
         ( Tree = node(CatName,Id,Dtrs) ) ) ).


lgi_collect_trees( [Id|Ids], [Tree|Trees] ) :-
   lgi_collect_tree(Id,Tree),
   lgi_collect_trees(Ids,Trees).

lgi_collect_trees([],[]).


%% --- END OF FILE: chartman.pl 
%%%      File: indexman.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: index manager for internal lexicon and chart     %%%
%%%   Created:                                                  %%%
%%%  Modified: Mon Jul 15 13:35:30 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   'LGla'/5
%%   'LGlp'/4
%%   lgi_lookup_current_lex_index/4
%%   lgi_mk_string_index/4
%%   lgi_mk_index_add/3
%%   lgi_mk_index_at/2
%%   lgi_mk_index_ga/6
%%   lgi_mk_index_gp/3
%%   lgi_mk_index_gt/2
%%   lgi_guess_premises/5
%%   lgi_hypos2ext/2
%%   lgi_hypos_union/3
%%   lgi_reset_chart_index/0
%%   lgi_same_hypos/2
%%   lgi_string2ext/2
%%   lgi_string_union/3
%% Imports:
%%   l/5
%%   length/2     % Prolog
%%   lgi_unify_hypos/2
%% Counters:
%%   lexsize

%% representations:
%% - string:  s(From,To)
%% - hypotheses: a Prolog list of item ids
%%
% lexicon
% 'LGl0'(Word,LeavesLength,RootType,Id)
:- dynamic 'LGl0'/4.  % index for words (before lexicon lookup)
% 'LGhl0'(Id,LeavesLength,RootType)
:- dynamic 'LGhl0'/3.  % filter for active lexicon/hypos
% 'LGhl'(Id,RootType)
:- dynamic 'LGhl'/2.  % filter for passive lexicon/hypos
% 'LGhl'(Id,LeavesLength,RootType), LeavesLength > 0
:- dynamic 'LGhl'/3.  % filter for active lexicon/hypos

% current parse
% 'LGl'(From,To,RootType,Id), LeavesLength = 0
:- dynamic 'LGl'/4.   % index for passive words (after lexicon lookup)
% 'LGl'(From,To,LeavesLength,RootType,Id)  , LeavesLength > 0
:- dynamic 'LGl'/5.   % index for active words (after lexicon lookup)
% 'LGhc'(Id,RootType)
:- dynamic 'LGhc'/2.   % filter for passive goal/hypos
% 'LGhc'(Id,LeavesLength,RootType), LeavesLength > 0
:- dynamic 'LGhc'/3.   % filter for active goal/hypos
% 'LGg'(From,To,HypoS,LeavesLength,RootType,GoalTableId)
:- dynamic 'LGg'/6.    % ressource index for goal table headers


%% ----------------------------------------------------------
%%   index lookup by chart browser
%% ----------------------------------------------------------

lgi_lookup_current_lex_index(word,From,To,Id) :-
   clause( 'LGl'(From,To,_RootType,Id), _ ).

lgi_lookup_current_lex_index(word,From,To,Id) :-
   clause( 'LGl'(From,To,_Length2,_RootType,Id), _ ).

lgi_lookup_current_lex_index(trace,_From,_To,Id) :-
   'LGhl'(Id,_RootType).

lgi_lookup_current_lex_index(trace,_From,_To,Id) :-
   'LGhl'(Id,_Length,_RootType).


%% ----------------------------------------------------------
%%   initialization stuff
%% ----------------------------------------------------------

lgi_reset_lex_index :-
   retractall('LGl0'(_,_,_,_)),
   retractall('LGhl0'(_,_,_)).

lgi_reset_chart_index :-
   retractall('LGl'(_,_,_,_)),
   retractall('LGl'(_,_,_,_,_)),
   retractall('LGhl'(_,_)),
   retractall('LGhl'(_,_,_)),
   retractall('LGhc'(_,_)),
   retractall('LGhc'(_,_,_)),
   retractall('LGg'(_,_,_,_,_,_)).
   

%% lgi_mk_string_index(+WordListCUF,+PreviousLength,-Length,-StringListPl)
%%
lgi_mk_string_index(ListCUF,Pos,Length,StringPl):-
   ( % non-empty list
     cuf_path_value(ListCUF,['F'],WordCUF),
     cuf_path_value(ListCUF,['R'],RestListCUF) )
   -> ( ( % calculate common information of all items for Word
          cuf_atom(Word,WordCUF),
write(Word), put(32),
          Pos1 is Pos +1,
          ( % assert ressource index for all entries of that Word
            ( 'LGl0'(Word,Leaves,Root,Id),
write(Id), put(32),          
              ( ( Leaves = 0 )
                -> assert( 'LGl'(Pos,Pos1,Root,Id) )
                ;  ( ( Leaves > 0 )
                     -> assert( 'LGl'(Pos,Pos1,Leaves,Root,Id) ) ) ),
              ( ( clause( l(Id,_Cat1,_GoalS1,_ConstrS1,LicSlS), _ ),
                  lgi_add_current_hypo_index(LicSlS) )
                -> true ),   % no backtracking here
              fail 
            ); 
            lgi_mk_string_index(RestListCUF,Pos1,Length,String1Pl) ),
            ( StringPl = [Word|String1Pl] )
        ); 
        ( \+ cuf_atom(Word,WordCUF),
          lgi_error(list_of_afs(ListCUF)) ) )
   ; ( cuf_atom([],ListCUF)   % end of word list
       -> ( ( Length = Pos ),
            ( StringPl = [] ) ) ).



lgi_add_current_hypo_index([]).

lgi_add_current_hypo_index( [Id | LicSlS ] ) :-
   ( ( ( clause( 'LGhl'(Id,_), _ )
         ; clause( 'LGhl'(Id,_,_), _ ) ) )
     -> true
     ; % if not yet in chart
       ( clause( 'LGhl0'(Id,Length,RootType), _ ),
         ( ( Length = 0 )
           -> assert( 'LGhl'(Id,RootType) )
           ; ( ( Length > 0 )
               -> assert( 'LGhl'(Id,Length,RootType) ) ) ),
         write(Id),put(32) ) ),
   lgi_add_current_hypo_index(LicSlS).


%% ----------------------------------------------------------
%%   produce the index of an item from psets and root info
%% ----------------------------------------------------------
%% data structures
%% - IndexInfoIn :== ii( RootTypeInfo, String, HypoS, LeavesLength )
%%    - RootTypeInfo: either a single TypeId or a list of TypeIds
%%    - HypoS :== a list of s(HypoId,HypoCat) terms
%%    - String :== PrologAtom | s(FromNumber,ToNumber)
%% - Flag :== word | trace(Flag1) | derived_phrase
%% - Flag1 :== hc | hl   (goal hypos vs. lexical hypos)
%%
%% lgi_mk_index_add(+Flag,+IndexInfoIn,+EntryId) 
%% lgi_mk_index_at(+IndexInfo,+Id)
%% - produce indices according to IndexInfoIn 
%% lgi_mk_index_ga(+IndexInfo,-HeadString,-HeadHypoS,-HeadCat,-Out,-Id,Flag)
%% lgi_mk_index_gp(+IndexInfo,-EntryId,-Flag) 
%% lgi_mk_index_gt(+IndexInfo,-Id)
%% - use indices for item lookup
%%
lgi_mk_index_add(
      word,
      ii( RootType, Word, _HypoS1, Length ), 
      Id ) :-
   assert( 'LGl0'(Word,Length,RootType,Id) ).

lgi_mk_index_add(
      trace(Flag),
      ii( RootType, _Word1, _HypoS1, Length ), 
      Id ) :-
   ( ( Flag = hc )
     -> ( ( Length = 0 )
          -> ( Clause =.. ['LGhc',Id,RootType] )
          ; ( ( Length > 0 )
              -> ( Clause =.. ['LGhc',Id,Length, RootType] ) ) )
     ;  ( ( Flag = hl )
          -> ( Clause =.. ['LGhl0',Id,Length,RootType] ) ) ),
   assert( Clause ).



lgi_mk_index_ga(
      ii( UnifS, s(GoalFrom,GoalTo), GoalHypoS, GLength ),
      s(HeadFrom,HeadTo),
      HeadHypoS,
      cin(s(GoalFrom,HeadFrom),s(HeadTo,GoalTo),HypoSOut),
      Id,
      Flag ) :- 
   lgi_in_interval(HeadFrom,GoalFrom,GoalTo),  % choose a position
   lgi_mk_index_ga1(
      RootType, GoalTo, GoalHypoS, HLength,
      s(HeadFrom,HeadTo),
      HeadHypoS, HypoSOut, 
      Id,
      Flag ),
   HLength > GLength,        % head leaves longer than goal leaves
   member(RootType,UnifS).   % check root type


% word 
lgi_mk_index_ga1(
      RootType, GoalTo, GoalHypoS, HLength,
      s(HeadFrom,HeadTo),
      [],
      GoalHypoS,
      Id,
      word ) :- 
   HeadFrom < GoalTo,   % ?? redundant with lgi_in_interval/
   'LGl'(HeadFrom,HeadTo,HLength,RootType,Id).

% a hypo
lgi_mk_index_ga1(
      RootType, _GoalTo, GoalHypoS, HLength,
      s(HeadPos,HeadPos),
      [s(Id,HypoCat)],
      HypoSOut,
      Id,
      trace(Flag) ) :- 
   lgi_hypos_diff( GoalHypoS, [ s(Id,HypoCat) ], HypoSOut ), % guess HeadHypo
   lgi_mk_index_ga0(Id,HLength,RootType,Flag).


lgi_mk_index_ga0(Id,Length,RootType,hc) :-
   'LGhc'(Id,Length,RootType).

lgi_mk_index_ga0(Id,Length,RootType,hl) :-
   'LGhl'(Id,Length,RootType).



% word
lgi_mk_index_gp(
      ii( UnifS,
          s(From,To), 
          [],
          Length ),   
      Id,
      word ) :- 
   To is From + 1,
   !,  % no other possibility
   lgi_mk_index_gp0(word,Length,From,To,Id,RootType),
   member(RootType,UnifS).

% trace
lgi_mk_index_gp(
      ii( UnifS,
          s(Pos,Pos), 
          [ s(Id,_HypoCat) ],
          Length ),   
      Id,
      trace(Flag) ) :- 
   !,  % no other possibility
   lgi_get_counter(lexsize,LexOffSet),
   ( ( Id =< LexOffSet )   % lexical trace
     -> ( Flag = hl )
     ;  ( ( Id > LexOffSet )  % goal trace
          -> ( Flag = hc ) ) ),
   lgi_mk_index_gp0(Flag,Length,Id,RootType),
   member(RootType,UnifS).


lgi_mk_index_gp0(word,0,From,To,Id,RootType) :-
   !,
   clause( 'LGl'(From,To,RootType,Id), _ ).

lgi_mk_index_gp0(word,Length,From,To,Id,RootType) :-
   Length > 0,
   !,
   clause( 'LGl'(From,To,Length,RootType,Id), _ ).

lgi_mk_index_gp0(hc,0,Id,RootType) :-
   !,
   clause( 'LGhc'(Id,RootType), _ ).

lgi_mk_index_gp0(hc,Length,Id,RootType) :-
   Length > 0,
   !,
   clause( 'LGhc'(Id,Length,RootType), _ ).

lgi_mk_index_gp0(hl,0,Id,RootType) :-
   !,
   clause( 'LGhl'(Id,RootType), _ ).

lgi_mk_index_gp0(hl,Length,Id,RootType) :-
   Length > 0,
   clause( 'LGhl'(Id,Length,RootType), _ ).


% create index for a goal table header
lgi_mk_index_at(
      ii( RootType, s(From,To), HypoS, Length ), 
      GTableId ) :-
   assert( 'LGg'(From,To,HypoS,Length,RootType,GTableId) ).


% lookup index for a goal table header
lgi_mk_index_gt(
      ii( UnifS, s(From,To), HypoS1, Length ),   
      GTableId ) :- 
   clause( 'LGg'(From,To,HypoS2,Length,RootType,GTableId), _ ),  
   lgi_same_hypos(HypoS2,HypoS1),
   member(RootType,UnifS). % check root type



%% ----------------------------------------------------------
%%   guess premises for a subgoal
%% ----------------------------------------------------------

%% lgi_guess_premises(+Direction,+GoalInfo,-OwnInfo,-RestInfo)
%% - guess the premises for an arbitrary subconstituent
%% - RestInfo: difference between GoalInfo and OwnInfo
%%
lgi_guess_premises(
      Dir,
      cin(FrontString,BackString,GoalHypoS),  % in
      c( OwnString,OwnHypoS,_Cat2,_G2,_C2),  % own
      cin(RestFrontString,RestBackString,HypoSOut) ) :- % out
   lgi_guess_position(Dir,FrontString,BackString,OwnString,
                      RestFrontString,RestBackString),
   lgi_ne_premises(OwnHypoS,OwnString), % exclude empty premise set
   lgi_hypos_diff(GoalHypoS,OwnHypoS,HypoSOut).  % guess OwnHypoS


lgi_guess_position(left,s(From,To),BackString,s(From1,To),
                   s(From,From1),BackString ) :-
   lgi_in_interval_downward(From1,From,To).

lgi_guess_position(right,FrontString,s(From,To),s(From,To1),
                   FrontString,s(To1,To) ) :-
   lgi_in_interval(To1,From,To).


lgi_ne_premises( [_|_], s(Pos,Pos) ).

lgi_ne_premises( _HypoS, s(Pos1,Pos2) ) :-
   Pos1 < Pos2.


%% ----------------------------------------------------------
%%   pset difference
%% ----------------------------------------------------------

%% lgi_hypos_diff(+BigPSet,-SmallPSet,-RestPSet)
%% - guess a subset SmallPSet from BigPSet 

% Element is also Element of the SmallSet
lgi_hypos_diff( 
      [Element|BigSet], 
      [Element|SmallSet], 
      RestSet ) :-
   lgi_hypos_diff(BigSet,SmallSet,RestSet).

% omit Element from SubSet
lgi_hypos_diff( 
      [Element|BigSet], 
      SmallSet,
      [Element|RestSet] ) :-
   lgi_hypos_diff(BigSet,SmallSet,RestSet).

lgi_hypos_diff([],[],[]).


%% lgi_is_hypos_subset(+BigPSet,+SmallPSet)
%% - checks whether SmallPSet is a subset of BigPSet 
%%   (for ordered sets)

% Element is also Element of the SmallSet
lgi_is_hypos_subset( 
      [s(HId,Hypo1)|BigSet], 
      [s(HId,Hypo2)|SmallSet] ) :-
   lgi_unify_cat(
      c1(Hypo1,[],[]),
      c1(Hypo2,[],[]),_Cat3),
   lgi_is_hypos_subset(BigSet,SmallSet).

% omit Element from SubSet
lgi_is_hypos_subset( 
      [_Element|BigSet], 
      SmallSet ) :-
   lgi_is_hypos_subset(BigSet,SmallSet).

lgi_is_hypos_subset([],[]).


%% ----------------------------------------------------------
%%   pset union
%% ----------------------------------------------------------

%% lgi_string_union(+PSet1,+PSet2,-BigPSet)
%% lgi_hypos_union(+PSet1,+PSet2,-BigPSet)

% for strings: only for adjoining strings  - for rightward search
lgi_string_union( 
      s(From,Middle), 
      s(Middle,To),
      s(From,To) ).

% for leftward search
lgi_string_union( 
      s(Middle,To),
      s(From,Middle), 
      s(From,To) ).


% for hypos:
lgi_hypos_union(PSet1,PSet2,BigPSet) :-
   lgi_ordered_hypo_union(PSet1,PSet2,BigPSet).


%% lgi_ordered_hypo_union(Set1,Set2,Union)
%% - merge two ordered sets into a new ordered set 
%%  (according to their hypo-ids)

lgi_ordered_hypo_union([],Set,Set).

lgi_ordered_hypo_union([Element|List1],List2,List3) :-
   lgi_ordered_hypo_insert(List2,Element,List1,List3).


lgi_ordered_hypo_insert([],Element,List,[Element|List]).

lgi_ordered_hypo_insert(
      [ s(Element2,Cat2) | List2 ],
      s(Element1,Cat1), 
      List1,
      [ s(Element,Cat) | List3 ] ) :-
   ( Element1 > Element2 )
   -> ( ( Element = Element2 ),
        ( Cat = Cat2 ),
        lgi_ordered_hypo_insert( List2, s(Element1,Cat1), List1, List3 ) )
   ; ( ( Element2 >= Element1 )
       -> ( ( Element = Element1 ),
            ( Cat = Cat1 ),
              lgi_ordered_hypo_union( 
                 List1, 
                 [ s(Element2,Cat2) | List2 ],
                 List3 ) ) ).


%% ----------------------------------------------------------
%%   external views on psets
%% ----------------------------------------------------------

%% lgi_string2ext(PSet,ExternalView)
%% lgi_hypos2ext(PSet,ExternalView)
%% - actually the internal representation 
%%   equals the external one
%%
lgi_string2ext(PSet,PSet).

lgi_hypos2ext(PSet,PSet).


%% --- END OF FILE: indexman.pl
%%%      File: expcat.pl                                        %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: string preprocessing, category compilation       %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Oct 11 16:46:56 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_add_hypos/4
%%   lgi_cat2fs/2
%%   lgi_collect_nt_hierarchy/1
%%   lgi_compile_lexicon/0
%%   lgi_expand_goal/6
%%   lgi_reset_goal_hypos/0
%%   lgi_same_hypos/2
%%   lgi_subsumes_cat/2
%%   lgi_unify_cat/3
%%   lgi_unify_hypos/2
%%   lgi_unify_leaves/2
%%   lgi_split_leaves/3
%% Imports:
%%   predicates:
%%     abbreviated_name/2
%%     concat/3               % CUF system / Prolog
%%     cuf_atom/2             % CUF ADT
%%     cuf_get_type/2         % CUF ADT
%%     cuf_goal/4             % CUF ADT
%%     cuf_extern2intern/4    % CUF ADT
%%     cuf_if_lookup_type_kids/2
%%     cuf_prove/6            % CUF ADT
%%     cuf_path_value/3       % CUF ADT
%%     cuf_put_path/3         % CUF ADT
%%     cuf_put_type/2         % CUF ADT
%%     length/2               % Prolog
%%     lgi_error/1
%%     lgi_leftcorner_candidate/4 
%%     lgi_mk_index_add/3
%%     lgi_mk_tree/6
%%     lgi_get_counter/2
%%     lgi_hypos2ext/2
%%     lgi_message/1
%%     lgi_reset_lex_index/0
%%     lgi_unify/3
%%     lgi_warning/1
%%   types:
%%      cat
:- dynamic hl/4.   % hypos from the lexicon
:- dynamic hc/4.   % hypos from the top goal
% l(Id,Category,GoalS,ConstraintS,LicensedHypoS) % word entries
:- dynamic l/5.    
:- dynamic 'LGnt'/1.
:- dynamic 'LGusednt0'/2. % nonterminals used in the lexicon
:- dynamic 'LGusednt'/2.  % nonterminals used in the lexicon, 
% counters:
% l        - entry id
% lexsize  - lexicon size 


%% ----------------------------------------------------------
%%   category compiler - top level -
%%
%% ----------------------------------------------------------

%% lgi_compile_lexicon/0
%% - compile CUF-clauses for sort l/1 into 
%%   Prolog facts l(Id,Category,GoalS,ConstraintS,LicensedHypoS)
%%
lgi_compile_lexicon :-
   ( ( lgi_message(lexicon_compilation),
       lgi_init_compile_lexicon, 
       % produce goal schema: l(Word) 
       cuf_goal( l, [WordCUF], CategoryFS, LexGoal ) )
     -> true ),  % no backtracking here
   cuf_prove(undelayed_only,[LexGoal],GoalS,[],ConstrS,_ClL),
   % lgi_cuf_clause(l(WordCUF),CategoryFS,GoalS,ConstrS), % may leave cat's
   %    too underspecified
   cuf_atom(Word,WordCUF),
   lgi_compile_cat(Word,CategoryFS,GoalS,ConstrS),
   fail.  % cause backtracking for cuf_prove/6 

% termination
lgi_compile_lexicon :-
   lgi_compress_nt_info,	 
   ( lgi_get_counter(l,LexSize) 
     ; ( ( LexSize = 0 ), % empty lexicon
         lgi_set_counter(l,LexSize) ) ),
   lgi_set_counter(lexsize,LexSize),  
   lgi_message( lexicon_count(LexSize) ).
  

%% ----------------------------------------------------------
%%   initialization steps
%% ----------------------------------------------------------

%% lgi_init_compile_lexicon/0
%%   - initializations for the lexicon compiler
%%
lgi_init_compile_lexicon :-
   lgi_reset_lex_index,
   lgi_set_counter(l,0),        % lexicon counter
   retractall(hl(_,_,_,_)), % initialize internal lexicon code - lexical hypos
   retractall(l(_,_,_,_,_)),  % initialize internal lexicon code - words
   % reinitialize the nonterminal hierarchy
   retractall('LGnt'(_)),
   lgi_mk_nt_hierarchy(nonterminal,Hierarchy),
   assert('LGnt'(Hierarchy)),
   retractall('LGusednt0'(_,_)),
   retractall('LGusednt'(_,_)).


% info local to a parse
lgi_reset_goal_hypos :-
   retractall(hc(_,_,_,_)).
   % initialize internal lexicon code - goal hypos

%% ----------------------------------------------------------
%%   termination steps
%% ----------------------------------------------------------

lgi_compress_nt_info :-
   clause( 'LGusednt0'(Type,UnifS), _ ), % assumes unique occurrences
   lgi_compress_nt_info(UnifS,UnifSCompressed),
   assert( 'LGusednt'(Type,UnifSCompressed) ),
   fail.

lgi_compress_nt_info.


lgi_compress_nt_info([NT|UnifS],UnifSOut) :-
   ( clause( 'LGusednt0'(NT,_UnifS1), _ )  % NT is used
   -> ( UnifSOut = [NT|UnifSOut1] )
   ;  ( \+ clause( 'LGusednt0'(NT,_UnifS1), _ ) % NT is not used
        -> ( UnifSOut = UnifSOut1 ) ) ),
   lgi_compress_nt_info(UnifS,UnifSOut1).

lgi_compress_nt_info([],[]).



%% ----------------------------------------------------------
%%   compilation of a category
%% ----------------------------------------------------------

%% ResultCatName :== cn1(CNType,CatNameAtom)
%% ThreadedCatName :== 
%%     cn0(InnerCNType,InnerCatNameAtom,CNType,CatNameAtom)
%% CNType :== 'ATOM' | 'COMPLEX'


lgi_compile_cat(Word,CategoryFS,GoalS,ConstrS) :-
   lgi_expand_premise(CategoryFS,Category,SlashS,Word),
   !,   % commit to this clause
   ( Category = cat( _CT1,root(RootType,_RFS1),
                     lvs(LLength,_L1,_RL1,_RemL1), _CtrS1,
                     node(_CN1,Id,[leaf(Word)]) ) ), % insert lexical tree
   lgi_inc_counter(l,Id),
   lgi_mk_index_add( word, ii(RootType,Word,_HypoS2,LLength), Id),
   % before assert/1 - instantion of HypoIdS !:
   lgi_add_hypos(SlashS,hl,Id,SlashIdS), 
   assert( l(Id,Category,GoalS,ConstrS,SlashIdS) ).

lgi_compile_cat(Word,CategoryFS,_GoalS,_ConstrS) :-
   lgi_error( unexpandable_category( db(Word,_Path,CategoryFS) ) ).


%% lgi_add_hypos(+SlashS,+Flag,+LicId,-SlashIdS)
%% - assert the hypotheses SlashS
%% - Flag :== hl | hc.   % lexical hypos vs. topmost goal hypos
%%
lgi_add_hypos( [ s(Id,Hypo) | SlashS ], Flag, LicId, [Id|IdS] ) :-
   lgi_add_hypo(Hypo,Id,LicId,Flag),
   lgi_add_hypos(SlashS,Flag,LicId,IdS).

lgi_add_hypos([],_Flag,_Licid,[]).


lgi_add_hypo(Category,Id,LicId,Flag) :-
   concat(i,LicId,Trace),
   ( Category = cat( _CT1,root(RootType,_RFS1),
                     lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,
                     node(_CN1,Id,[leaf(Trace)]) ) ),  % insert lexical tree
   lgi_inc_counter(l,Id),
   lgi_mk_index_add( trace(Flag), ii(RootType,_W2,_H2,LLength), Id ),
   Head =.. [Flag,Id,Category,[],[]],
( ( Flag = hc )   % for 'chart hypos' 
  -> ( write(Id),put(44) )
  ;  true ),
   assert(Head).             % GoalS, ConstrS empty, handled by emitter


%% ----------------------------------------------------------
%%   category expansion
%%
%% - translate CategoryFS into a Prolog term Category
%% - add 'NT' (compiled nonterminal info) and 'SLASH-IDS' information 
%% ----------------------------------------------------------

%% lgi_expand_premise(+CategoryFS,-Category,-SlashS,+Word)
%% lgi_expand_premise(+CategoryFS,-Category,+SlashAccu,-SlashS,+DebugInfo)
%% - SlashS :== list( s(ItemId,Category) )
%% - DebugInfo :== db(Word,InvertedPath,CategoryFS)
%%
lgi_expand_premise(CategoryFS,Category,SlashS,Word) :-
   lgi_expand_premise( CategoryFS, Category, [], SlashS, _CNTCN1,
                       db(Word,[],CategoryFS) ),
   % 1. mark Category as a word
   % 2. word is its own leftcorner
   ( Category = cat( WordCatType, _R1, _L1,          
                     ctrs(WordCatType,_CF1), 
		     _Tree1 ) ),
   cuf_extern2intern(word,_,_,WordCatType).


lgi_expand_premise(
      CategoryFS,
      Category,
      SlashSIn, SlashSOut,
      cn1(CNType,CatName),
      db(Word,Path,TopCategoryFS) ) :-
   ( cuf_path_value(CategoryFS,[root],RootFS)
     -> lgi_expand_root(
           RootFS,
           Root,
           RootName,
           db( Word, [root|Path], TopCategoryFS) )
     ; lgi_warning( missing_feature( db( Word, [root|Path], TopCategoryFS))) ),
   ( % leaves:LeavesFS &
     cuf_path_value(CategoryFS,[leaves],LeavesFS)
     -> ( lgi_expand_leaves(
             LeavesFS,
             [],
             _RevLeaves,
             cn0('ATOM',RootName,CNType,CatName),
             SlashSIn, SlashSOut,
             Category,  % head
             db( Word, [leaves|Path], TopCategoryFS) ),
          ( Category = 
              cat( ConstType, Root, _L6, _DConstrS6, _PI6 ) ),
          % produce an FS which contains only the ConstituentType information
          ( ( cuf_get_type(CategoryFS,TypeCode),
              cuf_put_type(TypeCode,ConstType) )
              -> true )  % no backtracking for this part % because of CUF bug
         )
     ; ( lgi_error( missing_feature( 
                         db( Word, [leaves|Path], TopCategoryFS) ) ),
         fail ) ).


lgi_expand_goal(
      CategoryFS,
      arg( ConstituentType, Dir, Root, Leaves, HypoSInt, 
           ctrs(LCCType,CompletionFlag), 
           _PI1, _HP1 ),
      SlashSIn, SlashSOut,
      cn1(CNType,CatName),
      db(Word,Path,TopCategoryFS) ) :-
   lgi_expand_premise(
      CategoryFS,
      cat( ConstituentType, Root, Leaves, _Ctrs2, _PI2 ),
      SlashSIn, SlashS1,
      cn1(CNType1,CatName1),
      db(Word,Path,TopCategoryFS) ),
   cuf_put_path(CategoryFS,[dir],DirCUF),
   ( cuf_atom(Dir,DirCUF) % CUF compil/er checked already for 'left' or 'right'
     -> true
    ; cuf_var(DirCUF) ), % leave Dir unspecified
   ( cuf_path_value(CategoryFS,[slash],Slash)
     -> lgi_expand_slash(
           Slash,
           HypoS,
           cn0(CNType1,CatName1,CNType,CatName),
           SlashS1,SlashSOut,
           db(Word,[slash|Path],TopCategoryFS) )
     ; ( lgi_warning( missing_feature( db(Word,[slash|Path],TopCategoryFS) ) ),
         lgi_message( default_feature_value([slash|Path],[]) ),
         ( HypoS = [] ),
         ( CNType = CNType1 ),
         ( CatName = CatName1 ),
         ( SlashSOut = SlashS1 ) ) ),
   ( ( lgi_hypos2ext(HypoSInt,HypoS),
       cuf_put_path(CategoryFS,[constraints],ConstraintSFS),
       cuf_put_path(ConstraintSFS,[completion_state],CompletionFlag),
       cuf_put_path(ConstraintSFS,[leftcorner],LCConstraintFS),
       % produce an FS which contains only the type information
       cuf_get_type(LCConstraintFS,LCTypeCode),
       cuf_put_type(LCTypeCode,LCCType) )
     -> true ).  % no backtracking for this part


lgi_expand_root(RootFS,root(IntRoot,RootFS),RootName,
                db(Word,Path,TopCategoryFS) ) :-
   ( cuf_path_value(RootFS,[syn],NT)
     -> ( lgi_classify_nt(NT,IntRoot,RootName)
          -> true   % no backtracking
          ;  ( % \+ lgi_classify_nt(NT,_IntRoot,_RootName),
               lgi_warning(
                  not_nt_classifiable(
                     NT,
                     db(Word,[syn|Path],TopCategoryFS) ) ),
               ( RootName = top ) ) )
      ;  ( lgi_warning( 
              missing_feature( db( Word, [syn|Path], TopCategoryFS ) ) ),
           ( RootName = top ) ) ).


lgi_expand_leaves(
       LeavesFS,
       FrontLeaves,
       RevLeaves,
       cn0(InnerCNType,InnerCatName,CNType,CatName), 
       SlashSIn, SlashSOut,
       cat( _CT3, Root,                    % head
            lvs(LLength,Leaves,RevLeaves,_RemL3),
            ctrs(LCCandIn,CFlag), 
            node(CatName,HeadId,_HeadDtrs) ),
       db(Word,Path,TopCategoryFS) ) :-
   ( ( cuf_path_value(LeavesFS,['F'],ArgFS),
       cuf_path_value(LeavesFS,['R'],RestLeavesFS) )
     -> ( lgi_expand_goal(
             ArgFS,
             Arg,
             SlashSIn,SlashS1,
             cn1(ArgCNType,ArgCatName),
             db( Word, ['F'|Path], TopCategoryFS ) ),
          lgi_expand_leaves(
             RestLeavesFS,
             FrontLeaves1,
             RestRevLeaves,
             cn0(InnerCNType,InnerCatName,HPCNType,HPCatName),
             SlashS1, SlashSOut,
             HeadProjection,
             db( Word, ['R'|Path], TopCategoryFS ) ),
          ( HeadProjection = 
              cat( HeadProjCatType, Root, 
                   lvs(RestLLength,RestLeaves,_RL2,_RemL2),
                   ctrs(LCCand2,CFlag), 
                   HeadProjTree) ),
          ( HeadProjectionSketch =  % omit leaves to avoid cyclic structure
              hp( HeadProjCatType, Root, RestLLength,
                   ctrs(LCCand2,CFlag), 
                   HeadProjTree) ),
          append( RestRevLeaves, [ a(Arg,FrontLeaves1)], RevLeaves ),
          append( FrontLeaves, [ Arg ], FrontLeaves1 ),  
          cuf_extern2intern(derived_phrase,_,_,HeadProjCatType),
          ( LLength is RestLLength + 1 ),
          ( Arg = arg( _CT1, Dir, _R1, _L1, _H1,
                       ctrs(ArgLC,_CF1), 
                       node(_ACN,ArgId,_ADtrs),
                       HeadProjectionSketch ) ),
          lgi_leftcorner_candidate(Dir,LCCandIn,ArgLC,LCCand2),
          ( HeadProjTree = node(_HPCN,HPId,_HPDtrs) ),
          lgi_mk_tree(Dir,HPCatName,HPId,HeadId,ArgId,HeadProjTree),
          lgi_mk_cgname( Dir, HPCNType, HPCatName,
                         ArgCNType, ArgCatName, CNType, CatName ),
          ( Leaves = [ Arg | RestLeaves ] ) )
     ;  ( cuf_atom(Leaves,LeavesFS)  % Leaves = []
          -> ( ( CNType = InnerCNType ),
               ( CatName = InnerCatName ),
               ( SlashSOut = SlashSIn ),
               ( LLength = 0 ),
               ( RevLeaves = [] ) )
         ; ( % cuf_var(LeavesFS) % or maybe something else
             lgi_error(
                missing_feature( db(Word,Path,TopCategoryFS) ) ),
             fail ) ) ).


lgi_expand_slash(
      HypoSFS,
      HypoS,
      cn0(InnerCNType,InnerCatName,CNType,CatName), 
      SlashSIn, SlashSOut,
      db(Word,Path,TopCategoryFS) ) :-
   ( % non empty slash
     cuf_path_value(HypoSFS,['F'],HypoFS),
     cuf_path_value(HypoSFS,['R'],RestSlashFS) )
   -> ( lgi_expand_premise(
           HypoFS,
           Hypo,
           SlashSIn,SlashS1,
           cn1(HypoCNType,HypoCatName),
           db(Word,['F'|Path],TopCategoryFS) ),
        lgi_expand_slash(
           RestSlashFS,
           RestSlash,
           cn0(InnerCNType,InnerCatName,CNType1,CatName1), 
           SlashS1,SlashSOut1,
           db(Word,['R'|Path],TopCategoryFS) ),
        ( HypoS =  [ s(HId,Hypo) | RestSlash ] ),
        ( Hypo = cat( HypoCatType, _R2, _L2, ctrs(HypoCatType,_CF2),
                      node(_HCN,HId,_HDtrs) ) ),
        cuf_extern2intern(trace,_,_,HypoCatType),
        lgi_mk_cgname( 'HYPO',CNType1,CatName1,HypoCNType,
                       HypoCatName,CNType,CatName),
        ( SlashSOut = [ s(HId,Hypo) | SlashSOut1 ] ) )
   ;  ( ( CNType = InnerCNType ),
        ( CatName = InnerCatName ),
        ( SlashSOut = SlashSIn ),
        ( HypoS = [] ),
        ( cuf_atom([],HypoSFS) % end of slash / empty slash
          -> true
          ;  ( cuf_var(HypoSFS) 
               -> ( lgi_warning( 
                       missing_feature( db(Word,Path,TopCategoryFS) ) ),
                    lgi_message( 
                       default_feature_value(Path,[]) ),
                    ( HypoS = [] ) ) ) ) ).


%% ----------------------------------------------------------
%%   hierarchy construction
%% ----------------------------------------------------------

%% lgi_mk_nt_hierarchy(+TypeSymbolProlog,-Hierarchy)
%% lgi_mk_nt_hierarchy(+TypeSymbolProlog,+Upper,-Lower,-Hierarchy)
%% - Hierarchy
%%   Node ::= t(TypeSymbolCUF,r(Type,UnifiableTypeS),BasicCatName)
%%   Hierarchy ::= h(Node,NodeList)
%%
lgi_mk_nt_hierarchy(Type,Hierarchy) :-
   lgi_mk_nt_hierarchy(Type,[],_Lower,Hierarchy).
 

lgi_mk_nt_hierarchy( 
      Type, 
      Upper,
      [Type|Lower],
      h( s(TypeFS,r(Type,UnifS),CatName), Hierarchies ) ) :-
   cuf_if_lookup_type_kids(Type,Kids),
   lgi_type_offspring(Kids,[Type|Upper],Lower,Hierarchies),
   append(Upper,[Type|Lower],UnifS),
   % construct an FS for Type for later subsumption checks
   cuf_extern2intern(Type,_,_,TypeFS), 
   ( ( current_predicate(abbreviated_name,_),
       abbreviated_name(Type,CatName) )  % look up abbreviation
     -> true
     ; CatName = Type ).


lgi_type_offspring(
      [ Kid | Kids ],
      Upper,
      Lower,
      [ Hierarchy | Hierarchies ] ) :-
   lgi_mk_nt_hierarchy(Kid,Upper,Lower1,Hierarchy),
   append(Lower1,Lower2,Lower),
   lgi_type_offspring(Kids,Upper,Lower2,Hierarchies).

lgi_type_offspring([],_Upper,[],[]).



%% ----------------------------------------------------------
%%   classifier
%% ----------------------------------------------------------

%% lgi_classify_nt(+TypeCUF,-IntTypeOut,-CatNameOut)
%% lgi_classify_nt(+TypeCUF,+Hierarchy,+IntTypeIn,-IntTypeOut,
%%                 +CatNameIn,-CatNameOut)
%% - classify TypeCUF in Hierarchy, and return its IntType
%% - Hierarchy, IntType: same as in lgi_mk_nt_hierarchy/3
%% - failure, if TypeCUF cannot be classified under any node
%%
lgi_classify_nt(TypeCUF,Type,CatName) :-
   clause('LGnt'(Hierarchy),_)
   -> ( lgi_classify_nt(TypeCUF,Hierarchy,_,r(Type,UnifS),_,CatName),
        ( clause( 'LGusednt0'(Type,UnifS), _ )
          -> true
          ;
	( ( \+ clause( 'LGusednt0'(Type,UnifS), _ ) )
               -> assert( 'LGusednt0'(Type,UnifS), _ ) ) ) )
   ; ( lgi_error(no_nt_hierarchy),
       fail ).


lgi_classify_nt(
      TypeCUF,
      h( s(MotherCUF,IntType1,CatName1), Hierarchies ), 
      _TM,
      IntType,
      _CN,
      CatName ) :-
   cuf_subsumes(MotherCUF,TypeCUF),
   lgi_classify_nt_l(Hierarchies,TypeCUF,IntType1,IntType,
		     CatName1,CatName).


lgi_classify_nt_l(
      [ Hierarchy | Hierarchies ],
      TypeCUF,
      IntTypeIn,
      IntTypeOut,
      CatNameIn,
      CatNameOut ) :-
   lgi_classify_nt(TypeCUF,Hierarchy,IntTypeIn,IntTypeOut,
                   CatNameIn,CatNameOut)
   -> true  % exclude second alternative
   ;
   lgi_classify_nt_l(Hierarchies,TypeCUF,IntTypeIn,IntTypeOut,
                     CatNameIn,CatNameOut).

% not subsumed by any of the daughters ==> Mother is smallest supertype
lgi_classify_nt_l([],_TypeCUF,IntType,IntType,CatName,CatName).



%% ----------------------------------------------------------
%%   hierarchy grapher
%% ----------------------------------------------------------

%%  lgi_collect_nt_hierarchy(+CompressedHierarchy)
%%
lgi_collect_nt_hierarchy(CompressedHierarchy) :-
   clause( 'LGnt'(Hierarchy), _ )
   -> lgi_collect_nt_hierarchy1(Hierarchy,CompressedHierarchy) 
   ;  ( lgi_error(no_nt_hierarchy),
        fail ).
  

lgi_collect_nt_hierarchy1(
      h( s(_MotherCUF,_TypeMellish,CatName), Hierarchies ),
      CHierarchy ) :-
   lgi_collect_nt_hierarchies(Hierarchies,CHierarchies),
   CHierarchy =.. [CatName|CHierarchies].


lgi_collect_nt_hierarchies(
      [ Hierarchy | Hierarchies ],
      [ CHierarchy | CHierarchies ] ) :-
   lgi_collect_nt_hierarchy1(Hierarchy,CHierarchy),
   lgi_collect_nt_hierarchies(Hierarchies,CHierarchies).

lgi_collect_nt_hierarchies([],[]).


%% ----------------------------------------------------------
%%   transform Prolog catname into a Prolog atom
%% ----------------------------------------------------------

%% lgi_mk_cgname(Direction,+ValCNType,+ValueCatName,
%%               +ArgCNType,+ArgCatName,-CNType,-CatName)
%% - produce a category description for a complex category
%% - reconstruct CatFS
%% - Direction :== left | right | Variable | 'HYPO'
%
lgi_mk_cgname( 
      Dir,
      ValCNType,ValCatName, 
      ArgCNType,ArgCatName, 
      'COMPLEX', CatName ) :-
   ( ( ( Dir = 'HYPO') ; var(Dir) )
     -> ( Op = '|' )
     ; ( ( Dir = left )
          -> ( Op = '\\' )
          ; ( ( Dir = right )
              -> ( Op = (/) ) ) ) ),
   lgi_mk_cgname2(ValCNType,ValCatName,ValCatName1),
   lgi_mk_cgname2(ArgCNType,ArgCatName,ArgCatName1),
   concat(Op,ArgCatName1,PrefixedArg),
   concat(ValCatName1,PrefixedArg,CatName).


%% lgi_mk_cgname2(+CNType,+CatName,-ExtendedCatName) 
%% - add parentheses if complex category
%%
lgi_mk_cgname2(CNType,CatName,ExtendedCatName) :- 
   ( CNType = 'ATOM' )
   -> ( ExtendedCatName = CatName )
   ; ( ( CNType = 'COMPLEX' )
       -> ( concat( CatName, ')', ExtendedCatName1 ),
            concat( '(', ExtendedCatName1, ExtendedCatName ) ) ).


%% ----------------------------------------------------------
%%   subsumption routine for internal category structures
%% ----------------------------------------------------------

%% lgi_subsumes_cat( 
%%    c1(Cat1,GoalS1,ConstrS1), 
%%    c1(Cat2,GoalS2,ConstrS2) )
%% - incorrectness: goals and constraints are ignored
%% - print info is never checked
%% - cattype, leftcorner, and completion flag are bottom-up info
%%   hence it doesn't make much sense to check them for subsumption
%% 
lgi_subsumes_cat(
      c1( cat( _CT1,
               root(_RootType1,RootFS1), 
               lvs(LLength,Leaves1,_RevL1,_RemL1), 
               _DCtrs1, _PI1 ),
          _GoalS1, 
          _ConstrS1 ),
      c1( cat(_CT2,
              root(_RootType2,RootFS2), 
              lvs(LLength,Leaves2,_RevL2,_RemL2),
              _DCtrs2, _PI2 ),
          _GoalS2, 
          _ConstrS2 ) ) :-
    cuf_subsumes(RootFS1,RootFS2),   
    ( ( var(Leaves1) ; var(Leaves2) )
      -> lgi_error( instantiation_error('expcat:lgi_subsumes_cat/2') )
      ;  % subsumption relation must be inverted for premises
         lgi_subsumes_leaves(Leaves2,Leaves1) ).


%% lgi_subsumes_leaves(GoalLeaves,PremiseLeaves)
%%
lgi_subsumes_leaves(Leaves1,Leaves2) :-
   ( Leaves1 = Leaves2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( Leaves1 = [ arg( CatType1,Direction,Root1,Leaves11,HypoS1,DConstrS1,
                        _PI1,_HP1)
                   | RestLeaves1 ],
       Leaves2 = [ arg( CatType2,Direction,Root2,Leaves21,HypoS2,DConstrS2,
                        _PI2,_HP2)
                   | RestLeaves2 ],
      lgi_subsumes_cat(
         c1( cat(CatType1,Root1,Leaves11,DConstrS1,_PI3), [], [] ),
         c1( cat(CatType2,Root2,Leaves21,DConstrS2,_PI4), [], [] ) ),
      % subsumption relation must be inverted for premises
      lgi_subsumes_hypos(HypoS2,HypoS1),  
      lgi_subsumes_leaves(RestLeaves2,RestLeaves1) ).

%%
%%
lgi_subsumes_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for s([],[]) )
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,Cat2) | RestHypoS2 ],
      lgi_subsumes_cat(
         c1(Cat1,[],[]),
         c1(Cat2,[],[]) ), 
      lgi_subsumes_hypos(RestHypoS1,RestHypoS2) ).


%% ----------------------------------------------------------
%%  identity of hypo lists
%% ----------------------------------------------------------

%%
%%
lgi_same_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for s([],[]) )
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,_Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,_Cat2) | RestHypoS2 ],
      lgi_same_hypos(RestHypoS1,RestHypoS2) ).


%% ----------------------------------------------------------
%%   unification routine for internal category structures
%% ----------------------------------------------------------


%% lgi_unify_cat( 
%%    c1(Cat1,GoalS1,ConstrS1), 
%%    c1(Cat2,GoalS2,ConstrS2),
%%    c1(_ResultCat,GoalSOut,ConstrSOut) )
%% !!! currently ResultCat is uninstantiated
%% - no unification of 'print info' 
%%
lgi_unify_cat(
      c1( cat( CatType1,root(_RootType1,RootFS1), 
               lvs(LLength,Leaves1,_RevL1,_RemL1), 
          ctrs(LC1,Flag1), _PI1 ),
          GoalS1, ConstrS1 ),
      c1( cat( CatType2,root(_RootType2,RootFS2), 
               lvs(LLength,Leaves2,_RevL2,_RemL2),
          ctrs(LC2,Flag2), _PI2 ),
          GoalS2, ConstrS2 ),
      c1( _Cat,     % currently ignored
          GoalSOut, ConstrSOut ) ) :-
    cuf_unify(CatType1,CatType2),
    cuf_unify(RootFS1,RootFS2),   
    ( ( var(Leaves1) ; var(Leaves2) )
      -> lgi_error( instantiation_error('expcat:lgi_unify_cat/3') )
      ;  ( lgi_unify_leaves(Leaves1,Leaves2),
           cuf_unify(LC1,LC2),      % leftcorner/leftcorner constraint
           lgi_unify( d(Flag1,GoalS1,ConstrS1),  % completion flag
                      d(Flag2,GoalS2,ConstrS2),
                      d(_,    GoalSOut,ConstrSOut) ) ) ).


%% lgi_unify_leaves(GoalLeaves,PremiseLeaves)
%%
%% - HypoCatS have to be guaranteed to be identical 
%%   due to abstraction operation, hence they can be ignored here
%%
lgi_unify_leaves(Leaves1,Leaves2) :-
   ( Leaves1 = Leaves2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( Leaves1 = [ arg( CatType1,Direction,Root1,Leaves11,HypoS1,ConstrS1,
                        _PI1,_HP1)
                   | RestLeaves1 ],
       Leaves2 = [ arg( CatType2,Direction,Root2,Leaves21,HypoS2,ConstrS2,
                        _PI2,_HP2)
                   | RestLeaves2 ],
      lgi_unify_cat(
         c1( cat(CatType1,Root1,Leaves11,ConstrS1,_PI3), [], [] ),
         c1( cat(CatType2,Root2,Leaves21,ConstrS2,_PI4), [], [] ),
         _Result ),
      lgi_unify_hypos(HypoS1,HypoS2),
      lgi_unify_leaves(RestLeaves1,RestLeaves2) ).


%%
%%
lgi_unify_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,Cat2) | RestHypoS2 ],
      lgi_unify_cat(
         c1(Cat1,[],[]),
         c1(Cat2,[],[]),
         _ ),
      lgi_unify_hypos(RestHypoS1,RestHypoS2) ).


%% lgi_split_leaves(+GoalReversedLeaves,+PremiseReversedLeaves,-RestLeaves)
%% - split PremiseReversedLeaves into GoalReversedLeaves 
%%   and RestLeaves to be parsed


lgi_split_leaves(
      [],
      [ a(_Arg,FrontLeaves) | _RestLeaves ],
      FrontLeaves ) :-
   !.

lgi_split_leaves(
      [ a( arg(CatType1,Direction,Root1,Leaves11,HypoS,ConstrS1,_PI1,_HP1 ),
           _RestLeaves1 )
        | RestLeaves1 ],
      [ a( arg(CatType2,Direction,Root2,Leaves21,HypoS,ConstrS2,_PI2,_HP2 ),
           _RestLeaves2 )
        | RestLeaves2 ],
      FrontLeaves ) :-
   lgi_unify_cat(
      c1( cat(CatType1,Root1,Leaves11,ConstrS1,_PI3), [], [] ),
      c1( cat(CatType2,Root2,Leaves21,ConstrS2,_PI4), [], [] ),
      _Result ),
   lgi_split_leaves(RestLeaves1,RestLeaves2,FrontLeaves).


%% ----------------------------------------------------------
%%  reverse compilation: calculate FS from cat/ term (for chart browser)
%% ----------------------------------------------------------

%% lgi_cat2fs(+Category,-CatFS)
%% - produce a feature structure which corresponds to Category
%
lgi_cat2fs(
      cat( CatFS,   % Const Type info
           root(_RT1,RootFS),
           lvs(_LL,Leaves,_RL1,_RemL1), 
           _CtrS1, _PI1 ),
      CatFS ) :-   % insert Const Type info
   cuf_put_path(CatFS,[root],RootFS),
   cuf_put_path(CatFS,[leaves],LeavesFS),
   lgi_leaves2fs(Leaves,LeavesFS).


lgi_leaves2fs([],LeavesFS) :-
   !,
   cuf_atom([],LeavesFS).

lgi_leaves2fs(
      [ arg(CatType,_Dir,Root,Leaves1,HypoS,ctrs(LC,Compl),_PI1,_HP1)
        | RestLeaves ],
      LeavesFS) :-
   cuf_extern2intern(nelist,_,_,LeavesFS), 
   cuf_put_path(LeavesFS,['F'],ArgFS),
   cuf_put_path(LeavesFS,['R'],RestLeavesFS),
   cuf_put_path(ArgFS,[slash],HypoSFS),
   cuf_put_path(ArgFS,[constraints],ConstraintSFS),
   cuf_put_path(ConstraintSFS,[completion_state],Compl),
   cuf_put_path(ConstraintSFS,[leftcorner],LC),
   lgi_cat2fs(
      cat(CatType,Root,Leaves1,_ConstrS,_PI2),
      ArgFS ),
   lgi_hypos2fs(HypoS,HypoSFS),
   lgi_leaves2fs(RestLeaves,RestLeavesFS).


lgi_hypos2fs( [], HypoSFS ) :-
   cuf_atom([],HypoSFS).

lgi_hypos2fs(
      [ s( _HId, 
           cat( CatType,Root,Leaves,_ConstrS,_PI1) )
        | RestHypoS ],
      HypoSFS ) :-
   cuf_extern2intern(nelist,_,_,HypoSFS), 
   cuf_put_path(HypoSFS,['F'],HypoFS),
   cuf_put_path(HypoSFS,['R'],RestHypoSFS),
   lgi_cat2fs(
      cat(CatType,Root,Leaves,_ConstrS,_PI2),
      HypoFS ),
   lgi_hypos2fs(RestHypoS,RestHypoSFS).


%% --- END OF FILE: expcat.pl


%% Remarks:
%% - little ambiguous grammars faster without subsumption check for
%%%      File: hdparser.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: CUF-based semi-directional Lambek calculus       %%%
%%%            - parsing variant -                              %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Nov  8 15:42:52 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% 95-01    Peter Krause: original Prolog version
%% 94-06-28 Esther Koenig: latest version of hdparse.cuf 
%% Exports:
%%   lgi_parse/3
%% Imports:
%%   Types:
%%     cat  and embedded types + subtypes
%%   Predicates:
%%     cuf_unify/2
%%     hpt/2        % head projection table
%%     lgi_add_gtable_entry/2
%%     lgi_add_hptable_entry/2
%%     lgi_fitting_gtable/2 
%%     lgi_fitting_hptable/4
%%     lgi_get_active_item/2
%%     lgi_get_passive_item/2
%%     lgi_guess_premises/4
%%     lgi_get_gtable_entry/4
%%     lgi_get_hptable_entry/4
%%     lgi_hypos_union/3
%%     lgi_ne_premises/2
%%     lgi_new_gtable/2
%%     lgi_new_hptable/4
%%     lgi_string_union/3
    
% ------------------------------------------------------------
% Contents:
% 1. choice of head
% 2. parsing the arguments of the head
% ------------------------------------------------------------

%%
%% lgi_parse1(?GoalInfo,-OutInfo,-ItemId)
%% - find a parse for GoalInfo
%% - Result = instantiation of ?GoalInfo + -OutInfo (Goals and Constraints)
%% Remarks: 
%% - It does not matter which of the subsuming previous goals G, G', ...
%%   is picked, since ||Goal|| = ||G|| intersect ||G'|| subset ||G|| etc.
%% - for ressource checks: cleaner would be to use interface  pred's
%%

% ------------------------------------------------------------
% 1. choice of head
% ------------------------------------------------------------

% remark: some redundant work is not excluded, because intermediate projections
% (which previously did not have a goal as a counterpart) are not
%  checked
%%%    ---> Zugriff auf intermediate head projections in
%%         'get_active_item' einbauen bzw. ala van Noord
%%          DifferenzenListen abspeichern

   
lgi_parse(Goal,Out,ItemId) :-
   ( % IF Goal is subsumed by some goal in a goal table
     lgi_fitting_gtable(Goal,GTableId) 
     ->  % lookup existing solutions - there may be none
         lgi_get_gtable_entry(Goal,GTableId,Out,ItemId)
     ;  ( % ELSE IF Goal is not subsumed by any goal in a goal table
          % THEN create a new table header
          ( lgi_new_gtable(Goal,GTableId) -> true ),
          ( ( lgi_new_parse(Goal,ItemId),
              ( lgi_add_gtable_entry(GTableId,ItemId) -> true ),
              fail )  % assert all solutions for this Goal
            ; lgi_get_gtable_entry(Goal,GTableId,Out,ItemId) ) ) ).


% a fitting passive non-derived item
lgi_new_parse(Goal,ItemId) :-
   lgi_get_passive_item(Goal,ItemId).

% a new derivation from an active non-derived item
lgi_new_parse(Goal,ItemId) :-
   ( ( ( Goal = c(_S1,_H1,cat(CatType,_R1,_L1,_DC1,_PI1),_G1,_C1) ),
       % to avoid CUF compiler error messages :
       cuf_extern2intern(derived_phrase,_,_,CatType1), 
       cuf_unify(CatType,CatType1) ) -> true ),
   lgi_get_active_item(Goal,Head,[Arg|Leaves],In,HeadId),  % backtrackable
   lgi_parse_args(Leaves,In,Arg,Goal,HeadId,Head,ItemId).


% ------------------------------------------------------------
% 2. parse arguments
% ------------------------------------------------------------

%% lgi_parse_args(+RestLeaves,+InInfo,+Arg,?Goal,+HeadId,+Head,-ItemId)
%% - completor step
%% - clauses for single argument check adjacency constraints 
%%   and that all remaining resources have been used
%%
% only one argument left  (leftward argument)
lgi_parse_args( 
      [],
      cin( ArgString, s(Pos,Pos), ArgHypoSPrime ),
      arg(CT1,left,R1,L1,H1,DC1,PI1,HP1),
      Goal,
      HeadId, Head,
      ItemId ) :-
   lgi_parse_last_arg( 
      ArgString, ArgHypoSPrime,
      arg(CT1,_D1,R1,L1,H1,DC1,PI1,HP1),
      Goal,
      HeadId, Head,
      ItemId ).

% only one argument left  (rightward argument)
lgi_parse_args( 
      [],
      cin( s(Pos,Pos), ArgString, ArgHypoSPrime ),
      arg(CT1,right,R1,L1,H1,DC1,PI1,HP1),
      Goal,
      HeadId, Head,
      ItemId ) :-
   lgi_parse_last_arg( 
      ArgString, ArgHypoSPrime,
      arg(CT1,_D1,R1,L1,H1,DC1,PI1,HP1),
      Goal,
      HeadId, Head,
      ItemId ).

% more than one remaining argument 
lgi_parse_args( 
      [Arg2|Leaves],  
      In,
      arg( ArgCatType, ArgDir, ArgRoot, ArgLeaves, ToBindHypoS, ArgDConstrS,
           node(_ACN,ArgId,_ADtrs),
           hp( HPCatType, Root, HPLLength, HPDConstrS, HPPI ) ),
      Goal,
      HeadId, 
      c( HeadString,HeadHypoS,   % head
         cat( _CT2, _R2, 
              lvs(_LL2,[_Arg1|AllLeaves],RevL,_RemL2), 
              _DC2, _PI2 ),
         GoalSIn, ConstrSIn ),
      ItemId ) :-
   lgi_guess_premises(  % guess premises for argument goal
      ArgDir, In,
      c(ArgString,ArgHypoSPrime,_Cat15,_G15,_C15),
      In1 ),
   ( % IF HeadProj of HeadId has been constructed earlier from these ressources
     lgi_fitting_hptable(HeadId,ArgString,ArgHypoSPrime,HPTableId)
        % THEN stick to these results
     -> lgi_get_hptable_entry(HPTableId,HeadProj,[Arg2|Leaves],HeadProjId)
     ;  % ELSE 
        ( % new head projection table
          ( lgi_new_hptable(HeadId,ArgString,ArgHypoSPrime,HPTableId)
            -> true ),
          ( ( % derive next head projection
              ( lgi_hypos_union(ToBindHypoS,ArgHypoSPrime,ArgHypoS)
                 -> true ),
              lgi_parse(              % parse current argument 
                 c( ArgString, ArgHypoS,                   % arg goal
                    cat(ArgCatType,ArgRoot,ArgLeaves,ArgDConstrS,_P12), 
                    GoalSIn, ConstrSIn ),
                 cth(GoalS1,ConstrS1),
                 ArgId ),
              ( ( % construct head projection
                  append(HPRevL,[_],RevL),        % cut off current arg
                  lgi_string_union(HeadString,ArgString,HeadProjString),
                  lgi_hypos_union(HeadHypoS,ArgHypoSPrime,HeadProjHypoS),
                  ( HPPI = node(_HPCN,HeadProjId,_HPDtrs) ),
                  lgi_add_derived_item(
                     c( HeadProjString, HeadProjHypoS,
                        cat( HPCatType, Root,
                             lvs(HPLLength,AllLeaves,HPRevL,[Arg2|Leaves]),
                             HPDConstrS, HPPI ),
                        GoalS1, ConstrS1 ),
                     HeadProjId ),
                  lgi_add_hptable_entry(HPTableId,HeadProjId) )
                -> true ),
              fail )   % create all variants of this projection
          ;  lgi_get_hptable_entry( HPTableId, HeadProj, [Arg2|Leaves], HeadProjId ) ) ) ),
   lgi_parse_args(Leaves,In1,Arg2,Goal,HeadProjId,HeadProj,ItemId).


lgi_parse_last_arg( 
      ArgString, ArgHypoSPrime,
      arg( ArgCatType,_D1,ArgRoot,ArgLeaves,
           ToBindHypoS,ArgDConstrS,node(_ACN,ArgId,_ADtrs),
           hp(HPCatType,Root,HPLLength,HPDConstrS,HPPI) ),
      Goal,
      HeadId, 
      c( HeadString, HeadHypoS,                   % head
         cat( _CT3, _R3, 
              lvs(_LL3,[_Arg1|AllLeaves],RevL,_RemL3), _DC3, _PI3 ),
         GoalSIn, ConstrSIn ),  
      ItemId ) :-
   lgi_ne_premises(ArgHypoSPrime,ArgString),
   %%% almost same code as in recursive clause of lgi_parse_args/
   ( % IF HeadProj of HeadId has been constructed earlier from these ressources
     lgi_fitting_hptable(HeadId,ArgString,ArgHypoSPrime,HPTableId)
     -> % THEN stick to these results
        lgi_get_hptable_entry(HPTableId,_HeadProj,_Leaves,ItemId) 
     ;  % ELSE 
        ( % create new head projection table
          ( ( lgi_hypos_union(ToBindHypoS,ArgHypoSPrime,ArgHypoS),
              lgi_new_hptable(HeadId,ArgString,ArgHypoSPrime,HPTableId) )
            -> true ),
          ( ( % create new projection
              lgi_parse(              % parse current argument 
                 c( ArgString, ArgHypoS,                   % arg goal
                    cat(ArgCatType,ArgRoot,ArgLeaves,ArgDConstrS,_P12), 
                    GoalSIn, ConstrSIn ),
                 cth(GoalS1,ConstrS1),
                 ArgId ),
              ( ( ( Goal = c( _S2, GoalHypoS, GoalCat, _G2, _C2 ) ),
                  ( GoalCat =
                      cat( _CT2, _R2, _L2, ctrs(_LCC2,GoalCFlag), _PI2 ) ),
                  ( HPCat =
                      cat( HPCatType, Root,
                           lvs(HPLLength,AllLeaves,HPRevL,_RemL4),
                           HPDConstrS, HPPI ) ),
                  % recover bindings of hypos
                  lgi_hypos_union(HeadHypoS,ArgHypoSPrime,HeadProjHypoS),
                  lgi_unify_hypos(GoalHypoS,HeadProjHypoS),       
                  % instantiate completion flag
                  cuf_atom(done,GoalCFlag),  
                  % propagate synthesized info into Goal
                  % goals & constrs of Goal checked at head lookup
                  lgi_unify_cat( 
                     c1(GoalCat,[],[]), 
                     c1(HPCat,GoalS1,ConstrS1), 
                     c1(_,GoalS2,ConstrS2) ),
                  % construct head projection
                  append(HPRevL,[_],RevL),        % cut off current arg
                  lgi_string_union(HeadString,ArgString,HeadProjString),
                  ( HPPI = node(_HPCN,HeadProjId,_HPDtrs) ),
                  lgi_add_derived_item(
                     c( HeadProjString, HeadProjHypoS, HPCat,
                        GoalS2, ConstrS2 ),
                        HeadProjId ),
                  lgi_add_hptable_entry(HPTableId,HeadProjId) )
                 -> true ),
               fail )   % assert all variants of this projection
              ; % lookup projections
              lgi_get_hptable_entry(HPTableId,_HeadProj,_Leaves,ItemId) ) ) ).


%% --- END OF FILE: hdparser.pl





%%%      File: msgs.pl                                          %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: LexGram parser: messages, warnings, and errors   %%%
%%%   Created:                                                  %%%
%%%  Modified: Thu Jun 20 11:39:41 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_error/1
%%   lgi_message/1
%%   lgi_warning/1
%% Imports:
%%   concat/3               % CUF system / Prolog
%%   cuf_out/2              % CUF system

    

%% ----------------------------------------------------------
%%   auxiliary stuff
%% ----------------------------------------------------------

%% lgi_reversed_concat(+ListOfAtoms,+AccuAtom,-ConcatenatedAtom)
%% - inserts in addition a ':' between the elements
%%
lgi_reversed_concat([],CList,CList).

lgi_reversed_concat([Element|List],CAccu,CList) :-
   concat(':',CAccu,CAccu0),
   concat(Element,CAccu0,CAccu1),
   lgi_reversed_concat(List,CAccu1,CList).


lgi_warning1( Message ) :-
   cuf_out( message,
            ['LexGramP',warning,':' | Message ] ).


lgi_error1( Message ) :-
   cuf_out( message, 
            ['LexGramP',error,':' | Message ]).


%% ----------------------------------------------------------
%%   compiler messages
%% ----------------------------------------------------------

lgi_message(lexicon_compilation) :-
   cuf_out(message,['LexGramP:',lexicon,compilation,'$NL$']).

lgi_message(lexicon_count(LexSize)) :-
   cuf_out(message,[lexicon,entries,'(including','traces)',':',
                    LexSize,'$NL$']).

lgi_message( default_feature_value(Path,ValueCUFsource) ) :-
   lgi_reversed_concat(Path,'',ReversedPath), 
   cuf_out(message,
           ['LexGramP',assumes,':',ReversedPath,ValueCUFsource]).

%% ----------------------------------------------------------
%%   parser messages
%% ----------------------------------------------------------

lgi_message( runtime(What,RelTime) ) :-
   cuf_out(message,[What,':',RelTime]).


%% ----------------------------------------------------------
%%   compiler warnings
%% ----------------------------------------------------------

lgi_warning( missing_feature( db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_warning1( [no,value,specified,for,path,ReversedPath,
                  in,'$FS$'(CategoryFS),for,word,'<',Word,'>','$NL$'] ).

lgi_warning(not_nt_classifiable( NT, db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_warning1(['$FS$'(NT),cannot,be,classified,in,nonterminal,hierarchy,
                '/',value,of,path,ReversedPath,in,
                    category,'$FS$'(CategoryFS),for,word,
                   '<',Word,'>','$NL$' ] ).

%% ----------------------------------------------------------
%%   compiler errors
%% ----------------------------------------------------------

lgi_error( missing_feature( db(Word,Path,CategoryFS) ) ) :-
   lgi_reversed_concat(Path,'',ReversedPath),
   lgi_error1( [no,value,specified,for,path,ReversedPath,
                  in,'$FS$'(CategoryFS),for,word,'<',Word,'>','$NL$'] ).

lgi_error(no_nt_hierarchy) :-
   lgi_error1( ['Nonterminal',hierarchy,does,not,exist,'$NL$'] ).

lgi_error( unexpandable_category( db(Word,_Path,CategoryFS) ) ):-
   lgi_error1( [ category,'$FS$'(CategoryFS),for,word,'<',Word,'>',
                 cannot,be,expanded,'$NL$'] ).


%% ----------------------------------------------------------
%%   parser errors
%% ----------------------------------------------------------

%lgi_error(empty_lexicon) :-
%   lgi_error1([no,lexicon,available]).

lgi_error(list_of_afs(ListCUF)) :-
   lgi_error1( ['$FS$'(ListCUF),must,consist,of,afs,'''',s] ).

lgi_error(illformed_input_string(ListCUF)) :-
   lgi_error1( ['ill-formed',input,string,'$FS$'(ListCUF)] ).


%% ----------------------------------------------------------
%%   LexGram self-debugging
%% ----------------------------------------------------------

lgi_error( instantiation_error(PredicateName) ) :-
   lgi_error1( [a,'LexGramP',bug,':',
                argument,not,instantiated,in,PredicateName] ).


%% --- END OF FILE:  msgs.pl
%%%      File: lgp_uif.pl                                       %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: user interface for the parser                    %%%
%%%   Created:                                                  %%%
%%%  Modified: Wed Oct 23 12:04:34 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% 95-06-02 Esther Koenig
%% 95-02-17 Peter Krause - access to Prolog version of parser
%% 95-01-21 Esther Koenig
%% Exports:
%%   p/1, p/2, p/4, p/5
%%   show_item/1
%% Imports:
%%   Predicates:
%%     cuf_goal/4     (CUF-ADT)
%%     cuf_prolog_term/2 (CUF ADT)
%%     cuf_put_path/3 (CUF-ADT)
%%     get_flag/2     (CUF system)
%%     lg_control_files/1  (local grammar)
%%     lgi_cat2fs/2
%%     lgi_compile_lexicon/0
%%     lgi_cuf_prove/6
%%     lgi_get_counter/2
%%     lgi_get_flag/2
%%     lgi_init_chart/4
%%     lgi_inc_counter/2
%%     lgi_mk_cnf_file/3
%%     lgi_mk_item_fs/8
%%     lgi_parse/3
%%     lgi_reset_chart/0
%%     lgi_set_counter/2
%%     lgi_tcl_output/3
%%     lgi_test_file_prefix/1  (local grammar)
%%     lgi_visit_item/8
%%     mk_text_item_fs/6
%%     pp_fs/2        (CUF-ADT)
%%     set_flag/2     (CUF system)
%%     visit_text_item/7
%%   Sorts:
%%     grammar_name/0
%%     psent/1
%%   Types:
%%     cat
%% Counters:
%%   hcoffset  % 
%%   l         % entry no
%%   rno       % reading number    
:- dynamic 'LGsid'/1.  % (generated) sentence id

%% p(+SentenceId)
%% p(+WordListPl)
%% p(+WordListPl,-SentenceId)
%% - call the parser with test SentenceId or with WordListPl
%% - do all parses
%%
p(Thing) :-
   p(Thing,_SentId).

p(Thing,SentenceId) :-
   statistics(runtime,[StartTime|_]),
   lgi_p(Thing,StartTime,SentenceId).


lgi_p(Thing,_StartTime,_SentId) :-
   lgi_init_parse(Thing,Goal,SentenceId),
   lgi_parse(Goal,Out,ItemId),  
 nl,write('RESULT':ItemId), nl,
   lgi_terminate_parse(SentenceId,Goal,Out),
   fail.

lgi_p(_Thing,StartTime,SentenceId) :-
   statistics(runtime,[End|_]),
   ( clause( 'LGsid'(SentenceId), _ ) 
     ; ( \+ clause( 'LGsid'(SentenceId), _ ) ) ),
   Total is End - StartTime,
   lgi_get_counter(l,LastItemId),     
   lgi_get_counter(hcoffset,OffSet),  % primitive entries
   Items is LastItemId - OffSet,      % derived entries
   nl,write('TOTAL TIME':Total), 
   write('  / number of derived items': Items ),nl,
   nl.


%% p(+SentenceFS,+GoalFT,-ResultFT,-ResultItemId)
%% p(+SentenceFS,+GoalFT,-ResultFT,-ResultItemId,-SentenceId)
%% - call the parser with test sentence 
%% - return a ResultFS (if there is a successful parse)
%% - ResultFT :==  d(ResultCatFS,ResiduatedGoalS,ResiduatedConstrS)
%%
p( SentenceFS, GoalFT, d(CatFS,GoalS,ConstrS), ItemId) :-
   p( SentenceFS, GoalFT, d(CatFS,GoalS,ConstrS), ItemId, _SentenceId ).


p( SentenceFS, GoalFT, d(CatFS,GoalS,ConstrS), ItemId, SentenceId) :-
   ( lgi_init_parse(SentenceFS,GoalFT,Goal,SentenceId) -> true ),
   lgi_parse(
      Goal,
      cth(GoalS,ConstrS),
      ItemId),
   ( Goal = c(_S1,_H1,GoalCat,_G1,_C1) ), 
   ( lgi_cat2fs(GoalCat,CatFS) -> true ).


%% lgi_init_parse(+TestNumber,-Own,-SentenceId)
%% - SentenceId = TestNumber
%% lgi_init_parse(+SentenceFS,-Own,-SentenceId)
%% lgi_init_parse(+SentenceFS,+GoalFT,-Goal,?SentenceId)
%% - SentenceId = generated atom
%%
lgi_init_parse(TestNumber,Goal,SentenceId) :-
   retractall('LGsid'(_)),
   ( ( ( var(TestNumber)   % does not make much sense
         -> cuf_var(TestNumberCUF)
         ;  cuf_atom(TestNumber,TestNumberCUF) )
       -> ( % THEN lookup corresponding WordList
            cuf_goal(psent,[TestNumberCUF,GoalFS],WordList,IntCall),
            lgi_cuf_prove(undelayed_only,[IntCall],GoalS,[],ConstrS,_),
            cuf_atom(SentenceId,TestNumberCUF) )
       ;  % ELSE - TestNumberCUF should be already a Prolog list of words
          ( cuf_extern2intern(TestNumber,[],[],WordList),
            cuf_goal(goal_category,[],GoalFS,GCIntCall),
            lgi_cuf_prove(undelayed_only,[GCIntCall],GoalS,[],ConstrS,_) ) ),
     lgi_init_parse(WordList,d(GoalFS,GoalS,ConstrS),Goal,SentenceId) )
   -> true.   % backtracking does not make sense


lgi_init_parse(
      WordList,
      d(GoalFS,GoalS,ConstrS),
      c(String,ToBindHypoS,
        cat(CatType,root(RootType,Root),Leaves,DConstrS,PrintInfo),
        GoalS,ConstrS),
      SentenceId ) :-
   ( ( lgi_set_counter(rno,1),  % initialize reading counter
       lgi_reset_chart, 
       lgi_init_chart(
          WordList,
          String,
          GoalFS,
          arg( CatType, _Dir2,
               root(RootType,Root),
               Leaves, ToBindHypoS, DConstrS, PrintInfo, _HeadProj ) ),
       ( var(SentenceId) 
         -> ( /*RB cuf:*/gensym('S',SentenceId),
              assert( 'LGsid'(SentenceId) ) )
         ;  true ) )
     -> true ).  % backtracking does not make sense


%% lgi_terminate_parse(+StringNumber/+WordList,Goal,Out)
%% ??? what should be done with resid. Goals? - currently ignored for GenGoal !%%
lgi_terminate_parse(
      StringNumber, 
      c( _S1,Slash,GoalCat,_G1,_C1),
      cth(_GoalS,_ConstrS) ) :-
   ( current_predicate(lgi_test_file_prefix, _ ) % test_file_prefix defined
     -> ( lgi_cat2fs(GoalCat,GoalFS),
          %% Extracting info from GoalCat:
          cuf_put_path(GoalFS, [root,syn], Syn),      % extract syntax 
          cuf_put_path(GoalFS, [root,sem,frml], Sem), % extract semantics
          cuf_put_path(GoalFS, [leaves], Leaves ),    % extract leaves
          %% Building the generation goal:
          cuf_put_path(GenGoalRoot, [syn], Syn),      % posit syntax
          cuf_put_path(GenGoalRoot, [sem,frml], Sem), % posit semantics
          cuf_put_path(GenGoal, [root], GenGoalRoot), 
          cuf_put_path(GenGoal, [leaves], Leaves),
          lgi_hypos2fs(Slash,SlashFS),
          cuf_put_path(GenGoal, [slash], SlashFS ),
          lgi_get_counter(rno,NewNumber),
          lgi_file_name(StringNumber,NewNumber,FileName),
          tell(FileName),                               % write result on file 
          % after tell/1, in case tell/1 fails:
          lgi_correct_flag_values(FlagValues), 
          ( ( %% if a grammar name is available, write it down
              cuf_goal(grammar_name,[],GrammarName,IntCall),
              cuf_prove(undelayed_only,[IntCall],_RGoalS,[],_RConstrS,_) )
            -> ( write('grammarname('),
                 pp_fs(GrammarName,[]),
                 write(').'), nl )
            ;  true ),
          write( 'cat(' ),
%          pp_fs(GenGoal,ConstrS),     % syntax goal + semantics 
          pp_fs(GenGoal),  % output of Constraints does properly in cuf format
          write( ').' ), nl,
          told,
          write(FileName),
          lgi_reset_flag_values(FlagValues),
          lgi_inc_counter(rno,_N) )
      ;  true ).   % test_file_prefix not defined 




lgi_correct_flag_values( [ XMFEDValue, OutputFormat ] ) :-
   get_flag(xmfed_output,XMFEDValue),
   get_flag(output_format,OutputFormat),
   set_flag(xmfed_output,no),
   set_flag(output_format,cuf).


lgi_reset_flag_values( [ XMFEDValue, OutputFormat ] ) :-
   set_flag(xmfed_output,XMFEDValue),
   set_flag(output_format,OutputFormat).
   
%% ----------------------------------------------------------
%%   chart inspection
%% ----------------------------------------------------------

%% show_item(ItemId)
%% - does a pp_fs on the information contained in the 
%%   item with ItemId
%%
show_item(ItemId) :-
   lgi_get_flag(chart,ChartValue),
   lgi_get_flag(tcl_output,TclValue),
   ( ( ChartValue = sentence )
     -> ( lgi_visit_item(From,To,ItemId,CatName,CatFT,Tree,_CT,HypoS),
          ( ( TclValue = no )
            -> ( lgi_mk_item_fs(
                    From,To,HypoS,ItemId,CatFT,CatName,Tree,d(FS,_Gs,ConstrS)),
                 pp_fs(FS,ConstrS) ) % GoalS are currently ignored
            ;  ( ( TclValue = yes ),
                 lgi_tcl_output(CatName,ItemId,CatFT) ) ) )
     ;  ( ( ChartValue = text ),
          visit_text_item(From,To,ItemId,CatName,SemFT,Tree,_CatType),
          ( ( TclValue = no )
            -> ( mk_text_item_fs(
                    From,To,ItemId,SemFT,Tree,d(FS,_Gs,ConstrS)),
                 pp_fs(FS,ConstrS) )  % GoalS are currently ignored
            ;  ( ( TclValue = yes ),
                 lgi_tcl_output(CatName,ItemId,SemFT) ) ) ) ).


%% --- END OF FILE: lgp_uif.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: chart browser (partly LexGram specific)       %%%
%%%      Created: Mon Mar 25 15:01:58 1996                      %%%
%%%     Modified: Tue Jul 23 15:51:06 1996 (esther)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% history:
%% - ek 96-07-16: 
%%   - data structure of syntax trees changed
%%   - interface to chart optimized
%%   - treatment of empty/non-existent chart added
%% - adaptation to LexGram by Joerg Junger
%% Exports:
%%   display_chart2emacs/0
%% Imports:
%%   collect_text_tree/2
%%   input_text/2
%%   lgi_collect_tree/2
%%   lgi_get_flag/2
%%   lgi_input_string/2
%%   lgi_visit_sentence_item/5
%%   show_item/1
%%   visit_text_item/5


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interfaces to sentence vs. text chart

%% lgi_visit_item(From,To,NodeId,CatName,CatType)
%% - interface between chart browser and actual charts in the database
%% - switch between sentence level chart and text level chart
%%
lgi_visit_item(From,To,NodeId,CatName,CatType) :-
   lgi_get_flag(chart,ChartFlag),
   ( ( ChartFlag = sentence )
     -> lgi_visit_sentence_item(From,To,NodeId,CatName,CatType)
     ;  ( ( ChartFlag = text )
          -> visit_text_item(From,To,NodeId,CatName,CatType) ) ).

%% lgi_input_chain(String,Length)
%%
lgi_input_chain(String,Length) :-
   lgi_get_flag(chart,ChartFlag),
   ( ( ChartFlag = sentence )
     -> lgi_input_string(String,Length) 
     ;  ( ( ChartFlag = text )
          -> input_text(String,Length) ) ).


lgi_get_tree(NodeId,Tree) :-
   lgi_get_flag(chart,Value),
   ( ( Value = sentence )
     -> lgi_collect_tree(NodeId,Tree)
     ;  ( ( Value = text )
          -> collect_text_tree(NodeId,Tree) ) ).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% display_chart2emacs/0
%%  Mit diesem Praedikat wird eine Chart ausgegeben, die dann ueber
%%  die Filterfunktion in cuf.el entsprechend behandelt wird. Zuerst
%%  wird die eigentlich Chart ausgerechnet (layout_chart), dann wird
%%  das Resultat, das zeilenweise als Liste vorliegt, in Strings
%%  umgewandelt, damit Emacs besser damit zurecht kommt. Mit
%%  konv_position werden dann die sensitiven Bereiche so umgerechnet,
%%  dass Emacs die Bezeichner fuer die Kanten highlighten kann.
%%  Ausserdem erleichtert dies die Positionsbestimmung.
%%

display_chart2emacs :-
	layout_chart,
        !,  % non-empty chart
	print_chart_from_layout(ListList,SensList,TraceSensList),
	konv_lists2string(ListList,[],String,ListOfLengths),
	konv_positions(SensList,TraceSensList,ListOfLengths,
		       NewSensList,NewTraceSensList),
	escape_backslash(String,NewString),!,
	cuf2emacs2(NewString,NewSensList,NewTraceSensList).

% empty chart
display_chart2emacs :-
        % name('CHART IS EMPTY',[67,72,65,82,84,32,73,83,32,69,77,80,84,89]),
	format("chart:(""~s"" ",[67,72,65,82,84,32,73,83,32,69,77,80,84,89]),
	format(" ",[]),
	format("):endchart",[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: layout_chart/1
%% args: 
%%     either
%%	Item 	item number for which subchart is to be displ.
%%     or
%%	full	for full chart
%% desc: build preformat representation (in internal DB) of a subchart
%% 	of an item resp. of the full chart.
%%      - fails for empty/non-existent chart
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

layout_chart :-
	eraseall('LG_prechart'),
	fail.
layout_chart :-
	ppc_item,
	fail.
layout_chart :-
	ppc_trace_item,
	fail.
layout_chart :-
	lgi_input_chain(String,EndVertex),
	recorda('LG_prechart',start_vertex(0),_),
	recorda('LG_prechart',end_vertex(EndVertex),_),
	adjust_columns_chart(String,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: ppc_item/0, /3
%% desc:
%%   print item string into database 'LG_prechart' (not adjusted)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppc_item :-
        ( ( CatType = word )
          ; ( CatType = derived_phrase ) ),
	lgi_visit_item(From,To,NodeId,CatName,CatType),
	\+ From = To,   % in case of derived phrases consisting of traces
        name(NodeId,NumberString),
	name(CatName,CatNameString),
	append(NumberString,[58|CatNameString],Label),
	ppc_item1(NodeId,From-To,Label).

ppc_item1(I, From-To, Chars) :-
	free_chart_row(From-To, 0, Row),
	length(Chars, Length),
	Len1 is Length+3,
	recordz('LG_prechart', pp_item(Row, From-To, I:Chars, Len1), _).

free_chart_row(FromTo, N, N) :-
	\+ (recorded('LG_prechart', pp_item(N, F1T1, _, _), _),
	    iv_intersect(FromTo, F1T1, _)),
	!.
free_chart_row(FT, N, Row) :-
	N1 is N+1,
	free_chart_row(FT, N1, Row).

iv_intersect(F1-T1, F2-T2, F-T) :-
	max(F1, F2, F),
	min(T1, T2, T),
	F < T.

ppc_trace_item :-
        ( ( CatType = trace )
          ; ( CatType = derived_phrase ) ),
	lgi_visit_item(Pos,Pos,NodeId,CatName,CatType),
        name(NodeId,NumberString),
	name(CatName,CatNameString),
	append(NumberString,[58|CatNameString],Label),
	ppc_trace_item1(NodeId,Pos,Label).


ppc_trace_item1(I,Pos, Chars) :-
	free_trace_chart_row(Pos, 0, Row),
	length(Chars, Length),
	Len1 is Length+3,
	recordz('LG_prechart', 
		pp_trace_item(Row, Pos, I:Chars, Len1), _).

free_trace_chart_row(Pos, N, N) :-
	\+ recorded('LG_prechart', pp_trace_item(N, Pos, _, _), _),
	!.
free_trace_chart_row(Pos, N, Row) :-
	N1 is N+1,
	free_trace_chart_row(Pos, N1, Row).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: adjust_columns_chart/2
%% desc:
%%   calculate minimal column widths -> database key 'LG_prechart'
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
adjust_columns_chart([Word|String],From) :-
	Colno is From+1,
	atom_chars(Word, List),
	length(List,L),
	ifthenelse(Colno<10, CW1 is L+5, CW1 is L+6),
	adjust_cc1(Colno, Colno, CW1, CW1, CWAux),
	adjust_cc2(From,CWAux,CW),
	recordz('LG_prechart', column_width(Colno, CW), _),
	adjust_columns_chart(String,Colno).

adjust_columns_chart([],_Pos).


%% Behandlung der Traces, die letzten Traceitems muessen nicht
%% beruecksichtigt werden

adjust_cc2(Pos,CW,CWOut) :-
	recorded('LG_prechart',pp_trace_item(_,Pos,_,LI),_),
	ToAdd is LI-CW,
	ToAdd > 0,!,
	CW1 is CW+ToAdd,
	adjust_cc2(Pos,CW1,CWOut).
adjust_cc2(_,CW,CW).

%% while there are items in (From1-1,To) not suiting into CW, add to CW
adjust_cc1(From1, To, TW, CW, CW_o) :-
	From is From1 - 1,
	recorded('LG_prechart', pp_item(_, From-To, _, LI), _),
	ToAdd is LI-TW,
	ToAdd > 0, !,
	TW1 is TW+ToAdd,  CW1 is CW+ToAdd,
	adjust_cc1(From1, To, TW1, CW1, CW_o).

%% decrease From1, adding next column to TW
adjust_cc1(From1, To, TW1, CW1, CW_o) :-
	From is From1 - 1,
	recorded('LG_prechart', column_width(From, CW), _), !,
	TW2 is TW1+CW,
	adjust_cc1(From, To, TW2, CW1, CW_o).
%% no more items or columns to be checked
adjust_cc1(_, _To, _TW, CW, CW).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: print_chart_from_layout/2
%% args: Lines		list of lines (each a list of atoms)
%%	 SensInfos	list of sens-info corresponding to lines
%% desc: computes print representation (incl. sens-info) for chart
%%       from preformat using column info
%% 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_chart_from_layout(PrintLines, SensLines, TraceSensLines) :-
        lgi_input_chain(String,_To),
        !,  % non-empty chart
        ( From = 0 ), 
	print_input(String,From,InputLine,[]),
	print_rows(0,From,Lines,SensLines),
	print_trace_rows(0,From,TraceLinesAux,TraceSensAux),
	reverse(TraceLinesAux,[],TraceLines),
	reverse(TraceSensAux,[],TraceSensLines),
	append(TraceLines,[InputLine|Lines],PrintLines).


print_input( [Word|String], From ) -->
	{ 
	Colno is From+1,
	recorded('LG_prechart', column_width(Colno, Wid), _),
	name(Word, WordChars),
	length(WordChars,L),
	name(From, NChars),
	append(NChars,"]",NChars1),
	length(NChars, NL),
	Diff is Wid-(L+NL+2),
	PreWid is Diff // 2,
	PostWid is Diff-PreWid
	},
	[[ 0'[ | NChars1],tab(PreWid),WordChars,tab(PostWid)],
	print_input(String,Colno).
print_input([],To) -->
	{ name(To,NChars),
	append(NChars,"]",NChars1)
	},
	[[0'[ | NChars1]].


print_rows(N, From0, [[tab(1)|LineList]|RLines], [Sens|RSens]) :-
	setof(From-pp_item(To,IStr,L),
	      Ref^recorded('LG_prechart', pp_item(N,From-To,IStr,L), Ref),
	      Bag),		%% sorted by From
	Bag\==[],
        !,
	print_row(From0, 2, Bag, LineList, Sens),
	N1 is N+1,
	print_rows(N1, From0, RLines, RSens).
print_rows(_, _, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: print_row/5
%% arguments: From - aktuelle Itemspalte, ab der zu drucken ist
%%	      Col - aktuelle Zeichenspalte, ab der zu drucken ist
%%	      ItemPPInfos - pp_infos der noch zu druckenden Items
%%	      LineList - Resultat: Liste von Strings, die der Ausdruck
%%			 von ItemPPInfos ergibt
%%	      Sens - sens. Regions in der Zeile, angegeben durch
%%		     Zeichenpositionen
%% desc:
%%   print row of chart into list of strings, calc. sens. regions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_row(From, Col, [From-pp_item(To,I:ItemChars,L) | RItem],
		     ["|",PreItemPost | RLine],
		     [[SensItem,I,StartItem,EndItem] | RSens]) :-
	!,
	total_chart_width(From, To, Wid),
	call(Diff is Wid+2-L),
	PreWid is Diff // 2,
	PostWid is Diff-PreWid,
	append(ItemChars,PostChars,ItemPost),
	n_chars(PreWid,45,PreItemPost,ItemPost),  %% 45 = 0'-
	n_chars(PostWid,45,PostChars,"|"),	%% append PostString and |
	ifthenelse( lgi_visit_item(
                       _From,_To,I,_CatN,word),
		     SensItem=lexitem, 
		     SensItem=item),
	StartItem is Col+1+PreWid,
	EndItem is StartItem+L-4,
	call(NewCol is Col+Wid),
	print_row(To, NewCol, RItem, [_FirstChar|RLine], RSens).
print_row(From0, Col, [From-PPItem | RItem], [tab(1),tab(N)|RLine], RSens) :-
	From0 < From, !,
	total_chart_width(From0, From, Wid),
	call(N is Wid - 1),
	call(NewCol is Col+Wid),
	print_row(From, NewCol, [From-PPItem | RItem], RLine, RSens).
print_row(_, _, [], [tab(1)], []) :- !.


%%% Hier werden die Zeilen fuer die Tracedarstellung in der Chart erstellt

print_trace_rows(N, From0, [LineList|RLines], [Sens|RSens]) :-
	setof(Pos-pp_trace_item(IStr,L),
	      Ref^recorded('LG_prechart', pp_trace_item(N,Pos,IStr,L), Ref),
	      Bag),		%% sorted by From
	Bag\==[],
        !,
	print_trace_row(From0, 1, Bag, LineList, Sens),
	N1 is N+1,
	print_trace_rows(N1, From0, RLines, RSens).
print_trace_rows(_, _, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: print_trace_row/5
%% arguments: From - aktuelle Itemspalte, ab der zu drucken ist
%%	      Col - aktuelle Zeichenspalte, ab der zu drucken ist
%%	      ItemPPInfos - pp_infos der noch zu druckenden Items
%%	      LineList - Resultat: Liste von Strings, die der Ausdruck
%%			 von ItemPPInfos ergibt
%%	      Sens - sens. Regions in der Zeile, angegeben durch
%%		     Zeichenpositionen
%% desc:
%%   print row of chart into list of strings, calc. sens. regions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_trace_row(From, Col, [From-pp_trace_item(I:ItemChars,L) | RItem],
		     [ItemChars|[tab(PostWid)| RLine]],
		     [[traceitem,I,StartItem,EndItem] | RSens]) :-
	!,
	To is From+1,
	total_chart_width(From, To, Wid),
	call(PostWid is Wid+3-L),
	StartItem is Col+1,
	EndItem is StartItem+L-4,
	call(NewCol is Col+Wid),
	print_trace_row(To, NewCol, RItem, RLine, RSens).
print_trace_row(From0, Col, [From-PPItem | RItem],
		[tab(Wid)|RLine],RSens) :-
	From0 < From, !,
	total_chart_width(From0, From, Wid),
%	call(N is Wid - 1),
	call(NewCol is Col+Wid),
	print_trace_row(From, NewCol, [From-PPItem | RItem], RLine, RSens).
print_trace_row(_, _, [], [tab(1)], []) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicate: total_chart_width/3
%% arguments: From 	- Startknoten (in)
%%	      To   	- Endknoten (in)
%%	      Width	- Breite (out)
%% desc: berechne Breite der Chart zwischen Start- und Endknoten
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_chart_width(N, N, 0) :- !.
total_chart_width(From, To, Wid+W) :-
	I is From+1,
	(recorded('LG_prechart', column_width(I,W), _),!
	; W=0),
	total_chart_width(I, To, Wid).


n_chars(0,_) --> !, [].
n_chars(N,Ch) --> {N > 0, N1 is N-1},
		  [Ch],
		  n_chars(N1,Ch).


%%%%%%%%%%% Praedikate aus adtuntils.pl %%%%%%%%%%%%%%

max(A,B,C) :-
   A >= B,
   !, C=A.
max(_,B,B).

min(A,B,C) :-
   A =< B,
   !, C=A.
min(_,B,B).

eraseall(Key) :-
	recorded(Key,_,Ref),
	erase(Ref),
	fail.
eraseall(_).


%% where needed ?
showall(Key) :-
	recorded(Key,Item,_),
	write(Item),
	nl,
	fail.
showall(_).

showall :-
	recorded('LG_prechart',Item,_),
	write(Item),
	nl,
	fail.
showall.

%%
%%  Mit diesem Praedikat wird das Format, das durch den
%%  Chart-Layout-Algorithmus geliefert wird, in eine grosse
%%  Zeichenkette umgewandelt, die emacs einfach darstellen kann
%%


konv_lists2string([],List,List,[]).
konv_lists2string([H|TRow],LIn,LOut,[Length|TLength]) :-
	konv_line(H,[10],KonvString),
	length(KonvString,Length),
	append(LIn,KonvString,LAux),
	konv_lists2string(TRow,LAux,LOut,TLength).

konv_line([],L,L).
konv_line([[H|T1]|T],LIn,LOut) :-
	append(LIn,[H|T1],LIn1),
	konv_line(T,LIn1,LOut).
konv_line([tab(N)|T],LIn,LOut) :-
	N >= 0,!,
	baue_liste(N,Liste),
	append(LIn,Liste,LIn1),
	konv_line(T,LIn1,LOut).
konv_line([tab(_)|T],LIn,LOut) :-
	konv_line(T,LIn,LOut).

baue_liste(0,[]) :- !.
baue_liste(N,[32|T]) :-
	N1 is N-1,
	baue_liste(N1,T).
displ2([]).
displ2([H|T]) :-
	display_string(H),
	nl,
	displ2(T).

test :-
	print_chart_from_layout(L,_,_),
	displ(L).

ifthenelse(A,B,C) :-
	(A -> B
	; C
	).

reverse([],L,L).
reverse([H|T],LAux,LOut) :-
	reverse(T,[H|LAux],LOut).

display_chart(S,TS) :-
	layout_chart,
	print_chart_from_layout(L,S,TS),
	displ(L).

%%
%%  Mit diesem Praedikat werden die Positionen ausgerechnet, die man
%%  im Emacsbuffer highlighten muss.
%%  


konv_positions(SensList,TraceSensList,LengthList,
	       NewSensList,NewTraceSensList) :-
	konv_pos(TraceSensList,LengthList,
		 NewTraceSensList,[H|NewLengthList],0,ThresholdOut),
	ThresholdIn is H + ThresholdOut + 1,
	konv_pos(SensList,NewLengthList,
		 NewSensList,[],ThresholdIn,_).

konv_pos([],LengthList,[],LengthList,Threshold,Threshold).
konv_pos([Row|Rows],[RowLength|LengthRest],[NewRow|NewRows],LengthListOut,
	 ThresholdIn,ThresholdOut) :-
         konv_pos_row(Row,NewRow,ThresholdIn),
	 NewThreshold is RowLength + ThresholdIn,
	 konv_pos(Rows,LengthRest,NewRows,LengthListOut,
		  NewThreshold,ThresholdOut).

konv_pos_row([],[],_).
konv_pos_row([[Type,Ident,Begin,End]|Rest],
	     [[Type,Ident,NewBegin,NewEnd]|Rest1],Threshold) :-
             NewBegin is Begin + Threshold,
	     NewEnd is End + Threshold + 1,
	     konv_pos_row(Rest,Rest1,Threshold).

%%%%%%%%%%%%%%%

escape_backslash([],[]).
escape_backslash([92|Rest],[92|[92|RestOut]]) :-
	escape_backslash(Rest,RestOut).
escape_backslash([H|Rest],[H|RestOut]) :-
	escape_backslash(Rest,RestOut).

cuf2emacs2(String,SensList,TraceSensList) :-
	format("chart:(""~s"" ",[String]),
	plists2llists(SensList),
	format(" ",[]),
	plists2llists(TraceSensList),
	format("):endchart",[]).

plists2llists([]) :-
	format("()",[]).
plists2llists([H|T]) :-
	format("(",[]),
	((compound(H),functor(H,'.',2)) -> %% is list
	     plists2llists(H)
	; format("~w",[H])
	),
	plists2llists1(T).

plists2llists1([]) :-
	format(")",[]).
plists2llists1([H|T]) :-
	((compound(H),functor(H,'.',2)) -> %% is list
	     format(" ",[]),
	     plists2llists(H)
	; format(" ~w", [H])
	),
	plists2llists1(T).


%%
%%  Kommando, das durch ein Popup-Menu beim Klicken auf eine Kante in
%%  der Chart selektiert werden kann. Es wird die Merkmalsstruktur der
%%  selektierten Konstituente durch den CUF Pretty Printer angezeigt.
%%  Die Art und Weise wie angezeigt wird, wird durch die
%%  entsprechenden CUF Flags gesteuert. Es wird eine Merkmalsstruktur
%%  angezeigt, die zwei Merkmale hat, naemlich die Merkmalsstruktur
%%  der Konstituente selbst, sowie bei nicht lexikalischen
%%  Konstituenten die Liste der in dieser Konstituente sichtbaren
%%  Hypothesen.
%%

show_fstructure(NodeIDAtom) :-
	name(NodeIDAtom,String),
	name(NodeID,String),
	show_item(NodeID).
	
%% 
%%  Kommando, das durch ein Popup-Menu beim Klicken auf eine Kante in
%%  der Chart selektiert werden kann. Es wird in einem neuen Buffer
%%  ein Syntaxbaum angezeigt, dessen Root die selektierte Konstituente ist.
%%

show_syntaxtree(NodeId) :-
   name(NodeId,STRING),
   name(NodeId1,STRING),   % why ?
   lgi_get_tree(NodeId1,Tree),
   reformat_tree(Tree,[41],NewTree), % ')' 
   format("cuf:~s:cuf",[[40|NewTree]]), % '('
   name(E,[40|NewTree]),write(E),nl.



% leaf node
reformat_tree( leaf(Tree), Suffix, TreeString ) :-
	name(Tree, TreeString0),
        append(TreeString0,Suffix,TreeString).

% internal node
reformat_tree( node(CatAtom, ItemId, Dtrs), Suffix, TreeString) :-
	name(CatAtom,NameString),
	transform_name(NameString,TransNameString),
	name(ItemId,IdString),
        append([41],Suffix,DtrsSuffix),  % ')' of Dtrs
	reformat_trees(Dtrs,DtrsSuffix,DtrsString),  
        % IdString:TransNameString
	append(IdString,[58|TransNameString],NodeString), 
        %                           (DtrsString
	append(NodeString,[32,40|DtrsString],TreeString). % '(' of Dtrs


% list of daughter trees
reformat_trees([Dtr|Dtrs],Suffix,[40|DtrsString]) :- % '(' first dtr
	reformat_tree(Dtr,[41],DtrString),   % ')'
	reformat_trees(Dtrs,Suffix,DtrsString0),
	append(DtrString,[32|DtrsString0],DtrsString). % ' '
reformat_trees([],Suffix,Suffix).


transform_name([],[]).
transform_name([92|Rest1],[92|[92|Rest2]]) :-
	transform_name(Rest1,Rest2).
transform_name([40|Rest1],[92|[40|Rest2]]) :- !,
	transform_name(Rest1,Rest2).
transform_name([41|Rest1],[92|[41|Rest2]]) :- !,
	transform_name(Rest1,Rest2).
transform_name([H|Rest1],[H|Rest2]) :-
	transform_name(Rest1,Rest2).

%%  Hilfspraedikat

add_newlines([],[10,10]).
add_newlines([H|Rest],[H|Rest1]) :-
	add_newlines(Rest,Rest1).

%%%      File: lgg.pl                                           %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: load file for the generator                      %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Oct 18 11:32:38 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgf_files/1
%% Imports:
%%   lg_module/1   % LexGram user interface
%%   lgi_expand_file_names/3

    
lgg_files( LGIFiles, Rest ) :-    
   lg_module('lg-sys/common/',lgcommon,LGIFiles,Rest), 
   % no CUF description files specific to the parser
   lgi_expand_file_names(
      [
       chartman,  % chart manager
       indexman,  % premise set manipulation/chart index
       expcat,    % category compiler
       hdgen,     % generator
       lgg_uif   % user interface for the generator
      ],
      'lg-sys/generator/', % directory relative to LexGram load file
      ExpandedFileNames ),
   ensure_loaded(ExpandedFileNames).



%% --- END OF FILE: lgp.pl
%%%      File: chartman.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: chart manager for generation                     %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Nov 12 11:44:50 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_add_gtable_entry/2
%%   lgi_add_hptable_entry/2
%%   lgi_collect_tree/2
%%   lgi_fitting_gtable/2 
%%   lgi_fitting_hptable/3
%%   lgi_get_active_item/5
%%   lgi_get_passive_item/2
%%   lgi_get_gtable_entry/4
%%   lgi_get_hptable_entry/4
%%   lgi_init_chart/3
%%   lgi_input_string/2
%%   lgi_mk_item_fs/7
%%   lgi_new_gtable/2
%%   lgi_new_hptable/4
%%   lgi_reset_chart/0
%%   lgi_visit_item/8
%% Imports:
%%   predicates:
%%     cuf_extern2intern/4    % CUF ADT
%%     cuf_prolog_term/2      % CUF ADT
%%     cuf_put_path/3         % CUF ADT
%%     'LGusednt'/2
%%     l/5
%%     lgi_add_hypos/4
%%     lgi_cat2fs/2
%%     lgi_compress_guide/3
%%     lgi_error/1
%%     lgi_expand_goal/6
%%     lgi_get_counter/2
%%     lgi_message/1
%%     lgi_mk_index_add/3
%%     lgi_mk_index_at/2
%%     lgi_mk_index_ga/6
%%     lgi_mk_index_gp/3
%%     lgi_mk_index_gt/2
%%     lgi_mk_guide_index/1
%%     lgi_inc_counter/2
%%     lgi_reset_chart_index/0
%%     lgi_reset_goal_hypos/0
%%     lgi_set_counter/2
%%     lgi_subsumes_cat/2
%%     lgi_unify/2            
%%     lgi_unify_cat/3
%%     lgi_unify_hypos/2
%%     lgi_split_leaves/3
%% Counters:
%%   hcoffset  lexicon size + number of goal triggered traces
%%   l         entry id
%%   lexsize   lexicon size 
%%   t         table id

% ci( ItemId, OwnInfo ) % derived chart item 
:- dynamic ci/2.  
% gt0( GoalTableId, GoalInfo ) % goal table header
:- dynamic gt0/2.
% gt( GoalTableId, ItemId ) % goal table: goal + id's of solutions
:- dynamic gt/2.
% head projection table header (for same set of resources):
% hpt0(HeadId,ArgGuide,ArgHypoS,HeadProjTableId).
:- dynamic hpt0/4.
% hpt( HeadTableId, ItemId ) % goal table: goal + id's of solutions
:- dynamic hpt/2.
% lgi_input_string(StringListPl,NumberOfWords)
:- dynamic lgi_input_string/2.


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   database initialization
%% ----------------------------------------------------------

lgi_reset_chart :-
   lgi_reset_chart_index,
   lgi_reset_goal_hypos,
   retractall(ci(_,_)),
   retractall(gt0(_,_)),
   retractall(gt(_,_)),
   retractall(hpt0(_,_,_,_)),
   retractall(hpt(_,_)),
   % reset entry id counter to lexicon size
   lgi_get_counter(lexsize,LexSize),  
   lgi_set_counter(l,LexSize),
   lgi_set_counter(t,0),        % table id
   retractall( lgi_input_string(_,_) ).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   string preprocessing
%% ----------------------------------------------------------

%% lgi_init_chart(+GoalFS,-SimpleGuide,-GoalArgCat)
%% - goal expansion
%% - assert chart items for the lexical entries and their hypos
%% - insert slash_ids and translated root type representation 
%%   into Category's
%%
lgi_init_chart(GoalFS,ComprGuidePl,Goal):-
   % goal expansion
   lgi_expand_goal( GoalFS,Goal,[],SlashS, _CNType,
                    db('STARTSYMBOL',[],GoalFS)),
   lgi_add_hypos(SlashS,hc,0,_IdS1),   % goal has id 0
   lgi_get_counter(l,LastEntryId),
   lgi_set_counter(hcoffset,LastEntryId),
   lgi_compress_guide(GoalFS,ComprGuidePl,_KW1),
   lgi_mk_guide_index(ComprGuidePl).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   handling of goal tables
%% ----------------------------------------------------------

%% ----------------------------------------------------------
%%   get goal table header
%% ----------------------------------------------------------

%% lgi_fitting_gtable(+Goal,-GTableId)
%% - if there a goal table for a goal Goal1 where Goal1 subsumes Goal,
%%   return the GTableId of that table

lgi_fitting_gtable(
      c(Guide,HypoS1,GoalCat,GoalGoalS,GoalConstrS,_Str1),
      GTableId) :-
   ( GoalCat = cat( _CT1, root( RootType, _RFS1), 
                    lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,_PI1) ),
   clause( 'LGusednt'(RootType,UnifS), _ ), % lookup UnifS
   lgi_mk_index_gt(
      ii(UnifS,Guide,HypoS1,LLength), 
      GTableId ),
   clause(
      gt0( 
         GTableId,
         c(Guide,_HypoS2,Goal1Cat,Goal1GoalS,Goal1ConstrS,_Str2) ), _ ),
   lgi_subsumes_cat(
      c1( Goal1Cat, Goal1GoalS, Goal1ConstrS ),
      c1( GoalCat, GoalGoalS, GoalConstrS ) ).

%% ----------------------------------------------------------
%%   get goal table entry
%% ----------------------------------------------------------

%% lgi_get_gtable_entry(+Goal,+GTableId,-Out,-ItemId)
%%
lgi_get_gtable_entry(
      c(_Guide1,HypoS,GoalCat,GoalGoalS,GoalConstrS,String),
      GTableId,
      cth(GoalSOut,ConstrSOut),
      Id ) :-
   gt(GTableId,Id),
   lgi_get_counter(lexsize,LexOffSet), 
   ( ( Id =< LexOffSet ) % lexical stuff
     -> lgi_get_gtable_entry_lex(HypoS,Cat,GoalS,ConstrS,String,Id)
     ;  % stuff in chart
        lgi_get_gtable_entry_chart(Cat,HypoS,GoalS,ConstrS,String,Id) ),
   lgi_unify_cat(
      c1(GoalCat,GoalGoalS,GoalConstrS), % check goal's goals + constraints
      c1(Cat,GoalS,ConstrS),
      c1(_Res,GoalSOut,ConstrSOut) ).


% lookup a word
lgi_get_gtable_entry_lex([],Cat,GoalS,ConstrS,[Word],Id) :-
   clause( l(Id,Cat,GoalS,ConstrS,_LicSl,Word), _ ).

% lookup a hypo in the lexicon
lgi_get_gtable_entry_lex([s(Id,Cat1)],Cat2,GoalS,ConstrS,[],Id) :-
   clause( hl(Id,Cat2,GoalS,ConstrS), _ ),
   % unify chart entry's cat with top-down expected hypo information
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ). 


% lookup a derived item
lgi_get_gtable_entry_chart(Cat,HypoS1,GoalS,ConstrS,String,Id) :-
   clause( ci(Id,Own), _ ),
   Own = c(_Guide2,HypoS2,Cat,GoalS,ConstrS,String),
   lgi_unify_hypos(HypoS1,HypoS2).

% lookup a hypo in the chart
lgi_get_gtable_entry_chart(Cat2,[s(Id,Cat1)],GoalS,ConstrS,[],Id) :-
   clause( hc(Id,Cat2,GoalS,ConstrS), _ ), 
   % unify chart entry's cat with top-down expected hypo information 
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).


%% ----------------------------------------------------------
%%   add goal table header
%% ----------------------------------------------------------

%% lgi_new_gtable(+Goal,-GTableId)
%%
lgi_new_gtable(Goal,GTableId) :-
   ( Goal =
      c( Guide, HypoS, 
         cat( _CT1, root( RootType, _RFS1), 
              lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1, _PI1 ),
         _GS1, _CS1, _Str1 ) ),
   lgi_inc_counter(t,GTableId), 
   lgi_mk_index_at(
      ii(RootType,Guide,HypoS,LLength), 
      GTableId ),
   assert( gt0(GTableId,Goal) ).


%% ----------------------------------------------------------
%%   add goal table entry
%% ----------------------------------------------------------

%% lgi_add_gtable_entry(+GTableId,+ItemId)
%%
lgi_add_gtable_entry(GTableId,ItemId) :-
   assert( gt(GTableId,ItemId) ).



%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   handling of head projection tables
%% ----------------------------------------------------------

%%%%  head projections for same ressources

%% ----------------------------------------------------------
%%   get head project table header
%% ----------------------------------------------------------

%% lgi_fitting_hptable(+HeadId,+ArgString,+ArgHypoS,-HeadProjTableId)
%% - IF there is a head projection table for HeadId and the argument
%%      ressources ArgGuide, ArgHypoS
%%   THEN return the HeadProjTableId of that table
%% - OTHERWISE fail

lgi_fitting_hptable(HeadId,ArgGuide,ArgHypoS,HPTableId) :-
   clause( hpt0(HeadId,ArgGuide,ArgHypoS,HPTableId), _ ).


%% ----------------------------------------------------------
%%   get head projection table entry
%% ----------------------------------------------------------

%% lgi_hptable_entry(+HPTableId,-HeadProj,-RemLeaves,-ItemId)
%% - head projections are always derived items
%%
lgi_get_hptable_entry(HPTableId,HeadProj,RemLeaves,Id) :-
   hpt(HPTableId,Id),
   clause( ci( Id, HeadProj), _ ),
   ( HeadProj =
        c( _S1, _H1, 
           cat( _CT1, _R1, lvs(_LL1,_L1,_RL1,RemLeaves),
                _DCtrs1, _PI1 ),
           _G1, _ConstrS1, _Str1 ) ).


%% ----------------------------------------------------------
%%   add head projection table header
%% ----------------------------------------------------------

%% lgi_new_hptable(+HeadId,+ArgString,+ArgHypoS,-HPTableId)
%% - ArgInfo: only ressource instantiations count
%%
lgi_new_hptable(HeadId,ArgGuide,ArgHypoS,HPTableId) :-
   lgi_inc_counter(t,HPTableId), 
   assert( hpt0(HeadId,ArgGuide,ArgHypoS,HPTableId) ).


%% ----------------------------------------------------------
%%   add head projection table entry
%% ----------------------------------------------------------

%% lgi_add_hptable_entry(+HPTableId,-ItemId)
%%
lgi_add_hptable_entry(HPTableId,ItemId) :-
   assert( hpt(HPTableId,ItemId) ).


%% ----------------------------------------------------------
%% ----------------------------------------------------------
%%   general item manipulation
%% ----------------------------------------------------------

%% ----------------------------------------------------------
%%   get lexical items
%% ----------------------------------------------------------

%%%%%%
%% lgi_get_passive_item(+GoalInfo,-ItemId)
%% - lookup a passive item 
%%
lgi_get_passive_item(
      Goal,
      Id ) :-
   ( ( ( Goal = c(Guide,HypoS,GoalCat,_GoalGoalS,_GoalConstrS,String) ),
       ( GoalCat = cat( _CT1, root( RootType, _RFS1), 
                        lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,_PI1) ),
       clause( 'LGusednt'(RootType,UnifS), _ ) % lookup UnifS
     ) -> true ),   % no backtracking for this part
   lgi_mk_index_gp(
      ii(UnifS,Guide,HypoS,LLength), 
      Id,
      Flag),
   lgi_get_passive_item0(Flag,Id,HypoS,OwnCat,_OwnGoalS,_OwnConstrS,String),
   lgi_unify_cat(   % GoalS, ConstrS considered at gtable lookup
      c1(GoalCat,[],[]), 
      c1(OwnCat,[],[] ),
      _Cat1 ).

% lookup a word
lgi_get_passive_item0(word,Id,[],Category,GoalS,ConstrS,[Word]) :-
   clause( l(Id,Category,GoalS,ConstrS,_LicSl,Word), _ ).

% lookup a hypo in the lexicon
lgi_get_passive_item0( 
      trace(hl),
      Id,
      [ s(Id,Cat1) ],
      Cat2,
      GoalS,
      ConstrS,
      [] ) :-
   clause( hl(Id,Cat2,GoalS,ConstrS), _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).

% lookup a hypo in the chart (hypo licensed by goal cat)
lgi_get_passive_item0(trace(hc),Id,[s(Id,Cat1)],Cat2,GoalS,ConstrS,[]) :-
   clause( hc(Id,Cat2,GoalS,ConstrS), _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).


%%%%%%
%% lgi_get_active_item((+GoalInfo,-HeadInfo,-RestLeaves,-PremiseRest,-ItemId)
%%
lgi_get_active_item(Goal,Head,RestLeaves,In1,Id) :-
   ( ( ( Goal = c( Guide,HypoS, 
                   cat( _CT1, root(GoalRT,GoalRoot),
                        lvs(GoalLL,_L1,GoalRLeaves,_RemL1),
                        _Ctrs1, _P1 ), 
                   _GoalGoalS, _GoalConstrS, _Str1 ) ),
       clause( 'LGusednt'(GoalRT,UnifS), _ ) % lookup UnifS
     ) -> true ),  % no backtracking for this part
   lgi_mk_index_ga( ii(UnifS,Guide,HypoS,GoalLL),
                    HeadGuide, HeadHypoS, In1, Id, Flag ), 
   lgi_get_active_item0(
      Flag,Id,HeadHypoS,HeadCat,HeadGoalS,HeadConstrS,HeadString),
   ( HeadCat = cat( _CT3, root(_RT3,Root),
                    lvs(_LL3,_L3,HeadRLeaves,_RemL3), _Ctrs3, _P3 )),
   lgi_unify(   
      d(GoalRoot,[],[]),  % GoalGoalS, GoalConstrS at gtable lookup
      d(Root,HeadGoalS,HeadConstrS),
      d(_Res,OutGoalS,OutConstrS) ),
   lgi_split_leaves(GoalRLeaves,HeadRLeaves,RestLeaves),
   ( Head = c(HeadGuide,HeadHypoS,HeadCat,OutGoalS,OutConstrS,HeadString) ).


%% lgi_get_active_item0(Flag,+Id,+HypoS,-Category,-GoalS,-ConstrS,-String)
%% - Category: output variable in case of word
%% - Category: input/output variable in case of hypo
%%
lgi_get_active_item0(word,Id,[],Category,GoalS,ConstrS,[Word]) :-
   clause( l(Id,Category,GoalS,ConstrS,_LicSl,Word), _ ).

lgi_get_active_item0(trace(Flag),Id,[s(Id,Cat1)],Cat2,GoalS,ConstrS,[]) :-
   Clause =.. [Flag,Id,Cat2,GoalS,ConstrS],
   clause( Clause, _ ),
   lgi_unify_cat( c1(Cat1,[],[]), c1(Cat2,[],[]), _ ).


%% ----------------------------------------------------------
%%   add item
%% ----------------------------------------------------------

%% lgi_add_derived_item(+OwnInfo,-ItemId)
%% - assert a derived chart item 
%%
lgi_add_derived_item(Own,ItemId) :-
   lgi_inc_counter(l,ItemId), 
   assert( ci(ItemId,Own) ), 
write(ItemId),put(44).


%% ----------------------------------------------------------
%%   view item
%% ----------------------------------------------------------

%% lgi_visit_item(Guide,ItemId,CatNameAtom,CatFT,Tree,
%%                ConstituentType,HypoS,String)
%%
lgi_visit_item( 
      Guide, Id, CatName, 
      d(CatFS,GoalS,ConstrS),
      Tree, CatType, HypoS, String ) :-
   lgi_visit_item0(CatType,Guide,Id,Category,GoalS,ConstrS,HypoS,String),
   lgi_cat2fs(Category,CatFS),
   ( Category = cat(_CT1,_R1,_L1,_Ctrs1,Tree) ),
   ( Tree = node(CatName,Id,_Dtrs) ).


lgi_visit_item0(
      derived_phrase,Guide,Id,Category,GoalS,ConstrS,HypoS,String) :-
   clause( ci( Id,
               c(Guide,HypoS,Category,GoalS,ConstrS,String) ), _ ).

lgi_visit_item0(
      word,
      Guide, Id, Category, GoalS, ConstrS, [], [Word] ) :-
   clause( l(Id,Category,GoalS,ConstrS,_SlashIdS1,Word), _ ),
   ( clause( 'LGl'(_RootType,Guide,Id), _ )
     ; clause( 'LGl'(_RootType,_Length,Guide,Id), _ ) ).

% lexical trace
lgi_visit_item0(
      trace,
      [],  % empty guide
      Id, Category, GoalS, ConstrS,
      [s(Id,_Hypo)],
      [] ) :-
   clause( hl(Id,Category,GoalS,ConstrS), _ ).

% goal trace
lgi_visit_item0(
      trace,
      [],  % empty guide
      Id, Category, GoalS, ConstrS,
      [s(Id,_Hypo)],
      [] ) :-
   clause( hc(Id,Category,GoalS,ConstrS), _ ).


%% lgi_mk_item_fs(+Guide,+HypoS,+ItemId,+CatFT,+CatNameAtom,+Tree,+String,
%%                -ItemFT)
%%
lgi_mk_item_fs(
       Guide,HypoS,ItemId,
       d(CatFS,GoalS,ConstrS),
       CatNameAtom,
       node(_CatName,_Id,Dtrs),
       String,
       d(ItemFS,GoalS,ConstrS) ) :-
    cuf_prolog_term(ItemId,ItemIdCUF),
    cuf_put_path(ItemFS, ['ITEM-ID'], ItemIdCUF),
    cuf_prolog_term(Guide,GuideCUF),
    cuf_put_path(ItemFS, ['GUIDE'], GuideCUF),
    lgi_extract_hypo_ids(HypoS,HypoIdS),
    cuf_prolog_term(String,StringCUF),
    cuf_put_path(ItemFS, ['STRING'], StringCUF),
    cuf_prolog_term(HypoIdS,HypoSCUF),
    cuf_put_path(ItemFS, ['TRACES'], HypoSCUF),
    cuf_prolog_term(CatNameAtom,CatNameAtomCUF),
    cuf_put_path(ItemFS, ['CAT-NAME'], CatNameAtomCUF),
    cuf_put_path(ItemFS, ['CAT'], CatFS),
    cuf_prolog_term(Dtrs,DtrsCUF),
    cuf_put_path(ItemFS, ['DTRS'], DtrsCUF).


%% lgi_extract_hypo_ids(+HypoS,-HypoIdS) 
%%
lgi_extract_hypo_ids( 
      [ s(Id,_Hypo) | RestSlash ], 
      [ Id | RestIdS ] ) :-
   lgi_extract_hypo_ids(RestSlash,RestIdS).

lgi_extract_hypo_ids([],[]).


%% lgi_collect_tree(+ItemId,-TreeOut)
%% - collect the Tree with for ItemId
%%

lgi_collect_tree(Id,Tree) :-
   ( % word
       l( Id,
        cat(_CT1,_R1,_L1,_Ctrs1,Tree),
        _G1, _C1, _LH1, _W1 )
     ; % lexical trace
       hl( Id,
           cat(_CT1,_R1,_L1,_Ctrs1,Tree),_G1,_C1)
     ; % goal trace
       hc( Id,
           cat(_CT1,_R1,_L1,_Ctrs1,Tree),_G1,_C1)
     ; % derived item
       ( ci( Id, 
             c( _S1, _H1,
                cat( _CT1, _R1, _L1, _Ctrs1, node(CatName,Id,DtrIds) ),
                _G1, _C1, _Str1 ) ),
         lgi_collect_trees(DtrIds,Dtrs),
         ( Tree = node(CatName,Id,Dtrs) ) ) ).


lgi_collect_trees( [Id|Ids], [Tree|Trees] ) :-
   lgi_collect_tree(Id,Tree),
   lgi_collect_trees(Ids,Trees).

lgi_collect_trees([],[]).


%% --- END OF FILE: chartman.pl 
%%%      File: indexman.pl                                      %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: index manager for internal lexicon and chart     %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Nov 12 12:34:27 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_compress_guide/3
%%   lgi_mk_guide_index/1
%%   lgi_mk_index_add/4
%%   lgi_mk_index_at/2
%%   lgi_mk_index_ga/6
%%   lgi_mk_index_gp/3
%%   lgi_mk_index_gt/2
%%   lgi_guess_premises/3
%%   lgi_guide_union/3
%%   lgi_hypos2ext/2
%%   lgi_hypos_union/3
%%   lgi_reset_chart_index/0
%%   lgi_same_hypos/2
%%   lgi_set_diff/3
%%   lgi_string2ext/2
%%   lgi_string_union/4
%% Imports:
%%   l/6
%%   length/2     % Prolog
%%   lgi_in_interval/3
%%   lgi_unify_hypos/2
%% Counters:
%%   lexsize

%% representations:
%% - guide: a Prolog list of guide items ( KeyWord(Multiplicity) )
%% - hypotheses: a Prolog list of item ids
%%
% lexiconLGl'(
% 'LGl0'(MainKeyWord,LeavesLength,RootType,CompressedGuide,Id)
:- dynamic 'LGl0'/5.  % index for words (before lexicon lookup)
% 'LGhl0'(Id,LeavesLength,RootType)
:- dynamic 'LGhl0'/3.  % filter for active lexicon/hypos
% 'LGhl'(Id,RootType)
:- dynamic 'LGhl'/2.  % filter for passive lexicon/hypos
% 'LGhl'(Id,LeavesLength,RootType), LeavesLength > 0
:- dynamic 'LGhl'/3.  % filter for active lexicon/hypos

% current parse
% 'LGl'(RootType,CompressedGuide,Id), LeavesLength = 0
:- dynamic 'LGl'/3.   % index for passive words (after lexicon lookup)
% 'LGl'(RootType,LeavesLength,ComprGuide,Id)  , LeavesLength > 0
:- dynamic 'LGl'/4.   % index for active words (after lexicon lookup)
% 'LGhc'(Id,RootType)
:- dynamic 'LGhc'/2.   % filter for passive goal/hypos
% 'LGhc'(Id,LeavesLength,RootType), LeavesLength > 0
:- dynamic 'LGhc'/3.   % filter for active goal/hypos
% 'LGg'(RootType,HypoS,LeavesLength,Guide,GoalTableId)
:- dynamic 'LGg'/5.    % ressource index for goal table headers


%% ----------------------------------------------------------
%%   initialization stuff
%% ----------------------------------------------------------

lgi_reset_lex_index :-
   retractall('LGl0'(_,_,_,_,_)),
   retractall('LGhl0'(_,_,_)).


lgi_reset_chart_index :-
   retractall('LGl'(_,_,_)),
   retractall('LGl'(_,_,_,_)),
   retractall('LGhl'(_,_)),
   retractall('LGhl'(_,_,_)),
   retractall('LGhc'(_,_)),
   retractall('LGhc'(_,_,_)),
   retractall('LGg'(_,_,_,_,_)).


%% lgi_mk_guide_index(+CompressedGuide)
%% lgi_mk_guide_index(+CompressedGuideRest,+TotalCompressedGuide)
%% - copy lexicon index to current working index
%%   for all words whose guides are subsets of the TotalCompressedGuide

lgi_mk_guide_index(Guide) :-
   lgi_mk_guide_index(Guide,Guide).


% assert ressource index for all entries of a MainKeyWord
lgi_mk_guide_index( [ (KeyWord-_Mult1) | RestGuide ], Guide ) :-
write(KeyWord), put(32),
   ( clause( 
        'LGl0'(KeyWord,Length,RootType,LexComprGuide,Id), _ ),
     % subset property of LexGuide
     lgi_simple_guide_diff(Guide,LexComprGuide,_Diff1), 
write(Id), put(32),          
     ( ( Length = 0 )
       ->  assert( 'LGl'(RootType,LexComprGuide,Id) ) 
       ; ( ( Length > 0 )
           -> assert( 'LGl'(RootType,Length,LexComprGuide,Id) ) ) ),

     ( ( clause( l(Id,_Cat1,_GoalS1,_ConstrS1,LicSlS,_Word), _ ),
         lgi_add_current_hypo_index(LicSlS) )
       -> true ),     % no backtracking here
     fail ) % for all fitting index entries
    ; lgi_mk_guide_index(RestGuide,Guide).

lgi_mk_guide_index( [ ], _Guide ).


%% lgi_add_current_hypo_index(+HypoIdS)
%% - copy lexicon index for hypos into current working index
%%
lgi_add_current_hypo_index([]).

lgi_add_current_hypo_index( [Id | LicSlS ] ) :-
   ( ( ( clause( 'LGhl'(Id,_), _ )
         ; clause( 'LGhl'(Id,_,_), _ ) ) )
     -> true
     ; % if not yet in chart
       ( clause( 'LGhl0'(Id,Length,RootType), _ ),
         ( ( Length = 0 )
           -> assert( 'LGhl'(Id,RootType) )
           ; ( ( Length > 0 )
               -> assert( 'LGhl'(Id,Length,RootType) ) ) ),
         write(Id),put(32) ) ),
   lgi_add_current_hypo_index(LicSlS).


%% ----------------------------------------------------------
%%   produce the index of an item from psets and root info
%% ----------------------------------------------------------
%% data structures
%% - IndexInfoIn :== ii( RootTypeInfo, String, HypoS, LeavesLength )
%%    - RootTypeInfo: either a single TypeId or a list of TypeIds
%%    - HypoS :== a list of s(HypoId,HypoCat) terms
%%    - Guide :== list(GuideItem)-list(GuideItem) % diff list
%%      - GuideItem :== KeyWord - Multiplicity
%%      - KeyWord :== PrologAtom
%% - Flag :== word | trace(Flag1) | derived_phrase
%% - Flag1 :== hc | hl   (goal hypos vs. lexical hypos)
%%
%% lgi_mk_index_add(+Flag,+IndexInfoIn,+Category,+EntryId) 
%% lgi_mk_index_at(+IndexInfo,+Id)
%% - produce indices according to IndexInfoIn 
%% lgi_mk_index_ga(+IndexInfo,-HeadString,-HeadHypoS,-HeadCat,-Out,-Id,Flag)
%% lgi_mk_index_gp(+IndexInfo,-EntryId,-Flag) 
%% lgi_mk_index_gt(+IndexInfo,-Id)
%% - use indices for item lookup
%%
lgi_mk_index_add(
      word,
      ii( RootType, _W1, _HypoS1, Length ), 
      ExtCategoryFS,
      Id ) :-
   lgi_compress_guide(ExtCategoryFS,ComprGuide,KeyWord),
   assert( 
      'LGl0'(KeyWord,Length,RootType,ComprGuide,Id) ).

lgi_mk_index_add(
      trace(Flag),
      ii( RootType, _Word1, _HypoS1, Length ), 
      _CatFS1,
      Id ) :-
   ( ( Flag = hc )
     -> ( ( Functor = 'LGhc' ) ) 
     ;  ( ( Flag = hl )     
          -> ( Functor = 'LGhl0' ) ) ),
   ( ( Length = 0 )
     -> ( Clause =.. [Functor,Id,RootType] )
     ;  ( ( Length > 0 )
          -> ( Clause =.. [Functor,Id,Length, RootType] ) ) ),
   assert( Clause ).



lgi_mk_index_ga(
      ii(UnifS,GoalGuide,GoalHypoS,GLength),
      HeadGuide, HeadHypoS,
      cin(GuideOut,HypoSOut),
      Id, Flag ) :- 
   member(RootType,UnifS), % choose a possible root type
   lgi_mk_index_ga1(
      RootType, GoalHypoS, 
      HLength, RealHeadGuide, HeadHypoS, HypoSOut, 
      Id, Flag ),
   HLength > GLength, % head leaves must be longer than goal leaves
   lgi_guide_diff(GoalGuide,HeadGuide,GuideOut,RealHeadGuide).


% word 
lgi_mk_index_ga1(
      RootType, GoalHypoS, 
      HLength, HeadGuide, [], 
      GoalHypoS, Id, word ) :- 
   'LGl'(RootType,HLength,HeadGuide,Id).

% a hypo
lgi_mk_index_ga1(
      RootType, GoalHypoS, 
      HLength, [], [s(Id,HypoCat)],
      HypoSOut, Id, trace(Flag) ) :- 
   lgi_set_diff( GoalHypoS, [ s(Id,HypoCat) ], HypoSOut ), % guess HeadHypo
   lgi_mk_index_ga0(Id,HLength,RootType,Flag).


lgi_mk_index_ga0(Id,Length,RootType,hc) :-
   'LGhc'(Id,Length,RootType).

lgi_mk_index_ga0(Id,Length,RootType,hl) :-
   'LGhl'(Id,Length,RootType).



% word
lgi_mk_index_gp(
      ii(UnifS,Guide-Rest,[],Length),   
      Id, word ) :- 
   lgi_simple_guide_diff(Guide,Rest,RealGuide),
   member(RootType,UnifS), % pick a possible root
   lgi_mk_index_gp0(word,Length,RealGuide,Id,RootType).

% trace
lgi_mk_index_gp(
      ii( UnifS,
          [], 
          [ s(Id,_HypoCat) ],
          Length ),   
      Id,
      trace(Flag) ) :- 
   lgi_get_counter(lexsize,LexOffSet),
   ( ( Id =< LexOffSet )   % lexical trace
     -> ( Flag = hl )
     ;  ( ( Id > LexOffSet )  % goal trace
          -> ( Flag = hc ) ) ),
   lgi_mk_index_gp0(Flag,Length,Id,RootType),
   member(RootType,UnifS).


lgi_mk_index_gp0(word,0,Guide,Id,RootType) :-
   !,
   clause( 'LGl'(RootType,Guide,Id), _ ).

lgi_mk_index_gp0(word,Length,Guide,Id,RootType) :-
   Length > 0,
   !,
   clause( 'LGl'(RootType,Length,Guide,Id), _ ).

lgi_mk_index_gp0(hc,0,Id,RootType) :-
   !,
   clause( 'LGhc'(Id,RootType), _ ).

lgi_mk_index_gp0(hc,Length,Id,RootType) :-
   Length > 0,
   !,
   clause( 'LGhc'(Id,Length,RootType), _ ).

lgi_mk_index_gp0(hl,0,Id,RootType) :-
   !,
   clause( 'LGhl'(Id,RootType), _ ).

lgi_mk_index_gp0(hl,Length,Id,RootType) :-
   Length > 0,
   clause( 'LGhl'(Id,Length,RootType), _ ).


% create index for a goal table header
lgi_mk_index_at(
      ii(RootType,Guide,HypoS,Length), 
      GTableId ) :-
   assert( 'LGg'(RootType,HypoS,Length,Guide,GTableId) ).



% lookup index for a goal table header
lgi_mk_index_gt(
      ii(UnifS,Guide,HypoS1,Length),   
      GTableId ) :- 
   clause( 'LGg'(RootType,HypoS2,Length,Guide,GTableId), _ ),  
   lgi_same_hypos(HypoS2,HypoS1),
   member(RootType,UnifS). % check root type



%% ----------------------------------------------------------
%%   guess premises for a subgoal
%% ----------------------------------------------------------

%% lgi_guess_premises(+GoalInfo,-OwnInfo,-HeadProjInfo,-RestInfo)
%% - guess the premises for an arbitrary subconstituent
%% - RestInfo: difference between GoalInfo and OwnInfo
%%
lgi_guess_premises(
      cin(GoalGuide,GoalHypoS),  % in
      c(OwnGuide-Rest,OwnHypoS,_Cat2,_G2,_C2,_Str2),  % own
      cin(GuideOut,HypoSOut) ) :- % out
   lgi_guide_diff(GoalGuide,OwnGuide-Rest,GuideOut,RealOwnGuide),
   lgi_ne_premises(RealOwnGuide,OwnHypoS), % exclude empty premise set
   lgi_set_diff(GoalHypoS,OwnHypoS,HypoSOut).  % guess OwnHypoS



%% lgi_ne_premises(+Guide,+HypoS)
%% -  Guide\HypoS  empty  nonempty
%%    empty         -       B      
%%    neempty       A       A
% A
lgi_ne_premises( [_|_], _HypoS ).
% B
lgi_ne_premises( [], [_|_] ).


%% ----------------------------------------------------------
%%   guide compression
%% ----------------------------------------------------------

%% lgi_compress_guide(+CategoryCUF,-ComprGuidePl,-MainKeyWord)
%% lgi_compress_guide1(+GuideCUF,+Accu,-ComprGuidePl)
%% - GuideCUF: CUF list of CUF constants
%% - Accu,Guide: Prolog list of  (PrologAtom-NatNumber) terms
%% - MainKeyWord: a member of Guide, the current heuristics is
%%   that the first element of Guide is taken (so, it's a task
%%   of the guide extractor to put the essential keyword first)
%% REMARK: guide must not be empty!
%%
lgi_compress_guide(CategoryCUF,ComprGuide,KeyWord) :-
   cuf_goal( guide, [CategoryCUF], GuideCUF, GuideGoal ),
   cuf_prove( undelayed_only, [GuideGoal], _GoalS, [], _ConstrS, _ClL ),
   lgi_compress_guide1(GuideCUF,[],ComprGuide1,KeyWord),
   keysort(ComprGuide1,ComprGuide). 
   % for efficiency, keysort should be integrated into insert_keyword


lgi_compress_guide1(GuideCUF,Accu,CGuide,KeyWord) :-
   ( ( cuf_path_value(GuideCUF,['F'],KeyWordCUF),
       cuf_path_value(GuideCUF,['R'],RestGuideCUF) )
     -> ( cuf_atom(KeyWord,KeyWordCUF),
          lgi_insert_keyword(Accu,KeyWord,Accu1),
          lgi_compress_guide1(RestGuideCUF,Accu1,CGuide,_KW1) )
     ; ( cuf_atom([],GuideCUF)   % end of word liste
         -> ( CGuide = Accu ) 
         ;  lgi_error(illformed_guide(GuideCUF) ) ) ).


%% lgi_insert_keyword(+GuideIn,+KeyWord,-GuideOut)
%% - map a multiset on a set whose members are annotated with
%%   the respective number of occurrences in the multiset
%% - IF KeyWord does not occur in GuideIn
%%   THEN KeyWord is added
%%   ELSE IF KeyWord occurs in GuideIn
%%        THEN its multiplicity is increased
%%
lgi_insert_keyword( [], KeyWord, [ (KeyWord-1) ] ).

lgi_insert_keyword( 
      [( KeyWord2-Mult) | GuideIn ],
      KeyWord1,
      GuideOut ) :-
   ( KeyWord1 = KeyWord2 )
   -> ( ( Mult1 is Mult + 1 ),
        ( GuideOut = [ ( KeyWord2 - Mult1 ) | GuideIn ] ) )
   ; ( % ( \+ KeyWord1 = KeyWord2 )
       lgi_insert_keyword(GuideIn,KeyWord1,Guide1),
       ( GuideOut = [ ( KeyWord2 - Mult ) | Guide1 ] ) ).



%% ----------------------------------------------------------
%%   pset difference
%% ----------------------------------------------------------

%% lgi_set_diff(+BigPSet,-SmallPSet,-RestPSet)
%% - guess a subset SmallPSet from BigPSet 

% Element is also Element of the SmallSet
lgi_set_diff( 
      [Element|BigSet], 
      [Element|SmallSet], 
      RestSet ) :-
   lgi_set_diff(BigSet,SmallSet,RestSet).

% omit Element from SubSet
lgi_set_diff( 
      [Element|BigSet], 
      SmallSet,
      [Element|RestSet] ) :-
   lgi_set_diff(BigSet,SmallSet,RestSet).

lgi_set_diff([],[],[]).


lgi_guide_diff(BigGuide,SmallGuide,RestGuide) :-
   lgi_guide_diff(BigGuide,SmallGuide,RestGuide,_RealSmallGuide).


lgi_guide_diff( Guide-Rest1, Guide-Rest2, Rest2-Rest1, RealSmallGuide ) :-
   lgi_simple_guide_diff(Guide,Rest1,RealGuide),
   lgi_simple_guide_diff(RealGuide,RealSmallGuide,RealRestGuide),
   lgi_simple_guide_union(RealRestGuide,Rest1,Rest2).



% Element is also Element of the SmallSet
% - BigNumber = 1
lgi_simple_guide_diff(
      [ (KeyWord-1) | BigSet ], 
      [ (KeyWord-1) | SmallSet ], 
      RestSet ) :-
   lgi_simple_guide_diff(BigSet,SmallSet,RestSet).


% Element is also Element of the SmallSet
% - BigNumber > 1
lgi_simple_guide_diff(
      [ (KeyWord-BigNumber) | BigSet ], 
      [ (KeyWord-SmallNumber) | SmallSet ], 
      RestSet0 ) :-
   ( BigNumber > 1 ),
   lgi_simple_guide_diff(BigSet,SmallSet,RestSet),
   lgi_in_interval(SmallNumber,1,BigNumber),
   ( RestNumber is BigNumber - SmallNumber ),
   ( ( RestNumber = 0 )
     -> ( RestSet0 = RestSet )
     ;  ( RestSet0 = [ (KeyWord-RestNumber) | RestSet ] ) ).


% omit Element completely from SubSet
lgi_simple_guide_diff(
      [Element|BigSet], 
      SmallSet,
      [Element|RestSet] ) :-
   lgi_simple_guide_diff(BigSet,SmallSet,RestSet).

lgi_simple_guide_diff([],[],[]).

    
%% ----------------------------------------------------------
%%   pset union
%% ----------------------------------------------------------

%% lgi_string_union(+Dir,+PSet1,+PSet2,-BigPSet)
%% - Dir : Argument direction where PSet1 is Head, PSet2 is Arg
%% lgi_hypos_union(+PSet1,+PSet2,-BigPSet)

% construction of output string
lgi_string_union(left,Head,Arg,All) :-
   append(Arg,Head,All).

lgi_string_union(right,Head,Arg,All) :-
   append(Head,Arg,All).


% for hypos:
lgi_hypos_union(PSet1,PSet2,BigPSet) :-
   lgi_ordered_hypo_union(PSet1,PSet2,BigPSet).


%% lgi_ordered_hypo_union(Set1,Set2,Union)
%% - merge two ordered sets into a new ordered set 
%%  (according to their hypo-ids)

lgi_ordered_hypo_union([],Set,Set).

lgi_ordered_hypo_union([Element|List1],List2,List3) :-
   lgi_ordered_hypo_insert(List2,Element,List1,List3).


lgi_ordered_hypo_insert([],Element,List,[Element|List]).

lgi_ordered_hypo_insert(
      [ s(Element2,Cat2) | List2 ],
      s(Element1,Cat1), 
      List1,
      [ s(Element,Cat) | List3 ] ) :-
   ( Element1 > Element2 )
   -> ( ( Element = Element2 ),
        ( Cat = Cat2 ),
        lgi_ordered_hypo_insert( List2, s(Element1,Cat1), List1, List3 ) )
   ; ( ( Element2 >= Element1 )
       -> ( ( Element = Element1 ),
            ( Cat = Cat1 ),
              lgi_ordered_hypo_union( 
                 List1, 
                 [ s(Element2,Cat2) | List2 ],
                 List3 ) ) ).


% for guide:
%% lgi_guide_union(+Guide1,+Guide2,?Guide)
%%
%% - leads to strange look of guide (unneeded multiplicities)
lgi_guide_union(Guide1-Rest1,Guide2-Rest2,Guide-Rest) :-
   lgi_simple_guide_union(Guide1,Guide2,Guide),
   lgi_simple_guide_union(Rest1,Rest2,Rest).


%% lgi_simple_guide_union(Set1,Set2,Union)
%% - merge two ordered sets into a new ordered set 
%%  (according to their KeyWords)

lgi_simple_guide_union([],Set,Set).

lgi_simple_guide_union([Element|List1],List2,List3) :-
   lgi_ordered_guide_insert(List2,Element,List1,List3).


lgi_ordered_guide_insert([],Element,List,[Element|List]).

lgi_ordered_guide_insert(
      [ ( KeyWord2-Mult2 ) | List2 ],
      KeyWord1-Mult1,
      List1,
      [ ( KeyWord-Mult ) | List ] ) :-
   compare(Operator,KeyWord1,KeyWord2),
   ( ( % KeyWord1 @> KeyWord2
       Operator = (>) )
   -> ( ( KeyWord = KeyWord2 ),
        ( Mult = Mult2 ),
        lgi_ordered_guide_insert( List2, KeyWord1-Mult1, List1, List ) )
   ; ( ( % KeyWord2 @>= KeyWord1 
         Operator = (<) )
       -> ( ( KeyWord = KeyWord1 ),
            ( Mult = Mult1 ),
            lgi_simple_guide_union(
               List1,
               [ ( KeyWord2-Mult2 ) | List2 ],
               List ) )
       ; ( % KeyWord2 == KeyWord1 
           ( Operator = (=) ),
           ( KeyWord = KeyWord1 ),
           ( Mult is Mult1 + Mult2 ),
             lgi_simple_guide_union(List1,List2,List) ) ) ).



%% ----------------------------------------------------------
%%   external views on psets
%% ----------------------------------------------------------

%% lgi_hypos2ext(PSet,ExternalView)
%% - actually the internal representation 
%%   equals the external one
%%

lgi_hypos2ext(PSet,PSet).


%% --- END OF FILE: indexman.pl

%%%      File: expcat.pl                                        %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: string preprocessing, category compilation       %%%
%%%   Created:                                                  %%%
%%%  Modified: Fri Nov  8 18:19:41 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% Exports:
%%   lgi_add_hypos/4
%%   lgi_cat2fs/2
%%   lgi_collect_nt_hierarchy/1
%%   lgi_compile_lexicon/0
%%   lgi_expand_goal/6
%%   lgi_reset_goal_hypos/0
%%   lgi_same_hypos/2
%%   lgi_subsumes_cat/2
%%   lgi_unify_cat/3
%%   lgi_unify_hypos/2
%%   lgi_unify_leaves/2
%%   lgi_split_leaves/3
%% Imports:
%%   predicates:
%%     abbreviated_name/2
%%     concat/3               % CUF system / Prolog
%%     cuf_atom/2             % CUF ADT
%%     cuf_get_type/2         % CUF ADT
%%     cuf_goal/4             % CUF ADT
%%     cuf_extern2intern/4    % CUF ADT
%%     cuf_if_lookup_type_kids/2
%%     cuf_prove/6            % CUF ADT
%%     cuf_path_value/3       % CUF ADT
%%     cuf_put_path/3         % CUF ADT
%%     cuf_put_type/2         % CUF ADT
%%     length/2               % Prolog
%%     lgi_error/1
%%     lgi_leftcorner_candidate/4 
%%     lgi_mk_index_add/4
%%     lgi_mk_tree/6
%%     lgi_get_counter/2
%%     lgi_hypos2ext/2
%%     lgi_message/1
%%     lgi_reset_lex_index/0
%%     lgi_unify/3
%%     lgi_warning/1
%%   types:
%%      cat
%%      extended_cat
:- dynamic hl/4.   % hypos from the lexicon
:- dynamic hc/4.   % hypos from the top goal
% l(Id,Category,GoalS,ConstraintS,LicensedHypoS,Word) % word entries
:- dynamic l/6.    
:- dynamic 'LGnt'/1.
:- dynamic 'LGusednt0'/2. % nonterminals used in the lexicon
:- dynamic 'LGusednt'/2.  % nonterminals used in the lexicon, 
% counters:
% l        - entry id
% lexsize  - lexicon size 


%% ----------------------------------------------------------
%%   category compiler - top level -
%%
%% ----------------------------------------------------------

%% lgi_compile_lexicon/0
%% - compile CUF-clauses for sort l/1 into 
%%   Prolog facts l(Id,Category,GoalS,ConstraintS,LicensedHypoS,Word)
%%
lgi_compile_lexicon :-
   ( ( lgi_message(lexicon_compilation),
       lgi_init_compile_lexicon, 
       % produce goal schema: l(Word) 
       cuf_goal( l, [WordCUF], ExtCategoryFS, LexGoal ) )
     -> true ),  % no backtracking here
   cuf_prove(undelayed_only,[LexGoal],GoalS,[],ConstrS,_ClL),
   ( ( cuf_atom(Word,WordCUF),
       lgi_compile_cat(Word,ExtCategoryFS,GoalS,ConstrS) )
     -> true ),  % no backtracking here 
   fail.  % cause backtracking for cuf_prove/6 

% termination
lgi_compile_lexicon :-
   lgi_compress_nt_info,	 
   ( lgi_get_counter(l,LexSize) 
     ; ( ( LexSize = 0 ), % empty lexicon
         lgi_set_counter(l,LexSize) ) ),
   lgi_set_counter(lexsize,LexSize),  
   lgi_message( lexicon_count(LexSize) ).
  

%% ----------------------------------------------------------
%%   initialization steps
%% ----------------------------------------------------------

%% lgi_init_compile_lexicon/0
%%   - initializations for the lexicon compiler
%%
lgi_init_compile_lexicon :-
   lgi_reset_lex_index,
   lgi_set_counter(l,0),        % lexicon counter
   retractall(hl(_,_,_,_)), % initialize internal lexicon code - lexical hypos
   retractall(l(_,_,_,_,_,_)),  % initialize internal lexicon code - words
   % reinitialize the nonterminal hierarchy
   retractall('LGnt'(_)),
   lgi_mk_nt_hierarchy(nonterminal,Hierarchy),
   assert('LGnt'(Hierarchy)),
   retractall('LGusednt0'(_,_)),
   retractall('LGusednt'(_,_)).


% info local to a parse
lgi_reset_goal_hypos :-
   retractall(hc(_,_,_,_)).
   % initialize internal lexicon code - goal hypos

%% ----------------------------------------------------------
%%   termination steps
%% ----------------------------------------------------------

lgi_compress_nt_info :-
   clause( 'LGusednt0'(Type,UnifS), _ ), % assumes unique occurrences
   lgi_compress_nt_info(UnifS,UnifSCompressed),
   assert( 'LGusednt'(Type,UnifSCompressed) ),
   fail.

lgi_compress_nt_info.


lgi_compress_nt_info([NT|UnifS],UnifSOut) :-
   ( clause( 'LGusednt0'(NT,_UnifS1), _ )  % NT is used
   -> ( UnifSOut = [NT|UnifSOut1] )
   ;  ( \+ clause( 'LGusednt0'(NT,_UnifS1), _ ) % NT is not used
        -> ( UnifSOut = UnifSOut1 ) ) ),
   lgi_compress_nt_info(UnifS,UnifSOut1).

lgi_compress_nt_info([],[]).



%% ----------------------------------------------------------
%%   compilation of a category
%% ----------------------------------------------------------

%% ResultCatName :== cn1(CNType,CatNameAtom)
%% ThreadedCatName :== 
%%     cn0(InnerCNType,InnerCatNameAtom,CNType,CatNameAtom)
%% CNType :== 'ATOM' | 'COMPLEX'


lgi_compile_cat(Word,ExtCategoryFS,GoalS,ConstrS) :-
   cuf_path_value(ExtCategoryFS,[cat],CategoryFS),
   lgi_expand_premise(CategoryFS,Category,SlashS,Word),
   !,   % commit to this clause
   ( Category = cat( _CT1,root(RootType,_RFS1),
                     lvs(LLength,_L1,_RL1,_RemL1), _CtrS1,
                     node(_CN1,Id,[leaf(Word)]) ) ), % insert lexical tree
   lgi_inc_counter(l,Id),
   lgi_mk_index_add( 
      word, 
      ii(RootType,_W2,_HypoS2,LLength), 
      ExtCategoryFS,
      Id),
   % before assert/1 - instantion of HypoIdS !:
   lgi_add_hypos(SlashS,hl,Id,SlashIdS), 
   assert( l(Id,Category,GoalS,ConstrS,SlashIdS,Word) ).

lgi_compile_cat(Word,CategoryFS,_GoalS,_ConstrS) :-
   lgi_error( unexpandable_category( db(Word,_Path,CategoryFS) ) ).


%% lgi_add_hypos(+SlashS,+Flag,+LicId,-SlashIdS)
%% - assert the hypotheses SlashS
%% - Flag :== hl | hc.   % lexical hypos vs. topmost goal hypos
%%
lgi_add_hypos( [ s(Id,Hypo) | SlashS ], Flag, LicId, [Id|IdS] ) :-
   lgi_add_hypo(Hypo,Id,LicId,Flag),
   lgi_add_hypos(SlashS,Flag,LicId,IdS).

lgi_add_hypos([],_Flag,_Licid,[]).


lgi_add_hypo(Category,Id,LicId,Flag) :-
   concat(i,LicId,Trace),
   ( Category = cat( _CT1,root(RootType,_RFS1),
                     lvs(LLength,_L1,_RL1,_RemL1), _Ctrs1,
                     node(_CN1,Id,[leaf(Trace)]) ) ),  % insert lexical tree
   lgi_inc_counter(l,Id),
   lgi_mk_index_add( 
      trace(Flag), 
      ii(RootType,_W2,_H2,LLength), 
      _CatFS1,
      Id ),
   Head =.. [Flag,Id,Category,[],[]],
( ( Flag = hc )   % for 'chart hypos' 
  -> ( write(Id),put(44) )
  ;  true ),
   assert(Head).             % GoalS, ConstrS empty, handled by emitter


%% ----------------------------------------------------------
%%   category expansion
%%
%% - translate CategoryFS into a Prolog term Category
%% - add 'NT' (compiled nonterminal info) and 'SLASH-IDS' information 
%% ----------------------------------------------------------

%% lgi_expand_premise(+CategoryFS,-Category,-SlashS,+Word)
%% lgi_expand_premise(+CategoryFS,-Category,+SlashAccu,-SlashS,+DebugInfo)
%% - SlashS :== list( s(ItemId,Category) )
%% - DebugInfo :== db(Word,InvertedPath,CategoryFS)
%%
lgi_expand_premise(CategoryFS,Category,SlashS,Word) :-
   lgi_expand_premise( CategoryFS, Category, [], SlashS, _CNTCN1,
                       db(Word,[],CategoryFS) ),
   % 1. mark Category as a word
   % 2. word is its own leftcorner
   ( Category = cat( WordCatType, _R1, _L1,          
                     ctrs(WordCatType,_CF1), 
		     _Tree1 ) ),
   cuf_extern2intern(word,_,_,WordCatType).


lgi_expand_premise(
      CategoryFS,
      Category,
      SlashSIn, SlashSOut,
      cn1(CNType,CatName),
      db(Word,Path,TopCategoryFS) ) :-
   ( cuf_path_value(CategoryFS,[root],RootFS)
     -> lgi_expand_root(
           RootFS,
           Root,
           RootName,
           db( Word, [root|Path], TopCategoryFS) )
     ; lgi_warning( missing_feature( db( Word, [root|Path], TopCategoryFS))) ),
   ( % leaves:LeavesFS &
     cuf_path_value(CategoryFS,[leaves],LeavesFS)
     -> ( lgi_expand_leaves(
             LeavesFS,
             [],
             _RevLeaves,
             cn0('ATOM',RootName,CNType,CatName),
             SlashSIn, SlashSOut,
             Category,  % head
             db( Word, [leaves|Path], TopCategoryFS) ),
          ( Category = 
              cat( ConstType, Root, _L6, _DConstrS6, _PI6 ) ),
          % produce an FS which contains only the ConstituentType information
          ( ( cuf_get_type(CategoryFS,TypeCode),
              cuf_put_type(TypeCode,ConstType) )
              -> true )  % no backtracking for this part % because of CUF bug
         )
     ; ( lgi_error( missing_feature( 
                         db( Word, [leaves|Path], TopCategoryFS) ) ),
         fail ) ).


lgi_expand_goal(
      CategoryFS,
      arg( ConstituentType, Dir, Root, Leaves, HypoSInt, 
           ctrs(LCCType,CompletionFlag), 
           _PI1, _HP1 ),
      SlashSIn, SlashSOut,
      cn1(CNType,CatName),
      db(Word,Path,TopCategoryFS) ) :-
   lgi_expand_premise(
      CategoryFS,
      cat( ConstituentType, Root, Leaves, _Ctrs2, _PI2 ),
      SlashSIn, SlashS1,
      cn1(CNType1,CatName1),
      db(Word,Path,TopCategoryFS) ),
   cuf_put_path(CategoryFS,[dir],DirCUF),
   ( cuf_atom(Dir,DirCUF) % CUF compil/er checked already for 'left' or 'right'
     -> true
    ; cuf_var(DirCUF) ), % leave Dir unspecified
   ( cuf_path_value(CategoryFS,[slash],Slash)
     -> lgi_expand_slash(
           Slash,
           HypoS,
           cn0(CNType1,CatName1,CNType,CatName),
           SlashS1,SlashSOut,
           db(Word,[slash|Path],TopCategoryFS) )
     ; ( lgi_warning( missing_feature( db(Word,[slash|Path],TopCategoryFS) ) ),
         lgi_message( default_feature_value([slash|Path],[]) ),
         ( HypoS = [] ),
         ( CNType = CNType1 ),
         ( CatName = CatName1 ),
         ( SlashSOut = SlashS1 ) ) ),
   ( ( lgi_hypos2ext(HypoSInt,HypoS),
       cuf_put_path(CategoryFS,[constraints],ConstraintSFS),
       cuf_put_path(ConstraintSFS,[completion_state],CompletionFlag),
       cuf_put_path(ConstraintSFS,[leftcorner],LCConstraintFS),
       % produce an FS which contains only the type information
       cuf_get_type(LCConstraintFS,LCTypeCode),
       cuf_put_type(LCTypeCode,LCCType) )
     -> true ).  % no backtracking for this part


lgi_expand_root(RootFS,root(IntRoot,RootFS),RootName,
                db(Word,Path,TopCategoryFS) ) :-
   ( cuf_path_value(RootFS,[syn],NT)
     -> ( lgi_classify_nt(NT,IntRoot,RootName)
          -> true   % no backtracking
          ;  ( % \+ lgi_classify_nt(NT,_IntRoot,_RootName),
               lgi_warning(
                  not_nt_classifiable(
                     NT,
                     db(Word,[syn|Path],TopCategoryFS) ) ),
               ( RootName = top ) ) )
      ;  ( lgi_warning( 
              missing_feature( db( Word, [syn|Path], TopCategoryFS ) ) ),
           ( RootName = top ) ) ).


lgi_expand_leaves(
       LeavesFS,
       FrontLeaves,
       RevLeaves,
       cn0(InnerCNType,InnerCatName,CNType,CatName), 
       SlashSIn, SlashSOut,
       cat( _CT3, Root,                    % head
            lvs(LLength,Leaves,RevLeaves,_RemL3),
            ctrs(LCCandIn,CFlag), 
            node(CatName,HeadId,_HeadDtrs) ),
       db(Word,Path,TopCategoryFS) ) :-
   ( ( cuf_path_value(LeavesFS,['F'],ArgFS),
       cuf_path_value(LeavesFS,['R'],RestLeavesFS) )
     -> ( lgi_expand_goal(
             ArgFS,
             Arg,
             SlashSIn,SlashS1,
             cn1(ArgCNType,ArgCatName),
             db( Word, ['F'|Path], TopCategoryFS ) ),
          lgi_expand_leaves(
             RestLeavesFS,
             FrontLeaves1,
             RestRevLeaves,
             cn0(InnerCNType,InnerCatName,HPCNType,HPCatName),
             SlashS1, SlashSOut,
             HeadProjection,
             db( Word, ['R'|Path], TopCategoryFS ) ),
          ( HeadProjection = 
              cat( HeadProjCatType, Root, 
                   lvs(RestLLength,RestLeaves,_RL2,_RemL2),
                   ctrs(LCCand2,CFlag), 
                   HeadProjTree) ),
          ( HeadProjectionSketch =  % omit leaves to avoid cyclic structure
              hp( HeadProjCatType, Root, RestLLength,
                   ctrs(LCCand2,CFlag), 
                   HeadProjTree) ),
          append( RestRevLeaves, [ a(Arg,FrontLeaves1)], RevLeaves ),
          append( FrontLeaves, [ Arg ], FrontLeaves1 ),  
          cuf_extern2intern(derived_phrase,_,_,HeadProjCatType),
          ( LLength is RestLLength + 1 ),
          ( Arg = arg( _CT1, Dir, _R1, _L1, _H1,
                       ctrs(ArgLC,_CF1), 
                       node(_ACN,ArgId,_ADtrs),
                       HeadProjectionSketch ) ),
          lgi_leftcorner_candidate(Dir,LCCandIn,ArgLC,LCCand2),
          ( HeadProjTree = node(_HPCN,HPId,_HPDtrs) ),
          lgi_mk_tree(Dir,HPCatName,HPId,HeadId,ArgId,HeadProjTree),
          lgi_mk_cgname( Dir, HPCNType, HPCatName,
                         ArgCNType, ArgCatName, CNType, CatName ),
          ( Leaves = [ Arg | RestLeaves ] ) )
     ;  ( cuf_atom(Leaves,LeavesFS)  % Leaves = []
          -> ( ( CNType = InnerCNType ),
               ( CatName = InnerCatName ),
               ( SlashSOut = SlashSIn ),
               ( LLength = 0 ),
               ( RevLeaves = [] ) )
         ; ( % cuf_var(LeavesFS) % or maybe something else
             lgi_error(
                missing_feature( db(Word,Path,TopCategoryFS) ) ),
             fail ) ) ).


lgi_expand_slash(
      HypoSFS,
      HypoS,
      cn0(InnerCNType,InnerCatName,CNType,CatName), 
      SlashSIn, SlashSOut,
      db(Word,Path,TopCategoryFS) ) :-
   ( % non empty slash
     cuf_path_value(HypoSFS,['F'],HypoFS),
     cuf_path_value(HypoSFS,['R'],RestSlashFS) )
   -> ( lgi_expand_premise(
           HypoFS,
           Hypo,
           SlashSIn,SlashS1,
           cn1(HypoCNType,HypoCatName),
           db(Word,['F'|Path],TopCategoryFS) ),
        lgi_expand_slash(
           RestSlashFS,
           RestSlash,
           cn0(InnerCNType,InnerCatName,CNType1,CatName1), 
           SlashS1,SlashSOut1,
           db(Word,['R'|Path],TopCategoryFS) ),
        ( HypoS =  [ s(HId,Hypo) | RestSlash ] ),
        ( Hypo = cat( HypoCatType, _R2, _L2, ctrs(HypoCatType,_CF2),
                      node(_HCN,HId,_HDtrs) ) ),
        cuf_extern2intern(trace,_,_,HypoCatType),
        lgi_mk_cgname( 'HYPO',CNType1,CatName1,HypoCNType,
                       HypoCatName,CNType,CatName),
        ( SlashSOut = [ s(HId,Hypo) | SlashSOut1 ] ) )
   ;  ( ( CNType = InnerCNType ),
        ( CatName = InnerCatName ),
        ( SlashSOut = SlashSIn ),
        ( HypoS = [] ),
        ( cuf_atom([],HypoSFS) % end of slash / empty slash
          -> true
          ;  ( cuf_var(HypoSFS) 
               -> ( lgi_warning( 
                       missing_feature( db(Word,Path,TopCategoryFS) ) ),
                    lgi_message( 
                       default_feature_value(Path,[]) ),
                    ( HypoS = [] ) ) ) ) ).


%% ----------------------------------------------------------
%%   hierarchy construction
%% ----------------------------------------------------------

%% lgi_mk_nt_hierarchy(+TypeSymbolProlog,-Hierarchy)
%% lgi_mk_nt_hierarchy(+TypeSymbolProlog,+Upper,-Lower,-Hierarchy)
%% - Hierarchy
%%   Node ::= t(TypeSymbolCUF,r(Type,UnifiableTypeS),BasicCatName)
%%   Hierarchy ::= h(Node,NodeList)
%%
lgi_mk_nt_hierarchy(Type,Hierarchy) :-
   lgi_mk_nt_hierarchy(Type,[],_Lower,Hierarchy).
 

lgi_mk_nt_hierarchy( 
      Type, 
      Upper,
      [Type|Lower],
      h( s(TypeFS,r(Type,UnifS),CatName), Hierarchies ) ) :-
   cuf_if_lookup_type_kids(Type,Kids),
   lgi_type_offspring(Kids,[Type|Upper],Lower,Hierarchies),
   append(Upper,[Type|Lower],UnifS),
   % construct an FS for Type for later subsumption checks
   cuf_extern2intern(Type,_,_,TypeFS), 
   ( ( current_predicate(abbreviated_name,_),
       abbreviated_name(Type,CatName) )  % look up abbreviation
     -> true
     ; CatName = Type ).


lgi_type_offspring(
      [ Kid | Kids ],
      Upper,
      Lower,
      [ Hierarchy | Hierarchies ] ) :-
   lgi_mk_nt_hierarchy(Kid,Upper,Lower1,Hierarchy),
   append(Lower1,Lower2,Lower),
   lgi_type_offspring(Kids,Upper,Lower2,Hierarchies).

lgi_type_offspring([],_Upper,[],[]).



%% ----------------------------------------------------------
%%   classifier
%% ----------------------------------------------------------

%% lgi_classify_nt(+TypeCUF,-IntTypeOut,-CatNameOut)
%% lgi_classify_nt(+TypeCUF,+Hierarchy,+IntTypeIn,-IntTypeOut,
%%                 +CatNameIn,-CatNameOut)
%% - classify TypeCUF in Hierarchy, and return its IntType
%% - Hierarchy, IntType: same as in lgi_mk_nt_hierarchy/3
%% - failure, if TypeCUF cannot be classified under any node
%%
lgi_classify_nt(TypeCUF,Type,CatName) :-
   clause('LGnt'(Hierarchy),_)
   -> ( lgi_classify_nt(TypeCUF,Hierarchy,_,r(Type,UnifS),_,CatName),
        ( clause( 'LGusednt0'(Type,UnifS), _ )
          -> true
          ;
	( ( \+ clause( 'LGusednt0'(Type,UnifS), _ ) )
               -> assert( 'LGusednt0'(Type,UnifS), _ ) ) ) )
   ; ( lgi_error(no_nt_hierarchy),
       fail ).


lgi_classify_nt(
      TypeCUF,
      h( s(MotherCUF,IntType1,CatName1), Hierarchies ), 
      _TM,
      IntType,
      _CN,
      CatName ) :-
   cuf_subsumes(MotherCUF,TypeCUF),
   lgi_classify_nt_l(Hierarchies,TypeCUF,IntType1,IntType,
		     CatName1,CatName).


lgi_classify_nt_l(
      [ Hierarchy | Hierarchies ],
      TypeCUF,
      IntTypeIn,
      IntTypeOut,
      CatNameIn,
      CatNameOut ) :-
   lgi_classify_nt(TypeCUF,Hierarchy,IntTypeIn,IntTypeOut,
                   CatNameIn,CatNameOut)
   -> true  % exclude second alternative
   ;
   lgi_classify_nt_l(Hierarchies,TypeCUF,IntTypeIn,IntTypeOut,
                     CatNameIn,CatNameOut).

% not subsumed by any of the daughters ==> Mother is smallest supertype
lgi_classify_nt_l([],_TypeCUF,IntType,IntType,CatName,CatName).



%% ----------------------------------------------------------
%%   hierarchy grapher
%% ----------------------------------------------------------

%%  lgi_collect_nt_hierarchy(+CompressedHierarchy)
%%
lgi_collect_nt_hierarchy(CompressedHierarchy) :-
   clause( 'LGnt'(Hierarchy), _ )
   -> lgi_collect_nt_hierarchy1(Hierarchy,CompressedHierarchy) 
   ;  ( lgi_error(no_nt_hierarchy),
        fail ).
  

lgi_collect_nt_hierarchy1(
      h( s(_MotherCUF,_TypeMellish,CatName), Hierarchies ),
      CHierarchy ) :-
   lgi_collect_nt_hierarchies(Hierarchies,CHierarchies),
   CHierarchy =.. [CatName|CHierarchies].


lgi_collect_nt_hierarchies(
      [ Hierarchy | Hierarchies ],
      [ CHierarchy | CHierarchies ] ) :-
   lgi_collect_nt_hierarchy1(Hierarchy,CHierarchy),
   lgi_collect_nt_hierarchies(Hierarchies,CHierarchies).

lgi_collect_nt_hierarchies([],[]).


%% ----------------------------------------------------------
%%   transform Prolog catname into a Prolog atom
%% ----------------------------------------------------------

%% lgi_mk_cgname(Direction,+ValCNType,+ValueCatName,
%%               +ArgCNType,+ArgCatName,-CNType,-CatName)
%% - produce a category description for a complex category
%% - reconstruct CatFS
%% - Direction :== left | right | Variable | 'HYPO'
%
lgi_mk_cgname( 
      Dir,
      ValCNType,ValCatName, 
      ArgCNType,ArgCatName, 
      'COMPLEX', CatName ) :-
   ( ( ( Dir = 'HYPO') ; var(Dir) )
     -> ( Op = '|' )
     ; ( ( Dir = left )
          -> ( Op = '\\' )
          ; ( ( Dir = right )
              -> ( Op = (/) ) ) ) ),
   lgi_mk_cgname2(ValCNType,ValCatName,ValCatName1),
   lgi_mk_cgname2(ArgCNType,ArgCatName,ArgCatName1),
   concat(Op,ArgCatName1,PrefixedArg),
   concat(ValCatName1,PrefixedArg,CatName).


%% lgi_mk_cgname2(+CNType,+CatName,-ExtendedCatName) 
%% - add parentheses if complex category
%%
lgi_mk_cgname2(CNType,CatName,ExtendedCatName) :- 
   ( CNType = 'ATOM' )
   -> ( ExtendedCatName = CatName )
   ; ( ( CNType = 'COMPLEX' )
       -> ( concat( CatName, ')', ExtendedCatName1 ),
            concat( '(', ExtendedCatName1, ExtendedCatName ) ) ).


%% ----------------------------------------------------------
%%   subsumption routine for internal category structures
%% ----------------------------------------------------------

%% lgi_subsumes_cat( 
%%    c1(Cat1,GoalS1,ConstrS1), 
%%    c1(Cat2,GoalS2,ConstrS2) )
%% - incorrectness: goals and constraints are ignored
%% - print info is never checked
%% - cattype, leftcorner, and completion flag are bottom-up info
%%   hence it doesn't make much sense to check them for subsumption
%% 
lgi_subsumes_cat(
      c1( cat( _CT1,
               root(_RootType1,RootFS1), 
               lvs(LLength,Leaves1,_RevL1,_RemL1), 
               _DCtrs1, _PI1 ),
          _GoalS1, 
          _ConstrS1 ),
      c1( cat(_CT2,
              root(_RootType2,RootFS2), 
              lvs(LLength,Leaves2,_RevL2,_RemL2),
              _DCtrs2, _PI2 ),
          _GoalS2, 
          _ConstrS2 ) ) :-
    cuf_subsumes(RootFS1,RootFS2),   
    ( ( var(Leaves1) ; var(Leaves2) )
      -> lgi_error( instantiation_error('expcat:lgi_subsumes_cat/2') )
      ;  % subsumption relation must be inverted for premises
         lgi_subsumes_leaves(Leaves2,Leaves1) ).


%% lgi_subsumes_leaves(GoalLeaves,PremiseLeaves)
%%
lgi_subsumes_leaves(Leaves1,Leaves2) :-
   ( Leaves1 = Leaves2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( Leaves1 = [ arg( CatType1,Direction,Root1,Leaves11,HypoS1,DConstrS1,
                        _PI1,_HP1)
                   | RestLeaves1 ],
       Leaves2 = [ arg( CatType2,Direction,Root2,Leaves21,HypoS2,DConstrS2,
                        _PI2,_HP2)
                   | RestLeaves2 ],
      lgi_subsumes_cat(
         c1( cat(CatType1,Root1,Leaves11,DConstrS1,_PI3), [], [] ),
         c1( cat(CatType2,Root2,Leaves21,DConstrS2,_PI4), [], [] ) ),
      % subsumption relation must be inverted for premises
      lgi_subsumes_hypos(HypoS2,HypoS1),  
      lgi_subsumes_leaves(RestLeaves2,RestLeaves1) ).

%%
%%
lgi_subsumes_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for s([],[]) )
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,Cat2) | RestHypoS2 ],
      lgi_subsumes_cat(
         c1(Cat1,[],[]),
         c1(Cat2,[],[]) ), 
      lgi_subsumes_hypos(RestHypoS1,RestHypoS2) ).


%% ----------------------------------------------------------
%%  identity of hypo lists
%% ----------------------------------------------------------

%%
%%
lgi_same_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for s([],[]) )
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,_Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,_Cat2) | RestHypoS2 ],
      lgi_same_hypos(RestHypoS1,RestHypoS2) ).


%% ----------------------------------------------------------
%%   unification routine for internal category structures
%% ----------------------------------------------------------


%% lgi_unify_cat( 
%%    c1(Cat1,GoalS1,ConstrS1), 
%%    c1(Cat2,GoalS2,ConstrS2),
%%    c1(_ResultCat,GoalSOut,ConstrSOut) )
%% !!! currently ResultCat is uninstantiated
%% - no unification of 'print info' 
%%
lgi_unify_cat(
      c1( cat( CatType1,root(_RootType1,RootFS1), 
               lvs(LLength,Leaves1,_RevL1,_RemL1), 
          ctrs(LC1,Flag1), _PI1 ),
          GoalS1, ConstrS1 ),
      c1( cat( CatType2,root(_RootType2,RootFS2), 
               lvs(LLength,Leaves2,_RevL2,_RemL2),
          ctrs(LC2,Flag2), _PI2 ),
          GoalS2, ConstrS2 ),
      c1( _Cat,     % currently ignored
          GoalSOut, ConstrSOut ) ) :-
    cuf_unify(CatType1,CatType2),
    cuf_unify(RootFS1,RootFS2),   
    ( ( var(Leaves1) ; var(Leaves2) )
      -> lgi_error( instantiation_error('expcat:lgi_unify_cat/3') )
      ;  ( lgi_unify_leaves(Leaves1,Leaves2),
           cuf_unify(LC1,LC2),      % leftcorner/leftcorner constraint
           lgi_unify( d(Flag1,GoalS1,ConstrS1),  % completion flag
                      d(Flag2,GoalS2,ConstrS2),
                      d(_,    GoalSOut,ConstrSOut) ) ) ).


%% lgi_unify_leaves(GoalLeaves,PremiseLeaves)
%%
%% - HypoCatS have to be guaranteed to be identical 
%%   due to abstraction operation, hence they can be ignored here
%%
lgi_unify_leaves(Leaves1,Leaves2) :-
   ( Leaves1 = Leaves2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( Leaves1 = [ arg( CatType1,Direction,Root1,Leaves11,HypoS1,ConstrS1,
                        _PI1,_HP1)
                   | RestLeaves1 ],
       Leaves2 = [ arg( CatType2,Direction,Root2,Leaves21,HypoS2,ConstrS2,
                        _PI2,_HP2)
                   | RestLeaves2 ],
      lgi_unify_cat(
         c1( cat(CatType1,Root1,Leaves11,ConstrS1,_PI3), [], [] ),
         c1( cat(CatType2,Root2,Leaves21,ConstrS2,_PI4), [], [] ),
         _Result ),
      lgi_unify_hypos(HypoS1,HypoS2),
      lgi_unify_leaves(RestLeaves1,RestLeaves2) ).


%%
%%
lgi_unify_hypos(HypoS1,HypoS2) :-
   ( HypoS1 = HypoS2 )   % Prolog unification (in particular for [])
   -> true   % no backtracking
   ; ( HypoS1 = [ s(HId,Cat1) | RestHypoS1 ],
       HypoS2 = [ s(HId,Cat2) | RestHypoS2 ],
      lgi_unify_cat(
         c1(Cat1,[],[]),
         c1(Cat2,[],[]),
         _ ),
      lgi_unify_hypos(RestHypoS1,RestHypoS2) ).


%% lgi_split_leaves(+GoalReversedLeaves,+PremiseReversedLeaves,-RestLeaves)
%% - split PremiseReversedLeaves into GoalReversedLeaves 
%%   and RestLeaves to be parsed


lgi_split_leaves(
      [],
      [ a(_Arg,FrontLeaves) | _RestLeaves ],
      FrontLeaves ) :-
   !.

lgi_split_leaves(
      [ a( arg(CatType1,Direction,Root1,Leaves11,HypoS,ConstrS1,_PI1,_HP1 ),
           _RestLeaves1 )
        | RestLeaves1 ],
      [ a( arg(CatType2,Direction,Root2,Leaves21,HypoS,ConstrS2,_PI2,_HP2 ),
           _RestLeaves2 )
        | RestLeaves2 ],
      FrontLeaves ) :-
   lgi_unify_cat(
      c1( cat(CatType1,Root1,Leaves11,ConstrS1,_PI3), [], [] ),
      c1( cat(CatType2,Root2,Leaves21,ConstrS2,_PI4), [], [] ),
      _Result ),
   lgi_split_leaves(RestLeaves1,RestLeaves2,FrontLeaves).


%% ----------------------------------------------------------
%%  reverse compilation: calculate FS from cat/ term (for chart browser)
%% ----------------------------------------------------------

%% lgi_cat2fs(+Category,-CatFS)
%% - produce a feature structure which corresponds to Category
%
lgi_cat2fs(
      cat( CatFS,   % Const Type info
           root(_RT1,RootFS),
           lvs(_LL,Leaves,_RL1,_RemL1), 
           _CtrS1, _PI1 ),
      CatFS ) :-   % insert Const Type info
   cuf_put_path(CatFS,[root],RootFS),
   cuf_put_path(CatFS,[leaves],LeavesFS),
   lgi_leaves2fs(Leaves,LeavesFS).


lgi_leaves2fs([],LeavesFS) :-
   !,
   cuf_atom([],LeavesFS).

lgi_leaves2fs(
      [ arg(CatType,_Dir,Root,Leaves1,HypoS,ctrs(LC,Compl),_PI1,_HP1)
        | RestLeaves ],
      LeavesFS) :-
   cuf_extern2intern(nelist,_,_,LeavesFS), 
   cuf_put_path(LeavesFS,['F'],ArgFS),
   cuf_put_path(LeavesFS,['R'],RestLeavesFS),
   cuf_put_path(ArgFS,[slash],HypoSFS),
   cuf_put_path(ArgFS,[constraints],ConstraintSFS),
   cuf_put_path(ConstraintSFS,[completion_state],Compl),
   cuf_put_path(ConstraintSFS,[leftcorner],LC),
   lgi_cat2fs(
      cat(CatType,Root,Leaves1,_ConstrS,_PI2),
      ArgFS ),
   lgi_hypos2fs(HypoS,HypoSFS),
   lgi_leaves2fs(RestLeaves,RestLeavesFS).


lgi_hypos2fs( [], HypoSFS ) :-
   cuf_atom([],HypoSFS).

lgi_hypos2fs(
      [ s( _HId, 
           cat( CatType,Root,Leaves,_ConstrS,_PI1) )
        | RestHypoS ],
      HypoSFS ) :-
   cuf_extern2intern(nelist,_,_,HypoSFS), 
   cuf_put_path(HypoSFS,['F'],HypoFS),
   cuf_put_path(HypoSFS,['R'],RestHypoSFS),
   lgi_cat2fs(
      cat(CatType,Root,Leaves,_ConstrS,_PI2),
      HypoFS ),
   lgi_hypos2fs(RestHypoS,RestHypoSFS).


%% --- END OF FILE: expcat.pl

%%%      File: hdgen.pl                                         %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: CUF-based semi-directional Lambek calculus       %%%
%%%            - generator variant -                            %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Nov 12 12:30:00 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%%  96-10-10: Esther Koenig: start of portation to Prolog and chart techn.
%%  fall 1993: Esther Koenig: hdgen.cuf
%% Exports:
%%   lgi_gen/3
%% Imports:
%%   Types:
%%     cat  and embedded types + subtypes
%%   Predicates:
%%     cuf_unify/2
%%     hpt/2        % head projection table
%%     lgi_add_gtable_entry/2
%%     lgi_add_hptable_entry/2
%%     lgi_fitting_gtable/2 
%%     lgi_fitting_hptable/4
%%     lgi_get_active_item/2
%%     lgi_get_passive_item/2
%%     lgi_get_gtable_entry/4
%%     lgi_get_hptable_entry/4
%%     lgi_guess_premises/3
%%     lgi_guide_union/3
%%     lgi_hypos_union/3
%%     lgi_new_gtable/2
%%     lgi_new_hptable/4
%%     lgi_string_union/4
    
% ------------------------------------------------------------
% Contents:
% 1. choice of head
% 2. parsing the arguments of the head
% ------------------------------------------------------------

%%
%% lgi_gen1(?GoalInfo,-OutInfo,-ItemId)
%% - find a gen for GoalInfo
%% - Result = instantiation of ?GoalInfo + -OutInfo (Goals and Constraints)
%% Remarks: 
%% - It does not matter which of the subsuming previous goals G, G', ...
%%   is picked, since ||Goal|| = ||G|| intersect ||G'|| subset ||G|| etc.
%% - for ressource checks: cleaner would be to use interface  pred's
%%

% ------------------------------------------------------------
% 1. choice of head
% ------------------------------------------------------------

% remark: some redundant work is not excluded, because intermediate projections
% (which previously did not have a goal as a counterpart) are not
%  checked
%%%    ---> Zugriff auf intermediate head projections in
%%         'get_active_item' einbauen bzw. ala van Noord
%%          DifferenzenListen abspeichern

lgi_gen(Goal,Out,ItemId) :-
   ( % IF Goal is subsumed by some goal in a goal table
     lgi_fitting_gtable(Goal,GTableId) 
     ->  % lookup existing solutions - there may be none
         lgi_get_gtable_entry(Goal,GTableId,Out,ItemId)
     ;  ( % ELSE IF Goal is not subsumed by any goal in a goal table
          % THEN create a new table header
          ( lgi_new_gtable(Goal,GTableId) -> true ),
          ( ( lgi_new_gen(Goal,ItemId),
              ( lgi_add_gtable_entry(GTableId,ItemId) -> true ),
              fail )  % assert all solutions for this Goal
            ; lgi_get_gtable_entry(Goal,GTableId,Out,ItemId) ) ) ).


% a fitting passive non-derived item
lgi_new_gen(Goal,ItemId) :-
   lgi_get_passive_item(Goal,ItemId).

% a new derivation from an active non-derived item
lgi_new_gen(Goal,ItemId) :-
   ( ( ( Goal = c(_Gu1,_H1,cat(CatType,_R1,_L1,_DC1,_PI1),_G1,_C1,_Str1) ),
       % to avoid CUF compiler error messages :
       cuf_extern2intern(derived_phrase,_,_,CatType1), 
       cuf_unify(CatType,CatType1) ) -> true ),
   lgi_get_active_item(Goal,Head,[Arg|Leaves],In,HeadId),  % backtrackable
   lgi_gen_args(Leaves,In,Arg,Goal,HeadId,Head,ItemId).


% ------------------------------------------------------------
% 2. gen arguments
% ------------------------------------------------------------

%% lgi_gen_args(+RestLeaves,+InInfo,+Arg,?Goal,+HeadId,+Head,-ItemId)
%% - completor step
%% - clauses for single argument check adjacency constraints 
%%   and that all remaining resources have been used
%%

% only one argument left 
lgi_gen_args( 
      [],
      cin( ArgGuide, ArgHypoSPrime ),
      Arg,
      Goal,
      HeadId, Head,
      ItemId ) :-
   lgi_gen_last_arg( 
      ArgGuide, ArgHypoSPrime, Arg,
      Goal,
      HeadId, Head,
      ItemId ).

% more than one remaining argument 
lgi_gen_args( 
      [Arg2|Leaves],  
      In,
      arg( ArgCatType, ArgDir, ArgRoot, ArgLeaves, ToBindHypoS, ArgDConstrS,
           node(_ACN,ArgId,_ADtrs),
           hp( HPCatType, Root, HPLLength, HPDConstrS, HPPI ) ),
      Goal,
      HeadId, 
      c( HeadGuide,HeadHypoS,   % head
         cat( _CT2, _R2, 
              lvs(_LL2,[_Arg1|AllLeaves],RevL,_RemL2), 
              _DC2, _PI2 ),
         GoalSIn, ConstrSIn, HeadString ),
      ItemId ) :-
   lgi_guess_premises(  % guess premises for argument goal
      In,
      c(ArgGuide,ArgHypoSPrime,_Cat15,_G15,_C15,_Str15),
      In1 ),
   ( % IF HeadProj of HeadId has been constructed earlier from these ressources
     lgi_fitting_hptable(HeadId,ArgGuide,ArgHypoSPrime,HPTableId)
        % THEN stick to these results
     -> lgi_get_hptable_entry(HPTableId,HeadProj,[Arg2|Leaves],HeadProjId)
     ;  % ELSE 
        ( % new head projection table
          ( lgi_new_hptable(HeadId,ArgGuide,ArgHypoSPrime,HPTableId)
            -> true ),
          ( ( % derive next head projection
              ( ( lgi_guide_union(ArgGuide,HeadGuide,HeadProjGuide),
                  lgi_hypos_union(ToBindHypoS,ArgHypoSPrime,ArgHypoS) )
                 -> true ),
              lgi_gen(              % gen current argument 
                 c( ArgGuide, ArgHypoS,                   % arg goal
                    cat(ArgCatType,ArgRoot,ArgLeaves,ArgDConstrS,_P12), 
                    GoalSIn, ConstrSIn, ArgString ),
                 cth(GoalS1,ConstrS1),
                 ArgId ),
              ( ( % construct head projection
                  append(HPRevL,[_],RevL),        % cut off current arg
                  lgi_string_union(ArgDir,HeadString,ArgString,HeadProjString),
                  lgi_hypos_union(HeadHypoS,ArgHypoSPrime,HeadProjHypoS),
                  ( HPPI = node(_HPCN,HeadProjId,_HPDtrs) ),
                  lgi_add_derived_item(
                     c( HeadProjGuide, HeadProjHypoS,
                        cat( HPCatType, Root,
                             lvs(HPLLength,AllLeaves,HPRevL,[Arg2|Leaves]),
                             HPDConstrS, HPPI ),
                        GoalS1, ConstrS1, HeadProjString ),
                     HeadProjId ),
                  lgi_add_hptable_entry(HPTableId,HeadProjId) )
                -> true ),
              fail )   % create all variants of this projection
          ;  lgi_get_hptable_entry( HPTableId, HeadProj, [Arg2|Leaves], HeadProjId ) ) ) ),

   lgi_gen_args(Leaves,In1,Arg2,Goal,HeadProjId,HeadProj,ItemId).



lgi_gen_last_arg( 
      ArgGuide, ArgHypoSPrime,
      arg( ArgCatType,ArgDir,ArgRoot,ArgLeaves,
           ToBindHypoS,ArgDConstrS,node(_ACN,ArgId,_ADtrs),
           hp(HPCatType,Root,HPLLength,HPDConstrS,HPPI) ),
      Goal,
      HeadId, 
      c( _HeadGuide, HeadHypoS,                   % head
         cat( _CT3, _R3, 
              lvs(_LL3,[_Arg1|AllLeaves],RevL,_RemL3), _DC3, _PI3 ),
         GoalSIn, ConstrSIn, HeadString ),  
      ItemId ) :-
   ( ArgGuide = FrontArgGuide-RestArgGuide ),
   lgi_simple_guide_diff(FrontArgGuide,RestArgGuide,RealArgGuide),
   lgi_ne_premises(RealArgGuide,ArgHypoSPrime),
   %%% almost same code as in recursive clause of lgi_gen_args/
   ( % IF HeadProj of HeadId has been constructed earlier from these ressources
     lgi_fitting_hptable(HeadId,ArgGuide,ArgHypoSPrime,HPTableId)
     -> % THEN stick to these results
        lgi_get_hptable_entry(HPTableId,_HeadProj,_Leaves,ItemId) 
     ;  % ELSE 
        ( ( ( lgi_hypos_union(ToBindHypoS,ArgHypoSPrime,ArgHypoS),
              % create new head projection table
              lgi_new_hptable(HeadId,ArgGuide,ArgHypoSPrime,HPTableId) )
            -> true ),
          ( ( lgi_gen(              % gen current argument 
                 c( ArgGuide, ArgHypoS,                   % arg goal
                    cat(ArgCatType,ArgRoot,ArgLeaves,ArgDConstrS,_P12), 
                    GoalSIn, ConstrSIn, ArgString ),
                 cth(GoalS1,ConstrS1),
                 ArgId ),
              ( ( ( Goal = c(HeadProjGuide,GoalHypoS,GoalCat,_G2,_C2,_Str2) ),
                  ( GoalCat =
                       cat( _CT2, _R2, _L2, ctrs(_LCC2,GoalCFlag), _PI2 ) ),
                  ( HPCat =
                       cat( HPCatType, Root,
                            lvs(HPLLength,AllLeaves,HPRevL,_RemL4),
                            HPDConstrS, HPPI ) ),
                  % recover bindings of hypos
                  lgi_hypos_union(HeadHypoS,ArgHypoSPrime,HeadProjHypoS),
                  lgi_unify_hypos(GoalHypoS,HeadProjHypoS),       
                  % instantiate completion flag
                  cuf_atom(done,GoalCFlag),  
                  % propagate synthesized info into Goal
                  % goals & constrs of Goal checked at head lookup
                  lgi_unify_cat( 
                     c1(GoalCat,[],[]), 
                     c1(HPCat,GoalS1,ConstrS1), 
                     c1(_,GoalS2,ConstrS2) ),
                  % construct head projection
                  append(HPRevL,[_],RevL),        % cut off current arg
                  lgi_string_union(ArgDir,HeadString,ArgString,HeadProjString),
                  ( HPPI = node(_HPCN,HeadProjId,_HPDtrs) ),
                  lgi_add_derived_item(
                     c( HeadProjGuide, HeadProjHypoS, HPCat,
                        GoalS2, ConstrS2, HeadProjString ),
                     HeadProjId ),
                  lgi_add_hptable_entry(HPTableId,HeadProjId) )
                 -> true ),
               fail )  % assert all variants of this projection
              ; % lookup projections
              lgi_get_hptable_entry(HPTableId,_HeadProj,_Leaves,ItemId) ) ) ).


%% --- END OF FILE: hdgen.pl





%%%      File: lgg_uif.pl                                       %%%
%%%    Author: Esther Koenig                                    %%%
%%%   Purpose: user interface for the generator                 %%%
%%%   Created:                                                  %%%
%%%  Modified: Tue Nov 12 13:13:21 1996 (esther)                %%%
%%% Copyright: Institut fuer maschinelle Sprachverarbeitung     %%%
%%%               Universitaet Stuttgart                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History:
%%
%% 95-01-21 Esther Koenig
%% Exports:
%%    g/1, g/2, g/3
%%   show_item/1
%% Imports:
%%   Predicates:
%%     cuf_prolog_term/2 (CUF ADT)
%%     cuf_put_path/3 (CUF-ADT)
%%     get_flag/2     (CUF system)
%%     lgi_cat2fs/2
%%     lgi_cuf_prove/6
%%     lgi_error/1
%%     lgi_get_counter/2
%%     lgi_get_flag/2
%%     lgi_file_name/3
%%     lgi_init_chart/3
%%     lgi_mk_item_fs/7
%%     lgi_gen/3
%%     lgi_reset_chart/0
%%     lgi_set_counter/2
%%     lgi_tcl_output/3
%%     lgi_test_file_prefix/1  (local grammar)
%%     lgi_visit_item/7
%%     mk_text_item_fs/6
%%     pp_fs/2        (CUF-ADT)
%%     visit_text_item/6
%%   Sorts:
%%     grammar_name/0
%%     psent/1
%%   Types:
%%     cat
%% Counters:
%%   hcoffset  % 
%%   l         % entry no
%%   rno       % reading number    

%% g(+Number)
%% g(+Number,+SubTestNumber)
%% - call the generator with test structure Number,SubTestNumber
%% - search for all alternatives
%%
g(Number) :-
   g(Number,1).


g(Number,SubNumber) :-
   statistics(runtime,[StartTime|_]),
   lgi_g(Number,SubNumber,StartTime).


%% lgi_g(+Number,+SubNumber,+StartTime)
%%
lgi_g(Number,SubNumber,_StartTime) :-
   lgi_init_gen(Number,SubNumber,Goal),
   lgi_gen(Goal,_Out,ItemId),  
 nl,write('RESULT':ItemId), nl,
   fail.

lgi_g(_Number,_SubNumber,StartTime) :-
   statistics(runtime,[End|_]),
   Total is End - StartTime,
   lgi_get_counter(l,LastItemId),     
   lgi_get_counter(hcoffset,OffSet),  % primitive entries
   Items is LastItemId - OffSet,      % derived entries
   nl,write('TOTAL TIME':Total), 
   write('  / number of derived items': Items ),nl,
   nl.


%% g(+GoalFT,-ResultFT,-ResultItemId,-GeneratedSentencePl)
%% g(+Number,+SubNumber,-ResultFT,-ResultItemId,-GeneratedSentencePl)
%% - call the generator with test sentence Number
%% - return a ResultFS and GeneratedSentencePl (if there is a successful generation)
%% - ResultFT :==  d(ResultCatFS,ResiduatedGoalS,ResiduatedConstrS)
%%
g(Number,SubNumber,ResultFT,ItemId,Sentence) :-
   ( ( % remove old generator input
       retractall( grammar_name(_) ),
       retractall( cat(_) ),
       lgi_lookup_parse_result(Number,SubNumber,GoalFT,_GrammarName) ) 
     -> true ),  % no backtracking
   g(GoalFT,ResultFT,ItemId,Sentence).


g( GoalFT, d(CatFS,GoalS,ConstrS), ItemId, Sentence ) :-
   ( lgi_init_gen(GoalFT,Goal) -> true ),
   lgi_gen(
      Goal,
      cth(GoalS,ConstrS),
      ItemId),
   ( Goal = c(_S1,_H1,GoalCat,_G1,_C1,Sentence) ), 
   ( lgi_cat2fs(GoalCat,CatFS) -> true ).


%% lgi_lookup_parse_result(+TestNumber,+ReadingNumber,-GoalFT,-GrammarName)
%% - read GoalFT (and GrammarName) from file
%%
lgi_lookup_parse_result(
       Number, SubNumber,
       d(GoalFS,GoalS,ConstrS),
       GrammarName ) :-
   lgi_file_name(Number,SubNumber,FileName),
   see(FileName),
   read(FirstItem),
   ( ( FirstItem = grammarname(GrammarName) )
     -> read(SecondItem) 
     ; ( SecondItem = FirstItem ) ),
   seen,
   ( SecondItem = cat(GoalCUFsource) ),
   cuf_extern2intern(GoalCUFsource,GoalS,ConstrS,GoalFS).




%% lgi_init_gen(+Number,+SubNumber,-Goal)
%% lgi_init_gen(+GoalFT,-Goal)
%%
lgi_init_gen(Number,SubNumber,Goal) :-
   ( atomic(Number), atomic(SubNumber) )
   -> ( lgi_lookup_parse_result(Number,SubNumber,GoalFT,_GrammarName),
        lgi_init_gen(GoalFT,Goal) )
   ;  lgi_error( instantiation_error(g) ).


lgi_init_gen(
      d(GoalFS,GoalS,ConstrS),
      c(Guide-[],ToBindHypoS,
        cat(CatType,root(RootType,Root),Leaves,DConstrS,PrintInfo),
        GoalS,ConstrS,_String) ) :-
   ( ( lgi_reset_chart, 
       lgi_init_chart(
          GoalFS,
          Guide,
          arg( CatType, _Dir2,
               root(RootType,Root),
               Leaves, ToBindHypoS, DConstrS, PrintInfo, _HeadProj ) ) )
     -> true ).  % backtracking does not make sense


%% ----------------------------------------------------------
%%   chart inspection
%% ----------------------------------------------------------

%% show_item(ItemId)
%% - does a pp_fs on the information contained in the 
%%   item with ItemId
%%
show_item(ItemId) :-
   lgi_get_flag(chart,ChartValue),
   lgi_get_flag(tcl_output,TclValue),
   ( ( ChartValue = sentence )
     -> ( lgi_visit_item(Guide,ItemId,CatName,CatFT,Tree,_CT,HypoS,String),
          ( ( TclValue = no )
            -> ( lgi_mk_item_fs(
                    Guide,HypoS,ItemId,CatFT,CatName,Tree,String,
                    d(FS,_Gs,ConstrS)),
                 pp_fs(FS,ConstrS) ) % GoalS are currently ignored
            ;  ( ( TclValue = yes ),
                 lgi_tcl_output(CatName,ItemId,CatFT) ) ) )
     ;  ( ( ChartValue = text ),
          visit_text_item(Guide,ItemId,CatName,SemFT,Tree,_CatType),
          ( ( TclValue = no )
            -> ( mk_text_item_fs(Guide,ItemId,SemFT,Tree,d(FS,_Gs,ConstrS)),
                 pp_fs(FS,ConstrS) )  % GoalS are currently ignored
            ;  ( ( TclValue = yes ),
                 lgi_tcl_output(CatName,ItemId,SemFT) ) ) ) ).


   

%% --- END OF FILE: lgg_uif.pl


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Joerg Junger                                  %%%
%%%      Purpose: Prolog-Interface for CUF                      %%%
%%%      Created: Wed May 11 14:25:00 1994                      %%%
%%%     Modified: Thu Nov  2 13:03:19 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.14  1995/12/13 14:27:10  junger
% - made get_it1/2 safe by swapping clauses and adding nonvar(Neg) in
% first clause
%
% Revision 1.13  1995/11/02  12:03:27  jochen
% added cuf_prolog_term/2
%
% Revision 1.12  1995/09/18  11:16:23  jochen
% Various predicates added:
%   cuf_compare_type/3, cuf_disj_types/3, cuf_subsumes/3,
%   cuf_variant/2/3, cuf_entails_constraints/2, cuf_nonvar/1,
%   cuf_corefs/2
%   cuf_is_atomic/1 no longer lets Prolog terms pass through
%
% Revision 1.11  1995/03/21  15:12:32  junger
% - bug in cuf_put_tpath fixed: now CUF unification is used
%
% Revision 1.10  1995/02/10  09:31:23  jochen
% cuf_put_path/3 now also works if all args are instantiated (bug fix)
%
% Revision 1.9  1994/06/30  17:14:27  jochen
%  * improved documentation
%  * cuf_unify, cuf_put* now w/out result parameter
%  * new pred: cuf_copy/2
%  * cuf_is_integer --> cuf_is_number
%  * cuf_put_type works now
%  * conversion typecode -> typename converts strings to char_lists
%
% Revision 1.8  1994/06/23  09:59:36  junger
% - some bugs fixed..
%
% Revision 1.7  1994/06/20  13:09:02  junger
% - cuf_put_tpath, cuf_is_atom, cuf_is_number, cuf_is_string, cuf_is_prolog,
% 	 cuf_is_comple,cuf_goal, cuf_goal_func, cuf_goal_arg, cuf_var added
%
% Revision 1.6  1994/06/14  11:17:42  jochen
% added cuf_subsumes
%
% Revision 1.5  1994/06/14  10:17:35  jochen
% bug fix cuf_eq
%
% Revision 1.4  1994/06/14  09:46:45  junger
% - bug fixed in cuf_eq/2: now variables can be tested on identity
%
% Revision 1.3  1994/06/08  15:20:40  junger
% - get_type_code/2 inserted
%
% Revision 1.2  1994/06/08  13:47:22  jochen
% cuf_atom made more robust
%
% Revision 1.1  1994/05/19  12:13:18  junger
% - initial check in for cuf abstract datatype
%


%  cuf_get_type/2
% cuf_get_type(+FStructure,-Typecode): takes a CUF feature structure and
% returns the typecode.
%
%  cuf_convert_type_name/2
% cuf_convert_type_name(+TypeInt, -TypeExt): takes a type in
% internal format and returns the type in CNF with the type codes replaced
% by the corresponding type name; if TypeInt is a variable [[top]] will be 
% returned.
%
% cuf_convert_type_name(-TypeInt, +TypeExt): takes a type with external
% type names in CNF and delivers the corresponding type in CUF internal
% format; if TypeExt contains only top as type,
% TypeInt will be a variable; in all other cases the type top will be ignored.%
%
%  cuf_put_type/2
% cuf_put_type(+Typecode, ?FStructure): takes a
% typecode and a CUF feature structure and unifies the top node of the
% fstructure with the type code.
%
%  cuf_compare_type/3
% cuf_compare_type(+Typecode1, +Typecode2, -Result)
% Result is   '=' if types are equivalent
%             '<' if second strictly subsumes first
%             '>' if first strictly subsumes second
%             'incomparable' otherwise
%
%
%  cuf_disj_types/3
% cuf_disj_types(+Typecode1, +Typecode2, -TypeCode)
% TypeCode represents disjunction of Typecode1 and 2
% naive version, but optim. for 1 and 2 incomparable
%
%  cuf_unify/2
% cuf_unify(+FStructure1, +FStructure2): unifies two
% CUF feature structures destructively.
%
%  cuf_copy/2
% cuf_copy(+FStructure, -CopyofFStructure):creates a copy of FStructure.
%
%  cuf_atom/2
% cuf_atom(+/-ExtAtom, -/+IntAtom): converts an atom in external CUF
% notation to the corresponding CUF feature structure and vice versa.
% cuf_atom/2 can also be used to test whether a given argument is an
% atom in CUF sense, because it fails when a given argument is not
% atomic. May not be used for atoms of type 'prolog' (see cuf_prolog_term).
%
%  cuf_prolog_term/2
% cuf_prolog_term(?PrologTerm, ?IntAtom): same as cuf_atom, but for atoms 
% of type 'prolog'; IntAtom is unified with the internal feature structure
% corresponding to '! PrologTerm' in external notation. 
%
%
%  cuf_path_value/3
% cuf_path_value(+FStructure, +Path, -Value): takes a feature structure
% and a path in the form of a list of features and delivers the value of
% the path in the feature structure if it exists, otherwise
% cuf_path_value/3 fails.
%
%  cuf_put_tpath/5
% cuf_put_tpath(?FStructure, +Path, +Value, +ConstraintsIn,
% -ConstraintsOut): takes a feature structure, a path, a
% path value in form of a feature structure in CUF internal format and
% constraints; unifies the feature structure with the value Value
% under the path Path. If new features are introduced into the
% structure the type restrictions from the current feature declarations
% are enforced, i.e., domain and range types are added to the structure.
% In case of polymorphic features this can cause the addition of constraints.
%
%  cuf_put_path/3
% cuf_put_path(?FStructureIn, +Path, +Value): takes
% a feature structure, a path and a path value and delivers the feature
% structure unfied with the path with value Value. In contrast to 
% @code{cuf_put_tpath/5} no type information is added when new feature
% paths are introduced, i.e., this may result in non-welltyped structures
% w.r.t. the current feature declarations.
%
%  cuf_eq/2
% cuf_eq(+FStructure1, +FStructure2): takes two CUF feature structures
% and checks whether they are restricted to be the same. For non-atomic
% structures this is the case only if they have been unified.
%
%  cuf_subsumes/2
% cuf_subsumes(+GeneralFStructure, +SpecificFStructure): takes two
% feature structures and checks whether one is more specific than the other 
% on.
%
%  cuf_subsumes/3
% cuf_subsumes(+GeneralFS, +SpecificFSm -WitnessList): 
% as before, but also gives back a list of terms of the form CUF-Var =
% CUF-term, which is a witnessing substitution for the subsumption.
% (should even work properly if CUF-term General contains structure
% sharing between different prolog/1 subterms)
%
%  cuf_variant/2/3
% mutual subsumption
% cuf_variant(+FStructure1,+FStructure2) = 
%     cuf_subsumes(+FStructure1,+FStructure2),
%     cuf_subsumes(+FStructure2,+FStructure1).
% cuf_variant(+FStructure1,+FStructure2,-WitnessList) = 
%     cuf_subsumes(+FStructure1,+FStructure2),
%     cuf_subsumes(+FStructure2,+FStructure1,-WitnessList).
%
%  cuf_entails_constraints/2
% cuf_entails_constraints(+Specific, +General)
% Specific and General are constraint lists (of 'POLY' or 'DIFF'
% constraints) and Specific logically entails General; both lists 
% are required to be checked before.
%
%  cuf_is_atom/1
% cuf_is_atom(+FStructure): takes a feature structure and tests whether
% it is instantiated to a CUF atom. Being of type afs is not
% sufficient. Note that CUF numbers, strings are atoms, but Prolog terms 
% are not (since V2.31).
%
%  cuf_is_number/1
% cuf_is_number(+FStructure): takes a feature structure and tests
% whether it is instantiated to a CUF number.
%
%  cuf_is_string/1
% cuf_is_string(+FStructure): takes a feature structure and tests,
% whether it is instantiated to a CUF string.
%
%  cuf_is_prolog/1
% cuf_is_prolog(+FStructure): tests a feature structure whether it
% is instantiated to a prolog term in the CUF internal format for prolog terms.
%
%  cuf_is_complex/1
% cuf_is_complex(+FStructure): takes a feature structure and tests
% whether it is instantiated to a complex feature structure, i.e, whether
% it has features.
%
%  cuf_goal/4
% cuf_goal(+Goalname, +ArgList, +ResultArg, -Goal): takes a goal name, a
% list of features in CUF internal format and a result argument; the
% list and the result argument will be the arguments of the returned CUF
% goal with name goal.
%
% cuf_goal(-Goalname, -ArgList, -ResultArg, +Goal): takes a CUF goal and
% delivers goal name, list of arguments and result argument.
%
%  cuf_goal/5
% cuf_goal(+Goalname, +ArgList, +ResultArg, ?Arity, -Goal): same as
% cuf_goal/4 except, that additionally the arity can be supplied with; if
% so, it is tested, whether the arity equals the number of the arguments
% of the goal to be build, otherwise the arity is given out.
%
% cuf_goal(-Goalname, -ArgList, -ResultArg, ?Arity, +Goal): takes a
% CUF goal and delivers name, list of arguments and result argument.
%
%  cuf_goal_functor/3
% cuf_goal_functor(+Goal, -GoalName, -Arity): takes a CUF goal and
% delivers its name and its arity.
%
% cuf_goal_functor(-Goal, +GoalName, +Arity): takes a name and an
% arity and delivers a CUF goal with uninstantiated arguments.
%
%  cuf_goal_arg/3
% cuf_goal_arg(+Goal, +ArgNumber, -Argument): takes a CUF goal
% and the number of the argument to be given out.
%
%  cuf_var/1
% cuf_var(+FStructure): tests whether the feature part of a given
% feature structure is variable and the feature structure is not an atom.
% The same as \+ cuf_is_atom(FS), \+ cuf_is_complex(FS).
%
%  cuf_nonvar/1
% cuf_nonvar(+FStructure): negation of cuf_var(FStructure)
%
%  cuf_corefs/2
% cuf_corefs(+FStructure,-ListOfCorefsLists): gives back the list 
% of all nontrivial coreference-classes, where each coref-class is
% a list of paths in the given FStructure that lead to the same
% substructure. Not taken into account are (trivial) coref-classes 
% whose substructure is atomic or which are singletons. If a proper 
% prefix of a path belongs a coref-class, only one of the
% coreferential paths appears in the coref-class of the larger path.
% Example:
% | ?- cuf_extern2intern(hd:(X & f:Y) & cd:[f:Y, X|R] & rest:R, _,_,FS),
%      cuf_corefs(FS,Corefs),writeq(Corefs),fail.
% % [[[cd,'R','R'],[rest]],[[hd,f],[cd,'F',f]],[[hd],[cd,'R','F']]]
% no
% | ?- 
% NOTE: [cd,'R','F',f] = [cd,'F',f] is implicit in the second class
% (due to the third)
%
%
%
% The predicates cuf_put_* together with cuf_atom/2 can be used to
% construct internal feature structures from scratch. Alternatively
% cuf_extern2intern can do this from CUF code (external format).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_get_type(+F-Structure,-Typecode)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_get_type([Type|_], Typecode) :-
	get_type(Type,Typecode,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_convert_type_name(+/-Typecode, -/+Typename)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_convert_type_name(X, TypeName) :-
	var(X),
	var(TypeName),!,
	TypeName = [[top]].
cuf_convert_type_name(Typecode, Typename) :-
	nonvar(Typecode),
	var(Typename),!,
	get_type_name(Typecode, Typename).

cuf_convert_type_name(Typecode1,Typename) :-
	nonvar(Typename),
	var(Typecode1),
	get_type_code(Typename, Typecode),
	( Typecode == [] ->
	      true
	; Typecode1 = Typecode
	).


get_type_code([],[]).
get_type_code([H|T],T2) :-
	get_it_list1(H, H1),
	( H1 == [] ->
	      T2 = T1
	; T2 = [H1|T1]
	),
	get_type_code(T, T1).

get_it_list1([TName],T2) :-
	get_it1(TName, TCode),
       ( var(TCode) ->
	     T2 = []
       ; T2 = [TCode]
       ).
get_it_list1([TName|T],T2) :-
       get_it1(TName, TCode),
       ( var(TCode) ->
	     T2 = T1
       ; T2 = [TCode|T1]
       ),
       get_it_list1(T, T1).

get_it1(~(Neg), TypeCode) :-  !,
	nonvar(Neg),
	get_it1(Neg, Term),
	( number(Term) ->
	      TypeCode is 0-Term
	; TypeCode = -Term
	).
get_it1(N,TID):- 
	number(N),!,
	is_const(N,TID).
get_it1(N, TID):- 
	'STRING_CONST'(N),!,
	( is_const(String,TID) ->    %% may have occurred in type axiom
	   true
	; TID = string(String)
	).
get_it1(N, TypeCode):- 
	atomic(N),
	symbol_table(N,0,TypeCode,_,const),!.
get_it1(TypeName,TypeCode) :-
	symbol_table(TypeName,0,TypeCode,_,type), !.
get_it1(top,_TypeCode).



get_type_name([],[]).
get_type_name([H|T],[H1|T1]) :-
	get_it_list(H, H1),
	get_type_name(T, T1).

get_it_list([TCode],[TName]) :-
	nonvar(TCode),
	get_it(TCode, TName),!.
get_it_list([TCode|T],[TName|T1]) :-
	nonvar(TCode),
	get_it(TCode, TName),
	get_it_list(T, T1).

get_it(number(N), N):- !.
get_it(string(N), Str):- conv_string(N,Str).
get_it(afs_symbol(N), N) :- !.
get_it(-Term, ~(Neg)) :-  !,%% only for -number(N), -string(S) ...
	get_it(Term, Neg).
get_it(TypeCode, Name) :-
	nonvar(TypeCode),
	type_code(TypeCode, TypeName), !,
	conv_string(TypeName, Name).
get_it(TypeCode, ~(Name)) :-
	TypeCode1 is 0-TypeCode,
	type_code(TypeCode1, TypeName), !,
	conv_string(TypeName, Name).
%get_it(_TypeCode, top).

conv_string(Name,Str) :-
	( 'STRING_CONST'(Name),
	  atom_chars(Name,[0'"|R])
	-> append(Str,[0'"],R)
	; Str = Name).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_put_type(+Typecode, +FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_put_type(Typecode, FStructure) :-
	cuf_put_type_(Typecode, FS1),
	reduce_typeinfo(FS1,FS2),
	unify(FS2, FStructure).

cuf_put_type_([], _FStructure).
cuf_put_type_([Conjunct|Rest], FStructure) :-
        cuf_put_type_conj(Conjunct, FStructure),
	cuf_put_type_(Rest, FStructure).

cuf_put_type_conj(Var, _) :- var(Var), !.
cuf_put_type_conj([ConstID], FStructure) :-
	is_const(Const,ConstID), !,
	unify([[[[ConstID]]],Const], FStructure).
cuf_put_type_conj(Conj, FStructure) :-
	unify([[[Conj]|_]|_], FStructure).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_compare_type(+Typecode1, +Typecode2, -Result)
%% Result is   '=' if types are equivalent
%%             '<' if second strictly subsumes first
%%             '>' if first strictly subsumes second
%%             'incomparable' otherwise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_compare_type(Type1, Type2, Result) :-
    type_subsumes(Type1, Type2, R1),
    type_subsumes(Type2, Type1, R2),
    ( R1 =:= 0 ->
	  ( R2 =:= 0 -> Result = incomparable
	  ; Result = (<)
	  )
    ; %% otherwise ->
	  ( R2 =:= 0 -> Result = (>)
	  ; Result = (=)
	  )
    ).
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_disj_types(+Typecode1, +Typecode2, -TypeCode)
%% TypeCode represents disjunction of Typecode1 and 2
%% naive version, but optim. for 1 and 2 incomparable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_disj_types([SingleConjunct], Type2, Result) :- !,
    ( Type2 = [AnotherSingleC] ->
	  append(SingleConjunct,AnotherSingleC,Disjtn0),
	  sort(Disjtn0,Disjtn),
	  Result = [Disjtn]
    ; %% otherwise: append SingleConjunct to all in Type2
          cuf_disj_types1(Type2,SingleConjunct,Result)
    ).
cuf_disj_types(Type1, [SingleConjunct], Result) :- !,
    cuf_disj_types([SingleConjunct], Type1, Result).
cuf_disj_types(Type1, Type2, Result) :-
    cuf_disj_types2(Type1, Type2, Result).


cuf_disj_types1([], _, []).
cuf_disj_types1([Conj|Rest], Single, Result) :-
    type_subsumes([Conj], [Single], 1), !,
    Result = [Conj|ResRest],
    cuf_disj_types1(Rest, Single, ResRest).
cuf_disj_types1([Conj|Rest], Single, Result) :-
    append(Single, Conj, Disjtn0),
    sort(Disjtn0, Disjtn),
    Result = [Disjtn|ResRest],
    cuf_disj_types1(Rest, Single, ResRest).


cuf_disj_types2([], _, []).
cuf_disj_types2([Conj|Rest], Type2, Result) :-
    type_subsumes([Conj], Type2, 1), !,
    Result = [Conj|ResRest],
    cuf_disj_types2(Rest, Type2, ResRest).
cuf_disj_types2([Conj|Rest], Type2, Result) :-
    cuf_disj_types1(Type2, Conj, Type2First),
    cuf_disj_types2(Rest, Type2, Type2Rest),
    ( Type2Rest == [] ->
	  Result = Type2First
    ; type_subsumes(Type2First, Type2Rest, 1) ->
	  Result = Type2Rest
    ; type_subsumes(Type2Rest, Type2First, 1) ->
	  Result = Type2First
    ; append(Type2First,Type2Rest,Result)
    ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_unify(+FStructure1, +FStructure2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_unify(FS1, FS2) :-
	unify(FS1, FS2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_atom(?ExtAtom, ?IntAtom)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_atom(ExtAtom, IntAtom) :-
	( (IntAtom = [T,FS], nonvar(FS),nonvar(T)) ->
	       \+ functor(FS,':',2),
	       \+ functor(FS,prolog,1),
	       ExtAtom = FS
	; (atomic(ExtAtom);is_string(ExtAtom)) ->
	       prep_ex(ExtAtom, ExtAtom1),
	       ex2in(ExtAtom1, [T,FS], [], [], _, []),
	       unify(IntAtom,[T,FS])
	).

%% Ex: cuf_atom(atom,<Struc>) --> 'CUF'-unifies <Struc> with atom
%%     cuf_atom(X,<Struc>) --> succeeds if <Struc> is an atomic type
%%                             and returns its external form in X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_prolog_term(?PrologTerm, ?IntAtom)
%% same as cuf_atom, but for atoms of type prolog
%% PrologTerm may be an arbitrary Prolog term; the internal 
%% form is the same as would be produced by the CUF input
%% '! PrologTerm'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_prolog_term(PrologTerm, IntAtom) :-
	ex2in('!'(PrologTerm),IntAtom,[],[],_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_copy(+FStructure, -CopyofFStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_copy(FStructure, CopyofFStructure) :-
	copy_term(FStructure, CopyofFStructure).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_path_value(+FStructure, +Path, -Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_path_value(FS,Path,FS) :- 
	nonvar(Path), 
	Path == [],!.
cuf_path_value(FS,[Attr|Rest],Value) :-
	nonvar(FS),
	nonvar(Attr),
	FS = [_|FS1],
	memb_nonvar(Attr:FSout, FS1),
	cuf_path_value(FSout, Rest, Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_put_tpath(+FStructure, +Path, +Value, +ConstraintsIn
%%               -ConstraintsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_put_tpath(+FStructure, +Path, +Value, +ConstraintsIn
%%               -ConstraintsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_put_tpath(FStructure, Path, Value, ConsIn, ConsOut) :-
	convert_the_path(Path, FStructure, Value, ConsIn, ConsOut).

convert_the_path([], FS, Value, Cons, Cons) :-
        unify(FS, Value).
convert_the_path([Feat|Rest], FS, Value, C_In, C_Out) :-
	nonvar(Feat),
        ( nonvar(FS), FS=[Dom|Feats], memb_nonvar(Feat:Val1,Feats) ->
              %% feature already there; no type checking
              convert_the_path(Rest, Val1, Value, C_In, C_Out)
	; ( is_feat(Feat) -> true
	  ; add_undef_symbol(Feat, 1, feat, tmp, default),
	    used_occ_table(Feat, 1, feat, tmp, default)
	  ),
	  ( is_monofeature(Feat, Dom, Ran) -> 
		(var(Dom) -> true ; Decl = [Dom|_]),
		(var(Ran) -> true ; FS2 = [[Ran|_]|_]),
		unify([Decl,Feat:FS2|_], FS),
		C1 = C_In
	  ; clause(feat_doms(Feat, Doms), true),
	    Dom = [Doms|_],
	    FS2 = [Ran|_],
	    unify([Dom,Feat:FS2|_], FS), 
	    clause(poly_feat_check(Feat, Dom, Ran, Cons), true),
	    append(C_In, Cons, C1)
	  ),
	  convert_the_path(Rest, FS2, Value, C1, C_Out)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_put_path(+FStructure, +Path, +Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_put_path(FS, [], FS1) :- !,
        unify(FS,FS1).
cuf_put_path(FS, [Attr|Rest], Value) :-
	unify([_,Attr:FS1|_], FS),
	cuf_put_path(FS1, Rest, Value).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_eq(+FStructure1, +FStructure2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_eq(FS1, FS2) :-
	\+ \+ type_eq(FS1, FS2).

type_eq([T1|FS1], [T2|FS2]) :-
	get_type(T1, _Type1, TI1),
	get_type(T2, _Type2, TI2),
	TI1 == TI2,
	(nonvar(TI1) ->
	     FS1 == FS2
	; true
	).
	       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_subsumes(+GeneralFStructure, +SpecificFStructure)
%%  cuf_subsumes(+GeneralFS, +SpecificFS, -WitnessList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_subsumes(General, Specific) :-
	\+ \+ ( numbervars(Specific,0,_),
                unify(General, Specific) ).

%% cuf_subsumes/3 gives back a list of terms of the form CUF-Var =
%% CUF-term, which is a witnessing substitution for the subsumption.
%% (should even work properly if CUF-term General contains structure
%% sharing between different prolog/1 subterms)

cuf_subsumes(General, Specific, WitList) :-
	copy_term(General,Copy),
	cuf_collect_subsumers(General,Copy,Specific,ProlGen,ProlSpec,WitList,[]),
	subsumes_chk(ProlGen,ProlSpec).

cuf_collect_subsumers(Gen, Copy, Spec, _, _) -->
	{var(Gen), !},
	({nonvar(Copy),Copy='$DONE'(OldSpec)} ->
	     {cuf_eq(OldSpec,Spec)}
	; {Copy = '$DONE'(Spec)},
	  ({Gen==Spec} -> []
	  ; [Gen=Spec])
	).
cuf_collect_subsumers([T|Feats], [TCopy|FCopy], Spec, ProlGen, ProlSpec) -->
	{nonvar(Spec),
	 Spec=[T1|Feats1],
	 get_type(T,Type,TVar), 
	 get_type(T1,Type1,TVar1)},
	({TVar == []} ->
	      {Type1==Type,
	       TVar1==[]},
	      ({Feats=[prolog(P)]} ->
		   {Feats1=[prolog(P1)],
		    ProlGen = [P],
		    ProlSpec = [P1]},
	           ({ground(P)} -> []
		   ; [[T|Feats]=Spec]
		   )
	      ; {Feats==Feats1}
	      )
	; {get_current_type(TCopy,_,TVarCopy)},
	  ( {nonvar(TVarCopy), TVarCopy='$DONE'(OldSpecTVar)} ->
		{OldSpecTVar==TVar1}
	  ; {TVarCopy = '$DONE'(TVar1)},
	    ( {TVar==TVar1} -> []
	    ; {type_subsumes(Type,Type1,1)},
	      [[T|Feats]=Spec],
	      cuf_collect_subsumers_fs(Feats, FCopy, Feats1, ProlGen, ProlSpec)
	    )
	  )
	).

cuf_collect_subsumers_fs([Feat:Val|Feats],[_:VCopy|FCopy],Feats1,ProlGen,ProlSpec) -->
	{nonvar(Feat), !,
	 memb_nonvar(Feat:Val1,Feats1)},
	cuf_collect_subsumers(Val,VCopy,Val1,PG,PS),
	{(var(PG) ->
	      ProlGen = PGRest, ProlSpec = PSRest
	; ProlGen = [PG|PGRest], ProlSpec = [PS|PSRest])},
	cuf_collect_subsumers_fs(Feats,FCopy,Feats1,PGRest,PSRest).
cuf_collect_subsumers_fs(_,_,_,_,_) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_variant(+FStructure1,+FStructure2)
%% cuf_variant(+FStructure1,+FStructure2,-WitnessList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% true if FS1 and FS2 are subsumption-equivalent, WitnessList as
%% above

cuf_variant(FS1, FS2) :-
	cuf_subsumes(FS2, FS1),
	cuf_subsumes(FS1, FS2).

cuf_variant(FS1, FS2, WitList) :-
	cuf_subsumes(FS2, FS1),
	cuf_subsumes(FS1, FS2, WitList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_entails_constraints(+Specific, +General)
%% Specific and General are constraint lists (of 'POLY' or 'DIFF'
%% constraints) and Specific logically entails General; both lists 
%% are required to be checked before.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_entails_constraints(Specific, General) :-
	\+ ( member(Constr, General),
	     violate_constr(Constr),
	     check_constraints(Specific,_)
	   ).

violate_constr('DIFF'(T1,T2)) :-
	unify(T1,T2).
violate_constr('POLY'(ID,TI1,TI2)) :-
	'POLY'(ID, DomType, _RanType, _NegDomType, NegRanType),
	get_type(TI1,Type1,TV1),
	get_type(TI2,Type2,TV2),
	gcs(Type1,DomType,TV1,_),
	gcs(Type2,NegRanType,TV2,_).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_is_atom(+FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_is_atom(IntAtom) :-
	IntAtom = [T,FS], 
	nonvar(FS),
	nonvar(T),
	\+ functor(FS,':',2),
	\+ functor(FS,prolog,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_is_number(+FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_is_number(IntAtom) :-
	IntAtom = [T,FS], 
	nonvar(FS),
	nonvar(T),
	get_type(T, Type, TRest),
	TRest == [],
	symbol_table(number,0,TCode,_,type),
	type_subsumes([[TCode]],Type,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_is_string(+FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_is_string(IntAtom) :-
	IntAtom = [T,FS],
	nonvar(FS),
	nonvar(T),
	get_type(T, Type, TRest),
	TRest == [],
	symbol_table(string,0,TCode,_,type),
	type_subsumes([[TCode]],Type,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_is_prolog(+FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_is_prolog(IntAtom) :-
	IntAtom = [_,FS],
	nonvar(FS),
	FS = prolog(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_is_complex(+FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_is_complex(FS) :-
	FS = [T,FS1|_],
	nonvar(T),
	nonvar(FS1),
	functor(FS1, ':', 2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_goal(+Goalname, +ArgList, +ResultArg, -Goal)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_goal(Goalname, ArgList, ResultArg,Goal) :-
	wrap_goal(OGoal, Goal),
	OGoal =.. [Goalname|[ResultArg|ArgList]].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_goal(+Goalname, +ArgList, +ResultArg, +Arity, -Goal)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_goal(Goalname, ArgList, ResultArg, Arity, Goal) :-
	wrap_goal(OGoal, Goal),
	OGoal =.. [Goalname|[ResultArg|ArgList]],
	length(ArgList,Len),
	Arity is Len + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_goal_functor(+WrappedGoal, -Functor, - Arity)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_goal_functor(WGoal, Func, Arity) :-
	wrap_goal(OGoal, WGoal),
	functor(OGoal, Func, Arity).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_goal_arg(+WrappedGoal, +ArgNumber, -Argument)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cuf_goal_arg(WGoal, ArgNo, Arg) :-
	wrap_goal(OGoal, WGoal),
	arg(ArgNo, OGoal, Arg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_var(FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_var(Term) :-
   \+ \+ ( Term = [_Type,Structure],
           var(Structure) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_nonvar(FStructure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_nonvar(Term) :-
   \+ ( Term = [_Type,Structure],
        var(Structure) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_corefs(FStructure,ListOfCorefsLists)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_corefs(FS0,Corefs) :-
	copy_term(FS0,FS),
        cuf_corefs_w_paths(FS,[],RefsPaths,[]),
	keysort(RefsPaths,RefsPathsSorted),
	cuf_collect_corefs(RefsPathsSorted,Corefs).

cuf_corefs_w_paths([T|_FS],PathSoFar) -->
	{nonvar(T), functor(T,'$DONE',1), !},
	[T-PathSoFar].
cuf_corefs_w_paths([T|FS],PathSoFar) -->
	{ get_current_type(T,_,TVar), TVar\==[], !},
	( {var(TVar)} ->
	      {TVar='$DONE'(PathSoFar)},
	      cuf_corefs_w_paths_fs(FS,PathSoFar)
	; [TVar-PathSoFar]   %% TVar contains primary path; record other
	                     %% no recursion here; FS already processed
	).
cuf_corefs_w_paths(_,_) --> [].

cuf_corefs_w_paths_fs([Feat:Val|Rest],PathSoFar) -->
	{ nonvar(Feat), !},
	cuf_corefs_w_paths(Val,[Feat|PathSoFar]),
	cuf_corefs_w_paths_fs(Rest,PathSoFar).
cuf_corefs_w_paths_fs(_,_) --> [].


%% filter out singleton paths
cuf_collect_corefs([],[]).
cuf_collect_corefs([Prim-Sec|R],[[RP1,RP2|RPaths]|Out]) :-
	Prim = '$DONE'(P1),
	rev(P1,RP1),	
	rev(Sec,RP2),
	cuf_collect_corefs(R,Prim,RR,RPaths,[]),
	cuf_collect_corefs(RR,Out).

cuf_collect_corefs([Prim-Sec|R],Prim0,RR) -->
	{ Prim==Prim0, !,
	  rev(Sec,RP)},
	[RP],
	cuf_collect_corefs(R,Prim0,RR).
cuf_collect_corefs(R,_,R) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: interface predicates                          %%%
%%%      Created: Wed Dec  2 15:57:06 1992                      %%%
%%%     Modified: Wed Jun 29 10:39:31 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.7  1994/06/30 17:52:00  jochen
% the cuf_debugging/1 fact is now handled in init_cuf_debugging (the flag action
% for debug, tracing and interactive flags).
%
% Revision 1.6  1994/05/18  15:33:25  junger
% - description for selected(GID) mode added
%
% Revision 1.5  1994/05/10  14:05:49  junger
% - cuf_prove1: new mode selected(GoalID) added; performs one resolve-step with
% 	goal with ID GoalID
%
% Revision 1.4  1994/05/10  13:51:59  junger
% - cuf_prove1: new mode selected(GoalID) added; performs one resolve-step with
% 	goal with ID GoalID
%
% Revision 1.3  1994/02/21  15:21:58  jochen
%  - fact cuf_debugging(X) has X=1 if a debugging-relevant flag is on
%    which is tested before cuf_prove/5/6
%
% Revision 1.2  1994/02/03  10:39:44  junger
%  - Adaptions to new goal format
%  - New predicate cuf_prove/7, as cuf_prove/6, but with depth-bound
%    argument
%
% Revision 1.1  1993/11/08  13:50:09  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Mon Sep 13 13:02:15 1993 (michel) : check_POLY_sat/1 in det_only added
% Fri Jan  8 17:29:16 1993 (michel) : force_unfold case changed
% Wed Dec 16 10:53:03 1992 (michel) : cuf_prove/4 --> cuf_prove/6
% Tue Dec 15 18:08:19 1992 (michel) : cuf_prove/3 --> cuf_prove/4

%%% cuf_prove(+Mode, +InGoals, -OutGoals, +ConsIn, -ConsOut, -RuleNoList)
%%%
%%% args: InGoals     list of CUF-goals (internal format) to prove
%%%       OutGoals    list of residual (suspended) goals
%%%       ConsIn      list of constraints to check
%%%       ConsOut     list of unrefuted constraints (all satisfiable)
%%%       RuleNoList  list of numbers of clauses of selected
%%%                   non-deterministic goals in propagation order
%%%       Mode        one of 'det_only', 'force_unfold',
%%%                   'undelayed_only', and 'all'; determines what
%%%                   residual goals are  
%%% cuf_prove tries to find solution instantiations for variables in
%%% InGoals s.t. the instantiated InGoals follows from the current
%%% CUF-rulebase and OutGoals. Thus, variables in InGoals may get
%%% instantiated to internal CUF-structures, representing solutions.
%%% What kind of goals are passed through to the OutGoals list instead
%%% of being proved is determined by Mode as follows:
%%% Mode = det_only   only deterministic (no choice point introducing)
%%%                   goals are proved (possibly none)
%%% Mode = force_unfold
%%%                   as before, but do at least one reduction step
%%% Mode = undelayed_only
%%%                   reduce as long as there are undelayed goals
%%% Mode = all        no outgoals allowed (OutGoals=[])
%%% Mode = selected(GoalID) the goal with ID GoalID will be resolved;
%%%                         requiers debug-mode
%%% The first mode runs deterministically (no backtracking necessary),
%%% the others need to be backtracked to get all `solutions'. 
%%%

cuf_prove(Mode, InGoals, OutGoals, ConsIn, ConsOut, RuleNoList) :-
        get_flag(depth_bound, Max),
	cuf_prove(Mode, InGoals, Max, OutGoals, ConsIn, ConsOut,
		  RuleNoList).



%%% cuf_prove(+Mode, +InG, +MaxDepth, -OutG, +ConsIn, -ConsOut, -RuleNos)
%%%
%%% as before, but with additional argument MaxDepth restricting the
%%% maximal depth of the proof
%%%

:- dynamic cuf_debugging/1.

init_cuf_debugging :-
	retractall(cuf_debugging(_)),
	( \+ ( get_flag(debug,no),
	       get_flag(tracing,no),
	       get_flag(interactive,no)
	     ) ->
             assert(cuf_debugging(1))
	; assert(cuf_debugging(0))
	).

cuf_prove(Mode, InGoals, Max, OutGoals, ConsIn, ConsOut, RuleNoList) :-
	integer(Max),
	Max > 0,
	init_cuf_debugging,
	cuf_prove1(Mode, InGoals, Max, OutGoals, ConsIn, ConsOut,
		   RuleNoList).

cuf_prove1(selected(ParID:NthSubgoal), InGoals, _Max, OutGoals, ConsIn, 
	   ConsOut, [RuleNo]) :-
	delete(g(ParID,NthSubgoal,oGoal,Flag), InGoals, RestGoals),
	resolve(g(ParID,NthSubgoal,oGoal,Flag), OutGoals, RestGoals,
		ConsIn, ConsNew, 0, 1, _, RuleNo, non_det),
	check_constraints(ConsNew, ConsOut),
	check_POLY_sat(ConsOut).


cuf_prove1(det_only, InGoals, Max, OutGoals, ConsIn, ConsOut, []) :-
 	expand_det(InGoals, OutGoals, ConsIn, ConsNew, 0, Max, _), !,
	check_constraints(ConsNew, ConsOut),
	check_POLY_sat(ConsOut).
cuf_prove1(force_unfold, InGoals, Max, OutGoals, ConsIn, ConsOut, RuleNoList) :-
	cuf_prove1(det_only, InGoals, Max, OutGoals0, ConsIn, ConsOut0, []),
	( goaleq(InGoals,OutGoals0) ->
	      resolve(InGoals, OutGoals1, ConsIn, ConsOut1, 0, 1, _, RuleNo1),
	      ( RuleNo1 == 'NO REDUCE' ->
		    cuf_out(message, ['forcing unfold ...']),
		    
		    InGoals = [Goal|RestGoals],
		    resolve(Goal, OutGoals, RestGoals, ConsIn, ConsOut,
			    0, 1, _, RuleNo2, non_det),
		    RuleNoList = [RuleNo2]
	      ; RuleNoList = [RuleNo1],
		OutGoals = OutGoals1,
		ConsOut = ConsOut1
	      )
	; OutGoals = OutGoals0,
	  RuleNoList = [],
	  ConsOut = ConsOut0
	).
cuf_prove1(undelayed_only, InGoals, Max, OutGoals, ConsIn, ConsOut, RuleNoList) :-
	prove(InGoals, OutGoals, ConsIn, ConsOut, 0, Max, _,
	      RuleNoList, undelayed_only).
cuf_prove1(all, InGoals, Max, OutGoals, ConsIn, ConsOut, RuleNoList) :-
	prove(InGoals, OutGoals, ConsIn, ConsOut, 0, Max, _, RuleNoList, all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: lookup tables for CUF compiler                %%%
%%%      Created: Fri Sep 18 13:21:57 1992                      %%%
%%%     Modified: Fri Mar 15 21:42:58 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.9  1996/04/09 14:03:28  jochen
% file_table/5 now works with new file IDs (atoms 'CUF file1'...),
% which at the same time are module names for certain dynamic facts;
%
% Revision 1.8  1994/06/30 17:39:33  jochen
%  * stream-lined init_defaults: now info about built-in type hierarchy is
%    concentrated in defaults.pl
%  * basic_type/2 (new) also provides fixed code for built-in
%
% Revision 1.7  1994/05/24  11:42:39  michel
% new predicate symbol_table_/4
% symbol_table/7: (bug fix)
% * assert case: if symbol was classified, it will be retracted first now
%
% Revision 1.6  1994/05/17  10:39:55  michel
% built_in_symbol/3:
% * sort case added
% new predicate is_basic_sort/2
% init_defaults:
% * new case for built-in sorts
%
% Revision 1.5  1994/01/18  12:07:51  jochen
% added handling of implicit typed numbers and strings in const_tbl/3 (doing
% (un)classify_atom for them).
% is_const/2 now also recognizes these
% using negate_TID instead of Neg is 0-TID everywhere
%
% Revision 1.4  1994/01/14  09:27:11  jochen
% 1) Use new newID/recycleID predicates for old counters typeID, fileID.
%
% 2) Handling of undeclared constants (internal forms: afs_symbol(S),
%   string(S), number(S)) added. These constants occur in symbol_table
%   but not in type_code table.
%   Changed predicates: symbol_table/7 (assert case), const_tbl/3
%   (retract case), is_const/2 (new case added for undeclared), and
%   is_symbol_id/1 (dito).
%   New predicate: is_undeclared_const(TID, Name) for the forms above.
%
% 3) New clause in init_defaults/0 calls init_sat/1 declaring atom domains
%   string, number, and afs_symbol. (These types receive the IDs 3, 5, and 7).
%
% Revision 1.3  1993/12/23  13:47:57  michel
% file_table/5: new options: current_ids, current_files, current_files_ids
%
% Revision 1.2  1993/12/13  13:20:40  michel
% file_table/3 is dynamic now
%
% Revision 1.1  1993/11/08  13:49:53  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Sun Aug  1 18:46:35 1993 (dorna) : table formats changed globally
% Thu Dec  3 11:16:28 1992 (michel) : error handling changed
% Thu Oct 29 10:38:45 1992 (michel) : add_symbol1/3 changed, add_symbol_* added
% Wed Oct 28 14:08:09 1992 (michel) : occ_table changed for constants

/*
%%% table(+Action, +Name, +ArgumentList)
table(list, Name, Args) :- !,
	atomic(Name),
	( integer(Args) -> listing(Name/Args)
	; listing(Name)
	).
table(Action, Name, Args) :-
	atomic(Name),
	prolog_list(Args),
	TableEntry =.. [Name|Args],
	table(Action, TableEntry).

table(assert, TableEntry) :-
	      assert(TableEntry).
table(retract, TableEntry) :-
	      retract((TableEntry :- _)).
table(look, TableEntry) :-
	      clause(TableEntry, true).
*/

:- dynamic feat_decl/5.

feat_decl_table(assert, Feat, Dom, Ran, FID, EID, _Flag) :-
	atomic(Feat),
	( feat_decl_table(check, Feat, OldDom, OldRan, FID, EID, unknown) ->
	      assert(feat_decl(Feat, Dom, Ran, FID, EID))
	; /* Flag == known */
	  ( \+ ( OldDom == Dom, OldRan == Ran) ->
		assert(feat_decl(Feat, Dom, Ran, FID, EID))
	  ; true
	  )
	).
feat_decl_table(check, Feat, Dom, Ran, FID, EID, Flag) :-
	atomic(Feat),
	( feat_decl(Feat, Dom, Ran, FID, EID) ->
	  Flag = known
	; Flag = unknown
	).
feat_decl_table(retract, Feat, Dom, Ran, FID, EID, _Flag) :-
	atomic(Feat),
	retract(feat_decl(Feat, Dom, Ran, FID, EID)).

:- dynamic sort_decl/5.

sort_decl_table(assert, Sort, Arity, TypeList, FID, EID, Flag) :-
	atomic(Sort), integer(Arity),
	sort_decl_table(check, Sort, Arity, OldTypeList, _, _, CFlag) ->
	( CFlag == known -> 
	      ( OldTypeList = TypeList -> Flag = ok
	      ; cuf_out(error,[ 'multiple sort declaration:',Sort/Arity]),
		cuf_out(entry, [FID, EID]),
		!, fail
	      )
	; /* CFlag == unknown -> */
	  assert(sort_decl(Sort, Arity, TypeList, FID, EID)),
	  Flag = ok
	).
sort_decl_table(check, Sort, Arity, TypeList, FID, EID, Flag) :-
	atomic(Sort), integer(Arity),
	( sort_decl(Sort, Arity, TypeList, FID, EID) ->
	      Flag = known
	; Flag = unknown
	).

:- dynamic file_table/3.

%%% file_table(+Action, ?FileName, ?Date, ?FileNo, ?Flag)
file_table(check, FileName, Date, FileID, Flag) :- 
    ( nonvar(FileName); atom(FileID) ),
    ( file_table(FileName, OldDate, FileID) -> 
	  RFlag = old 
    ; RFlag = new 
    ),
    ( RFlag == new -> Flag = new
    ; /* RFlag == old, */
      var(Date) -> Date = OldDate, Flag = old 
    ; /* RFlag == old, nonvar(Date), */
      OldDate == Date -> Flag = old 
    ; Flag = new
    ).
file_table(assert, FileName, Date, FileID, Flag) :- 
    nonvar(FileName),
    ( nonvar(Date) -> true
    ; file_property(FileName, modify_time, Date)
    ),
    ( Flag == new, var(FileID) ->
	  newID(fileID, FileNo, 1), 
	  concat_atom(['CUF file',FileNo],FileID),
	  assert(file_table(FileName, Date, FileID))
    ; Flag == old, nonvar(FileID) ->
	  assert(file_table(FileName, Date, FileID))
    ; /* var(Flag), */
      file_table(check, FileName, OldDate, CFileID, CFlag),
      ( CFlag == old ->
	( OldDate == Date -> Flag = old  	 
	; /* OldDate /== Date, */
	  file_table(retract, FileName, OldDate, CFileID, old),
	  file_table(assert, FileName, Date, FileID, new),
	  Flag = new
	) 	 
      ; /* CFlag == new, */
        file_table(assert, FileName, Date, FileID, new),
	Flag = new
      )
    ).
file_table(retract, FileName, Date, FileID, Flag) :- 
    ( nonvar(FileName) 
    ; atom(FileID) ),
    ( retract(file_table(FileName, Date, FileID)) -> 
	  Flag = old, 
	  atom_chars(FileID,Chars),
	  append("CUF file",Rest,Chars),
	  number_chars(FileNo,Rest),
	  recycleID(fileID, FileNo)
    ; Flag = new ).
file_table(current_ids, _, _, FIDs, old) :-
    findall(FID,file_table(_F,_Date,FID),FIDs).
file_table(current_files, FileNames, _, _, old) :-
    findall(FileName,file_table(FileName,_Date,_FID),FileNames).
file_table(current_files_ids, FileNames, _, FIDs, old) :-
    findall(FID,file_table(_F,_Date,FID),FIDs),
    findall(FileName,file_table(FileName,_Date,_FID),FileNames).

:- dynamic symbol_table/5.

%%%  symbol_table(+Action, ?Symbol, ?Arity, ?TypeID, ?Mark, ?Type, -Result)
%  possible actions are:
%  check: table lookup
%         a) Symbol and Type given: Result is either the Type or
%            []
%         b) Symbol given, Type variable: Result will be a list of
%            Symbol/Arity-Type pairs
%  retraact: entry deletion
%          Symbol and Type given: Result is either 'ok' or 
%          'unknown'
%  assert: entry addition
%         Symbol, Arity and Type given: Result is either 'ok' or
%         'known'
symbol_table(check, Sym, Ar, TID, Mark, Type, Result) :-
	( atomic(Sym), 
	  integer(Ar) ->
	      ( symbol_table(Sym, Ar, TID, Mark, Type) -> 
		    Result = ok
	      ; Result = [] 
	      )
	; integer(TID) ->
	      type_code(TID, Sym),
	      ( symbol_table(Sym, Ar, TID, Mark, Type) ->
		    Result = ok
	      ; Result = []
	      )
	; Result = []
	), !.
/*
symbol_table(test, Sym, Ar, TID, Ma, Type, Result) :-
   ( atomic(Sym), 
     integer(Ar) ->
	( var(Type) ->
	      ( setof([Ty,Ma],
		      TID^symbol_table(Sym, Ar, TID, Ma, Ty),
		      Result)
	      ; Result = [] )
	; symbol_table(Sym, Ar, TID, Ma, Type) ->
	      Result = ok
	; Result = [] 
	)
   ; integer(TID) ->
	 type_code(TID, Sym),
	 ( symbol_table(Sym, Ar, TID, Ma, Type) ->
	       Result = ok
	 ; Result = []
	 )
   ; Result = []
   ), !.
*/
symbol_table(retract, Symbol, Arity, TID, Mark, Type, Result) :-
    atomic(Symbol), integer(Arity),
    is_symbol_id(TID), nonvar(Type),
    Mark \== (+),
    ( retract(symbol_table(Symbol, Arity, TID, Mark, Type)) ->
      Result = ok
    ; Result = unknown
    ), !.
symbol_table(assert, Symbol, Arity, TID, Mark, Type, Result) :- 
	atomic(Symbol), integer(Arity), nonvar(Type),
	( atomic(Mark) ; Mark = (*) ),
	symbol_table(check, Symbol, Arity, TID, MarkOld, Type, Res),
	( Res == [] ->
	      symbol_table_(Type, Symbol, Mark, TID),
	      assert(symbol_table(Symbol, Arity, TID, Mark, Type)),
	      Result = ok
	; Mark == (*), MarkOld == (-) ->
	      retract(symbol_table(Symbol, Arity, _, MarkOld, Type)),
	      symbol_table_(Type, Symbol, Mark, TID),
	      assert(symbol_table(Symbol, Arity, TID, Mark, Type)),
	      Result = ok
	; Res == ok -> Result = known 
	), !.
symbol_table(look, Symbol, Arity, _, _, _, Result) :-
    ( setof(def(KindOf,Mark),
	    TID^symbol_table(Symbol, Arity, TID, Mark, KindOf),
	    Result) -> true
    ; Result = [] ).
 
symbol_table_(KindOf, Symbol, Mark, TID) :-
    ( KindOf == const, TID \== -1  ->
	  ( Mark \== (-) ->
		const_tbl(assert, Symbol, TID)
	  ; TID = afs_symbol(Symbol)
	  )
    ; KindOf == type, TID \== 1  ->
	  type_tbl(assert, Symbol, TID)
    ; atomic(KindOf), ( atomic(TID) ; TID = (*) )
    ).

:- dynamic type_code/2.

%%%  type_tbl(+Action, ?Symbol, ?TID)
type_tbl(Action, Symbol, TID) :-
	( Action == assert ->
	      atomic(Symbol), var(TID),
	      newID(typeID, TID, 2),
	      assert(type_code(TID, Symbol))
	; Action == look ->
	      integer(TID),
	      type_code(TID, Symbol)
	; Action == retract ->
	      atomic(Symbol), integer(TID),
	      retract(type_code(TID, Symbol)),
	      symbol_table(cfs, 0, CFS, +, type),
	      ( TID \== CFS ->
		    recycleID(typeID, TID)
	      ; true )
	), !.

%%%  const_tbl(+Action, ?Symbol, ?TID)
const_tbl(Action, Symbol, TID) :-
	( Action == assert ->
	      atomic(Symbol), var(TID),
	      newID(constID, TID, 2),
	      assert(type_code(TID, Symbol)),
	      (number(Symbol) ->
		   symbol_table(number, 0, NType, +, type),
		   classify_atom(TID,NType)
	      ; 'STRING_CONST'(Symbol) ->
		   symbol_table(string, 0, SType, +, type),
		   classify_atom(TID,SType)
	      ; true
	      )
	; Action == look ->
	      integer(TID),
	      type_code(TID, Symbol)
	; Action == retract ->
	      atomic(Symbol),
	      ( integer(TID) ->
		    retract(type_code(TID, Symbol)),
		    symbol_table(afs, 0, AFS, +, type),
		    ( TID \== AFS ->
			  recycleID(constID, TID)
		    ; true
		    ),
		    ( number(Symbol) ->
			  unclassify_atom(TID)
		    ; 'STRING_CONST'(Symbol) ->
			  unclassify_atom(TID)
		    ; true
		    )	      
	      ; true )
	), !. 

%%%
built_in_symbol(const, Name, _) :-
   is_basic_const(Name, _).
built_in_symbol(feat, Name, _) :-
   is_basic_feat(Name).
built_in_symbol(type, Name, _) :-
   is_basic_type(Name, _).
built_in_symbol(sort, Name, Arity) :-
   is_basic_sort(Name, Arity).

is_basic_feat(Name) :-
	atomic(Name),
	symbol_table(Name, _, *, +, feat).
is_basic_type(Name, TID) :-
	atomic(Name),
	symbol_table(Name, _, TID, +, type).
is_basic_const(Name, TID) :-
	atomic(Name),
	symbol_table(Name, 0, TID, +, const).
is_basic_sort(Name, Arity) :-
	atomic(Name),
	integer(Arity),
	symbol_table(Name, Arity, *, +, sort).

is_defined_feat(Name) :-
	atomic(Name),
	symbol_table(Name, _, *, *, feat).
is_defined_sort(Name, Arity) :-
	atomic(Name),
	integer(Arity),
	symbol_table(Name, Arity, *, *, sort).
is_defined_const(Name, TID) :-
	atomic(Name),
	symbol_table(Name, 0, TID, *, const),
	integer(TID).
is_defined_type(Name, TID) :-
	atomic(Name),
	symbol_table(Name, 0, TID, *, type),
	integer(TID).

is_const(Name, TID) :-
	( atomic(Name) ->
	  ( symbol_table(Name, 0, TID, _, const) -> true
	  ; number(Name) -> TID = number(Name)
	  ; 'STRING_CONST'(Name) -> TID = string(Name)
	  )
	; integer(TID) ->
	  type_code(TID, Name),
	  symbol_table(Name, 0, TID, _, const)
	; nonvar(TID), is_undeclared_const(TID, Name)
	).

is_undeclared_const( afs_symbol(N), N ).  % for undeclared constants
is_undeclared_const( string(N), N ).
is_undeclared_const( number(N), N ).


is_type(Name, TID) :-
	( atomic(Name) ->
	  symbol_table(Name, 0, TID, _, type)
	; integer(TID) ->
	      type_code(TID, Name),
	      symbol_table(Name, 0, TID, _, type)
	).
is_sort(Name, Arity) :-
	atomic(Name),
	integer(Arity),
	symbol_table(Name, Arity, _, _, sort).
is_feat(Name) :-
	atomic(Name),
	symbol_table(Name, _, _, _, feat).

is_symbol_id(ID) :-
	( integer(ID), !             % for constants and types
	; ID == (*), !                 % for features and sorts
	; is_undeclared_const(ID, _)
	).

is_symbol_mark(M) :-
	( M == (*)                  % defined
	; M == (-)                  % generated / not defined
	; M == (+) ).               % basic


%%%  basic types
basic_type(Name/0) :-
	current_predicate(basic_mono_type, basic_mono_type(_)),
	basic_mono_type(Name).

basic_type(Name/0,Code) :-
	current_predicate(basic_mono_type, basic_mono_type(_,_)),
	basic_mono_type(Name,Code).


init_symbol_table :-
	retractall(symbol_table(_,_,_,_,_)).

init_type_table :-
	retractall(type_code(_,_)).

init_declaration_tables :-
	retractall(feat_decl(_,_,_)),
	retractall(sort_decl(_,_,_,_,_)),
	retractall(mono_feat(_,_,_)),
	retractall(poly_feat(_,_)),
	retractall(feat_doms(_,_)),
	retractall(poly_feat_check(_,_,_,_)).

init_defaults :-
	basic_type(Symbol/Arity,Code),
	( Code == top ->
	      assert(symbol_table(Symbol,Arity,_,+,type))
	; var(Code) ->
	      %% need to generate code
	      symbol_table(assert, Symbol, Arity, _, +, type, _)
	; assert(symbol_table(Symbol,Arity,Code,+,type)),
	  ( number(Code) -> assert(type_code(Code,Symbol))
	  ; true )
	),
	fail.
init_defaults :-
	%% here we fix that string, number, and afs_symbol are the
	%% atom domains
	basic_type(string/0,SCode),
	basic_type(number/0,NCode),
	basic_type(afs_symbol/0,AFSCode),
	init_sat([SCode, NCode, AFSCode]),
	fail.
init_defaults :-
	current_predicate(basic_constants, basic_constants(_,_)),
	basic_constants(Type, Cs),
	symbol_table(assert, Type, 0, TID, +, type, _),
	init_consts(Cs, TID, CIDs, Axioms, []),
	negate_TID(TID,Out),
	sat(add_axioms, [[Out|CIDs]|Axioms], default, default),
	fail.
init_defaults :-
	current_predicate(basic_sort, basic_sort(_,_,_,_)),
	basic_sort(Sort, Arity, HeadDecl, ResultDecl),
	symbol_table(assert, Sort, Arity, *, +, sort, _),
	compile_sort_declaration([ResultDecl|HeadDecl], Decls),
	sort_decl_table(assert, Sort, Arity, Decls, default, default, ok),
	fail.
init_defaults :-
	current_predicate(basic_feature, basic_feature(_,_,_)),
	basic_feature(F, D, R),
	symbol_table(assert, D, 0, DID, +, type, _),
	( D == top -> true
	; Dom = [[DID]] ),
	symbol_table(assert, R, 0, RID, +, type, _),
	( R == top -> true
	; Ran = [[RID]] ),
	symbol_table(assert, F, 1, *, +, feat, ok),
	feat_decl_table(assert, F, Dom, Ran, default, default, _),
	( D == top -> true
	; negate_TID(DID,Out),
	  sat(add_axioms,[[Out,1]],default,default)
	),
	fail.
init_defaults :-
	current_predicate(basic_hierarchy, basic_hierarchy(_)),
	( basic_hierarchy(Type = SubTList),
	  symbol_table(assert, Type, 0, TID, +, type, _),
	  init_subtypes(SubTList, TID, STIDs, Axioms, Rest),
	  init_subtypes_disjoints(STIDs, Rest, []),
	  negate_TID(TID,Out),
	  sat(add_axioms, [[Out|STIDs]|Axioms], default, default)
	; basic_hierarchy(SubTList < Type),
	  symbol_table(assert, Type, 0, TID, +, type, _),
	  init_subtypes(SubTList, TID, STIDs, Axioms, Rest),
	  init_subtypes_disjoints(STIDs, Rest, []),
	  sat(add_axioms, Axioms, default, default)
	), fail.
init_defaults.


init_consts([], _, []) --> [].
init_consts([C|Cs], TID, [CID|CIDs]) -->
	{ symbol_table(assert, C, 0, CID, +, const, _),
	  negate_TID(CID,Out) }, !,
	[[Out,TID]], 
	init_consts(Cs, TID, CIDs).

init_subtypes([], _, []) --> [].
init_subtypes([ST|STs], TID, [STID|STIDs]) -->
	{ symbol_table(assert, ST, 0, STID, +, type, _),
	  negate_TID(STID,Out) }, !,
	[[Out,TID]],
	init_subtypes(STs, TID, STIDs).

init_subtypes_disjoints([_]) --> [].
init_subtypes_disjoints([X,Y]) --> !,
	{ negate_TID(X,Out1), negate_TID(Y,Out2) },
	[[Out1,Out2]].
init_subtypes_disjoints([X,Y|R]) -->
	init_subtypes_disjoints([X,Y]), !,
	init_subtypes_disjoints([X|R]), !,
	init_subtypes_disjoints([Y|R]).

sat(add_axioms, Axioms, FID, EID) :-
	add_axioms(Axioms, Flag), !,
	( Flag == 0 ->
	      cuf_out(error, ['type inconsistency!!!']),
	      cuf_out(entry, [FID, EID])
	; assert(added_axiom(FID, EID, Axioms)) ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: pretty-printer for CUF structures             %%%
%%%      Created: Mon Dec  7 10:48:23 1992                      %%%
%%%     Modified: Thu Mar  7 16:04:35 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.18  1996/03/07 15:06:24  jochen
% corrected last fix, which left a few \ too much in the output
%
% Revision 1.17  1995/11/07 17:27:24  jochen
% adapted to work in Sicstus3 as well (5x \ in 'atom')
%
% Revision 1.16  1994/06/30 17:45:03  jochen
% output to xmfed won't destroy bindings any more
%
% Revision 1.15  1994/06/22  09:38:16  michel
% print_body/6 prints out GoalIDs now if NodeID is instantiated
%
% Revision 1.14  1994/06/21  16:27:19  michel
% pp_fs/{4,5} --> pp_fs/{5,6}
% pp_fs_list/4 --> pp_fs_list/5
% pp_fs_list_/4 --> pp_fs_list_/5
% pp_fs_list_rest/6 --> pp_fs_list_rest/7
% from top_level.pl: print_rules/4, print_head/5,
%                    print_body/6, print_const/2
% new get_printing_options/1, printing_types/2,
%     printing_lists/2
%
% Revision 1.13  1994/06/13  12:23:17  michel
% pp_get_tag/2:
% * 'PROLOG'/1 case added
%
% Revision 1.12  1994/05/16  16:36:31  michel
% a lot of changes to produce correct output of constants:
% uses write_atom/1 now, uses type_term2atom/2 for type term conversion,
% my_length/2 checks now if a feature symbol has to be printed with quotes
%
% Revision 1.11  1994/05/10  16:01:33  jochen
% xmfed_output option now also works in kernel system (pp_fs/2 changed)
%
% Revision 1.10  1994/05/10  07:40:08  michel
% pp_fs/2: output_format flag sensitive now
%
% Revision 1.9  1994/03/16  12:47:57  michel
% pp_fs_list_/4, 2nd clause:
% 	bug in rest list handling removed
% pp_fs_list_rest/6 replaced by pp_fs_list_rest/7
% convert2list/3:
% 	3rd clause added
% convert2list_/3:
% 	3rd case (cuf_list/1 test) added (was part
% 	of the bug)
%
% Revision 1.8  1994/02/18  12:14:28  michel
% my_length/2 changed: new length is old length+2
% cuf_list/1 changed: old version caused problems, if a list contains
%                     more than 'F' & 'R' features (open world, you know)
% pp_fs_alist/3: cut in first clause added
% pp_fs_list_/4: 'STRUC'/4 case new (bug fix, see below)
% pp_fs_list_rest/5 --> /6: a shared rest FS will be printed out now (bug fix)
% convert2list/3: cases tested with functor/3 now (=/2 removed)
% convert2list_/3: L == [] case added
%
% Revision 1.7  1994/02/04  17:08:05  michel
% bug fixed in convert2list/3.
%
% Revision 1.6  1994/01/31  15:50:10  michel
% pp_fs_alist/3 bug fixed
%
% Revision 1.5  1994/01/31  14:27:43  michel
% pp_fs_alist/3 changed
%
% Revision 1.4  1994/01/18  16:03:42  michel
% a lot of changes for new preparation format
%
% Revision 1.3  1993/12/08  13:31:35  michel
% '\+ \+' in front of pp_fs/3 calls
% (instead of copy_term/2 calls in prepare_printing.pl)
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Mon Sep 27 17:33:30 1993 (michel) : pp_get_tag 'ATOM' case added
% Tue Sep 14 10:37:52 1993 (michel) : prefix/1 added
% Mon Sep 13 19:15:46 1993 (michel) : pp_constraint: 'POLY' case added
% Wed Jul 28 16:34:02 1993 (dorna) : variables in inequality constraints will be printed as 'top'
% Tue Apr  6 22:25:13 1993 (michel) : rest list bug removed (?) :-)
% Mon Mar 22 14:26:57 1993 (michel) : prepare_printing and dependent parts extracted
% Mon Mar  8 14:01:03 1993 (michel) : "ttynl" replaced by "nl, ttyflush"
% Sat Feb 27 14:56:37 1993 (michel) : prepare_pp_m added
% Sat Feb 27 13:45:12 1993 (michel) : type_term2atom/2 added

% pp_tt(+TypeTerm)

% pp_c(+Head, +Subgoals)

% pp_ft(+FeatureTerm)

/*
  There are promblems with shared parts of lists. If a sublist is shared with 
  another list in a structure, the sharing will be ignored in the ASCII list
  output. But sometimes the shared list elements have sharings which cannot
  be seen too. The one and only correct way is to use the first-rest feature
  output.

  The problem obove is a preparation problem. The feature structures are
  marked only at the outest level of structure sharings. The reason for this
  treatment is the unnessecary introduction of labels in shared parts during
  printing. These labels will never occur twice in one output. So if a list 
  is printed without labeling sublists, sharings cannot be seen, even in 
  the structures which are elements of the sublist.

  An old version forgot about the structure after a second occurence of a
  substructure. The current version prepares shared substructures twice and
  more times. This was done for the graphical output only. What shall we do?
  Is this really necessary?

  MD.
*/

prefix(['$TAB$'(2)]).

%%%  pp_fs(+FeatureStructure)
%%%  pp_fs(+FeatureStructure, +ConstraintList)
%%%  prints out FeatureStructure (internal CUF format --> AVM)
pp_fs(FS) :-
   pp_fs(FS, []), !.

pp_fs(FS, Cs) :-
   get_flag(xmfed_output,yes), !,
   \+ \+ ( prepare_printing(FS, Cs, PrepFS, PrepCs),
           pp2fed(PrepFS, PrepCs)).
pp_fs(FS, Cs) :-
   ( get_flag(output_format,matrix) ->
	 prefix(Prefix),
	 \+ \+ pp_fs(FS, Cs, Prefix), !
   ; get_flag(output_format,cuf) ->
	 \+ \+ (cuf_intern2extern(FS, Cs, [], FT),
	        pp_ft(FT)), !
   ).
pp_fs(_,_) :-
   cuf_out(error, ['printing of feature structure failed','$NL$']).

pp_fs(FS, Cs, Prefix) :-
   prepare_printing(FS, Cs, PrepFS, PrepCs),
   nl, writel_('$PRE$'(Prefix)),
   get_printing_options(Options),
   pp_fs(PrepFS, Options, Prefix, 1, _),
   pp_cl(PrepCs, Prefix).

%%%  pp_cl(+ListOfConstraints, Tabular, CurrentIndex,
%%%        -IndexOut)
%%%  ascii output procedure for a list of constraints
pp_cl([], _) :- nl.
pp_cl([C|R], Prefix) :-
   pp_constraint(C, Prefix),
   pp_cl(R, Prefix).

pp_constraint('DIFF'(FS1, FS2), Prefix) :-
   pp_get_tag(FS1, T1),
   pp_get_tag(FS2, T2),
   ( sort([T1,T2],[S1,S2]) ->
	 nl, writel_('$PRE$'(Prefix)),
	 write_atom(S1), write(' =/= '), write_atom(S2)
   ; true
   ).
pp_constraint('POLY'(_ID,TI1,DTA,TI2,RTA), Prefix) :-
   pp_get_tag(TI1, T1),
   pp_get_tag(TI2, T2),
   nl, writel_('$PRE$'(Prefix)),
   write_atom(T1), write(' & '), write_atom(DTA), write(' --> '),
   write_atom(T2), write(' & '), write_atom(RTA).

ground_var('_') :- !.
ground_var(_).

pp_get_tag('STRUC'(_,Tag,_,_), Tag) :-
	ground_var(Tag).
pp_get_tag('ATOM'(Atom), Atom).
pp_get_tag('REF'(Tag), Tag).
pp_get_tag('PROLOG'(X), !(X)).

%%%  pp_fs(+MarkedFeatureStructure, Tabular, CurrentIndex,
%%%        -IndexOut)
%%%  ascii output procedure for feature structures (matrix notation)
pp_fs('ATOM'(A), _, _, I, I) :-
   write_atom(A), nl.
pp_fs('PROLOG'(P), _, _, I, I) :-
   write('!('), write(P), write(')'), nl.
pp_fs('STRUC'(Multi,Tag,TT,FS), Options, Prefix, I0, In) :-
   ( Multi == 'MULTI' ->
       ( var(Tag) ->        % first occurence
	     mk_and_print_tag(Tag, Tab, I0, I1),
	     type_term2atom(TT, Atom),
	     pp_fs(FS, Atom, Options, ['$PRE$'(Prefix), '$TAB$'(Tab)], I1, In)
       ; write(Tag), nl,
	 I0 = In
       )
   ;  pp_fs(FS, TT, Options, Prefix, I0, In)
   ), !.
pp_fs('CYCLE'(Tag), _, _Prefix, I, I) :-
   write(Tag), nl.
pp_fs('REF'(Tag), _, _, I, I) :-
   write_atom(Tag), nl.

mk_and_print_tag(Tag, Tab, I0, I1) :-
   number_chars(I0, Cs),
   append([0'( | Cs], ")", Chars),
   atom_chars(Tag, Chars),
   write(Tag), write(' '),
   I1 is I0+1,
   length(Cs, L),
   Tab is L+3.

%%%  pp_fs(+SortedFeatureValuePairs, +TypeTerm, +Options, +Tab, +Prefix,
%%%         +IndexIn, -IndexOut)
pp_fs([], TT, _, _, I, I) :- !,
   type_term2atom(TT,Atom),
   write_atom(Atom), nl.
pp_fs(FS, TT, Options, Prefix, I0, In) :-
   ( cuf_list(FS), printing_lists(Options,yes) ->
	 pp_fs_list(FS, Options, Prefix, I0, In)
   ; write('+--'),
     ( printing_types(Options, no) -> nl
     ; TT == top -> nl
     ; type_term2atom(TT,Atom),
       write(' '), write_atom(Atom), nl
     ), 
     pp_fs_(FS, Options, Prefix, I0, In)
   ).

pp_fs_([], _, Prefix, I , I) :-
   writel_('$PRE$'(Prefix)), write('+--'), nl, ttyflush.
pp_fs_([F:V|R], Options, Prefix, I0, In) :-
   my_length(F, Tab),
   writel_('$PRE$'(Prefix)), write('|'), writeq(F), write(': '),
   pp_fs(V, Options, ['$PRE$'(Prefix),'|', '$TAB$'(Tab)], I0, I1),
   pp_fs_(R, Options, Prefix, I1, In).

cuf_list(['F':_,'R':_]) :- !.
%cuf_list(['R':_,'F':_]).

%%%  pp_fs_list(+ListOf'F'and'R'Features, +Options, +Prefix, +IndexIn, -IndexOut)
pp_fs_list(List, Options, Prefix, I0, In) :-
   convert2list(List, CList, Flag), !,
   ( Flag == no ->
	 write('['), nl, writel_('$PRE$'(Prefix)), write(\),write(' '),
	 pp_fs_list_(CList, Options, Prefix, I0, In)
   ; write('['),
     pp_fs_alist(CList, I0, In),
     nl
   ).

%%%  pp_fs_alist(+ListOfAtomsOrLabels, +IndexIn, -IndexOut)
pp_fs_alist([L], I0, In) :- !,
   pp_fs_a(L, A, I0, In),
   write_atom(A), write(']').
pp_fs_alist([F|R], I0, In) :-
   pp_fs_a(F,A,I0,I1),
   ( R = 'STRUC'(Multi, Tag, TT, FS) ->
	 write_atom(A), write(' | '),
	 ( nonvar(Tag) ->
	       I1 = In, write(Tag)
         ; Multi == 'MULTI' ->
	       mk_and_print_tag(Tag, _, I1, I2),
	       ( FS == [] ->
		     I2=In
	       ; write('['),
		 pp_fs_alist(FS, I2, In)
	       )
	 ; I1 = In,
	   type_term2atom(TT,Atom),
	   write_atom(Atom)
	 ),
	 write(']')
   ; R = 'CYCLE'(Tag) ->
         I1 = In, write_atom(A), write(' | '), write_atom(Tag), write(']')
   ; R = 'REF'(Tag) ->
         I1 = In, write_atom(A), write(' | '), write_atom(Tag), write(']')
   ; write_atom(A), write(', '),
     pp_fs_alist(R, I1, In)
   ).

pp_fs_a('ATOM'(A), A, I, I).
pp_fs_a('STRUC'(Multi,Tag, TT, _), A, I0, In) :-
   ( nonvar(Tag) ->
	 A = Tag, I0 = In
   ; Multi == 'MULTI' ->
	 mk_and_print_tag(Tag, _, I0, In),
	 type_term2atom(TT,A)
   ; I0 = In, type_term2atom(TT,A)
   ).
pp_fs_a('REF'(Tag), Tag, I, I).

%%%  pp_fs_list_(+ListOfComplexStructures, +Options, +Prefix, +IndexIn, -IndexOut)
pp_fs_list_([F], Options, Prefix, I0, In) :- !,
   pp_fs(F, Options, ['$PRE$'(Prefix), (\),' '], I0, In),
   writel(['$PRE$'(Prefix), ']','$NL$']).
pp_fs_list_([F|R], Options, Prefix, I0, In) :-
   pp_fs(F, Options, ['$PRE$'(Prefix), (\),' '], I0, I1),
   ( R = 'STRUC'(Multi, Tag, TT, FS) ->
	 pp_fs_list_rest(Multi, Tag, FS, TT, Options, Prefix, I1, In)
   ; R = 'CYCLE'(Tag) ->
         pp_fs_list_rest('CYCLE', Tag, _, _, _Options, Prefix, I1, In)
   ; R = 'REF'(Tag) ->
	 pp_fs_list_rest('REF', Tag, _, _, _Options, Prefix, I1, In)
   ; writel(Prefix), write(\), write(' '),
     pp_fs_list_(R, Options, Prefix, I1, In)
   ).

pp_fs_list_rest(Multi, Tag, FS, TT, Options, Prefix, I1, In) :-
   writel(['$PRE$'(Prefix), '. ']),
   ( nonvar(Tag) ->
	 I1 = In,
	 writel([Tag,'$NL$','$PRE$'(Prefix), ']','$NL$'])
   ; Multi == 'MULTI' ->
	 mk_and_print_tag(Tag, Tab, I1, I2),
	 ( FS == [] ->
	       type_term2atom(TT,Atom),
	       write_atom(Atom), nl, I2=In
	 ; writel(['[', '$NL$','$PRE$'(Prefix),'  ','$TAB$'(Tab),(\),' ']),
           pp_fs_list_(FS,Options,['$PRE$'(Prefix),'  ','$TAB$'(Tab)],I2,In)
	 ),
	 writel(['$PRE$'(Prefix), ']','$NL$'])
%   ; (Multi == 'CYCLE' ; Multi == 'REF') ->
%	 fail
   ; FS == [] ->
         I1 = In,
	 writel([list,'$NL$','$PRE$'(Prefix), ']','$NL$'])
   ; writel(['[','$NL$']),  % not a well-formed list!!!
     pp_fs_(FS, Options, ['$PRE$'(Prefix),'  ',(\),' '],I1,In),
     writel(['$PRE$'(Prefix), ']','$NL$'])
   ).

convert2list(['F':FV, 'R':R], [FV|CRV], Flag) :- !,
   ( functor(FV,'ATOM',1)
   ; functor(FV,'STRUC',4),
     ( arg(2,FV,Tag),nonvar(Tag) ; arg(4,FV,[]) )
   ; functor(FV,'REF',1)
   ; Flag = no ), !,
   convert2list_(R, CRV, Flag).
convert2list([], [], _) :- !.
convert2list(X, X, no).

convert2list_('ATOM'([]), [], _).
convert2list_('STRUC'(Multi,Tag,TT,L), CL, Flag) :-
   ( Multi == 'MULTI' ->
	 ( nonvar(Tag) ->
	       CL = 'REF'(Tag)
	 ; CL = 'STRUC'(Multi,Tag,TT,CLR),
	   convert2list(L, CLR, Flag)
	 )
   ; L == [] ->
	 type_term2atom(TT,R),
	 CL = 'REF'(R) % CL = 'STRUC'(Multi,Tag,TT,L) would be correct
                        % but not so efficient
   ; cuf_list(L) ->
	 convert2list(L, CL, Flag)
   ; CL = 'STRUC'(Multi,Tag,TT,CLR),
     convert2list(L, CLR, Flag)
   ).
convert2list_('CYCLE'(T), 'CYCLE'(T), _).
convert2list_('REF'(R), 'REF'(R), _).


print_rules([],_,_,_).
print_rules([Clause|R],No,Sort,Arity) :-
   cuf_out(message,[clause,No,of,(Sort/Arity):'','$NL$']),
   prepare_printing_clause(Clause, PrepHead, PrepBody, PrepCons), !,
   prefix(Prefix),
   get_printing_options(Options),
   print_head(PrepHead, Options, Prefix, 1, I1), !,
   print_body(PrepBody, 1, Options, Prefix, I1, _), !,
   print_const(PrepCons, Prefix), !,
   No1 is No+1,
   print_rules(R,No1,Sort,Arity).

print_head('GOAL'(_,_,_,[Result|ArgList]), Options, Prefix, I, O) :-
   goal_args_to_fs(ArgList, 1, FeaturedArgs),
   cuf_out(message,[head:'','$NL$'|Prefix]),
   pp_fs('STRUC'(_,_,top,['Result':Result|FeaturedArgs]), Options, Prefix, I, O).

print_body([], _, _, _, I, I).
print_body(['GOAL'(NodeID:SubG,Name/Arity,Flag,[Result|ArgList])|GL],
           No, Options, Prefix, I, O) :-
   ( var(NodeID) ->
	 cuf_out(message,[subgoal,No:'',(Name/Arity), Flag,'$NL$'|Prefix])
   ; cuf_out(message,[subgoal,No:'',(Name/Arity),goalID(NodeID:SubG),Flag,'$NL$'|Prefix])
   ),
   goal_args_to_fs(ArgList, 1, FeaturedArgs),
   pp_fs('STRUC'(_,_,top,['Result':Result|FeaturedArgs]), Options, Prefix, I, I1),
   No1 is No+1,
   print_body(GL, No1, Options, Prefix, I1, O).

print_const([], _) :- !.
print_const(CL, Prefix) :-
   cuf_out(message,[constraints:'','$NL$']),
   pp_cl(CL, Prefix).

:- dynamic 'LENGTH'/2.

%%%  my_length(+Atom, ?Length)
my_length(Symbol, Length) :-
   ( 'LENGTH'(Symbol, Length) -> true
   ; ( number(Symbol) -> number_chars(Symbol, Chars)
     ; atom_chars(Symbol, [F|Chars])),
     length([F|Chars], Length0),
     ( F =< 0'z, F >= 0'a -> X=0 ; X=2 ), % X is for the two '
     Length is Length0+2+X,
     assert('LENGTH'(Symbol, Length))
   ).

%%%  writel(+ListOfTokens)
%%%  write list of (possibly annotated) tokens
writel([]).
writel([F|R]) :-
   writel_(F),
   writel(R).

writel_('$NL$') :- !, nl.
writel_('$TAB$'(I)) :- !, tab(I).
writel_('$PRE$'(Prefix)) :- !, writel(Prefix).
writel_(X) :- write(X).

printing_types(print(Types,_), Types).
printing_lists(print(_,Lists), Lists).

get_printing_options(Options) :-
    get_flag(print_types, Types),
    printing_types(Options, Types),
    get_flag(print_lists, Lists),
    printing_lists(Options, Lists).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: unification of typed feature structures       %%%
%%%      Created: Fri Sep 18 13:25:10 1992                      %%%
%%%     Modified: Thu Apr  4 17:59:45 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.8  1996/04/09 13:22:48  jochen
% unification of atoms speeded up
%
% Revision 1.7  1994/10/10 12:14:27  jochen
% unify/4 first clause had been commented; but needed for variable type
% info
%
% Revision 1.6  1994/06/30  17:33:43  jochen
% modified get_type/3/4 and gcs/4 for cuf_subsumes to work with numbervars
% this is still no solution for subsumption checks of atoms w. equiv. types
%
% Revision 1.5  1994/05/19  14:48:30  jochen
% unify/2 made cyclic-structures-proof
%
% Revision 1.4  1994/01/18  12:10:06  jochen
% gcs/4 hacked to fix the bug where unification of an atom with a formula that
% is _equivalent_ to it failed
%
% Revision 1.3  1994/01/10  11:17:05  jochen
% New predicate: negate_TID(+TID, -Negated_TID)
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Wed Dec  2 11:40:15 1992 (michel) : gcs/4 changed for type_clash flag
% Thu Nov  5 15:08:03 1992 (michel) : unify/4 changed

%%%%%%%  unify(?TypedFeatureStructure1, ?TypedFeatureStructure2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  graph unification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unify(X,X) :- !.
unify([TI1|FS1], [TI2|FS2]) :-
    unify(TI1, TI2, FS1, FS2), !.

%% next clause needed in case TI is variable !
unify(TI, TI, FS1, FS2) :- !,    
    unify_fs(FS1, FS2).
unify(TI1, TI2, FS1, FS2) :-
    get_type(TI1, Type1, TypeInfo1),
    get_type(TI2, Type2, TypeInfo2),
    ( var(TypeInfo1),TypeInfo1 == TypeInfo2 ->
	  true  %% already unified; e.g. cyclic str.
    ; ( var(FS1) ; var(FS2) 
%       ; arg(1,FS1,FS1A),compound(FS1A) ; arg(1,FS2,FS2A),compound(FS2A) ) ->
      ; arg(1,FS1,FS1A),functor(FS1A,':',2) 
      ; arg(1,FS2,FS2A),functor(FS2A,':',2) ) ->
	  gcs(Type1, Type2, TypeInfo1, TypeInfo2), 
	  unify_fs(FS1, FS2)
    ; %otherwise atom or prolog-term ->
          FS1 = FS2 
%,
%           ( Type1 == Type2 -> true
%  	  ; write('CUF internal error: unifier error1'(Type1,Type2)),nl),
%           ( TypeInfo1 == [], TypeInfo2 == [] -> true
%  	  ; write('CUF internal error: unifier error2'(TypeInfo1,TypeInfo2)),nl)
    ).

%unifiable(X, Y) :-
%	\+ \+ unify(X, Y), !.

%%%%%%%  get_type(+TypeInfoIn, -Type, -TypeInfoOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  TypeInfo is an open ended list. the current type is the most
%%%  nested one.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
get_type(TypeInfoI, Type, TypeInfoO) :-
    ( var(TypeInfoI) -> 
	  TypeInfoO = TypeInfoI
    ; TypeInfoI = [Type0|Rest],
      ( var(Rest) -> 
	Type = Type0, TypeInfoO = Rest
      ; atomic(Rest) ->  %% Rest == []
	Type = Type0, TypeInfoO = []
      ; get_type(Rest, Type, TypeInfoO)
      )
    ).
*/

get_type(TypeInfoI, _Type, TypeInfoO) :-
    var(TypeInfoI), !, 
    TypeInfoO = TypeInfoI.
get_type([Type0|Rest], Type, TypeInfoO) :- !,
    get_type(Rest,Type0, Type, TypeInfoO).
get_type(TypeInfo, _Type, TypeInfo).

get_type(Rest, Type, Type, Rest) :-
    var(Rest), !.
get_type([Type0|Rest], _, Type, TypeInfoO) :- !,
    get_type(Rest,Type0, Type, TypeInfoO).
get_type(R, Type, Type, R).


%%%%%%%  gcs(+Type1, +Type2, ?TypeInfo1, ?TypeInfo2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  greatest common subtype 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We avoid the problem with Type1 being an atom (TI1=[]) and Type2
%% being a formula equivalent to it (in that case check_formula(Type1,
%% Type2, F) returns 2) by exchanging the arguments if TI1 is bound
gcs(Type, Type, TypeInfo, TypeInfo) :- !.
gcs(Type1, Type2, TI1, TI2) :-
    ( var(TI1) ->
	  check_formulas(Type1, Type2, Flag),
	  gcs(Flag, Type1, Type2, TI1, TI2)
    ; var(TI2) ->
	  check_formulas(Type2, Type1, Flag),
	  gcs(Flag, Type2, Type1, TI2, TI1)
    ; %% TI1==TI2, wg. cuf_subsumes
      check_formulas(Type1, Type2, 2),  %% hack: both are
      check_formulas(Type2, Type1, 2)   %% equivalent
    ).


gcs(0, Type1, Type2, _TI1, _TI2) :-
    get_flag(type_clash, yes),
    cuf_out(message, ['TYPE CLASH:','$TT$'(Type1),'&','$TT$'(Type2)]),
    fail.
gcs(1, Type1, _Type2, TI1, [Type1|TI1]).
gcs(2, _Type1, Type2, [Type2|TI2], TI2).
gcs(3, Type1, Type2, [SType3|R], [SType3|R]) :- !,
    append(Type1, Type2, Type3),
    sort(Type3, SType3).
gcs(3, Type1, _, [], [Type1]) :- !.
gcs(3, _, Type2, [Type2], []).


% check_formulas(_,_,3).              % dummy
% check_formula(_,1).                 % dummy

%%%%%%%  unify_fs(?FeatureStructure1, ?FeatureStructure2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  unification of feature structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unify_fs(X, X) :- !.
unify_fs([Feat:Val1|Rest1], Graph) :-
    del_once(Feat:Val2, Graph, GraphRest),
    unify(Val1, Val2),
    unify_fs(Rest1, GraphRest).


%%%%%%%  negate_TID(+TID, -Negated_TID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  give back negative type ID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

negate_TID(TID, Neg) :-
	integer(TID), !,
	Neg is -TID.
negate_TID(TID, []) :-
	var(TID), !.
negate_TID([], _) :- !.
negate_TID(TID, -TID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: utilities                                     %%%
%%%      Created: Fri Sep 18 13:08:19 1992                      %%%
%%%     Modified: Tue Mar 19 15:29:07 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.15  1996/04/09 13:58:21  jochen
% set_flag, newID, recycleID speeded up;
% a few retractall's eliminated
%
% Revision 1.14  1996/03/07 15:15:02  jochen
% delay_pattern/3 elinimated
%
% Revision 1.13  1995/11/07 16:14:43  jochen
% nth/3, nth_del/4 made bidirectional; delete_nonvar/3, rev/2, cuf_term_variables/3
% added (for PCUF and memo)
%
% Revision 1.12  1995/06/12  08:04:25  jochen
% more safety: predicate prolog_list no longer accepts atom '.'
%
% Revision 1.11  1995/02/01  15:40:43  jochen
% global_cuf_init made saved-state-proof; i.e. cuf_save_program/1 can
% now be used to make saved-states containing (partial) grammars
%
% Revision 1.10  1994/06/30  17:50:25  jochen
% basic_type/1 moved to table_operations.pl
%
% Revision 1.9  1994/06/15  13:08:36  junger
% - memb_nonvar/2 added (from tree_in_xmfed.pl)
%
% Revision 1.8  1994/05/17  10:41:26  michel
% yn_question/1:
% * accepts a RETURN now (instead of typing 'n' or 'N')
%
% Revision 1.7  1994/02/03  11:28:42  junger
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9
%
% Revision 1.6  1994/01/18  12:14:25  jochen
% annotatedterm2prolog/2 now also takes strings in pre-parser format (makes
% some error messages fancier)
% retract 'STRING_CONST' facts in init_data_base.
% is_string/1 moved from cuf_extern2intern to here
%
% Revision 1.5  1994/01/10  10:36:16  jochen
% 1) Handling of IDs changed. IDs which can be recycled are no longer
%   'CUF_COUNTER's, but are 'CUF_ID's. New predicates
%   newID(Name, Value, Inc)  %% Inc is used to determine next free value,
%                            %% if no more recycled IDs are available
%   recycleID(Name, Value)   %% Value is registered free for ID Name
%   setID(Name, Value)       %% Values for ID Name are counted up from Value
%
%   retract_all_counters_and_IDs/0 replaces retract_all_counters/0
%
%   inc_counter/3 has no longer the functionality of maintaining several
%   counter values; just increments the (unique) counter Name.
%
%   New predicate get_counter/2.
%
%
% 2) annotatedterm2prolog/2 accomodated for new number terms ('N'-terms).
%
% 3) init_sat now done in init_defaults (now needs parameters).
%
% Revision 1.4  1993/12/22  15:11:59  jochen
% forall/2 added
%
% Revision 1.3  1993/11/24  17:21:26  michel
% new predicate deL_counter/1 added
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Fri Sep 24 17:55:17 1993 (michel) : annotatedterm2prolog1 changed
% Fri Sep 24 11:54:32 1993 (michel) : ls/1 removed
% Wed Sep 22 15:39:33 1993 (jochen) : init_grammar_conf added
% Thu Jul  1 20:03:16 1993 (dorna) : annotatedterm2prolog/3 removed
% Wed Jun 16 17:16:01 1993 (michel) : init_data_base extended
% Tue Apr  6 22:55:30 1993 (michel) : parser output format modifications
% Tue Apr  6 16:18:49 1993 (michel) : undo/1 moved to quintus.pl
% Tue Dec  8 20:58:39 1992 (michel) : writel/1 removed
% Fri Dec  4 17:20:37 1992 (michel) : prep_output_vars/1 removed
% Thu Dec  3 16:51:52 1992 (michel) : yn_question/1 changed
% Thu Dec  3 15:27:33 1992 (michel) : compiler_init: init_flags removed
% Thu Dec  3 11:44:51 1992 (michel) : dot/0 removed
% Thu Dec  3 11:41:11 1992 (michel) : error_handler/(0,2),
% warning_handler/(0,2), msg/(1,3), nlmsg/1, msgnl/1, errormsg/3,
% msg_in_procedure/1, error_in_procedure/1 removed
% Wed Dec  2 11:55:45 1992 (michel) : version/0 removed
% Tue Dec  1 14:12:19 1992 (michel) : version changed
% Fri Nov 20 14:31:26 1992 (michel) : version changed
% Fri Nov 20 11:14:33 1992 (michel) : member/3 --> member_no/3
% Mon Nov 16 18:14:13 1992 (michel) : member/2,memberchk/2 removed
% Tue Nov 10 15:46:07 1992 (michel) : init_database changed
% Tue Nov  3 19:04:01 1992 (michel) : version changed
% Mon Nov  2 10:34:30 1992 (michel) : gensym/2, concat/3 removed
% Wed Oct 28 18:41:33 1992 (michel) : assert_if_new/1 changed
% Wed Oct 28 18:10:35 1992 (michel) : spys/0,ls/0,sh/0 added
% Wed Oct 28 15:46:06 1992 (michel) : retractall(formula_table(_)) added


term2list(Functor, Term, [Arg1|Rest]) :-
    Term =.. [Functor, Arg1, Arg2], !,
    term2list(Functor, Arg2, Rest).
term2list(_, Term, [Term]).

%%%%%%%%%%%%%%%%%%%%%%%  FLAGS

:- dynamic 'CUF_FLAG'/2.

%%% set_flag(+Name, +Value)
%%% set value of flag Name on Value
set_flag(Name, Value) :-
    atomic(Name),
    nonvar(Value),
    retractall('CUF_FLAG'(Name, _)),
    asserta('CUF_FLAG'(Name, Value)).

%%% get_flag(+Name, ?Value)
%%% get the value Value of flag Name 
get_flag(Name, Value) :-
%    atomic(Name),
    ( 'CUF_FLAG'(Name, Value)
    ; Value = unknown
    ), !.

%%% del_flag(+Name)
%%% del_flag(+Name, ?Value)
%%% delete flag Name and match Value with current value
del_flag(Name, Value) :-
    atomic(Name),
    get_flag(Name, Value),
    retractall('CUF_FLAG'(Name, _)).

del_flag(Name) :-
    atomic(Name),
    retractall('CUF_FLAG'(Name, _)).

%%%%%%%%%%%%%%%%%%%%%%%  IDs

%%%  newID(+Name, -Value, +Increment)
%%%  return ID value Value and 
%%%  increment counter Name by Increment (only if not an old value was
%%%  reused) 
%%%  N.B.: it's possible to have more than one ID value in the DB
%%%  Recycled values are added with assertz.
%%%  The first clause has always a larger value than other clauses.
%%%  Recycled values are reused in a FIFO way (always take out and
%%%  give back second value, if more than one exists).

% newID(Name, Value, Inc) :-
%     atomic(Name),
%     integer(Inc),
%     var(Value),
%     clause('CUF_ID'(Name, Value1), true, DBRef) ->
%     ( clause('CUF_ID'(Name, Value2), true, DBRef2),
%       \+ DBRef == DBRef2 ->	% there exist other values
%           Value=Value2,
% 	  erase(DBRef2)
%     ; Value=Value1,
%       ValueNew is Value+Inc,
%       assert('CUF_ID'(Name, ValueNew)),	% safety first!
%       erase(DBRef)
%     ).
newID(Name, Value, Inc) :-
    atomic(Name),
    integer(Inc),
    var(Value),
    retract('CUF_ID'(Name,Value)),!,
    ( 'CUF_ID'(Name,_) -> true
    ; ValueNew is Value+Inc,
      assert('CUF_ID'(Name, ValueNew))
    ).


%%%  recycleID(+Name, +Value)
%%% give back ID value (Value is registered free for ID Name)
% recycleID(Name, Value) :-
%     clause('CUF_ID'(Name, Value), true), !.  %% shouldn't occur
% recycleID(Name, Value) :-
%     integer(Value),
%     assertz('CUF_ID'(Name, Value)).
recycleID(Name, Value) :-
    clause('CUF_ID'(Name, Value), true), !.  %% shouldn't occur
recycleID(Name, Value) :-
    integer(Value),
    asserta('CUF_ID'(Name, Value)).


%%%  setID(+Name, +Value)
%%%  Set (initialize) global ID Name to value Value.
%%%  Next newID gets that Value.
setID(Name, Value) :-
    atomic(Name),
    integer(Value),
    retractall('CUF_ID'(Name, _)),
    assert('CUF_ID'(Name, Value)), !.


retract_all_counters_and_IDs :-
    retractall('CUF_COUNTER'(_,_)),
    retractall('CUF_ID'(_,_)).


%%%%%%%%%%%%%%%%%%%%%%%  ordinary COUNTERS

%%%  inc_counter(+Name, -Value, +Increment)
%%%  return counter value Value and 
%%%  increment counter Name by Increment

inc_counter(Name, Value, Inc) :-
    atomic(Name),
    integer(Inc),
    var(Value),
    retract('CUF_COUNTER'(Name, Value)),
    ValueNew is Value+Inc,
    assert('CUF_COUNTER'(Name, ValueNew)).


%%%  get_counter(+Name, -Value)
%    get value of global counter Name
get_counter(Name, Value) :-
    atomic(Name),
    var(Value),
    clause('CUF_COUNTER'(Name, Value), true).


%%%  set_counter(+Name, +Value)
%    set (initialize) global counter Name to value Value
set_counter(Name, Value) :-
    atomic(Name),
    integer(Value),
    retractall('CUF_COUNTER'(Name, _)),
    assert('CUF_COUNTER'(Name, Value)), !.

del_counter(Name) :-
    atomic(Name),
    retractall('CUF_COUNTER'(Name, _)), !.


rule_counter(FID, EID, NewNo) :-
    ( clause('rule counter'(FID, EID, No), true, DBRef) ->
	  NewNo is No+1,
	  assert('rule counter'(FID, EID, NewNo)),
	  erase(DBRef)
    ; assert('rule counter'(FID, EID, 1)),
      NewNo = 1
    ).

%%%%%%%%%%%%%%%%%%%%%%%  SYMBOL GENERATOR

/* library(strings) used instead!!!
:- dynamic concat_string/3.

gensym(Prefix, V) :-
    atomic(Prefix),
    var(V),
    ( inc_counter(Prefix, N, 1), !
    ; set_counter(Prefix, 1), N = 0
    ),
    concat(Prefix, N, V),
    !.

concat(Prefix, No, Atom) :-
    ( clause(concat_string(Prefix, NoString, String), true) -> true
    ; name(Prefix, PrefixString),
      append(PrefixString, NoString, String),
      assert(concat_string(Prefix, NoString, String))
    ), 
    name(No, NoString),
    name(Atom, String).
*/

%%%  annotatedterm2prolog(+AnotatedTerm, -PrologTerm)
%%%  eliminates the annotations of the CUF parser (e.g. for output)
%%%  elimination of 'A'/1, 'N'/1, 'S'/1, 'T'/1, 'L'/1, 'V'/1 and 'V'/2 
annotatedterm2prolog('V'(X), X) :- !.
annotatedterm2prolog('V'(X,_), X) :- !.
annotatedterm2prolog('A'(X), X) :- !.
annotatedterm2prolog('N'(X), X) :- !.
annotatedterm2prolog('S'(X), String) :- is_string(X), !,
    append([0'"|X], [0'"], Cs),
    atom_chars(String, Cs).
annotatedterm2prolog('S'(X), X) :- !.
annotatedterm2prolog('T'(X), Trans) :- !,
    annotatedterm2prolog(X, Trans).
annotatedterm2prolog('L'([]), []) :- !.
annotatedterm2prolog('L'(X), Trans) :- !,
    annotatedterm2prolog1(X, Trans).
annotatedterm2prolog(Atom, Atom) :-
    atomic(Atom), !.
annotatedterm2prolog(TermIn, TermOut) :-
    TermIn =.. [Atom|Args],
    annotatedterm2prolog2(Args, Trans),
    TermOut =.. [Atom|Trans].

annotatedterm2prolog2([],[]).
annotatedterm2prolog2([Arg|Args], [TArg|TArgs]) :-
    annotatedterm2prolog(Arg, TArg),
    annotatedterm2prolog2(Args, TArgs).

annotatedterm2prolog1([], [])  :- !.
annotatedterm2prolog1([F|R], [FTrans|RTrans]) :- !,
    annotatedterm2prolog(F, FTrans),
%    append(FTrans, RTrans, Trans),
    annotatedterm2prolog1(R, RTrans).
annotatedterm2prolog1(Term, OTerm) :-
    annotatedterm2prolog(Term,OTerm).


%%%%%%%%%%%%%%%%%%%%%%%  MISCELLANOUS (sp? :-)

%%%  prolog_list(X)
%%%  tests, if X is a list
prolog_list(X) :-
	functor(X, '.', 2).
/*
member(X, [X|_]).
member(X, [_|L]) :-
    member(X, L).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :-
    memberchk(X, L).
*/
delete(X, [X|L], L).
delete(X, [Y|L], [Y|L1]) :-
	delete(X, L, L1).

del_once(X, [X|L], L) :- !.
del_once(X, [Y|L], [Y|L1]) :-
	del_once(X, L, L1).

% delete_all(?Elem, ?List, ?RestList)
delete_all(X, List, Rest) :-
	( delete(X, List, Rest1) ->
	      delete_all(X, Rest1, Rest)
	; List = Rest
	).


%   nth(?N, +Element, ?List)
%   nth/3 is true when Element is the Nth element of List, counting the first
%   element as 1.

nth(N, Element, List) :-
	integer(N), !,
	N >= 1,
	N1 is N-1,
	nth0i(N1, List, Element).
nth(N, Element, List) :-
	var(N),
	nth0v(List, Element, 1, N).


%   nth_del(?N, +List, ?Element, ?Rest)
%   nth is true when Element is the N:th element in List and Rest is all the
%   elements in the List except Element.

nth_del(N, List, Element, Rest) :-
	integer(N), !,
	N >= 1,
	N1 is N-1,
	nth0i_del(N1, List, Element, Rest).
nth_del(N, List, Element, Rest) :-
	var(N),
	nth0v_del(List, Element, 1, N, Rest).


nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	N is M + 1,
	nth0v(Tail, Element, N, Index).

nth0i(0, [Head|_], Head) :- !.
nth0i(N, [_|Tail], Element) :-
	M is N - 1,
	nth0i(M, Tail, Element).


nth0v_del([Element|Tail], Element, Index, Index, Tail).
nth0v_del([Head|Tail], Element, M, Index, [Head|Rest]) :-
	N is M + 1,
	nth0v_del(Tail, Element, N, Index, Rest).

nth0i_del(0, [Head|Tail], Head, Tail) :- !.
nth0i_del(N, [Head|Tail], Element, [Head|Rest]) :-
	M is N - 1,
	nth0i_del(M, Tail, Element, Rest).



/* used in cuf_dbg_xt.pl !! */
member_no(X, L, N) :-
   member_no(X, L, 1, N).

member_no(X, [X|_], N, N) :- !.
member_no(X, [_|L], N0, N) :-
   N1 is N0+1,
   member_no(X, L, N1, N).
/* */

%% assert_if_new/1, should be used only for small tables!
assert_if_new(Fact) :-
	( clause(Fact, true), !
	; assert(Fact)
	).

erase_file(AbsFileName) :-
	source_file(Mod:Pred, AbsFileName),
	functor(Pred, Predicate, Arity),
	abolish(Mod:Predicate/Arity),
%	dot,
	fail.
erase_file(_).

%%%%%%%  var_mem(+List, +Var)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  member for variables.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1: success: found Var in List
var_mem([V|_],Var) :- 
    V == Var, !.
% 2: first element is a variable but not the right one. looking
%    for Var in Rest.
var_mem([V|Rest],Var) :- 
    var(V), !, 
    var_mem(Rest,Var).
% 3: disjunction variables found. looking for Var in VarList.
var_mem([d(VarList)|_],Var) :- 
    var_mem(VarList,Var), !.
% 4: else, looking for Var in Rest.
var_mem([_|Rest],Var) :- 
    var_mem(Rest,Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spys :-
   current_spypoint(predicate(X)),
   format('~N~n~8|~w',[X]),
   fail.
spys :-
   ttyflush.

:- dynamic cuf_tables_initialised/0.

%% global initialization; done when saved state is loaded
global_cuf_init :-
	cuf_version,
	cuf_out(message, ['global initialization:']),
        ( cuf_tables_initialised ->
	      init_sat,
	      add_axioms,
	      show_loaded
	; assert(cuf_tables_initialised),
	  init_data_base,
	  init_symbol_table, dot,
	  init_type_table, dot,
	  init_declaration_tables, dot,
	  init_flags, dot,
	  init_counters, dot,
%%	init_sat, dot,  %% now done in: init_defaults
	  init_defaults,
	  init_grammar_conf   %% in gram_conf.pl
        ).
%	compile(system_ops).




init_data_base :-
	retractall('CUF_FLAG'(_,_)), dot,
	retractall('CUF_COUNTER'(_,_)), dot,
	retractall('LENGTH'(_,_)), dot,
	retractall('AMBIGOP'(_,_,_,_,_,_,_,_)), dot,
	retractall('INFIXOP'(_,_,_,_,_)), dot,
	retractall('PREFIXOP'(_,_,_,_)), dot,
	retractall('POSTFIXOP'(_,_,_,_)), dot,
	retractall('POLY'(_,_,_,_,_)), dot,
	retractall('rule counter'(_,_,_)), dot,
	retractall(cuf_task(_,_)), dot,
	retractall(symbol_table(_,_,_,_,_)), dot,
	retractall(type_code(_,_)), dot,
	retractall(added_axiom(_,_,_)), dot,
	retractall(file_table(_,_,_)), dot,
%	abolish(delay_pattern/3), dot,
	retractall(added_axiom(default,_,_)), dot,
	retractall(index_table(_,_,_)), dot,
	retractall_index_tables, dot,
	retractall(feat_decl(_,_,_,_,_)), dot,
	retractall(mono_feat(_, _, _)),  dot,
	retractall(poly_feat(_, _)), dot,
	retractall(feat_doms(_, _)), dot,
	retractall(poly_feat_check(_, _, _, _)), dot,
%	retractall('PARSER OUTPUT'(_,_,_,_)), dot,
	retractall('DEF OCC'(_,_,_,_,_)), dot,
	retractall('STRING_CONST'(_)), dot,
	retractall('USED OCC'(_,_,_,_,_)), dot,
	retractall('TYPE CONST'(_,_,_)), dot,
	retractall('SORT TYPE CONST'(_,_,_)), dot,
%	retractall(trans_output(_,_,_,_,_,_,_)), dot,
% 	retractall(entry_info(_,_,_,_,_,_)), dot,
	retractall(sort_decl(_,_,_,_,_)), dot,
	retractall('CUF clause'(_,_,_,_,_,_,_,_,_)), dot,
	retractall(added_axiom(_,_,_,_)), dot,
	retractall(formula_table(_)), dot,      
	retractall(flag_setting(_,_,_,_)), dot,
	retractall(control_file(_,_)), dot,
	retractall(solution(_,_)).

retractall_index_tables :-
	retract(index_table_name(_,_,Name,Arity)),
	abolish(Name/Arity),
	fail.
retractall_index_tables.

/*
yn_question(Question) :-
	repeat,
	cuf_out(message, Question),
	get(X),
	( ( X == 0'y ; X == 0'Y ), !
	; ( X == 0'n ; X == 0'N ), !, fail
	; cuf_out(message, ['Please answer with ''y'' or ''n''!']),
	  fail
	).
*/
yn_question(Question) :-
	repeat,
	cuf_out(message, Question),
	get0(X),
	( X == 10, !, fail % RETURN
	; get0(10), % RETURN
	  ( ( X == 0'y ; X == 0'Y ), !
	  ; ( X == 0'n ; X == 0'N ), !, fail
	  ; cuf_out(message, ['Please answer with ''y'', ''n'' or ''RET''!']),
	    fail
	  )
	).

for(X,X,X) :- !.
for(X,X,_).
for(X,U,O) :-
    U1 is U+1,
    for(X,U1,O).


%% forall(Goal, Condition)
%% test Condition for each solution of Goal

%:- meta_predicate forall(0,0).

forall(Goal, Condition) :-
	call((Goal, (Condition -> fail ; true) 
	      -> fail
	      ; true
	      )).



is_string(String) :-
   functor(String, '.',2),
   is_string_(String), !.

is_string_([]).
is_string_([C|R]) :-
   is_printable_char(C),
   is_string_(R).

is_printable_char(C) :-
   integer(C),
   32 =< C,
   C =< 126, !.

memb_nonvar(_,X) :- var(X), !, fail.
memb_nonvar(E,[E|_]).
memb_nonvar(E,[_|R]) :- memb_nonvar(E,R).

delete_nonvar(_,X,_) :- var(X), !, fail.
delete_nonvar(E,[E|R],R).
delete_nonvar(E,[X|R],[X|R1]) :- delete_nonvar(E,R,R1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reverse a list
%% rev(+List,?Reversed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%
rev(List, Reversed) :-
	rev(List, [], Reversed).

rev([], Reversed, Reversed).
rev([Head|Tail], Sofar, Reversed) :-
	rev(Tail, [Head|Sofar], Reversed).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cuf_term_variables(+Term, +Vars_i, -Vars)
%% binds Vars to the union of Vars_i and the variables which occur in 
%% CUFTerm, which is a <CUFTerm>, <CUFGoal>, or <CUFConstr> (no lists)
%% Vars_i must have been produced by cuf_term_variables and no unifications 
%% of parts of the terms thus scanned may have taken place thereafter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_term_variables(V, Vars_i, Vars) :- var(V), !,
    ( covered_var(Vars_i, V) -> Vars = Vars_i
    ; Vars = ['CUF VAR'(V)|Vars_i]
    ).
cuf_term_variables([TI|FS], Vars_i, Vars) :- !,
        get_type(TI,_,TVar),
        ( var(TVar) ->
              ( covered_var(Vars_i,TVar) -> Vars = Vars_i
              ; Vars1 = ['CUF VAR'(TVar,FVar)|Vars_i],
                cuf_term_variables_fs(FS,FVar,Vars1,Vars)
              )
        ; FS = [prolog(P)] ->
              cuf_pl_term_variables(P,Vars_i,Vars)
        ; Vars = Vars_i
        ).
cuf_term_variables(Goal, Vars_i, Vars) :-
        functor(Goal,_,Ar),
        cuf_term_variables(Ar, Goal, Vars_i, Vars).

cuf_term_variables(Ar, Goal) -->
    ( { Ar =:= 0 } -> []
    ; { arg(Ar, Goal, Arg), Ar1 is Ar-1 },
      cuf_term_variables(Arg),
      cuf_term_variables(Ar1, Goal)
    ).

cuf_term_variables_fs(Var,Var) -->
	{var(Var), !},
        [].
cuf_term_variables_fs([_:Val|R],FVar) -->
        cuf_term_variables(Val),
        cuf_term_variables_fs(R,FVar).

cuf_pl_term_variables(Term) --> 
        { nonvar(Term), !, 
          functor(Term,_,Ar)
        },
        cuf_pl_term_variables(Ar,Term).
cuf_pl_term_variables(Var,Vars_i,Vars) :-
        memq(Var,Vars_i), !,
        Vars = Vars_i.
cuf_pl_term_variables(Var,Vars_i,Vars) :-
	Vars = [Var|Vars_i].


cuf_pl_term_variables(Ar,Term) -->
    ( { Ar =:= 0 } -> []
    ; { arg(Ar, Term, Arg), Ar1 is Ar-1 },
      cuf_pl_term_variables(Arg),
      cuf_pl_term_variables(Ar1, Term)
    ).


memq(E, [E1|_]) :- E==E1, !.
memq(E, [_|R]) :- memq(E,R).

covered_var([H|T], Var) :-
    ( arg(1,H,H1), H1 == Var -> true
    ; covered_var(T, Var)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: output handling                               %%%
%%%      Created: Tue Dec  1 16:52:15 1992                      %%%
%%%     Modified: Fri Mar 15 21:07:06 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.12  1996/04/09 14:05:47  jochen
% some dead code removed (concern. flag graphic)
%
% Revision 1.11  1994/06/28 13:14:05  jochen
% goal_args_to_fs now generates open-ended list (bug fix for xmfed_output:yes)
%
% Revision 1.10  1994/06/28  11:25:48  junger
% - new otion for cuf_out: '$GOAL LIST$'(GoalList,Constraints)
%
% Revision 1.9  1994/05/18  16:58:55  michel
% anonymous bug fixing ... :)
%
% Revision 1.8  1994/05/18  16:10:08  michel
% ttynl replace by nl,ttyflush  for output redirection reasons
%
% Revision 1.7  1994/05/11  10:50:58  michel
% outl/3: suppressing of '$E$'(tmp,_) output
%
% Revision 1.6  1994/05/10  16:03:05  jochen
% xmfed_output now works also in kernel system (message_output/2 changed)
%
% Revision 1.5  1994/02/21  15:28:45  jochen
%  - ground_vars (new pred) used to ground (singleton) vars to _ in output.
%  - new output option '$TRACE GOAL$' whose output is like '$GOAL$' if
%    trace_structures=yes, but outputs only skeleton if not.
%
% Revision 1.4  1994/01/29  13:05:54  michel
% outl/3: '$GOAL$' case changed
%
% Revision 1.3  1993/11/24  17:25:29  michel
% cuf_out/2 for errors and warnings changed (bug fix)
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Fri Sep 24 16:24:33 1993 (michel) : message_output changed
% Wed Sep 22 14:39:16 1993 (jochen) : handling of GOAL BUNDLES added
% Tue Jul 27 15:56:32 1993 (jochen) : option '$GOAL$'(Goal) added to cuf_out
% Wed Jul 28 17:04:38 1993 (dorna) : tt_out/1 changed
% Tue Apr  6 22:27:02 1993 (michel) : pp_string/1 added
% Mon Feb  1 16:35:22 1993 (michel) : '$FS$'/2 added
% Mon Feb  1 16:03:49 1993 (michel) : '$CO$' (constraints) added
% Mon Dec  7 12:37:07 1992 (michel) : cnf2type_term/2, get_type/2 --> ppcuf.pl
% Mon Dec  7 11:08:25 1992 (michel) : outl/(1,2) --> outl/(2,3)
% Fri Dec  4 18:27:13 1992 (michel) : warning print now sensible to warnings flag
% Fri Dec  4 17:20:10 1992 (michel) : prep_output_vars/1 added
% Thu Dec  3 16:20:32 1992 (michel) : cuf_out/2 is now sensible to output flag
% Thu Dec  3 15:12:03 1992 (michel) : cnf2type_term/2, get_type/2 added
% Thu Dec  3 12:24:20 1992 (michel) : c/2 case in outl/2 added
% Thu Dec  3 11:44:33 1992 (michel) : dot/0 added
% Wed Dec  2 11:42:21 1992 (michel) : tt/1 case in outl/2 added

:- multifile error_output/2,
	     warning_output/2,
	     message_output/2,
	     entry_output/2,
	     misc_output/2.

% cuf_out(+Switch, +TokenList)
cuf_out(error, List) :- !,
        (inc_counter(errors,_,1) ; true),   % first branch for compiler
	error_output(ascii, List), !.
cuf_out(warning, List) :- !,
        (inc_counter(warnings,_,1) ; true), % first branch for compiler
	( get_flag(warnings, no) ->
	      true
	; warning_output(ascii, List)
	), !.
cuf_out(message, List) :- !,
	message_output(ascii, List), !.
cuf_out(entry, List) :- !,
	entry_output(ascii, List), !.
cuf_out(misc, Misc) :- !,
	misc_output(ascii, Misc), !.
cuf_out(Switch, _) :-
	outl(['cuf_out: unknown output switch',Switch,'$NL$'],0).

dot :-  cuf_out(misc, '.').

error_output(ascii, List) :-
	nl, nl, write('### CUF ERROR: '), ttyflush,
	outl(List, 0).

warning_output(ascii, List) :-
	nl, nl, write('%%% CUF WARNING: '), ttyflush,
	outl(List, 0).

message_output(ascii, List) :-
	nl, write('%%% '), ttyflush,
%%	get_flag(xmfed_output,Old),
%%	set_flag(xmfed_output,no),
	( outl(List, 0) ; true ).
%%	set_flag(xmfed_output,Old).

entry_output(ascii, [FID, EID]) :-
	entry_info(FID, EID, LineB, LineE, Entry,Vars),
	file_table(check, FName, _, FID, _),
	nl, write('%%% in file: '), write(FName),
	nl, write('%%% between lines: '), write(LineB-LineE),
	annotatedterm2prolog(Entry, Original),
	prep_output_vars(Vars),
	ground_vars(Original),
	nl, write('%%% clause: '), print(Original), nl, nl, ttyflush.

misc_output(ascii, Misc) :-
	write(Misc),ttyflush.

ground_vars('_') :- !.
ground_vars(A) :- atomic(A), !.
ground_vars(Term) :-
   Term =.. [_|Args],
   ground_vars_list(Args).

ground_vars_list([]).
ground_vars_list([F|R]) :-
   ground_vars(F),
   ground_vars_list(R).

prep_output_vars([]).
prep_output_vars([VarEq|R]) :-
	call(VarEq), !,
	prep_output_vars(R).

% outl(+List, +AbsoluteTab)
outl([], _) :- ttyflush.
outl([F|R], Tab) :-
	outl(F, R, Tab).

outl('$NL$', R, Tab) :- !,
	nl, ttyflush, tab(Tab),
	outl(R, Tab).
outl('$FTAB$'(Offset), R, OldTab) :- !,
	Tab is Offset - OldTab,
	tab(Tab),
	outl(R, Offset).
outl('$TAB$'(I), R, Tab) :- !,
	tab(I),
	outl(R, Tab).
outl('$FT$'(Feature_Term), R, Tab) :-
	ft_out(Feature_Term), !,
	outl(R, Tab).
outl('$FS$'(Feature_Structure), R, Tab) :-
	pp_fs(Feature_Structure), !,
	outl(R, Tab).
outl('$FS$'(Feature_Structure, Constraints), R, Tab) :-
	pp_fs(Feature_Structure, Constraints), !,
	outl(R, Tab).
outl('$GOAL$'('GOAL BUNDLE'(Name,[Res|Args])), R, Tab) :- !,
        length(Args,Ar),
	goal_args_to_fs(Args, 1, FeaturedArgs),
	outl([Name/Ar,'$NL$','$FS$'([_Type,'Result':Res|FeaturedArgs])|R],
	     Tab).
outl('$GOAL$'(Goal), R, Tab) :-
	Goal =.. [Name,Res|Args],
	goal_args_to_fs(Args, 1, FeaturedArgs),
	functor(Goal,Name,Arity), Ar is Arity-1,
	outl([Name/Ar,'$NL$','$FS$'([_Type,'Result':Res|FeaturedArgs])|R],
	     Tab).
outl('$TRACE GOAL$'('GOAL BUNDLE'(Name,[Res|Args])), R, Tab) :- !,
        length(Args,Ar),
	OutLine=[Name/Ar,'$NL$'|RestLine],
	( get_flag(trace_structures,no) ->
	      RestLine=R
	; goal_args_to_fs(Args, 1, FeaturedArgs),
	  RestLine=['$FS$'([_Type,'Result':Res|FeaturedArgs])|R]
	),
	outl(OutLine, Tab).
outl('$TRACE GOAL$'(Goal), R, Tab) :-
	functor(Goal,Name,Arity), Ar is Arity-1,
	OutLine=[Name/Ar,'$NL$'|RestLine],
	( get_flag(trace_structures,no) ->
	      RestLine=R
	; Goal =.. [Name,Res|Args],
	  goal_args_to_fs(Args, 1, FeaturedArgs),
	  RestLine=['$FS$'([_Type,'Result':Res|FeaturedArgs])|R]
	),
	outl(OutLine, Tab).
outl('$GOAL LIST$'(GoalList,Constraints), R, Tab) :-
	prepare_printing_goals(GoalList, Prep_GL),
	prepare_pp_cl(Constraints, Prep_CL),
	prefix(Prefix),
	get_printing_options(Options),
	print_body(Prep_GL,1, Options, Prefix, 1, _), !,
	print_const(Prep_CL, Prefix), !,
	outl(R, Tab).
outl('$TT$'(Type_Term), R, Tab) :-
	tt_out(Type_Term), !,
	outl(R, Tab).
outl('$CO$'(Constraints), R, Tab) :-
	constraints_out(Constraints), !,
	outl(R, Tab).
outl('$C$'(Head,Body), R, Tab) :-
	c_out(Head, Body), !,
	outl(R, Tab).
outl('$E$'(FID,EID), R, Tab) :- !,
     ( FID == tmp -> true
     ; entry_output(ascii, [FID,EID]),
       outl(R, Tab)
     ).
outl('$S$'(String), R, Tab) :- !,
	pp_string(String),
	outl(R, Tab).
outl(Token, R, Tab) :-
	write(Token), write(' '), !,
	outl(R, Tab).

constraints_out([]).
constraints_out([C|R]) :-
	pp_constr(C), nl, ttyflush,
	constraints_out(R).

pp_constr(C) :- write(C).

ft_out(FT) :- write(FT), write(' '), !.

tt_out(TT) :-
	cnf2type_term(TT, TTOut),
	write(TTOut), write(' '), !.

c_out(Head, SubGoalList) :-
	ft_out(Head), write(' :- '), nl,
	ft_list_out(SubGoalList), !.

ft_list_out([]) :- write('.').
ft_list_out([FT|R]) :-
	tab(5), ft_out(FT), write('&'), nl,
	ft_list_out(R).


pp_string([]) :- write('""'),!.
pp_string(String) :-
   write('"'),
   pp_string_(String).

pp_string_([F|R]) :-
   put(F),
   pp_string_(R).
pp_string_([]) :-
   write('"').



%% goal2fslist(Goal,OutList)
%% builds an output specification for cuf_out from a goal

% goal2fslist(Goal, [Name,'$NL$','$FS$'(_Type,'Result':Res|FeaturedArgs)) :-
% 	Goal =.. [Name,Res|Args],
% 	goal_args_to_fs(Args, 1, FeaturedArgs).
 
goal_args_to_fs([], _, _).
goal_args_to_fs([Arg|Rest], N, [ArgLabel:Arg|RestArgs]) :-
    N1 is N+1,
    concat('Arg',N, ArgLabel),
    goal_args_to_fs(Rest, N1, RestArgs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: compilation of type axioms                    %%%
%%%      Created: Fri Sep 18 11:00:57 1992                      %%%
%%%     Modified: Thu Jun 23 11:50:03 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1994/06/23 09:54:01  jochen
% New CNF conversion. No more exponential explosion.
% May generate new types (for embedded conjunctions).
%
% Revision 1.3  1994/01/14  10:23:29  jochen
% Use predicate negate_TID/2 (typed_unify.pl) for negating type IDs.
%
% Revision 1.2  1993/11/18  14:15:29  jochen
% check in 2.28d:
%


% History (before RCS):
% Mon Sep 27 17:53:19 1993 (michel) : ~ bottom case repaired
% Fri Sep 24 17:08:38 1993 (michel) : symbol_conversion1 changed
% Mon Aug 23 17:27:37 1993 (michel) : cnf_disjoints2axioms change by jochen
% Sun Aug  1 18:03:09 1993 (dorna) : symbol_table format changed
% Tue Apr  6 22:42:05 1993 (michel) : parser output format modified
% Tue Jan 26 16:36:12 1993 (michel) : cnf for '~' changed
% Wed Dec  2 14:26:31 1992 (michel) : error handling changed

compile_axioms(FID) :-
   axiom_exists(FID),
   'PARSER OUTPUT'(type_axiom, FID, EID, TAX),
   flatten_struc(TAX, FTAX),
   set_flag(entryID, EID),
   ( cnf0(FTAX, FID, EID) ->
	 dot
   ; cuf_out(error, ['compilation of axiom failed']),
     cuf_out(entry, [FID,EID])
   ),
   fail.
compile_axioms(_FID).

axiom_exists(FID) :-
   'PARSER OUTPUT'(type_axiom, FID, _, _),
   set_flag(axiom_system, new), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  cnf0(+Formula)
%%%  cnf0(+Formula, +FIleID, +EntryID)
%%%  brings formula in conjunctive normal form (not always minimal!!!).
%%%  adds it to the axiom system of the sat module.
%%%  formula should be in a format produced by flatten_struc/2!!!
cnf0(TAX, FID, EID) :-
	cnf(TAX, CNF), !,
	add_cnf(CNF, FID, EID).
cnf0(TAX) :-
	get_flag(fileID, FID),
	get_flag(entryID, EID),
	cnf0(TAX, FID, EID).
%%%  cnf(+Formula, -ConjunctiveNormalForm)
%%%  brings formula in conjunctive normal form (not always minimal!!!).
%%%  formula should be in a format produced by flatten_struc/2!!!
cnf('<'(Ant, Con), CsList) :- !,
    cnf_prep(Ant, AntP),
    cnf_prep(Con, ConP),
    flatten_struc(';'('~'(AntP),ConP), Flat),
    cnf(Flat, CsList).
cnf('='(Eq1, Eq2), CsList) :- !,
	cnf_prep(Eq1, Eq1P),
	cnf_prep(Eq2, Eq2P),
	flatten_struc('&'(';'('~'(Eq1P),Eq2P),';'('~'(Eq2P),Eq1P)), Flat),
	cnf(Flat, CsList).
cnf('Disjs'(Parts), CsList) :- !,
    cnf_disj(Parts, CsList).
cnf(F, CNF) :-
	nnf(F, 1, NNF),
%%	setof(Clause, clauseFromNNF(NNF, 0, _, Clause, []), CNF).
%%	nnf2cnf([[NNF]], CNF, []).
	( NNF = '&'(Cs) ->
	      conj2clauses(Cs, ClauseList),
	      nnf2cnf_careful(ClauseList, CNF, [])
	; nnf2cnf_careful([[NNF]], CNF, [])).

nnf('&'(Cs), Pol, NNF) :- !,
    ( Pol =:= 1 ->
	  nnf_and(Cs, Pol, NNFlist, []),
	  NNF = '&'(NNFlist)
    ; nnf_or(Cs, Pol, NNFlist, []),
      NNF = ';'(NNFlist)).
nnf(';'(Ds), Pol, NNF) :- !,
    ( Pol =:= 0 ->
	  nnf_and(Ds, Pol, NNFlist, []),
	  NNF = '&'(NNFlist)
    ; nnf_or(Ds, Pol, NNFlist, []),
      NNF = ';'(NNFlist)).
nnf('~'(Neg), Pol, NNF) :- !,
    Pol1 is 1-Pol,
    nnf(Neg, Pol1, NNF).
nnf(Atom, 1, Atom) :- !.
nnf(Atom, 0, '~'(Atom)).


nnf_and([], _) --> [].
nnf_and([C|Cs], Pol) -->
	{ nnf(C, Pol, NNF) },
	( { NNF = '&'(NNFlist) } ->
	      NNFlist
	; [NNF]
	),
	nnf_and(Cs, Pol).

nnf_or([], _) --> [].
nnf_or([C|Cs], Pol) -->
	{ nnf(C, Pol, NNF) },
	( { NNF = ';'(NNFlist) } ->
	      NNFlist
	; [NNF]
	),
	nnf_or(Cs, Pol).


conj2clauses([], []).
conj2clauses([F|R], [[F]|RR]) :-
	conj2clauses(R, RR).


nnf2cnf_careful([]) --> [].
nnf2cnf_careful([PClause|R]) -->
	nnf2cnf_careful(PClause, [], 0, NewClause),
	[NewClause],
	nnf2cnf_careful(R).

nnf2cnf_careful([], Clause, _Splitted, Clause) --> [].
nnf2cnf_careful([F|R], SoFar, Splitted, Clause) -->
	nnf2cnf_careful_(F, R, SoFar, Splitted, Clause).

nnf2cnf_careful_('&'(Cs), R, SoFar, 0, Clause) --> !,
	nnf2cnf_careful(R, SoFar, 1, ClauseR),
	{ conj2clauses(Cs, ClauseList),
	  nnf2cnf_careful(ClauseList, CNF1, []),
	  combine_with_clause(CNF1, ClauseR, [Clause|Rest]) },
	Rest.
nnf2cnf_careful_('&'(Cs), R, SoFar, 1, Clause) --> !, 
	{ gen_new_type(Type),
	  conj2clauses(Cs, ClauseList),
	  nnf2cnf_careful(ClauseList, CNF1, []),
	  combine_with_clause(CNF1, [-Type], CNF2) },
	CNF2,    %% corresp. ~Type ; (&(Cs))
	nnf2cnf_careful(R, [Type|SoFar], 1, Clause).
nnf2cnf_careful_(';'(Ds), R, SoFar, Splitted, Clause) --> !,
	{ append(Ds, R, DsR) },
	nnf2cnf_careful(DsR, SoFar, Splitted, Clause).
nnf2cnf_careful_('~'(Atom), R, SoFar, Splitted, Clause) --> !,
	nnf2cnf_careful(R, [-Atom|SoFar], Splitted, Clause).
nnf2cnf_careful_(Atom, R, SoFar, Splitted, Clause) -->
	nnf2cnf_careful(R, [Atom|SoFar], Splitted, Clause).

combine_with_clause([], _, []).
combine_with_clause([F|R], Clause, [NewF|NewR]) :-
	append(F, Clause, NewF),
	combine_with_clause(R, Clause, NewR).




%%%  cnf_disj(+ListOfDisjoints, -Conjunctions)
cnf_disj(Disjoints, CNF) :-
    cnf_disjoints2axioms(Disjoints, CNF, []).

/* JD: caused exponential behaviour, changed
%%%  cnf_disjoints2axioms(+ListOfDisjoints, -CNF, [])
%%%  builds disjoint pairs and converts them to cnf
cnf_disjoints2axioms([D1,D2]) --> !,
    { flatten_struc('~'('&'(D1,D2)), Flat),
      cnf(Flat, CNF) }, CNF.
cnf_disjoints2axioms([D1,D2|Ds]) -->
    { flatten_struc('~'('&'(D1,D2)), Flat),
      cnf(Flat, CNF) }, 
    CNF, !,
    cnf_disjoints2axioms([D1|Ds]), 
    cnf_disjoints2axioms([D2|Ds]).
*/

%%%  cnf_disjoints2axioms(+ListOfDisjoints, -CNF, [])
%%%  builds disjoint pairs and converts them to cnf
cnf_disjoints2axioms([]) --> [].
cnf_disjoints2axioms([D|Ds]) -->
    cnf_disjoints2axioms_(Ds,D), 
    cnf_disjoints2axioms(Ds).

cnf_disjoints2axioms_([],_) --> [].
cnf_disjoints2axioms_([D1|Ds],D) -->
    { flatten_struc('~'('&'(D,D1)), Flat),
      cnf(Flat, CNF) }, 
    CNF,                  %% or simply [[-D,-D1]]
    cnf_disjoints2axioms_(Ds,D).

%%%  cnf_prep(+Formula, -Formula)
%%%  replaces disjointness expressions by disjunctions
%%%  adds disjointness axioms
cnf_prep(Form, Out) :-
    ( Form =.. ['Disjs', Ds] ->
	  prep_disj(Ds, SDs), !,
	  cnf0('Disjs'(SDs)),	    % compute disjointness axioms
	  Out = ';'(SDs)
    ; Out = Form
    ).

%%%  prep_disj(+Disjoints, -NewDisjoints)
prep_disj(Ds, SDs) :-
	prep_disjs1(Ds, DsP),
	sort(DsP, SDs), !.

%%%  prep_disjs1(+Disjoints, -NewDisjoints)
%%%  - disjunctions will be replaced by new symbols
%%%  - adds new axioms: equalities of replaced disjunctions and 
%%%    new symbols resp. 
prep_disjs1([], []).
prep_disjs1([D|R], [DP|RP]) :-
	( D =.. [';',Args] ->     % is disjoint a disjunction?
	      ( Args = [A] ->     % disjunction list of one element only
		    DP = A
	      ; gen_new_type(Type),
		cnf0('='(Type,D)), !,
		DP = Type
	      )
	; D = DP                  % conjunction and negation case
	),
	prep_disjs1(R, RP).

%%%  gen_new_type(-NewTypeSymbol)
gen_new_type(Type) :-
	gensym(genType, Type),
	symbol_table(assert, Type, 0, _, -, type, ok), !.

%%%  add_cnf(+Axiom)
%%%  interface to sat module, adds axiom
add_cnf(Ax, FID, EID) :-
	symbol_conversion(Ax, CNFList),
	( CNFList = [I], integer(I), I < 0 ->
	      SCNFList = [[I]]
	; sort(CNFList, SCNFList)
	),
	sat(add_axioms, SCNFList, FID, EID).

%%%  recursive_sorting(+Formula, -SortedFormula)
%%%  recursive sorting (to eliminate duplicates only)
recursive_sorting(L, SL) :-
	recursive_sorting1(L, SL0),
	sort(SL0, SL).

recursive_sorting1([], []).
recursive_sorting1([F|R], [SF|SR]) :-
	sort(F, SF),
	recursive_sorting1(R, SR).


% symbol_conversion(+CNFList, -CNFList)
% conversion from symbols to type IDs and constant IDs resp. incl. sorting
symbol_conversion([], []).
symbol_conversion([[top]], [[1,-1]]) :- !.
symbol_conversion([[bottom]], [[1],[-1]]) :- !.
symbol_conversion([F|R], [SF|CR]) :-
	symbol_conversion1(F, CF), 
	sort(CF, SF), !,
	symbol_conversion(R, CR).

symbol_conversion1([], []) :- !.
symbol_conversion1([F|R], COut) :-
	( F == -top -> CF = []
	; ( F = -Sym ->
		( Sym == bottom -> CF = [1,-1]
		; is_type(Sym, TID) ->
		      negate_TID(TID,CF0), CF=[CF0]
		; is_const(Sym, TID) ->
		      negate_TID(TID,CF0), CF=[CF0]
		)
	  )
	; F == top -> CF = [1,-1]
	; F == bottom -> CF = []
	; is_type(F, CF0) -> CF=[CF0]
	; is_const(F, CF0) -> CF=[CF0]
	; cuf_out(error, ['unknown type or constant:',F]),
	  !, fail 
	),
	append(CF,CR,COut),
	symbol_conversion1(R, CR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: compiling CUF clauses                         %%%
%%%      Created: Fri Sep 18 11:21:23 1992                      %%%
%%%     Modified: Fri Mar 15 22:02:01 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.13  1996/04/09 13:00:32  jochen
% module prefix for trans_output facts;
% transform_negations speeded up;
% code rearrangement (output param.)
%
% Revision 1.12  1995/02/01 14:28:33  jochen
%  - now handles !-terms containing variables properly
%  - !-terms in negations allowed now; these generate 'DIFF' constraints
%  (bound/1 terms eliminated (cleanup))
%
% Revision 1.11  1994/09/12  15:08:25  michel
% member4vars/2 bug fixed and replaced by a new definition
%
% Revision 1.10  1994/07/01  13:26:53  jochen
% internal format for !-terms now has type prolog
%
% Revision 1.9  1994/05/25  09:39:00  michel
% transform_negations/2, transform_negations1/2:
% * !/1 cases added (bug fix)
% * some cuts added to make it more robust against backtracking from outside
%
% Revision 1.8  1994/05/16  16:02:31  michel
% trans_rhs/9:
% * new '$true$'/1 built-in sort
% * '!'/1 for Prolog terms
%
% Revision 1.7  1994/05/11  12:52:40  michel
% assert_disjunct/5: warns if a disjunction branch fails constraint
% 		   checking (failed formerly; bug fix)
%
% Revision 1.6  1994/03/16  12:35:56  michel
% new predicate flat_disj/3:
% 	collects a list of disjuncts
% is_type_term/3 replaced by is_type_term/2:
% 	tests whether the feature term is a type term,
% 	removes annotations ('N'/1, 'S'/1)
% transform_negations1/2, 4th clause:
% 	checking mono-/polyfeature;
%     	generating no disjunction, if possible;
% 	'NONE'/1's argument is the negated domain now
% 	(was: feature symbol)
% collect_variable_occurences/3, ';'/2 case:
% 	new treatment of disjunctions in the compiler (see below)
% trans_rhs/9, ';'/2 case:
% 	A disjunction is no longer treated as a binary relation because this
%         causes the compiler to generate more clauses as necessary. The new
%  	compilation flattens down the (possibly) nested disjunctions and treats
% 	these as an N-ary disjunction relation (where N is the number of
% 	disjuncts).
% new predicate trans_rhs_disj/6:
% 	translates a list of disjunctions
% new predicate assert_disjuncts/4 calls assert_disjunct/5 now:
% 	asserts generated clause for each disjunct, resp.
% trans_rhs/9, '~' cases reduced to one clause
% new predicate trans_rhs_neg/9 for the different cases of negations
% trans_rhs/9, 'NONE' case changed: see above
% rest/3 removed
%
% Revision 1.5  1994/02/18  12:26:50  michel
% trans_rhs/9, 2nd ';'/2 case:
% generated disjunctions occurences mark the FID/EID of the origin entry
% now (former FID/tmp).
% NB: Deterministic expansion during compile time didn't work correctly
%     with the old IDs, i.e. tmp.
%
% Revision 1.4  1994/01/18  12:21:36  jochen
% 'N' and 'S' cases in trans_rhs/7 simplified (is_const now works also for
% implicitly type stuff).
%
% Revision 1.3  1994/01/14  10:26:54  jochen
% String and number handling adapted to new reader and internal format.
%
% Revision 1.2  1993/11/18  14:15:40  jochen
% check in 2.28d:
%


% History (before RCS):
% Mon Sep 27 18:07:37 1993 (michel) : transform_negations1/2: top/bottom case in last clause
% Mon Sep 27 17:27:03 1993 (michel) : trans_rhs: ~ bottom case repaired
% Mon Sep 13 16:51:19 1993 (michel) : check_POLY_sat/1 added
% Sun Aug  1 17:42:26 1993 (dorna) : disjunctive type term case added
% Sat Jul 31 19:03:57 1993 (dorna) : transform_negations1/2 changed
% Tue Apr 20 15:58:21 1993 (michel) : delayed_unify for variables
% Tue Apr  6 22:44:08 1993 (michel) : parser output format modified
% Mon Mar 22 13:44:10 1993 (michel) : some delayed unifies removed
% Mon Mar 22 13:43:32 1993 (michel) : type_inference/2 added
% Mon Feb 15 15:02:50 1993 (michel) : disjunction compilation changed
% Thu Feb  4 16:41:35 1993 (michel) : negation code changed
% Mon Feb  1 18:14:23 1993 (michel) : negation bug repared
% Mon Feb  1 16:32:23 1993 (michel) : negated constant -> _
% Mon Dec 21 18:26:16 1992 (michel) : bug in trans_rhs (7th clause) eliminated
% Mon Dec 14 19:22:58 1992 (michel) : separate constraint list for
%                                     DIFFs and POLYs --> 2 additional
%                                     arguments for trans_lhs, trans_rhs,
%                                     and trans_args
% Thu Dec  3 12:51:38 1992 (michel) : warning_handling/0 removed
% Wed Dec  2 14:35:16 1992 (michel) : error handling changed
% Mon Nov  9 11:52:42 1992 (michel) : polyfeature compilation changed
% Mon Nov  9 10:11:36 1992 (michel) : some modifications
% Fri Oct 30 15:09:53 1992 (michel) : clause format changed

%%:- dynamic trans_output/7. now in file-specific modules

trans_output(Pred, Arity, Head, Body, Cons, FID, EID) :-
    clause(FID:trans_output(Pred, Arity, Head, Body, Cons, EID), true).

compile_clauses(FID) :- 
    'PARSER OUTPUT'(clause, FID, EID, (LHS,RHS)),
    set_flag(entryID, EID),
    entry_info(FID, EID, _, _, _, Vars),
    ( Vars == [] -> VFlag = no ; VFlag = yes ),
    ( trans(LHS, RHS, Pred/Arity, Head, Body, Cons, VFlag) ->
	  assertz(FID:trans_output(Pred, Arity, Head, Body, Cons, EID)),
	  dot
    ; cuf_out(error, ['compilation failed (empty denotation)',
                      '$E$'(FID,EID)])
    ),
    fail.
compile_clauses(_FID).

%%%  trans(+CUF-Clause-LHS, +CUF-Clause-RHS, -HeadName/Arity, 
%%%        -NewHead, -NewBody, -Constraints, +VariableFlag)
trans(LHS0, RHS0, Name/Ar, Head, Body, ConsOut, VFlag) :-
	transform_negations(LHS0, LHS),
	transform_negations(RHS0, RHS),
	( VFlag ==  yes -> disj_var_list(LHS, RHS, VarList)
	; VarList = []
	),
	trans_lhs(LHS, Name/Ar, Head, Result, VarList, RestVars,
		  [], Cons, Goals, Goals1), !,
	trans_rhs(RHS, Result, _, RestVars, _, Cons, Cons1, Goals1, []), !,
	do_delayed_unify(Goals, Body),
	check_constraints(Cons1, ConsOut), 
	check_POLY_sat(ConsOut), !.

%%%  transform_negations(+FeatureTerm, -TransformedFeatureTerm)
%%%  transformation of negated feature terms to bring the negation
%%%  in front of innermost feature terms (types, constants or variables)
%%%  sorts must not occur in the scope of a negation operator!
%%%  transformation is assumed to be logically equivalent!
transform_negations('~'(Arg), Output) :- !,
	transform_negations1(Arg, Output).
transform_negations('V'(Var), Output) :- !, Output = 'V'(Var).
transform_negations('S'(String), Output) :- !, Output = 'S'(String).
transform_negations('N'(Number), Output) :- !, Output = 'N'(Number).
transform_negations('!'(Prolog), Output) :- !, Output = '!'(Prolog).
transform_negations(Term, Output) :-
	functor(Term,Func,Ar),
	functor(Output,Func,Ar),
	transform_negations(Ar, Term, Output).

transform_negations(Ar, Term, Out) :-
	( Ar =:= 0 -> true
	; arg(Ar,Term,Arg),
	  arg(Ar,Out,OutArg),
	  transform_negations(Arg,OutArg),
	  Ar1 is Ar-1,
	  transform_negations(Ar1, Term, Out)
	).

%%%  transform_negations1
%%%  negation found, further proceeding
transform_negations1('~'(Arg), Output) :- !,
	transform_negations(Arg, Output).
transform_negations1('&'(Arg1,Arg2), Output) :- !,
	Output = ';'(Out1,Out2),
	transform_negations('~'(Arg1), Out1),
	transform_negations('~'(Arg2), Out2).
transform_negations1(';'(Arg1,Arg2), Output) :- !,
	Output = '&'(Out1,Out2),
        transform_negations('~'(Arg1), Out1),
	transform_negations('~'(Arg2), Out2).
transform_negations1(':'(Feat,V), Out) :- !,
	( is_monofeature(Feat, Dom, _) ->
	      ( var(Dom) /* top */ -> Out = Feat:OutV
	      ; Out = ';'(Feat:OutV, 'NONE'(None)),
		neg_cnf2cnf(Dom, None)
	      )
	; Out = ';'(Feat:OutV, 'NONE'(None)),
	  feat_doms(Feat, Doms),
	  neg_cnf2cnf(Doms, None)
	),
	transform_negations1(V, OutV).
transform_negations1('V'(Var), Output) :- ! , Output = '~'('V'(Var)).
transform_negations1('S'(String), Output) :- ! , Output = '~'('S'(String)).
transform_negations1('N'(Number), Output) :- !, Output = '~'('N'(Number)).
transform_negations1('!'(Prolog), Output) :- ! , Output = '~'('!'(Prolog)).
transform_negations1(Term, Output) :-
	( functor(Term, Name, Ar), is_sort(Name, Ar) ->
	  cuf_out(error, ['sort in scope of a negation:',Name/Ar]),
	  !, fail
	; Term == top -> Output = bottom
	; Term == bottom -> Output = top
	; atomic(Term) -> Output = '~'(Term)
	).



%%%%%%  DISJUNCTION VARIABLE HANDLING

%%%  comp_disj_var_list(+Head, +Body, -DisjVarList)
%%%  if Head or Body contains disjunctions, DisjVarList will give back
%%%  the list of non-local variables of each disjunction respectivly in
%%%  order of appearence (top-down, left-to-right).
disj_var_list(Head, Body, DisjVarList) :-
    collect_variable_occurences(Head, OccListHead, []),
    collect_variable_occurences(Body, OccListBody, []),
    compute_variable_scopes(OccListHead, OccListBody, DisjVarList, 
			    DisjVarListHead),
    compute_variable_scopes(OccListBody, OccListHead, DisjVarListHead, []), !.

%%%  collect_variable_occurences(+Term, -VariableList, +Rest)
%%%  collect variables in CUF terms. Rest should be '[]'.
% 1: empty variable list; finish.
collect_variable_occurences([]) --> !, [].
% 2: Term is a variable; put it in the list.
collect_variable_occurences('V'(V)) --> !, ['V'(V)].
% 3: string or number; finish.
collect_variable_occurences('S'(_)) --> !, [].
collect_variable_occurences('N'(_)) --> !, [].
% 4: nonempty list; split up in first and rest.
collect_variable_occurences([F|R]) --> !,
    collect_variable_occurences(F),
    collect_variable_occurences(R).
% 5: Term is a disjunction; treat it as PROLOG conjunction (calling 5),
%    mark the variables occuring in the disjunction with d/1 and put
%    d/1 in the variable list.
collect_variable_occurences(';'(A,B)) --> !,
    { flat_disj(';'(A,B), Flat, []),
      collect_variable_occurences(Flat,VL,[]) },
    ['D'(VL)].
% 6: !-terms: collect and wrap with 'V'/1
collect_variable_occurences('!'(PTerm)) --> !,
    collect_variable_occurences_pterm(PTerm).
% 7: Term is a conjunction or a sort; proceed with argument list.
collect_variable_occurences(F) -->
    { functor(F,_,Ar) },
    collect_variable_occurences(1,Ar,F).

collect_variable_occurences(ArgNo,Ar,T) -->
    ( {ArgNo > Ar} -> []
    ; {arg(ArgNo,T,TA), ArgNo1 is ArgNo+1},
      collect_variable_occurences(TA),
      collect_variable_occurences(ArgNo1,Ar,T)
    ).


collect_variable_occurences_pterm(V) --> {var(V), !}, ['V'(V)].
collect_variable_occurences_pterm(T) -->
    { functor(T,_,Ar) },
    collect_variable_occurences_pterm_args(Ar,T).

collect_variable_occurences_pterm_args(Ar,T) -->
    ( {Ar =:= 0} -> []
    ; { arg(Ar,T,TA), Ar1 is Ar-1},
      collect_variable_occurences_pterm(TA),
      collect_variable_occurences_pterm_args(Ar1,T)
    ).

flat_disj(';'(X,Y)) --> !,
    flat_disj(X),
    flat_disj(Y).
flat_disj(X) --> [X].

%%%  compute_variable_scopes(+VarList1, +VarList2, -DisjVarList, ?Rest)
%%%  computes variable scopes of variables used in disjunctions.
%%%  detects non-local variables and puts them in DisjVarList in order of
%%%  appearence in VarList1 (top-down, left-to-right).
% 1: no variables no more
compute_variable_scopes([], _) --> [].
compute_variable_scopes([F|R], OutSideVars) -->
	compute_variable_scopes(F, R, OutSideVars).

% 2: no disjunction in first position but variable Var found. add Var
%    to OutSideVars and proceed with Rest.
compute_variable_scopes('V'(V), Rest, OutSideVars) -->
    compute_variable_scopes(Rest, ['V'(V)|OutSideVars]).
% 3: variables in disjunction are marked within 'D'/1. first take
%    DisjVarList and test for intersection with OutSideVars. then
%    detecting next disjunction in recursion.
compute_variable_scopes('D'(DisjVarList), Rest, OutSideVars) -->
    { append(Rest, OutSideVars, NewOutSideVars),
      intersect_vars(DisjVarList, NewOutSideVars, InterVars, []),
      sort(InterVars, InterVars_),             % remove duplicates
      append(DisjVarList, Rest, VarList) },
    [InterVars_],
    compute_variable_scopes(VarList, OutSideVars).

%%%  intersect_vars(+VarList1, +VarList2, -CommonVars)
%%%  collects variables occuring in both VarList1 and VarList2.
% 1: no variables no more
intersect_vars([],_) --> [].
intersect_vars([F|R], VarList) -->
	intersect_vars(F, R, VarList).

intersect_vars('V'(Var), Rest, VarList) -->
    { member4vars(VarList, 'V'(Var)) }, 
% 2: variable Var in intersection. looking forward in Rest.
    !, ['V'(Var)],
    intersect_vars(Rest, VarList)
% 3: variable Var not in intersection. looking forward in Rest.
.%%RB  | intersect_vars(Rest, VarList).
% 4: DisjVarList found. concatenate DisjVarList with rest list Rest
%    and go in recursion.
intersect_vars('D'(DisjVarList), Rest, VarList) -->
    { append(DisjVarList, Rest, VarList1) },
    intersect_vars(VarList1, VarList).


member4vars([F|Rest], Var) :-
    ( F == Var
    ; var(F) -> member4vars(Rest, Var)
    ; functor(F,'D',1), arg(1,F,VarList), member4vars(VarList, Var)
    ; member4vars(Rest, Var)
    ), !.

%%%%%%%  PROGRAM TRANSFORMATION

%%%  trans_lhs(+CUFClauseHead, -Name/Arity, -NewHead, ?ResultVar,
%%%            +DisjVarList, -DisjVarListRest, +ConstraintsIn,
%%%            -ConstraintsOut, -Goals,  -GoalsR)
trans_lhs(CHead, Name/Arity, NewHead, Result, DisjVarList,
	   DisjVarListRest, ConsIn, ConsOut) --> 
    { functor(CHead,Name,Ar),
      ( Ar == 0,
        ( is_basic_type(Name, _) ->
	      cuf_out(error, ['redefinition of built-in type:',Name]),
	      !, fail
	; is_defined_type(Name, _) ->
	      cuf_out(error, ['redefinition of defined type:',Name]),
	      !, fail
	)
      ; is_sort(Name, Ar),
	( sort_decl_table(check, Name, Ar, [Decl|DeclList], _, _, known) ->
	      type_inference(Result, Decl)
	; true )
      ),
      % construct new head with arity+1, first argument will be the
      % "result". 
      Arity is Ar + 1,
      functor(NewHead,Name,Arity),
      arg(1, NewHead, Result)
    },
    % proceed with the arguments
    trans_args(1, Arity, DeclList, NewHead, CHead, DisjVarList,
	       DisjVarListRest, ConsIn, ConsOut), !.

%%%  trans_args(+ArgNo, +Arity, +ArgDecls, +NewHead, +OldHead, 
%%%             +DisjVarList, -DisjVarListR, +ConstraintsIn,
%%%             -ConstraintsOut, -Goals,  -GoalsR)
%%%  argument transformation, constructs new body goals sucessively
% ArgNo = Arity, finish.
trans_args(ArgNo, ArgNo, _, _, _, DisjVarList, DisjVarListR, Cons, ConsR) -->
	!, [],
        { DisjVarListR = DisjVarList, ConsR = Cons }.
trans_args(ArgNo, Arity, [Decl|Decls], NewHead, OldHead, DisjVarList,
           DisjVarListRest, Cons, ConsR) -->
    % take new variable Var and CUF subterm from old head
    { ArgNo1 is ArgNo+1, 
      arg(ArgNo1, NewHead, Var),
      arg(ArgNo, OldHead, SubTerm)
    },
    % construct new body goals
    trans_rhs(SubTerm, Var, Decl, DisjVarList, DisjVarList1, Cons, Cons1),
    % proceed with next argument position
    trans_args(ArgNo1, Arity, Decls, NewHead, OldHead, DisjVarList1, 
	       DisjVarListRest, Cons1, ConsR).

%%%  trans_rhs(+CUFTerm, +Var, +Decl, +DisjVarList, -DisjVarListRest, -Goals,
%%%             -GoalsRest)
%%%  construction of body goals from CUF terms
% 1: term is (was) a variable
trans_rhs('V'(V), Var, Decl, DList, DListR, Cs, CsR) --> !,
	{ DListR = DList, CsR = Cs,
          type_inference(Var, Decl) },
	[delayed_unify(V, Var)].
% 2: term is a string or a number
trans_rhs('S'(String), Var, Decl, DisjVarList, DisjVarListR, Cons, ConsR) --> !,
	{ DisjVarListR = DisjVarList, ConsR = Cons,
          type_inference(Var, Decl),
	  is_const(String,TID),    %% must succeed
	  unify(Var, [[[[TID]]], String]) }.
trans_rhs('N'(Number), Var, Decl, DisjVarList, DisjVarListR, Cons, ConsR) --> !,
	{ DisjVarListR = DisjVarList, ConsR = Cons,
          type_inference(Var, Decl),
	  is_const(Number,TID),    %% must succeed
	  unify(Var, [[[[TID]]], Number]) }.
% 3: term is a conjunction
trans_rhs('&'(TermA,TermB), Var, Decl, DList, DListRest, CsIn, CsOut) --> !,
    trans_rhs(TermA, Var, Decl, DList, DList1, CsIn, Cs1),
    trans_rhs(TermB, Var, Decl, DList1, DListRest, Cs1, CsOut).
% 4: term is a disjunction, DisjVars contains non-local variables of
%    current disjunction, generate new call. asserts new clause for each
%    disjunct respectively.
trans_rhs(';'(TermA,TermB), Var, Decl, DList, DListRest, Cs, CsR) -->
	{ ( DList == [] -> DListRest = []
	  ; DList = [[]|DListRest]   % checks, if disjunction contains variables
	  ),
	  is_type_term(';'(TermA,TermB),';'(TermC,TermD)), !,
          CsR = Cs,
	  type_inference(Var, Decl),
	  flatten_struc(';'(TermC,TermD), FTT),
	  cnf(FTT, CNF),
	  symbol_conversion(CNF, CNFList),
	  type_inference(Var, CNFList) }.
trans_rhs(';'(TermA,TermB), Var, Decl, DList, DListRest, CsIn, CsOut) --> !,
	{ ( DList == [] -> DisjVars = [], DisjVarList = []
	  ; DList = [DisjVars|DisjVarList]
	  ),
	  % flat the disjunction
	  flat_disj(';'(TermA,TermB), List, []),
	  % remove duplicates: (esp. useful for 'NONE's)
	  sort(List, SList),
	  % proceed with the disjuncts:
	  trans_rhs_disj(SList, VarH, Decl, DisjVarList, DListRest, Clauses),
	  ( Clauses == [] -> fail
	  ; Clauses = [disj(Goals,Cons)] ->
		unify(Var,VarH),
		DisjCall = Goals,
		append(CsIn, Cons, CsOut)
	  ; CsIn=CsOut,
	    % generate new predicate:
	    gensym(genDisj,DisjName),
	    length(DisjVars,DisjArity0),
	    DisjArity is DisjArity0+1,
	    get_flag(fileID, FID),
	    get_flag(entryID, EID),
	    add_def_symbol(DisjName, DisjArity0, sort, FID, EID),
	    def_occ_table(DisjName, DisjArity0, sort, FID, EID),
	    used_occ_table(DisjName, DisjArity0, sort, FID, EID), !,
	    annotatedterm2prolog(DisjVars, TransVars),
	    DisjCall0   =.. [DisjName,Var|TransVars],
	    DisjHead =.. [DisjName,VarH|TransVars],
	    DisjCall = [DisjCall0],
	    % add the generated clauses:
	    assert_disjuncts(Clauses, DisjHead, DisjName, DisjArity), !
	  )  }, 	DisjCall.
% 5: term is a selection
% a: monofeature
trans_rhs(':'(Feat,Term), Var, Decl, DList, DListRest, CsIn, CsOut) -->
	{ is_monofeature(Feat, Dom, Ran), !,
	  type_inference(Var, Decl),
	  type_inference(Var, Dom),
	% unify Var with a new graph and assert Feat
	  unify([_,Feat:Var1|_], Var) }, !,
	% proceed with selected term
	trans_rhs(Term, Var1, Ran, DList, DListRest,
		  CsIn, CsOut).
% b: polyfeature
trans_rhs(':'(Feat,Term), Var, Decl, DList, DListRest, CsIn, CsOut) --> !,
	{ type_inference(Var, Decl),
	  clause(feat_doms(Feat, Doms), true),
	  type_inference(Var, Doms),
          unify(Var,[Dom,Feat:[Ran|Var1]|_]), !,
	  clause(poly_feat_check(Feat, Dom, Ran, Cons), true),
	  append(CsIn, Cons, Cs1)
	}, !,
	trans_rhs(Term, [Ran|Var1], _, DList, DListRest,
		  Cs1, CsOut).
% 6: term is a negation
trans_rhs('~'(NegTerm), Var, Decl, DList, DListRest, CsIn, CsOut) --> !, [],
	trans_rhs_neg(NegTerm, Var, Decl, DList, DListRest, CsIn, CsOut).
% 7: term is part of a disjunction, 'NONE'/1 was generated in
%    negation transformation part above
trans_rhs('NONE'(None), Var, Decl, DisjVarList, DisjVarListR, Cons, ConsR) -->
	!, [],
	{ DisjVarListR = DisjVarList, ConsR = Cons,
          type_inference(Var,  Decl),
	  unify([[None|_]|_],Var) }.
% 8: term is sort '$true$'/1
trans_rhs('$true$'(Term), _, _, DList, DListRest, CsIn, CsOut) --> !,
   trans_rhs(Term, _, _, DList, DListRest, CsIn, CsOut).
% 9: term is prolog term
trans_rhs('!'(Prolog), Var, Decl, DisjVarList, DisjVarListR, Cons, ConsR) -->
	!, [],
	{ DisjVarListR = DisjVarList, ConsR = Cons,
          type_inference(Var,  Decl),
          is_type(prolog,TID),
	  unify(Var, [[[[TID]]],prolog(Prolog)]) }.
% 10: term is constant, type or sort with arity zero
trans_rhs(Term, Var, Decl, DisjVarList, DisjVarListR, Cons, ConsR,
          GoalsIn, GoalsOut) :-
	atomic(Term), !,
	DisjVarListR = DisjVarList, ConsR = Cons,
	type_inference(Var, Decl),
	( is_const(Term, TID) ->
	      unify([[[[TID]]],Term], Var),
	      GoalsIn = GoalsOut
	; is_type(Term, TID) ->
	      ( var(TID) -> true
	      ; TID == [] -> fail
	      ; type_inference(Var, [[TID]])
	      ),
	      GoalsIn = GoalsOut
	; is_defined_sort(Term, 0) ->
	      ( sort_decl_table(check, Term, 0, [Decl], _, _, known) ->
		  type_inference(Var, Decl)
	      ; true ),
	      functor(NewTerm, Term, 1),
	      arg(1, NewTerm, Var),
	      'C'(GoalsIn, NewTerm, GoalsOut)
	).
% 11: term is a sort. generate new predicate and proceed with 
%     the arguments of the sort.
trans_rhs(Term, Var, Decl0, DisjVarList, DisjVarListRest, ConsIn, ConsOut,
          GoalsIn, GoalsOut) :-
	type_inference(Var, Decl0),
	functor(Term, Name, Ar),
	( is_sort(Name, Ar) ->
	      ( sort_decl_table(check, Name, Ar, [Decl|Decls], _, _, known) ->
	        type_inference(Var, Decl)
	      ; true )
%	; is_basic_type(Name, TID) -> true
	; get_flag(fileID, FID),
	  get_flag(entryID, EID),
	  cuf_out(warning, ['undefined sort symbol:',Name/Ar,'$E$'(FID,EID)]),
	  symbol_table(assert, Name, Ar, *, -, sort, _)
	),
	Arity is Ar+1,
	functor(NewTerm, Name, Arity),
	arg(1, NewTerm, Var),
	'C'(GoalsMid, NewTerm, GoalsOut), !,
	trans_args(1, Arity, Decls, NewTerm, Term, DisjVarList,
		   DisjVarListRest, ConsIn, ConsOut, GoalsIn, GoalsMid).

%%% trans_rhs_neg(NegTerm, Var, Decl, DList, DListRest, CsIn, CsOut)
trans_rhs_neg('V'(V), Var, Decl, Ds, DsR, Cs, CsR) --> !,
	{ DsR = Ds, CsR = ['DIFF'(Var, V)|Cs],
          type_inference(Var, Decl) }.
trans_rhs_neg('S'(String), Var, Decl, Ds, DsR, Cs, CsR) --> !,
	{ DsR = Ds, CsR = Cs,
	  type_inference(Var,  Decl),
	  is_const(String,TID),    %% must succeed
	  negate_TID(TID,NegTID),
	  type_inference(Var, [[NegTID]]) }.
trans_rhs_neg('N'(Number), Var, Decl, Ds, DsR, Cs, CsR) --> !,
	{ DsR = Ds, CsR = Cs,
          type_inference(Var,  Decl),
	  is_const(Number,TID),    %% must succeed
	  negate_TID(TID,NegTID),
	  type_inference(Var, [[NegTID]]) }.
trans_rhs_neg('!'(Prolog), Var, Decl, DisjVarList, DisjVarList, 
          Cs, ['DIFF'([[[[TID]]],prolog(Prolog)], Var)|Cs]) -->
	!, [],
	{ type_inference(Var,  Decl),
          is_type(prolog,TID) }.
trans_rhs_neg(Term, Var, Decl, DisjVarList, DisjVarList, Cons, Cons) -->
	{ type_inference(Var, Decl),
	  ( Term == bottom -> true
	  ; Term == top -> fail
	  ; (is_const(Term, TID) ; is_type(Term, TID))  ->
		 negate_TID(TID, NewTID),
		 NewType = [[NewTID]]
	  ), type_inference(Var, NewType) }.

%%%  trans_rhs_disj(FlatList, Var, Decl, DisjVarList, DisjListRest, Clauses)
trans_rhs_disj([], _, _, DisjVarList, DisjVarList, []).
trans_rhs_disj([Term|R], Var, Decl, DisjVarList, DListRest, Clauses) :-
     ( trans_rhs(Term, VarA, Decl, DisjVarList, DisjVarList1, [],
		 Cons, Goals, [delayed_unify(Var,VarA)]) ->
           Clauses = [disj(Goals, Cons)|RR]
     ; RR=Clauses
     ),
     trans_rhs_disj(R, Var, Decl, DisjVarList1, DListRest, RR).

%%%  assert_disjuncts(Clauses, DisjHead, DisjName, DisjArity)
assert_disjuncts([], _, _, _).
assert_disjuncts([disj(Goals,Cons)|R], DisjHead, DisjName, DisjArity) :-
    assert_disjunct(DisjHead, Goals, Cons, DisjName, DisjArity), !,
    assert_disjuncts(R, DisjHead, DisjName, DisjArity).

%%%  assert_disjunct(+DisjFuncHead, +Goals, +DisjName, +Arity)
%%%  asserts generated goal
assert_disjunct(DisjFuncHead, Goals, Constraints, DisjName, Arity) :-
    get_flag(fileID, FID),
    get_flag(entryID, EID),
      \+ \+ ( do_delayed_unify(Goals, NewGs),
              ( check_constraints(Constraints, NewCs) ->
		    assertz(FID: trans_output(DisjName, Arity, DisjFuncHead, 
					      NewGs, NewCs, EID))
	      ; cuf_out(warning,
		     ['a disjunction branch has an empty denotation',
		      '$E$'(FID,EID)])
	      )).

do_delayed_unify([],[]).
do_delayed_unify([delayed_unify(VarX, VarY)|GoalsRest], NewGoalList) :- !,
    ( unify(VarX, VarY) , !
    ; cuf_out(error, ['not well-typed ... check declaration(s)','$NL$',
                      '$FS$'(VarX),'&','$FS$'(VarY),'failed.']),
      !, fail
    ),
    do_delayed_unify(GoalsRest, NewGoalList).
do_delayed_unify([Goal|GoalsRest], [Goal|NewGoalList]) :-
    do_delayed_unify(GoalsRest, NewGoalList).

type_inference(ResultVar, TypeTerm) :-
	( var(TypeTerm) -> true % ResultVar = [TypeTerm|_]
	; ( unify(ResultVar, [[TypeTerm|_]|_]) -> true
	  ; cuf_out(error, ['not well-typed ... check declaration(s)',
	                    '$NL$','$FS$'(ResultVar),'&',
			    '$FS$'([[TypeTerm|_]|_]),'failed.']),
	    !, fail
	  )
	).

is_type_term(Var, _) :-
	var(Var), !, fail.
is_type_term(';'(Term1,Term2), Out) :- !,
        Out = ';'(Term3,Term4),
	is_type_term(Term1,Term3),
	is_type_term(Term2,Term4).
is_type_term('&'(Term1,Term2), Out) :- !,
	Out = '&'(Term3,Term4),
        is_type_term(Term1,Term3),
	is_type_term(Term2,Term4).
is_type_term('~'(Term), Out) :- !,
        Out = '~'(Term1),
	is_type_term(Term, Term1).
is_type_term('N'(N),Out) :- !, Out = N.
is_type_term('S'(S),Out) :- !, Out = S.
is_type_term(Atom, Atom) :-
	atomic(Atom),
	(is_type(Atom,_);is_const(Atom,_)), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: compilation of CUF declarations               %%%
%%%      Created: Fri Sep 18 11:02:10 1992                      %%%
%%%     Modified: Thu Mar 14 17:39:37 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1996/03/14 17:01:53  jochen
% change in is_monofeature due to better recognition of
% polyfeatures (see polyfeatures2.pl)
%
% Revision 1.3  1994/01/18 12:19:18  jochen
% use negate_TID/2 instead of Neg is 0-TID everywhere
%
% Revision 1.2  1994/01/14  10:28:32  jochen
% Changed:  inc_counter(typeID, TID, 2)  -->  newID(typeID, TID, 2)
%
% Revision 1.1  1993/11/08  13:49:59  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Tue Apr  6 22:47:31 1993 (michel) : parser output format modified
% Tue Jan 26 16:43:48 1993 (michel) : convert_declaration/2 changed for negation

compile_declarations(FID) :-
	compile_feature_declarations(FID),
	compile_sort_declarations(FID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_feature_declarations(FID) :-
    feature_decls_exists(FID),
    'PARSER OUTPUT'(fdecl, FID, EID, (Dom, FeatRans)),
    set_flag(entryID, EID),
    compile_feature_declarations(Dom, FeatRans, FID, EID),
    dot,
    fail.
compile_feature_declarations(_FID).

feature_decls_exists(FID) :-
      'PARSER OUTPUT'(fdecl, FID, _, _), 
      set_flag(feat_decls, yes),
      set_flag(axiom_system, new), !.

compile_feature_declarations(Dom, FeatRans, FID, EID) :-
	( compile_declaration(Dom, TransDom)
	; cuf_out(error, ['translation failed','$E$'(FID,EID)]),
	  !, fail
	),
	neg_cnf2cnf(TransDom, Neg),
	is_type(cfs, CFS),
	domains2cnf([Neg,[[CFS]]], Axiom),
	add_axioms(Axiom, Flag),
	( Flag == 0 ->
	      cuf_out(error, ['inconsistent type declaration:',
	                      '$FS$'([[Dom|_]|_]),'$E$'(FID,EID)]),
	      !, fail
	;  assert(added_axiom(FID, EID, Axiom))
	), !,
	( compile_feature_ranges(FeatRans, TransDom, FDecls, []), !
	; cuf_out(error, ['translation failed','$E$'(FID,EID)]),
	  !, fail
	),
	add_feature_declarations(FDecls, FID, EID), !.

compile_feature_ranges([], _) --> [].
compile_feature_ranges([Feat:Ran|FRs], Dom) -->
	{ compile_declaration(Ran, TransRan) }, !,
	[decl(Feat, Dom, TransRan)],
	compile_feature_ranges(FRs, Dom).

compile_declaration(Term, Trans) :-
	flatten_struc(Term, FTerm),
	cnf(FTerm, CNF),
	convert_declaration(CNF, Trans), !.

add_feature_declarations([], _FID, _EID).
add_feature_declarations([decl(Feat,Dom,Ran)|Decls], FID, EID) :-
	feat_decl_table(assert, Feat, Dom, Ran, FID, EID, ok),
	add_feature_declarations(Decls, FID, EID).

add_default_feature_declaration(Feat, FID, EID) :-
	newID(typeID, TID, 2),
	feat_decl_table(assert, Feat, [[TID]], _, FID, EID, ok).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_sort_declarations(FID) :- 
	'PARSER OUTPUT'(sdecl, FID, EID, d(Sort, Arity, Doms)),
	set_flag(entryID, EID),
	( compile_sort_declaration(Doms, TransDoms) ->
	      sort_decl_table(assert, Sort, Arity, TransDoms, FID, EID, ok),
	      dot
	; cuf_out(entry, [FID,EID])
	),
	fail.
compile_sort_declarations(_FID).

compile_sort_declaration([], []).
compile_sort_declaration([Term|R], [Trans|TR]) :-
	compile_declaration(Term, Trans),
	compile_sort_declaration(R, TR), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  neg_cnf2cnf(+CNFIn, -CNFOut)
%%%  negation of a formula in cnf and transformation to cnf again
%%%  Input, Output: list of lists of integers, 
%%%                 negatives are negated positives
neg_cnf2cnf(X, Y) :-
	neg_cnf2cnf1(X, Z),
	recursive_sorting(Z, Y).

neg_cnf2cnf1([], [[]]).
neg_cnf2cnf1([C1|R], CNF) :-
	neg_cnf2cnf1(R, Cs), !,
	setof(DNF, 
	      setof([NegD1|D2], 
		    D1^(D2^(member(D1, C1),
			   negate_TID(D1,NegD1),
			   member(D2, Cs))),
		    DNF),
	      [CNF]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  domains2cnf(+ListOfCNFs, -CNF)
%%%  converts a disjunction of formulas in cnf to a new formula in cnf
%%%  cnf v ... v cnf --> cnf
domains2cnf(Domains, CNF) :-
	sort_by_card(Domains, SDomains),
	collect_disjunctions(SDomains, Cs, Disj),
	( Disj = [] ->
	      domains2cnf1(Cs, CNF0)
	; sort(Disj, SDisj),
	  domains2cnf1([[SDisj]|Cs], [CNF0])
	),
	del_supersets(CNF0, CNF).

domains2cnf1([], []).
domains2cnf1([F], [F]) :- !.
domains2cnf1([F1,F2|R], CNF) :-
	domains2cnf1(F1, F2, CNF0),
	recursive_sorting(CNF0, CNF1),
	del_supersets(CNF1, CNF2),
	domains2cnf1([CNF2|R], CNF).

domains2cnf1([], _, []) :- !.
domains2cnf1(Cs1, Cs2, CNFOut) :-
	del_once(C1, Cs1, Cs1R),
	setof(Ds,
	      C2^(member(C2, Cs2), append(C1, C2, Ds)),
	      CNF0),
	append(CNF0, CNF1, CNFOut),
	domains2cnf1(Cs1R, Cs2, CNF1).

%%%  collect_disjunctions(+ListOfCNFs, 
%%%                       -ListOfConjunctions, -Disjunction)
%%%  input is assumed to be sorted by the length of cnf lists
%%%  conjunctions may contain disjunctions
%%%  the disjunction is a list of integers
collect_disjunctions([], [], []).
collect_disjunctions([[[F|R]]|Rs], Cs, D) :- !,
	append([F|R], D1, D),
	collect_disjunctions(Rs, Cs, D1).
collect_disjunctions(Cs, Cs, []).    % all lists in the rest are longer
                                     % --> no pure disjunctions anymore

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  is_monofeature(+Feature, -Domain, -Range)
%%%  if Feature is single defined Domain and Range will be returned; 
%%%  else is_monofeature/3 fails
is_monofeature(Feat, Dom, Ran) :-
	( clause(mono_feat(Feat, Dom, Ran), true), !
	; \+ clause(feat_doms(Feat,_),true),
	  bagof(Dom-Ran, 
		FID^(EID^feat_decl(Feat, Dom, Ran, FID, EID)),
		[Dom-Ran]),
	  asserta(mono_feat(Feat, Dom, Ran))
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  convert_declaration(+FormulaIn, -FormulaOut)
%%%  conversion of types and constants to internal integer codes
convert_declaration([], []).
convert_declaration([-I], [CF]) :- !,
	convert1([-I], CF).
convert_declaration([[top]], _) :- !.
convert_declaration([F|R], [CF|CR]) :-
	convert1(F, CF), 
	convert_declaration(R, CR).

convert1([], []).
convert1([top], _) :- !.
convert1([F|R], [CF|CR]) :-
	( F = -Sym ->
	      ( is_type(Sym, TID) ->
		    negate_TID(TID,CF)
	      ; is_const(Sym, TID) ->
		    negate_TID(TID,CF)
	      )
	; is_type(F, CF), !
	; is_const(F, CF), !
	; cuf_out(error, ['unknown type or constant:',F]),
	  !, fail 
	),
	convert1(R, CR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: flag compilation                              %%%
%%%      Created: Fri Sep 18 17:08:49 1992                      %%%
%%%     Modified: Wed Apr  7 19:39:53 1993 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1994/07/20 15:30:40  junger
% adaption to the new handling for constraint declaration for foreign
% calls
%
% Revision 1.3  1994/05/19  14:09:42  junger
% - bug fixed
%
% Revision 1.2  1994/05/11  13:56:02  junger
% - handling of foreign declarations added
%
% Revision 1.1  1993/11/08  13:50:00  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Thu Dec  3 11:25:33 1992 (michel) : flag/2: error handling changed

:- dynamic index_table/3, cuf_foreign/3.

compile_prolog_flags(FID) :-
    'PARSER OUTPUT'(prolog, FID, EID, Term),
    ( flag_compiler(Term, FID, EID) -> dot
    ; delete_parser_output(FID, EID)
    ),
    fail.
compile_prolog_flags(_FID).

flag_compiler(Term, FID, EID) :-
    ( Term = index_table(SortArity) ->
	  ( SortArity = Sort/Arity, atomic(Sort), integer(Arity) ->
		assert(index_table(Sort, Arity, FID))
	  ; cuf_out(error,['argument of wrong type','$E$'(FID, EID)]),
	    fail
	  )
    ; Term = flag(FlagName, FlagValue) ->
	  ( flag(FlagName, FlagValue), !
	  ; cuf_out(entry, [FID, EID]),
	    fail
	  )
    ; Term = control_file(FileName) ->
	  ( ground(FileName) ->
		( cuf_abs_file_name(FileName, AbsName,ctrl,read) ->
		      assert(control_file(FID, AbsName))
		; cuf_out(error, ['could not find absolute file name for',
		                  FileName,'$E$'(FID,EID)]),
		  fail
		)
	  ; cuf_out(error, ['a file name should be ground',
	                    '$E$'(FID, EID)]),
	    fail
	  )
    ; Term = foreign('->'(Pred_Decl,Result)) ->
         functor(Pred_Decl,Sort,Arity),
	 ( Ar is Arity+1,
	   \+ cuf_foreign(Sort, Ar,_) ->
	   ( generate_foreign_clause(Pred_Decl, Result, FID, EID, normal) ->
		assert(cuf_foreign(Sort,Ar,FID)),
		add_symbol_occ(def, Sort, Arity, sort, FID, EID)
	   ; cuf_out(error, ['cannot translate foreign declaration',
	                     '$E$'(FID, EID)]),
	     fail
	   )
	 ; cuf_out(error, [Sort/Arity,'already declared as foreign sort']),
	   fail
	 )

    ; Term = foreign('->'(Pred_Decl,Result),constraints) ->
         functor(Pred_Decl,Sort,Arity),
	 ( Ar is Arity+1,
	   \+ cuf_foreign(Sort, Ar,_) ->
	   ( generate_foreign_clause(Pred_Decl, Result, FID, EID, constraints) ->
		assert(cuf_foreign(Sort,Ar,FID)),
		add_symbol_occ(def, Sort, Arity, sort, FID, EID)
	   ; cuf_out(error, ['cannot translate foreign declaration',
	                     '$E$'(FID, EID)]),
	     fail
	   )
	 ; cuf_out(error, [Sort/Arity,'already declared as foreign sort']),
	   fail
	 )

    ; cuf_out(error,['could not handle PROLOG clause or directive',
                     '$E$'(FID,EID)]),
      fail
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: CUF compiler                                  %%%
%%%      Created: Fri Sep 18 10:57:51 1992                      %%%
%%%     Modified: Wed Jun 22 15:28:50 1994 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.8  1994/06/22 13:39:26  michel
% * finish_compiling/1 added for collecting tasks triggered by unloading
%                      files (see unloading_files.pl for new `finish' tasks)
% * finish_compilation/1 calls finish_compiling/1 now
% * some comments added
%
% Revision 1.7  1994/05/16  16:05:41  michel
% report_statistics: reports as well if no counters exist (bug fix)
%
% Revision 1.6  1994/05/06  18:01:23  michel
% compiler0/1 added: similar to compiler0/0 but for grammars (bug fix)
%
% Revision 1.5  1994/02/03  11:51:24  junger
% - New predicate compiler0/0 was necessary to fix the bug with multiple
%   calls of report_statistics/0 for each call of compiler/0 in
%   compiler/1
% - compiler0/0 doesn't report anything anymore
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9
%
% Revision 1.4  1994/01/14  10:20:04  jochen
% Use predicate get_counter for errors/warnings counter.
% Bug in delete_interpreter_code (with index_table facts) removed.
%
% Revision 1.3  1993/12/23  13:32:42  michel
% compiler changed
% new predicates: compiler/1 for grammars, report_statistics/0,
%                 run_compilation/2, finish_compilation/1,
%                 delete_interpreter_code/1
%
% Revision 1.2  1993/11/18  14:17:13  jochen
% check in 2.28d:
%


% History (old):
% Thu Nov 18 14:16:18 1993 (jochen) : now loads control file before rule and table generation
% Wed Jul 21 21:03:16 1993 (michel) : delete_db_entries1/1 --> delete_db_entries1/2
% Thu Mar 25 10:14:33 1993 (michel) : retractall(trans_output/7) added
% Wed Dec  2 15:14:26 1992 (michel) : error handling changed
% Mon Nov  2 10:23:49 1992 (michel) : add_axioms added
% Thu Oct 29 11:58:48 1992 (michel) : recompiling/1 changed

:- dynamic cuf_task/2.

%%% compiler used by dload/1
compiler :-
   ( get_flag(syntax_check_only,yes) ->
	 cuf_out(message, ['syntax check finished:']),
	 report_statistics
   ; compiler0,
     cuf_out(message, ['compilation finished:']),
     report_statistics
   ).

compiler0 :-
   recompiling(FIDs1),
   compiling(FIDs2),
   append(FIDs1,FIDs2,FIDs0),
   sort(FIDs0, FIDs),
   ( del_flag(axiom_system, new) ->
	 init_sat,
	 add_axioms
   ; true
   ),
   ( FIDs == [] -> true
   ; run_compilation(FIDs)
   ).

report_statistics :-
   ( get_counter(errors,ENo) ; ENo=0 ), !,
   ( get_counter(warnings,WNo) ; WNo=0 ), !,
   cuf_out(message, ['errors:  ',ENo]),
   cuf_out(message, ['warnings:',WNo,'$NL$']).


%%% collect recompilation tasks
recompiling(FIDs) :-
   recompiling_(FIDs),
   output_recomp_tasks(FIDs).

recompiling_(FIDs) :-
   ( bagof(FID, ( retract(cuf_task(recompile,FID)),
                  delete_db_entries1(recompile,FID)
	        ), FIDs), !
   ; FIDs = [] ).

output_recomp_tasks([]).
output_recomp_tasks([FID|R]) :-
	file_table(check, FileName, _Date, FID, old),
	cuf_out(message, ['recompiling',FileName,'$NL$']),
	output_recomp_tasks(R).

%%% collect compilation tasks
compiling(FIDs) :-
    ( bagof(FID, retract(cuf_task(compile,FID)), FIDs), !
    ; FIDs = [] ).

%%% collect finish compilation tasks triggered by some file unloading
finish_compiling(FIDs) :-
   ( bagof(FID, retract(cuf_task(finish,FID)), FIDs), !
   ; FIDs = [] ).

%%% the main compilation steps
run_compilation([]).
run_compilation(Files) :-
    cuf_out(message, ['checking compilation flags:']),
    compile_prolog_flags0(Files),
    cuf_out(message, ['building rest of tables:']),
    compute_rest_of_tables0(Files),
    ( cuf_task(recompile,_) -> fail ; true),    % <--- the reason why!
    cuf_out(message, ['compiling type axioms:']),
    compile_axioms0(Files),
    cuf_out(message, ['compiling declarations:']),
    compile_declarations0(Files), !,
    ( del_flag(feat_decls, yes) ->
	  treating_feature_polymorphism
    ; true ),
    ( del_flag(axiom_system, new) ->
	  cuf_out(message, ['checking denotations of types and constants:']),
	  type_consistency_check
    ; true
    ),
    cuf_out(message, ['compiling clauses:']),
    compile_clauses0(Files),
    cuf_out(message, ['compiling control file(s):']),
    load_control(Files,_),
    finish_compilation(Files).
run_compilation(FIDs) :-
   save_compilation_tasks(FIDs),  %% this is necessary. see above. md
   compiler0.

%%% save recompilation tasks for later processing
save_compilation_tasks([]).
save_compilation_tasks([FID|R]) :-
	assert_if_new(cuf_task(recompile, FID)),
	save_compilation_tasks(R).

%%% compile flags and other CUF directives
compile_prolog_flags0([]).
compile_prolog_flags0([FID|R]) :-
    set_flag(fileID, FID),
    compile_prolog_flags(FID),
    compile_prolog_flags0(R).

compile_axioms0([]).
compile_axioms0([FID|R]) :-
    set_flag(fileID, FID),
    compile_axioms(FID),
    compile_axioms0(R).

compile_declarations0([]).
compile_declarations0([FID|R]) :-
    set_flag(fileID, FID),
    compile_declarations(FID),
    compile_declarations0(R).

%%% load control files
load_control([], Flag) :-
	( Flag == yes -> process_folders ; true ).
load_control([FID|R], Flag) :-
    ( clause(control_file(FID, FileName),true) ->
	  Flag = yes,
	  load_control_file(FileName)
    ; true
    ),
    load_control(R, Flag).

compile_clauses0([]).
compile_clauses0([FID|R]) :-
    set_flag(fileID, FID),
    compile_clauses(FID),
    compile_clauses0(R).

compute_tables0([]).
compute_tables0([FID|R]) :-
    set_flag(fileID, FID),
    compute_tables(FID),
    compute_tables0(R).

compute_rules0([]).
compute_rules0([FID|R]) :-
    set_flag(fileID, FID),
    compute_rules(FID),
    compute_rules0(R).

expand_det0([]).
expand_det0([FID|R]) :-
    set_flag(fileID, FID),
    expand_det(FID),
    expand_det0(R).

%%% collect axioms and feed the SAT module incrementally
add_axioms :-
   cuf_out(message, ['computing new axiom system:']),
   current_predicate(added_axiom,added_axiom(_,_,_)),
   added_axiom(_, _, Axiom),
   add_axioms(Axiom, _), dot,
   fail.
add_axioms.

%%% the compiler for grammars
compiler(Gram) :-
   ( get_flag(syntax_check_only,yes) ->
	 cuf_out(message, ['syntax check of grammar',Gram,'finished:']),
	 report_statistics
   ; compiler0(Gram),
     cuf_out(message, ['compilation of grammar',Gram,'finished:']),
     report_statistics
   ).

compiler0(Gram) :-
   recompiling(FIDs1),
   compiling(FIDs2),
   append(FIDs1,FIDs2,FIDs0),
   sort(FIDs0, FIDs),
   ( del_flag(axiom_system, new) ->
	 init_sat,
	 add_axioms
   ; true
   ),
   ( FIDs == [] -> true
   ; run_compilation(FIDs, Gram)
   ).

run_compilation([],_).
run_compilation(FIDs, Gram) :-
    cuf_out(message, ['checking compilation flags:']),
    compile_prolog_flags0(FIDs),
    cuf_out(message, ['building rest of tables:']),
    compute_rest_of_tables0(FIDs),
    ( cuf_task(recompile,_) -> fail ; true),
    cuf_out(message, ['compiling type axioms:']),
    compile_axioms0(FIDs),
    cuf_out(message, ['compiling declarations:']),
    compile_declarations0(FIDs), !,
    ( del_flag(feat_decls, yes) ->
	  treating_feature_polymorphism
    ; true ),
    ( del_flag(axiom_system, new) ->
	  cuf_out(message, ['checking denotations of types and constants:']),
	  type_consistency_check
    ; true ),
    cuf_out(message, ['compiling clauses:']),
    compile_clauses0(FIDs),
    load_grammar_control(Gram),
    finish_compilation(FIDs).
run_compilation(FIDs, Gram) :-
   save_compilation_tasks(FIDs),
   compiler0(Gram).

%%% the computation of the CUF rule entries
finish_compilation([]) :- !.
finish_compilation(Files0) :-
   finish_compiling(FIDs),
   append(Files0,FIDs,Files1),
   sort(Files1, Files),
   cuf_out(message, ['deleting old interpreter code:']),
   delete_interpreter_code(Files),
   cuf_out(message, ['building new interpreter code:']),
   ( current_predicate(index_table, index_table(_,_,_)) ->
	 compute_tables0(Files)
   ; true
   ),
   compute_rules0(Files),
   ( get_flag(expand_det, yes) ->
	 cuf_out(message, ['expansion of deterministic goals:']),
	 expand_det0(Files)
   ; true
   ).

delete_interpreter_code([]).
delete_interpreter_code([FID|R]) :-
	delete_index_tables0(FID),dot,
	retractall('CUF clause'(_,FID,_,_,_,_,_,_,_)),dot,
	retractall('rule counter'(FID, _, _)),dot,
	delete_interpreter_code(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: compute delay patterns                        %%%
%%%      Created: Fri Sep 18 13:06:16 1992                      %%%
%%%     Modified: Mon Jun  6 11:41:21 1994 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.6  1994/06/06 09:44:02  michel
% new (unambiguous) error messages
%
% Revision 1.5  1994/02/03  11:18:04  junger
% - Adaptation to new goal format
%
% Revision 1.4  1993/12/10  19:07:34  michel
% wait_condition/1 clauses generation added
%
% Revision 1.3  1993/12/10  10:39:10  michel
% new delay pattern format:
%
% 	delay(Sort(Path1, ..., PathN) -> Path0).
%
% 	Path ::= TypeTerm
% 	      |  Path & Path
% 	      |  Feature:Path
%
% new predicates:
%
% 	mk_delay_pattern/5, delay_args/{4,5,6}, delay_path/2,
%         mk_disj_body/2, typeTerm/1, groundVars/1, feature_chk/3
%
% Revision 1.2  1993/11/18  14:17:46  jochen
% check in 2.28d:
%


% History (old):
% Thu Nov 18 14:18:54 1993 (jochen) : handling of trigger and folder control  added
% Wed Dec  2 15:53:38 1992 (michel) : error handling changed
% Mon Nov  9 10:20:18 1992 (michel) : '*' --> '***',
%                                     has_variable_type/2 for future use

:- multifile term_expansion/2.

term_expansion(delay(Sort/Arity, Pattern), ExpTerm) :-
    ( is_defined_sort(Sort, Arity) ->
	  Arity1 is Arity+1,
	  functor(Goal, Sort, Arity1),
	  ctrl_stmt_args(Pattern, Arity1, Goal, Body, true),
	  ExpTerm = (delay_pattern(Sort, Arity1, Goal) :- Body),
	  dot
    ; cuf_out(error, ['undefined sort', Sort/Arity,
                      'in delay statement found']),
      ExpTerm = []
    ).

term_expansion(wait('->'(Goal,Result)), ExpTerm) :-
    functor(Goal,Sort,Arity),
    ( is_defined_sort(Sort, Arity) ->
	  mk_wait_pattern(Goal,Result,Sort,Arity,ExpTerm)
    ; cuf_out(error, ['undefined sort', Sort/Arity,
                      'in wait statement found']),
      ExpTerm = []
    ).

term_expansion(trigger_goal(Sort/Arity, Pattern), ExpTerm) :-
    ( is_defined_sort(Sort, Arity) ->
	  Arity1 is Arity+1,
	  functor(Goal, Sort, Arity1),
	  ctrl_stmt_args(Pattern, Arity1, Goal, Body, fail),
	  ExpTerm = ['TRIGGER GOAL'(Sort, Arity1, trigger),
                     ('CHECK TRIGGER'(Goal):- \+ Body)],
	  dot
    ; cuf_out(error, ['undefined sort', Sort/Arity,
                      'in trigger_goal statement found']),
      ExpTerm = []
    ).

term_expansion(lazy_goal(Sort/Arity), ExpTerm) :-
    ( is_defined_sort(Sort, Arity) ->
	  Arity1 is Arity+1,
	  ExpTerm = ['LAZY GOAL'(Sort, Arity1)],
	  dot
    ; cuf_out(error, ['undefined sort', Sort/Arity,
                      'in lazy_goal statement found']),
      ExpTerm = []
    ).

term_expansion(folder(Name,PredList,Cond), ExpTerm) :-
    dot,
    PredList = [First|_],
    functor(First,FFunc, FAr),
    ExpTerm = ['TRIGGER GOAL'(FFunc, FAr, folder(Name)),
               folder(Name,PredList,Cond)].


mk_wait_pattern(Goal,Result,Sort,Arity,ExpTerm) :-
	Arity1 is Arity+1,
	functor(Master,Sort,Arity1),
	functor(NewGoal,Sort,Arity1),
	Goal =.. [Sort|Args],
	Pattern =.. [Sort,Result|Args],
	wait_args(0, Arity1, Pattern, NewGoal, BodyList, []), !,
	mk_body(BodyList, Body),
	( current_predicate(wait_condition,wait_condition(_)),
	  wait_condition(Master) ->
	      ExpTerm = (wait_pattern(NewGoal) :- Body)
	; ExpTerm = [(wait_pattern(NewGoal) :- Body),
	             (wait_condition(Master) :- \+ wait_pattern(Master))]
	),
	dot.
mk_wait_pattern(Goal,Result,_,_,[]) :-
	groundVars((Goal,Result)),
	cuf_out(error, ['compilation failed for wait(',Goal,'->',
			Result,')']).


%% folders are processed after all control statements have been loaded

:- dynamic 'FOLDER DEF'/4.

process_folders :-
	retractall('FOLDER DEF'(_,_,_,_)),
	( current_predicate(folder, folder(_,_,_)),
	  folder(Name,PredList,Cond),
	  check_predlist(PredList,FlaggedPList,Vars,true),
	  assert('FOLDER DEF'(Name,'GOAL BUNDLE'(Name,Vars),
			      FlaggedPList, Cond)),
	  fail
	; true
	).

	

check_predlist([],[],[],true).
check_predlist([Skel|Rest],[WrappedGoal|RR],AllVars,Result) :-
    wrap_goal(Skel,WrappedGoal),
    goal_flag(WrappedGoal,Flag),
    functor(Skel,Sort,Arity),
    Skel =.. [_|Vars],
    append(Vars,NextVars,AllVars),
    Ar1 is Arity-1,
    ( is_defined_sort(Sort, Ar1) ->
	  Result = Result1
    ; cuf_out(error, ['sort of folder statement unknown:',Sort/Arity]),
      Result = fail
    ),
    det_flag(Sort,Arity,Flag),
    check_predlist(Rest,RR,NextVars,Result1).
    
ctrl_stmt_args([], _, _, Body, Body) :- !.
ctrl_stmt_args([F|R], Arity, Goal, Body, Body_in) :-
    integer(F),
    F =< Arity,
    F >= 0, !,
    F1 is F+1,
    arg(F1, Goal, X),
    add_goal(Body_in,X=[_|'***'],Body1),  %% value '***' may never occur
    ctrl_stmt_args(R, Arity, Goal, Body, Body1).
ctrl_stmt_args([F:Path|R], Arity, Goal, Body, Body_in) :-
    integer(F),
    F =< Arity,
    F >= 0,
    convert_path(Path, PList), !,
    F1 is F+1,
    arg(F1, Goal, X),
    add_goal(Body_in, has_variable_attribute(PList,X), Body1),
    ctrl_stmt_args(R, Arity, Goal, Body, Body1).
ctrl_stmt_args(A,_,_, B, B) :-
    cuf_out(error, ['illegal argument in control statement:',A]).

wait_args(A, A, _, _) --> !, [].
wait_args(A, Max, Pattern, Goal) -->
	{ A1 is A+1,
	  arg(A1,Pattern,Arg) },
	wait_args(Arg,A1,Goal),
	wait_args(A1,Max,Pattern,Goal).

wait_args(Var,_,_) --> [],
	{ var(Var) }, !.
wait_args(Path,A1,Goal) -->
	{ arg(A1,Goal,Arg) },
	wait_args(Path, Arg).

wait_args(TypeTerm,[TI2|R]) -->
	{ typeTerm(TypeTerm), !,
	  flatten_struc(TypeTerm, TT),
	  cnf(TT, CNF),
	  symbol_conversion(CNF, T1)
	},
	[ nonvar(TI2),
	  get_type(TI2,T2,_),
	  type_subsumes(T1,T2,1),
	  ( type_subsumes(T2,T1,1) ->
		nonvar(R)
	  ; true
	  )
	].
wait_args('&'(Path1,Path2),Arg) --> !,
	wait_args(Path1,Arg),
	wait_args(Path2,Arg).
wait_args(':'(Feat,Path),Arg) --> !,
	[ feature_chk(FeatList,Arg,Val),Body ],
	{ wait_path(':'(Feat,Path),FeatList,TypeTerm),
	  wait_args(TypeTerm,Val,Test,[]),
	  mk_body(Test,Body)}.
wait_args(Path,_) -->
	{ cuf_out(error, ['illegal path in wait statement:',Path]),
	  !, fail }.

wait_path(':'(Feat,Path),[Feat|R],TT) :- !,
	( nonvar(Feat) -> wait_path(Path,R,TT)
	; cuf_out(error, ['path contains a variable']),
	  !, fail
	).
wait_path(TypeTerm,[],TypeTerm).

%%%%%  mk_disj_body(+BodyList, -Body)
mk_disj_body([Body], Body) :- !.
mk_disj_body([Goal|Goals], ';'(Goal,Body)) :-
	mk_disj_body(Goals,Body).

typeTerm('~'(TT)) :- !,
	typeTerm(TT).
typeTerm('&'(TT1,TT2)) :- !,
	typeTerm(TT1),
	typeTerm(TT2).
typeTerm(';'(TT1,TT2)) :- !,
	typeTerm(TT1),
	typeTerm(TT2).
typeTerm(T) :-
	( symbol_table(T,0,_,_,type)
	; symbol_table(T,0,_,_,const)), !.

groundVars('_') :- !.
groundVars(A) :- atomic(A), !.
groundVars(T) :-
	T =.. [_|R],
	groundVars_(R).

groundVars_([]).
groundVars_([F|R]) :-
	groundVars(F),
	groundVars_(R).

convert_path(A, [A]) :-
    atomic(A), !.
convert_path(A:Path, [A|R]) :-
    atomic(A), !,
    convert_path(Path, R).
convert_path(P, []) :-
    cuf_out(error, ['illegal path in control statement:',P]).

add_goal(true, X, X) :- !.
add_goal(B, G, (B,G)).


%% runtime predicates
has_variable_attribute([], [_|'***']).
has_variable_attribute([Attr|Rest], [_|X]) :-
    member(Attr:Value, X), !,
    has_variable_attribute(Rest, Value).

has_variable_type([], ['***'|_]).
has_variable_type([Attr|Rest], [_|X]) :-
    member(Attr:Value, X), !,
    has_variable_type(Rest, Value).

%%%%%%%  get_attribute(+FeatureList, +FeatureGraph, -ValueGraph)
get_attribute([], X, X).
get_attribute([Attr|Rest], [_|X], Res) :-
    member(Attr:Value,X), !,
    get_attribute(Rest, Value, Res).

%%%%%%%  feature_chk(+FeatureList,+ListOfFeatureValuePairs,-PathValue)
feature_chk([], Val, Val).
feature_chk([F|R],[F1:V|_],Val) :-
	F1 == F, !,
	feature_chk(R,V,Val).
feature_chk(Fs,[_|R],Val) :-
	nonvar(R),
	feature_chk(Fs,R,Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: generating of intermediate (interpreter) code %%%
%%%      Created: Fri Sep 18 10:58:37 1992                      %%%
%%%     Modified: Wed Jun 12 15:02:24 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.11  1996/06/12 13:14:59  jochen
% bug fix: disjunctions compiled by cuf_extern2intern got det_flag wrong
% (leading to only 1st branch being evaluated)
%
% Revision 1.10  1996/04/09 12:50:20  jochen
% module prefix for trans_output facts;
% computation of det_flag speeded up
%
% Revision 1.9.1.1  1995/11/07 16:03:49  jochen
% PCUF; initial check-in.
%
% Revision 1.9  1994/07/20  15:31:34  junger
% now 'CUF clause' for foreign calls get a rule ID inserted
%
% Revision 1.8  1994/05/19  14:47:51  jochen
% reduce_typeinfo made cyclic-structures-proof
%
% Revision 1.7  1994/05/19  14:10:12  junger
% - bug fixed
%
% Revision 1.6  1994/05/18  16:55:25  michel
% reduce_typeinfo/2:
% * functor(A, prolog, 1) case added for new internal format
%
% Revision 1.5  1994/05/11  13:50:50  junger
% rule_trans-clause for generating clauses for prolog calls added
%
% Revision 1.4  1994/02/18  12:31:21  michel
% rule_trans2/3 changed: reduces the number of constraints now (if possible)
% new predicate: rule_trans2_/3 (same as old rule_trans2/3)
%
% Revision 1.3  1994/02/03  11:24:34  junger
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9
% - numbering of subgoals
% - functions for goalformat abstraction added
%
% Revision 1.2  1993/11/18  14:18:09  jochen
% check in 2.28d:
%


% History (old):
% Thu Nov 18 14:21:02 1993 (jochen) : det_flag for triggers, folders and lazy_goals added
% Wed Mar 24 17:27:13 1993 (michel) : retract(trans_output/7) retracted
% Mon Dec 21 20:02:06 1992 (michel) : Name/Arity --> Name, Arity
% Mon Dec 21 19:50:39 1992 (michel) : '$OR$'/11 added
% Tue Dec 15 11:33:20 1992 (michel) : separate constraint list for
%                                     DIFFs and POLYs --> 2 additional
%                                     arguments for rule entries
% Wed Dec  2 15:57:24 1992 (michel) : error handling changed


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   generates rule/7 clauses and table entries from trans_output/6
%%%   facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute_rules(FID) :-
	trans_output(Pred, Ar, Head, Body, Cons, FID, EID),
	( rule_trans(Pred, Ar, Head, Body, Cons, FID, EID, Rule) ->
	      assert(Rule), dot
	; cuf_out(error, ['$E$'(FID,EID)]),   % better than nothing ...
	  % some more error messages will be added sometimes ...
	  retract(FID:trans_output(Pred, Ar, Head, Body, Cons, EID))
	),
	fail.
compute_rules(_).

%%%%%%%  rule_trans(+Name, +Arity, +Head, +Body, +Constraints,
%%%%%%%             +FileID, +EntryID-RuleOrTable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   generation of rule or table entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_trans(Name, Arity, Head, Body, Cons, FID, EID, Out) :-
    cuf_foreign(Name, Arity,FID), !,
    Out = ('CUF clause'(Head, FID, EID, 1, _NID,
			S_Goals, S_Goals, Cons1, Cons2) :- Body),
    Cons = Cons1/Cons2.
rule_trans(Name, Arity, Head, Body, Cons, FID, EID, TableEntry) :-
    Arity1 is Arity-1,
    index_table(Name, Arity1, FID), !,
    index_table_name(Name, Arity, TableName, TableArity),
    rule_trans1(Body, NodeID, 1, BodyNew, BodyRest),
    rule_trans2(Cons, ConsNew, ConsRest), !,
    functor(TableEntry, TableName, TableArity),
    reduce_parameters(Name, Arity, Head, NewHead),
    NewHead =.. [_, Result, [TypeInfo,Atom]|RArgs],   
    TableEntry =.. [_, Atom, FID, EID, TypeInfo, NodeID^BodyNew, BodyRest,
                    ConsNew, ConsRest, Result|RArgs].
rule_trans(Name, Arity, Head, Body, Cons, FID, EID,
	   ('CUF clause'(Goal, FID, EID, No, NodeID, BodyNew, BodyRest,
                 ConsNew, ConsRest) :- SubGoals)) :- 
    rule_trans1(Body, NodeID, 1, BodyNew, BodyRest),
    rule_trans2(Cons, ConsNew, ConsRest), 
    functor(Goal, Name, Arity),
    ExtArity is Arity-1,
    ( current_predicate('COST PREDS',_),'COST PREDS'(Name,ExtArity,PList) ->
	  length(PList,Len),
	  ( Len =:= 1 -> PList = [CostPred]
	  ; findall(FID0-EID0,'DEF OCC'(Name,ExtArity,sort,FID0,EID0),ClauseIDs),
	    member_no(FID-EID,ClauseIDs,CNo),
	    ( CNo > Len -> CNo1 = Len
	    ; CNo1 = CNo
	    ),
	    nth(CNo1,PList,CostPred)
	  ),
          CostCall =.. [CostPred,Goal,[],Cost],
%%        CostCall =.. [CostPred,Goal,CostArgs,Cost], %% CostArgs to be collected still
	  SubGoalTail = [CostCall]
    ; SubGoalTail = []
    ),
    head_vars(Arity, Head, [], Goal, SubGoalList, SubGoalTail),
    mk_body(SubGoalList, SubGoals),
    rule_counter(FID, EID, No0),
    ( SubGoalTail == [] -> No = No0
    ; No = cost(No0,Cost)
    ).
    


%%%%%%%  rule_trans1(+GoalList,+NodeID,+SubGoalNo,-NewGoalList,-RestGoals)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   goal transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_trans1([], _, _) --> [].
rule_trans1([Goal|Goals], NodeID, SubGNo) -->
    {functor(Goal, Name, Arity),
     NextSubGNo is SubGNo+1     },
    rule_trans_goal(Name, Arity, Goal, NodeID:SubGNo),  
                                               %% NodeID:No forms GoalID
    rule_trans1(Goals, NodeID, NextSubGNo).

%%%%%%%  rule_trans2(+ConstraintList, -NewConstraintList, -RestCons)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   constraint transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_trans2([], X, Out) :- !, Out = X.
rule_trans2(Cs, ConsNew, ConsRest) :-
   rule_trans2_(Cs, ConsNew0, []),
   sort(ConsNew0, ConsNew1),
   append(ConsNew1, ConsRest, ConsNew).

rule_trans2_([]) --> [].
rule_trans2_([Cons|R]) -->
    trans_constraint(Cons),
    rule_trans2_(R).

trans_constraint('POLY'(ID, Dom, Ran)) -->
    { reduce_typeinfo0(Ran, NewRan),
      reduce_typeinfo0(Dom, NewDom) },
    ['POLY'(ID, NewDom, NewRan)].
trans_constraint('DIFF'(FS1, FS2)) -->
    { reduce_typeinfo(FS1, FS1New),
      reduce_typeinfo(FS2, FS2New) },
    ['DIFF'(FS1New, FS2New)].

%%%%%%%  rule_trans_goal(+Name, +Arity, +Goal, GoalID, -NewGoalList, -Rest)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   checks determinism and puts markers on new goal.
%%%   type_checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_trans_goal(Name, Arity, Goal, GoalID) -->    
    { det_flag(Name, Arity, DetFlag),
      reduce_parameters(Name, Arity, Goal, NewGoal),
      wrap_goal(NewGoal,WrappedGoal),
      goal_flag(WrappedGoal,DetFlag),
      goalID(WrappedGoal,GoalID) },
    [WrappedGoal].
%    [NewGoal:DetFlag].

%%%%%%%%  det_flag(+Name ,+Arity, +Goal, -DetFlag)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   if Name/Arity is deterministic, DetFlag is set; else it remains
%%%   variable.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
det_flag('$OR$', 11, disj) :- !.
det_flag(Name, Arity, foreign) :-
    cuf_foreign(Name, Arity, _), !.
det_flag(Name, Arity, table) :-
    index_table_name(Name, Arity, _, _), !.
det_flag(Name, Arity, memo) :-
    current_predicate('MEMO GOAL', 'MEMO GOAL'(_,_)),
    'MEMO GOAL'(Name, Arity), !.
det_flag(Name, Arity, nondet) :-
    current_predicate('LAZY GOAL', 'LAZY GOAL'(_,_)),
    'LAZY GOAL'(Name, Arity), !.
det_flag(Name, Arity, _) :-
    Arity1 is Arity-1,
    ( 'DEF OCC'(Name,Arity1,sort,FID,_) -> true),
    ( FID \== tmp ->
	  reset_counter,
	  trans_output(Name,Arity,_,_,_,FID,_), %% FID must be bound!
	  incr_counter(2)
    ; true 
    ),
    !.
det_flag(_,_,det).

%%%%%%%  head_vars(+ArgNo, +Head, +VarListRest, +NewHead, -GoallList,
%%%%%%%            +RestGoals) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  connects old and new head and if necessary collects goals for
%%%  later processing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
head_vars(ArgNo, Head, HeadVarList, NewHead) -->
    ( {ArgNo =:= 0} -> []
    ; { arg(ArgNo, Head, HeadArg), 
        arg(ArgNo, NewHead, NewHeadVar), 
	ArgNo1 is ArgNo-1 },
      head_arg(HeadArg, NewHeadVar, HeadVarList),
      head_vars(ArgNo1, Head, [NewHeadVar|HeadVarList], NewHead)
    ).


%%%%%%%  head_arg(+HeadArg, ?NewHeadVar, +HeadVarList, -GoalList,
%%%%%%%           -RestGoals) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  instantiates NewHeadVar or adds a unify/2 goal to GoalList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
head_arg(HeadArg, HeadArg, VarsSoFar) -->
    { var(HeadArg), 
      \+ var_mem(VarsSoFar, HeadArg) }, !,
    [].
head_arg(HeadArg, NewHeadVar,_) -->
    { reduce_typeinfo(HeadArg, NewHeadArg) }, 
    [unify(NewHeadVar, NewHeadArg)].

%%%%%%%  mk_body(+GoalList, -Goals)
mk_body([], true).
mk_body([Goal], Goal) :- !.
mk_body([Goal|Goals], (Goal, Body)) :-
    mk_body(Goals, Body).

%%%  reduce_parameters(+Name, +Arity, +Goal, -NewGoal)
%%%  reduces typeinfos in goal's parameters
reduce_parameters(Name, Arity, Goal, NewGoal) :-
	functor(NewGoal, Name, Arity),
	reduce_parameters1(1, Arity, Goal, NewGoal).

reduce_parameters1(N, Max, Goal, NewGoal) :-
	arg(N, Goal, Arg),
	reduce_typeinfo(Arg, NewArg),
	arg(N, NewGoal, NewArg),
	( N =:= Max -> true
	; N1 is N+1,
	  reduce_parameters1(N1, Max, Goal, NewGoal)
	).

%%%  reduce_typeinfo(+Graph, -ReducedGraph)
%%%  builds a new graph by removing old type infos
reduce_typeinfo(G, RG) :- reduce_typeinfo(G, [], RG).

reduce_typeinfo(X, SoFar, Out) :- 
    ( var(X) -> Out = X
    ; X = [TI|R], Out = [TypeInfo|RNew],
      reduce_typeinfo0(TI, TypeInfo),
      ( var(R) -> RNew = R
      ; R = [A], 
        ( atomic(A)             /* constant */
	; functor(A,'.',2)      /* string */
	; functor(A, prolog, 1) /* prolog term */
	) -> RNew = R
      ; memberchk_eq(TypeInfo, SoFar) -> RNew = R
      ; reduce_typeinfo1(R, [TypeInfo|SoFar], RNew)
      )
    ).

reduce_typeinfo0(OldTI, NewTI) :-
	( var(OldTI) -> NewTI = OldTI
	; get_type(OldTI, Type, RType),
	  NewTI = [Type|RType]
	).

reduce_typeinfo1([F:V|R], SoFar, [F:VNew|RNew]) :-
	reduce_typeinfo(V, SoFar, VNew),
	( var(R) -> RNew = R
	; reduce_typeinfo1(R,SoFar, RNew) ).









%%%%% functions for goal-abstraction

%%%%%  goalID(+WrappedGoal, -GoalID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  delivers the GoalID of Goal as :-term
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goalID(g(ParID,NthSubgoal,_,_),ParID:NthSubgoal).


%%%%%  wrap_goal(-Goal, +WrappedGoal)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  delivers the goal "per se"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrap_goal(Goal,g(_,_,Goal,_)).


%%%%%  goal_flag(+WrappedGoal, -DetFlag)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% delivers DetFlag-value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goal_flag(g(_,_,_,DetFlag),DetFlag).


%%%%%  initial_nodeID(-NodeID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  gives ID for root-node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_nodeID([]).



%%%%%  set_initial_goalIDs(GoalList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  instantiates initial goal list with IDs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_initial_goalIDs(Goals) :-
   initial_nodeID(NID),
   set_goalIDs(Goals, NID, 1).

set_goalIDs([],_,_).
set_goalIDs([WG|R], NodeID, N) :-
   goalID(WG, NodeID:N),
   N1 is N+1,
   set_goalIDs(R, NodeID, N1).

   
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: scomputes tables after a file access          %%%
%%%      Created: Tue Apr  6 21:50:13 1993                      %%%
%%%     Modified: Fri Mar 15 20:51:00 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.7  1996/04/09 13:14:19  jochen
% module prefix for 'PARSER OUTPUT' facts (in delete_parser_output/2)
%
% Revision 1.6  1994/05/06 18:05:01  michel
% cuts added in compute_rest_of_tables0/1 to make it more robust
% (backtracking from outside, bug fix)
%
% Revision 1.5  1994/01/18  12:23:02  jochen
% build_symbol_table/2: 3rd clause (for safety) added
%
% Revision 1.4  1993/12/20  18:35:46  michel
% delete_parser_output/2: retraction of 'TYPE CONST'/3 added
%
% Revision 1.3  1993/12/20  18:24:08  michel
% bug fix in delete_parser_output/2: 'SORT TYPE CONST'/4 --> 'SORT TYPE CONST'/3
%
% Revision 1.2  1993/11/18  14:18:22  jochen
% check in 2.28d:
%


% History (before RCS):
% Tue Apr  6 21:51:37 1993 (michel) : created

%%%%%%%%%%  compute_rest_of_tables
% Conflicts can only occur with symbols of arity 0, i.e. sorts, types and 
% constants. So we can concentrate on these symbols ignoring features and 
% sorts of arity > 0 which are handled during parsing. Of course, there 
% can be non-defined symbols too. These will be added to the symbol table 
% at the end of this computation step.
compute_rest_of_tables0([]) :- !.
compute_rest_of_tables0(Files) :-
   build_symbol_table0(Files),
   type_const_conflicts,
   sort_type_const_conflicts,
   detect_undefined_symbols0(Files), !.

build_symbol_table0([]).
build_symbol_table0([FID|R]) :-
   build_symbol_table(FID),
   build_symbol_table0(R).

detect_undefined_symbols0([]).
detect_undefined_symbols0([FID|R]) :-
   detect_undefined_symbols(FID),
   detect_undefined_symbols0(R).

build_symbol_table(FID) :-
   setof(Symbol,
	 KindOf^(EID^(ID^('DEF OCC'(Symbol,0,KindOf,FID,EID),
			\+ symbol_table(Symbol,0,ID,*,KindOf)))),
	 Symbols),  % are there defined symbols not in the symbol table?
   build_symbol_table(Symbols, FID).
build_symbol_table(_FID).

build_symbol_table([], _).
build_symbol_table([Symbol|Symbols], FID) :-
   setof(KindOf,
	 F^(E^'DEF OCC'(Symbol,0,KindOf,F,E)),
	 KindOfs), % what kind of symbol was defined?
   ( KindOfs = [KindOf] ->  % unary case
       'DEF OCC'(Symbol, 0, KindOf, FID, EID),
       add_def_symbol(Symbol, 0, KindOf, FID, EID),
       dot
   ; analyze_conflict(KindOfs, Symbol)
   ), !,
   build_symbol_table(Symbols, FID).
build_symbol_table([_|Symbols], FID) :-  %% for safety
   build_symbol_table(Symbols, FID).

analyze_conflict([KindOf1, KindOf2], Symbol) :-
   prep_cuf_out(KindOf1,_,_,Kind1,_),
   prep_cuf_out(KindOf2,_,_,Kind2,_),
   cuf_out(error, [symbol,Symbol,'defined as',Kind1,and,Kind2]),
   analyze_conflict(Symbol).
analyze_conflict([KindOf1, KindOf2, KindOf3], Symbol) :-
   prep_cuf_out(KindOf1,_,_,Kind1,_),
   prep_cuf_out(KindOf2,_,_,Kind2,_),
   prep_cuf_out(KindOf3,_,_,Kind3,_),
   cuf_out(error, [symbol,Symbol,'defined as',Kind1,and,Kind2,
                   'as well as',Kind3]),
   analyze_conflict(Symbol).

analyze_conflict(Symbol) :-
   setof(occ(FID,EID,KindOf),
	 'DEF OCC'(Symbol, 0, KindOf, FID, EID),
	 Occs),
   delete(occ(F,E,K), Occs, RestOccs),
   prep_cuf_out(K,_,_,K1,_),
   cuf_out(message, ['... ignoring all entries of',Symbol,
                     'not defining a',K1]),
   add_def_symbol(Symbol, 0, K, F, E),
   dot,
   delete_parser_outputs(RestOccs, K).

delete_parser_outputs([], _).
delete_parser_outputs([occ(_,_,KindOf)|R], KindOf) :- !,
   delete_parser_outputs(R, KindOf).
delete_parser_outputs([occ(FID,EID,_)|R], KindOf) :-
%   cuf_out(message, ['$E$'(FID, EID)]),
   delete_parser_output(FID, EID),
   delete_parser_outputs(R, KindOf).

delete_parser_output(FID, EID) :-
   retractall('DEF OCC'(_,_,_,FID,EID)),
   retractall('USED OCC'(_,_,_,FID,EID)),
   retractall(FID:'PARSER OUTPUT'(_,EID,_)),
   retractall('TYPE CONST'(_,FID,EID)),
   retractall('SORT TYPE CONST'(_,FID,EID)).
   
type_const_conflicts :-
   setof(occ(Symbol,FID,EID),
	 'TYPE CONST'(Symbol, FID, EID),
	 Conflicts), !,
   resolve_conflicts(Conflicts, tc).
type_const_conflicts.

sort_type_const_conflicts :-
   setof(occ(Symbol,FID,EID),
	 'SORT TYPE CONST'(Symbol, FID, EID),
	 Conflicts), !,
   resolve_conflicts(Conflicts, stc).
sort_type_const_conflicts.

resolve_conflicts([], C) :-
   ( C == tc -> retractall('TYPE CONST'( _, _, _))
   ; /* C == stc */ 
     retractall('SORT TYPE CONST'( _, _, _))
   ).
resolve_conflicts([occ(Symbol,FID,EID)|R], C) :-
    ( is_const(Symbol, _) -> KindOf = const
    ; is_type(Symbol, _) -> KindOf = type
    ; C == stc, is_sort(Symbol, 0) -> KindOf = sort
    ; % default:
      KindOf = const,
      add_undef_symbol(Symbol, 0, const, FID, EID),
      dot
    ),
    used_occ_table(Symbol, 0, KindOf, FID, EID),
    resolve_conflicts(R, Symbol, KindOf, C).

resolve_conflicts([], _, _, C) :-
    resolve_conflicts([], C).
resolve_conflicts([occ(Symbol,FID,EID)|R], Symbol, KindOf, C) :- !,
    used_occ_table(Symbol, 0, KindOf, FID, EID),
    resolve_conflicts(R, Symbol, KindOf, C).
resolve_conflicts(List, _, _, C) :-
    resolve_conflicts(List, C).

detect_undefined_symbols(FID) :-
    setof(undef(Symbol,Arity,KindOf),
	  EID^(TID^(M^('USED OCC'(Symbol,Arity,KindOf,FID,EID),
		     % Arity > 0,  % features or sorts only
		     \+ symbol_table(Symbol,Arity,TID,M,KindOf)))),
	  Undefs),
    detect_undefined_symbols(Undefs, FID).
detect_undefined_symbols(_).

detect_undefined_symbols([], _).
detect_undefined_symbols([undef(Symbol,Arity,KindOf)|R], FID) :-
    'USED OCC'(Symbol,Arity,KindOf,FID,EID),
    add_undef_symbol(Symbol, Arity, KindOf, FID, EID),
    dot, !,
    detect_undefined_symbols(R, FID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: conversion of external to internal CUF code   %%%
%%%               and vice versa                                %%%
%%%      Created: Fri Dec 18 09:40:46 1992                      %%%
%%%     Modified: Tue Jan 31 10:36:42 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.14  1995/02/01 14:33:33  jochen
%  - now handles !-terms containing variables properly
%  - !-terms in negations allowed now; these generate 'DIFF' constraints
%
% Revision 1.13  1994/09/13  17:41:19  michel
% bug fix in last clause of ex2in_neg/6
%
% Revision 1.12  1994/07/01  12:02:36  jochen
% internal representation of prolog-terms (!-terms) now with type
% 'prolog'
%
% Revision 1.11  1994/05/31  08:54:49  michel
% prep_ex/2, :/2 case:
% * add_default_feature_declaration/3 call removed; already handled by
%   add_undef_symbol/5 (bug fix)
%
% Revision 1.10  1994/05/16  16:09:47  michel
% prep_ex/2:
% * !/1 case for Prolog terms
% ex2in/6:
% * !/1 case for Prolog terms
% * '$true$'/1 built-in sort
%
% Revision 1.9  1994/05/11  10:56:29  michel
% cuf_extern2intern/4:
%     	delays variable unification for disjunction handling now
% 	(calls do_delayed_unify/2 after ex2in/6)
% prep_ex/2: '|'/2 case removed;
%            handles all undefined symbols now
% new predicate prep_ex_args/4: replaces prep_ex_list/2
% mk_conj/2 removed
% ex2in/6: 'V' case adds delayed_unify/2 goal now
% 	 ';' case added, generates dynamic goals if neccessary;
%          treatment of undefined symbols moved to prep_ex/2 (see above)
%          '~' cases now handles by ex2in_neg/6
% new predicate ex2in_neg/6: treatment of negated symbols
% new predicate ex2in_disj/3: translates disjunction branches
% new predicate collect_vars/3: collects the variables occuring in the
%                               disjunction branches
% new predicates assert_disjs/4, assert_disj/5: assert the dynamic clauses
%
% Revision 1.8  1994/05/06  18:27:33  michel
% cuts added in cuf_extern2intern/4, prep_ex/2 to make it more robust
% (backtracking from outside, bug fix)
% ex2in/6, atomic case: bug fix for Term == top
%
% Revision 1.7  1994/02/03  11:07:40  junger
% - Adaptation to new goal format
%
% Revision 1.6  1994/01/18  12:18:02  jochen
% handling of 'N' cases in ex2in simplified
% is_string/1 moved to utils.pl
%
% Revision 1.5  1994/01/14  10:36:23  jochen
% 1) Handling of numbers and strings adapted to new reader and internal
% formats.
%
% 2) prep_nelist/2 predicate was superfluous, eliminated.
%
% 3) ex2in made more consistent with trans_rhs (more code sharing should
%   be possible). Some bugs having to do with negated terms removed.
%
% Revision 1.4  1993/12/23  13:01:14  michel
% ex2in/6: disjunction case produces a message and fails now
%
% Revision 1.3  1993/12/08  13:37:50  michel
% *** empty log message ***
%
% Revision 1.2  1993/12/08  13:09:40  michel
% atom_chars for strings added (like in cuf_parser.pl)
%
% Revision 1.1  1993/11/08  13:50:05  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Thu Sep  2 13:44:58 1993 (michel) : sort_decl/3 --> sort_decl/5

%%%  the following code is only a fast hack. Don't think about it ...

%%%  cuf_extern2intern(Feature_Term, Internal_Code, Result)
cuf_extern2intern(Extern, Intern, Constraints, Result) :-
   prep_ex(Extern, Extern1), !,
   transform_negations(Extern1, Extern2),
   ex2in(Extern2, Result, [], Constraints, Intern0, []),
   do_delayed_unify(Intern0, Intern1),
   trans_goal_list(Intern1, Intern, []), !.



trans_goal_list([]) --> [].
trans_goal_list([Goal|Goals]) -->
   trans_goal(Goal),
   trans_goal_list(Goals).

trans_goal(Goal) -->
   {functor(Goal,Name,Arity),
    det_flag(Name,Arity,Flag),
    wrap_goal(Goal,WG),
    goal_flag(WG,Flag) },
    [WG].
   
%%%  prep_ex(+TermIn, -TermOut)
%%%  adds some annotations for variables and strings
%%%  detects undefined symbols and adds defaults if possible
%%%  the rest remains unchanged
prep_ex(Var, 'V'(Var)) :- var(Var),!.
prep_ex(Num, 'N'(Num)) :- number(Num),!.
prep_ex(Atom, Atom) :- atomic(Atom), !,
   ( ( is_type(Atom,_)
     ; is_const(Atom,_)
     ; is_defined_sort(Atom,0)
     ) -> true
   ; add_undef_symbol(Atom, 0, const, tmp, default),
     used_occ_table(Atom, 0, const, tmp, default)
   ).
prep_ex([C|Cs], 'S'(String)) :- 
	is_string([C|Cs]), !,
	append([0'",C|Cs], [0'"], Chars),
	atom_chars(String, Chars),!,
	assert_if_new('STRING_CONST'(String)),
	used_occ_table(String, 0, const, tmp, default).
prep_ex(Feat:V, Feat:PV) :- !,
   ( is_feat(Feat) -> true
   ; add_undef_symbol(Feat, 1, feat, tmp, default),
     used_occ_table(Feat, 1, feat, tmp, default)
   ),
   prep_ex(V, PV).     
prep_ex([], []) :- !.
prep_ex([F|R], '&'('F':PF,'R':PR)) :- !,
   prep_ex(F, PF),
   prep_ex(R, PR).
prep_ex('&'(X,Y), '&'(PX,PY)) :- !,
   prep_ex(X, PX),
   prep_ex(Y, PY).
prep_ex(';'(X,Y), ';'(PX,PY)) :- !,
   prep_ex(X, PX),
   prep_ex(Y, PY).
prep_ex('~'(X), '~'(PX)) :- !,
   prep_ex(X, PX).
prep_ex('!'(Prolog), '!'(Prolog)) :- !.
prep_ex(Ex, Prep) :-
   functor(Ex, Name, Ar),
   ( is_sort(Name, Ar) -> true
   ; cuf_out(error, ['undefined sort symbol:',Name/Ar]),
     !, fail
   ),
   functor(Prep, Name, Ar),
   prep_ex_args(0, Ar, Ex, Prep).

prep_ex_args(Ar, Ar, _, _) :- !.
prep_ex_args(Ar0, Ar, Ex, Prep) :-
   Ar1 is Ar0+1,
   arg(Ar1, Ex, ExA),
   arg(Ar1, Prep, PrepA),
   prep_ex(ExA, PrepA),
   prep_ex_args(Ar1, Ar, Ex, Prep).

%%%  ex2in(Extern, Result, ConsIn, ConsOut, Goals, GoalsRest)
ex2in('V'(Var), Result, C, C) --> !, [delayed_unify(Result, Var)].
ex2in('N'(Number), Result, C, C) --> !,
   { is_const(Number,TID),    %% must succeed
     unify(Result, [[[[TID]]], Number]) }.
ex2in('S'(String), Result, C, C) --> !,
   %% we don't assert 'STRING_CONST' facts for new strings here
   %% so hack TID here.
   { ( is_const(String,TID) ->    %% may have occurred in type axiom
	   true
     ; TID = string(String)
     ),
     unify(Result, [[[[TID]]], String]) }.
ex2in('&'(TermA,TermB), Result, C_In, C_Out) --> !,
   ex2in(TermA, Result, C_In, C_1),
   ex2in(TermB, Result, C_1, C_Out).
ex2in(';'(TermA,TermB), Result, C, C) --> [],
   { is_type_term(';'(TermA,TermB),';'(TermC,TermD)), !,
     flatten_struc(';'(TermC,TermD), FTT),
     cnf(FTT, CNF),
     symbol_conversion(CNF, CNFList),
     type_inference(Result, CNFList) }.
%ex2in(';'(_,_),_,_,_,_,_) :- !,
%    cuf_out(message,['CUF does not support the full interpretation of']),
%    cuf_out(message,['uncompiled disjunctions at the moment. Sorry!']),
%    !,fail.
ex2in(';'(TermA,TermB), Result, C_In, C_Out) --> !,
    { collect_vars((TermA,TermB),[],Vars),
      % flat the disjunction
      flat_disj(';'(TermA,TermB), List, []),
      % remove duplicates: (esp. useful for 'NONE's)
      sort(List, SList),
      % proceed with the disjuncts:
      ex2in_disj(SList, ResH, Clauses),
      ( Clauses == [] -> fail
      ; Clauses = [disj(Goals,Cons)] ->
	    unify(Result,ResH),
	    DisjCall = Goals,
	    append(C_In, Cons, C_Out)
      ; C_In=C_Out,
	% generate new predicate:
	gensym(genDisj,DisjName),
	length(Vars,DisjArity0),
	DisjArity is DisjArity0+1,
	add_def_symbol(DisjName, DisjArity0, sort, tmp, default),
	def_occ_table(DisjName, DisjArity0, sort, tmp, default),
	used_occ_table(DisjName, DisjArity0, sort, tmp, default), !,
	DisjCall0   =.. [DisjName,Result|Vars],
	DisjHead =.. [DisjName,ResH|Vars],
	DisjCall = [DisjCall0],
	% add the generated clauses:
	assert_disjs(Clauses, DisjHead, DisjName, DisjArity), !
      )  }, 	DisjCall.
ex2in(':'(Feat,Val), Result, C_In, C_Out) -->
   { is_monofeature(Feat, Dom, Ran), !,
     (var(Dom) -> true ; Decl = [Dom|_]),
     unify([Decl,Feat:Result1|_], Result),
     type_inference(Result1, Ran)
   }, !,
   ex2in(Val, Result1, C_In, C_Out).
ex2in(':'(Feat,Val), Result, C_In, C_Out) --> !,
   { clause(feat_doms(Feat, Doms), true),
     Dom = [Doms|_],
     unify([Dom,Feat:[Ran|Result1]|_], Result), !,
     clause(poly_feat_check(Feat, Dom, Ran, Cons), true),
     append(C_In, Cons, C_1)
   }, !,
   ex2in(Val, [Ran|Result1], C_1, C_Out).
ex2in('~'(Term), Result, C_In, C_Out) --> !, [],
   ex2in_neg(Term, Result, C_In, C_Out).
ex2in('NONE'(None), Result, C, C) --> !, [],
   { unify([[None|_]|_], Result) }.
ex2in('$true$'(Term), _, C_In, C_Out) --> !,
   ex2in(Term, _, C_In, C_Out).
ex2in('!'(Prolog), Result, C, C) --> !, [],
   { is_type(prolog,TID),
     unify(Result, [[[[TID]]],prolog(Prolog)]) }.
ex2in(Term, Result, C, C, G_In, G_Out) :-
   atomic(Term), !,
   ( is_const(Term, TID) ->
	 unify([[[[TID]]], Term], Result),
	 G_In = G_Out
   ; is_type(Term, TID) ->
	 TID \== [],            % fail if Term == bottom 
	 ( var(TID) -> true     % Term == top
	 ; type_inference(Result,[[TID]])
	 ),
	 G_In = G_Out
   ; is_defined_sort(Term, 0) ->
	 ( sort_decl_table(check, Term, 0, [Decl], _, _, known) ->
	       type_inference(Result, Decl)
	 ; true ),
	 functor(NewTerm, Term, 1),
	 arg(1, NewTerm, Result),
	 'C'(G_In, NewTerm, G_Out)
   ).
ex2in(Term, Result, C_In, C_Out, G_In, G_Out) :-
   functor(Term, Name, Ar),
   ( sort_decl_table(check, Name, Ar, [Decl|Decls], _, _, known) ->
	 type_inference(Result, Decl)
   ; true ),
   Arity is Ar+1,
   functor(NewTerm, Name, Arity),
   arg(1, NewTerm, Result),
   'C'(G_Mid, NewTerm, G_Out),
   ex2in_args(1, Arity, Decls, NewTerm, Term, C_In, C_Out, G_In, G_Mid).

ex2in_disj([], _, []).
ex2in_disj([Term|R], Result, Clauses) :-
   ( ex2in(Term, ResH, [], Cons, Goals, [delayed_unify(Result,ResH)]) ->
           Clauses = [disj(Goals, Cons)|RR]
   ; RR=Clauses
   ), ex2in_disj(R, Result, RR).

assert_disjs([], _, _, _).
assert_disjs([disj(Goals,Cons)|R], DisjHead, DisjName, DisjArity) :-
   assert_disj(DisjHead, Goals, Cons, DisjName, DisjArity), !,
   assert_disjs(R, DisjHead, DisjName, DisjArity).

assert_disj(DisjHead, Goals, Cons, DisjName, DisjArity) :-
   \+ \+ ( do_delayed_unify(Goals, NewGs),
           ( check_constraints(Cons, NewCs) ->
		 rule_trans(DisjName, DisjArity, DisjHead, NewGs, NewCs,
			    tmp, default, Rule),
		 assert(Rule)
	   ; cuf_out(warning,
		     ['a disjunction branch has an empty denotation'])
	   )).

collect_vars(A, Vs, Vs) :-
   atomic(A), !.
collect_vars('V'(V), Seen, Vars) :- !,
   ( member4vars(Seen, V) ->
	 Vars=Seen
   ; Vars=[V|Seen]
   ).
collect_vars([A|Args], VIn, VOut) :-
  collect_vars(A, VIn, VMid),
  collect_vars(Args, VMid, VOut).
collect_vars('!'(PTerm), VIn, VOut) :- !,
  collect_vars_pterm(PTerm, VIn, VOut).
collect_vars(T, VIn, VOut) :-
  T =.. [_|Args],
  collect_vars(Args, VIn, VOut).


collect_vars_pterm(V, Seen, Vars) :- var(V), !,
   ( member4vars(Seen, V) ->
	 Vars=Seen
   ; Vars=[V|Seen]
   ).
collect_vars_pterm(T, VIn, VOut) :-
  functor(T,_Func,Ar),
  collect_vars_pterm_args(Ar, T, VIn, VOut).

collect_vars_pterm_args(0, _, Vars, Vars).
collect_vars_pterm_args(Ar, T, VIn, VOut) :-
  Ar1 is Ar-1,
  arg(Ar, T, TA),
  collect_vars_pterm(TA, VIn, VMid),
  collect_vars_pterm_args(Ar1, T, VMid, VOut).


ex2in_args(ArgNo, ArgNo, _, _, _, C, C) --> !, [].
ex2in_args(ArgNo, Arity, [Decl|Decls], NewHead, OldHead, C_In, C_Out) -->
   { ArgNo1 is ArgNo+1, 
     arg(ArgNo1, NewHead, Result),
     arg(ArgNo, OldHead, SubTerm),
     type_inference(Result, Decl)
   },
   ex2in(SubTerm, Result, C_In, C_Mid),
   ex2in_args(ArgNo1, Arity, Decls, NewHead, OldHead, C_Mid, C_Out).

ex2in_neg('V'(Var), Result, C_In, ['DIFF'(Var, Result)|C_In]) --> !.
ex2in_neg('N'(Number), Result, Cs, Cs) --> !,
   { is_const(Number,TID),    %% must succeed
     negate_TID(TID,NegTID),
     type_inference(Result, [[NegTID]]) }.
ex2in_neg('S'(String), Result, Cs, Cs) --> !,
   %% we don't assert 'STRING_CONST' facts for new strings here
   %% so hack TID here.
   { ( is_const(String,TID) ->    %% may have occurred in type axiom
	   true
     ; TID = string(String)
     ),
     negate_TID(TID,NegTID),
     type_inference(Result, [[NegTID]]) }.
ex2in_neg('!'(Prolog), Result, C_In, 
      ['DIFF'([[[[TID]]],prolog(Prolog)],Result)|C_In]) --> !, [],
   { is_type(prolog,TID) }.
ex2in_neg(Term, Result, C, C) --> !,
   { ( Term == bottom -> true
     ; Term == top -> fail
     ; ( is_const(Term, TID) ; is_type(Term, TID) )->
	     negate_TID(TID,NegTID),
	     type_inference(Result, [[NegTID]])
     ) }.

/* is_string now in utils.pl */

/*
t1 :-	cuf_extern2intern(word(X), Intern, Cons, Result).
t2 :-   cuf_extern2intern((word(X) ; sign), Intern, Cons, Result).
t3 :-   cuf_extern2intern(test(1), I, C, R),
	cuf_prove(all, I, [], C, C_out, _),
	pp_fs(R).
t4 :-   cuf_extern2intern("Hase"'|'"Igel", I, C, R),
	cuf_prove(all, I, [], C, C_out, _),
	pp_fs(R).
t5 :-   cuf_extern2intern("Hase"'|'top, I, C, R),
	cuf_prove(all, I, [], C, C_out, _),
	pp_fs(R).
t6(X) :-   cuf_extern2intern((!(term(X,Y)) ; f:X), I, C, R),
	cuf_prove(all, I, [], C, C_out, _),
	pp_fs(R).

*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: CUF's internal format to CUF feature terms    %%%
%%%      Created: Thu Apr 21 11:48:42 1994                      %%%
%%%     Modified: Fri Jul  8 20:52:44 1994 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.9  1994/07/08 19:17:15  michel
% i2e/4:
% * functor(Tag,~,1) case added for correct negation handling
% * calls in some cases the new i2e_fs/6 for eliminating unnecessary
%   `top' output
% write_atom/1:
% * `_' case added
%
% Revision 1.8  1994/07/05  16:34:29  michel
% write_atom/1: ~ case added
%
% Revision 1.7  1994/06/28  15:10:13  michel
% i2e_c_/1: 'POLY' case, warning now followed by a newline ("bug" fix)
%
% Revision 1.6  1994/06/22  12:34:10  michel
% write_atom/1: !/1 case added
%
% Revision 1.5  1994/06/13  12:30:08  michel
% i2e_c_/1:
% * calls pp_get_tag/2 now (replaces i2e_c_diff/2 which was removed)
%
% Revision 1.4  1994/05/31  13:23:31  michel
% new predicate i2e_c_diff/2:
% * 'STRUC' case added (bug fix)
%
% Revision 1.3  1994/05/31  08:52:49  michel
% pp_ft_a/1, 1st clause:
% * pp_ft_alist/1 call replaced by pp_ft_a/1 call (bug fix)
%
% Revision 1.2  1994/05/16  16:15:14  michel
% i2e/4:
% * new 'PROLOG'/1 case
% write_atom/1:
% * handles correct output of atoms
% alength/2:
% * checks now if symbol has to be printed with quotes
%

%%%  cuf_intern2extern(+FeatureStructure, +Constraints, +Goals,
%%%                    -FeatureTerm)
% the internal structure is assumed to be totally resolved wrt. goals
% at the moment!!!
cuf_intern2extern([], [], [], top) :- !.
cuf_intern2extern(FS, Cs, [], FT) :- !,
   prepare_printing(FS, Cs, PFS, PCs),
   i2e_c(PCs),
   i2e(PFS,0,_,FT), !.
cuf_intern2extern(_, _, _, _) :-
   cuf_out(error,['The conversion of unresolved goals is not yet implemented.']),
   fail.

i2e_c([]).
i2e_c([C|Ds]) :-
   i2e_c_(C),
   i2e_c(Ds).

i2e_c_('POLY'(_,_,_,_,_)) :- !,
   cuf_out(warning,['The conversion of polyfeature constraints cannot be supported!','$NL$']).
i2e_c_('DIFF'(X,Y)) :-
   pp_get_tag(X, '~'(R)),
   pp_get_tag(Y, R), !.
i2e_c_(X) :-
   cuf_out(error, ['The conversion of the following constraint failed:',X]).

i2e('STRUC'(Multi, Tag, Type, FS), In, Out, FT) :-
   ( Multi == 'MULTI' ->
	 ( var(Tag) ->
	       numbervars(Tag, In, Mid),
	       i2e_fs(FS, Type, Mid, Out, Tag, FT)
	 ; functor(Tag,~,1) ->
	       i2e_fs(FS, Type, In, Out, Tag, FT)
	 ; FT = Tag, In=Out
	 )
   ; /* var(Multi) -> */
     i2e_fs(FS, Type, In, Out, FT)
   ).
i2e('ATOM'(A), I, I, A).
i2e('REF'(Ref), I, I, Ref).
i2e('CYCLE'(Ref), I, I, Ref).
i2e('PROLOG'(Prolog), I, O, !(Prolog)) :-
   numbervars(Prolog, I, O).

i2e_fs([], top, I, I, Tag, Tag) :- !.
i2e_fs([], Type, I, I, Tag, '&'(Tag,Atom)) :- !,
   type_term2atom(Type,Atom).
i2e_fs(FS, Type, In, Out, Tag, '&'(Tag,Output)) :-
   i2e_fs(FS, Type, In, Out, Output).

i2e_fs([], Type, I, I, Atom) :- !,
   type_term2atom(Type,Atom).
i2e_fs(FS, Type, In, Out, Output) :-
   ( Type == top ->
	 i2e_fs(FS, In, Out, Output)
   ; cuf_list(FS) ->
	 ( Type == nelist ->
	       Output = FT
	 ; type_term2atom(Type,Atom),
	   Output = '&'(Atom, FT)
	 ),
	 i2e_list(FS, In, Out, FT)
   ; type_term2atom(Type,Atom),
     Output = '&'(Atom, FT),
     i2e_fs(FS, In, Out, FT)
   ).

i2e_fs([F:Val], In, Out, F:FT) :- !,
   i2e(Val, In, Out, FT).
i2e_fs([F:Val|FS], In, Out, '&'(F:FT1, FT2)) :-
   i2e(Val, In, Mid, FT1),
   i2e_fs(FS, Mid, Out, FT2).

i2e_list(['F':F, 'R':R], In, Out, [FV|RV]) :- !,
   i2e(F, In, Mid, FV),
   i2e(R, Mid, Out, RV).
i2e_list(FS, In, Out, FS) :-
   i2e_fs(FS, In, Out, FS).

pp_ft(FT) :-
   nl,tab(3),
   \+ \+ (numbervars(FT,0,_),
          pp_ft(FT, 3)),
   ttynl, !.

pp_ft(FT, _Tab) :-
   atomic(FT), !,
%   write(' '),
   write_atom(FT).
pp_ft('$VAR'(V), _Tab) :- !,
   write('$VAR'(V)).
pp_ft(F:V, Tab) :- !,
   alength(F, L),
   writeq(F),write(':'),
   NewTab is Tab+L+1,
   pp_ft(V, NewTab).
pp_ft(';'(D1,D2), Tab) :- !,
   write('('),
   NewTab is Tab+2,
   pp_ft(D1, ';', NewTab),
   nl,tab(Tab),write('; '),
   pp_ft(D2, ';', NewTab),
%   nl,tab(Tab),
   write(')').
pp_ft('&'(C1,C2), Tab) :- !,
   write('('),
   NewTab is Tab+1,
   pp_ft(C1, '&', NewTab),
   write(' &'),nl,tab(NewTab),
   pp_ft(C2, '&', NewTab),
%   nl,tab(Tab),
   write(')').
pp_ft('~'(FT), Tab) :- !,
   write(' ~'),
   NewTab is Tab+2,
   pp_ft(FT, NewTab).
pp_ft('!'(Prolog), _Tab) :- !,
   write('!('), write(Prolog), write(')').
pp_ft(String, _Tab) :-
   is_string(String), !,
   pp_string(String).
pp_ft([F|R], Tab) :- !,
   ( get_flag(print_lists, yes) ->
	 pp_ft_list([F|R], Tab)
   ; pp_ft('&'('F':F,'R':R), Tab)
   ).
pp_ft(Term, Tab) :-
   Term =.. [Func|Args],
   writeq(Func),write('('),
   alength(Func, L),
   NewTab is Tab+2+L,
   pp_ft_args(Args, NewTab),
%   NewTab1 is Tab+L,
%   nl,tab(NewTab1),
   write(')').

pp_ft('&'(C1,C2), '&', Tab) :- !,
   pp_ft(C1, '&', Tab),
   write(' &'),nl,tab(Tab),
   pp_ft(C2, '&', Tab).
pp_ft(';'(D1,D2), ';', Tab) :- !,
   pp_ft(D1, ';', Tab),
   nl,tab(Tab),write('; '),
   pp_ft(D2, ';', Tab).
pp_ft(FT, _, Tab) :-
   pp_ft(FT, Tab).

pp_ft_args([], _).
pp_ft_args([Arg|Args], Tab) :-
   pp_ft(Arg, Tab),
   ( Args == [] -> true
   ; write(','),nl,tab(Tab),
     pp_ft_args(Args, Tab)
   ).

pp_ft_list(List, Tab) :-
   write('['),
   ( ft_alist(List) ->
	 pp_ft_alist(List)
   ; NewTab is Tab+1,
     List = [F|R],
     pp_ft_list(F, R, NewTab)
   ).

ft_alist(A) :- atomic(A), !.
ft_alist([F|R]) :-
   ft_alist(F),
   ft_alist(R).
ft_alist('&'(A1,A2)) :-
   \+ functor(A1, ':', 2),
   ft_alist(A2).

pp_ft_alist([F]) :- !,
   pp_ft_a(F),
   write(']').
pp_ft_alist([F|R]) :- 
   pp_ft_a(F),
   ( functor(R, '.', 2) ->
         write(', '),
	 pp_ft_alist(R)
   ; write('|'),
     pp_ft_a(R),
     write(']')
   ).

pp_ft_a('&'(A,B)) :- !,
   pp_ft_a(A),
   write('&'),
   pp_ft_a(B).
pp_ft_a('$VAR'(V)) :- !,
   write('$VAR'(V)).
pp_ft_a(A) :-
   writeq(A).

pp_ft_list(F, R, Tab) :-
   ( R == [] ->
	 pp_ft(F, Tab),
	 write(']')
   ; R = [A|B] ->
         pp_ft(F, Tab),
	 write(','),nl,tab(Tab),
	 pp_ft_list(A, B, Tab)
   ; /* R \= [_|_] */
     pp_ft(F, Tab),
     NewTab is Tab-1,
     nl,tab(NewTab), write('|'),
     pp_ft(R,Tab),
     write(']')
   ).


write_atom(X) :-
   ( number(X) -> write(X)
   ; X == '_' -> write(X)
   ; symb_char(X) -> write(' '), write(X)
   ; functor(X,!,1) -> arg(1,X,P), write('!('),write(P),write(')')
   ; atom_chars(X, [F|_]), spec_char(F) -> 
        ( F =:= 0'~ -> write(' ') ; true ), write(X)
   ; writeq(X)
   ).

symb_char(-).
symb_char(+).
symb_char(*).
symb_char(/).
symb_char(\).
symb_char(^).
symb_char(?).
symb_char(@).
symb_char($).
symb_char(~).

spec_char(0'").
spec_char(0'().
spec_char(0'~).

:- dynamic 'A LENGTH'/2.

alength(Atom, Length) :-
  ( 'A LENGTH'(Atom, Length) -> true
  ; atom_chars(Atom, [F|Chars]),
    length([F|Chars], L),
    ( F =< 0'z, F >= 0'a -> X=0
    ; symbol_char(F) -> X=0
    ; X=2
    ), % X is for the two '
    Length is L+X,
    assert('A LENGTH'(Atom, Length))
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Joerg Junger                                  %%%
%%%      Purpose: Prolog-Interface for CUF                      %%%
%%%      Created: Wed May 11 14:25:00 1994                      %%%
%%%     Modified: Fri Mar 15 19:39:46 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.11  1996/04/09 13:17:34  jochen
% module prefix for trans_output facts;
%
% Revision 1.10  1994/07/20 15:34:44  junger
% adaption to the new constraints declaration format
%
% Revision 1.9  1994/07/13  08:25:15  jochen
% cuf_to_prolog/2 repared
%
% Revision 1.8  1994/07/01  13:25:28  jochen
% internal format of !-terms now has type prolog
%
% Revision 1.7  1994/06/28  11:29:55  junger
% - now +/- atomic/intern possible again for compatibility reasons
%
% Revision 1.6  1994/06/14  09:33:55  junger
% - some bugs fixed...
%
% Revision 1.5  1994/06/09  09:34:57  junger
% - cuf_to_atomic, atomic_to_cuf, make_args changed
%
% Revision 1.4  1994/05/19  14:44:43  junger
% - prepare_pp0 replaced by prepare_pp; result arguments reordered
%
% Revision 1.3  1994/05/19  10:21:44  junger
% - result parameter is now last argument of prolog call
%
% Revision 1.2  1994/05/11  14:16:09  junger
% - generation of bodys for clauses that handle prolog calls
%
% Revision 1.1  1994/05/11  13:57:35  junger
% - generation of bodys for clauses that handle prolog calls
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate_foreign_clause(+Goal,+Result,+FID,+EID)
%% generate_foreign_clause/4 is the top level goal for building clauses out of
%% the foreign predicate declarations. The actual CUF clause will be build in
%% compute_rules.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_foreign_clause(Goal, Result, FID, EID, Type) :-
	Goal =.. Decl_List,
	build_clause(Type, Decl_List, Result, Cuf_Goal, Body, Cons),
	functor(Cuf_Goal, Sort, Arity),
	assert(FID:trans_output(Sort, Arity, Cuf_Goal, Body, Cons, EID)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build_clause(+ListofNameandArgDeclarations,+ResultArgDeclaration,-CUFGoal,
%%              -BodyofCUFclause,-ConstraintsArguments)
%% build_clause/5 takes the name and the argument declarations for a foreign
%% predicate and delivers the body of the CUF clause which will be constructed
%% for this predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_clause(normal, [Name|Args], Result, Cuf_Goal, Body, COut/CIn) :-
	make_args([Result|Args], Vor_Prolog_Call, Nach_Prolog_Call, 
		  Cuf_Goal_Arguments, Prolog_Call_Arguments, Unify_Liste,
		  COut/CIn, CIn),
	Prolog_Call_Arguments=[Resultarg|Rest],
	append(Rest, [Resultarg], Prolog_Call_Arguments1),
	P_Goal =.. [Name|Prolog_Call_Arguments1],
	Cuf_Goal =.. [Name|Cuf_Goal_Arguments],
	append(Vor_Prolog_Call,[P_Goal|Nach_Prolog_Call],Goallist1),
	append(Goallist1, Unify_Liste, Goallist),
	mk_body(Goallist, Body).


build_clause(constraints, [Name|Args], Result, Cuf_Goal, Body, COut/CIn) :-
	make_args([Result|Args], Vor_Prolog_Call, Nach_Prolog_Call, 
		  Cuf_Goal_Arguments, Prolog_Call_Arguments, Unify_Liste,
		  COut/Cons, CIn),
	Prolog_Call_Arguments=[Resultarg|Rest],
	append(Rest, [CIn/Cons, Resultarg], Prolog_Call_Arguments1),
	P_Goal =.. [Name|Prolog_Call_Arguments1],
	Cuf_Goal =.. [Name|Cuf_Goal_Arguments],
	append(Vor_Prolog_Call,[P_Goal|Nach_Prolog_Call],Goallist1),
	append(Goallist1, Unify_Liste, Goallist),
	mk_body(Goallist, Body).


make_constraints(CIn/COut, CIn1/COut, CIn1/CIn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_args(+ListofArgumentDeclarations, -ListofSubgoalsbeforePrologCall,
%%           -ListofSubgoalsafterPrologCall, -ListofArgumentsofCUFgoal,
%%           -ListofArgumentsofPrologCall, -ListofCUFUnifyCalls, 
%%           ConstraintsArgumentafterprologcall, +Constraintbeforeprologcall
%% make_args/8 goes through the list of argument declarations of a foreign
%% predicate and delivers the arguments and subgoals for construction of the 
%% CUF clause for the foreign predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_args([],[],[],[],[],[],A/A,_).
make_args([intern|Args], V_P, N_P, [X|C_C], [X|P_C], U_L, Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args([atomic|Args], [cuf_to_atomic(X,Y,Flag)|V_P], 
	  [(Flag =:= 0 -> cuf_atom(Y,Z);true)|N_P], [X|C_C], [Y|P_C], 
          [(Flag =:= 0 -> unify(Z,X);true)|U_L], Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args([prolog|Args], [cuf_to_prolog(X,Y)|V_P], N_P, [X|C_C], 
                         [Y|P_C], U_L, Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args([F|Args], V_P, N_P, C_C, P_C, U_L, Cons, Consin) :-
	F =.. [Func, Argtyp],
        make_args(Func, Argtyp, Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).

make_args(-,Arg, Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin) :- 
	make_args_neg(Arg, Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args(+,Arg, Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin) :- 
	make_args_pos(Arg, Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).

make_args_pos(extern, Args, [cuf_to_extern(A, Consin, B)|V_P], N_P, 
	      [A|C_C], [B|P_C], U_L, Cons, Consin) :- 
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args_pos(intern, Args, V_P, N_P, [A|C_C], [A|P_C], U_L, Cons, Consin) :- 
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args_pos(atomic,Args, [cuf_to_atomic(X,Y,Flag)|V_P], 
	  [(Flag =:= 0 -> cuf_atom(Y,Z);true)|N_P], [X|C_C], [Y|P_C], 
          [(Flag =:= 0 -> unify(Z,X);true)|U_L], Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args_pos(prolog,Args, [cuf_to_prolog(X,Y)|V_P], N_P, [X|C_C], 
                         [Y|P_C], U_L, Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).

make_args_neg(extern, Args, V_P, [cuf_ex_in(PrArg, ResArg, CIn, COut)|N_P], 
       [CUFArg|C_C], [PrArg|P_C], [unify(CUFArg,ResArg)|U_L], COut1/CIn, Consin) :- 
	make_args(Args, V_P, N_P, C_C, P_C, U_L, COut1/COut, Consin).
make_args_neg(intern, Args, V_P, N_P, [A|C_C], [A|P_C], U_L, Cons, Consin) :- 
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args_neg(atomic,Args, [cuf_to_atomic(X,Y,Flag)|V_P], 
	  [(Flag =:= 0 -> cuf_atom(Y,Z);true)|N_P], [X|C_C], [Y|P_C], 
          [(Flag =:= 0 -> unify(Z,X);true)|U_L], Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
make_args_neg(prolog,Args, [cuf_to_prolog(X,Y)|V_P], N_P, [X|C_C], 
                         [Y|P_C], U_L, Cons, Consin) :-
	make_args(Args, V_P, N_P, C_C, P_C, U_L, Cons, Consin).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_to_extern(+CUF_Term, +ConsIn, -Extern_Term)
%% cuf_to_extern/3 converts a CUF term from internal format to 
%% external format. Due to the conversion routine cuf_intern2extern
%% which works destructively, it is necessary to wrap the conversion
%% call with not-not, to prevent variable bindings. This means that
%% it is not possible to have several arguments with shared variables
%% in the foreign language call, when one of the arguments is declared
%% +extern, because the connection between these structures is destroyed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_to_extern(CUF_Term, ConsIn, Extern_Term) :-
	\+ \+ (cuf_intern2extern(CUF_Term, ConsIn, [], Extern_Term),
	       assert(cuf_in_ex(Extern_Term))),
	retract(cuf_in_ex(Extern_Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_to_atomic(+CUFTerm, -AtomicPrologTerm, -Flag)
%% CUF atoms are konverted to Prolog atoms,
%% all other CUF terms are 'converted' into
%% Prolog variables. If a CUF atom is converted, Flag is set 1 other-
%% wise Flag is set 0. With this flag reconversion and unification of 
%% arguments of the prolog call is controlled; if an atom is used as
%% input for the prolog call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_to_atomic(CUF_Term, Atomic_Term, Flag) :-
	cuf_atom(Atomic_Term, CUF_Term) ->
	Flag = 1
	; Flag = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_ex_in(PrologArg, Result, ConsIn ConsOut)
%% Terms in CUF external format are converted to CUF internal format;
%% the conversion is not allowed to introduce new CUF goals; this will lead
%% to an error message, whereas constraints resulting from the conversion
%% will be appended to the list of constraints 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_ex_in(PrologArg, Result, ConsIn, ConsOut) :-
	cuf_extern2intern(PrologArg, GoalList, Cons, Result),
	( GoalList == [] -> true
	; cuf_out(error,['conversion of -extern argument failed because of illegal introduction of CUF goals']),
	  fail
	),
	append(Cons, ConsIn, ConsOut).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cuf_to_prolog(+PrologTerminCUFFormat, -PrologTerm)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_to_prolog(FS,T) :-
	is_type(prolog,TID),
	unify([[[[TID]]],prolog(T)],FS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: building of index table during compilation    %%%
%%%      Created: Mon Sep 21 09:27:45 1992                      %%%
%%%     Modified: Tue Mar 19 17:37:39 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1996/04/09 12:51:37  jochen
% computation of multiply used indeces made more efficient
%
% Revision 1.3  1994/05/11  13:07:35  michel
% compute_tables/1: trans_output assertion with FID,EID only (no tmp anymore)
%
% Revision 1.2  1994/02/03  11:30:13  junger
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9
%
% Revision 1.1  1993/11/08  13:50:06  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Thu Feb 25 18:02:43 1993 (michel) : error output changed
% Thu Dec 17 15:39:13 1992 (michel) : bug in assert_rule_entry_for_table removed
% Tue Dec 15 18:22:25 1992 (michel) : constraints list added
% Thu Dec  3 11:05:15 1992 (michel) : error handling changed

%%%%%%%  compute_tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% triggered by index_table/1 facts
%%%  - collects multiple entries for same index in tables (to prevent
%%%    uncontrolled nondeterminism)
%%%  - checks for atomic indeces
%%%  - generates table names, asserts rule entries for tables
%%%  - asserts index_table_name/4 and trans_output/7 facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic index_table_name/4.

compute_tables(FID) :-
    % check if there are tables to be build
    index_table(Name, Arity, FID),
    % generate a new table name and assert a index_table_name/4 fact
    Arity1 is Arity+1,
    ( index_table_name(Name, Arity1, TableName, TableArity) -> true
    ; gensym(index_table,TableName),
      TableArity is Arity+8,
      assert(index_table_name(Name, Arity1, TableName, TableArity))
    ),
    % get a head of the predicate in the table
    functor(Head, Name, Arity1),
    Head =.. [_, Result, Arg1|RestArgs],
    % build rule entry for table access and assert it 
    assert_rule_entry_for_table(Head, TableName, TableArity, FID),
    % for each index (Arg1) do
    bagof(DBRef,
	  Result^(RestArgs^(Body^(Cons^(EID^
	    clause(FID:trans_output(Name, Arity1, Head, Body, Cons, EID),
		   true,
		   DBRef))))),
	  Bag),
    ( (Arg1 = [_,Atom], atomic(Atom)) ->
	   dot,
	   Bag = [_,_|_],		% at least two entries for one index
	   gensym(genDisj, TableName1),
	   Arity0 is Arity-1,
	   add_def_symbol(TableName1, Arity0, sort, FID, _),
	   def_occ_table(TableName1, Arity0, sort, FID, _),
	   functor(Headnew, TableName1, Arity), 
	   Headnew =.. [_,Result|RestArgs],
	   assert(FID:trans_output(Name, Arity1, Head, [Headnew], [], _EID)),
           member(DBRef, Bag),
           clause(FID:trans_output(Name, Arity1, Head, Body, Cons, EID),
		  true,
		  DBRef),
           erase(DBRef), %% much faster than with retract
	   assert(FID:trans_output(TableName1, Arity, Headnew, Body, Cons, EID)),
	   fail
    ;  Bag = [DBRef|_],
       clause(FID:trans_output(_,_,_,_,_,EID),true,DBRef),
       cuf_out(error, ['first argument should be atomic for indexing (ignoring clause):',
                         '$E$'(FID,EID)]),
       member(DBRef,Bag),
       erase(DBRef),
       fail   
    ).
compute_tables(_FID).

%%%%%%%  assert_rule_entry_for_table(+Head, +Arity, +TableName)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds rule entry for table access 
%%%  asserts rule/6 clause
%%%  CAUTION: assumes knowledge of internal fs data structures !!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_rule_entry_for_table(Head, TableName, TableArity, FID) :-
    \+ \+ ( Head =.. [_, Result, [TypeInfo, Atom]|RArgs],
	    functor(TableGoal, TableName, TableArity),
	    TableGoal =.. [_, Atom, FID, EID, TypeInfo0, NodeID^GoalsIn, GoalsOut,
	                   ConsIn, ConsOut|TArgs],
	    mk_unify_goals([Result|RArgs], TArgs, UnifyGoals),
	    assert(('CUF clause'(Head, FID, EID, 0, NodeID, GoalsIn, GoalsOut,
                    ConsIn, ConsOut) :-  TableGoal,
		                         unify([TypeInfo|_],[TypeInfo0|_]),
		                         UnifyGoals))
          ).

%%%%%%%  mk_unify_goals(+GoalArgs, +TableArgs, -UnifyGoals),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  agreement of variables by unify/2 subgoals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mk_unify_goals([GArg], [TArg], unify(GArg,TArg)) :- !.
mk_unify_goals([GArg|GArgs], [TArg|TArgs], (unify(GArg,TArg), UGoals)) :- 
    mk_unify_goals(GArgs, TArgs, UGoals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: treating feature polymorphism                 %%%
%%%      Created: Fri Sep 18 11:15:44 1992                      %%%
%%%     Modified: Thu Mar 14 16:48:38 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.3  1996/03/14 17:03:18  jochen
% better recognition of polyfeatures (not poly if all same range)
%
% Revision 1.2  1994/05/11 12:55:33  michel
% consistency_check_/4: generated axioms get file ID `sys' now
%
% Revision 1.1  1993/11/08  13:50:07  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Wed Jun  2 18:25:20 1993 (michel) : treatment of ranges changed because of top == _
% Wed Jun  2 16:16:34 1993 (michel) : consistency_check/2 --> consistency_check/3
% Mon May  3 17:17:39 1993 (michel) : feature classification changed
% Mon Feb  8 14:37:00 1993 (michel) : 'POLY'/4 --> 'POLY'/5
% Fri Feb  5 19:20:36 1993 (michel) : monofeature declaration tests added
% Thu Feb  4 11:11:25 1993 (michel) : 'POLY'/3 & 'POLY'/4
% Mon Jan 18 16:58:24 1993 (michel) : consistency_check/2 --> consistency_check/3
% Thu Dec  3 12:33:38 1992 (michel) : output changed
% Fri Nov 27 12:30:34 1992 (michel) : checking_polyfeatures in closure part cutted
% Mon Nov  9 11:53:25 1992 (michel) : polyfeature type checking changed

treating_feature_polymorphism :-
	cuf_out(message, ['classifying features:']),
	feature_classification,
	polyfeature_declaration_tests,
	monofeature_declaration_tests.

feature_classification :-
	setof(F, Dom^(Ran^(FID^(EID^feat_decl(F,Dom,Ran,FID,EID)))), Feats),
	member(Feat, Feats),
	retractall_feat_entries1(Feat),
	classify_feature(Feat),
	dot,
	fail.
feature_classification.

:- dynamic poly_feat/2,
	   mono_feat/3.

polyfeature_declaration_tests :-
   ( poly_feat(_,_) ->
	 cuf_out(message, ['testing polyfeature declarations: ']),
%	 trace,
	 checking_polyfeatures,
	 build_polyfeature_tests
   ; true ).

monofeature_declaration_tests :-
   ( mono_feat(_,_,_) ->
	 cuf_out(message, ['testing monofeature declarations: ']),
	 checking_monofeatures
   ; true ).

%%%  classify_feature(+Feature)
%%%  if Feature defined in multiple definitions then
%%%  poly_feat(Feature, AllDomainRangePairs) will be asserted; 
%%%  else a fact of form mono_feat(Feature, Domain, Range).
classify_feature(Feat) :-
    feat_decl(Feat,_,Ran0,_,_), !,
    ( ( var(Ran0),
        forall(feat_decl(Feat,Dom,Ran,_,_),
    	   ( var(Ran), Dom = [[_]] )) ) ->
               %% all ranges top, all domains simple
               findall(DomLit,feat_decl(Feat,[[DomLit]],_,_,_),DomLits),
	       assert(mono_feat(Feat,[DomLits],_))
    ; ( nonvar(Ran0),
        forall(feat_decl(Feat,Dom,Ran,_,_),
    	   ( Ran==Ran0, Dom = [[_]] )) ) ->
               %% all ranges same, all domains simple
	       findall(DomLit,feat_decl(Feat,[[DomLit]],_,_,_),DomLits),
	       assert(mono_feat(Feat,[DomLits],Ran0))
    ;
	findall([Dom,Ran], 
		feat_decl(Feat, Dom, Ran, _FID, _EID),
		DomRans),
	( DomRans = [[D,R]] -> 
	      assert(mono_feat(Feat, D, R))
	; cuf_out(message, ['polyfeature found:',Feat,'$NL$']),
	  assert(poly_feat(Feat, DomRans))
	)
    ).

checking_monofeatures :-
   mono_feat(Feat, Dom, Ran),
   ( check_formula(Dom, 1) -> true
   ; cuf_out(error, ['domain declaration of feature',Feat,
                     'has empty denotation']),
     retractall_feat_entries1(Feat),
     fail ),
  ( var(Ran) -> true
  ; check_formula(Ran, 1) -> true   % Range is top
  ; cuf_out(error, ['range declaration of feature',Feat,
                    'has empty denotation']),
     retractall_feat_entries1(Feat),
     fail
  ), dot,
  fail.
checking_monofeatures.

%%%  checking_polyfeatures
%%%  for each polymorphically defined feature it will be tested
%%%  if the definitions are consistent.
checking_polyfeatures :-
	poly_feat(Feat, DomRans),
	consistency_check(DomRans, Feat, Flag),
	( Flag == fail ->
	      cuf_out(error, ['type error in declaration of feature',Feat]),
	      retractall_feat_entries1(Feat),
	      fail
	; Flag == new_axiom ->
	      checking_polyfeatures
	% ; fail
	).
checking_polyfeatures.

%%%  consistency_check(+DomainRangePairs, +Feat, -Flag)
%%%  consistency check for domains and ranges of one polyfeature
consistency_check(DomRans, Feat, Flag) :-
	( collect_ranges(DomRans, Rans, []), % builds the conjunction
	  check_formula(Rans, 1), !,     % ranges are all unifiable?
	  dot
	; collect_domains_ranges(DomRans, Doms, Rans),
	  consistency_check(Rans, Doms, Feat, Flag), ! 
	; Flag = fail).

%%%  consistency_check(+Domains,+Ranges,+Feat,-Flag)
consistency_check([], [], _Feat, _Flag) :- !.
consistency_check([_], [_], _Feat, _Flag) :- !.
consistency_check(Domains, Ranges, Feat, Flag) :-
	consistency_check_(Flag, Domains, Ranges, Feat).

consistency_check_(Flag, [R1, R2|Rs], [D1, D2|Ds], Feat) :-
    var(Flag), !,
    ( var(R1) -> CF = 2
    ; var(R2) -> CF = 1
    ; check_formulas(R1,R2,CF)
    ), dot,
    ( CF == 0 ->
	  ( check_formulas(D1, D2, 0) ->
	        consistency_check([R1|Rs], [D1|Ds], Feat, Flag), !,
	        consistency_check([R2|Rs], [D2|Ds], Feat, Flag)
	  ; append(D1, D2, DisjDom),
	    neg_cnf2cnf(DisjDom, NewAxiom),
	    add_axioms(NewAxiom, AF),
	    ( AF == 0 -> Flag = fail
	    ; Flag = new_axiom,
	      assert_if_new(added_axiom(sys, Feat, NewAxiom))
	    )
	  )
    ; ( CF == 1 ; CF == 2 ) ->
	    consistency_check([R1|Rs], [D1|Ds], Feat, Flag), !,
	    consistency_check([R2|Rs], [D2|Ds], Feat, Flag)
    ; /* CF == 3 -> */
      append(R1, R2, NewR),
      append(D1, D2, NewD),
      consistency_check([R1|Rs], [D1|Ds], Feat, Flag), !,
      consistency_check([R2|Rs], [D2|Ds], Feat, Flag), !,
      consistency_check([NewR|Rs], [NewD|Ds], Feat, Flag)
    ).
consistency_check_(fail, _, _, _).
consistency_check_(new_axiom, _, _, _).

%%%  build_polyfeature_tests
%%%  builds interpreter tests for declaration checking
%%%  goals will be inserted in the clause compiling step
%%%  (for further explanation see my diploma thesis)
build_polyfeature_tests :-
	poly_feat(Feat, DomRans),
	compute_feature_domains(Feat, DomRans),
	build_polyfeature_tests(DomRans, DomVar, RanVar, Goals, []),
	assert(poly_feat_check(Feat, DomVar, RanVar, Goals)),
	dot, fail.
build_polyfeature_tests.

:- dynamic feat_doms/2.

compute_feature_domains(Feat, DomRans) :-
	collect_domains(DomRans, Doms, []),   % builds the disjunction (!)
	domains2cnf(Doms, CNF),
	assert(feat_doms(Feat, CNF)), !, dot.

build_polyfeature_tests([], _, _) --> [].
build_polyfeature_tests([[Dom,Ran]|DomRans], DomVar, RanVar) -->
	{ gensym(poly, DisjName),
	  neg_cnf2cnf(Dom, NegDom),
	  ( var(Ran) -> NegRan=[[1],[-1]]
	  ; neg_cnf2cnf(Ran, NegRan) ),
	  PolyCall = 'POLY'(DisjName, DomVar, RanVar),
	  PolyTest = 'POLY'(DisjName, Dom, Ran, NegDom, NegRan),
	  assert(PolyTest)
	}, [PolyCall],
	build_polyfeature_tests(DomRans, DomVar, RanVar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_domains_ranges([], [], []).
collect_domains_ranges([[D,R]|DomRans], [D|Doms], [R|Rans]) :-
	collect_domains_ranges(DomRans, Doms, Rans).

/* causes an instantiation error if range is top because the 
   internal representation of top is the anonymous variable
collect_ranges([]) --> [].
collect_ranges([[_,R]|DomRans]) -->
	R,
	collect_ranges(DomRans).
*/

collect_ranges([]) --> [].
collect_ranges([[_,R]|DomRans]) -->
        { ( var(R) -> Out = []  % ignore top
          ; Out = R)
	},
	Out,
	collect_ranges(DomRans).

collect_domains([]) --> [].
collect_domains([[D,_]|DomRans]) -->
	[D],
	collect_domains(DomRans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: checking denotations of types                 %%%
%%%      Created: Fri Sep 18 11:19:25 1992                      %%%
%%%     Modified: Tue Jun 21 16:54:33 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.5  1994/06/21 14:58:54  jochen
% now we generate only one common warning for all indetermate atoms indicating the types
% that cause indeterminacy
%
% Revision 1.4  1994/05/09  12:54:56  michel
% new error messages
%
% Revision 1.3  1994/01/20  16:51:06  jochen
% producing better error msg when built-in types get inconsistent
%
% Revision 1.2  1994/01/14  11:08:19  jochen
% Type consistency checking, as well as new atom (domain)
% determinateness checking now done in SAT module. Only production of
% error messages left here.
%
% Revision 1.1  1993/11/08  13:50:08  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Fri Feb  5 18:45:47 1993 (michel) : constant checking added
% Wed Dec  2 17:36:42 1992 (michel) : error handling changed


type_consistency_check :-
	check_n_close_axioms(L),
	handle_type_check_errors(L),
	( setof(Type, Err^(AtomID^(TID^( ( Err=indet_atom(AtomID, TID)
				       ; Err=indet_atom_domain(AtomID, TID)
				       ),
				       member(Err,L),
				       type_code(TID,Type)
				     ))),
		Set)
	-> cuf_out(warning, ['these types cause indeterminate atoms (at least):']),
	   forall(member(Type,Set),cuf_out(message,[Type])),
	   nl
	; true
	),
	!.
type_consistency_check :-
	write(woooops), nl.


handle_type_check_errors([]).
handle_type_check_errors([E|L]) :-
	handle_type_check_errors(L), %% process from back
	handle_type_check_error(E).

handle_type_check_error(incons(TID)) :- !,
	(TID == 0 -> Type=other_afs ; type_code(TID,Type)),
        ( ( is_basic_type(Type,_) ; is_basic_const(Type,_) ) ->
	      cuf_out(error,
		      ['built-in type/constant restricted to be empty:',
	               Type])
	; cuf_out(error, ['the definition of type', Type, 'is inconsistent'])
	),
	set_flag(inconsistent_types, yes).
handle_type_check_error(_).
% handle_type_check_error(indet_atom(AtomID, TID)) :-
% 	type_code(AtomID,Atom),
% 	type_code(TID,Type),
% 	cuf_out(warning, ['indeterminate atom:',Atom,'$NL$',
% 	                '%%% (e.g., consistent with ',Type,' as well as its complement)']),
% 	set_flag(indeterminate_atoms, yes).
% handle_type_check_error(indet_atom_domain(AtomID, TID)) :-
% 	type_code(AtomID,Atom),
% 	type_code(TID,Type),
% 	cuf_out(warning, ['indeterminate atom domain:',Atom,'$NL$',
% 	                '%%% (e.g., consistent with ',Type,' as well as its complement)']),
% 	set_flag(indeterminate_atoms, yes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: expansion of deterministic goals              %%%
%%%      Created: Sat Jun 20 19:54:35 1992                      %%%
%%%     Modified: Tue Feb  8 11:37:50 1994 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.8  1994/07/21 15:06:11  junger
% stupid bug fixed
%
% Revision 1.7  1994/07/21  15:04:53  junger
% \= (Arg1, [_,'***']) resembled by \+ Arg1=[_,'***']
%
% Revision 1.6  1994/06/09  13:57:56  junger
% -in expand_det_in_rule(FID) \+ cuf_foreign added, to prevent deterministic
% 	expansion of foreign goals
%
% Revision 1.5  1994/06/01  15:08:47  jochen
% generate debug/trace info also for determin. failing preds
% (when not_twice returns 0)
%
% Revision 1.4  1994/05/18  10:34:13  junger
% - not_twice changed: uses now foreign counter defined in counter.c
%
% Revision 1.3  1994/02/18  12:37:57  michel
% * expand_all_det/10 changed:
%   rule_trans2/3 is called to reduce the number of constraints (if
%   possible).
% * new_goalIDs_in_body/5 changed:
%   reduce_parameters/4 is called to minimize asserted structures (type
%   infos)
%
% Revision 1.2  1994/02/03  10:44:10  junger
%  - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9)
%  - expand_det for compiler also generates new subgoal numbering
%  - deterministic resolution steps now also count to depth of proof
%  - using goaleq for testing termination condition in expand_goal/10
%  - deterministic expansion now also calls resolve/10 (merged
%    expand_det_goal/10 to resolve/10)
%
% Revision 1.1  1993/11/08  13:50:11  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Tue Jul 27 15:48:02 1993 (jochen) : use '$GOAL' wrapper for goals in cuf_out
% Wed Apr  7 13:31:25 1993 (michel) : occur_table --> 'DEF OCC'
% Mon Dec 21 20:07:18 1992 (michel) : expand_det_goal: disj case
% Wed Dec 16 10:54:32 1992 (michel) : several changes for constraint handling
% Tue Dec 15 11:54:02 1992 (michel) : expand_det_goal changed
% Mon Dec 14 14:34:27 1992 (michel) : expand_det/4 added
% Fri Dec 11 18:26:16 1992 (michel) : expand_det/4 --> expand_det/5
% Fri Dec 11 11:16:58 1992 (michel) : some more !s
% Thu Dec 10 16:53:19 1992 (michel) : expand_one_goal/4 added
% Thu Dec 10 11:59:18 1992 (michel) : expand_det/5 --> expand_det/4 + recursion 
% Thu Dec  3 11:55:48 1992 (michel) : output changed
% Fri Nov 20 11:49:57 1992 (michel) : expand_det0/7, keep/6, forget/6 added
% Mon Nov  9 12:02:26 1992 (michel) : 'POLY'/3 handling added
% Thu Nov  5 11:57:20 1992 (michel) : not_twice changed
% Thu Nov  5 11:45:41 1992 (michel) : expand_det_in_rule/1 changed







%%%%%%%  expand_det(+FID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  expands deterministic goals in rule or table entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_det(FID) :-
    get_flag(debug,Old),
    set_flag(debug,no),
    expand_det_in_rule(FID),
    expand_det_in_table(FID),
    set_flag(debug,Old).

%%%%%%%  expand_det_in_rule(+FID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  expands deterministic goals in rule entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_det_in_rule(FID) :-
   'DEF OCC'(Name, Arity0, sort, FID, EID),
   Arity is Arity0+1,
   \+ clause(index_table_name(Name, Arity, _, _), true),
   \+ clause(cuf_foreign(Name,Arity,_), true),
   functor(Goal, Name, Arity),
   'CUF clause'(Goal, FID, EID, No, NodeID, Body, [], Cons, []),
   ( expand_all_det(Goal, Body, Cons, NodeID, NewHead,
		    NewBody, BodyRest, NewCons, ConsRest, NewGs) ->
	      retract(('CUF clause'(Goal, FID, EID, No, _, _, _ , _, _) :- _)),
	      assertz(('CUF clause'(NewHead, FID, EID, No, NodeID, NewBody, BodyRest,
			    NewCons, ConsRest) :- NewGs)),
	      dot
   ; cuf_out(warning, ['expansion failed for sort',Name/Arity0,
                       '$E$'(FID,EID)])
   ), fail.
expand_det_in_rule(_).

%%%%%%%  expand_det_in_table(+FID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   expands deterministic goals in table entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_det_in_table(FID) :-
    clause(index_table_name(Name, Arity, TableName, TableArity), true),
    functor(TableEntry, TableName, TableArity),
    TableEntry =.. [_, Atom, FID, EID, TypeInfo, NodeID^Body, [], Cons, []|RArgs],
    clause(TableEntry, true),
    ( expand_all_det(table, Body, Cons, NodeID, _, NewBody, BodyRest,
		     NewCons, ConsRest, _) ->
         NewEntry =.. [TableName, Atom, FID, EID, TypeInfo,
                       NodeID^NewBody, BodyRest, NewCons, ConsRest|RArgs],
         retract(TableEntry),
         assert(NewEntry),
	 dot
    ;  cuf_out(warning, ['expansion failed for sort',Name/Arity,
                         '$E$'(FID,EID)])
    ), fail.
expand_det_in_table(_).

%%%%%%%  expand_all_det(+Head, +Body, -NewHead, -NewBody,
%%%%%%%                 -Rest, -NewGoals) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  expands all deterministic subgoals 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_all_det(Head, Body, Cons, NodeID, NewHead, NewBody, BodyRest,
               NewCons, ConsRest, NewGoals) :- 
    cuf_prove(det_only, Body, Body1, Cons, Cons1, _),
    new_goalIDs_in_body(Body1, NodeID, 1, BodyRest, NewBody),
    rule_trans2(Cons1, NewCons, ConsRest),
    functor(Head, Name, Arity),
    functor(NewHead, Name, Arity),
    head_vars(Arity, Head, [], NewHead, GoalList, []),
    mk_body(GoalList, NewGoals), !.


new_goalIDs_in_body([], _, _, R, R).
new_goalIDs_in_body([WGoal|R], NodeID, SubGNo, BodyRest, [WGoal1|R1]) :-
    wrap_goal(Goal, WGoal),
    goal_flag(WGoal, Flag),
    functor(Goal, Name, Arity),
    reduce_parameters(Name, Arity, Goal, NewGoal),
    wrap_goal(NewGoal, WGoal1),
    goal_flag(WGoal1, Flag),
    goalID(WGoal1, NodeID:SubGNo),
    SubGNo1 is SubGNo+1,
    new_goalIDs_in_body(R, NodeID, SubGNo1, BodyRest, R1).
    



%%%%%%%  expand_det(+GoalsToExpand, -ExpandedGoals, +ConstraintsIn,
%%%%%%%             -ConstraintsOut, +Indentation, +MaxDepth, -NewIndentation)
%%%%%%%  expand_det(+GoalsToExpand, +NonDetGoals, -ExpandedGoals,  
%%%%%%%             +ConstraintsIn, -ConstraintsOut, +Indentation,
%%%%%%%             +MaxDepth, -NewIndentation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  expands all deterministic goals in GoalsToExpand recursively
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Input:  GoalsToExpand  - a list of goals
%%%          NonDetGoals    - a list of goals classified to be not
%%%                           deterministic expandable
%%%          ConstraintsIn  - a list of constraints
%%%          Indentation    - tabular for tracing
%%%          MaxDepth       - bound for  recursion
%%%  Output: ExpandedGoals  - a list of goals
%%%          ConstraintsOut - a new list of constraints
%%%          NewIndentation - new tabular
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Expansion takes place in a depth first manner and it is a kind of
%  computing a closure. The idea is to pass each goal only once after
%  the last goal expansion by saving its (the expansion's) occurence
%  position in a difference list. Additionaly we hope that in the clause
%  body of a just expanded goal there is a big chance to expand new goal.
%  At least the same chance as in the rest of the goal list. So we use
%  a depth first strategy.
%  1. Take the first goal of GoalsToExpand and try to expand it. 
%     If expansion will be non-deterministic, try next one and
%     so on, until one goal expands deterministically.
%  2. Divide the goal list in two parts: the part from the beginning of
%     the list to the last goal which was tested to be non-deterministic
%     expandable (prefix), and the part from the expanion of the first
%     deterministic goal to the end of the goal list (tail). The subgoals
%     of the deterministic goal are in front of the tail.
%  3. After checking (incl. goal expansion) the tail recursively,
%     we get a new tail of the goal list of already checked goals and
%     a new prefix of the goal list of goals to be checked for
%     deterministic expansion. If no expansion takes place in the
%     recursive treatment of the prefix, the tail can be ignored
%     for further proceeding because it was checked already. So finish.
%     If not, look forward for the next prefix and tail of the goal list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_det(InGoals, OutGoals, ConsIn, ConsOut, Ind, MaxDepth, NewInd) :-
   expand_det(InGoals, [], OutGoals, ConsIn, ConsOut, Ind, MaxDepth, NewInd).

%expand_det([], _, [], Cons, Cons, Ind, _, Ind) :- !.
%expand_det(Goals, NonDetGoals, Goals, Cons, Cons, Ind, _, Ind) :-
%   NonDetGoals == Goals, !.    % all goals already tested to be non-det.

expand_det(Goals, NonDetGoals, ExpGoals, ConsIn, ConsOut,
           Ind, MaxDepth, NewInd) :-
   expand_goal(Goals, NonDetGoals, ExpGoals1, ConsIn, ConsOut1,
	       Ind, MaxDepth, Ind1, Goals, NonDetGs1),
   ( Ind =:= Ind1 ->
	 (ExpGoals=Goals, NewInd = Ind, ConsOut=ConsIn)
   ; expand_det(ExpGoals1, NonDetGs1, ExpGoals, ConsOut1, ConsOut,
		Ind1, MaxDepth, NewInd)
   ).

%expand_det_aux(Goals, Goals, Ind, Ind, Cons, Cons).

%%%  expand_goal(+GoalsToExpand, +NonDetGoals, -ExpandedGoals,
%%%             +ConstraintsIn, -ConstraintsOut,
%%%             +Indentation, +MaxDepth, -NewIndentation, +NonDetTestGoals,
%%%             -TailOfNonDetGoals)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Input:  GoalsToExpand   - a list of goals
%%%          NonDetGoals     - a list of goals classified to be not
%%%                            deterministic expandable
%%%          ConstraintsIn   - a list of constraints
%%%          Indentation     - tabular for tracing
%%%          NonDetTestGoals - the goal list
%%%          MaxDepth        - recursion-bound
%%%  Output: ExpandedGoals     - a list of goals
%%%          ConstraintsOut    - a new list of constraints
%%%          NewIndentation    - new tabular
%%%          TailOfNonDetGoals - non-deterministic tail of the
%%%                              goal list (fully expanded)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% expand_goal geaendert am 20 12.93 von Joerg
%%%  fuer neues goal-format g(ParentID,NthSubnode,Goal,Flag)

expand_goal([], _, [], Cons, Cons, Ind, _, Ind, Goals, Goals) :- !.
expand_goal(Goals, NonDetGoals, Goals, Cons, Cons, Ind, _, Ind,
            GoalsToTest, GoalsToTest) :-
%   NonDetGoals == Goals, !.
   goaleq(NonDetGoals, Goals),!.
expand_goal([Goal|Goals], NonDetGoals, NewGoals, ConsIn, ConsOut,
	    Ind, MaxDepth, NewInd, TestGoals, TestedGoals) :-
    goal_flag(Goal,Flag),
    expand_det_goal(Flag, Goal, Goals, Goals1, ConsIn, ConsOut1,
		    Ind, MaxDepth, Ind1), !,
    ( Ind =:= Ind1 ->  % Goal is non-deterministic --> try next one to expand
         NewGoals = [Goal|Goals2],
         expand_goal(Goals, NonDetGoals, Goals2, ConsIn, ConsOut,
		     Ind, MaxDepth,NewInd, TestGoals, TestedGoals)
    ; expand_goal(Goals1, [], NewGoals, ConsOut1, ConsOut,
		  Ind1, MaxDepth, NewInd, Goals1, TestedGoals)
    ).

expand_det_goal(Det, Goal, Rest, OutGoals, ConsIn, ConsOut, Ind, MaxDepth, NewInd) :-
   var(Det), !,
   wrap_goal(OnlyGoal,Goal),
   ( not_twice(OnlyGoal,Flag) ->
	 ( Flag == 0 ->
	       ifdebug(debug_message1(Goal, _NodeID, MaxDepth, Ind, det)),
	       fail
	 ; resolve(Goal, OutGoals, Rest, ConsIn, ConsOut,
			 Ind, MaxDepth, NewInd,_, det)
	 )
   ; NewInd = Ind
   ).
expand_det_goal(det, Goal, Rest, OutGoals, ConsIn, ConsOut, Ind, MaxDepth, NewInd) :- !,
   resolve(Goal, OutGoals, Rest, ConsIn, ConsOut, Ind, MaxDepth, NewInd, _, det).
expand_det_goal(table, Goal, Rest, OutGoals, ConsIn, ConsOut, Ind, MaxDepth, NewInd) :-
   wrap_goal(OnlyGoal,Goal),
   arg(2, OnlyGoal, Arg1), \+ Arg1=[_,'***'], !,
   resolve(Goal, OutGoals, Rest, ConsIn, ConsOut, Ind, MaxDepth, NewInd, _, det).
%expand_det_goal(disj, _, _, _, _, _, Ind, Ind) :- !.
expand_det_goal(_, _, _, _, _, _, Ind, _, Ind).


%%%%%%%  not_twice(?Goal, -FileID, -EntryID, -RuleNumber)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  tests if there is only one rule/5 which matches with Goal.
%%%  returns Number of rule if exists, 0 if there is no rule/5 at all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_twice(Goal, _) :-
     reset_counter,
     'CUF clause'(Goal, _, _, _, _, _, _, _, _),
     incr_counter(2),
     !, fail.
not_twice(_,_) :-
     get_counter(1), !.
not_twice(_, 0).

%%%%%  goaleq(+GoalList1, +GoalList2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  tests whether goallist 1 and goallist 2 are equal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goaleq([g(ParID, NthSubgoal, _, _)|_T], [g(ParID1, NthSubgoal, _, _)|_T1]) :- 
    ParID==ParID1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: prepare internal structures for printing      %%%
%%%      Created: Mon Mar 22 14:17:24 1993                      %%%
%%%     Modified: Wed Mar  1 09:38:04 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.22  1995/03/01 08:38:08  jochen
% prepare_printing now orders variables in DIFF constraints (term
% order) and sorts the set of (POLY or DIFF) constraints.
% (bug fix; cuf([X & ~Y,Y & ~X]) lead to loop with output_format:cuf)
%
% Revision 1.21  1995/01/26  08:27:03  jochen
% dead code and argument removed from rm_and_sort_feats (now 3-place).
% NB: the prepared term 'CYCLE'(X) is never produced; may be removed
%
% Revision 1.20  1995/01/25  12:19:33  michel
% *** empty log message ***
%
% Revision 1.19  1995/01/25  12:16:35  michel
% bug fixing in rm_and_sort_feats/4 (for cyclic structures)
%
% Revision 1.18  1994/07/27  14:43:00  junger
% \= replaced by \+
%
% Revision 1.17  1994/07/01  14:45:24  jochen
% now prefer tree nodes over other places when printing shared strucs
%
% Revision 1.16  1994/07/01  13:28:15  jochen
% adapted to new internal format for !-terms
%
% Revision 1.15  1994/06/30  17:48:17  jochen
%  * convert4fed/4 --> convert4fed/2
%  * made rm_and_sort_feats not to determine the place where a shared struc. is
%    printed; but still it won't traverse it more than once (see $DONE, formerly $VAR)
%  * tried to produce better output for trees; not yet finished; abgeklemmt
%
% Revision 1.14  1994/06/24  17:25:41  jochen
% bug fixes, more robustness in tree-pp
% * close_list/1: cut added
% * assume dtrs list empty when prepare_pp_tree_dtrs/3 fails
%
% Revision 1.13  1994/06/22  12:30:43  michel
% get_flags/2: nonvar case added mainly because of possible side
%              effects when using the Prolog interface (goals flag
%              value is `foreign' then)
%
% Revision 1.12  1994/06/22  09:44:23  michel
% prepare_printing_goals/2 calls now
% get_flags/2 for computing information about the non-/deterministic
%             or unresolvable and un-/delayed status of a goal
%
% Revision 1.11  1994/06/21  16:02:40  michel
% eliminate_and_sort_feats/6 replaced by rm_and_sort_feats/4
% eliminate_and_sort_feats_/6 replaced by rm_and_sort_feats_/4
% sort_fs/4 replaced by sort_fs_node/3
% prepare_pp_cl/5 replaced by prepare_pp_cl/3
% get_layout_options/4 replaced by get_layout_options/1
% prepare_printing_goals/6 replaced by prepare_printing_goals/3
% prepare_printing_goal/8 replaced by prepare_printing_goal/5
% new layout_attribute_order/2, layout_invisible_attributes/2,
%     layout_alphabetic_order/2, layout_xmfed_output/2
% prepare_pp/3, prepare_pp0/5, prepare_pp_tree_dtrs/3 modified
%
% Revision 1.10  1994/06/13  12:20:34  michel
% prepare_pp0_/3 --> prepare_pp0_/5:
% * minor changes for shared_lists flag
%
% Revision 1.9  1994/05/17  10:37:34  michel
% eliminate_and_sort_feats/{6,7}:
% * handles prolog/1 (internal) and 'PROLOG'/1 (prepared) formats
% prepare_pp/3:
% * 'PROLOG'/1 case added
% type_info2type_term/3:
% * does no longer call type_term2atom/2
% type_term2chars/3:
% * puts space between ~ and the following symbol if necessary (bug fix)
% * adds quotes to the symbol if necessary (bug fix)
%
% Revision 1.8  1994/05/10  16:07:21  jochen
% changes for tree preparation; prepare_printing0... stuff commented out
%
% Revision 1.7  1994/02/03  11:32:01  junger
%  - get_layout_options/3 --> /4 (xmfed flag) (michel)
%  - bugs removed l.98 and l.131 (michel)
%  - prepare_pp0_ produces 'CYCLE' and 'REF' terms
%  - prepare_pp_{cl/2,rule/4} prepare_printing_clause{/4,s/3}
%    moved to here (from top_level.pl) (new goal and clause format)
%  - prepare_constraint now with layout options
%  - prepare_{pp,printing}_goal{,s} major revision
%
% Revision 1.6  1994/01/18  16:24:39  michel
% a lot of changes for the new preparation format:
%
% *  'ATOM'(Atom)
% *  'CYCLE'(Tag)
% *  'REF'(Tag)
% *  'STRUC'(MultiFlag,Tag,TypeTerm,AVPs)
%
% new predicates: get_layout_options/3, prepare_printing_goals/2
%
% Revision 1.5  1993/12/15  09:42:26  michel
% type_term2atom/2 changed: minimization of brackets
%
% Revision 1.4  1993/12/08  13:20:42  michel
% copy_term in prepare_printing/4 removed
%
% Revision 1.3  1993/12/08  12:52:24  michel
% several bug fixes
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Fri Sep 24 15:49:27 1993 (michel) : prepare_pp0 changed
% Wed Sep 15 10:38:21 1993 (jochen) : tree handling included (see: tree_in_xmfed.pl)
% Tue Sep 14 11:59:47 1993 (michel) : prepare_pp_goals/2 from pp4dbg.pl included
% Mon Sep 13 18:57:08 1993 (michel) : prepare_constraint: 'POLY' case added
% Fri Jul 30 18:36:06 1993 (dorna) : type_term2chars/3 last clause changed
% Fri Jul  9 11:59:44 1993 (dorna) : cnf2type_term --> extra file
% Mon Mar 22 14:31:30 1993 (michel) : string handling changed
% Mon Mar 22 14:25:49 1993 (michel) : new

%%%  prepare_printing(+FSInternalFormat, +ConstraintsInternalFormat,
%%%                   -PreparedFS, -PreparedConstraints)
prepare_printing(FS, Cs, PrepFS, PrepCs) :-
   get_layout_options(Options),
   layout_xmfed_output(Options, Output),
   ( Output == no_xmfed -> true
   ; convert4fed(FS, Output)
   ),
   rm_and_sort_feats(FS, Options, MFS),
   prepare_pp(MFS, Output, PrepFS),
   prepare_pp_cl(Cs, Options, PrepCs), !.

%%%  rm_and_sort_feats(+FSInternalFormat,
%%%                    +LayoutOptions, -ReducedFSInternalFormat)
%%%  deletes invisible parts of the feature structure
%%%  sorts attributes
rm_and_sort_feats([TI|FS], Options, Out) :-
   analyze_type_info(TI, TT, Var),
   ( Var == [] ->
	 ( FS = [prolog(P)] -> Out = 'PROLOG'(P)
	 ; FS = [A],
	   Out = 'ATOM'(A)
	 )
   ; var(Var) ->
% 	 ( FS = [] -> Out = '$STRUC'(_NewVar,TT,[])
%  	 ; FS = [prolog(P)] -> Out = 'PROLOG'(P)
% 	 ;
	 Out = '$STRUC'(_NewVar,TT,FSOut),
	 Var = '$DONE'(Out),  %% bind before recursion to catch cycles
	 rm_and_sort_feats_(FS, Options, FSOut)
% 	 ),
%	 Var = '$DONE'(Out)
   ; Var = '$DONE'(Out) -> true
   ).

analyze_type_info(TypeInfo, TypeTerm, RestVar) :-
   ( var(TypeInfo) ->
	 RestVar = TypeInfo, TypeTerm = top
   ; functor(TypeInfo,'$DONE',1) ->
	 RestVar = TypeInfo 
   ; type_info2type_term(TypeInfo,TypeTerm,RestVar)
   ).

memberchk_eq(E,[F|_]) :- E == F, !.
memberchk_eq(E,[_|R]) :-
   memberchk_eq(E, R).

%%%  rm_and_sort_feats_(+FSInternalFormat, +LayoutOptions,
%%%                     -ReducedFSInternalFormat)
%%%  deletes invisible parts of the feature structure
%%%  closes open ended lists of internal FS encoding
rm_and_sort_feats_([], _, []) :- !.
rm_and_sort_feats_(FS, Options, FSOut) :-
   layout_invisible_attributes(Options, Invisibles),
   ( Invisibles == [] ->
	 FSMin = FS,
	 close_list(FS)
   ; rm_feats(FS, Invisibles, FSMin)
   ),
   sort_fs_node(FSMin, Options, SortedFS),
   rm_and_sort_feat_vals(SortedFS, Options, FSOut).

rm_and_sort_feat_vals([], _, []).
rm_and_sort_feat_vals([F:V|FS], Options, [F:VOut|FSOut]) :-
   rm_and_sort_feats(V, Options, VOut),
   rm_and_sort_feat_vals(FS, Options, FSOut).

close_list([]) :- !.
close_list([_|R]) :- close_list(R).


rm_feats([], _, []) :- !.
rm_feats(FS, [], FS) :- !, close_list(FS).
rm_feats([F:V|R], Invisibles, FSOut) :-
   ( delete(F, Invisibles, NewInvisibles) ->
	rm_feats(R, NewInvisibles, FSOut)
   ; FSOut = [F:V|FSOut0],
     rm_feats(R, Invisibles, FSOut0)
   ).

%%%  sort_fs_node(+FeatureValuePairs, +LayoutOptions
%%%               -SortedFeatureValuePairs)
%%%  sorts feature value pairs either in the order defined by 
%%%  AttributeList or alphabetically, if AttributeList = [].
%%%  All features of FeatureValuePairs which are not member
%%%  of AttributeList will be sorted at the end in alphabetic
%%%  order.
sort_fs_node([], _, []) :- !.
sort_fs_node(FS, Options, FSOut) :-
   ( delete('$DTRS':V, FS, FSNew) ->      % for xmfed output
	 FSOut = ['$DTRS':V|FSOut0]
   ; FSOut = FSOut0, FSNew = FS
   ),
   layout_attribute_order(Options, Order),
   layout_alphabetic_order(Options, Alpha),
   sort_fs_node(Order, FSNew, Alpha, FSOut0).

sort_fs_node([], FS, Alpha, FSOut) :-
   ( Alpha == yes -> sort(FS, FSOut) ; FSOut=FS ).
sort_fs_node([Feat|Fs], FS, Alpha, FSOut) :-
   ( delete(Feat:V, FS, FSR) ->
        FSOut = [Feat:V|FSOut0],
        sort_fs_node(Fs, FSR, Alpha, FSOut0)
   ; sort_fs_node(Fs, FS, Alpha, FSOut)
   ).

%%%  prepare_pp(+FSIn, +Output, -FSOut)
%%%  marks sharing parts of a feature structure
%%%  uses open ended lists of TypeInfo to identify sharings
prepare_pp('$STRUC'(Var,TT,FS), Output, FSOut) :-
	prepare_pp0(Var, TT, FS, Output, FSOut).
prepare_pp('PROLOG'(P), _, 'PROLOG'(P)).
prepare_pp('ATOM'(A), _, 'ATOM'(A)).
prepare_pp('$CYCLE'('$DONE'('$STRUC'('VAR'('MULTI',Ref),_,_))), _, 'CYCLE'(Ref)).

prepare_pp0(Var, TT, FS, Output, Struc) :-
   var(Var), !,
   Var = 'VAR'(Multi, Tag),
   ( functor(Output,xmfed,2), get_dtrs_list(FS,Dtrs,MotherFS) ->
      ( get_flag(print_trees_as_in_2_29,yes) ->
	    ( prepare_pp_tree_dtrs(Dtrs, Output, DtrsOut) -> % we have to prep
				                   % the dtrs first!!!
		  true
	    ; DtrsOut = []  %% assuming no daughters
	    ),
	    Struc = 'TREE'('STRUC'(Multi,Tag,TT,FSOut), DtrsOut),
	    prepare_pp_(MotherFS, Output, FSOut)
      ; %% new print strategy for dtrs
        prep_pp_tree_dtrs_top(Dtrs, Output, DtrsOut, FSlists_prep_goals, []),
	Struc = 'TREE'('STRUC'(Multi,Tag,TT,FSOut), DtrsOut),
	prepare_pp_fs_lists([prepare_pp_(MotherFS,Output,FSOut)
			    |FSlists_prep_goals])
      )
   ; Struc = 'STRUC'(Multi,Tag,TT,FSOut), 
     prepare_pp_(FS, Output, FSOut)
   ).
prepare_pp0('VAR'(Multi,Tag), TT, FS, Output, Struc) :-
   ( get_flag(shared_lists, no), cuf_list(FS) ->
	 Struc = 'STRUC'(Multi,Tag,TT,FSOut),
	 prepare_pp_(FS, Output, FSOut)
   ; Struc = 'REF'(Tag), Multi = 'MULTI'
   ).



prep_pp_tree_dtrs_top(['F':F,'R':R], Output, [FOut|ROut]) --> !,
   ( {F = '$STRUC'(Var,TT,FS)} ->  %% recursive case in prepare_pp
	 ( {nonvar(Var),
	    Var = 'VAR'(Multi,Tag),
	    \+ (get_flag(shared_lists, no), cuf_list(FS))} ->
		    {FOut = 'REF'(Tag), Multi = 'MULTI'}
	 ; {var(Var)} ->
	       {Var = 'VAR'(Multi,Tag)},
	       ( {functor(Output,xmfed,2),
	          get_dtrs_list(FS,Dtrs,MotherFS)} ->
			    [prepare_pp_(MotherFS,Output,FSOut)],
			    prep_pp_tree_dtrs_top(Dtrs, Output, DtrsOut),
			    {FOut = 'TREE'('STRUC'(Multi,Tag,TT,FSOut), DtrsOut)}
	       ; {FOut = 'STRUC'(Multi,Tag,TT,FSOut)}, 
	         [prepare_pp_(FS, Output, FSOut)]
	       )
% 	 ; %% delay prep, collect goal
% 	   [prepare_pp0(Var,TT,FS,Output,FOut)]
	 )
   ; %% nonrecursive cases
     {prepare_pp(F, Output, FOut)}
   ),
   ( {R == 'ATOM'([])} ->
	 {ROut=[]}
   ; {R = '$STRUC'(_,_,RFS)},
     prep_pp_tree_dtrs_top(RFS, Output, ROut)
   ).
%% else assume empty list
prep_pp_tree_dtrs_top(_, _, []) --> [].


prepare_pp_fs_lists([]).
prepare_pp_fs_lists([prepare_pp_(FS,O,Res)|FSlists_prep_goals]) :-
	prepare_pp_(FS,O,Res),
	prepare_pp_fs_lists(FSlists_prep_goals).


%% old code; to be eliminated when new one works
prepare_pp_tree_dtrs(['F':F,'R':R], Output, [FOut|ROut]) :- !,
	prepare_pp(F, Output, FOut),
	( R == 'ATOM'([]) ->
	      ROut=[]
	; R = '$STRUC'(_,_,RFS),
	  prepare_pp_tree_dtrs(RFS, Output, ROut)
	).

prepare_pp_([], _Output, []).
prepare_pp_([Feat:Val|FS], Output, [Feat:ValOut|FSOut]) :-
   prepare_pp(Val, Output, ValOut),
   prepare_pp_(FS, Output, FSOut).


%%%  prepare_pp_cl(+ListOfConstraints, -ListOfConstraints)
%%%  prepare list of constraints (DIFFs, POLYs)
prepare_pp_cl(ConsList, PrepConsList) :-
   get_layout_options(Options),
   prepare_pp_cl(ConsList,Options,PrepConsList).

%%%  prepare_pp_cl(+ListOfConstraints, +Invisibles, +AttributeOrder,
%%%                -ListOfConstraints)
prepare_pp_cl([], _, []) :- !.
prepare_pp_cl(CL, O, PCL) :-
   prepare_pp_cl(CL, O, PCL0, []),
   sort(PCL0,PCL).

prepare_pp_cl([], _, X, X).
prepare_pp_cl([C|R], Options) -->
   prepare_constraint(C, Options),
   prepare_pp_cl(R, Options).

prepare_constraint('DIFF'(FS1, FS2), Options) --> !,
   { rm_and_sort_feats(FS1, Options, MFS1),
     prepare_pp(MFS1, no_xmfed, PFS1), !,
     rm_and_sort_feats(FS2, Options, MFS2),
     prepare_pp(MFS2, no_xmfed, PFS2) }, !,
   ( {PFS1 @> PFS2} -> ['DIFF'(PFS2,PFS1)]
   ; ['DIFF'(PFS1,PFS2)]
   ).
prepare_constraint('POLY'(ID,TI1,TI2), Options) --> !,
   { rm_and_sort_feats([TI1], Options, MTI1),
     prepare_pp(MTI1,no_xmfed,PTI1),
     rm_and_sort_feats([TI2], Options, MTI2),
     prepare_pp(MTI2,no_xmfed,PTI2), !,
     'POLY'(ID,DomType,RanType,_,_),
     cnf2type_term(DomType,DT),
     type_term2atom(DT,DTA),
     cnf2type_term(RanType,RT),
     type_term2atom(RT,RTA) }, !,
   ['POLY'(ID,PTI1,DTA,PTI2,RTA)].

%%%  type_info2type_term(+TypeInfo, -TypeTerm, -RestVar)
%%%  transforms the type of a graph into an external type term
type_info2type_term(TypeInfo, TypeTerm, TypeRest) :-
   get_current_type(TypeInfo, CNF, TypeRest),
   cnf2type_term(CNF, TypeTerm), !.
%   type_term2atom(TypeTerm0, TypeTerm), !.

%%%  get_current_type(+TypeInfo, -CurrentType, -RestVariable)
%%%  finds CurrentType out of type list TypeInfo and gives back 
%%%  RestVariable, too.
get_current_type(TypeInfoI, Type, TypeInfoO) :-
    ( var(TypeInfoI) -> 
	  TypeInfoO = TypeInfoI
    ; TypeInfoI = [Type0|Rest] ->
	  ( var(Rest) -> 
		Type = Type0, TypeInfoO = Rest
%	  ; functor(Rest,'VAR',2) ->
%		Type = Type0, TypeInfoO = Rest
	  ; functor(Rest,'$DONE',1) ->
		Type = Type0, TypeInfoO = Rest
	  ; Rest == [] ->
		Type = Type0, TypeInfoO = []
	  ; get_current_type(Rest, Type, TypeInfoO)
	  )
    ), !.

%%% type_term2atom(+TypeTerm, -Atom)
%%% makes an Atom out of TypeTerm
%%% inserted for graphic interface only (not neccessary for ASCII output)
type_term2atom(TT, TT) :- atomic(TT), !.
type_term2atom(TTIn, TTOut) :-
   flatten_struc(TTIn, Flat),
   type_term2chars(Flat, Chars, []),
   atom_chars(TTOut, Chars).

type_term2chars('&'(Cs)) --> !,
   "(", cs2chars(Cs), ")".
type_term2chars(';'(Ds)) --> !,
   "(", ds2chars(Ds), ")".
type_term2chars('~'(TT)) --> !, "~",
   ( { symb_char(TT) }, !, " "
   ; ! ),
   type_term2chars(TT).
type_term2chars(TT) -->
   { number(TT) -> number_chars(TT, Chars)
   ; atom_chars(TT, [F|R]),
     ( F =< 0'z, F >= 0'a -> Chars = [F|R]
     ; F =:= 0'" -> Chars = [F|R]
     ; symbol_char(F) -> Chars = [F|R]
     ; append([0''',F|R],"'",Chars)
     )
   }, Chars.

cs2chars([TT]) --> !,
   type_term2chars(TT).
cs2chars([TT|Cs]) -->
   type_term2chars(TT), " & ", cs2chars(Cs).

ds2chars([TT]) --> !,
   type_term2chars(TT).
ds2chars([TT|Ds]) -->
   type_term2chars(TT), " ; ", ds2chars(Ds).


prepare_printing_clauses([], _, []).
prepare_printing_clauses([Clause|Rest], No,
			 ['CUF clause'(No,PrepHead,PrepBody,PrepCons)|R]) :-
   prepare_printing_clause(Clause, PrepHead, PrepBody, PrepCons),
   No1 is No+1,
   prepare_printing_clauses(Rest, No1, R).

prepare_printing_clause('CUF clause'(Head,Body,Cons),
                        PrepHead, PrepBody, PrepCons) :-
   wrap_goal(Head, WrappedHead),
   prepare_pp_rule([WrappedHead|Body],Cons,
		   [PrepHead|PrepBody],PrepCons), !.

prepare_pp_rule(GL, CL, PrepGL, PrepCL) :-
   prepare_printing_goals(GL, PrepGL),
   prepare_pp_cl(CL, PrepCL).

%%%  prepare_printing_goals(+GoalList, -FullyPrepGoalList)
%%%  this predicate is used for clauses/1

prepare_printing_goals(GoalList, PrepGoalList) :-
   get_layout_options(Options),
   get_flags(GoalList, PrepGoalList), 
   prepare_printing_goals(GoalList, Options, PrepGoalList).

get_flags([], []).
get_flags([WrappedGoal|R], ['GOAL'(GoalID, _, status(Det), _)|NewR]) :-
   wrap_goal(Goal, WrappedGoal),
   goalID(WrappedGoal, GoalID),
   goal_flag(WrappedGoal, F0),
   /*
   ( nonvar(F0) ->
	 functor(F0,F,_),
	 ( F == table ->  
           Det = Det0:tabled,
	       ( arg(2,Goal,Arg), \+ Arg=[_,'***'] ->
		     Det0 = deterministic
	       ; Det0 = nondet
	       )
	 ; F == det ->
	       Det = deterministic
	 ; Det = nondet:F
	 )
   ; not_twice(Goal,Flag) ->
	 ( Flag == 0 ->
	       Det = unresolvable
	 ; Det = deterministic
	 )
   ; Det = nondet:Del,
     ( is_undelayed(Goal) ->
	 Del = undelayed
     ; Del = delayed 
     )
   ),
   */
   get_flags(R, NewR).

prepare_printing_goals([], _, []).
prepare_printing_goals([WrappedGoal|R], Options,
                       ['GOAL'(_, Name/Ar, _, GoalArgs)|NewR]) :-
   wrap_goal(Goal, WrappedGoal),
   functor(Goal, Name, Arity),
   Ar is Arity-1,
   prepare_printing_goal(0, Arity, Goal, Options, GoalArgs),
   prepare_printing_goals(R, Options, NewR).

prepare_printing_goal(Max, Max, _, _, []) :- !.
prepare_printing_goal(Par0, Arity, Goal, Options, [NewArg|NArgs]) :-
   Par is Par0+1,
   arg(Par,Goal,Arg),
   rm_and_sort_feats(Arg, Options, MFS),
   layout_xmfed_output(Options, Output),
   prepare_pp(MFS, Output, NewArg),
   prepare_printing_goal(Par, Arity, Goal, Options, NArgs).

%%%  prepare_pp_goals(+GoalList, -GoalListWithTopLevelSharings)
%%%  warning: will partially destroy the original bindings!!!
prepare_pp_goals([], []).
prepare_pp_goals([WrappedGoal|R], [WrappedNewGoal|NewR]) :-
   wrap_goal(Goal, WrappedGoal),
   wrap_goal(NewGoal, WrappedNewGoal),
   functor(Goal, Name, Arity),
   functor(NewGoal, Name, Arity),
   prepare_pp_goals(0, Arity, Goal, NewGoal),
   prepare_pp_goals(R, NewR).

prepare_pp_goals(Max, Max, _, _) :- !.
prepare_pp_goals(Par0, Arity, Goal, NewGoal) :-
   Par is Par0+1,
   arg(Par,Goal,Arg),
   arg(Par,NewGoal,NewArg),
   prepare_pp_arg(Arg, NewArg),
   prepare_pp_goals(Par, Arity, Goal, NewGoal).

prepare_pp_arg([TypeInfo|FS],NewArg) :-
   ( var(TypeInfo) ->
	 TypeInfo = 'VAR'(Multi,Tag),
	 NewArg = 'STRUC'(Multi,Tag,top,FS)
   ; functor(TypeInfo,'VAR',2) ->
	 prepare_pp_arg(TypeInfo,FS,top,NewArg)
   ; type_info2type_term(TypeInfo,TT,Var),
     prepare_pp_arg(Var,FS,TT,NewArg)
   ).

prepare_pp_arg(Var,FS,TT,'STRUC'(Multi,Tag,TT,FS)) :-
   var(Var), !,
   Var = 'VAR'(Multi,Tag).
prepare_pp_arg([], [A], _,'ATOM'(A)).
prepare_pp_arg('VAR'('MULTI',Tag),FS,TT,'STRUC'('MULTI',Tag,TT,FS)).

get_layout_options(Options) :-
   ( current_predicate(attribute_order, attribute_order(_)) ->
	 attribute_order(Order)
   ; Order = []
   ),
   layout_attribute_order(Options, Order),
   ( current_predicate(invisible_attributes, invisible_attributes(_)) ->
	 invisible_attributes(Invisibles)
   ; Invisibles = []
   ),
   layout_invisible_attributes(Options, Invisibles),
   ( ( get_flag(xmfed_output,yes),
       current_predicate(tree_display_dtr_pred,tree_display_dtr_pred(_,_)),
       tree_display_dtr_pred(Type,Func/Arity),
       Arity > 1,
       symbol_table(Type,0,TCode,_,type) )
   -> Output=xmfed(TCode,Func/Arity)
   ; Output=no_xmfed
   ),
   layout_xmfed_output(Options, Output),
   get_flag(alphabetic_order, Alpha),
   layout_alphabetic_order(Options, Alpha).

layout_attribute_order(layout(Order,_,_,_), Order).
layout_invisible_attributes(layout(_,Invisibles,_,_), Invisibles).
layout_alphabetic_order(layout(_,_,Order,_), Order).
layout_xmfed_output(layout(_,_,_,Output), Output).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: default settings for CUF system               %%%
%%%      Created: Fri Sep 18 12:58:45 1992                      %%%
%%%     Modified: Wed Apr 24 14:03:04 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.13  1996/04/24 12:03:24  jochen
% default for expand_det flag set to 'no'
%
% Revision 1.12  1996/03/14 17:09:43  jochen
% op. prec. corrected (|, <, and :)
%
% Revision 1.11  1994/06/30 17:35:45  jochen
%  * moved built-in type IDs to here
%  * new built-in type prolog
%  * valid_flag_value relation now also contains flag_actions
%
% Revision 1.10  1994/06/13  12:22:01  michel
% shared_lists flag added
%
% Revision 1.9  1994/05/16  16:23:39  michel
% new prefix cuf_op ! for Prolog terms
% new flag force_reloading for recompilation
%
% Revision 1.8  1994/05/10  14:12:40  michel
% new priority for ':=', '->';
% new operator for guards '#';
% output and choice_points flags removed;
% new flag output_format added;
%
% Revision 1.7  1994/02/21  15:31:21  jochen
%  - flags removed: choice_points,
%                   pp_trace,
%                   output (made invisible)
%  - new flag: trace_structures
%
% Revision 1.6  1994/01/19  17:12:42  michel
% new flag 'debug',
% depth_bound default is now 400
% new predicates: compiler_flags/1, interpreter_flags/1, printing_flags/1
%
% Revision 1.5  1994/01/14  15:21:15  michel
% new flag: alphabetic_order - the default feature order can be switched off now
% finish_compilation flag commented out, not in use
%
% Revision 1.4  1994/01/14  09:16:30  jochen
% New monotypes: number, afs_symbol
% string < afs no longer in basic_hierarchy, but is done by init_sat.
% Type codes 3, 5, 7 now reserved for string, number, and afs_symbol.
%
% Revision 1.3  1993/12/21  10:32:41  michel
% show_clauses_dbg flag renamed to show_clauses
% new flags: finish_compilation and syntax_check_only
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Thu Sep 16 12:07:16 1993 (michel) : xmfed_output flag added
% Thu Sep  2 11:06:36 1993 (michel) : pp_trace flag added
% Sun Aug  1 15:45:23 1993 (dorna) : precedence of ~ changed
% Fri Jun 18 20:03:30 1993 (michel) : precedence of : changed
% Fri Feb 26 11:18:01 1993 (michel) : dbg added
% Thu Feb 18 12:54:48 1993 (michel) : print_lists flag added
% Fri Feb  5 17:46:21 1993 (michel) : const_warnings flag added
% Thu Dec 17 10:36:54 1992 (michel) : choice_points default: no
% Tue Dec  8 10:56:38 1992 (michel) : print_types flag added
% Thu Dec  3 14:28:20 1992 (michel) : type_clash flag added
% Wed Dec  2 12:15:25 1992 (michel) : ~-operator added
% Tue Nov  3 15:44:13 1992 (michel) : tree type added


basic_mono_type(T) :- basic_mono_type(T,_).

%% table of built-in types and their codes
basic_mono_type(top,top).           % universe
basic_mono_type(bottom,[]).        % empty set
basic_mono_type(list,_).          % elist | nelist
basic_mono_type(elist,_).         % []
basic_mono_type(nelist,_).        % [_|_]
basic_mono_type(afs,-1).           % atomic feature structure
basic_mono_type(cfs,1).           % complex feature structure

basic_mono_type(string,3).        % list of character codes(" ... ")
basic_mono_type(number,5).        % numbers: integers and floats
basic_mono_type(afs_symbol,7).    % undeclared constants, simple symbols
basic_mono_type(prolog,9).        % prolog terms (prefixed in input with !)

%basic_mono_type(tree).
%basic_mono_type(utree).         % unary tree
%basic_mono_type(btree).         % binary tree

basic_feature('F', nelist, top).      % first
basic_feature('R', nelist, list).     % rest
%basic_feature('M', tree, afs).       % mother, root
%basic_feature('SD', utree, tree)     % single daughter
%basic_feature('LD', btree, tree).    % left daughter
%basic_feature('RD', btree, tree).    % right daughter

basic_constants(elist, [[]]).

basic_sort('$true$', 1, [top], top).

basic_hierarchy(list = [elist, nelist]).

%%basic_hierarchy([string,number,afs_symbol,prolog] < afs).  
%%                  This axiom is guaranteed by the codes given to
%%                  string ...



%basic_hierarchy(tree = [atree, ctree]).
%basic_hierarchy(ctree = [utree, btree]).
%basic_hierarchy([atree] < afs).

%%%  connectives with (internal) priorities
cuf_op(1100, xfx, :=).              %% Sort definition
cuf_op(999,  xfx, ->).              %% Sort declaration
cuf_op(1200, xfx, =).               %% Type equality
cuf_op(1200, xfx, ::).              %% Feature declaration
cuf_op(1200,  xfx, <).               %% Type hierarchy
cuf_op(1100,  xfy, '|').             %% Disjointness; op. prec.1100 hard-wired
                                    %%               in cuf_reader.pl
cuf_op(650,  xfy, ;).               %% Disjunktion
cuf_op(610,  xfy, &).               %% Conjunction
cuf_op(590,   fx, ~).               %% Negation
cuf_op(500,   fx, !).               %% Prolog term follows
%cuf_op(580,  xfy, :).               %% Selection
cuf_op(600,  xfy, :).               %% Selection
cuf_op(1200, xfx, #).               %% Guard Definition

%%%  flags
init_flags :-
   % output
%   set_flag(output, ascii),             % output (ascii/graphic)
   set_flag(output_format, matrix),     % ppcuf output (matrix/cuf)
   set_flag(graphic_dbg, no),           % window debugger (yes/no)
   set_flag(show_clauses, no),          % debugger shows clauses (yes/no)
   set_flag(print_types, yes),          % ppcuf prints types (yes/no)
   set_flag(shared_lists, yes),         % ppcuf prints list sharings (yes/no)
   set_flag(print_lists, yes),          % ppcuf prints PROLOG-style lists (yes/no)
   set_flag(alphabetic_order, yes),     % default feature order (yes/no)
   set_flag(type_clash, no),            % print type clashes (yes/no)
   set_flag(xmfed_output, no),
   set_flag(trace_structures, no),      % show structures in trace  (yes/no)
   % compiler
   set_flag(warnings, yes),             % print warnings      (yes/no)
   set_flag(const_warnings, no),        % undef. constants warnings  (yes/no)
%   set_flag(singleton_warnings, yes)    % singleton variable warnings (yes/no)
   set_flag(expand_det, no),           % expand det. goals   (yes/no)
   set_flag(syntax_check_only, no),     % syntax checker only (yes/no)
   set_flag(force_reloading, no),          % real reloading of files (yes/no)
   % interpreter
   set_flag(depth_bound, 400),          % depth bound for run/1 (integer)
   set_flag(interactive, no),           % interactive         (yes/no)
%   set_flag(choice_points, no),         % show choice points  (yes/no)
   set_flag(tracing, no),               % tracing             (all/yes/no)
   set_flag(debug, no).                 % debugger            (yes/no)

%%% global counters
init_counters :-
   retract_all_counters_and_IDs,
   setID(typeID, 2),          %%% even numbers for types
   setID(constID,11),          %%% and odd ones for constants!
   % constID = -1 reserved for afs type/ 1 for cfs type
   % 3,5,7 are reserved for string, number and afs_symbol
   % 9 is prolog
   setID(fileID, 1),
   set_counter(index_table, 0).     % can be removed (?)

%%% valid flag values and implicit flag actions
valid_flag_value(warnings, yes, true).
valid_flag_value(warnings, no, true).
valid_flag_value(const_warnings, yes, true).
valid_flag_value(const_warnings, no, true).
valid_flag_value(expand_det, yes, true).
valid_flag_value(expand_det, no, true).
valid_flag_value(depth_bound, '$integer', true).
valid_flag_value(interactive, yes, init_cuf_debugging).
valid_flag_value(interactive, no, init_cuf_debugging).
valid_flag_value(tracing, yes, init_cuf_debugging).
valid_flag_value(tracing, no, init_cuf_debugging).
valid_flag_value(tracing, all, init_cuf_debugging).
valid_flag_value(type_clash, yes, true).
valid_flag_value(type_clash, no, true).
%valid_flag_value(output, ascii).  %% flag output made internal
%valid_flag_value(output, graphic). 
valid_flag_value(print_lists, yes, true).
valid_flag_value(print_lists, no, true).
valid_flag_value(print_types, yes, true).
valid_flag_value(print_types, no, true).
valid_flag_value(graphic_dbg, yes, true).
valid_flag_value(graphic_dbg, no, true).
valid_flag_value(show_clauses, yes, true).
valid_flag_value(show_clauses, no, true).
valid_flag_value(xmfed_output, yes, true).
valid_flag_value(xmfed_output, no, true).
valid_flag_value(syntax_check_only,yes, true).
valid_flag_value(syntax_check_only,no, true).
valid_flag_value(alphabetic_order,yes, true).
valid_flag_value(alphabetic_order,no, true).
valid_flag_value(debug,yes, init_cuf_debugging).
valid_flag_value(debug,no, init_cuf_debugging).
valid_flag_value(trace_structures,yes, true).
valid_flag_value(trace_structures,no, true).
valid_flag_value(output_format,cuf, true).
valid_flag_value(output_format,matrix, true).
valid_flag_value(force_reloading,yes, true).
valid_flag_value(force_reloading,no, true).
valid_flag_value(shared_lists, yes, true).
valid_flag_value(shared_lists, no, true).

%% for upward compatibility; (for gui, stuf-gui?)
valid_flag_value(X,Y) :- valid_flag_value(X,Y,_).

compiler_flags([const_warnings, depth_bound, expand_det, syntax_check_only,
                warnings,force_reloading]).
interpreter_flags([debug, depth_bound, interactive, tracing]).
printing_flags([alphabetic_order, graphic_dbg, print_lists,
                print_types, show_clauses, trace_structures,
		type_clash, xmfed_output, output_format,
		shared_lists]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: version header                                %%%
%%%      Created: Wed Dec  2 11:50:32 1992                      %%%
%%%     Modified: Fri Apr 12 12:55:33 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.12  1996/04/12 10:55:44  jochen
% *** empty log message ***
%
% Revision 1.11  1996/04/09 15:14:28  jochen
% no longer beta-version 2.31; new date
%
% Revision 1.10  1996/03/14 13:07:00  jochen
% new date
%
% Revision 1.9  1995/12/20 14:58:42  junger
% versionfile changed for 2.31 beta
%
% Revision 1.8  1994/07/21 09:58:14  junger
% updated for version 2.30
%
% Revision 1.7  1994/06/10  09:35:47  junger
% *** empty log message ***
%
% Revision 1.6  1994/05/11  08:23:15  jochen
% *** empty log message ***
%
% Revision 1.5  1994/05/06  18:30:23  michel
% new date
%
% Revision 1.4  1994/02/21  16:42:30  michel
% date revision
%
% Revision 1.3  1994/01/18  09:05:08  michel
% new version, new programmer: Joerg added
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


cuf_version :-
    format("~2n~8+                CUF - Version 2.31     ~n          ", []),
    format( "~n~8+Institut fuer maschinelle Sprachverarbeitung (IMS),", []),
    format( "~n~8+          Universitaet Stuttgart, Germany          ", []),
    format("~2n~8+    Michael Dorna, Jochen Doerre & Joerg Junger    ", []),
    format("~2n~8+                    April 1996~n                  ", []),
    ttynl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: constraint checker                            %%%
%%%      Created: Tue Dec 15 11:36:19 1992                      %%%
%%%     Modified: Thu Dec 23 14:02:13 1993 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1993/12/23 13:03:10  michel
% check_constraints/2: in first clause `!' added
%
% Revision 1.3  1993/12/14  11:52:59  michel
% check_constraints/2 changed:
% rerun check_constraints/3 if some propagation was done by a unification
%
% Revision 1.2  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Tue Sep 28 11:30:49 1993 (michel) : check_POLY/7 changed
% Mon Sep 13 15:59:59 1993 (michel) : check_POLY/7 changed
% Mon Sep 13 13:00:24 1993 (michel) : check_POLY_sat/1 added

%%%  check_constraints(+ConstraintListIn, -ConstraintListOut)
% check_constraints(Var, Var) :- var(Var), !.
check_constraints([], []) :- !.
check_constraints(CsIn, CsOut) :-
	check_constraints(CsIn, CsMid, UnifyFlag), !,
	( UnifyFlag == yes ->
	      check_constraints(CsMid,CsOut)
	; CsMid=CsOut
	).

check_constraints([], [], _).
check_constraints([Cons|Cs], ConsOut, UnifyFlag) :-
   check_constraint_(Cons, Result, UnifyFlag), !,
   ( Result == keep ->
	 ConsOut = [Cons|CsOut]
   ; /* Result == forget -> */
         ConsOut = CsOut
   ),
   check_constraints(Cs, CsOut, UnifyFlag).


check_constraint_('DIFF'(T1,T2), Result, _) :-
   check_DIFF(T1, T2, Result).
check_constraint_('POLY'(ID, TI1, TI2), Result, UnifyFlag) :-
   check_POLY(ID, TI1, TI2, Result, UnifyFlag).


check_DIFF([TI1,A1|_], [TI2,A2|_], _) :-
    get_type(TI1, _, R1),
    get_type(TI2, _, R2),
    ( R1 \== [] -> R1 == R2
    ; R2 == [] -> A1 == A2
    ), !, fail.
check_DIFF(T1, T2, forget) :-
    \+ unify(T1, T2), !.
check_DIFF(_, _, keep).

check_POLY(ID, TypeInfo1, TypeInfo2, Result, UnifyFlag) :-
	'POLY'(ID, DomType, RanType, NegDomType, NegRanType), !,
	check_POLY(TypeInfo1, TypeInfo2, DomType, RanType,
		   NegDomType, NegRanType, Result, UnifyFlag), !.

check_POLY(TypeInfo1, TypeInfo2, _, RanType, NegDomType, NegRanType,
           forget, UnifyFlag) :-
	nonvar(TypeInfo2),
	get_type(TypeInfo2, Type2, _),
%	nonvar(Type2),
	( /* Ty --> R(y) <==> Ty & ~R(y) --> false */
          append(Type2, NegRanType, Formula),
	  check_formula(Formula, 0), !
	; /*      (Ty --> ~R(y)) --> Tx & ~D(x) 
	     <==> (Ty & R(y) --> false) --> Tx & ~D(x) */
	  nonvar(RanType),
	  append(Type2, RanType, Formula),
	  check_formula(Formula, 0), !,
	  unify([TypeInfo1,_], [[NegDomType|_],_]),
	  UnifyFlag = yes
	).
check_POLY(TypeInfo1, TypeInfo2, DomType, RanType, NegDomType, _,
           forget, UnifyFlag) :-
	nonvar(TypeInfo1),
	get_type(TypeInfo1, Type1, _),
%	nonvar(Type1),
	( /*      (Tx --> D(x)) --> Ty & R(y)
	     <==> (Tx & ~D(x) --> false) --> Ty & R(y) */
	  append(Type1, NegDomType, Formula),
	  check_formula(Formula, 0), !,
	  ( var(RanType) -> true
	  ; unify([TypeInfo2,_], [[RanType|_],_]),
	    UnifyFlag = yes
	  )
	; /* Tx --> ~D(x) <==> Tx & D(x) --> false */
	  append(Type1, DomType, Formula),
	  check_formula(Formula, 0), !
	).
check_POLY(_, _, _, _, _, _, keep, _).


check_POLY_sat([]).
check_POLY_sat([C|R]) :-
   \+ \+ check_POLY_sat_(C,R), !.

check_POLY_sat_([]).
check_POLY_sat_([C|R]) :-
   check_POLY_sat_(C,R).

check_POLY_sat_('POLY'(ID, TI1, TI2), R) :- !,
   'POLY'(ID, DomType, RanType, NegDomType, _NegRanType),
    ( unify([TI1,_], [[DomType|_],_]),
      unify([TI2,_], [[RanType|_],_])
    ; unify([TI1,_], [[NegDomType|_],_])
%      unify([TI2,_], [[NegRanType|_],_])
    ),
    check_POLY_sat_(R).
check_POLY_sat_(_, R) :-       % e.g. 'DIFF'(_,_) 
   check_POLY_sat_(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: CUF task predicates                           %%%
%%%      Created: Thu Oct 15 14:47:10 1992                      %%%
%%%     Modified: Tue Mar 26 09:35:42 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.27  1996/04/09 13:43:36  jochen
% .cnf-files now compiled in module cuf;
% expand_det/0 new top-level predicate;
% bug fix in feature/1
%
% Revision 1.26  1995/06/12 07:16:33  jochen
% clauses/1 now also works for foreign sorts
%
% Revision 1.25  1994/07/21  14:27:16  junger
% goal_at(GoalID, ExtractNodeID,StopNodeID) as option for cufpp/1 added
%
% Revision 1.24  1994/07/20  15:33:29  junger
% cuf/1 adapted to the new write_success_node
%
% Revision 1.23  1994/07/08  17:26:31  jochen
% cufpp(ancestors(N)) now prints external arities
%
% Revision 1.22  1994/07/01  15:50:43  jochen
% added: cufpp(goal/2)
%
% Revision 1.21  1994/06/30  17:41:36  jochen
%  * flag/2 predicate now also performs flag_action attached to flag
%  * a few minor cosmetics
%
% Revision 1.20  1994/06/28  11:16:03  junger
% - minor changes in cufpp
%
% Revision 1.19  1994/06/24  17:30:19  jochen
% more cufpp stuff
%
% Revision 1.18  1994/06/21  15:12:25  jochen
% rename: cuf_goal --> cuf_top_level_goal (was in conflict with new adt)
%
% Revision 1.17  1994/06/21  13:18:04  jochen
% user-level predicates trace_filter and cufpp added
%
% Revision 1.16  1994/06/20  12:32:24  michel
% * print_rules/4, print_head/4, print_body/5 and print_const/2 moved to ppcuf1.pl
% * cuf/1 calls new cuf0/1 for hiding internal variable bindings
%
% Revision 1.15  1994/05/10  15:12:13  michel
% flags/0: flags now in alphabetic order
% cuf/1: removes all dynamic assertions now (tmp-entries)
%
% Revision 1.14  1994/05/06  18:32:10  michel
% cuf/1 reports absolute timings now
%
% Revision 1.13  1994/03/18  09:47:30  jochen
% in cuf/1:
% now shows whether depth_bound was hit in summary (after displaying total time)
%
% Revision 1.12  1994/02/21  15:24:57  jochen
%  - cuf/1 now with statistics (generalized from former run/1)
%    run/1 now obsolete (maps to cuf/1).
%
% Revision 1.11  1994/02/03  11:33:46  junger
%  - flag/2 adapted to new keyword '$integer' (see valid_flag_value)
%    and improved
%  - bug in flags removed (!)
%  - printing of clauses, major revision (new format)
%
% Revision 1.10  1994/01/19  09:05:03  michel
% clauses/1 changed:
% prepare_printing_goals/2 used for goal output preparation,
% pp_fs/4-calls in new format now
%
% Revision 1.9  1993/12/23  13:17:13  michel
% finish_compilation/1 goals added after (re)loading of control files
% new predicate: cload/0, gload/{0,1}, unload/0, load_cnf/1,
% cload/1 now possible with list argument
% help updated
%
% Revision 1.8  1993/12/20  17:06:02  michel
% unload_CUF_grammar/1 renamed to unload_CUF_files/1
% unload/1 can be called with a list of files now
% load_CUF_grammar/1 renamed to load_CUF_files/1
% gload/1 for grammar loading now (was the same as dload/1 in the past)
%
% Revision 1.7  1993/12/10  20:54:42  michel
% cuf/1 added
%
% Revision 1.6  1993/12/10  12:27:49  michel
% show_dirs/0 changed:
% collects cuf_grammar_directory/1 and cuf_demo_directory/1 facts now
%
% Revision 1.5  1993/12/08  12:09:51  michel
% print_rules/4: some cuts added
%
% Revision 1.4  1993/12/08  10:13:03  michel
% print_rules/4: output changed
%
% Revision 1.3  1993/12/07  17:01:58  michel
% clauses/1 changed: has now the same functionality as listing/1 in Prolog
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Wed Sep 22 15:16:47 1993 (jochen) : added show(result;active;passive)
% Tue Sep 14 18:05:45 1993 (michel) : show_feat_decl/1 added
% Tue Sep 14 17:52:47 1993 (michel) : user_help/0 added
% Tue Sep 14 15:56:11 1993 (michel) : add_cuf_dir/1 added
% Tue Sep 14 10:38:57 1993 (michel) : clauses/1 added
% Mon Sep 13 17:37:50 1993 (michel) : show_loaded added
% Thu Jun  3 17:40:29 1993 (michel) : {type,constant,feature,csort}/[1,2] added
% Mon Feb 22 20:00:31 1993 (michel) : rerun added
% Thu Dec  3 11:37:34 1992 (michel) : error handling changed,
%                                     output of run/1 changed

%%%  cload(+File)
%%%  1. loads a CUF control file
%%%  2. replaces old delay patterns by these specified in File 
cload(File) :-
    load_CUF_control(File),
    file_table(current_ids, _, _, FIDs, old),
    finish_compilation(FIDs), !.

%%%  cload
%%%  reloads control file(s) of current grammar
cload :-
    ( current_grammar(GrammarName) ->
	  load_grammar_control(GrammarName),
	  file_table(current_ids, _, _, FileIDs, old),
	  finish_compilation(FileIDs), !
    ; cuf_out(error, ['no current grammar specified (use gload/1 first)']),
      !, fail
    ).

%%%  dload(+File(s))
%%%  1. loads one or more CUF grammar files
%%%  2. (re)compiles these and all dependent files
dload(Files) :-
    load_CUF_files(Files), !.

%%%  load_cnf(+GrammarConfigurationsFile)
%%%  load configuration file for CUF grammars
load_cnf(ConfigName) :-
    ( ground(ConfigName) ->
	  ( cuf_abs_file_name(ConfigName,AbsName,cnf,read) ->
		retractall(grammar_files(_,_)),
		retractall(control_files(_,_)),
		retractall(current_grammar(_)),
		/*RB cuf:*/compile(AbsName)
	  ; cuf_out(error, ['grammar configurations file',ConfigName,
	                    'unknown or file name not expandable']),
	    !, fail
	  )
    ; cuf_out(error, ['The configuration file name has to be atomic.']),
      !, fail
    ).

%%%  gload(+Grammar)
%%%  1. sets current grammar
%%%  2. loads current grammar
gload(Grammar) :-
    load_CUF_grammar(Grammar), !.

%%%  gload
%%%  reload current grammar files
gload :-
    ( current_grammar(GrammarName) ->
	  load_current_grammar(GrammarName)
    ; cuf_out(error, ['no current grammar specified (use gload/1 first)']),
      !, fail
    ).

%%%  unload(+File(s))
%%%  1. deletes the internal code of File(s)
%%%  2. recompiles dependent files
unload(Files) :-
    unload_CUF_files(Files), !.

%%%  unload
%%%  deletes current grammar (CUF and control files)
unload :-
    ( current_grammar(GrammarName) ->
	  unload_current_grammar(GrammarName)
    ; cuf_out(error, ['no grammar loaded']),
      !, fail
    ).


%%% expand_det
%%% partial evaluate deterministic calls in all clauses
expand_det :-
    expand_det(_).


%%%  flag(+Name, +Value)
%%%  flag value setting
flag(Name, Value) :-
    ( atomic(Name) ->
	  ( nonvar(Value) ->
		 ( valid_flag_value(Name, _, _) ->
		       ( ( valid_flag_value(Name, Value0, Action),
		           ( Value0 == '$integer' ->
				 integer(Value)
			   ; Value=Value0
			   )
			 ) -> set_flag(Name, Value),
			      call(Action)
		       ; cuf_out(error, ['flag value not allowed']),
			 !, fail
		       )
		 ; cuf_out(error, ['undefined flag']),
		   !, fail
		 )
	  ; cuf_out(error, ['second argument should not be a variable']),
	    !, fail
	  )
    ; cuf_out(error, ['first argument should be atomic']),
      !, fail
    ).

%%% flags
%%% shows flag settings
flags :-
	cuf_out(message, ['$TAB$'(4),'flag name: flag value','$NL$']),
	setof(Name:Value, 'CUF_FLAG'(Name, Value), Flags),
	( member(Name:Value, Flags),
	  cuf_out(message, ['$TAB$'(4),Name:'',Value]),
	  fail
	; !
	).

%%% run(+Atom)
%%% runs examples (test/1 sorts), obsolete, kept for upward
%%% compatibility (output is a little different)
run(Atom) :-
	atomic(Atom),
	cuf(test(Atom)).

% run(Atom) :-
% 	atomic(Atom),
% 	get_flag(depth_bound, Max),
% 	statistics(runtime,[O,_]),
% 	( run(Atom, X, Max, Cs, Depth),
% 	  undo(statistics(runtime,_)),
% 	  statistics(runtime,[_,T]),
% %	  Depth is Max-Y,
% 	  cuf_out(message, ['run:', test(Atom)]),
% 	  cuf_out(message, ['used time (msec):',T]),
% 	  cuf_out(message, ['used depth:',Depth]),
% 	  cuf_out(message, ['result:','$NL$','$FS$'(X,Cs)]),
% 	  ( yn_question(['other results? (y/n)']) ->
% 		fail
% 	  ; true )
% 	; statistics(runtime,[N,_]),
% 	  T is N-O,
% 	  cuf_out(message, ['total time for', test(Atom), '(msec):',T])
% 	).

%%% inspecting the symbol table:

classify(*, defined) :- !.
classify(-, classified) :- !.
classify(+, built-in) :- !.
classify(M,_) :- var(M),
                 cuf_out(error,['second argument should be',
                                '"defined", "classified", or "built-in"']),
		 fail.

%%% type(?Type)
%%% type(?Type,?Class)
%%% matches Type with a built-in/defined type, backtrackable
type(X) :-
     type(X,_).
type(X,C) :-
     ( var(C) ->
          symbol_table(X,0,_,M,type),
          classify(M,C)
     ; classify(M, C),
       symbol_table(X,0,_,M,type)
     ).

%%% constant(?Constant)
%%% constant(?Constant,?Class)
%%% matches Constant with a built-in/defined/classified constant, backtrackable
constant(X) :-
     constant(X,_).
constant(X,C) :-
     ( var(C) ->
          symbol_table(X,0,_,M,const),
          classify(M,C)
     ; classify(M, C),
       symbol_table(X,0,_,M,const)
     ).

%%% feat(?Feature)
%%% feat(?Feature,?Class)
%%% matches Feature with a defined/classified feature, backtrackable
feature(X) :-
     feature(X,_).
feature(X,C) :-
     ( var(C) ->
          symbol_table(X,1,_,M,feat),
          classify(M,C)
     ; classify(M, C),
       symbol_table(X,1,_,M,feat)
     ).

%%% csort(?Sort/?Arity)
%%% csort(?Sort/?Arity,?Class)
%%% matches Sort/Arity with a defined/classified sort, backtrackable
%%% 'csort' can't have the name 'sort' because of the built-in sort/2 :(
csort(X) :-
     csort(X,_).
csort(S/A,C) :-
     ( var(C) ->
          symbol_table(S,A,_,M,sort),
          classify(M,C)
     ; classify(M, C),
       symbol_table(S,A,_,M,sort)
     ).

%%% show_loaded
%%% lists all loaded CUF files
show_loaded :-
    file_table(current_files, Files, _, _, old),
    ( Files == [] ->
	  cuf_out(message,['no *.cuf files loaded'])
    ; cuf_out(message,['loaded *.cuf file(s):']),
      ( member(File,Files),
        cuf_out(message,[File]),
	fail
      ; true
      )
    ).

%%% clauses(+Sort)
%%% clauses(+Sort/+Arity)
%%% lists all clauses defining Sort/Arity
clauses(Sort) :-
     ( atomic(Sort) ->
	   ( csort(Sort/A),
	     clauses(Sort/A),
	     fail
           ; true)
     ; var(Sort) ->
	   cuf_out(error,['argument of clauses/1 has to be non-variable']),
	   !, fail
     ).
clauses(Sort/Arity) :-
	Ar is Arity+1,
	cuf_foreign(Sort,Ar,FID),
	!,
	file_table(check,FName,_,FID,old),
	cuf_out(message,[Sort/Arity,'declared foreign (',FName,')','$NL$']),
	functor(Skel,Sort,Ar),
	findall(Prop,predicate_property(Skel,Prop),Props),
	(Props == [] ->
	     cuf_out(message,['but Prolog predicate',Sort/Ar,'not defined'])
	; cuf_out(message,['Prolog predicate',Sort/Ar,'properties:',Props]),
	  findall(Source,source_file(Skel,Source),Sources),
	  cuf_out(message,['Source file(s):',Sources])).
clauses(Sort/Arity) :- 
   ( atomic(Sort), integer(Arity) ->
	 Ar is Arity+1,
	 functor(Term,Sort,Ar),
	 findall('CUF clause'(Term,Body,Cons),
		 'CUF clause'(Term,_,_,_,_,Body,[],Cons,[]),Rules),
	 ( Rules==[] ->
	       cuf_out(message,[Sort/Arity,'is not defined'])
	 ; print_rules(Rules, 1, Sort, Arity), !
	 )
   ; cuf_out(error,['argument Sort/Arity of clauses/1 is of wrong type'])
   ).


:- dynamic cuf_grammar_directory/1.

%%%  add_dir(+GramDir)
%%%  adds a grammar directory for *.cuf file search
add_dir(GramDir) :-
   atomic(GramDir),
   on_exception(_, 
		( absolute_file_name(GramDir, AbsGramDir),
                  ( cuf_grammar_directory(AbsGramDir) ->
			cuf_out(message,[directory,AbsGramDir,'already known'])
		  ; assertz(cuf_grammar_directory(AbsGramDir)),
		    cuf_out(message,[directory,AbsGramDir,added])
		  )
		),
		( cuf_out(error, ['grammar directory',GramDir,
                                  'unknown or name not expandable']),            
		  fail
		)).

%%%  show_dirs
%%%  lists all current search paths for *.cuf files
show_dirs :-
   findall(Dir, (cuf_grammar_directory(Dir);cuf_demo_directory(Dir)), Dirs),
   cuf_out(message,['CUF grammar directories:']),
   ( member(D,Dirs),
     cuf_out(message,[D]),
     fail
   ; true
   ).

%%%  show_feat_decl(?Feature)
%%%  lists all declarations for Feature
feat_decl(Feature) :-
   ( var(Feature) ->
	 cuf_out(error,['argument of feat_decl/1 must be instantiated']),
	 fail
   ; true
   ),
   findall((Dom,Ran),feat_decl(Feature,Dom,Ran,_,_),DomRan),
   ( DomRan==[] ->
	 cuf_out(message,[feature,Feature,'is not defined'])
   ; DomRan=[(D,R)] ->
	   cnf2type_term(D, TypeTerm0),
	   type_term2atom(TypeTerm0, Domain),
	   cnf2type_term(R, TypeTerm1),
	   type_term2atom(TypeTerm1, Range),
	   ( Domain==top,Range==top ->
		 cuf_out(message,[feature,Feature,'is not declared'])
           ; cuf_out(message,['declaration of feature',Feature:'']),
             cuf_out(message,[Domain,'-->',Range])
	   )
   ; cuf_out(message,['declarations of feature',Feature:'']),
     member((D,R),DomRan),
     cnf2type_term(D, TypeTerm0),
     type_term2atom(TypeTerm0, Domain),
     cnf2type_term(R, TypeTerm1),
     type_term2atom(TypeTerm1, Range),
     cuf_out(message,[Domain,'-->',Range]),
     fail
   ; true
   ).

%r(Extern) :-
%       cuf_extern2intern(Extern, Intern, Constraints, Result),
%       cuf_prove(all, Intern, [], Constraints, ConsOut, _),
%       pp_fs(Result,ConsOut).

cuf(Extern) :-
        cuf0(Extern), !, fail.

cuf0(Extern) :-
	delete_db_entries(tmp), % safety first (see below)
	proof_init,                        %% necessary for debug
	statistics(runtime,[TOff,_]),
	( \+ \+ ( cuf_extern2intern(Extern, Intern, Constraints, Result),
                  set_initial_goalIDs(Intern),       %% necessary for debug
		  retractall(cuf_top_level_goal('#1',_,_,_)),
		  assert(cuf_top_level_goal('#1',Intern,Constraints,Result)),
		  cuf_prove(all, Intern, ResiduatedGoals, Constraints, ConsOut,
			    RuleNoList),
		  write_success_node(RuleNoList,ResiduatedGoals),  %% necessary for debug
		  statistics(runtime,[TAbs,_]),
		  TRel is TAbs-TOff,
		  cuf_out(message, ['used time (msec):',TRel]),
		  cuf_out(message, ['result:','$NL$','$FS$'(Result,ConsOut)]),
		  ( ResiduatedGoals = [] -> true
		  ; cuf_out(message,['residuated goals',
		                     '$GOAL LIST$'(ResiduatedGoals,[])])),
		  ( yn_question(['other results? (y/n)']) ->
			fail
		  ; true ))
	; statistics(runtime,[TEnd,_]),
	  Total is TEnd-TOff,
	  numbervars(Extern,0,_),
	  cuf_out(message, ['total time for', cuf(Extern),
	                    '(msec):',Total]),
	  ( depth_check(depth_exhausted) ->   %% in prooftree.pl
		cuf_out(message,
			['search incomplete (depth limit exhausted)'])
	  ; true
	  )
	),
	delete_db_entries(tmp).  % removes all dynamic assertions



user_help :-
	format('~nThe CUF built-in predicates are:',[]),
	format('~n~n   add_dir(<CUF description directory>).',[]),
	format('~n   show_dirs.~n',[]),
	format('~n   load_cnf(<configuration file>).~n',[]),
	format('~n   gload.',[]),
	format('~n   gload(<grammar name>).',[]),
	format('~n   dload(<CUF description file>).',[]),
	format('~n   dload(<list of CUF description files>).',[]),
	format('~n   show_loaded.~n',[]),
	format('~n   unload.',[]),
	format('~n   unload(<CUF description file names>).',[]),
	format('~n   unload(<list of CUF description files names>).~n',[]),
	format('~n   cload.',[]),
	format('~n   cload(<control file name>).',[]),
	format('~n   cload(<list of control file names>).~n',[]),
	format('~n   run(<atom>).          % sort test(<atom>) must be defined!',[]),
	format('~n   cuf(<CUF feature term>).~n',[]),
	format('~n   feat_decl(<feature name>).~n',[]),
	format('~n   clauses(<sort name>).',[]),
	format('~n   clauses(<sort name>/<arity>).~n',[]),
	format('~n   type(<type name>).',[]),
	format('~n   type(<type name>, <class>).',[]),
	format('~n   constant(<constant name>).',[]),
	format('~n   constant(<constant name>, <class>).',[]),
	format('~n   feature(<feature name>).',[]),
	format('~n   feature(<feature name>, <class>).',[]),
	format('~n   csort(<sort name>/<arity>).',[]),
	format('~n   csort(<sort name>/<arity>, <class>).~n',[]),
%        format('~n   show(result(<number>)).',[]),
%        format('~n   show(passive(<number>)).',[]),
%        format('~n   show(active(<number>)).~n',[]),
	format('~n   trace_filter(<sort name>).',[]),
	format('~n   trace_filter(<sort name>/<arity>).',[]),
	format('~n   trace_filter(-<sort name>).',[]),
	format('~n   trace_filter(-<sort name>/<arity>).',[]),
	format('~n   trace_filter(-<variable>).',[]),
	format('~n   trace_filter(depth(<number>,<number>).~n',[]),
	format('~n   cufpp(<cufpp-spec>).',[]),
	format('~n     where <cufpp-spec> is one of',[]),
	format('~n       search_tree',[]),
	format('~n       search_tree(<nodeID>)',[]),
	format('~n       proof(<var_or_number>)',[]),
	format('~n       goal_list(<nodeID>)',[]),
	format('~n       goal(<var_or_goalID>,<nodeID>)',[]),
	format('~n       logdep_tree(<nodeID>)',[]),
	format('~n       ancestors(<goalID>)',[]),
	format('~n       result(<var_or_number>)',[]),
	format('~n       result_at(<nodeID>)',[]),
	format('~n       goalarg_at(<goalID>,<fromNodeID>,<toNodeID>,<argNo>)',[]),
	format('~n     (for post-mortem debugging; "debug" flag needs to be "yes")~n',[]),
	format('~n   flag(<flag name>, <flag value>).',[]),
	format('~n   flags.~n',[]). %	format('~n',[]),



show(result(N)) :-
	topgoal(GNo),
	findall(PNo, goal_passive(GNo,PNo,_,_,_), Res),
	nth(N,Res,ActPNo),
	goal_passive(GNo,ActPNo,Head,Body,_),
	print_passive(Body,Head,ActPNo).
show(active(N)) :-
	active(N,Sel,BodyR+Cons,Head,_,_),
	Body = [Sel:_|BodyR]+Cons,       %% hacky
	print_active(Body,Head,N).
show(passive(N)) :-
	goal_passive(_,N,Head,Body,_),
	print_passive(Body,Head,N).

topgoal(GNo) :-
	goal_active(GNo,[]), !.



%% trace filter stuff

:- dynamic cuf_trace_filter_pred/2.
:- dynamic cuf_trace_depth/2.

trace_filter(X) :- var(X), !, fail.
trace_filter(depth(Min,Max)) :- !,
   retractall(cuf_trace_depth(_,_)),
   asserta(cuf_trace_depth(Min,Max)),
   trace_filter_message.
trace_filter(-Spec) :- !, notrace_filter(Spec).
trace_filter(Sort/Arity) :- !,
   ( symbol_table(Sort,Arity,_,_,sort) ->
	 trace_filter(Sort,Arity)
   ; cuf_out(error,['no clauses for sort',Sort/Arity]),
     ( setof(Sort/Arity1,symbol_table(Sort,Arity1,_,_,sort),Set) ->
	   cuf_out(message,['However,', Set,are,defined]),
	   fail
     )
   ).
trace_filter(Sort) :-
   ( symbol_table(Sort,_,_,_,sort) ->
	 forall(symbol_table(Sort,Arity,_,_,sort),
		do_trace_filter(Sort,Arity)),
	 trace_filter_message
   ; cuf_out(error,['no clauses for sort',Sort]),
     !, fail
   ).

trace_filter(Sort,Arity) :-
   do_trace_filter(Sort,Arity),
   trace_filter_message.
do_trace_filter(Sort,Arity) :-
   Ar is Arity+1,
   ( cuf_trace_filter_pred(Sort,Ar) ->
	 true
   ; assertz(cuf_trace_filter_pred(Sort,Ar))
   ).

notrace_filter(Sort/Arity) :- nonvar(Arity), !,
   Ar is Arity+1,
   do_notrace_filter(Sort,Ar).
notrace_filter(Sort) :-
   do_notrace_filter(Sort,_).

do_notrace_filter(Sort,Ar) :-
   retractall(cuf_trace_filter_pred(Sort,Ar)),
   trace_filter_message.

trace_filter_message :-
   get_flag(tracing,Flag),
   cuf_out(message,['flag ''tracing'' currently set to:',Flag]),
   trace_filter_message1,
   trace_depth_message.
trace_filter_message1 :-	
   bagof(Sort/Arity, Ar^(cuf_trace_filter_pred(Sort,Ar),Arity is Ar-1), Bag),!,
   cuf_out(message,['tracing shows only:']),
   forall(member(X,Bag),cuf_out(message,[X])).
trace_filter_message1 :-
   cuf_out(message,['tracing all predicates']).

trace_depth_message :-
   cuf_trace_depth(Min,Max),
   cuf_out(message,['min.','depth for tracing:',Min]),
   ( Max > 0 -> cuf_out(message,['max.','depth for tracing:',Max])
   ; true ).
trace_depth_message.



%% cufpp stuff

cufpp(search_tree) :- 
    build_sld_tree_new('#1',X),pp_sld_tree(X).
cufpp(search_tree(NodeID)) :- 
    build_sld_tree(NodeID,X),pp_sld_tree(X).
cufpp(proof(Numb)) :- 
    success_node(Numb,SN,_,_),
    write(success_node(SN)),nl,
    build_logdep_tree(SN,T),
    pp_logdep_tree(T).    
cufpp(result(Numb)) :- 
    success_node(Numb,SN,_,_),
    write(success_node(SN)),nl,
    result_internal(SN,Result, ConsOut),
    pp_fs(Result,ConsOut).
cufpp(result_at(StopNodeID)) :- 
    result_internal(StopNodeID, Result, ConsOut),
    pp_fs(Result,ConsOut).
/*cufpp(goalarg_at(GoalID,ExtractNodeID,StopNodeID,ArgNo)) :-
    clause(cuf_top_level_goal('#1', InitialGoals, InitialConstraints,_),true),
    content_of_goalarg_at(GoalID, ExtractNodeID, StopNodeID, ArgNo,
			  InitialGoals, InitialConstraints, Cuf_Arg, Cuf_Constraints),
    pp_fs(Cuf_Arg, Cuf_Constraints).*/
cufpp(goal_list(NodeID)) :- 
    ( success_node(_,NodeID,_,_) ->
	  cuf_out(message, ['empty goal list'])
    ; resolve_port(NodeID,_,_,_,_,_,_) ->
	  goal_list_internal(NodeID,GL,CL,_),
	  cuf_out(message, ['$GOAL LIST$'(GL, CL)])
    ; cuf_out(message, ['NodeID unknown:',NodeID])
    ).
cufpp(goal(GoalID, ExtractNodeID)) :- 
    ( resolve_port(ExtractNodeID,_,_,_,_,_,_) ->
	  goal_list_internal(ExtractNodeID,GL,_,_),
	  member(WG, GL),
	  goalID(WG, GoalID),
	  wrap_goal(G, WG),
	  cuf_out(message, ['$GOAL$'(G)])
    ; cuf_out(message, ['NodeID unknown:',ExtractNodeID])
    ).
cufpp(goal_at(GoalID, ExtractNodeID,StopNodeID)) :- 
    ( resolve_port(ExtractNodeID,_,_,_,_,_,_) ->
	  goal_list_internal(ExtractNodeID,GL,Cons,_),
	  member(WG, GL),
	  goal_list(ExtractNodeID,StopNodeID,GL,Cons,_,_),
	  goalID(WG, GoalID),
	  wrap_goal(G, WG),
	  cuf_out(message, ['$GOAL$'(G)])
    ; cuf_out(message, ['NodeID unknown:',ExtractNodeID])
    ).
/*cufpp(logdep_tree(NodeID)) :-
    ( ( success_node(_,NodeID,_,_) ; resolve_port(NodeID,_,_,_,_,_,_) ) ->
	    build_logdep_tree(NodeID,T),
	    pp_logdep_tree(T)
    ; cuf_out(message, ['NodeID unknown:',NodeID])
    ).*/
cufpp(ancestors(GoalID)) :-
    ( GoalID = ParID:NthSubGoal, resolve_port(_,ParID,_,NthSubGoal,_,_,_) ->
	  ancestor_list(GoalID,List)
	  %forall(member(NID:Num:Func/Ar,List),(Arity is Ar-1, cuf_out(message,[NID:Num,Func/Arity])))
    ; cuf_out(message, ['GoalID unknown:',GoalID])
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: deletion of intermediate code of CUF files    %%%
%%%      Created: Fri Oct 16 09:47:48 1992                      %%%
%%%     Modified: Tue Mar 26 09:51:42 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.15  1996/04/09 13:19:35  jochen
% module prefix for trans_output/entry_info facts;
% yn_questions for unloading removed
%
% Revision 1.14  1995/08/03 10:36:13  michel
% abs/2 renamed to typeID_abs/2
%
% Revision 1.13  1994/05/19  14:11:17  junger
% - deletion of cuf_foreign/3 added
%
% Revision 1.12  1994/05/17  17:10:27  michel
% defined_retracted/5:
% * (KindOf == const ; KindOf == type) case added
%
% Revision 1.11  1994/05/16  16:27:38  michel
% used_retracted/5:
% * built-in symbol case (bug fix)
% undo_flag_settings/1
% * nonvar check for safety reasons (bug fix)
%
% Revision 1.10  1994/05/10  12:07:58  michel
% delete_db_entries/1:
% 	FID == tmp case for removing dynamic symbol entries
% retractall_feat_entries1/1:
% 	axioms for polyfeatures are `added_axiom(sys,_,_)' now
%
% Revision 1.9  1994/02/18  19:04:30  michel
% recompiling_task/3, 2nd clause added for correct polyfeature treatment
% (bug fix)
%
% Revision 1.8  1994/02/03  11:46:33  junger
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9
%
% Revision 1.7  1994/01/24  12:07:48  jochen
% recycle_typeIDs now only called for generated (dom.) types of undecl. features
%
% Revision 1.6  1994/01/18  12:03:12  jochen
% corrected handling of 'STRING_CONST' facts: in used_retracted/5, retract_dependents/4
%
% Revision 1.5  1994/01/14  13:55:17  jochen
% delete_index_tables0 added, for call in run_compilation (compiler)
%
% Revision 1.4  1994/01/10  11:08:12  jochen
% 'Counter' typeID now handled with new ID predicates (setID/2,
% newID/3, recycleID/2).
% inc_type_counters/1 renamed to recycle_typeIDs/1.
%
% Revision 1.3  1993/12/23  13:12:45  michel
% unload_CUF_files/1: first clause added
% new predicates: unload_current_grammar/1, check_unloading/1,
%                 unload_grammar/1, unload_control/2
%
% Revision 1.2  1993/12/20  16:49:34  michel
% unload_CUF_grammar/1 renamed to unload_CUF_file/1
% unload_CUF_files/1 added
%
% Revision 1.1  1993/11/08  13:49:45  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Wed Jul 21 20:16:43 1993 (dorna) : recompiling_task/3 --> recompiling_task/5
% Tue Jul 20 17:54:12 1993 (dorna) : get_TID removed
% Tue Jul 20 17:49:38 1993 (dorna) : refresh_symbol_table changed
% Thu Jun 17 14:38:39 1993 (michel) : retract_generated_tests: 'POLY'/5 format
% Wed Jun  2 16:26:17 1993 (michel) : retraction of generated axioms (polyfeature handling)
% Thu Apr  8 11:41:13 1993 (michel) : retractall_feat_entries1/1 always succeeds
% Tue Apr  6 22:50:42 1993 (michel) : parser output format modifications
% Wed Mar 31 15:39:00 1993 (michel) : cuf_abs_file_name/4 used
% Thu Dec 17 11:13:33 1992 (michel) : gram_dir --> cuf_code
% Wed Dec 16 13:29:02 1992 (michel) : format changes
% Tue Dec  1 17:45:06 1992 (michel) : absolute_file_name2/2 added, errmsgs removed
% Wed Oct 28 18:40:15 1992 (michel) : recompiling_task/3 changed
% Wed Oct 28 16:45:45 1992 (michel) : retractall_added_axioms/1 changed
% Wed Oct 28 14:25:21 1992 (michel) : refresh_symbol_table/1 changed
% Tue Oct 27 13:48:21 1992 (michel) : delete_file --> erase_file (ctrl.file)

%%%  unload_CUF_file(+RelativeFileName)
%%%  unload_CUF_files(+ListOfRelativeFileNames)
%%%  deletes the named file(s) by erasing internal code produced by 
%%%  the compiler
unload_CUF_file(RelName) :-
    ( atomic(RelName) ->
	  ( cuf_abs_file_name(RelName, AbsName, cuf, exist) ->
		( file_table(check, AbsName, _, FileID, old) ->
%		      ( yn_question(['Unload file',AbsName,'? (y/n)']) ->
		      ( true ->
			    delete_db_entries(FileID),
			    delete_contol_file(FileID),
%			    retractall(cuf_task(recompile,FileID)),
			    compiler
		      ; true )
		; cuf_out(error, ['grammar file unknown:',RelName,'!!!'])
		)
	  ; cuf_out(error, ['grammar file',RelName,
	                    'unknown or file name not expandable'])
	  )
    ; cuf_out(error, ['a file name has to be atomic'])
    ).

unload_CUF_files([]) :- !.
unload_CUF_files(Files) :-
	check_file_list(Files,Files0),
	( Files0==[] -> fail ; true ),
	check_unloading(Files0,Flag),
	( Flag == yes ->
	      compiler
	; true
	),!.

check_unloading([],_).
check_unloading([RelName|R],Flag) :-
	( cuf_abs_file_name(RelName, AbsName, cuf, exist) ->
	      ( file_table(check, AbsName, _, FileID, old) ->
%		    ( yn_question(['Unload file',AbsName,'? (y/n)']) ->
		    ( true ->
			  delete_db_entries(FileID),
			  delete_contol_file(FileID),
			  retractall(cuf_task(recompile,FileID)),
			  Flag=yes
		    ; true )
	      ; cuf_out(error, ['grammar file unknown:',RelName,'!!!'])
	      )
	; cuf_out(error, ['grammar file',RelName,
	                  'unknown or file name not expandable'])
	),
	check_unloading(R,Flag).

check_unloading([]).
check_unloading([AbsName|R]) :-
	file_table(check, AbsName, _, FileID, old),
%	( yn_question(['Unload file',AbsName,'? (y/n)']) ->
	( true ->
	      delete_db_entries(FileID),
	      delete_contol_file(FileID),
	      retractall(cuf_task(recompile,FileID))
	; true ),
	check_unloading(R).

unload_current_grammar(GrammarName) :-
%    ( yn_question(['Unload whole grammar',GrammarName,'? (y/n)']) ->
    ( true ->
	  grammar_files(GrammarName,Files),
	  unload_grammar(Files),
	  ( control_files(GrammarName,CFiles) ->
		unload_control(CFiles,CFlag),
		( CFlag == yes -> true
%		      assert(cuf_task(finish_grammar,GrammarName)),
		; true
		)
	  ; true
	  ),
	  retractall(current_grammar(GrammarName)),
	  compiler
    ; yn_question(['Unload part of grammar',GrammarName,'? (y/n)']) ->
	  grammar_files(GrammarName,Files),
	  check_unloading(Files),
	  compiler
    ; true
    ).

unload_grammar([]).
unload_grammar([File|Files]) :-
	( file_table(check, File, _, FileID, old) ->
	      delete_db_entries(FileID),
	      delete_contol_file(FileID),
	      retractall(cuf_task(recompile,FileID))
	; true
	),
	unload_grammar(Files).

unload_control([],_).
unload_control([File|Files],Flag) :-
   erase_file(File),Flag=yes,
   unload_control(Files,Flag).


%%%  delete_db_entries(+FileID)
%%%  erases the internal code of the file with the id FileID completely
delete_db_entries(FID) :-
    retractall(FID:entry_info(_,_,_,_,_)),
    delete_db_entries1(unload, FID),
    delete_parser_output(FID,_),
    ( FID == tmp -> true
    ; file_table(retract, _, _, FID, old)
    ).

%%%  delete_db_entries1(+(Re)CompilationFlag, +FileID)
%%%  erases the internal code of the file with the id FileID for
%%%  recompilation without reloading
delete_db_entries1(Flag, FID) :-
    retractall(FID:trans_output(_,_,_,_,_,_)),
    retractall('rule counter'(FID, _, _)),
    retractall(cuf_foreign(_,_,FID)),
    delete_index_tables(FID),
    retractall('CUF clause'(_,FID,_,_,_,_,_,_,_)),
    retractall_added_axioms(FID),
    undo_flag_settings(FID),
    ( Flag == unload ->
         refresh_symbol_table(FID)
    ; /* Flag == recompile -> */
      true
    ),
    retract_sort_decls(FID),
    retract_feat_decls(FID).

retractall_added_axioms(FID) :-
	( added_axiom(FID,_,_) ->
	      set_flag(axiom_system, new),
	      retractall(added_axiom(FID,_,_))
	; true ).
	
%%%  refresh_symbol_table(+FileID)
%%%  modification of the current symbol table
%%%  FileID stands for the file to be deleted
refresh_symbol_table(FID) :-
	retract('DEF OCC'(Symbol, Arity, KindOf, FID, EID)),
	defined_retracted(Symbol, Arity, KindOf, FID, EID),
	fail.
refresh_symbol_table(FID) :-
	retract('USED OCC'(Symbol, Arity, KindOf, FID, EID)),
	used_retracted(Symbol, Arity, KindOf, FID, EID),
	fail.
refresh_symbol_table(_).

defined_retracted(Symbol, Arity, KindOf, FID, EID) :-
   check_recompiling(KindOf, Symbol, Arity, FID, EID),
   \+ 'DEF OCC'(Symbol, Arity, KindOf, _, _),
   % look for other occurences of this symbol
   ( (KindOf == const ; KindOf == type) -> true % recompilation tests above
   ; recompiling_task(Symbol, Arity, KindOf, FID, EID)
   ),
   \+ 'USED OCC'(Symbol, Arity, KindOf, FID, _),
   symbol_table(Symbol, Arity, TID, _, KindOf),
   retract_dependents(KindOf, Symbol, Arity, TID), !.

used_retracted(Symbol, 0, const, _, _) :-
   'STRING_CONST'(Symbol),              %% asserted in cuf_parser.pl
   \+ 'USED OCC'(Symbol, 0, const, _, _),
   \+ 'DEF OCC'(Symbol, 0, const, _, _), !,
   retract('STRING_CONST'(Symbol)).     %% not in symbol_table
%% nothing to do for numbers not in symbol_table
used_retracted(Symbol, Arity, KindOf, _, _) :-
   symbol_table(Symbol, Arity, TID, X, KindOf),
   ( X == (-), !,              % symbol was classified!
     \+ 'USED OCC'(Symbol, Arity, KindOf, _, _),
     retract_dependents(KindOf, Symbol,Arity,TID)
   ; X == (+)                  % built-in symbol
   ), !.
used_retracted(Symbol, Arity, KindOf, FID, _) :-
   \+ 'USED OCC'(Symbol, Arity, KindOf, FID, _),
   \+ 'DEF OCC'(Symbol, Arity, KindOf, _, _),
   symbol_table(Symbol, Arity, TID, _, KindOf),
   retract_dependents(KindOf, Symbol, Arity, TID), !.

% type and constant removing causes always a recompilation
check_recompiling(type, Symbol, Arity, FID, EID) :-
   recompiling_task(Symbol, Arity, type, FID, EID), !.
check_recompiling(const, Symbol, Arity, FID, EID) :-
   recompiling_task(Symbol, Arity, const, FID, EID), !.
check_recompiling(_,_,_,_,_).

retract_dependents(type, Symbol, Arity, TID) :-
   type_tbl(retract, Symbol, TID),
   symbol_table(retract, Symbol, Arity, TID, _, type, _).
retract_dependents(const, Symbol, Arity, TID) :-
   const_tbl(retract, Symbol, TID),
   symbol_table(retract, Symbol, Arity, TID, _, const, _),
   ( retract('STRING_CONST'(Symbol)), !  %% for defined strings
   ; true
   ).
retract_dependents(feat, Symbol, Arity, TID) :-
   retractall_feat_entries(Symbol),
   symbol_table(retract, Symbol, Arity, TID, _, feat, _).
retract_dependents(sort, Symbol, Arity, TID) :-
   symbol_table(retract, Symbol, Arity, TID, _, sort, _).


%%%  retractall_feat_entries(+Feature)
%%%  Feature was deleted in the symbol table, so erase all intermediate
%%%  code containing it
retractall_feat_entries(Feat) :-
    retractall_feat_decls(Feat),
    retractall_feat_entries1(Feat).

retractall_feat_entries1(Feat) :-
    ( retract(mono_feat(Feat, _, _))
    ; retract(poly_feat(Feat, _)),
      retract(feat_doms(Feat, _)),
      retractall(added_axiom(sys,Feat,_)),
      retract(poly_feat_check(Feat, _, _, Goals)),
      retract_generated_tests(Goals)
    ; true), !.

%%%  retractall_feat_decls(+Feature)
%%%  retracts all delarations belonging to Feature
retractall_feat_decls(Feat) :-
    retract(feat_decl(Feat,Dom,_Ran,_,_)),
    (symbol_table(check,Feat,1,_,-,feat,ok) -> recycle_typeIDs(Dom)
    ; true ),
%    recycle_typeIDs(Ran),
    fail.
retractall_feat_decls(_).

retractall_feat_decls(Feat, FID, EID) :-
    retract(feat_decl(Feat,Dom,_Ran,FID,EID)),
    (symbol_table(check,Feat,1,_,-,feat,ok) -> recycle_typeIDs(Dom)
    ; true ),
%    recycle_typeIDs(Ran),
    fail.
retractall_feat_decls(_,_,_).


%%%  retract_generated_tests(+ListOfGeneratedTests)
%%%  erases all entries belonging to each generated test in 
%%%  ListOfGeneratedTests
retract_generated_tests([]).
retract_generated_tests(['POLY'(ID,_,_)|R]) :-
	retract('POLY'(ID,_,_,_,_)),
	retract_generated_tests(R).

%%%  retract_feat_decls(+FileID)
%%%  retracts all feature declarations belonging to FileID
retract_feat_decls(FID) :-
    retract(feat_decl(Feat, Dom, _Ran, FID, EID)),
    (symbol_table(check,Feat,1,_,-,feat,ok) -> recycle_typeIDs(Dom)
    ; true ),
%    recycle_typeIDs(Ran),
    recompiling_task(Feat, 1, feat, FID, EID),
    fail.
retract_feat_decls(_).

%%%  recycle_typeIDs(+CNF)
%%%  returning unused typeID values
%%%  only used for type codes generated for undef. features (not in
%%%  type_code table)
recycle_typeIDs([]) :- !.
recycle_typeIDs([F|R]) :- !,
	recycle_typeIDs(F),
	recycle_typeIDs(R).
recycle_typeIDs(TID) :-
	typeID_abs(TID, ATID),
	( type_code(ATID, _) -> true        % not generated type
%% better: is_const(_,ATID) -> recycleID(constID, ATID) 
	; recycleID(typeID, ATID)
	).

typeID_abs(TID, ATID) :-
	integer(TID),
	TID < 0, !,
	ATID is 0-TID.
typeID_abs(TID, TID).

%%%  retract_sort+decl(+FileID)
%%%  retracts all sort declarations belonging to FileID
retract_sort_decls(FID) :-
    retract(sort_decl(Sort, Arity, _, FID, EID)),
    recompiling_task(Sort, Arity, sort, FID, EID),
    fail.
retract_sort_decls(_).

%%%  delete_index_tables(+FileID)
%%%  deletes index tables generated by FileID
delete_index_tables(FID) :-
   delete_index_tables0(FID),
   retract(index_table(Name, Arity, FID)),
   Arity1 is Arity+1,   
   ( index_table(Name, Arity, _) ->
	 index_table_name(Name, Arity1, TableName, TableArity)
   ; retract(index_table_name(Name, Arity1, TableName, TableArity))
   ),
   fail.
delete_index_tables(_).

delete_index_tables0(FID) :-
   current_predicate(index_table,index_table(_,_,_)),
   index_table(Name, Arity, FID),
   Arity1 is Arity+1,
   index_table_name(Name, Arity1, TableName, TableArity),
   functor(TableCall, TableName, TableArity),
   arg(2, TableCall, FID),
   retractall(TableCall),
   fail.
delete_index_tables0(_FID).

%%%  undo_flag_settings(+FileID)
%%%  resets old flag values
undo_flag_settings(FID) :-
	nonvar(FID),
	current_predicate(flag_setting, flag_setting(_,_,_,_)),
	retract(flag_setting(FID, Flag, Value, OldValue)),
	get_flag(Flag, Value),
	set_flag(Flag, OldValue),
	fail.
undo_flag_settings(_FID).

%%%  delete_contol_file(+FileID)
delete_contol_file(FID) :-
    retract(control_file(FID, FileName)),
    erase_file(FileName),
    fail.
delete_contol_file(_).

%%%  recompiling_task(+KindOf, +Symbol/Arity, +FID)
%%%  checks if symbol is used in an other file; if true, recompile it
recompiling_task(KindOf, Symbol/Arity, FID) :-
	'USED OCC'(Symbol, Arity, KindOf, FID1, _),
	FID \== FID1,
	assert_if_new(cuf_task(recompile, FID1)),
	fail.
recompiling_task(feat, Symbol/Arity, FID) :- % for polyfeatures!
	'DEF OCC'(Symbol, Arity, feat, FID1, _),
	FID \== FID1,
	assert_if_new(cuf_task(recompile, FID1)),
	fail.
recompiling_task(_,_,_).

%% future format, recompiling_task/3 will be changed
recompiling_task(Symbol, Arity, KindOf, FID, _EID) :-
   recompiling_task(KindOf, Symbol/Arity, FID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: loading of CUF files                          %%%
%%%      Created: Fri Oct 16 10:48:01 1992                      %%%
%%%     Modified: Tue Mar 26 09:53:51 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.20  1996/04/09 13:34:30  jochen
% module prefix for entry_info facts;
% .ctrl-files now compiled in module cuf
%
% Revision 1.19  1996/03/14 17:11:08  jochen
% changes due to changes in cuf_reader/tokenizer
% sped up singleton checking
%
% Revision 1.18  1994/07/20  15:49:26  michel
% * "init_sat, add_axioms" at different places added
% * load_tmp_for_file/2: compiler_init call moved to the right position
%
% Revision 1.17  1994/06/22  15:04:45  michel
% * load_tmp_for_file/2 modified
% * further minor changes
%
% Revision 1.16  1994/06/21  13:28:08  michel
% * some silly output messages changed
% * load_init, compiler_finish:
%   during compilation the xmfed_output and tracing flag is set
%   to `no' and reset afterwards
%
% Revision 1.15  1994/05/16  16:31:04  michel
% load_current_grammar/1, load_CUF_files1/1:
% * force_reloading flag handling added
%
% Revision 1.14  1994/05/06  18:57:14  michel
% load_CUF_grammar/1: difference/3 checks difference between current files and
%                     grammar files before unloading now (bug fix)
%
% Revision 1.13  1994/02/18  10:53:01  michel
% singleton variable check added
% new predicates: singleton_check/{1,4}, check_vars/2
%
% Revision 1.12  1994/01/10  11:04:43  jochen
% 'Counter' entryID now handled with new ID predicates (setID/2,
% newID/3). We don't do del_counter(entryID) in load_finish/1 any more,
% since setID resets anyway.
%
% Revision 1.11  1993/12/23  13:46:39  michel
% term_expansion for grammar_files/2, control_files/2 and flag/2 facts
% new predicates: exp_file_names/4, load_grammar_control/1,
%                 load_control_files/1, load_CUF_grammar/1,
%                 load_current_grammar/1, load_grammar/2
% load_CUF_control1/1 renamed: now load_control_file/1
%
% Revision 1.10  1993/12/20  17:56:43  michel
% load_finish/0 replaced by load_finish/1 for syntax_check_only option
%
% Revision 1.9  1993/12/20  17:10:53  michel
% load_CUF_grammar/* renamed to load_CUF_file(s)/*
% check_file_list/2 changed
%
% Revision 1.8  1993/12/10  19:06:24  michel
% load_CUF_control1/1 changed:
% no_style_check(discontiguous) during ctrl file compilation
%
% Revision 1.7  1993/12/09  15:34:16  michel
% -> operator setting for loading a ctrl file
% (dangerous, if ctrl file contains if-then-else constructs!!!!)
%
% Revision 1.6  1993/12/08  10:37:18  michel
% compiler_finish  removes entryID and fileID flags now
%
% Revision 1.5  1993/12/07  17:18:55  michel
% "1 warnings" bug fixed
%
% Revision 1.4  1993/11/24  17:40:40  michel
% new predicate compiler_finish/0; replaces load_finish/0 at some places
%
% Revision 1.3  1993/11/24  17:23:39  michel
% load_finish/0 changed: error and warning counter setting removed
%
% Revision 1.2  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%


% History (before RCS):
% Fri Jun 18 19:58:07 1993 (michel) : file_loading+init_sat
% Wed Jun  2 20:44:03 1993 (michel) : load_finish changed
% Mon Apr  5 14:11:16 1993 (michel) : warning and op-changing only once
% Wed Mar 31 15:53:05 1993 (michel) : cuf_abs_file_name/4 used
% Thu Dec 17 11:12:18 1992 (michel) : gram_dir --> cuf_code
% Thu Dec  3 15:28:35 1992 (michel) : load_CUF_grammar: init_flags removed
% Thu Dec  3 11:20:43 1992 (michel) : load_init/_finish changed
% Wed Dec  2 10:43:32 1992 (michel) : check_file_list/2 added
% Tue Dec  1 18:18:09 1992 (michel) : absolute_file_name1/2 added,
%                                     output changed
% Tue Dec  1 16:39:57 1992 (michel) : load_CUF_file/1 --> load_CUF_file/2,
%                                     parse_CUF_expr/2 --> parse_CUF_expr/3
% Tue Nov 10 15:50:14 1992 (michel) : load_CUF_files: local file searching

:- multifile term_expansion/2.

:- dynamic current_grammar/1,
	   grammar_files/2,
	   control_files/2.

term_expansion(grammar_files(Gram,Files),[]) :- !,
	( atomic(Gram) ->
	      ( Files == [] ->
		    cuf_out(error, ['no CUF files specified for grammar',
		                    Gram]),
		    cuf_out(error,['compilation failed for',
		                   grammar_files(Gram,Files)])
	      ; grammar_files(Gram,_) ->
	            cuf_out(error, [grammar,Gram,'already specified']),
		    cuf_out(error,['ignoring',grammar_files(Gram,Files)])
	      ; exp_file_names(Files,ExpFiles,Gram,cuf) ->
		    assert(grammar_files(Gram,ExpFiles))
	      ; cuf_out(error,['compilation failed for',
		               grammar_files(Gram,Files)])
	      )
	; cuf_out(error, ['a grammar name has to be atomic']),
	  cuf_out(error,['compilation failed for',
		         grammar_files(Gram,Files)])
	).

term_expansion(control_files(Gram,Files),[]) :- !,
	( atomic(Gram) ->
	      ( Files == [] ->
		    cuf_out(warning, ['no control files specified for grammar',
		                      Gram])
	      ; control_files(Gram,_) ->
	            cuf_out(error, ['control files for grammar',Gram,
		                    'already specified']),
		    cuf_out(error,['ignoring',control_files(Gram,Files)])
	      ; exp_file_names(Files,ExpFiles,Gram,ctrl) ->
		    assert(control_files(Gram,ExpFiles))
	      ; cuf_out(error,['compilation failed for',
		               control_files(Gram,Files)])
	      )
	; cuf_out(error, ['a grammar name has to be atomic']),
	  cuf_out(error,['compilation failed for',control_files(Gram,Files)])
	).

term_expansion(flag(Name,Value),[]) :- !,
        ( flag(Name,Value)
	; cuf_out(error,['compilation failed for',flag(Name,Value)])
	).

exp_file_names([], [], _, _).
exp_file_names([RelFileName|R],[AbsFileName|RR],Gram,Ext) :-
	( atomic(RelFileName) ->
	  ( cuf_abs_file_name(RelFileName, AbsFileName, Ext, read) ->
		exp_file_names(R,RR,Gram,Ext)
	  ; (Ext==ctrl -> File=control ; File='CUF' ),
	    cuf_out(error, [File,file,RelFileName,'of grammar',Gram,
	                    'unknown or file name not expandable']),           
	    !,fail
	  )
	; cuf_out(error, ['a file name has to be atomic']),
	  !,fail
	).

%%%%%%%%%%%%%%%%%%%%  control files

%%%  load_CUF_control(+File)
%%%  loads and compiles a CUF control file
%%%  compilation by term_expansion/2 for delay/2, ... facts
load_CUF_control(RelFileName) :-
    ( atomic(RelFileName) ->
	  ( cuf_abs_file_name(RelFileName, AbsFileName, ctrl, read) ->
		cuf_out(message, ['compiling control file:']),
		load_control_file(AbsFileName)
	  ; cuf_out(error, ['control file',RelFileName,
	                    'unknown or file name not expandable']),
            !, fail
	  )
    ; cuf_out(error, ['argument (file name) has to be atomic']),
      !, fail
    ).

load_control_file(AbsoluteFileName) :- 
   erase_file(AbsoluteFileName),
   current_op(X,Y,->),
   op(999,xfx,->),
   no_style_check(discontiguous),
   /*RB cuf:*/compile(AbsoluteFileName),
   op(X,Y,->),
   style_check(discontiguous).

%%%  load_grammar_control(+GrammarName)
load_grammar_control(Gram) :-
    ( control_files(Gram,CTRL_Files) ->
	cuf_out(message, ['compiling control file(s) for grammar',Gram:' ']),
        load_control_files(CTRL_Files),
	process_folders
    ; cuf_out(warning, ['no control file(s) specified for grammar',Gram])
    ).

load_control_files([]).
load_control_files([FileName|R]) :-
    load_control_file(FileName),
    load_control_files(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%  grammar files

%%%  load_CUF_grammar(+GrammarName)
load_CUF_grammar(GrammarName) :-
	( atomic(GrammarName) ->
	      ( current_grammar(Gram) ->
		    ( Gram == GrammarName ->
			  Flag = old, load_current_grammar(GrammarName)
		    ; cuf_out(message,['current grammar is',Gram]),
		      unload_current_grammar(Gram)
		    )
	      ; true
	      ),
	      ( Flag == old ->
		    true
	      ; ( grammar_files(GrammarName,_) ->
		      file_table(current_files, Files, _, _, old),
		      grammar_files(Gram, Files1),
		      difference(Files1, Files, FilesR),
		      check_unloading(FilesR)
		; cuf_out(error, ['unknown grammar name']),
		  !, fail
		),
		retractall(current_grammar(_)),
		assert(current_grammar(GrammarName)),
		load_current_grammar(GrammarName)
	      )
	; cuf_out(error, ['grammar name has to be atomic']),
	  !, fail
	).

difference([], F, F).
difference([F|R], L, Out) :-
    ( delete(F, L, L1) ->
         difference(R, L1, Out)
    ; difference(R, L, Out)
    ).

load_current_grammar(GrammarName) :-
	grammar_files(GrammarName,CUF_Files),
        ( get_flag(force_reloading,yes) ->
	      cuf_out(message, ['unloading files, please wait ...']),
	      unload_grammar(CUF_Files)
	; true),
	cuf_out(message, ['(re)loading grammar',GrammarName:'']),
        cuf_out(warning,['Do not interrupt during file handling!']),
	compiler_init,
	load_grammar(CUF_Files,GrammarName),
	( del_flag(axiom_system,new) ->
	    init_sat,
	    add_axioms
	; true ),
	compiler(GrammarName),
	compiler_finish.

%%%  load_grammar(+ListOfGrammarFiles,+GrammarName)
load_grammar([],_).
load_grammar(List,Gram) :-
    cuf_ops,
    load_grammar_(List,Gram),
    old_ops.
    
load_grammar_([],_).
load_grammar_([FileName|R],Gram) :-
	file_property(FileName, modify_time, Date),
	( file_table(check, FileName, Date, _FileNo, new) ->
	      ( file_table(check, FileName, _, OldFileNo, old) ->
		    delete_db_entries(OldFileNo)
	      ; true
	      ),
	      file_table(assert, FileName, Date, FileID, new),
	      load_CUF_file(FileName, FileID),
	      assert(cuf_task(compile, FileID))
	; cuf_out(message, ['version of file',FileName,'already known'])
	),
	load_grammar_(R, Gram).

%%%%%%%%%%%%%%%%%%%%%%%%%%%  single CUF files
%%%  load_CUF_files(+File(s))
%%%  loads and compiles CUF grammar files
load_CUF_files(FileList0) :-
    cuf_out(warning,['Do not interrupt during file handling!']),
    compiler_init,
    check_file_list(FileList0, FileList), !,
    load_CUF_files0(FileList),
    ( del_flag(axiom_system,new) ->
        init_sat,
	add_axioms
    ; true ),
    compiler,
    compiler_finish.

check_file_list(File, []) :-
    var(File), !,
    cuf_out(error, ['a file name has to be a atomic']).
check_file_list([], []) :- !.
check_file_list([File|R], FileList) :- !,
    ( var(File) ->
	  cuf_out(error, ['a file name has to be a atomic']),
	  check_file_list(R, FileList)
    ; FileList=[File|Rest],
      check_file_list(R, Rest)
    ).
check_file_list(File, FileList) :-
    ( atomic(File) -> FileList=[File]
    ; cuf_out(error, ['a file name has to be a atomic']),
      FileList=[]
    ).

check_file(File, []) :-
    var(File), !,
    cuf_out(error, ['a file name has to be a atomic']).
check_file(File, [File]).


%%%  load_CUF_files(+ListOfGrammarFiles)
load_CUF_files0([]).
load_CUF_files0(List) :-
    cuf_ops,
    load_CUF_files1(List),
    old_ops.
    
load_CUF_files1([]).
load_CUF_files1([RelativeFileName|R]) :-
    ( cuf_abs_file_name(RelativeFileName, AbsoluteFileName, cuf, read) ->
	file_property(AbsoluteFileName, modify_time, Date),
	( ( get_flag(force_reloading, yes)
	  ; file_table(check, AbsoluteFileName, Date, _FileNo, new)
	  ) ->
	       ( file_table(check, AbsoluteFileName, _, OldFileNo, old) ->
		  delete_db_entries(OldFileNo)
		; true
		),
	    file_table(assert, AbsoluteFileName, Date, FileID, new),
	    load_CUF_file(AbsoluteFileName, FileID),
	    assert(cuf_task(compile, FileID))
	; cuf_out(message, ['version of file',RelativeFileName,
	                    'already known'])
	)
    ; cuf_out(error, ['CUF file',RelativeFileName,
	              'unknown/file name not expandable'])
    ),
    load_CUF_files1(R).

%%%  load_tmp_for_file(+AbsoluteFileName, +TmpFileName)
%%%  loads TmpFileName under the name of AbsoluteFileName
load_tmp_for_file(AbsoluteFileName, TmpFileName) :-
    file_property(TmpFileName, modify_time, Date),
    ( ( get_flag(force_reloading, yes)
      ; file_table(check, AbsoluteFileName, Date, _FileNo, new)
      ) -> 
           ( file_table(check, AbsoluteFileName, _, OldFileNo, old) ->
	        delete_db_entries(OldFileNo)
	    ; true
	    ),
	    file_table(assert, AbsoluteFileName, Date, FileID, new),
	    cuf_out(warning,['Do not interrupt during file handling!']),
	    compiler_init,
	    cuf_ops,
	    load_CUF_file(AbsoluteFileName, TmpFileName, FileID),
	    old_ops,
	    assert(cuf_task(compile, FileID)),
	    ( del_flag(axiom_system,new) ->
		init_sat,
		add_axioms
	    ; true )
    ; cuf_out(message, ['version of file',AbsoluteFileName,
                        'already known!!!'])
    ),
    compiler,
    compiler_finish.

%%%  load_CUF_file(+AbsoluteFileName, +TmpFileName, +FileID)
load_CUF_file(LogicalFileName, PhysicalFileName, FileID) :-
    load_init(FileID),
    load_CUF_file_(LogicalFileName, PhysicalFileName, FileID),
    load_finish(FileID).


%%%  load_CUF_file(+File, +FileID)
load_CUF_file(PhysicalFileName, FileID) :-
    load_init(FileID),
    load_CUF_file_(PhysicalFileName, FileID),
    load_finish(FileID).

load_init(FileID) :-
	set_flag(fileID, FileID), 
	setID(entryID, 1), !.


load_finish(FileID) :-
    del_flag(fileID),
    del_flag(entryID), !,
    ( get_flag(syntax_check_only,yes) ->
	  delete_db_entries(FileID)
    ; true
    ).

load_init :- compiler_init.

compiler_init :-
    get_flag(xmfed_output,Old),
    ( Old == no -> true
    ; set_flag(xmfed_old,Old),
      set_flag(xmfed_output,no)
    ),
    get_flag(tracing, Old1),
    ( Old1 == no -> true
    ; set_flag(tracing_old, Old1),
      set_flag(tracing, no)
    ),
    set_counter(errors,0),
    set_counter(warnings,0).

compiler_finish :-
    del_flag(fileID),
    del_flag(entryID),
    (del_flag(xmfed_old,Old), Old \== unknown -> set_flag(xmfed_output,Old) ; true),
    (del_flag(tracing_old,Old1), Old1 \== unknown -> set_flag(tracing, Old1) ; true),
    del_counter(errors),
    del_counter(warnings).

%%%  load_CUF_file_(+LogicalFileName, +PhysicalFileName, +FileID)
load_CUF_file_(LogicalFileName, PhysicalFileName, FileID) :-
    cuf_out(message, ['reading file',LogicalFileName:'']),
    seeing(OldStream),
    see(PhysicalFileName),
    repeat,
      cuf_read(Entry, Variables, Lines),
      ( Entry == 'A'(end_of_file), !
      ; save_entry_info(EntryID, Entry, Variables, Lines),
	parse_CUF_expr(Entry, FileID, EntryID),
	singleton_check(Entry, Variables, FileID, EntryID),
        fail
      ),
    seen,
    see(OldStream).

%%%  load_CUF_file_(+FileName, +FileID)
load_CUF_file_(FileName, FileID) :-
    cuf_out(message, ['reading file',FileName:'']),
    seeing(OldStream),
    see(FileName),
    repeat,
      cuf_read(Entry, Variables, Lines),
      ( Entry == 'A'(end_of_file), !
      ; save_entry_info(EntryID, Entry, Variables, Lines),
	parse_CUF_expr(Entry, FileID, EntryID),
	singleton_check(Entry, Variables, FileID, EntryID),
        fail
      ),
    seen,
    see(OldStream).

%%:- dynamic entry_info/6.

save_entry_info(EID, Entry, Vars, LineB-LineE) :-
    newID(entryID, EID, 1),
    get_flag(fileID, FID),
    assert(FID:entry_info(EID, LineB, LineE, Entry, Vars)).

entry_info(FID, EID, LineB, LineE, Entry, Vars) :-
    clause(FID:entry_info(EID, LineB, LineE, Entry, Vars), true).


% singleton variable check for clauses
singleton_check(':='(Head,Body), Vars, FID, EID) :-
    singleton_check(Head),
    singleton_check(Body),
    check_vars(Vars,Singletons),
    ( Singletons == [] -> true
    ; append(['singleton variable(s):'],Singletons,Msg),
      append(Msg,['$E$'(FID,EID)],Warning),
      cuf_out(warning, Warning)
    ), !.

singleton_check('V'(Var)) :- !,
   ( var(Var) -> Var=v(_)
   ; Var=v(v)).
singleton_check(Atom) :- atomic(Atom), !.
singleton_check(Term) :-
   functor(Term,_,Ar),
   singleton_check(Ar,Term).

singleton_check(N, T) :-
    ( N=:=0 -> true
    ; arg(N,T,Arg),
      singleton_check(Arg),
      N1 is N-1,
      singleton_check(N1,T)
    ).

check_vars([], []).
check_vars([Name=v(V)|R], Singletons) :-
   ( var(V),
     \+ atom_chars(Name, [0'_|_]) ->
        Singletons = [Name|RR]
   ; RR = Singletons
   ),
   check_vars(R, RR).

%%%  cuf_ops
%%%  change operators for CUF loading
cuf_ops :-
    cuf_op(P, K, N),
    cuf_ops(P, K, N),
    fail.
cuf_ops.

cuf_ops(P, K, N) :-
    ( current_op(CP, CK, N) ->
	  assertz('SYSOP'(CP, CK, N))
    ; assertz('SYSOP'(0, K, N))
    ),
    op(P, K, N).

%%%  old_ops
%%%  undo operator settings
old_ops :-
    retract('SYSOP'(P, K, N)),
    op(P, K, N),
    fail.
old_ops.
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: interperter for CUF                           %%%
%%%      Created: Sat Jun 20 20:14:59 1992                      %%%
%%%     Modified: Fri Mar 24 15:21:45 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.15  1995/03/24 14:51:56  jochen
% Quintus-dependency removed (gensym reset)
% undo/1 eliminated (was not correct in Sicstus version)
%
% Revision 1.14  1994/07/20  15:25:27  junger
% foreign_result/4 facts are added, which save the result of foreign
% calls for debugging purposes.
%
% write_success_node/1 -> write_success_node/2; now it is saved in
% success_node whether there are residuated goals at the end of a proof
% or not
%
% Revision 1.13  1994/07/04  16:12:59  jochen
% cut added in treat_delayed(all,...); bug fix
%
% Revision 1.12  1994/07/01  14:57:40  junger
% - incremental proofs start now with the supplied node ID
%
% Revision 1.11  1994/06/27  10:10:28  jochen
% depth-exhausted msg now with nodeIDs in debug mode
%
% Revision 1.10  1994/06/21  13:22:31  jochen
% write_trace_info reorganized; handling of trace_filter added
% producing stuf_traceInfo also for success_nodes now
%
% Revision 1.9  1994/06/20  13:06:15  junger
% - hard delaying for foreign calls added
%
% Revision 1.8  1994/05/10  12:33:32  jochen
% write_success_node now also works for goal-less 'proofs' with debug-flag set
%
% Revision 1.7  1994/05/10  11:34:05  michel
% resolve/9: '$OR$'/11 cases removed, not neccessary anymore
%
% Revision 1.6  1994/03/18  09:45:18  jochen
% changed output of 'NO UNDELAYED FOUND' message
%
% Revision 1.5  1994/02/21  15:19:04  jochen
%  - tracing output changed: write out depth instead of indentation
%    flag choice_points no longer exists; use tracing=yes,
% 	trace_structures=no instead
%  - interdependency of debug and tracing flag removed: debug forces assertion
%    of debug info (for searchtree generation), tracing shows usual trace
%  - fact cuf_debugging(X) has X=1 if a debugging-relevant flag is on
%
% Revision 1.4  1994/02/03  10:59:25  junger
% - Adaptions to new goal and clause format (rule/8 --> 'CUF clause'/9)
% - debug-mode added: if debug-flag is set to yes information about the
%   proof is asserted, which gives the possibility to simulate the proof
% - depth bound also counts for deterministic resolution
% - bug removed: current_predicate('CHECK_TRIGGER'....)
%
% Revision 1.3  1993/12/10  17:48:13  michel
% is_undelayed/1: wait_condition case added
%
% Revision 1.2  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%


% History (old):
% Thu Nov 18 14:39:15 1993 (jochen) : Trigger and Folder handling added (for Earley ded.)
% Mon Sep 13 13:12:57 1993 (michel) : check_POLY_sat/1 added
% Mon Sep 13 12:34:26 1993 (michel) : /* ... */ removed (see resolve_interactive.pl now)
% Tue Jul 27 15:46:43 1993 (jochen) : use '$GOAL' wrapper for goals in cuf_out
% Mon Feb 22 19:59:29 1993 (michel) : rerun added
% Mon Feb  1 15:58:23 1993 (michel) : run/4 --> run/5
% Wed Dec 23 12:04:40 1992 (michel) : rule/6 --> rule/8
% Thu Dec  3 12:18:02 1992 (michel) : output changed
% Tue Dec  1 13:53:55 1992 (michel) : get_rule_number/5 removed,
%                                     RuleNo is now a triple of [FID, EID, RNo]
% Tue Nov 24 16:58:10 1992 (michel) : resolve1 changed
% Fri Nov 20 11:15:56 1992 (michel) : member/3 --> member_no/3



/*resolve_ports for resolution step are asserted as first clause of all resolve_ports. 
This is done this way because then it is easy to establish the link between
a node in the SLD tree and his mother by just taking the first clause of
resolve_port with type exit, which gives the information needed, before asserting
the resolve_port for the actual resolution step. Unfortunately, this treatment works
only straightforward, when the proof is done in one step. If the proof shall be done 
incrementally, one needs a facility to memorize the parent node of the start node 
for the incremental proof, because it may be the case that the last resolve_port 
belongs to another branch of the sld tree. The storing can be done in two ways: 

-asserting the parent node ID
-introducing two more arguments for handing down the parent node ID

where the parent node ID can be gained from the search_tree_link clauses. 
At the moment, the first option is used in the system. */




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  CUF interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic resolve_port/7, resolve_port_parent_access/2, success_node/4,
	   depth_exhausted/2, search_tree_link/3, foreign_result/4.





%%%%%%%  run(+Number, -Structure, +MaxRecursionDepth, -SolutionDepth)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  old run/5 renamed as run1/5: proves a test sentecence with number Number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(SentenceNo, Structure, MaxDepth, ConsOut, SolDepth) :-
%     ( get_flag(debug,yes) -> debug_init
%     ; true
%     ),
%     proof_init,
%     cuf_extern2intern(test(SentenceNo),[Intern],ConsIn,Structure),
%     goalID(Intern,[]:1),
%     prove([Intern], [], ConsIn, ConsOut, 0, MaxDepth, SolDepth, RuleNoList, all),
%     write_success_node(RuleNoList).

debug_init :-
    retractall(resolve_port(_,_,_,_,_,_,_)),
    retractall(search_tree_link(_,_,_)),
%    retractall(strings:gensym_counter('#',_)),
    reset_gensym_counter('#'),      %% see $PROLOG/src/prolog.pl
    retractall(foreign_result(_,_,_,_)).


ifdebug(Call) :-
    cuf_debugging(Flag),
    (Flag =:= 1 -> Call ; true).

%% proof_init
%% called in top_level.pl and prooftree.pl
proof_init :-
    retractall(depth_exhausted(_,_)),
    retractall(incremental_depth_exhausted(_,_)),
    retractall(success_node(_,_,_,_)),
    retractall(nthsolution(_)),
    retractall(incremental_nthsolution(_)),
    retractall(start_incremental_proof(_)),
    assert(nthsolution(0)),
    assert(incremental_nthsolution(0)),
    ( get_flag(debug,no) -> true
    ; debug_init
    ).


%%%%%%%  prove(+GoalList, -OutGoalList, +ConstraintsIn, -ConstraintsOut,
%%%%%%%        +Indentation, +RecursionBound, -SolutionDepth,
%%%%%%%        -RuleNoList, +Mode) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  main prove procedure
%%%  RuleNoList will contain a list of numbers for all non-deterministic
%%%  choice points in the prove. The number identifies the selected
%%%  clause for the solution.
%%%  Mode can be either 'all', then OutGoalList will be empty, or
%%%  'undelayed_only', in which case OutGoalList will contain delayed
%%%  goals. 
%%%  Indentation is the number of resolution steps so far
%%%  (deterministic and nondet.), used for indenting output when
%%%  tracing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prove([], [], ConsIn, ConsOut, Depth, _, Depth, [], _) :- !,
    check_constraints(ConsIn, ConsOut),
    check_POLY_sat(ConsOut).
prove(Goals, OutGoals, ConsIn, ConsOut, Ind, MaxDepth, SolDepth,
      RuleNoList, Mode) :-
    expand_det(Goals, NewGoals, ConsIn, NewCons, Ind, MaxDepth, NewInd), !,
    check_constraints(NewCons, NewCons1), !,
    ( NewGoals == [] ->
           OutGoals = [],
           SolDepth = NewInd, % MaxDepth,
           RuleNoList = [],
           ConsOut = NewCons1,
           check_POLY_sat(ConsOut)
%    ; Mode == rerun ->
%         RuleNoList = [RuleNo|RuleNos],
%         prove_nondet(NewGoals, SubGoals, NewCons1, NewCons2,
%                      NewInd, MaxDepth, Depth1, RuleNo),
%         NewInd1 is NewInd+1,
%         prove(SubGoals, OutGoals, NewCons2, ConsOut,
%               NewInd1, Depth1, SolDepth, RuleNos, Mode)
    ; current_predicate('TRIGGER GOAL','TRIGGER GOAL'(_,_,_)),
        %% triggers or folders exist
      del_trigger_folder(NewGoals,Selected,RestGoals) ->
                      OutGoals = [Selected|RestGoals],
                      SolDepth = NewInd, %MaxDepth,
                      RuleNoList = [],
                      ConsOut = NewCons1
    ; prove_nondet(NewGoals, SubGoals, NewCons1, NewCons2,
                   NewInd, MaxDepth, NewInd1, RuleNo), 
      ( RuleNo == 'NO REDUCE' ->
            treat_delayed(Mode, SubGoals, OutGoals, NewCons2, ConsOut,
                          NewInd, MaxDepth, SolDepth, RuleNoList)
      ; %% prove_nondet/6 did reduce
        RuleNoList = [RuleNo|RuleNos],
        prove(SubGoals, OutGoals, NewCons2, ConsOut,
              NewInd1, MaxDepth, SolDepth, RuleNos, Mode)
      )
    ).

treat_delayed(undelayed_only, GL, GL, Cs, Cs, Depth, _, Depth, []).

%% 'NO UNDELAYED FOUND' message displays following further info
%% dependent on flags:
%% if debug=yes outputs nodeID 
%% else if tracing\=no outputs goal list (obeying flag trace_structures)
%%      else no further info is displayed
treat_delayed(all, GoalList, OutG, ConsIn, ConsOut,
              Ind, MaxDepth, SolDepth, [RuleNo|RuleNos]) :-
	check_for_foreign_goals(GoalList, Goal, GL), !,
        ( get_flag(debug,yes) ->
	      next_nodeID(NodeID),
	      InfoString=['Node =',NodeID]
	; ( get_flag(tracing,no) -> InfoString=[]
	  ; generate_goals4output([Goal|GL],1,InfoString1),
	    InfoString=['GOAL LIST:','$NL$'|InfoString1]
	  )
	),
        write_trace(message, ['NO UNDELAYED FOUND!!!' |InfoString],
		    cuf_no_undelayed_foundInfo(NodeID)),
        resolve(Goal,SubG,GL,ConsIn,NewCons,Ind,MaxDepth,NewInd,RuleNo,non_det),
        prove(SubG,OutG,NewCons,ConsOut,NewInd,MaxDepth,SolDepth,RuleNos,all).
%% only unrefuted foreigns, DIFFs and POLYs left over
treat_delayed(all, GL, GL, Cons, Cons, _, Depth, Depth, []).

%% Code by Odd Hacks Inc.
next_nodeID(NodeID) :-
	( /*RB strings:*/gensym_counter('#',N) -> 
	      N1 is N+1
	; N1 = 1
	),
	concat('#',N1,NodeID).

generate_goals4output([],_,[]).
generate_goals4output([WG|GL],No,['$NL$',goal,No:'','$TRACE GOAL$'(G)|R]) :-
	wrap_goal(G,WG),
	No1 is No+1,
	generate_goals4output(GL,No1,R).

%%%%%%% check_for_foreign_goals(+GoalList, -SelGoal, -RestGoalList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% takes a list of delayed goals and looks for the first goal, which
%% is not a foreign call; this goal is returned in -SelGoal; the
%% rest of the goal list is returned in RestGoalList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_for_foreign_goals([WGoal|GoalList], WGoal1, GoalList1) :-
	wrap_goal(OGoal, WGoal),
	functor(OGoal, Name, Arity),
	( cuf_foreign(Name, Arity, _) ->
	      check_for_foreign_goals(GoalList, WGoal1, GoalList2),
	      GoalList1 = [WGoal|GoalList2]
	; WGoal1 = WGoal,
	  GoalList = GoalList1
	).



del_trigger_folder(NewGoals,Selected,RestGoals) :-
    current_predicate('CHECK TRIGGER','CHECK TRIGGER'(_)),
    del_trigger(NewGoals,Selected,RestGoals), !.
del_trigger_folder(NewGoals,Selected,RestGoals) :-
    current_predicate('FOLDER DEF','FOLDER DEF'(_,_,_,_)), 
    del_folder(NewGoals,Selected,RestGoals).

          

%%%%%%%  del_trigger(+GoalList, -TriggerGoal, -RestofGoalList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% extracts the first trigger goal from GoalList, if such a thing is
%%% present, gives also back the list of all other goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

del_trigger([WG|R],Trigger,Others) :-
   goal_flag(WG,Flag),
   (Flag == trigger(trigger) ->
	wrap_goal(G,WG),
        'CHECK TRIGGER'(G),
        is_undelayed(G),
        Trigger = WG, Others = R
   ; Others = [WG|RR],
     del_trigger(R,Trigger,RR)
   ).

%%%%%%%  del_folder(+GoalList, -FolderGoal, -RestofGoalList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% extracts the first folder goal from GoalList, if such a thing is
%%% present, gives also back the list of all other goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

del_folder(GoalList, Folder:trigger(folder(Name)), Rest) :-
    goal_flag(WG, trigger(folder(Name))),
    delete(WG, GoalList, Others),
    nonvar(Name),
    wrap_goal(G,WG),
    wrap_goal(G,WG1),
    'FOLDER DEF'(Name, Folder, [WG1|RGoals], Cond),
    del_folder_rest(RGoals, Others, Rest),
    call(Cond), !.

del_folder_rest([], Goals, Goals).
del_folder_rest([WG|R],Goals,Rest) :-
   wrap_goal(G,WG),
   wrap_goal(G,WG1),
   delete(WG1, Goals, Others),
   del_folder_rest(R, Others, Rest).


%%%%%%%  write_success_node(+RuleNoList, +GoalList)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% asserts success_node if debug_flag = yes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_success_node(RuleNoList, GoalList) :-
    nthsolution(OldSolution),
    retract(nthsolution(OldSolution)),
    ( clause(incremental_nthsolution(IOldSolution), true) ->
	  retract(incremental_nthsolution(IOldSolution)),
	  INthSolution is IOldSolution + 1,
	  assert(incremental_nthsolution(INthSolution))
    ; true
    ),
    NthSolution is OldSolution + 1,
    assert(nthsolution(NthSolution)),
    (get_flag(debug,yes) ->
	 ( resolve_port(NodeID,_,_,_, RuleID,_,_) -> true
	 ; NodeID=[], RuleID=[]
	 ),
	 gensym('#', ID),
	 assertz(search_tree_link(NodeID,RuleID,ID)),
	 ( get_flag(stuf_gui, yes) ->
	       ieSendTraceMsg(stufTraceInfo(0, [], [], ID, success, success)),!
	 ; true
	 )
    ;  true
    ),
    ( GoalList == [] -> Type = success
    ; Type = residuated_goals
    ),
    assertz(success_node(NthSolution, ID, RuleNoList,Type)).


%%%%%%%  prove_nondet(+GoalList, -OutGoals, +Indentation, +RecursionBound,
%%%%%%%               -SolutionDepth, -RuleNo) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prove_nondet(GoalList, SubGoals, ConsIn, ConsOut,
             Ind, MaxDepth, Ind1, RuleNo) :- 
  ( cuf_debugging(1), get_flag(interactive, yes) ->
      resolve_interactive(GoalList, SubGoals, ConsIn, ConsOut,
                          Ind, MaxDepth, Ind1, RuleNo) 
  ; resolve(GoalList, SubGoals, ConsIn, ConsOut,
            Ind, MaxDepth, Ind1, RuleNo)
  ).


%%%%%%%  resolve(+GoalList, +SubGoals, +Indentation, +MaxDepth,
%%%%%%%           -SolutionDepth, -RuleNumber) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve(GoalList, Subgoals, ConsIn, ConsOut,
        Ind, MaxDepth, Ind1, RuleNo) :-
    delete(WGoal, GoalList, RestGoals), 
    wrap_goal(Goal,WGoal),
    is_undelayed(Goal), !,
    resolve(WGoal, Subgoals, RestGoals, ConsIn, ConsOut,
            Ind, MaxDepth, Ind1, RuleNo, non_det).
resolve(Delayed, Delayed, Cons, Cons, Depth, _, Depth, 'NO REDUCE').

%%%%%%%  resolve(+Goal, +SubGoals, +RestGoals, +Indentation, +MaxDepth,
%%%%%%%          -NewIndentation, -RuleNo, +Type) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  +Type marks resolution-step deterministic or non-deterministic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve(Goal, Subgoals, RestGoals, ConsIn, ConsOut,
        Ind, MaxDepth, Ind1, rule_id(FID, EID, No), Type) :-
    ifdebug(debug_message1(Goal, NodeID, MaxDepth, Ind, Type)),
    wrap_goal(OnlyGoal,Goal),
    ( MaxDepth > Ind -> true
    ; functor(OnlyGoal, Func, Arity),
      assert(depth_exhausted(NodeID, Func/Arity)),
      assert(incremental_depth_exhausted(NodeID, Func/Arity)),
      ( get_flag(debug,yes) -> MsgRest = [nodeID(NodeID)]
      ; MsgRest = [] ),
      write_trace(message, ['DEPTH EXHAUSTED !!!  '|MsgRest],
		  cuf_depth_exhaustedInfo(NodeID)),
      fail
    ),
    'CUF clause'(OnlyGoal, FID, EID, No, NodeID, Subgoals, RestGoals, ConsOut, ConsIn),
    ifdebug(debug_message2(Goal, NodeID, rule_id(FID, EID, No), Ind, Type, ConsIn, ConsOut)),
    Ind1 is Ind+1.



%%%%%%%  convert_subgoals(+Number, +ParID, +SubgoalsIn, -SubgoalsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  convert SubgoalsIn into appropriate format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_subgoals(_K, _P, L, L) :-  
  var(L),!.
convert_subgoals(K, P, [H:F|T], [g(P, K, H, F)|T1]) :-
  K1 is K+1,
  convert_subgoals(K1, P, T, T1).

%%%%%% debug_message1(+Goal, -NodeID, +MaxDepth, +Ind, +Type)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks different flags and asserts fail-nodes if wanted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug_message1(WGoal, NodeID, MaxDepth, Ind, Type):-  
    ( retract(start_incremental_proof(NodeID)) -> true
    ; gensym('#', NodeID)
    ),
%	gensym('#', NodeID),
    goalID(WGoal, ParID:NthSubgoal),
    wrap_goal(Goal, WGoal),
    ( get_flag(debug, no) -> true
    ; assert_search_tree_link(NodeID),
      functor(Goal, Func, Arity),
      ( MaxDepth > Ind -> 
% 	    undo((asserta(resolve_port(NodeID, ParID, Func/Arity, 
% 				       NthSubgoal, [], Type, fail)),
% 		  asserta(resolve_port_parent_access(ParID, NodeID))))
	    ( true
	    ; asserta(resolve_port(NodeID, ParID, Func/Arity, 
				   NthSubgoal, [], Type, fail)),
	      asserta(resolve_port_parent_access(ParID, NodeID)),
	      fail
	    )
      ; asserta(resolve_port(NodeID, ParID, Func/Arity, 
			     NthSubgoal, [], Type, depth_exhausted))
      )
    ),
    write_trace_info(Type, Goal, NodeID, Ind, call).


%%%%%%% debug_message2(+Goal, +NodeID, +RuleID, +Ind, +Type, +ConsIn, +ConsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks flags and asserts exit-nodes if wanted 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug_message2(WGoal, NodeID, RuleID, Ind, Type, ConsIn, ConsOut):-
    goalID(WGoal,ParID:NthSubgoal),
    wrap_goal(Goal, WGoal),
    functor(Goal,Func, Arity),
    ( get_flag(debug, no) -> true
    ; asserta(resolve_port(NodeID, ParID, Func/Arity, 
			   NthSubgoal, RuleID, Type, exit)),
      asserta(resolve_port_parent_access(ParID, NodeID)),
      ( cuf_foreign(Func,Arity,_) ->
	    assert(foreign_result(NodeID,Goal,ConsIn,ConsOut))
      ; true
      )
    ),
    write_trace_info(Type, Goal, NodeID, Ind, exit).


%%%%% write_trace_info(+Type, +Goal, +NodeID, +Ind, +Port)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks tracing-flags and calls write_trace to give out trace messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_trace_info(Type, Goal, NodeID, Ind, Port) :-
    get_flag(tracing, Flag),
    functor(Goal, Func, Arity),
    ( Flag \== no,
      cuf_trace_filter(Type,Flag,Ind,Func,Arity,Port,Prefix) ->
          write_trace(message, [[Ind],Prefix,'$TRACE GOAL$'(Goal)],
		      stufTraceInfo(Ind,Func,Arity,NodeID,Type,Port))
    ; true ).

cuf_trace_filter(non_det,_,Ind,Func,Arity,Port,Prefix) :-
	( cuf_trace_depth(Min,Max) ->
	      Ind >= Min,
	      ( Max > 0 -> Ind =< Max ; true)
	; true),
	( cuf_trace_filter_pred(_,_) ->
	      cuf_trace_filter_pred(Func,Arity)
	; true ),
	trace_prefix_non_det(Port,Prefix).
cuf_trace_filter(det,all,Ind,Func,Arity,Port,Prefix) :-
	( cuf_trace_depth(Min,Max) ->
	      Ind >= Min,
	      ( Max > 0 -> Ind =< Max ; true)
	; true),
	( cuf_trace_filter_pred(_,_) ->
	      cuf_trace_filter_pred(Func,Arity)
	; true ),
	trace_prefix_det(Port,Prefix).

trace_prefix_non_det(call,'G:').
trace_prefix_non_det(exit,'N>').
trace_prefix_det(call,'G:').
trace_prefix_det(exit,'D>').


%%%%%  write_trace(+CufOutType,+CufOutArg, +StufTraceOutArg)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks stuf_gui-flag and sends trace-messages to appropriate output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_trace(CufOutType, CufOutArg, StufTraceOutArg):-
    ( get_flag(stuf_gui, yes) ->
	  (ieSendTraceMsg(StufTraceOutArg),!)
    ; cuf_out(CufOutType, CufOutArg)
    ).




%%%%%%%  is_undelayed(+Goal)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks if Goal is undelayed (there is no delay pattern for Goal) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_undelayed(Head) :-
    ( current_predicate(delay_pattern, delay_pattern(_,_,_)) ->
          functor(Head, Name, Arity),
          \+ delay_pattern(Name, Arity, Head)
    ; current_predicate(wait_pattern, wait_pattern(_)) ->
          \+ wait_condition(Head)
    ; true ).


%%%%%%  assert_search_tree_link(+ NodeID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  gets last resolve_port of type exit and asserts link
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_search_tree_link(NodeID) :-
    ( clause(proof_restart_link(LastNodeID, RuleID),true) ->
          retract(proof_restart_link(LastNodeID, RuleID))
    ; resolve_port(LastNodeID,_,_,_,RuleID,_,exit) ->
          true
    ; LastNodeID = [], RuleID = []
    ),!,
    assertz(search_tree_link(LastNodeID, RuleID, NodeID)).


%% prolog.pl for SICSTUS-Prolog Version 3, automatically generated, based on:



%% PL-DEP-SRC/prolog.pl: to be processed with awk -
%% configuration-script pl-dep.awk generates Prolog-implementation
%% specific code 


%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.8  1996/04/09 14:29:28  jochen
% changed handling of file_search_path facts, due to use of module cuf
%
% Revision 1.7  1995/12/08 15:33:46  junger
% for 1 3: ensure_loaded(library(terms)) added because of
% subsumes_chk/2
%
% Revision 1.6  1995/11/15  13:02:18  junger
% adapted for the integration of 1 3
%
% Revision 1.5  1995/03/24 14:47:42  jochen
% reset_gensym_counter/1 added (bug fix for 1 version)
% gensym/2 (Sicstus version) now starts counting with 1 (compat. with Q.)
%
% Revision 1.4  1994/06/16  13:59:10  jochen
% cuf_abs_file_name (quintus version) hacked; accepts full file paths now, as in 1 version
%
% Revision 1.3  1994/05/13  14:45:41  jochen
% cuf_abs_file_name was buggy, changed; now current dir. is always included in search
%
% Revision 1.2  1994/05/13  12:43:39  jochen
% hack for 1 version of cuf_absolute_file_name downward compatibly adapted to #9.
%
% Revision 1.1  1994/03/25  14:12:53  jochen
% Changes for new Quintus/SICStus handling
% now generic file for former {quintus,1}.pl
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: 1 patch file                            %%%
%%%      Created: Tue Apr  6 16:19:17 1993                      %%%
%%%     Modified: Fri Mar 25 15:12:52 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% OldLog: 1.pl,v
% Revision 1.6  1993/12/10  19:03:00  michel
% new predicates: style_check/1, no_style_check/1
%
% Revision 1.5  1993/12/10  12:37:32  michel
% dynamic declaration for cuf_grammar_directory/1 and cuf_demo_directory/1
% removed
%
% Revision 1.4  1993/12/10  12:15:50  michel
% cuf_abs_file_name/4 looks now for cuf_demo_directory/1 facts as well
% (the system default for a grammar directory)
%
% Revision 1.3  1993/12/02  09:06:23  jochen
% mktemp/2 added (currently not used, needed for gui later)
%
% Revision 1.2  1993/11/29  13:19:32  jochen
% added: environ/2
%
% Revision 1.1  1993/11/08  13:49:21  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Fri Nov  5 15:49:13 1993 (jochen) : prolog_flag(compiling,_,fastcode) added
% Thu May  6 12:57:41 1993 (michel) : concat_atom included (JD)
% Tue Apr  6 16:20:46 1993 (michel) : append/3 added

% the following predicates must be defined for using CUF:
% append/3
% member/2
% memberchk/2
% gensym/2
% cuf_abs_file_name/4
% cuf_save_program
% file_property(+AbsoluteFileName, modify_time, -Date) will be
%     last_modification_time(++AbsoluteFileName, -Date)



:- prolog_flag(compiling, _, fastcode).

%:- ensure_loaded('../src/foreign').
%:- ensure_loaded(library(charsio)).
%:- ensure_loaded(library(system)).


%%RB :- ensure_loaded(library(terms)).


file_property(AbsoluteFileName, modify_time, Date) :-
	cuf_file_mtime(AbsoluteFileName, Date).



cuf_save_program(X) :-
    save(X, N),
    initialization.
    %(N == 1 -> initialization ; N == 0).

append([], L, L).
append([F|R], L, [F|S]) :-
   append(R, L, S).

member(X, [X|_]).
member(X, [_|R]) :-
  member(X, R).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|R]) :-
  memberchk(X, R).

:- dynamic 'CONCAT STRING'/3.

gensym(Prefix, V) :-
    atomic(Prefix),
    var(V),
    ( inc_counter(Prefix, N, 1), !
    ; set_counter(Prefix, 2), N = 1
    ),
    concat(Prefix, N, V),
    !.

reset_gensym_counter(X) :-
    del_counter(X).  %for 1



concat(Prefix, No, Atom) :-
    ( 'CONCAT STRING'(Prefix, NoString, String) -> true
    ; name(Prefix, PrefixString),
      append(PrefixString, NoString, String),
      assert('CONCAT STRING'(Prefix, NoString, String))
    ), 
    name(No, NoString),
    name(Atom, String).

concat_atom(List, Atom) :-
	constants_to_chars(List, Chars, []),
	name(Atom, Chars).

constants_to_chars([]) --> [].
constants_to_chars([F|R]) -->
	{atom(F)},!,
	atom_to_chars(F),
	constants_to_chars(R).
constants_to_chars([F|R]) -->
	number_to_chars(F),
	constants_to_chars(R).




% the following predicates are showing a kind of
% straight forward hacking! :-)

%:- dynamic cuf_grammar_directory/1,
%	   cuf_demo_directory/1.

%% _Acc darf nur 'exist' oder 'read' sein

cuf_abs_file_name(RelativeFileName, AbsoluteFileName, Ext, _Acc) :-
   absolute_file_name('.',CWD),


   undo(working_directory(_,CWD)),

   on_exception(_, 
		( cuf_abs_file_name_cd(RelativeFileName),
                  cuf_absolute_file_name(RelativeFileName, Ext, AbsoluteFileName)),
		fail), !,


   working_directory(_,CWD).



cuf_abs_file_name_cd(_).           %% start searching cwd
cuf_abs_file_name_cd(RelativeFileName) :-
   name(RelativeFileName,Chars),
   member(0'/,Chars), !, fail.     %% the following only for simple
				   %% files 
cuf_abs_file_name_cd(_) :-
   ( cuf_grammar_directory(X) ; cuf_demo_directory(X)),


   working_directory(_,X).





%% hack using internal absolute_file_name/4
cuf_absolute_file_name(RelativeFileName, Ext, AbsoluteFileName) :-
   name(Ext, Suffix),
   name(Ext0, [0'.|Suffix]),
   ( predicate_property(prolog:absolute_file_name(_,_,_,_,_,_),_) ->
	 /*RB prolog:*/absolute_file_name(RelativeFileName, Ext0,
           cuf_absolute_file_name(RelativeFileName,Ext,AbsoluteFileName),
				AbsoluteFileName,
				_Dir,Flag),Flag==true % no exception
   ; predicate_property(prolog:absolute_file_name(_,_,_,_,_),_) ->
   %% sicstus2.1.9 ?
         /*RB prolog:*/absolute_file_name(RelativeFileName, Ext0,
           cuf_absolute_file_name(RelativeFileName,Ext,AbsoluteFileName),
				AbsoluteFileName,
				_Dir)
   ; /*RB prolog:*/absolute_file_name(RelativeFileName, Ext0, AbsoluteFileName,
			       _Dir)
   ),
   cuf_file_exists(AbsoluteFileName).


hash_term(T,H) :-
	term_hash(T,10,100000000,H).



% style_check/1
% no_style_check/1
style_check(all) :-
	style_check(single_var),
	style_check(multiple),
	style_check(discontiguous).
style_check(single_var) :-
	prolog_flag(single_var_warnings,_,on).
style_check(multiple) :-
	prolog_flag(redefine_warnings,_,on).
style_check(discontiguous).

no_style_check(all) :-
	no_style_check(single_var),
	no_style_check(multiple),
	no_style_check(discontiguous).
no_style_check(single_var) :-
	prolog_flag(single_var_warnings,_,off).
no_style_check(multiple) :-
	prolog_flag(redefine_warnings,_,off).
no_style_check(discontiguous).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: flattening structures of CUF expressions      %%%
%%%      Created: Fri Sep 18 13:10:17 1992                      %%%
%%%     Modified: Thu Oct  1 11:18:58 1992 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.1  1993/11/08 13:49:48  jochen
% check in cuf2.28 (initial check in)
%


%%%  flatten_struc(+Term, -FlattenedOne)
% collects 'Disjs', '&', ';' in lists instead of binary terms
flatten_struc(Struc, Flat) :-
    Struc =.. [Func|Args], !,
    flatten_struc(Func, Args, Flat), !.

flatten_struc(Atom, [], Atom) :- !.
flatten_struc('.', [Arg1, Arg2], [Flat1|Flat2]) :- !,
    flatten_struc(Arg1, Flat1),
    flatten_struc(Arg2, Flat2).
flatten_struc('V', [Arg], 'V'(Arg)) :- !.
flatten_struc('TYP', [Arg], 'TYP'(Arg)) :- !.
flatten_struc('S', [String], 'S'(String)) :- !.
flatten_struc('SORT', [Arg1, Arg2], 'SORT'(Arg1, Flat)) :- !,
    flatten_struc(Arg2, Flat).
flatten_struc('A', [Arg], 'A'(Arg)) :- !.
flatten_struc(':', [Arg1, Arg2], ':'(Arg1, Flat)) :- !,
    flatten_struc(Arg2, Flat).
flatten_struc('~', [Arg], '~'(Flat)) :- !,
    flatten_struc(Arg, Flat).
flatten_struc('&', Args, '&'(Flat)) :- !,
	flatten_struc1(Args, '&', Flat0),
	sort(Flat0, Flat).
flatten_struc(';', Args, ';'(Flat)) :- !,
	flatten_struc1(Args, ';', Flat0),
	sort(Flat0, Flat).
flatten_struc('Disjs', Args, 'Disjs'(Flat)) :- !,
	flatten_struc1(Args, 'Disjs', Flat0),
	sort(Flat0, Flat).
flatten_struc(Func, [Arg1|Arg2], Flat) :-
    flatten_struc(Arg1, FArg1),
    flatten_struc(Arg2, FArg2),
    Flat =.. [Func, FArg1|FArg2].

flatten_struc1([], _, []).
flatten_struc1([Arg|Args], Func, Flat) :-
      flatten_struc2(Arg, Func, FArg),
      append(FArg, FArgs, Flat), !,
      flatten_struc1(Args, Func, FArgs).

flatten_struc2(Arg, Func, Flat) :-
	functor(Arg, Name, _),
	( Name == '.' ->
	      flatten_struc(Arg, Flat)
	; Name == Func ->
	      Arg =.. [Func|Args],
	      flatten_struc1(Args, Func, Flat)
	; flatten_struc(Arg, FArg),
	  Flat = [FArg]
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: misc. set operations (used for formulas       %%%
%%%               represented as lists of lists of integers)    %%%
%%%      Created: Fri Sep 18 13:19:25 1992                      %%%
%%%     Modified: Thu Oct  1 11:25:29 1992 (michel)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.1  1993/11/08 13:49:52  jochen
% check in cuf2.28 (initial check in)
%


%%%  sort_by_card(+ListOfLists, -ListOfLists)
%%%  sorting by cardinality (length :-)
%%%  "quicksort" algorithm
sort_by_card([], []).
sort_by_card([Set|Sets], OrderedSets) :-
	sort_by_card1(Sets, Set, Sets1, Sets2), !,
	sort_by_card(Sets1, OrderedSets1),
	append(OrderedSets1, [Set|OrderedSets2], OrderedSets),
	sort_by_card(Sets2, OrderedSets2), !.

%%%  sort_by_card1(+ListOfLists, +List, 
%%%                -ListOfLists>List, -ListOfLists=<List)
%%%  splitting ListOfLists by length of element lists,
%%%  reference list is List
sort_by_card1([], _Set, [], []).
sort_by_card1([Set1|Sets1], Set, Sets2, Sets3) :-
	card(Flag, Set, Set1),
	( Flag == greater ->
	      Sets2 = [Set1|Sets2R],
	      sort_by_card1(Sets1, Set, Sets2R, Sets3)
	; /* ( Flag == equal ; Flag == less ) -> */
	  Sets3 = [Set1|Sets3R],
	  sort_by_card1(Sets1, Set, Sets2, Sets3R)
	).

%%%  card(-Flag, +List1, +List2)
%%%  sets flag (less, equal, greater) by comparing the length of 
%%%  the input lists
card(Flag, List1, List2) :-
	length(List1, N1),
	length(List2, N2),
	( N1 < N2 ->
	      Flag = less
	; /* N1 >= N2 -> */
	  ( N1 =:= N2 ->
		Flag = equal
	  ; /* N1 > N2 -> */
	    Flag = greater
	  )
	).

%%%  del_supersets(+ListOfLists, -ListOfLists)
%%%  deletes supersets; assuming a list stands for a set
%%%  lists contains integers which are ordered
del_supersets([], []).
del_supersets(Sets, Out) :-
	sort_by_card(Sets, OrderedSets),
	del_supersets1(OrderedSets, Out), !.

del_supersets1([], []).
del_supersets1(InSets, OutSets) :-
	part_sets_by_card(InSets, Equal, Greater),
	( Greater == [] -> OutSets = InSets
	; del_supersets2(Equal, Greater, Sets),
	  append(Equal, Out, OutSets), !,
	  del_supersets1(Sets, Out)
	).

del_supersets2([], X, X).
del_supersets2([Set|Equal], Greater, Sets) :-
	( bagof(Set2, 
		( member(Set2, Greater),
	          \+ subset(Set, Set2)
		),
		Sets0), !,
	  del_supersets2(Equal, Sets0, Sets)
	; Sets = []
	).

%%%  part_sets_by_card(+Sets, -SetsOut1, -SetsOut2)
%%%  SetsOut1 are the sets of Sets which have the smallest cardinality
%%%  SetsOut2 are the others of Sets
%%%  Sets must be ordered!	      
part_sets_by_card(Sets, Equal, Greater) :-
	memberchk(Set, Sets),
	sort_by_card2(Sets, Set, Equal, Greater).

%%%  sort_by_card2(+ListOfLists>=List, +List, 
%%%                -ListOfLists=List, -ListOfLists>List)
%%%  input list contains no list of length less than |List|
%%%  splitting input list by length
sort_by_card2([], _Set, [], []).
sort_by_card2([Set1|Sets1], Set, Sets2, Sets3) :-
	card(Flag, Set, Set1),
	( Flag == less ->
	      Sets2 = [], Sets3 = [Set1|Sets1]
	; /* Flag == equal */
	  Sets2 = [Set1|Sets2R],
	  sort_by_card2(Sets1, Set, Sets2R, Sets3)
	).

%%%  subset(+Set, +Superset)
%%%  subset/2 works only for ground lists of integers!
%%%  Set must be ordered!
subset(X, X) :- !.
subset([], _).
subset([X|SubList], [Y|List]) :-
	( X =:= Y ->
	      subset(SubList, List)
	; Y < X ->
	      subset([X|SubList], List)
    /*	; X < Y -> fail  */
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Joerg Junger                                  %%%
%%%      Purpose: STUF-interface predicates                     %%%
%%%      Created: Wed May 11 14:25:00 1994                      %%%
%%%     Modified: Tue Mar 19 15:25:36 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.17  1996/04/09 14:08:17  jochen
% file IDs no longer numbers; =:= -> ==
%
% Revision 1.16  1995/01/26 07:54:10  jochen
% bug in pp_sld_tree/build_sld_tree, which led to abort, removed;
% pp_sld_tree now prints ports in uppercase, and special events
% (success, depth exhausted, proof interrupted) with ***
%
% Revision 1.15  1994/12/15  17:13:01  michel
% bug fix: ruleID_to_clauseNo/2 changed for unknown EID in index tables
% (HACK!)
%
% Revision 1.14  1994/07/22  14:05:26  junger
% goal_lsit/4 and goal_list_internal/4 can now handle success node IDs
%
% Revision 1.13  1994/07/21  14:29:48  junger
% pp_sld_tree changed: some bus fixed and now nodes which introduced
% choice points are displayed as often as there has been choices; clause
% numbers are now printed behind the node where the clause has been
% called
%
% Revision 1.12  1994/07/20  15:28:53  junger
% build_sld_tree_new added for handling the different success nodes.
% pp_sld_tree changed: rule IDs are converted to clause numbers; adapted
% to use with build_sld_tree_new.
% resolve_step changed so that it can handle foreign calls.
%
% Revision 1.11  1994/07/15  11:50:33  jochen
% pp_sld_tree now prints clause numbers (relative to predicate) instead
% of 'rule_id's.
%
% Revision 1.10  1994/07/08  13:05:58  junger
% - now pp_sld_tree and pp_logdep_tree display external arity of goals
%
% Revision 1.9  1994/07/01  15:01:21  junger
% - incremental proofs start now with the supplied node ID, resolve_port
% facts for fails caused by non determinism will be deleted, if an
% incremental proof is made
%
% Revision 1.8  1994/06/28  11:13:38  junger
% - build_sld_tree/2 now possible for interrupted proofs
%
% Revision 1.7  1994/06/27  11:59:02  junger
% - goal_list_internal/5 and result_internal/3 added
%
% Revision 1.6  1994/06/21  15:12:35  jochen
% rename: cuf_goal --> cuf_top_level_goal (was in conflict with new adt)
%
% Revision 1.5  1994/05/18  15:20:41  junger
% - content_of_result/3 added
%


:- dynamic cuf_top_level_goal/4.

%%%%% goal_list(+NodeID, -GoalList, -ConstraintList, -SelectedGoalID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface-predicate for the gui-jobcall; 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goal_list(NodeID, PrepGL, PrepCL, Selected) :-
  clause(cuf_top_level_goal(StartNodeID, GoalsIn, ConsIn,_),true),
  goal_list(StartNodeID, NodeID, GoalsIn, ConsIn, GoalList,
	    Constraints),
  ( resolve_port(NodeID,ParID,_,SubGNo,_,_,_) ->
	Selected = ParID:SubGNo
  ; success_node(_,NodeID,_,_),!,
    Selected = none
  ),
  prepare_printing_goals(GoalList, PrepGL),
  prepare_pp_cl(Constraints, PrepCL),!.

%%%%% goal_list_internal(+NodeID, -GoalList, -ConstraintList, -SelectedGoalID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% takes a NodeID and delivers the goallist and the constraints at this node
%% plus the goal ID of the selected goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal_list_internal(NodeID, GoalList, Constraints, Selected) :-
  clause(cuf_top_level_goal(StartNodeID, GoalsIn, ConsIn,_),true),
  goal_list(StartNodeID, NodeID, GoalsIn, ConsIn, GoalList,
	    Constraints),
  ( resolve_port(NodeID,ParID,_,SubGNo,_,_,_) ->
	Selected = ParID:SubGNo
  ; success_node(_,NodeID,_,_),!,
    Selected = none
  ).

%%%%%  goal_list(+StartNodeID, +StopNodeID, +GoalListIn, +ConsIn, -GoalListOut, -ConsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds a list of goals starting at Node with ID StartNodeID ending at node StopNodeID
%%%  by first collecting the path between the two NodeIDs and then doing all resolution-steps
%%%  along this path. StartNodeID identifies a node which is a parent to the node 
%%%  identified by StopNodeID. SelectedGoalID identifies the Goal selected at StopNodeID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goal_list(StartNodeID, StopNodeID, GoalListIn, ConsIn, GoalListOut, ConsOut) :-
  collect_path(StartNodeID, StopNodeID, Path),
  resolve_step(Path, GoalListIn, ConsIn, GoalListOut, ConsOut).


%%%%% content_of_result(+StopNodeID, -PreparedResult, - PreparedConstraints)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gives the prepared Result of proof ending at Node with ID StopNodeID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


content_of_result(StopNodeID, PrepResult, PrepConstraints) :-
	cuf_top_level_goal(StartNodeID, GoalsIn, ConsIn, Result),
	goal_list(StartNodeID, StopNodeID, GoalsIn, ConsIn, _, ConsOut),
	prepare_printing(Result, ConsOut, PrepResult, PrepConstraints).
	

%%%%%  collect_path(+StartNodeID, +StopNodeID, -PathOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds the path consisting of a list of Goal/RuleIDs beginning an StartNodeID, 
%%%  ending at StopNodeID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_path(StartNodeID, EndNodeID, Path) :-
  search_tree_link(_,_,StartNodeID),
  collect_path1(StartNodeID, EndNodeID, Path).

collect_path1(NodeID, NodeID, []).
collect_path1(NodeID, EndNodeID, [NodeID:[ParID:NthSubgoal,RuleID,FA]|Path]) :-
  search_tree_link(NodeID, RuleID, NewNodeID),
  resolve_port(NodeID,ParID,FA,NthSubgoal,RuleID,_,_),
  collect_path1(NewNodeID, EndNodeID, Path).


%%%%%  resolve_step(+Path, +GoalsIn, +ConsIn, -GoalsOut, -ConsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  does all the resolutions along path, yielding the Goal-List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_step([], Goals, Cons, Goals, Cons) :-!.
resolve_step([NodeID:[GoalID,rule_id(FID,EID,RNo),_]|Path], GoalsIn, ConsIn, GoalsOut, ConsOut) :-
  goalID(WGoal,GoalID),
  delete_goal(WGoal, GoalsIn, OutGoals, SubGoals, SharedVar),
  wrap_goal(SelectedGoal,WGoal),
  functor(SelectedGoal, Func, Arity),
  ( cuf_foreign(Func,Arity,_) ->
	foreign_result(NodeID,SelectedGoal,ConsIn,OutCons),
	SubGoals = SharedVar
  ;
  'CUF clause'(SelectedGoal, FID, EID, RNo, NodeID, SubGoals, SharedVar, OutCons, ConsIn)
  ),
  check_constraints(OutCons, CheckedOutCons),
  resolve_step(Path, OutGoals,CheckedOutCons, GoalsOut, ConsOut).

delete_goal(Goal, [Goal|Goals], Subgoals, Subgoals, Goals) :-!.
delete_goal(Goal, [DifGoal|Goals], [DifGoal|OutGoals], Subgoals, SharedVar) :-
  delete_goal(Goal, Goals, OutGoals, Subgoals, SharedVar).


%%%%%  content_of_goalarg_at(+GoalID, +ExtractNodeID, +StopNodeID, +ArgNo, -Arg, -Constraints)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  interface-predicate for gui-jobcall
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
content_of_goalarg_at(GoalID, ExtractNodeID, StopNodeID, ArgNo, PrepArg, PrepConstraints) :-
  clause(cuf_top_level_goal('#1', InitialGoals, InitialConstraints,_),true),
  content_of_goalarg_at(GoalID, ExtractNodeID, StopNodeID, ArgNo, InitialGoals, InitialConstraints, Cuf_Arg, Cuf_Constraints),
  prepare_printing(Cuf_Arg, Cuf_Constraints, PrepArg, PrepConstraints).





%%%%%  content_of_goalarg_at(+GoalID,+ExtractNodeID, +StopNodeID, +ArgNo, +GoalsIn, +ConsIn, -Arg, -Constraints)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  shows content of ArgNoth argument Arg of Goal identified by GoalID (=ParID:NthSubgoal) 
%%%  at Node ExtractNodeID after doing resolutions up to StopNodeID. ExtractNodeID must be parent to StopNodeID.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
content_of_goalarg_at(GoalID, ExtractNodeID, StopNodeID, ArgNo, GoalsIn, ConsIn, Arg, Constraints) :-
  goal_list('#1', ExtractNodeID, GoalsIn, ConsIn, GoalsOut, ConsOut),
  goalID(WGoal,GoalID),
  member(WGoal, GoalsOut),
  wrap_goal(Goal,WGoal),
  goal_list(ExtractNodeID, StopNodeID, GoalsOut, ConsOut, _OutGoals, Constraints),
  arg(ArgNo, Goal, Arg).


%%%%%%% result_internal(+SucessNodeID, -ResultArg, -ConsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% takes the ID of a node and delivers the result and the constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

result_internal(SN,Result, ConsOut) :-
	cuf_top_level_goal(StartNodeID, GoalsIn, ConsIn, Result),
	goal_list(StartNodeID, SN, GoalsIn, ConsIn, _, ConsOut).


%%%%%  build_sld_tree(+NodeID, -Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds SLD-tree Tree with root NodeID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_sld_tree(NodeID, [node(NodeID,Func/Arity, Type)|TreeList]) :-
  resolve_port(NodeID,_,Func/Arity,_,_,Type,_),
  findall(RuleID:NewNodeID,
	  search_tree_link(NodeID, RuleID, NewNodeID),
	  Bag),
  Bag\==[], !,
  build_sld_tree_dtrs(Bag,TreeList).
build_sld_tree(NodeID,success_node(NodeID)):-
  success_node(_,NodeID,_,_),!.
build_sld_tree(NodeID,depth_exhausted_node(NodeID,Func/Arity, Type)) :-
  resolve_port(NodeID,_,Func/Arity,_,_,Type,depth_exhausted), !.
build_sld_tree(NodeID,fail_node(NodeID,Func/Arity, Type)) :-
  resolve_port(NodeID,_,Func/Arity,_,_,Type,fail),
  \+ resolve_port(NodeID,_,_,_,_,_,exit), !.
build_sld_tree(active(ANo),store_node(ANo,active)) :-!.
build_sld_tree(passive(PNo),store_node(PNo,passive)) :-!.
build_sld_tree(NodeID,proof_interrupted_node(NodeID,Func/Arity, Type)) :-
  resolve_port(NodeID,_,Func/Arity,_,_,Type,_),!.
 

build_sld_tree_dtrs([RuleID:NodeID|Rest], [RuleID:SubTree|Trees]) :-
  ( build_sld_tree(NodeID,SubTree) -> true
  ; SubTree = proof_interrupted_node(NodeID,[]/1,[])
  ),
  build_sld_tree_dtrs(Rest,Trees).
build_sld_tree_dtrs([],[]).

%%%%%  build_sld_tree_new(+NodeID, -Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds SLD-tree Tree with root NodeID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_sld_tree_new(NodeID, [node(NodeID,Func/Arity,ParID:NthSubGoal, Type)|TreeList]) :-
  resolve_port(NodeID,ParID,Func/Arity,NthSubGoal,_,Type,_),
  findall(RuleID:NewNodeID,
	  search_tree_link(NodeID, RuleID, NewNodeID),
	  Bag),
  Bag\==[], !,
  build_sld_tree_new_dtrs(Bag,TreeList).
build_sld_tree_new(NodeID,success_node(Type,NodeID)):-
  success_node(_,NodeID,_,Type),!.
build_sld_tree_new(NodeID,depth_exhausted_node(NodeID,Func/Arity, ParID:NthSubGoal, Type)) :-
  resolve_port(NodeID,ParID,Func/Arity,NthSubGoal,_,Type,depth_exhausted), !.
build_sld_tree_new(NodeID,fail_node(NodeID,Func/Arity, ParID:NthSubGoal, Type)) :-
  resolve_port(NodeID,ParID,Func/Arity,NthSubGoal,_,Type,fail),
  \+ resolve_port(NodeID,_,_,_,_,_,exit), !.
build_sld_tree_new(active(ANo),store_node(ANo,active)) :-!.
build_sld_tree_new(passive(PNo),store_node(PNo,passive)) :-!.
build_sld_tree_new(NodeID,proof_interrupted_node(NodeID,Func/Arity, ParID:NthSubGoal, Type)) :-
  resolve_port(NodeID,ParID,Func/Arity,NthSubGoal,_,Type,_),!.
 

build_sld_tree_new_dtrs([RuleID:NodeID|Rest], [RuleID:SubTree|Trees]) :-
  ( build_sld_tree_new(NodeID,SubTree) -> true
  ; SubTree = proof_interrupted_node(NodeID,[]/1,[]:[],[])
  ),
  build_sld_tree_new_dtrs(Rest,Trees).
build_sld_tree_new_dtrs([],[]).


%%%%%  pp_sld_tree(+Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  pretty-printer for SLD-Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pp_sld_tree(Tree) :-
	pp_sld_tree_(Tree,0).
pp_sld_tree_([Node|Dtrs], Ind) :- !,
	pp_sld_tree_dtrs(Dtrs,Ind,Node).
pp_sld_tree_(Leaf,Ind) :-
	pp_sld_tree_leaf(Leaf,Ind),
	nl.

pp_sld_tree_leaf(depth_exhausted_node(NID,F/Ar,GID,Ty),Ind) :-
	Ar1 is Ar-1,
	write([Ind]),
	write('   nodeID:'),
	write(NID),
	write('   '),
	write([Ty]),
	write('   '),
	write(F/Ar1),
	write('   goalID:'),
	writeq(GID), 
        write('  *** DEPTH EXHAUSTED ***').
pp_sld_tree_leaf(fail_node(NID,F/Ar,GID,Ty),Ind) :-
	Ar1 is Ar-1,
        write([Ind]),
	write('   nodeID:'),
	writeq(NID),
	write('   '),
	write([Ty]),
	write('   '),
	write(F/Ar1),
	write('   goalID:'),
	writeq(GID),
        write('  FAIL').
pp_sld_tree_leaf(success_node(Type, NID),Ind) :-
	write([Ind]),
        write('   nodeID:'),
	writeq(NID),
	write('   *** SUCCESS ***'),
	(Type=residuated_goals -> write(' (residuated goals)') ; true).
pp_sld_tree_leaf(store_node(ItemID,Type),Ind) :-
	write([Ind]),
	write('   '),
	write(('STORE' --> Type:ItemID)).
pp_sld_tree_leaf(proof_interrupted_node(NID,[]/1,_,_),Ind) :-
	!,
	write([Ind]),
	write('   nodeID:'),
	writeq(NID),
	write('   *** PROOF INTERRUPTED ***').
pp_sld_tree_leaf(proof_interrupted_node(NID,F/Ar,GID,Ty),Ind) :-
	Ar1 is Ar-1,
	write([Ind]),
	write('   nodeID:'),
	writeq(NID),
	write('   '),
	write([Ty]),
	write('   '),
	write(F/Ar1),
	write('   goalID:'),
	writeq(GID),
        write('  *** PROOF INTERRUPTED ***').

pp_sld_tree_node(node(NID,F/Ar,GID,Ty),Ind,RuleID) :-
	Ar1 is Ar-1,
        write([Ind]),
	write('   nodeID:'),
	writeq(NID),
	write('   '),
	write([Ty]),
	write('   '),
	write(F/Ar1),
	write('   goalID:'),
	writeq(GID),
        write('  EXIT'),
	ruleID_to_clauseNo(F/Ar,RuleID,ClauseNo),
	write('   clause:'),
	write(ClauseNo).

pp_sld_tree_dtrs([],_,_).
pp_sld_tree_dtrs([RuleID:Tree|R],Ind,Node) :- !,
	pp_sld_tree_node(Node,Ind,RuleID),
	nl,
	Ind1 is Ind+1,
	pp_sld_tree_(Tree,Ind1),
	pp_sld_tree_dtrs(R,Ind,Node).

 
ruleID_to_clauseNo(Func/Ar,rule_id(FID, EID, No),ClauseNo) :-
	functor(Goal, Func, Ar),  %% internal arity
	( ground(rule_id(FID, EID, No)) ->
	    reset_counter,	
	    ( cuf_foreign(Func,Ar,FID1) ->
	         No1 = 1,
	         EID1 = EID
	    ; 'CUF clause'(Goal, FID1, EID1, No1, _, _, _, _, _)
            ),
	    incr_counter(ClauseNo),
	    FID == FID1,
	    EID =:= EID1,
	    No =:= No1
	; /* bug fixing (hack) for tabled goals */
	  No == 0 -> % tabled
	      ClauseNo = ?
	), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%  hier fangen die cuf_praedikate fuer das gui an
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%% reset_goal(+Goal, -NodeID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  converts and asserts Goal, so that access via NodeID for complete_proof
%%%  is possible; also initialize database for a new proof
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reset_goal(Goal, '#1') :-
   cuf_extern2intern(Goal, IntGoal, Cons, Result),
   set_initial_goalIDs(IntGoal),
   retractall(cuf_top_level_goal('#1',_,_,_)),
   assert(cuf_top_level_goal('#1',IntGoal,Cons,Result)).


test_reset_goal(Goal, '#1') :-
    retractall(cuf_top_level_goal('#1',[g([],1,_,_)],_)),
    assert(cuf_top_level_goal('#1',[g([],1,Goal,_)],[])).

%%%%%  complete_proof(+NodeID, -Status)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  calling of cuf-prove-procedure; reports success or failure in Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_proof(NodeID, Status) :-
  get_flag(depth_bound, MaxDepth),
  complete_proof(NodeID, all, MaxDepth, Status).

%%%%%  complete_proof(+NodeID, +Mode, -Status)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  calling of cuf-prove-procedure; reports success or failure in Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_proof(NodeID, Mode, Status) :-
  get_flag(depth_bound, MaxDepth),
  complete_proof(NodeID, Mode, MaxDepth, Status).

%%%% complete_proof(+NodeID, +Mode, +MaxDepth, -Status)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  calling of cuf-proof-procedure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_proof(NodeID, Mode, MaxDepth, Status) :-
   get_goals(NodeID, GoalList, Cons),
   ( cuf_prove(Mode,GoalList,MaxDepth,GoalsOut,Cons,_ConsOut,RuleNoList),
     write_success_node(RuleNoList,GoalsOut),
     fail
   ; check_status(Status)
   ).


%%%%% check_status(-Status)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  checks the cuf-db for obtaining the status of the actuall proof
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_status(N:Depth_Check) :- 
  clause(incremental_nthsolution(N),true),
  depth_check(Depth_Check).          


depth_check(depth_exhausted) :-
  clause(incremental_depth_exhausted(_,_),true), !.
depth_check(not_depth_exhausted).


%%%%% get_goals(+NodeID, -GoalList, ConsOut)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  builds goallist up to goal with id NodeID; deletes relevant facts 
%%%  concerning the subtree starting at goal-id from the cuf-db.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_goals('#1', GoalList, Constraints) :- !,
    ( get_flag(debug,yes) -> debug_init
    ; true
    ),
    proof_init,
    clause(cuf_top_level_goal('#1', GoalList, Constraints,_),true).


get_goals(NodeID, GoalList, ConsOut) :-
    get_flag(debug, yes),
    build_sld_tree(NodeID, Tree),
    assert(start_incremental_proof(NodeID)),
    retract_facts(Tree),
    clause(cuf_top_level_goal('#1', IniGoalList, Constraints,_),true),
    goal_list('#1', NodeID, IniGoalList, Constraints, GoalList, ConsOut),
    retract(search_tree_link(ParNode, RuleID ,NodeID)),
    assert(proof_restart_link(ParNode, RuleID)),
    retractall(incremental_depth_exhausted(_,_)),
    retractall(incremental_nthsolution(_)),
    assert(incremental_nthsolution(0)).



%%%%% retract_facts(+Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  retracts all facts from the cuf-db concerning the nodes of Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

retract_facts([node(NodeID,_,_)|Tree]) :-
  retractall(search_tree_link(NodeID,_,_)),
  retractall(resolve_port(NodeID,_,_,_,_,_,_)),!,
  retract_facts_for_dtrs(Tree).

retract_facts(success_node(NodeID)) :-
  retractall(success_node(_,NodeID,_,_)),!.

retract_facts(fail_node(NodeID,_,_)) :-
  retractall(resolve_port(NodeID,_,_,_,_,_,_)),!.

retract_facts(depth_exhausted_node(NodeID,_,_)) :-
  retractall(depth_exhausted(NodeID,_)),
  retractall(resolve_port(NodeID,_,_,_,_,_,_)),!.

retract_facts_for_dtrs([]).
retract_facts_for_dtrs([_RuleID:Tree|Rest]) :-
  retract_facts(Tree),
  retract_facts_for_dtrs(Rest).



%%%%% ancestor_list(+GID, -List)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gives the list of all logical ancestors for the
%%% goal with ID GID.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ancestor_list(ParID:NthSubGoal,[ParID:(NthSubGoal:FA)|List]) :- 
        resolve_port(_,ParID,FA,NthSubGoal,_,_,_),
        ancestor_list1(ParID, List),!.

ancestor_list1([], []).
ancestor_list1(NID, [ParID:(NthSubGoal:FA)|List]) :-
        resolve_port(NID,ParID,FA,NthSubGoal,_,_,_),
        ancestor_list1(ParID,List).


%%%%%  build_logdep_tree(+NodeID, -Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% builds for success nodes with NODE-ID NodeID
%%% a tree which shows the logical dependencies
%%% of the goals used in the proof.
%%% Tree structure:
%%% a tree has the form 
%%% - leaf(NodeID:(GoalID):Func/Arity) or
%%% - [node(NodeID:(GoalID):Func/Arity)|SubTree]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_logdep_tree(NodeID, Tree) :-
        success_node(_,NodeID,_,_),
        collect_path('#1',NodeID,Path),
        member('#1':[[]:1,_,FA],Path),
        build_tree('#1',[]:1,FA,Path,Tree),!.
build_tree(NodeID,ParID:No,FA,Path,[node(NodeID:((ParID:No):FA))|Tree]) :-
        setof(n(NID,SNo,FA1),RID^member(NID:[NodeID:SNo,RID,FA1],Path),List),!,
        build_dtrs(List,NodeID,Path,Tree).
build_tree(NodeID,ParID:No,FA,_Path,leaf(NodeID:((ParID:No):FA))).

build_dtrs([],_,_,[]).
build_dtrs([n(NID,No,FA)|Rest],ParID,Path,[SubTree|SubTrees]) :-
        build_tree(NID,ParID:No,FA,Path,SubTree),
        build_dtrs(Rest,ParID,Path,SubTrees).


%%%%%  pp_logdep_tree(+Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pretty printer for logical dependency trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pp_logdep_tree([node(Term)|Tree]) :-
        write_node(Term),
        pp_logdep_tree_dtrs(Tree,1).
pp_logdep_tree(leaf(Term)) :-
        write_node(Term).


pp_logdep_tree_dtrs([],_).
pp_logdep_tree_dtrs([[node(Term)|SubTree]|Rest],Ind) :-
        tab(Ind),
        write_node(Term),
        Ind1 is Ind+1,
        pp_logdep_tree_dtrs(SubTree,Ind1),
        pp_logdep_tree_dtrs(Rest,Ind).
pp_logdep_tree_dtrs([leaf(Term)|Rest],Ind) :-
        tab(Ind),
        write_node(Term),
        pp_logdep_tree_dtrs(Rest,Ind).

write_node(NodeID:((GoalID):Func/Arity)) :-
	Ar is Arity-1,
	write('nodeID:'),
        writeq(NodeID),
	write('   goalID:'),
	writeq(GoalID),
	write('   '),
	write(Func/Ar), nl.

%%%%% known_nodeID(+NodeID)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checks whether a given NodeID has been introduced
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


known_nodeID(NodeID) :-
	 ( success_node(_,NodeID,_,_) ; resolve_port(NodeID,_,_,_,_,_,_)),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: Interface between Earley ded. and CUF SLD prover%
%%%      Created: Fri Aug  6 13:26:28 1993                      %%%
%%%     Modified: Thu Feb  3 11:48:27 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1995/12/05 14:34:18  junger
% - definition of memq/2 removed because of clash with definition in
% utils.pl
%
% Revision 1.3  1994/06/22  12:00:46  junger
% - cuf_goal_functor/3 deleted (now in cuf_adt.pl)
%
% Revision 1.2  1994/02/03  11:02:43  junger
%  - Adaptions to new goal format
%  - moved run_earley/1 (and adapted) from j_lemma.pl to here
%  - using proof_restart_link (for debug) in completer calls of
%    cuf_expand_body and in cuf_resolve_goal (called by predictor)
%  - using cuf_prove/6 instead of prove in cuf_expand_body
%  - new predicate cuf_debug_store_earley/1
%  - moved print_active/3, print_passive/3, conv_bundle/2 from
%    j_lemma.pl to here
%
% Revision 1.1  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%


%%% RCS-INFO: 
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1995/12/05 14:34:18  junger
% - definition of memq/2 removed because of clash with definition in
% utils.pl
%
% Revision 1.3  1994/06/22  12:00:46  junger
% - cuf_goal_functor/3 deleted (now in cuf_adt.pl)
%
% Revision 1.2  1994/02/03  11:02:43  junger
%  - Adaptions to new goal format
%  - moved run_earley/1 (and adapted) from j_lemma.pl to here
%  - using proof_restart_link (for debug) in completer calls of
%    cuf_expand_body and in cuf_resolve_goal (called by predictor)
%  - using cuf_prove/6 instead of prove in cuf_expand_body
%  - new predicate cuf_debug_store_earley/1
%  - moved print_active/3, print_passive/3, conv_bundle/2 from
%    j_lemma.pl to here
%
% Revision 1.1  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%
% Revision 1.1  1993/09/16  08:56:39  jochen
% initial check-in (V2.27)
%


%% test predicate
run_earley(SentenceNo) :-
    clear_earley,

    ( get_flag(debug,yes) -> debug_init
    ; true
    ),
    proof_init,
    cuf_extern2intern(test(SentenceNo),[Intern],_ConsIn,Structure),
    goalID(Intern,[]:1),

    statistics(runtime,_),
    prove(Intern, GNo),
    statistics(runtime,[_,T]),
    (goal_passive(GNo,_PNo,Intern,_Body,_RuleNos) -> pp_fs(Structure) ; true),
    cuf_out(message, ['used time (msec):',T]).



cuf_expand_body(predict, Body1, Body2, RuleNos) :-
	cuf_expand_body1(Body1, Body2, RuleNos).
cuf_expand_body(complete(ANo, PNo), Body1, Body2, RuleNos) :-
	( get_flag(debug,yes) ->
	      assert(proof_restart_link([], complete(ANo, PNo)))
	; true
	),
	cuf_expand_body1(Body1, Body2, RuleNos).

cuf_expand_body1(Goals+ConsIn, Subgoals+ConsOut, RuleNos) :-	   
%	get_flag(depth_bound, Max),
%	prove(Goals, Subgoals, ConsIn, ConsOut, 0, Max, _,
	cuf_prove(all, Goals, Subgoals, ConsIn, ConsOut, RuleNos).


%%%%%%% cuf_debug_store_earley(+EarleyAction)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% marks last NodeID (of SLD tree) as 'store'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_debug_store_earley(Action) :-
	assert_search_tree_link(Action).




%%%%%%% cuf_combine_bodies(+Body1, +Body2, -Body)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_combine_bodies(Goals1+Cons1, Goals2+Cons2, Goals+Cons) :-
	append(Goals1,Goals2,Goals),
	append(Cons1,Cons2,Cons).


%%%%%%% cuf_select(+Body, -SelectedGoal, -RestBody)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Takes Body apart into Goals and Constraints and returns first goal
%%% as SelectedGoal and everything else as RestBody. Fails if Goals
%%% are empty.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_select([Sel|RGoals]+Cons, Sel, RGoals+Cons).


%%%%%%% cuf_unfold_folders(+Goal, -Selected, -RBody) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The single goal Goal is tested of being a def. folder, in which
%%% case it is unfolded into selected literal Selected and rest RBody. 
%%% Otherwise Selected=Goal and RBody=[]+[].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_unfold_folders('GOAL BUNDLE'(Name,Vars), Selected, Rest+[]) :-
   'FOLDER DEF'(Name, 'GOAL BUNDLE'(Name,Vars), [Selected|Rest], _). 
cuf_unfold_folders(Goal, Goal, []+[]).




%%%%%%% cuf_resolve_goal(+Goal, -NewBody, +RBody, -RuleNo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Goal is matched against the DB giving back the new subgoal and
%%% constraints lists NewBody onto which those of RBody have been
%%% appended. RuleNo is the usual identifier of the rule applied.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_resolve_goal(Goal, LinkInfo, Subgoals+ConsOut, RestGoals+ConsIn, RuleNo) :-
	( get_flag(debug,yes) ->
	      assert(proof_restart_link([], LinkInfo))
	; true
	),	      
	resolve(Goal, Subgoals, RestGoals, ConsIn, ConsOut,
		0, 1, _, RuleNo,non_det).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   unification of goal lists
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



cuf_unify_body([],[]).
cuf_unify_body([G1|R1],[G2|R2]) :-
	wrap_goal(UnwrappedG1,G1),
	wrap_goal(UnwrappedG2,G2),
	cuf_unify_goal(UnwrappedG1,UnwrappedG2),
	cuf_unify_body(R1,R2).


% cuf_unify_goal([G1|R1],GL2) :- !,
% 	cuf_unify_goallist([G1|R1],GL2).
cuf_unify_goal(WG1,WG2) :-
	wrap_goal(G1,WG1),
	wrap_goal(G2,WG2),
	G1 =.. [F|Args1],
	G2 =.. [F|Args2],
	cuf_unify_list(Args1,Args2).

cuf_unify_list([],[]).
cuf_unify_list([A1|R1],[A2|R2]) :-
	unify(A1,A2),
	cuf_unify_list(R1,R2).



cuf_subsumes_head(General, Specific) :-
	\+ \+ ( numbervars(Specific,0,_),
                cuf_unify_goal(General, Specific) ).

cuf_subsumes_body(GenG+GenC, SpecG+SpecC) :-
	\+ \+ ( numbervars(SpecG,0,_),
                cuf_unify_body(GenG, SpecG) ),
	cuf_subsumes_cons(GenC, SpecC).

cuf_subsumes_cons([], _).
cuf_subsumes_cons([Cons|RCons], SpecC) :-
	memq(Cons, SpecC),
	cuf_subsumes_cons(RCons, SpecC).


%memq(E, [E1|_]) :- E==E1, !.
%memq(E, [_|R]) :- memq(E,R).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% statistics and printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_active([WGoal0|RBody]+Cons,WHead0,No) :-
   wrap_goal(Head0,WHead0),
   conv_bundle(Head0,Head),
   functor(Head,Sort,Ar),
   Arity is Ar-1,
   cuf_out(message,['Active',No,(Sort/Arity):'','$NL$']),
   wrap_goal(F0,WGoal0),
   goal_flag(WGoal0,Flag),
   conv_bundle(F0,F),
   wrap_goal(F,WGoal),
   goal_flag(WGoal,Flag),
   Body = [WGoal|RBody],
  
   prepare_printing_clause('CUF clause'(Head,Body,Cons),
			   PrepHead, PrepBody, PrepCons), !,
   prefix(Prefix),
   print_head(PrepHead, Prefix, 1, I1),
   print_body(PrepBody, Body, Prefix, I1, _),
   print_const(PrepCons, Prefix), !.

print_passive([]+Cons,WHead0,No) :-
   wrap_goal(Head0,WHead0),
   conv_bundle(Head0,Head),
   functor(Head,Sort,Ar),
   Arity is Ar-1,
   cuf_out(message,['Passive',No,of,(Sort/Arity):'','$NL$']),
   prepare_printing_clause('CUF clause'(Head,[],Cons),
			   PrepHead, [], PrepCons), !,
   prefix(Prefix),
   print_head(PrepHead, Prefix, 1, _I1),
   print_const(PrepCons, Prefix), !.


conv_bundle('GOAL BUNDLE'(Name,Vars),Res) :- !,
   Res =.. [Name|Vars].
conv_bundle(X, X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: Implementation of Generalized Earley ded.     %%%
%%%      Created: Fri Aug  6 18:22:39 1993                      %%%
%%%     Modified: Wed Feb  2 13:01:52 1994 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.2  1994/02/03 11:53:14  jochen
%  - run_earley, print_{active,passive}, conv_bundle now in earley_if.pl
%  - cuf_expand_body now has argument for generation of debug info (search_tree_link)
%  - cuf_debug_store_earley notifies cuf of storing (for debug)
%
% Revision 1.1  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%


%%% RCS-INFO: 
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.2  1994/02/03 11:53:14  jochen
%  - run_earley, print_{active,passive}, conv_bundle now in earley_if.pl
%  - cuf_expand_body now has argument for generation of debug info (search_tree_link)
%  - cuf_debug_store_earley notifies cuf of storing (for debug)
%
% Revision 1.1  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%
% Revision 1.1  1993/09/16  08:57:53  jochen
% initial check-in (V2.27)
% earley_action facts record the sequence of addition of pass. or act. edges
%


% use a pointer scheme in the chart in the fashion of Johnson's
% assert-less implementation of Earley ded. (see ~mark/ed2/).
% 1) Each goal encountered is stored in goal(Goal,GNumber)
% 2) Passive items are stored with the GNumber of the goal for which
%    they have deen derived:
%    goal_passive(GNumber,PNumber,Fact,Body,RuleNos) 
%    Body will contain normally no literals, but may contain
%    constraints. RuleNos is a trace of nondeterministic choices used
%    in the last SLD-resolution (which led to Body).
% 3) Active items have an ANumber and are stored in
%    active(ANumber,Selected,RBody,Head,GNumber,RuleNos). Here,
%    GNumber is the number of the goal corresponding to Head.
% 4) Goals know which active items may need them to complete; stored
%    in goal_active(GNumber,ANumber).
% Subsumption check is done only for new goals and new passives, not
% for actives.


:- dynamic goal/2, goal_active/2, goal_passive/5, active/6, earley_action/1.
:- dynamic restriction/1.



clear_earley :-
    retractall(earley_action(_)),
    retractall(goal(_,_)),
    retractall(goal_passive(_,_,_,_,_)),
    retractall(goal_active(_,_)),
    retractall(active(_,_,_,_,_,_)),
    (current_predicate(restrict,restrict(_,_)) -> assert(restriction(yes)) 
    ; true
    ).



prove(Goal, GNo) :-
	predict(Goal, [], GNo).


predict(InGoal, Parent_No, OldG_No) :-
	( restriction(yes) ->
	      restrict(InGoal, Goal)
	; Goal = InGoal
	),
	predict_(Goal, Parent_No, OldG_No).
	
predict_(Goal, Parent_No, OldG_No) :-
	subsumed_goal(Goal, OldG_No), !,
	assert(goal_active(OldG_No, Parent_No)),
	a_complete(OldG_No, Parent_No).
predict_(Goal, Parent_No, No1) :-
	last_goal_number(No),
	No1 is No+1,
	asserta(goal(Goal,No1)),
	assert(goal_active(No1,Parent_No)),
	cuf_unfold_folders(Goal, Selected, RBody),
	( cuf_resolve_goal(Selected, predict(No1), Body1, RBody, RuleNo),
	  cuf_expand_body(predict, Body1, Body2, RuleNos),
	  store(Body2, Goal, No1, [RuleNo|RuleNos]),
	  fail
	; true
	).



p_complete(No,PNo, PHead,PBody) :-
	goal_active(No, ANo),
	active(ANo, Selected, BodyR, AHead, ParGoalNo, _), %% is unique
	cuf_unify_goal(PHead, Selected),
	cuf_combine_bodies(PBody, BodyR, BodyR1),
	cuf_expand_body(complete(ANo,PNo), BodyR1, BodyR2, RuleNos),
	store(BodyR2, AHead, ParGoalNo, RuleNos),
	fail.
p_complete(_,_,_).


a_complete(GoalNo, ANo) :-
        active(ANo, Selected, BodyR, AHead, ParGoalNo, _), %% is unique
	goal_passive(GoalNo,PNo,PHead,PBody,_RuleNos),
	cuf_unify_goal(Selected, PHead),
	cuf_combine_bodies(PBody, BodyR, BodyR1),
	cuf_expand_body(complete(ANo,PNo), BodyR1, BodyR2, RuleNos),
	store(BodyR2, AHead, ParGoalNo, RuleNos),
	fail.
a_complete(_,_).



store(Body, Head, No, RuleNos) :- 
	cuf_select(Body, Selected, BodyR), !,
%	\+ subsumed_active(Selected, BodyR, Head),
	last_active_number(ANo),
	ANo1 is ANo+1,
	
	( \+ get_flag(tracing, no) ->
	      \+ \+ print_active(Body,Head,ANo1)
	; true),
	asserta(active(ANo1, Selected, BodyR, Head, No, RuleNos)),
	assert(earley_action(active(ANo1))),
	cuf_debug_store_earley(active(ANo1)),
        predict(Selected,ANo1,_).
store(Body, Head, No, RuleNos) :- 
	\+ subsumed_passive(No,Head, Body),
	last_passive_number(PNo),
	PNo1 is PNo+1,
	( \+ get_flag(tracing, no) ->
	      \+ \+ print_passive(Body,Head,PNo1)
	; true),
	asserta(goal_passive(No,PNo1, Head, Body, RuleNos)),
	assert(earley_action(passive(PNo1))),
	cuf_debug_store_earley(passive(PNo1)),
	p_complete(No,PNo1,Head,Body).






%% Body are just Constraints to be passed up
subsumed_passive(No,Head,Body) :-
	goal_passive(No,_,GenHead,GenBody,_),
	cuf_subsumes_head(GenHead, Head),
	cuf_subsumes_body(GenBody, Body).

% subsumed_active(Selected, BodyR, Head) :-
% 	functor(Selected, SFunc, SAr),
% 	functor(GenSelected, SFunc, SAr),
% 	active(GenSelected,GenBodyR,GenHead),
% 	subsumes(t(GenSelected,GenBodyR,GenHead),t(Selected,BodyR,Head)).

subsumed_goal(Goal, OldNo) :-
	cuf_goal_functor(Goal, F, Ar),
	cuf_goal_functor(GenGoal, F, Ar),
	goal(GenGoal, No),
	cuf_subsumes_head(GenGoal, Goal), !,
	OldNo = No.

last_goal_number(No) :-
	goal(_,No) -> true; No=0.
last_active_number(No) :-
	active(No, _,_,_,_,_) -> true; No=0.
last_passive_number(No) :-
	goal_passive(_,No, _,_,_) -> true; No=0.


%restriction(no).

chart_statistics(GNo, Passives, Actives, Analyses) :-
   findall(1, goal_passive(_,_,_,_,_), Ps),
   length(Ps, Passives),
   findall(1, active(_,_,_,_,_,_), As),
   length(As, Actives),
   findall(1, goal_passive(GNo,_,_,_,_), Res),
   length(Res, Analyses).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: Prolog Interface File to atomsat              %%%
%%%      Created: Fri Oct 23 13:49:41 1992                      %%%
%%%     Modified: Mon Mar 25 12:14:36 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.6  1996/04/09 14:39:12  jochen
% mended a leak in SAT cache causing failed calls with given
% return value not to be cached (was fatal for atom unification);
% debug wrapper for SAT calls removed
%
% Revision 1.5  1995/07/28 08:57:16  michel
% type_subsumes/3: bug fixed reported by Stefan Momma, IBM:
% 		 treatment of uninstantiated FSs (`top' represented as
% 		 a variable) added
%
% Revision 1.4  1995/07/10  09:00:29  jochen
% now using own hashing function(s) for SAT-cache
% (cuf_type_term_hash{,2})
%
% Revision 1.3  1994/03/25  16:03:21  jochen
% change due to new handling of prolog-dependent stuff
%
% Revision 1.2  1994/01/16  10:20:54  jochen
% MAJOR REVISION:
%
% New internal formats for undeclared constants (no longer odd integers, but):
% string(String), number(Number), afs_symbol(Symbol)
%
% Note that these may not occur in axioms. In calls to check_formula/s
% or type_subsumes these constants are replaced by 'volatile' codes (odd
% integers, larger than the biggest used constID).
%
% Consistency checking of types is now done in SAT module, new
% predicate: check_n_close_axioms. This also checks for determinateness
% of atoms and atom domains (string, number, afs_symbol).
%
% New foreign predicates:
%     check_n_close_axioms,
%     classify_atom,
%     unclassify_atom,
%     set_sat_trace,  (formerly: set_trace)
%     set_sat_debug,
%     assert_atom_domain,
%
% Revision 1.1  1993/11/08  13:49:27  jochen
% check in cuf2.28 (initial check in)
%


% trace version of atomsat-pl.pl
%:- ensure_loaded(time).

:- dynamic sat_debug/0,
	   'ATOM DOMAINS'/1,
	   'CHECK FORMULA'/3,'CHECK FORMULAS'/4,
	   'TYPE SUBSUMES'/4,
	   'VOLATILE_TYPE_CODE'/2.



is_atom_domain(Atom) :-
    'ATOM DOMAINS'(DomList), !,
    member(Atom,DomList).

init_sat :-
    'ATOM DOMAINS'(AtomDoms), !,    %% init_sat/1 already called
    init_sat0,
    forall(member(X,AtomDoms),assert_atom_domain(X)), !,
    init_sat_tables.
init_sat :-
    cuf_out(error, ['INTERNAL ERROR (init_sat/0): no atom domains']).


init_sat(AtomDoms) :-
    init_sat0,
    forall(member(X,AtomDoms),
	   (integer(X), X>1, 1 is X mod 2, assert_atom_domain(X))), !,
    init_sat_tables,
    retractall('ATOM DOMAINS'(_)),
    assert('ATOM DOMAINS'(AtomDoms)).
init_sat(AtomDoms) :-
    cuf_out(error, ['INTERNAL ERROR: bad argument in init_sat/1',
                    AtomDoms]).

init_sat_tables :-
    retractall('CHECK FORMULA'(_,_,_)),
    retractall('CHECK FORMULAS'(_,_,_,_)),
    retractall('TYPE SUBSUMES'(_,_,_,_)).



/* 
  add_axioms(+List_of_Clauses,-Consistency)

  List_of_Clauses is added temporarily to the current set of axioms.
  If the new set is consistent, the addition is made permanent and the
  second argument is unified with 1. If the new set is inconsistent,
  the original set of axioms remains in force and 0 is returned as
  second argument. 

*/
add_axioms(A,C) :-
%    ifsatdebug((time(add_axioms_0(A,C),T),
%		write(add_axioms(A,C):T),nl),
	       add_axioms_0(A,C) %)
    ,
    (C==1 -> init_sat_tables ; true).
    



/* 
  check_formula(+List_of_Clauses,-Consistency)

  If List_of_Clauses is consistent with the current set of axioms, the
  second argument is unified with 1, otherwise with 0. In any case,
  the original set of axioms remains in force.

*/

check_formula(F,C_o) :-
    cuf_type_term_hash(F,N),
    ( 'CHECK FORMULA'(N,F,C), !
    ; add_volatile_codes(F,F1,VolatAtomList),
      forall(member(VolatAtom^AtomDom,VolatAtomList),
	     classify_atom(VolatAtom, AtomDom)),
%      ifsatdebug((time(check_formula_0(F1,C),T),
%		  write(check_formula(F1,C):T),nl),
		 check_formula_0(F1,C) %)
      ,
      forall(member(VolatAtom^AtomDom,VolatAtomList),
	     unclassify_atom(VolatAtom)),
      assert('CHECK FORMULA'(N,F,C))
    ),
    C_o = C.
    



/* 
  check_formulas(+List_of_Clauses1,+List_of_Clauses2,-Consistency)

  The third argument is unified with an integer between 0 and 3,
  according to the following conditions:

  0   Ax & LoC1 & LoC2 inconsistent
  1   Ax & LoC1 & LoC2 consistent, but Ax & LoC1 & not LoC2 inconsistent
              i.e.  Ax & LoC1 & LoC2 <=> Ax & LoC1
  2   Ax & LoC1 & LoC2 consistent, but Ax & not LoC1 & LoC2 inconsistent
              i.e.  Ax & LoC1 & LoC2 <=> Ax & LoC2
  3   Ax & LoC1 & LoC2 consistent, but no simplification possible, i.e.
      Ax & LoC1 & not LoC2 and Ax & not LoC1 & LoC2 are also consistent

*/

check_formulas(F1,F2,C_o) :-
    cuf_type_term_hash2(F1,F2, N),
    ( 'CHECK FORMULAS'(N,F1,F2,C) -> true
    ; add_volatile_codes([F1|F2], [FA1|FA2],VolatAtomList),
      forall(member(VolatAtom^AtomDom,VolatAtomList),
	     classify_atom(VolatAtom, AtomDom)),
%      ifsatdebug((time(check_formulas_0(FA1,FA2,C),T),
%		  write(check_formulas(FA1,FA2,C):T),nl),
		 check_formulas_0(FA1,FA2,C) %)
      ,
      forall(member(VolatAtom^AtomDom,VolatAtomList),
	     unclassify_atom(VolatAtom)),
      assert('CHECK FORMULAS'(N,F1,F2,C))
    ),
    C_o = C.


/* 
  type_subsumes(+List_of_Clauses1,+List_of_Clauses2,-Subsumed?)

  The third argument is:

  1   iff Ax & not LoC1 & LoC2 inconsistent, 
              i.e.  Ax & LoC1 & LoC2 <=> Ax & LoC2
  0   otherwise
*/

type_subsumes(F1,F2,C_o) :-
    cuf_type_term_hash2(F1,F2, N),
    ( 'TYPE SUBSUMES'(N,F1,F2,C), !
    ; ( var(F1) ->
	    ( var(F2) -> C=1
	    ; add_volatile_codes([[[1,-1]]|F2], [FA1|FA2],VolatAtomList),
	      type_subsumes(FA1,FA2,VolatAtomList,C)
	    )
      ; /* nonvar(F1) */
        var(F2) ->
	    add_volatile_codes([F1|[[1,-1]]],[FA1|FA2],VolatAtomList),
	    type_subsumes(FA1,FA2,VolatAtomList,C)
      ; /* nonvar(F1), nonvar(F2) */
        add_volatile_codes([F1|F2], [FA1|FA2],VolatAtomList),
	type_subsumes(FA1,FA2,VolatAtomList,C)
      ),
      assert('TYPE SUBSUMES'(N,F1,F2,C))
    ),
    C_o = C.

type_subsumes(FA1,FA2,VolatAtomList,C) :-
	forall(member(VolatAtom^AtomDom,VolatAtomList),
	       classify_atom(VolatAtom, AtomDom)),
%	ifsatdebug((time(type_subsumes_0(FA1,FA2,C),T),
%		    write(type_subsumes(FA1,FA2,C):T),nl),
		   type_subsumes_0(FA1,FA2,C) %)
	,
	forall(member(VolatAtom^AtomDom,VolatAtomList),
	       unclassify_atom(VolatAtom)).



%:- meta_predicate ifsatdebug(0,0).

% ifsatdebug(X,Y) :- sat_debug -> X ; Y.


check_formulas_0(F1,F2,Cons) :-
	append(F1,F2,F),
	check_formula_0(F,C1),
	(C1==0 -> Cons=0
	; myneg_cnf2cnf(F1,NF1),
	  append(NF1,F2,F1N),
	  check_formula_0(F1N,C1N),
	  (C1N==0 -> Cons=2
	  ; myneg_cnf2cnf(F2,NF2),
	    append(NF2,F1,F2N),
	    check_formula_0(F2N,C2N),
	    (C2N==0 -> Cons=1
	    ; Cons=3  ) ) ).


type_subsumes_0(F1,F2,Cons) :-
	myneg_cnf2cnf(F1,NF1),
	append(NF1,F2,F1N),
	check_formula_0(F1N,ICons),
	Cons is 1-ICons.



myneg_cnf2cnf(Formula,Negated) :-
	neg_cnf2cnf_1(Formula,[],0,Negated,[]).
%	numbervars(Negated,1,_).


neg_cnf2cnf_1([Clause],AuxLits,_) --> !,
	negate_clause(Clause,AuxLits).
neg_cnf2cnf_1(Formula,AuxLits,AuxV) -->
	{ length(Formula,Len),
	  LenH is Len>>1,
	  length(HalfFormula1,LenH),
	  append(HalfFormula1,HalfFormula2,Formula), !,
	  Aux is AuxV+2,
	  NAux is -Aux },
	neg_cnf2cnf_1(HalfFormula1,[aux(Aux)|AuxLits],Aux),
	neg_cnf2cnf_1(HalfFormula2,[aux(NAux)|AuxLits],Aux).



negate_clause([],_AuxLits) --> [].
negate_clause([Lit|R],AuxLits) -->
	{ NLit is 0-Lit },
	[[NLit|AuxLits]],
	negate_clause(R,AuxLits).




%% add_volatile_codes(+InForm,-OutForm,-VolatAtomList)
%%
%% Converts expressions string(StringAtom), number(NumbAtom), and
%% afs_symbol(OtherAtom) which originate from undeclared atoms in type
%% expressions into new type codes. The new type codes are volatile
%% and only valid for one call of the predicates check_formula/2,
%% check_formulas/3, and type_subsumes/3. VolatAtomList contains pairs
%% of these new codes together with the atom domain code (e.g. string,
%% number, and afs_symbol) of the resp. atom.

add_volatile_codes(InForm,OutForm,VolatAtomList) :-
	clause('CUF_ID'(constID,CurrentConstID), true),!,  %% hack
	retractall('VOLATILE_TYPE_CODE'(_,_)),
	add_volatile_codes(InForm,OutForm,CurrentConstID,_,VolatAtomList,[]).

add_volatile_codes(I,I,CurID,CurID) --> {integer(I), ! }.
add_volatile_codes([],[],CurID,CurID) --> [].
add_volatile_codes(string(Atom),Code,CurID_i,CurID_o) -->
	{ get_volatile_code(Atom,Code,CurID_i,CurID_o),
	  symbol_table(check,string,0,StringCode,_,type,ok)}, 
	[Code^StringCode].
add_volatile_codes(number(Atom),Code,CurID_i,CurID_o) -->
	{ get_volatile_code(Atom,Code,CurID_i,CurID_o),
	  symbol_table(check,number,0,NumberCode,_,type,ok)}, 
	[Code^NumberCode].
add_volatile_codes(afs_symbol(Atom),Code,CurID_i,CurID_o) -->
	{ get_volatile_code(Atom,Code,CurID_i,CurID_o),
	  symbol_table(check,afs_symbol,0,Afs_symbolCode,_,type,ok)}, 
	[Code^Afs_symbolCode].
add_volatile_codes([H|T],[HA|TA],CurID,CurID2) -->
	add_volatile_codes(H,HA,CurID,CurID1),
	add_volatile_codes(T,TA,CurID1,CurID2).
add_volatile_codes(-T,NegTA,CurID_i,CurID_o) -->
	add_volatile_codes(T,TA,CurID_i,CurID_o),
	{ integer(TA),   %% just safety
          NegTA is -TA  }.

get_volatile_code(Atom,Code,CurID,CurID) :-
	'VOLATILE_TYPE_CODE'(Atom,Code), !.
get_volatile_code(Atom,CurID_i,CurID_i,CurID_o) :-
	assert('VOLATILE_TYPE_CODE'(Atom,CurID_i)),
	CurID_o is CurID_i+2.
	



check_n_close_axioms(ErrList) :-
	check_n_close_axioms_0(ErrList).


%% classify_atom(Const, AtomDom)  (foreign)
%% tell SAT that code Const is in the domain with the code AtomDom

%% unclassify_atom(Const)  (foreign)
%% redo effect of classify_atom(Const, AtomDom) 



foreign_file('../obj/atomsat-pl',[set_sat_trace,set_sat_debug,init_sat,
                           assert_atom_domain, check_formula,
                           add_axioms,check_n_close_axioms,
			   classify_atom, unclassify_atom]).

foreign(check_n_close_axioms, c, check_n_close_axioms_0([-term])).
foreign(classify_atom, c, classify_atom(+integer,+integer)).
foreign(unclassify_atom, c, unclassify_atom(+integer)).
foreign(set_sat_trace, c, set_sat_trace(+integer)).
foreign(set_sat_debug, c, set_sat_debug(+integer)).
foreign(init_sat, c, init_sat0).
foreign(assert_atom_domain, c, assert_atom_domain(+integer)).
foreign(check_formula, c, check_formula_0(+term,[-integer])).
foreign(add_axioms, c, add_axioms_0(+term,[-integer])).


%:- load_foreign_files('atomsat-pl', []).
%:- abolish([foreign/3, foreign_file/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: CUF parser                                    %%%
%%%      Created: Fri Sep 18 13:17:17 1992                      %%%
%%%     Modified: Tue Mar 26 13:43:30 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.12  1996/04/09 14:12:21  jochen
% now handling parse errors with exceptions;
% module prefix for 'PARSER OUTPUT';
% some error msgs made less redundant;
% code rearrangements (output param.)
%
% Revision 1.11  1995/06/13 16:06:23  jochen
% bug fix (compiler loop) in undef_symbol_exists/6 (see
%   ~jochen/tmp/CUF/recomp_loop1+2)
%
% Revision 1.10  1994/05/18  16:57:11  michel
% parse_feat_term_/5:
% * !/1 case for prolog terms added
%
% Revision 1.9  1994/05/17  17:08:16  michel
% add_undef_symbol/5:
% * 2nd clause added for updating 'USED OCC' table (bug fix)
%
% Revision 1.8  1994/05/11  10:37:42  michel
% some minor changes:
% undef_symbol_exists/6, clause 1: retracts all default feat_decls now
% add_undef_symbol/5: calls add_default_feature_declaration/3 with FID now
%
% Revision 1.7  1994/02/11  18:50:34  michel
% new predicate: unwrap_feat/2
% A feature can be a number now (was a bug because only afs_symbols were
% allowed). Should we allow strings as well?
%
% Revision 1.6  1994/01/23  17:18:39  michel
% some error messages changed
% parse_type_term_/6: 'N'/1 and 'S'S/1 case added
%
% Revision 1.5  1994/01/18  11:58:28  jochen
% Now all strings that are read cause 'STRING_CONST' facts to be asserted.
% All string and number occ.s are reflected in 'DEF OCC' and 'USED OCC'.
% (Important if a string of implicit type gets assigned a type.)
% Changes in parse_sort_/5, parse_feat_term_/5, parse_constant/2,
% undef_symbol_exists/6 (last clause fixed, but not used any longer)
% add_undef_symbol/5 (new first clause)
%
% Revision 1.4  1994/01/14  10:13:23  jochen
% 1) Changed handling of numbers. Reader gives back tokens of form
%   'N'(Number).
%
% 2) parse_constants changed. Strings (and numbers) now allowed in
%   constant declarations (e.g. t1 = {"mystring",3,4,5}. )
%
% 3) No checking of 'redefinition of a built-in type' in type axioms any
%   more.
%
% Revision 1.3  1993/12/08  14:49:57  michel
% internal string format changed
%
% Revision 1.2  1993/12/07  17:21:39  michel
% "undefinded" bug fixed
%         ^
%
% Revision 1.1  1993/11/08  13:50:14  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Thu Jul  1 20:05:07 1993 (dorna) : annotatedterm2prolog/3 calls removed
% Thu Jul  1 19:55:52 1993 (dorna) : univs removed
% Tue Apr  6 19:46:00 1993 (michel) : var_checker/4 added
% Mon Mar 22 13:58:25 1993 (michel) : atom_chars/2 for 'S'/1 case
% Wed Dec  2 12:19:56 1992 (michel) : ~ as negation operator added
% Tue Dec  1 16:45:41 1992 (michel) : parser_CUF_expr/2 --> parser_CUF_expr/3,
%                                     cuf_out/2 added
% Fri Oct 30 15:07:14 1992 (michel) : clause output format changed

%%:- dynamic 'PARSER OUTPUT'/4. now in file-specific modules


:- dynamic 'STRING_CONST'/1.  %% recorded for occ. of strings
%% We should add a 2nd Kindof field to 'DEF OCC', 'USED OCC', and
%% symbol_table, but this would cause major revisions.
%% 'STRING_CONST'/1 facts are introduced here and deleted in
%% file_deletion.pl.


%%%  parse_CUF_expr(+CUF_Expression, +FileID, +EntryID)
parse_CUF_expr(Expr, FileID, EntryID) :-
    on_exception(cuf_parse_error(Message),
		 parse_CUF_expr_(Expr, FileID, EntryID),
		 cuf_out(error,Message)
		).

parse_CUF_expr_(Expr, FileID, EntryID) :-
    parse_CUF_expr(Expr, Kind, Struc, FileID, EntryID), !,  %% for safety
    assert(FileID:'PARSER OUTPUT'(Kind, EntryID, Struc)),
    dot.


'PARSER OUTPUT'(Kind, FileID, EntryID, Struc) :-
    ( atom(FileID) -> true
    ; file_table(_,_,FileID)
    ),
    clause(FileID:'PARSER OUTPUT'(Kind, EntryID, Struc), true).

    
%%%  parse_CUF_expr(+Functor, +Arguments, -Kind, -Structure)
% sort definition
parse_CUF_expr(':='(Sort, FT), clause, Struc, FID, EID) :- !, 
    parse_CUF_clause(Sort, FT, Struc, FID, EID).
% sort declaration
parse_CUF_expr('->'(Sort, Type), sdecl, Struc, FID, EID) :- !,  
    parse_sort_decl(Sort, Type, Struc, FID, EID).
% feature decl.
parse_CUF_expr('::'(Type, FeatTypes), fdecl, Struc, FID, EID) :- !, 
    parse_feat_decl(Type, FeatTypes, Struc, FID, EID).
% prolog clause or call
parse_CUF_expr(':-'(Arg1,Arg2), prolog, Trans, _, _) :- !,  
    annotatedterm2prolog(':-'(Arg1,Arg2), Trans).
% prolog fact
parse_CUF_expr('T'(Arg), prolog, Trans, _, _) :- !,
    annotatedterm2prolog('T'(Arg), Trans).
parse_CUF_expr(Term, prolog, Trans, _, _) :-
    functor(Term,Op,_),       
    \+ current_op(_,_,Op), !,
    annotatedterm2prolog(Term, Trans).
% type axiom
parse_CUF_expr(TypeAxiom, type_axiom, Struc, FID, EID) :-       
    parse_type_axiom(TypeAxiom, Struc, FID, EID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  parse_CUF_clause(+Sort, +FeatureTerm, -Structure)
parse_CUF_clause(Sort, FT, (SParse, FTParse), FID, EID) :-
    parse_sort(Sort, SParse, def, FID, EID),
    parse_feat_term(FT, FTParse, used, FID, EID).

%%%  parse_sort(+Sort, -Structure)
parse_sort('A'(Name), Name, def, _FID, _EID) :-
    (is_const(Name,_), DefAs=constant ; is_type(Name,_), DefAs=type), !,
    cuf_parse_error([symbol, Name, 'known as',DefAs,'(ignoring redefinition)']).
parse_sort('A'(Name), Name, Occ, FID, EID) :- !,
    add_symbol_occ(Occ, Name, 0, sort, FID, EID).
parse_sort('T'(Sort), Parse, Occ, FID, EID) :- !,
    functor(Sort,Name,Ar), 
    Ar > 0,  %% safety
    functor(Parse,Name,Ar),
    add_symbol_occ(Occ, Name, Ar, sort, FID, EID),
    parse_feat_term_args(0,Ar,Sort,Parse,FID,EID).
parse_sort(Sort, _Parse, _Occ, FID, EID) :-
    annotatedterm2prolog(Sort, TransSort),
    cuf_parse_error(['CUF sort not well-formed:', 
		     '$FT$'(TransSort), '$E$'(FID,EID)]).


%%%  parse_feat_term(+FeatureTerm, -Structure)
parse_feat_term('T'(Arg), SortStruc, Occ, FID, EID) :- !,
    parse_sort('T'(Arg), SortStruc, Occ, FID, EID).
parse_feat_term('A'(Arg), SortStruc, Occ, FID, EID) :- !,
    parse_sort('A'(Arg), SortStruc, Occ, FID, EID).
parse_feat_term('V'(Var), Out, _, _, _) :- !,
    Out = 'V'(Var).
parse_feat_term('S'(Chars), Out, _, FID, EID) :- !,
    Out = 'S'(String),
    append([0'"|Chars], [0'"], Cs),
    atom_chars(String, Cs),
    assert_if_new('STRING_CONST'(String)),
    add_symbol_occ(used, String, 0, const, FID, EID).
parse_feat_term('N'(Number), Out, _, FID, EID) :- !,
    Out = 'N'(Number),
    add_symbol_occ(used, Number, 0, const, FID, EID).
parse_feat_term(!(Term), Out, _, _, _) :- !,
    Out = !(Prolog),
    annotatedterm2prolog(Term, Prolog).
parse_feat_term('&'(FT1, FT2), Out, Occ, FID, EID) :- !,
    Out = '&'(FT1Struc, FT2Struc),
    parse_feat_term(FT1, FT1Struc, Occ, FID, EID),
    parse_feat_term(FT2, FT2Struc, Occ, FID, EID).
parse_feat_term(';'(FT1, FT2), Out, Occ, FID, EID) :- !,
    Out = ';'(FT1Struc, FT2Struc),
    parse_feat_term(FT1, FT1Struc, Occ, FID, EID),
    parse_feat_term(FT2, FT2Struc, Occ, FID, EID).
parse_feat_term(':'(WFeat, FT), Out, Occ, FID, EID) :- !,
    Out = ':'(Feat, FTStruc),
    unwrap_feat(WFeat, Feat),
    add_symbol_occ(Occ, Feat, 1, feat, FID, EID),
    parse_feat_term(FT, FTStruc, Occ, FID, EID).
parse_feat_term('~'(FT), Out, Occ, FID, EID) :- !,
    Out = '~'(FTStruc),
    parse_feat_term(FT, FTStruc, Occ, FID, EID).
parse_feat_term([], Out, _, _, _) :- !,
    Out = [].
parse_feat_term([F|R], Out, Occ, FID, EID) :- !,
    Out = '&'(':'('F', FStruc),':'('R', RStruc)),
    parse_feat_term(F, FStruc, Occ, FID, EID),
    parse_feat_term(R, RStruc, Occ, FID, EID).
parse_feat_term(FeatTerm, _Struc, _Occ, FID, EID) :-
    annotatedterm2prolog(FeatTerm, TransFeatTerm),
    cuf_parse_error(['feature term not well-formed:',
		     '$FT$'(TransFeatTerm), '$E$'(FID,EID)]).



unwrap_feat('A'(Feat), Feat).
unwrap_feat('N'(Feat), Feat).

%%%  parse_feat_term_args(+Arg,+Arity,+SortTerm,+ParsedTerm,+FID,+EID)
parse_feat_term_args(N,M,InTerm,OutTerm,FID,EID) :-
    ( N =:= M -> true
    ; Next is N+1,
      arg(Next,InTerm,InArg),
      arg(Next,OutTerm,OutArg),
      parse_feat_term(InArg,OutArg,used,FID,EID),
      parse_feat_term_args(Next,M,InTerm,OutTerm,FID,EID)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  parse_sort_decl(+TypedSort, +TypeTerm, -Structure)
parse_sort_decl(TypedSort, TypeTerm, d(Sort, Arity, [Struc2|Struc1]),
                FID, EID) :-
    ( parse_typed_sort(TypedSort, Sort, Arity, Struc1, FID, EID),
      parse_type_term(TypeTerm, Struc2, no, used, FID, EID), !
    ; cuf_parse_error(['sort declaration not well-formed:', '$E$'(FID,EID)])
    ).
    
%%%  parse_typed_sort(+Functor, +Argument, -Structure)
parse_typed_sort('A'(Name), Name, 0, [], FID, EID) :-
    add_symbol_occ(def, Name, 0, sort, FID, EID).
parse_typed_sort('T'(Sort), Name, N, SortStruc, FID, EID) :-
    Sort =.. [Name|Args],
    length(Args, N),
    add_symbol_occ(def, Name, N, sort, FID, EID),
    parse_type_term_list(Args, SortStruc, FID, EID).

%%%  parse_type_term(+Term, -Structure, +AxiomFlag)
parse_type_term(Term, Struc, Axiom, Occ, FID, EID) :-
    ( parse_type_term_(Term, Struc, Axiom, Occ, FID, EID), !
    ; annotatedterm2prolog(Term, TransTerm),
      cuf_parse_error(['type term not well-formed:', 
		       '$FT$'(TransTerm), '$E$'(FID,EID)])
    ).

%%%  parse_type_term_(+Term, -Structure, +AxiomFlag, +OccFlag)
parse_type_term_('&'(Arg1, Arg2), '&'(Struc1, Struc2), Axiom,
                Occ, FID, EID) :-
    parse_type_term(Arg1, Struc1, Axiom, Occ, FID, EID),
    parse_type_term(Arg2, Struc2, Axiom, Occ, FID, EID).
parse_type_term_(';'(Arg1, Arg2), ';'(Struc1, Struc2), Axiom,
                Occ, FID, EID) :-
    parse_type_term(Arg1, Struc1, Axiom, Occ, FID, EID),
    parse_type_term(Arg2, Struc2, Axiom, Occ, FID, EID).
parse_type_term_('~'(Arg), '~'(Struc), Axiom, Occ, FID, EID) :-
    parse_type_term(Arg, Struc, Axiom, Occ, FID, EID).
parse_type_term_('A'(Arg), Parse, Axiom, Occ, FID, EID) :-
    ( is_basic_type(Arg, _) ->
%	  ( Axiom == yes -> 
%	      cuf_out(error, ['redefinition of a built-in type:',Arg]),
%     	      !, fail	
%	  ; Parse = Arg )
          Parse = Arg
    ; parse_type(Axiom, Arg, Parse, Occ, FID, EID)
    ).
parse_type_term_('N'(Number), Number, no, used, FID, EID) :-
    add_symbol_occ(used, Number, 0, const, FID, EID).
parse_type_term_('S'(Chars), String, no, used, FID, EID) :-
    append([0'"|Chars], [0'"], Cs),
    atom_chars(String, Cs),
    assert_if_new('STRING_CONST'(String)),
    add_symbol_occ(used, String, 0, const, FID, EID).

%%%  parse_type_term_list(+TypeTermList, -Structure, +AxiomFlag)
parse_type_term_list([], [], _, _).
parse_type_term_list([TT|R], [TTStruc|RStruc], FID, EID) :-
    parse_type_term(TT, TTStruc, no, used, FID, EID),
    parse_type_term_list(R, RStruc, FID, EID).

%%%  parse_type(+AxiomFlag, +Functor, +Argument, -Structure)
parse_type(yes, Name, Name, Occ, FID, EID) :-
    add_symbol_occ(Occ, Name, 0, type, FID, EID).
% next clause may cause problems for supertypes which can now be constants
parse_type(no, Name, Name, Occ, FID, EID) :-
    add_symbol_occ(Occ, Name, 0, typeORconst, FID, EID).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  parse_feat_decl(+Type, +FeatTypes, -Declarations)
parse_feat_decl(Type, FeatTypes, (TypeStruc, Decls), FID, EID) :-
   ( parse_type_term(Type, TypeStruc, yes, def, FID, EID),
     parse_feat_types(FeatTypes, Decls, FID, EID), !
   ; cuf_parse_error(['feature declaration not well-formed','$E$'(FID,EID)])
   ).

%%%  parse_feat_types(+FeatureTypes, +TypeStructure, -Declarations)
parse_feat_types(':'(WFeat,TT), [':'(Feat,Struc)], FID, EID) :-
    unwrap_feat(WFeat, Feat),
    add_symbol_occ(def, Feat, 1, feat, FID, EID), !,
    parse_type_term(TT, Struc, no, used, FID, EID).
parse_feat_types((':'(WFeat,TT), FTs), [':'(Feat,Struc)|RDs], FID, EID) :-
    unwrap_feat(WFeat, Feat),
    add_symbol_occ(def, Feat, 1, feat, FID, EID),
    parse_type_term(TT, Struc, no, used, FID, EID), !,
    parse_feat_types(FTs, RDs, FID, EID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  parse_type_axiom(+Functor, +Arguments, -Structure)
parse_type_axiom('|'(Arg1,Arg2), Struc, FID, EID) :- !,
    parse_disjoints('|'(Arg1,Arg2), Struc, FID, EID).
parse_type_axiom('{}'(Arg), Struc, FID, EID) :- !,
    parse_disjoints('{}'(Arg), Struc, FID, EID).
parse_type_axiom('<'(S, T), Struc, FID, EID) :- !,
    parse_type_hier(S, T, Struc, FID, EID).
parse_type_axiom('='(T1, T2), Struc, FID, EID) :- !,
    parse_type_eq(T1, T2, Struc, FID, EID).
parse_type_axiom(TypeTerm, Struc, FID, EID) :-
    parse_type_term(TypeTerm, Struc, yes, def, FID, EID).

%%%  parse_disjoints(+Term, -Structure)
parse_disjoints(Term, Struc, FID, EID) :-
    ( parse_disjoints_(Term, Struc, FID, EID)
    ; annotatedterm2prolog(Term, TransTerm),
      cuf_parse_error(['disjoints not well-formed:', 
		       '$FT$'(TransTerm), '$E$'(FID,EID)])
    ).

%%%  parse_disjoints_(+Term, -Structure)
parse_disjoints_('|'(Arg1,Arg2), 'Disjs'(Struc), FID, EID) :- !,
    parse_disjoint(Arg1, Arg2, Struc, FID, EID).
parse_disjoints_('{}'(Arg), Trans, FID, EID) :- !,
    parse_constants(Arg, Struc, FID, EID),
    ( Struc = [C] ->
	  Trans = C
    ; Trans = ';'(Struc)
    ).
parse_disjoints_(TypeTerm, Struc, FID, EID) :-
    parse_type_term(TypeTerm, Struc, yes, def, FID, EID).

parse_disjoint('|'(Arg1,Arg2), Struc, FID, EID) :- !,
    parse_disjoint(Arg1, Arg2, Struc, FID, EID).
parse_disjoint(Term, [Struc], FID, EID) :-
    parse_disjoints(Term, Struc, FID, EID).

parse_disjoint(D1, D2, Struc, FID, EID) :-
    parse_disjoint(D1, S1, FID, EID), !,
    append(S1, S2, Struc),
    parse_disjoint(D2, S2, FID, EID).

%%% parse_constants(+Constants, -Structure)
parse_constants((InC, Cs), [C|CsStruc], FID, EID) :- !,
    parse_constant(InC, C),
    add_symbol_occ(def, C, 0, const, FID, EID), !,
    parse_constants(Cs, CsStruc, FID, EID).
parse_constants(InC, [C], FID, EID) :-
    parse_constant(InC, C),
    add_symbol_occ(def, C, 0, const, FID, EID), !.


parse_constant('A'(C),C).
parse_constant('S'(Chars),String) :-
    append([0'"|Chars], [0'"], Cs),
    atom_chars(String, Cs),
    assert_if_new('STRING_CONST'(String)).
parse_constant('N'(C),C).
%% etwas inkonsequent, hier nur Atome zurueckzugeben (vgl. oben), but
%% it's getting late.

%%%  parse_type_hier(+SubTypes, +Type, -Structure)
parse_type_hier(SubT, Type, '<'(SubTStruc, TypeStruc), FID, EID) :-
    ( (functor(SubT, '{}', _) ; functor(SubT, '|', _)) ->
           parse_disjoints(SubT, SubTStruc, FID, EID)
    ; parse_type_term(SubT, SubTStruc, yes, def, FID, EID)
    ),
    parse_type_term(Type, TypeStruc, no, hier, FID, EID), !.
parse_type_hier(_, _, _, FID, EID) :-
    cuf_parse_error(['type axiom not well-formed', '$E$'(FID,EID)]).

%%%  parse_type_eq(+Type1, +Type2, -Structure)
parse_type_eq(Type1, Type2, '='(Struc1, Struc2), FID, EID) :-
    parse_type_term(Type1, Struc1, yes, def, FID, EID),
    ( functor(Type2,Func,_),
      (Func == '{}'; Func == '|') ->
         parse_disjoints(Type2, Struc2, FID, EID)
    ; parse_type_term(Type2, Struc2, yes, def, FID, EID)
    ), !.
parse_type_eq(_, _, _, FID, EID) :-
    cuf_parse_error(['type axiom not well-formed', '$E$'(FID,EID)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 'TYPE CONST'/3,
	   'SORT TYPE CONST'/3.
:- dynamic 'DEF OCC'/5,
	   'USED OCC'/5.

used_occ_table(Name, Arity, KindOf, FID, EID) :-
   ( built_in_symbol(KindOf, Name, Arity) -> true
   ; assert_if_new('USED OCC'(Name, Arity, KindOf, FID, EID)) ).

%%%  add_symbol_occ(Occurence, Name, Arity, KindOf, FileID, EntryID)
add_symbol_occ(def, Name, Arity, KindOf, FID, EID) :-
   def_occ_table(Name, Arity, KindOf, FID, EID).
add_symbol_occ(used, Name, Arity, KindOf, FID, EID) :-
   add_used_occ(KindOf, Name, Arity, FID, EID).
add_symbol_occ(hier, Name, Arity, typeORconst, FID, EID) :- !,
   def_occ_table(Name, Arity, type, FID, EID).
add_symbol_occ(hier, Name, Arity, KindOf, FID, EID) :-
   def_occ_table(Name, Arity, KindOf, FID, EID).

add_used_occ(typeORconst, Name, _, FID, EID) :- !,
   assert_if_new('TYPE CONST'(Name, FID, EID)).
add_used_occ(sort, Name, Arity, FID, EID) :- !,
   ( Arity > 0 ->
       used_occ_table(Name, Arity, sort, FID, EID)
   ; assert_if_new('SORT TYPE CONST'(Name, FID, EID))
   ).
add_used_occ(KindOf, Name, Arity, FID, EID) :-
   used_occ_table(Name, Arity, KindOf, FID, EID).

def_occ_table(Name, Arity, KindOf, FID, EID) :-
   ( built_in_symbol(KindOf, Name, Arity) -> true % should cause an error (?)
   ; assert_if_new('DEF OCC'(Name, Arity, KindOf, FID, EID)),
     ( Arity > 0 ->
	 add_def_symbol(Name, Arity, KindOf, FID, EID)
     ; true)
   ).

add_def_symbol(Symbol, Arity, KindOf, FID, EID) :-
   symbol_table(look, Symbol, Arity, _, _, _, Result),
   add_def_symbol(Result, Symbol, Arity, KindOf, FID, EID).

add_def_symbol([], Symbol, Arity, KindOf, _, _) :-
   symbol_table(assert, Symbol, Arity, _, *, KindOf, ok).
add_def_symbol([def(K,M)], Symbol, Arity, KindOf, FID, EID) :- !,
   add_def_symbol(M, K, Symbol, Arity, KindOf, FID, EID).
add_def_symbol([def(K,M)|R], Symbol, Arity, KindOf, FID, EID) :-
   add_def_symbol(M, K, Symbol, Arity, KindOf, FID, EID),
   add_def_symbol(R, Symbol, Arity, KindOf, FID, EID).

add_def_symbol('-', KindOfOld, Symbol, Arity, KindOf, FID, EID) :-
   undef_symbol_exists(KindOfOld, KindOf, Symbol, Arity, FID, EID).
add_def_symbol('*', KindOfOld, Symbol, Arity, KindOf, FID, EID) :-
   def_symbol_exists(KindOfOld, KindOf, Symbol, Arity, FID, EID).
add_def_symbol('+', KindOfOld, Symbol, Arity, KindOf, FID, EID) :-
   def_symbol_exists(KindOfOld, KindOf, Symbol, Arity, FID, EID).

def_symbol_exists(KindOf, KindOf, _, _, _, _) :- !.
def_symbol_exists(_, feat, Symbol, Arity, _, _) :- !,
   symbol_table(assert, Symbol, Arity, _, *, feat, _).
def_symbol_exists(feat, KindOf, Symbol, Arity, _, _) :- !,
   symbol_table(assert, Symbol, Arity, _, *, KindOf, _).
%% KindOf and KindOfOld is one of sort, type, or const:
def_symbol_exists(KindOfOld, KindOf, Symbol, _, FID, EID) :-
   prep_cuf_out(KindOfOld, _, _, KindOld, _),
   prep_cuf_out(KindOf, _, _, Kind, _),
   cuf_out(error, [KindOld,symbol,Symbol,'defined as a',Kind,
                   '$E$'(FID,EID)]),
   delete_parser_output(FID, EID),
   !, fail. 

undef_symbol_exists(KindOf, KindOf, Symbol, Arity, FID, EID) :- !,
   ( KindOf == feat ->
	 retractall(feat_decl(Symbol,_,_,_,default)),
	 recompiling_task(Symbol, Arity, KindOf, FID, EID),
	 cuf_out(message, ['overwriting default declaration for feature',
                           Symbol,'(recompilation follows)','$NL$'])
   ; true
   ), symbol_table(assert, Symbol, Arity, _, *, KindOf, ok).
undef_symbol_exists(_, feat, Symbol, Arity, _, _) :- !,
   symbol_table(assert, Symbol, Arity, _, *, feat, _).
undef_symbol_exists(feat, KindOf, Symbol, Arity, _, _) :- !,
   symbol_table(assert, Symbol, Arity, _, *, KindOf, _).
undef_symbol_exists(KindOfOld, KindOf, Symbol, Arity, FID, EID) :-
   recompiling_task(Symbol, Arity, KindOfOld, FID, EID),
   prep_cuf_out(KindOfOld, _, _, KindOld, _),
   prep_cuf_out(KindOf, _, _, Kind, _),
   symbol_table(retract, Symbol, Arity, _, -, KindOfOld,_),
   symbol_table(assert, Symbol, Arity, _, *, KindOf, _),
   cuf_out(warning, ['changing symbol kind for symbol',Symbol,from,KindOld,to,Kind,
                   '(recompilation follows)', '$E$'(FID,EID)]), !.


add_undef_symbol(Symbol, 0, const, _FID, _EID) :-
    ( number(Symbol) ; 'STRING_CONST'(Symbol) ), !.  
    %% don't add undef. numbers or strings
add_undef_symbol(Symbol, 0, KindOfOld, FID, EID) :-
    symbol_table(Symbol, 0, _, *, KindOfNew),
    KindOfNew \== KindOfOld, !,
    retract('USED OCC'(Symbol, 0, KindOfOld, FID, EID)),
    assert('USED OCC'(Symbol, 0, KindOfNew, FID, EID)).	  
%% next clause is only used for constants, features and sorts with arity > 0
add_undef_symbol(Symbol, Arity, KindOf, FID, EID) :-
    ( KindOf == const ->
	( get_flag(const_warnings, yes) ->
	    cuf_out(warning, ['treating undefined symbol',Symbol,
	                      'as a constant','$E$'(FID,EID)])
	  ; true )
    ; prep_cuf_out(KindOf, Symbol, Arity, Kind, SymAr),
      cuf_out(warning, ['undefined',Kind,symbol,SymAr,found,'$E$'(FID,EID)]),
      ( KindOf == feat ->
%	  cuf_out(warning, ['default declaration for feature',Symbol,added]),
	  add_default_feature_declaration(Symbol, FID, default)
      ; true )
    ),
    symbol_table(assert, Symbol, Arity, _, -, KindOf, ok).

%%%  prep_cuf_out(+KindOf,+Symbol,+Arity,-KindOfOut,-SymbolArity)
prep_cuf_out(const,Symbol,_,constant,Symbol).
prep_cuf_out(feat,Symbol,_,feature,Symbol).
prep_cuf_out(sort,Symbol,Arity,sort,Symbol/Arity).
prep_cuf_out(type,Symbol,_,type,Symbol).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Error handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cuf_parse_error(Message) :-
    raise_exception(cuf_parse_error(Message)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: building terms of tokens                      %%%
%%%      Created: Fri Mar 26 17:12:00 1993                      %%%
%%%     Modified: Mon Mar 18 13:14:25 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.4  1996/04/09 14:25:01  jochen
% 'L' functor eliminated;
% now recognize monadic operators ~ and ! even when followed by parens
%
% Revision 1.3  1996/03/14 16:47:29  jochen
% made faster; eliminated uses of assert (flags entry_begin/end, '|'-handling)
% side effect: -(a,b) is now correctly read as a -/2 term
%
% Revision 1.2  1994/01/14  10:15:09  jochen
% Changed handling of numbers. Reader gives back tokens of form 'N'(Number).
%
% Revision 1.1  1993/11/08  13:50:15  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Sun Aug  1 16:26:54 1993 (dorna) : after_prefix_op: ~ case added
% Tue Jul 20 17:07:43 1993 (dorna) : small bug fix in read/5
	
cuf_read(Answer, Variables, Lines) :-
   repeat,
      read_tokens0(Tokens, Variables, Lines),
      (   read(Tokens, 1200, Term, LeftOver), 
	  all_read(LeftOver),
	  syntax_error([], 0, _, _)    % remove any leftovers
      ;   Lines = LineB-LineE,
          syntax_error(Tokens, LineB, LineE) -> fail
      ), !,
   Answer = Term.

%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.
read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax_error(['expression expected'], []).

%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)
% read(X, _, _, _, _) :- var(X), !, fail. % space saver (????, md)

/*
%%%  macros
read(var(_, Name), ['(',')'|S1], Precedence, Answer, S) :- !,
    read_rest0(S1, 'M'(Name, []), Precedence, Answer, S).
read(var(_, Name), ['('|S1], Precedence, Answer, S) :- !,
    read(S1, 999, Arg1, S2),
    read_args(S2, RestArgs, S3), 
    read_rest0(S3, 'M'(Name, [Arg1|RestArgs]), Precedence, Answer, S).
*/
%%%  variables
read(var(Variable,_), S0, Precedence, Answer, S) :- !,
   read_rest0(S0, 'V'(Variable), Precedence, Answer, S).

read(atom(-), [number(Number)|S1], Precedence, Answer, S) :-
   Negative is -Number, !,
   read_rest0(S1, 'N'(Negative), Precedence, Answer, S).
read(atom(Functor), ['('|S1], Precedence, Answer, S) :- 
%   \+ prefixop(Functor, _, _), !,
   read(S1, 999, Arg1, S2),
   read_args(S2, RestArgs, S3),
   Term =.. [Functor,Arg1|RestArgs], !,
   ( (Functor == '~' ; Functor == '!') ->
	  Read = Term
   ; Read = 'T'(Term)
   ),
   read_rest0(S3, Read, Precedence, Answer, S).
read(atom(Atom), S0, Precedence, Answer, S) :-
   prefixop(Atom, Prec, Right), !,
   after_prefix_op(Atom, Prec, Right, S0, Precedence, Answer, S).
read(atom(Atom), S0, Precedence, Answer, S) :- !,
   read_rest0(S0, 'A'(Atom), Precedence, Answer, S).


read(number(Number), S0, Precedence, Answer, S) :- !,
   read_rest0(S0, 'N'(Number), Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
   read_rest0(S1, [], Precedence, Answer, S).
read('[', S1, Precedence, Answer, S) :- !,
   read(S1, 999, Arg1, S2),
   read_list(S2, RestArgs, S3), !,
   read_rest0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- !,
   read(S1, 1200, Term, S2),
   expect(')', S2, S3), !,
   read_rest0(S3, Term, Precedence, Answer, S).
read(' (', S1, Precedence, Answer, S) :- !,
   read(S1, 1200, Term, S2),
   expect(')', S2, S3), !,
   read_rest0(S3, Term, Precedence, Answer, S).
read('{', ['}'|S1], Precedence, Answer, S) :- !,
   read_rest0(S1, '{}', Precedence, Answer, S).
read('{', S1, Precedence, Answer, S) :- !,
   read(S1, 1200, Term, S2),
   expect('}', S2, S3), !,
   read_rest0(S3, '{}'(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- !,
   read_rest0(S0, 'S'(List), Precedence, Answer, S).

%%%  errors
read('}', S0, _, _, _) :- !,
   syntax_error(['"}" cannot start an expression'], S0).
read(']', S0, _, _, _) :- !,
   syntax_error(['"]" cannot start an expression'], S0).
read(')', S0, _, _, _) :- !,
   syntax_error(['")" cannot start an expression'], S0).
read(',', S0, _, _, _) :- !,
   syntax_error(['"," cannot start an expression'], S0).
read('|', S0, _, _, _) :- !,
   syntax_error(['"|" cannot start an expression'], S0).
read(_Token, S0, _, _, _) :-
%   syntax_error(['"',Token,'" cannot start an expression'], S0).
    syntax_error(['operator expected'], S0).

%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.
read_list([','|S1], [Term|Rest], S) :- !,
%   TermList = [Term|Rest],
   read(S1, 999, Term, S2), !,
   read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
   read(S1, 999, Rest, S2), !,
   expect(']', S2, S).
read_list([']'|S], [], S) :- !.
%   TermList = [],
%   S = S1.
read_list(S, _, _) :-
   syntax_error(['",", "|" or "]" expected in list'], S).

%%%  read_rest0(+Tokens, +Term, +Precedence, -Answer, -LeftOver)
%   Is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.  
read_rest0([], Term, _, Term, []).
read_rest0([F|S], Term, Prec, Answer, Left) :-
    read_rest0(F, S, Term, Prec, Answer, Left).

read_rest0('}', S, Term, _, Term, ['}'|S]).
read_rest0(']', S, Term, _, Term, [']'|S]).
read_rest0(')', S, Term, _, Term, [')'|S]).
read_rest0(',', S1, Term, Prec, Answer, S) :-
    ( Prec >= 1000 ->
	  read(S1, 1000, Next, S2), !,
	  read_rest(S2, 1000, (Term,Next), Prec, Answer, S)
    ; Answer = Term, S = [','|S1]
    ).
read_rest0('|', S1, Term, Prec, Answer, S) :-
    ( Prec >= 1100 ->        %% hard-wired op. prec. 1100 of |
	  read(S1, 1100, Next, S2), !,
	  read_rest(S2, 1100, '|'(Term,Next), Prec, Answer, S)
    ; S = ['|'|S1], Answer = Term
    ).
read_rest0(atom(F), S1, Term, Prec, Answer, S) :-
    ( ambigop(F, Prec, L1, O1, R1, L2, O2) ->
      ( prefix_is_atom(S1, Prec) ->
	    read_rest([postfixop(F,L2,O2) |S1], 0, Term, Prec, Answer, S)
      ; read_rest([infixop(F,L1,O1,R1)|S1], 0, Term, Prec, Answer, S)
      ; read_rest([postfixop(F,L2,O2) |S1], 0, Term, Prec, Answer, S)
      )
    ; infixop(F, L1, O1, R1) ->
	  read_rest([infixop(F,L1,O1,R1)|S1], 0, Term, Prec, Answer, S)
    ; postfixop(F, L2, O2) ->
	  read_rest([postfixop(F,L2,O2) |S1], 0, Term, Prec, Answer, S)
    ; syntax_error(['non-operator',F,'follows expression'], [atom(F)|S1])
    ).
read_rest0(Thing, S1, _, _, _, _) :-
    cant_follow_expr(Thing, Culprit) ->
	syntax_error([Culprit,'follows expression'], [Thing|S1]).

%   read_rest0(+Tokens, +LPrec, +Term, +Prec, -Answer, -LeftOver)
%   The tokens Tokens-LeftOver are parsed and combined with Term into 
%   Answer; LPrec and Prec are the lower and upper bounds for the
%   precedence of the topmost operator.
read_rest([F|R], LPrec, Term, Prec, Answer, S) :-
   read_rest(F, R, LPrec, Term, Prec, Answer, S), !.
read_rest(S, _, Term, _, Term, S).

read_rest(infixop(F,L,O,R), S1, LPrec, Term, Prec, Answer, S) :-
   Prec >= O, LPrec =< L, !,
   read(S1, R, Other, S2),
   !,
   functor(Expr,F,2),
   arg(1,Expr,Term),
   arg(2,Expr,Other),
   read_rest(S2, O, Expr, Prec, Answer, S).
read_rest(postfixop(F,L,O), S1, LPrec, Term, Prec, Answer, S) :-
   Prec >= O, LPrec =< L, !,
   functor(Expr,F,1),
   arg(1,Expr,Term),
   peepop(S1, S2), !,
   read_rest(S2, O, Expr, Prec, Answer, S).
read_rest(',', S1, LPrec, Term, Prec, Answer, S) :-
   Prec >= 1000, LPrec < 1000, !,
   read(S1, 1000, Next, S2), !,
   read_rest(S2, 1000, (Term,Next), Prec, Answer, S).
read_rest('|', S1, LPrec, Term, Prec, Answer, S) :-
   ( %infixop('|', P0, P, P1),   %% hard-wired op.prec.1100 of |
     (Prec >= 1100, LPrec =< 1099) ->
          read(S1, 1100, Next, S2),
	  read_rest(S2, 1100, '|'(Term, Next), Prec, Answer, S)
%    ; (Prec >= 1100, LPrec < 1100) ->
% 	  read(S1, 1100, Next, S2),
% 	  read_rest(S2, 1100, ';'(Term, Next), Prec, Answer, S)
   ; S = ['|'|S1], Term = Answer
   ).

cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(number(_),	number).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		parenthesis).
cant_follow_expr('(',		parenthesis).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		brace).

%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.
read_args([','|S1], [Term|Rest], S) :- !,
%   TermList = [Term|Rest],
   read(S1, 999, Term, S2), !,
   read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
%   TermList = [],
%   S = S1.
read_args(S, _, _) :-
   syntax_error(['"," or ")" expected in arguments'], S).

%   I experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   are interfaces to current_op.

:- dynamic 'PREFIXOP'/4,
	   'POSTFIXOP'/4,
	   'INFIXOP'/5,
	   'AMBIGOP'/8.

prefixop(Op, Prec, Prec1) :-
    ( 'PREFIXOP'(Op, Prec, Prec1, Answer) -> Answer
    ; ( current_op(Prec, fy, Op) -> Prec1 is Prec
      ; current_op(Prec, fx, Op) -> Prec1 is Prec-1
      ) -> assert('PREFIXOP'(Op, Prec, Prec1, true))
    ; assert('PREFIXOP'(Op, _, _, fail)), fail
    ).

postfixop(Op, Prec1, Prec) :-
    ( 'POSTFIXOP'(Op, Prec1, Prec, Answer) -> Answer
    ; ( current_op(Prec, yf, Op) -> Prec1 is Prec
      ; current_op(Prec, xf, Op) -> Prec1 is Prec-1
      ) -> assert('POSTFIXOP'(Op, Prec1, Prec, true))
    ; assert('POSTFIXOP'(Op, _, _, fail)), fail
    ).

infixop(Op, Prec0, Prec, Prec1) :-
    ( 'INFIXOP'(Op, Prec0, Prec, Prec1, Answer) -> Answer
    ; ( current_op(Prec, xfx, Op) -> Prec0 is Prec-1, Prec1 is Prec0
      ; current_op(Prec, xfy, Op) -> Prec0 is Prec-1, Prec1 is Prec
      ; current_op(Prec, yfx, Op) -> Prec1 is Prec-1, Prec0 is Prec
      ) -> assert('INFIXOP'(Op, Prec0, Prec, Prec1, true))
    ; assert('INFIXOP'(Op, _, _, _, fail)), fail
    ).
ambigop(Op, Prec, L1, Prec1, R1, L2, Prec2) :-
    ( 'AMBIGOP'(Op, Prec, L1, Prec1, R1, L2, Prec2, Answer) -> Answer
    ; ( postfixop(Op, L2, Prec2),
        Prec2 =< Prec,    
	infixop(Op, L1, Prec1, R1), 
	Prec1 =< Prec
      ) -> assert('AMBIGOP'(Op, Prec, L1, Prec1, R1, L2, Prec2, true))
    ; assert('AMBIGOP'(Op, Prec, L1, Prec1, R1, L2, Prec2, fail)), fail
    ).
      

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.
all_read([]) :- !.
all_read(S) :-
	syntax_error(['operator expected after expression'], S).

%   expect(Token, TokensIn, TokensOut)
%   parses the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.
expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error(['"',Token,'" or operator expected'], S0).

%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, -Ans, -LeftOver)
after_prefix_op(~, Oprec, Aprec, S0, Prec, Answer, S) :- !,
	 read(S0, Aprec, Arg, S1),
	 Term = '~'(Arg),
	 read_rest(S1, Oprec, Term, Prec, Answer, S).
after_prefix_op(Op, Oprec, Aprec, S0, Prec, Answer, S) :-
	r_op(S0, Flag),
	( Flag < 0 ->          % Op not prefix operator
	      read_rest0(S0, 'A'(Op), Prec, Answer, S)
	; Prec < Oprec ->
	      syntax_error(['prefix operator "', Op,
                            '" in context with precedence', Prec], S0)
	; Flag > 0 ->           % Op must be prefix operator
	      read(S0, Aprec, Arg, S1), !,
	      functor(Term,Op,1),
	      arg(1,Term,Arg),
	      read_rest(S1, Oprec, Term, Prec, Answer, S)
	; % Flag =:= 0,         %  Op might be atom
	  peepop(S0, S1),
	  prefix_is_atom(S1, Oprec), % can't cut but would like to
	  read_rest(S1, Oprec, Op, Prec, Answer1, S),
	  ( Op == Answer1 -> Answer = 'A'(Op)
	  ; Answer = Answer1
	  )
	; % Flag =:= 0,         %  Op is not an atom, it's an prefix operator
	  read(S0, Aprec, Arg, S1),
	  functor(Term,Op,1),
	  arg(1,Term,Arg),
	  read_rest(S1, Oprec, Term, Prec, Answer, S)
	).

%%%  r_op(+Tokens, -Flag)
% lookup for next token. 
% Flag = -1 => previous token (atom) is not an operator.
% Flag =  0 => previous token (atom) might be an operator.
% Flag =  1 => previous token (atom) have to be an operator.

r_op([], -1).
r_op([H|T], Flag) :-
    r_op(H, Flag, T).

r_op('(',       0, _).
r_op('[',       0, [']'|_]) :- !.
r_op(atom(_),   0, _).
r_op('{',       0, ['}'|_]) :- !.
r_op(var(_,_),  1, _).
r_op(number(_),	1, _).
r_op(string(_),	1, _).
r_op(' (',      1, _).
r_op('{',       1, _).
r_op('[',       1, _).
r_op('}',      -1, _).
r_op(',',      -1, _).
r_op('|',      -1, _).
r_op(')',      -1, _).
r_op(']',      -1, _).

%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).

%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Prec forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Prec) :-
	prefix_is_atom(Token, Prec).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- infixop('|',_,CP,_), CP >= P, !.
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to read(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.

% :- dynamic 'SYNTAX ERROR'/2.

syntax_error(Message, List) :-
	length(List, Length),
	asserta('SYNTAX ERROR'(Message,Length)), !,
	fail.

syntax_error(List, LineB, LineE) :-
	syntax_error([], 1000000, Msg, AfterError),
	trans_list(Msg, 999999, Msg0, []),
	append(Error, [_], Msg0), 
	cuf_out(error, ['(READER)'|Error]),
	get_flag(fileID, FID),
	file_table(check, FName, _, FID, _),
	cuf_out(message, ['in file:', FName]),
	cuf_out(message, ['between lines:', LineB-LineE]),
	length(List, Length),
	BeforeError is Length-AfterError,
	trans_list(List, BeforeError, ToPrint, []),
	cuf_out(message, ToPrint).

syntax_error(Msg0, AfterError0, Msg, AfterError) :-
	retract('SYNTAX ERROR'(Msg1,AfterError1)), !,
	( AfterError0 > AfterError1 ->
	      syntax_error(Msg1, AfterError1, Msg, AfterError)
	;   /* true */
	  syntax_error(Msg0, AfterError0, Msg, AfterError)
	).
syntax_error(Msg, AfterError, Msg, AfterError).

trans_list(X, 0) --> !,
	['<<HERE>>'],
	trans_list(X, 99999).
trans_list([Head|Tail], BeforeError) -->
	trans_token(Head),
	{ Left is BeforeError-1 },
	trans_list(Tail, Left).
trans_list([], _) --> ['$NL$'].

trans_token(atom(X))   --> !, [X].
trans_token(var(_V,X)) --> !, [X].
trans_token(number(X)) --> !, [X].
trans_token(string(X)) --> !, ['$S$'(X)].
trans_token(X)         --> [X].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: tokenizer for CUF                             %%%
%%%      Created: Fri Sep 18 13:23:26 1992                      %%%
%%%     Modified: Thu Mar 14 11:30:45 1996 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.3  1996/03/14 16:50:48  jochen
% made faster; eliminated uses of assert (flags entry_begin/end, '|'-handling)
% read_tokens0/3 (old: read_tokens/2) now returns line counts
%
% Revision 1.2  1994/02/10 13:49:47  jochen
% EOF handling improved
%
% Revision 1.1  1993/11/08  13:50:16  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Fri Mar 26 16:40:56 1993 (michel) : minor changes
% Fri Dec  4 16:10:28 1992 (michel) : escape_char/2 --> escape_char/3
% Fri Dec  4 14:44:11 1992 (michel) : error output changed
% Fri Dec  4 14:12:26 1992 (michel) : read_tokens0/3 changed for
%                                     better line count results
% Fri Dec  4 12:58:47 1992 (michel) : sccs_id/1 removed
% Mon Sep  7 15:10:33 1992 (michel) : read_tokens0/3 added (line counters)

%   Package: tokens
%   Author : Richard A. O'Keefe
%   Updated: 8/29/89
%   Defines: a public-domain tokeniser in reasonably standard Prolog.
/*
:- module(tokens, [
	read_tokens/1,
	read_tokens/2
   ]).
*/
/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

	%( is not accepted as an alternative to {

	%) is not accepted as an alternative to }

	NOLC convention is not supported (read_name could be made to do it)

	,.. is not accepted as an alternative to | (hooray!)

	large integers are not read in as xwd(Top18Bits,Bottom18Bits)

	After a comma, "(" is read as ' (' rather than '('.  This does the
	parser no harm at all, and the Dec-10 tokeniser's behaviour here
	doesn't actually buy you anything.  This tokeniser guarantees never
	to return '(' except immediately after an atom, yielding ' (' every
	other where.

    Some (KL-10, DEC-10 Prolog v3.53) times might be of interest.
    Applied to an earlier version of this file:
	this code took			1.66 seconds
	the Dec-10 tokeniser took	1.28 seconds
	A Pascal version took		0.96 seconds

    Some more (Sun-3/50M, Quintus Prolog 2.4.2) times.
	Test file		tokens.pl	read.pl		long.pl
	This code		5.25 sec	5.57 sec	15.02 sec
	Quintus tokeniser	3.07 sec	2.97 sec	 7.90 sec
	The "ppl" utility	0.40 sec	0.50 sec	 1.50 sec
	ppl + hsearch for ids	0.60 sec	0.70 sec	 2.20 sec
    The main factor explaining the greater speed of the Quintus tokeniser
    is that it reads most of the characters in C, thus making fewer trips
    across the C interface.  It turns out that even in the built-in
    tokeniser, the overhead from the existing I/O system is
	Get0 time		2.57 sec	2.43 sec	 6.60 sec
	Old I/O overhead	0.68 sec	0.68 sec	 1.85 sec
	C interface overhead	0.80 sec	1.25 sec	 3.25 sec
	New I/O total saving:	1.48 sec	1.93 sec	 5.10 sec
	This code MAY be	3.78 sec	3.64 sec	 9.92 sec
	Quintus code MAY be	2.39 sec	2.29 sec	 6.05 sec
	Quintus/ppl+hsearch	3.98 times	3.27 times	 2.74 times
    The savings for this code may be even greater, as a Prolog call and
    the corresponding stack frame creation will be avoided.  It is worth
    noting that this doesn't look as though it will be a good idea to
    replace the existing Quintus tokeniser.  The appropriate C comparison
    is ppl + hsearch (a hash table lookup) applied to atoms and variable
    names.  Given that these numbers are significant to 1 figure, if that,
    a factor of 3..4 times slower than C code doesn't sound too bad.
	
    These figures predict that the Quintus tokeniser with the future 
    new I/O code will be about 30% faster than it is now, and about
    1.6 times as fast as this tokeniser will be then.
    By a curious coincidence, read/1 is now about 1.6 times as fast as
    portable_read/1 is now.

    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The Pascal
    version is the program WLIST which lists "words" and their frequencies.
    It uses a hash table.  Another difference is the way characters are
    classified: the Dec-10 tokeniser and WLIST have a table which maps
    ASCII codes to character classes, and don't do all this comparison
    and and memberchking.  We could do that without leaving standard Prolog,
    but what do you want from one evening's work?

    Changes:
	integers can have underscores in them.
	characters in 0'x, "x", and 'x' can be escaped as in C or PopLog.
	So in 'foo\t%t\n' the \t and \n are now single characters.
	Radix notation is no longer exactly like DEC-10 Prolog v3.53.
	The radix may be any number of digits, e.g. 0016'deed.

    BEWARE: this file does _not_ recognise floating-point numbers.
    In order to make this file independent of whether character-escapes
    are enabled or not, the magic numbers
	9	(TAB)	and
	92	( \ )
    are used.
*/

% sccs_id('"@(#)89/08/29 tokens.pl    33.1"').


%   read_tokens(-TokenList)
%   returns a list of tokens.  The difference between it and
%   read_tokens/2 is that it doesn't bother to return the dictionary.
%   Now that read_tokens/2 sorts the dictionary, this can be a useful
%   saving.  Note that var(_,_) tokens still contain the variable name,
%   so it is possible to reconstruct the dictionary if you really need it.
/*
read_tokens(TokenList) :-
	get(C1),
	read_tokens(C1, _, ListOfTokens),
	!,
	TokenList = ListOfTokens.
read_tokens([atom(end_of_file)]).
*/
%   read_tokens(TokenList, Dictionary)
%   returns a list of tokens, and a dictionary of VarName=Variable pairs
%   in standard order, where the VarNames are atoms and the variables
%   are all the named variables among the tokens.
%   This predicate "primes" read_tokens/3 with the initial blank, and
%   checks for end of file.
%   The way end of file is handled is that everything else FAILS when it
%   hits character -1, sometimes printing a warning.  It might have been
%   an idea to return the atom 'end_of_file' instead of the same token list
%   that you'd have got from reading "end_of_file. ", but (1) this file is
%   for compatibility, and (b) there are good practical reasons for wanting
%   this behaviour.

read_tokens0(TokenList, Dictionary, Lines) :-
	on_exception(_,get(C1),fail),
	read_tokens0(C1, Dict, ListOfTokens, Lines),
	terminate_list(Dict),		%  fill in the "hole" at the end.
	!,				%  we have to unify explicitly so
	sort(Dict, Dictionary),		%  that we'll read and then check
	TokenList = ListOfTokens.	%  even with filled in arguments.
read_tokens0([atom(end_of_file)], [], _). %  End Of File is only problem.

terminate_list([]).
terminate_list([_|Tail]) :-
	terminate_list(Tail).

%  read_tokens0/3 skips over comments to the beginning of a clause.
%  So the line count gives better results for the entry information
%  (line of beginning and ending of a clause). (md)
read_tokens0(C1, Dict, Tokens, Lines) :-
    (   C1 =< " " ->			% layout: CR, LF, TAB, space, &c
	C1 >= 0,			% FAIL at end of file
	get(C2),
	read_tokens0(C2, Dict, Tokens, Lines)
    ;   C1 =:= "%" ->		                %  %comment
	repeat,					%  skip characters to any
	    get0(Ch),				%  line terminator
	    Ch < " ", Ch =\= 9 /*TAB*/,		%  control char, not tab
	!,					%  stop when we find one
	Ch \== -1,				%  fail on EOF
	get0(NextCh),
	read_tokens0(NextCh, Dict, Tokens, Lines)
    ;   C1 =:= "/" ->
	   get0(C2),
	   ( C2 =:= "*" ->			% /*comment*/
	       read_solidus(0' , NextCh),
	       read_tokens0(NextCh, Dict, Tokens, Lines)
	   ; /* C2 =\= "*" */			% begins symbol
	     current_input(Stream),
	     Lines = LineB-LineE,
	     line_count(Stream, LineB),
%	     set_flag(entry_begin, LineB),
	     rest_symbol(C2, Chars, NextCh),
	     read_after_atom(NextCh, Dict, Tokens, [0'/|Chars]),
	     line_count(Stream, LineE)%,
%	     set_flag(entry_end, LineE)
	   )
    ;   current_input(Stream),
	Lines = LineB-LineE,
	line_count(Stream, LineB),
%	set_flag(entry_begin, LineB),
	read_tokens(C1, Dict, Tokens), !,
	line_count(Stream, LineE)%,
%	set_flag(entry_end, LineE)
    ).

read_tokens(C1, Dict, Tokens) :-
    (	C1 =< " " ->			% layout: CR, LF, TAB, space, &c
	C1 >= 0,			% FAIL at end of file
	get(C2),
	read_tokens(C2, Dict, Tokens)
    ;	C1 >= "a", C1 =< "z" ->		% plain identifier
	read_identifier(C1, Dict, Tokens)
    ;	C1 >= "A", C1 =< "Z" ->		% variable name
	read_variable(C1, Dict, Tokens)
    ;	C1 >= "0", C1 =< "9" ->
	read_number(C1, Dict, Tokens)
    ;   C1 < 127 ->			% special character
	read_special(C1, Dict, Tokens)
    ;   C1 =< 160 ->			% DEL or unassigned control
	get(C2),
	read_tokens(C2, Dict, Tokens)
    ;   C1 >= 223, C1 =\= 247 ->	% ISO lower case letter
	read_identifier(C1, Dict, Tokens)
    ;	C1 >= 192, C1 =\= 215 ->	% ISO upper case letter
	read_variable(C1, Dict, Tokens)
    ;	C1 =\= 170, C1 =\= 186 ->	% ISO symbol char
	read_symbol(C1, Dict, Tokens)
    ;   				% _a_ or _o_ ordinal characters
	read_identifier(C1, Dict, Tokens)
    ).

read_special(0'_, Dict, Tokens) :-	% underscore; starts variables
	read_variable(0'_, Dict, Tokens).
read_special(247, Dict, Tokens) :-	% -:- (division sign)
	read_symbol(247, Dict, Tokens).
read_special(215, Dict, Tokens) :-	% x (multiplication sign)
	read_symbol(215, Dict, Tokens).
read_special(0'%, Dict, Tokens) :-		%  %comment
	repeat,					%  skip characters to any
	    get0(Ch),				%  line terminator
	    Ch < " ", Ch =\= 9 /*TAB*/,		%  control char, not tab
	!,					%  stop when we find one
	Ch \== -1,				%  fail on EOF
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'/, Dict, Tokens) :-		%  /*comment?
	get0(C2),
	(   C2 =:= "*" ->			% is /*comment*/
	    read_solidus(0' , NextCh),
	    read_tokens(NextCh, Dict, Tokens)
	;/* C2 =\= "*" */			% begins symbol
	    rest_symbol(C2, Chars, NextCh),
	    read_after_atom(NextCh, Dict, Tokens, [0'/|Chars])
	).
read_special(0'!, Dict, [atom(!)|Tokens]) :-	%  This is a special case so
	get0(NextCh),				%  that "!." is two tokens
	read_after_atom(NextCh, Dict, Tokens).	%  It could be cleverer.
read_special(0'(, Dict, [' ('|Tokens]) :-	%  NB!!!  "(" turns into
	get0(NextCh),				%  the token ' ('.
	read_tokens(NextCh, Dict, Tokens).
read_special(0'), Dict, [')'|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0',, Dict, [','|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0';, Dict, [atom(;)|Tokens]) :-	%   ; is not a punctuation
	get0(NextCh),				%   mark but an atom (e.g.
	read_after_atom(NextCh, Dict, Tokens).	%   you can :-op declare it).
read_special(0'[, Dict, ['['|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'], Dict, [']'|Tokens]) :-
	get0(NextCh),
	read_after_atom(NextCh, Dict, Tokens).
read_special(0'{, Dict, ['{'|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'|, Dict, ['|'|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'}, Dict, ['}'|Tokens]) :-
	get0(NextCh),
	read_after_atom(NextCh, Dict, Tokens).
read_special(0'., Dict, Tokens) :-		%  full stop
	get0(NextCh),				%  or possibly .=. &c
	read_fullstop(NextCh, Dict, Tokens).
read_special(0'", Dict, [string(Chars)|Tokens]) :-	%  "string"
	read_string(Chars, 0'", NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0''', Dict, Tokens) :-		%  'atom'
	read_string(Chars, 0''', NextCh),
	read_after_atom(NextCh, Dict, Tokens, Chars).
read_special(0'#, Dict, Tokens) :-
	read_symbol(0'#, Dict, Tokens).
read_special(0'$, Dict, Tokens) :-
	read_symbol(0'$, Dict, Tokens).
read_special(0'&, Dict, Tokens) :-
	read_symbol(0'&, Dict, Tokens).
read_special(0'*, Dict, Tokens) :-
	read_symbol(0'*, Dict, Tokens).
read_special(0'+, Dict, Tokens) :-
	read_symbol(0'+, Dict, Tokens).
read_special(0'-, Dict, Tokens) :-
	read_symbol(0'-, Dict, Tokens).
read_special(0':, Dict, Tokens) :-
	read_symbol(0':, Dict, Tokens).
read_special(0'<, Dict, Tokens) :-
	read_symbol(0'<, Dict, Tokens).
read_special(0'=, Dict, Tokens) :-
	read_symbol(0'=, Dict, Tokens).
read_special(0'>, Dict, Tokens) :-
	read_symbol(0'>, Dict, Tokens).
read_special(0'?, Dict, Tokens) :-
	read_symbol(0'?, Dict, Tokens).
read_special(0'@, Dict, Tokens) :-
	read_symbol(0'@, Dict, Tokens).
read_special( 92, Dict, Tokens) :-		% 92 is "\\"
	read_symbol( 92, Dict, Tokens).
read_special(0'^, Dict, Tokens) :-
	read_symbol(0'^, Dict, Tokens).
read_special(0'`, Dict, Tokens) :-
	read_symbol(0'`, Dict, Tokens).
read_special(0'~, Dict, Tokens) :-
	read_symbol(0'~, Dict, Tokens).


%   read_symbol(+C1, +Dict, -Tokens)
%   C1 is the first character of an atom made up of the following characters:
%	#$&*+-./:<=>?\^'~ (which are ASCII codes) and the ISO 8859/1 codes
%	215 (x) 247 (-:-) 161-169, 171-185, 187-191

read_symbol(C1, Dict, Tokens) :-
	get0(C2),
	rest_symbol(C2, Chars, NextCh),	% might read 0 chars
	read_after_atom(NextCh, Dict, Tokens, [C1|Chars]).


%   rest_symbol(+C2, -String, -NextCh)
%   reads the second and subsequence characters of an atom made up of
%   "symbol" characters.  It returns those characters as the list
%   String, and the following character as NextCh.  Note that it need
%   not read any characters at all, e.g. Ch might be " ".

rest_symbol(C2, [C2|Chars], LastCh) :-
	(   C2 > 160 -> C2 < 192, C2 =\= 186, C2 =\= 170
	;   symbol_char(C2)
	),
	!,
	get0(NextCh),
	rest_symbol(NextCh, Chars, LastCh).
rest_symbol(C2, [], C2).

symbol_char(0'#).
symbol_char(0'$).
symbol_char(0'&).
symbol_char(0'*).
symbol_char(0'+).
symbol_char(0'-).
symbol_char(0'.).	% yes, +./* is a legal atom
symbol_char(0'/).
symbol_char(0':).
symbol_char(0'<).
symbol_char(0'=).
symbol_char(0'>).
symbol_char(0'?).
symbol_char(0'@).
symbol_char(92 /* \ */).
symbol_char(0'^).
symbol_char(0'`).	% CHAT-80 uses `` as an atom.
symbol_char(0'~).
symbol_char(215).  % x
symbol_char(247).  % -:-


read_after_atom(Ch, Dict, [atom(Atom)|Tokens], Chars) :-
	( Ch < 0 ->
	      cuf_out(error, ['(SCANNER) unexpected end of file']),
	      fail
	; atom_chars(Atom, Chars),
	  read_after_atom(Ch, Dict, Tokens)
	).

%   The only difference between read_after_atom(Ch, Dict, Tokens) and
%   read_tokens/3 is what they do when Ch is "(".  read_after_atom
%   finds the token to be '(', while read_tokens finds the token to be
%   ' ('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol application.
%   See the public-domain library file READ.PL for details.

read_after_atom(0'(, Dict, ['('|Tokens]) :- !,
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_after_atom(Ch, Dict, Tokens) :-
	read_tokens(Ch, Dict, Tokens).




%   read_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate FAILS.
%   It does not return any special structure.  That is the only reason
%   it can ever fail.  The other complication is that when we find a Quote
%   we have to look ahead one character in case it is doubled.  Note that
%   if we find an end-of-file after the quote we *don't* fail, we return
%   a normal string and the end of file character is returned as NextCh.
%   If we were going to accept C-like escape characters, as I think we
%   should, this would need changing (as would the code for 0'x).  But
%   the purpose of this module is not to present my ideal syntax but to
%   present something which will read present-day Prolog programs.

read_string(Chars, Quote, NextCh) :-
	get0(Ch),
	read_char(Ch, Quote, Char, Next),
	rest_string(Char, Next, Chars, Quote, NextCh).


rest_string(-1, NextCh, [], _, NextCh) :- !.		% string ended
rest_string(Char, Next, [Char|Chars], Quote, NextCh) :-
	read_char(Next, Quote, Char2, Next2),
	rest_string(Char2, Next2, Chars, Quote, NextCh).


%   read_char(C1, Quote, Char, C2)
%   reads a single `character' from a string, quoted atom, or character
%   constant.  C1 is the first character it is to look at, and has been
%   read already.  Quote is the surrounding quotation mark, which is "
%   for strings, ' for quoted atoms, and the radix character (also ')
%   for character constants.  A Dec-10 Prolog incompatibility is that
%   it does not allow newline characters in strings unless they are
%   preceded by an escape character.  As reading an extended character
%   would sometimes read one character too many, it is made to do so
%   always, and to return the first character which does not belong in
%   the character as C2.  When we have hit the end of the string, we
%   return Char = -1 (which does not necessarily mean that we have hit
%   the end of the source file, look at C2 for that).

read_char(Char, Quote, Result, Next) :-
    (	Char =:= 92, /* \ */ prolog_flag(character_escapes,on) ->
	get0(C1),
	(   C1 < 0 ->
		% format(user_error, '~N** end of file in ~cquoted~c~n',
		%        [Quote,Quote]),
		atom_chars(C, [Quote,113,117,111,116,101,100,Quote]),
		cuf_out(error, ['(SCANNER) end of file in',C]),
		Result = -1, Next = C1
	;   C1 =< " " ->
		/* \<layout> is skipped */
		get0(C2),
		read_char(C2, Quote, Result, Next)
	;   C1\/32 =:= "c" ->
		/* \c<layout>* is skipped; to get a blank after this */
		/* do e.g. "...\c      \ <space>" where the "\ " ends */
		/* the skipping and the NEXT blank is taken.  */
		get(C2),
		read_char(C2, Quote, Result, Next)
	;   C1 =< "7", C1 >= "0" ->
		/* \<1-3 octal digits> */
		/* hairy bit: \1234 is S4 */
		get0(C2),
		(   C2 =< "7", C2 >= "0" ->
		    get0(C3),
		    (   C3 =< "7", C3 >= "0" ->
			get0(Next),
			Result is (C1*8+C2)*8+C3 - 73*"0"
		    ;   Next = C3,
			Result is (C1*8+C2) - 9*"0"
		    )
		;   Next = C2,
		    Result is C1-"0"
		)
	;   C1 =:= "^" ->
		get0(C2),
		(   C2 < 0 ->
		    % format(user_error, '~N** end of file in ~c..~c^..~c~n',
		    %        [Quote,92 /* \ */,Quote]),
		    atom_chars(C, [Quote,46,46,92,94,46,46,Quote]),
		    cuf_out(error, ['(SCANNER) end of file in', C]),
         	    Result = -1, Next = C2
		;   C2 =:= "?" ->
		    Result = 127,	% \^? = DEL
		    get0(Next)
		;   Result is C2/\31,	% \^X -> control-X
		    get0(Next)
		)
	;   escape_char(C1, Result, _) ->
		get0(Next)
	;   /* otherwise */
		Result = C1,		% probably "'", '"',  or \ itself
		get0(Next)
	)

    ;	Char =:= Quote ->
	get0(Ch),
	(   Ch =:= Quote ->
	    Result = Quote,
	    get0(Next)
	;   Result = -1, Next = Ch
	)

    ;	Char < " ", Char =\= 9, /*TAB */
        prolog_flag(character_escapes,on)
    ->	Result = -1, Next = Char,
	% format(user_error, '~N** Strange character ~d ends ~ctoken~c~n',
	%        [Char, Quote, Quote])
	( escape_char(_, Char, CharS) -> true ; Char = CharS),
	atom_chars(C, [Quote,116,111,107,101,110,Quote]),
	cuf_out(error, ['(SCANNER) strange character '(CharS),ends,C])
    ;
	Result = Char,
	get0(Next)
    ).

%  This table is for ASCII.  On Xerox Lisp systems, \n maps to
%  13 (CR).  The whole table needs replacing in EBCDIC systems,
%  in which the assumption that A..Z and a..z are contiguous
%  blocks also needs correcting.

escape_char(0'a,  7, 'audible alarm').          % \a = BEL =^G
escape_char(0'A,  7, 'audible alarm').          % \A = BEL =^G
escape_char(0'b,  8, 'backspace').		% \b = Backspace
escape_char(0'B,  8, 'backspace').		% \B = Backspace
escape_char(0'd,127, 'delete').                 % \d = Delete
escape_char(0'D,127, 'delete').                 % \D = Delete
escape_char(0'e, 27, 'escape').                 % \e = Escape
escape_char(0'E, 27, 'escape').                 % \E = Escape
escape_char(0'f, 12, 'form feed').		% \f = FormFeed
escape_char(0'F, 12, 'form feed').		% \F = FormFeed
escape_char(0'n, 10, 'new line').		% \n = NewLine
escape_char(0'N, 10, 'new line').		% \N = NewLine
escape_char(0'r, 13, 'return').		        % \r = Return
escape_char(0'R, 13, 'return').		        % \R = Return
escape_char(0's, 32, 'space').                  % \s = visible Space
escape_char(0'S, 32, 'space').                  % \S = visible Space
escape_char(0't,  9, 'tabular').		% \t = Tab
escape_char(0'T,  9, 'tabular').		% \T = Tab
escape_char(0'v, 11, 'vertical tabular').	% \v = Vertical tab
escape_char(0'V, 11, 'vertical tabular').	% \V = Vertical tab
escape_char(0'z, -1, 'end or file').		% \z = end of file
escape_char(0'Z, -1, 'end or file').		% \Z = end of file



%   read_variable(+C1, +Dict, -Tokens)
%   C1 is the first character of a variable name.  If the whole
%   variable name is "_", this is an anonymous variable, not identical
%   to any other variable.  Otherwise, the variable and its name are
%   looked up in (or added to) the dictionary, which is an improper list.
%   This is the only place that read_lookup/2 is called.

read_variable(C1, Dict, [var(Var, Name)|Tokens]) :-
	read_name(C1, Chars, NextCh),
	atom_chars(Name, Chars),
	(   Name == '_' -> true
	;   read_lookup(Dict, Name, Var)
	),
	read_after_atom(NextCh, Dict, Tokens).

read_lookup([N=V|L], Name, Var) :-
	(   N = Name -> V = Var
	;   read_lookup(L, Name, Var)
	).


%   read_solidus(+Ch, -LastCh)
%   is called when we have read the "/" and "*" that open a PL/I-style
%   comment.  It skips the rest of the comment.  We have to take great
%   care to handle end of file inside a comment; if the end-of-file is
%   is reported, we return -1 as LastCh,  while a space is returned if
%   the "*" and "/" that terminate the comment are found, and the next
%   character is left unread.  That might be changed.

read_solidus(Ch, LastCh) :-
    (	Ch =:= 0'* ->		% maybe end of comment
	get0(NextCh),
	(   NextCh =:= 0'/ ->	% end of comment*/ found
	    get(LastCh)		% skip over any layout following
	;   read_solidus(NextCh, LastCh)
	)
    ;	Ch =\= -1 ->		% ordinary comment character
	get0(NextCh),
	read_solidus(NextCh, LastCh)
    ;				% end of file
	LastCh = Ch,
%	format(user_error, '~N** end of file in /* comment~n',[])
	cuf_out(error, ['(SCANNER) end of file in /*comment'])
    ).


%   read_identifier(+C1, +Dict, -Tokens)
%   reads an atom which begins with a lower case letter C1 and
%   continues with letters, digits, and underscores.

read_identifier(C1, Dict, Tokens) :-
	read_name(C1, Chars, NextCh),
	read_after_atom(NextCh, Dict, Tokens, Chars).


%   read_name(+C1, -Chars, -LastCh)
%   reads a sequence of letters, digits, and underscores, where the
%   last character read was C1 and it is known that C1 is to be
%   included in the result.  The desired characters are returned as
%   the list Chars, and the next character as LastCh.
%   This version has been tuned, oy, has it been tuned!
%   A table-driven version is nearly as fast in Prolog.

read_name(C1, [C1|Chars], LastCh) :-
    get0(C2),
    (   C2 >= "a" ->
	(	C2 =< "z" ->			% ASCII lower case letter
	    read_name(C2, Chars, LastCh)
	;   C2 < 192, C2 \/ 16 =\= 186 ->	% {|}~, ISO 8859/1 symbols
	    Chars = [], LastCh = C2
	;   C2 \/ 32 =:= 247 ->		% times or divide-by chars
	    Chars = [], LastCh = C2
	;					% ISO 8859/1 top letters
	    read_name(C2, Chars, LastCh)
	)
    ;   C2 >= "A" ->
	(	C2 > "Z", C2 =\= "_" ->		% [\]^`
	    Chars = [], LastCh = C2
	;					% ASCII upper case or "_"
	    read_name(C2, Chars, LastCh)
      )
    ;   (	C2 >= "0", C2 =< "9" ->		% ASCII digits
	    read_name(C2, Chars, LastCh)
	;					% other characters
	    Chars = [], LastCh = C2
	)
    ).


%   read_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%	(a) the next character is an end of file.  We treat this
%	    as an unexpected end of file.  The reason for this is
%	    that we HAVE to handle end of file characters in this
%	    module or they are gone forever; if we failed to check
%	    for end of file here and just accepted .<EOF> like .<NL>
%	    the caller would have no way of detecting an end of file
%	    and the next call would abort.
%	(b) the next character is a layout character.  This is a
%	    clause terminator.
%	(c) the next character is anything else.  This is just an
%	    ordinary symbol and we call read_symbol to process it.

read_fullstop(Ch, Dict, Tokens) :-
	(   Ch =< "9", Ch >= "0" ->
	    Tokens = [number(Number)|Tokens1],
	    read_float(Number, Dict, Tokens1, "0", Ch)
	;   Ch > " " ->		% ordinary token starting with "."
	    rest_symbol(Ch, Chars, NextCh),
	    read_after_atom(NextCh, Dict, Tokens, [0'.|Chars])
	;   %% Ch >= 0 ->		% END OF CLAUSE
	    Tokens = []
% 	;			% END OF FILE
% %	    format(user_error, '~N** end of file just after full stop~n',[]),
% 	    cuf_out(error, ['(SCANNER) end of file just after full stop']),
% 	    fail
	).



%   read_float(N, C, Dict, Tokens)
%   is called when we have parsed <digit>* "." <digit>; N is the integer
%   value of the characters preceding the decimal point, and C is the
%   first digit after the decimal point.

read_float(Number, Dict, Tokens, Digits, Digit) :-
	prepend(Digits, Chars, Rest),
	read_float(Digit, Rest, NextCh, Chars),
	number_chars(Number, Chars),
	read_tokens(NextCh, Dict, Tokens).

prepend([]) --> ".".
prepend([C|Cs]) --> [C], prepend(Cs).

read_float(C1, [C1|Chars], NextCh, Total) :-
	get0(C2),
	(   C2 >= "0", C2 =< "9" ->
	    read_float(C2, Chars, NextCh, Total)
	;   C2\/32 =:= "e" ->
	    get0(C3),
	    (   C3 =:= "-" -> get0(C4), Chars = [C2,0'-|More]
	    ;   C3 =:= "+" -> get0(C4), Chars = [C2|More]
	    ;		      C4 = C3,  Chars = [C2|More]
	    ),
	    (   C4 >= "0", C4 =< "9" ->
		read_exponent(C4, More, NextCh)
	    ;   More = "",
	        % format(user_error, '~N** Missing exponent in ~s~n', [Total]),
	        number_chars(N, Total),
		cuf_out(error, ['(SCANNER) missing exponent in', N]),
		fail
	    ;   More = "0", NextCh = C4
	    )
	;   Chars = [], NextCh = C2
	).

read_exponent(C1, [C1|Chars], NextCh) :-
	get0(C2),
	(   C2 >= "0", C2 =< "9" ->
	    read_exponent(C2, Chars, NextCh)
	;   Chars = [], NextCh = C2
	).


%   read_number(+C1, +Dict, -Tokens)
%   C1 is the digit which begins the number.

read_number(C1, Dict, [number(Number)|Tokens]) :-
	read_number(C1, C2, 0, N),
	(   C2 =:= 0''' ->
	    (   N >= 2, N =< 36 ->
		read_based(N, 0, Number, C)
	    ;   N =:= 0 ->
		get0(C3),
		read_char(C3, -1, Number, C)
	    ;   % format(user_error, '~N** ~d'' read as ~d ''~n', [N,N]),
	        number_chars(N, Digits),
		append(Digits, [39], A1S),
		append(Digits, [32,39], A2S),
		atom_chars(A1, A1S),
		atom_chars(A2, A2S),
	        cuf_out(error, ['(SCANNER)',A1,'read as',A2]),
		Number = N, C = C2
	    ),
	    read_tokens(C, Dict, Tokens)
	;   C2 =:= 0'. ->
	    get0(C3),
	    (   C3 >= "0", C3 =< "9" ->
		number_chars(N, Digits),
		read_float(Number, Dict, Tokens, Digits, C3)
	    ;   Number = N,
		read_fullstop(C3, Dict, Tokens)
	    )
	;   Number = N,
	    read_tokens(C2, Dict, Tokens)
	).


%   read_number(+C0, -C, +N0, -N)
%   read a decimal integer.

read_number(C0, C, N0, N) :-
    (	C0 >= "0", C0 =< "9" ->
	N1 is N0*10 - "0" + C0,
	get0(C1),
	read_number(C1, C, N1, N)
    ;   C0 =:= 0'_ ->
	get0(C1),
	read_number(C1, C, N0, N)
    ;   C = C0, N = N0
    ).


%   read_based(+Base, +N0, -N, -LastCh)
%   read an integer in base Base.

read_based(Base, N0, N, C) :-
	get0(C1),
	(   C1 >= "0", C1 =< "9" -> Digit is C1-"0"
	;   C1 >= "A", C1 =< "Z" -> Digit is C1-("A"-10)
	;   C1 >= "a", C1 =< "z" -> Digit is C1-("a"-10)
	;   Digit is 99
	),
	(   Digit < Base ->
	    N1 is N0*Base + Digit,
	    read_based(Base, N1, N, C)
	;   C1 =:= "_" ->
	    read_based(Base, N0, N, C)
	;   N = N0, C = C1
	).






%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.3  1993/12/23 13:52:25  michel
% grammar_files/2 and control_files/2 facts removed
%
% Revision 1.2  1993/11/29  13:14:47  jochen
% loading of library(environ) now in quintus/sicstus.pl
%
% Revision 1.1  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%

init_grammar_conf :-
	environ('CUF_GRAMMAR_CNF',GConf),
	( cuf_abs_file_name(GConf,X,cnf,read) ->
	      abolish(grammar_files/2),
	      abolish(control_files/2),
	      abolish(current_grammar/1),
	      compile(X)
	; cuf_abs_file_name(GConf,X,cuf,read),
	  abolish(grammar_files/2),
	  abolish(control_files/2),
	  abolish(current_grammar/1),
	  assert(grammar_files(default,[X])),
	  assert(current_grammar(default))
	), !.
init_grammar_conf.


%% foreign.pl for SICSTUS-Prolog Version 3

%% foreign.pl: to be processed with cpp
%%             generates Prolog-implementation specific code

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.7  1995/11/15  13:01:26  junger
% adapted for the integration of 1 3
%
% Revision 1.6  1995/07/10  09:08:09  jochen
% tt_hash added
%
% Revision 1.5  1994/05/20  13:41:50  junger
% cosmetics (JD)
%
% Revision 1.4  1994/05/19  14:14:31  jochen
% nochmal wg. counter.o
%
% Revision 1.3  1994/05/19  13:33:38  junger
% - counter.o wird richtig geladen
%
% Revision 1.2  1994/05/18  10:28:05  junger
% - loading of foreign file counter.so for not_twice counter added
%
% Revision 1.1  1994/03/25  14:10:42  jochen
% Changes for new Quintus/SICStus handling
% now generic file for former f_{quintus,1}.pl
%


%% Foreign functions - window system independent

%% functions for not_twice-predicate in expand_det1.pl

foreign_file('../../foreign/counter',[reset_counter,incr_counter,get_counter]).
foreign(reset_counter, c, reset_counter).
foreign(incr_counter, c, incr_counter([-integer])).
foreign(get_counter, c, get_counter([-integer])).
:- load_foreign_files('../../foreign/counter',[]).
%:- abolish(foreign_file, 2).
%:- abolish(foreign, 3).

% maketemp.c is needed for interface to xmfed

foreign_file('../../foreign/maketemp', [cuf_mktemp]).




%% Sicstus needs to know exact sizeof string

foreign(cuf_mktemp, c, cuf_mktemp(+string, [-string(256)])).


:- load_foreign_files('../../foreign/maketemp',[]).

%#ifdef quintus
%:- abolish(foreign_file, 2).
%:- abolish(foreign, 3).
%#endif quintus


%% Sicstus needs another foreign file for file-access-functions
foreign_file('../../foreign/filestatus', [cuf_newer,cuf_file_exists,cuf_file_mtime]).

foreign(cuf_file_mtime, c, cuf_file_mtime(+string, [-integer])).
foreign(cuf_newer, c, cuf_newer_file(+string, +string, [-integer])).
foreign(cuf_file_exists, c, cuf_file_exists(+string, [-integer])).

:- load_foreign_files(['../../foreign/filestatus'], []).

%:- abolish(foreign_file, 2).
%:- abolish(foreign, 3).


cuf_newer_file(Old, New) :-
    cuf_newer_file(Old, New, 0). 

cuf_file_exists(File) :-
    cuf_file_exists(File, 0).

cuf_environment(Name, Value) :-
    cuf_env(Name, Value),
    cuf_not_eq(Value, 'no value').


%% tt_hash.o resides in SP/QP-dependent directory
foreign_file('../obj/tt_hash',[cuf_type_term_hash,cuf_type_term_hash2]).

foreign(cuf_type_term_hash, c, cuf_type_term_hash(+term,[-integer])).
foreign(cuf_type_term_hash2, c, cuf_type_term_hash2(+term,+term,[-integer])).
:- load_foreign_files('../obj/tt_hash', []).
:- abolish([foreign/3, foreign_file/2]).






%%RB :- ensure_loaded('atomsat-pl-t').
%%RB :- load_foreign_files('../obj/atomsat-pl', []).
:- abolish([foreign/3, foreign_file/2]).











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Michael Dorna                                 %%%
%%%      Purpose: conversion of internal type term format for pp %%
%%%      Created: Fri Jul  9 11:56:48 1993                      %%%
%%%     Modified: Mon Jun 12 09:49:38 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.11  1995/06/12 08:10:29  jochen
% bug fix in minimize_out/3 and minimize_new/6; now able to print out
% '.'
%
% Revision 1.10  1994/06/21  13:33:23  michel
% * minimize_ds_new/4 is called now for every formula;
%   it reduces now if (t <--> T), (t ; T) to T
%
% Revision 1.9  1994/05/02  14:54:34  michel
% bug fix in minimize_new/6: functor/3 calls added for different
%                            cases of check_formulas/3 calls
%
% Revision 1.8  1994/01/29  15:18:15  michel
% minimize_ds_new/2: length > 2 condition added (bug fix)
%
% Revision 1.7  1994/01/29  10:13:27  michel
% minimize_tt/6: second clause, minimize_ds_new/4 call in case statement now
%
% Revision 1.6  1994/01/18  12:00:41  jochen
% using negate_TID/2 now
%
% Revision 1.5  1994/01/14  11:10:50  michel
% Bug fixing: top --> (top,TID) at several places in this file
%
% Revision 1.4  1994/01/14  08:57:27  jochen
% new type encoding for undeclared constants  included
%  -> number(N), string(S), afs_symbol(A)
%
% Revision 1.3  1993/12/17  15:39:37  michel
% changes in almost every predicate in this file.
% type term minimization with subsumption test now.
% new predicates: minimize_new/{2,3,4,5,6},
% 	        minimize_ds_new/{2,4},
% 	        minimize_new_ds/{2,4},
% 	        minimize_out/3,find_ids/2,ds2tt/2
%
% Revision 1.2  1993/12/08  15:24:42  michel
% bug fix
%
% Revision 1.1  1993/11/08  13:49:43  jochen
% check in cuf2.28 (initial check in)
%


% History (before RCS):
% Fri Jul  9 14:41:04 1993 (dorna) : totally new implementation

%%%  cnf2type_term(+InternalTypeTerm, -ExternalTT)
%%%  Transforms type terms in internal format to a cuf type term.
%%%  If a type is not defined (it was generated during compilation),
%%%  the type will be named 'top'.
%%%  The transformation includes a symbolic minimization as well as
%%%  subsumption tests to reduce the type terms.
%%%  The InternalTypeTerm is assumed to be a satisfiable formula!!!
%%%  I.e. it is assumed to  be part of the output of a CUF prove.
cnf2type_term(Var, top) :- var(Var), !.
cnf2type_term(TTIn, TTOut) :-
	cnf2type_term(TTIn, Ds, [], PosCs, [], NegCs, []), !,
	% sort(Ds, DsS), sort(NegDs, NegDsS), sort(PosCs, PosCsS),
	minimize_tt(Ds, PosCs, NegCs, TTOut).

%%%  cnf2type_term(+InternalTypeTerm, -DisjsOut, +DisjIn,
%%%                -PosConjsOut, +PosConjsIn, -NegConjsOut, +NegConjsIn)
%%%  Splits the type term for later minimization.
%%%  Translates internal type encoding into external the type symbol.
cnf2type_term([], Ds, Ds, PosCs, PosCs, NegCs, NegCs).
cnf2type_term([Disj|R], DsO, DsI, PosCsO, PosCsI, NegCsO, NegCsI) :-
	disj2type_term(Disj, Ds1, DsI, PosCs1, PosCsI, NegCs1, NegCsI),
	cnf2type_term(R, DsO, Ds1, PosCsO, PosCs1, NegCsO, NegCs1).

disj2type_term([TypeCode], Ds, Ds, PosCsO, PosCsI, NegCsO, NegCsI) :- !,
	get_type_name(TypeCode, PosC, NegC),
        match_type(PosC, NegC, PosCsO, PosCsI, NegCsO, NegCsI).
disj2type_term(Ds, DsO, DsI, PosCsO, PosCsI, NegCsO, NegCsI) :-
        disj2type_term(Ds, PosDs, [], NegDs, []),
        minimize_disj(PosDs, NegDs, DsO, DsI, PosCsO, PosCsI, NegCsO, NegCsI).

disj2type_term([], PosDs, PosDs, NegDs, NegDs).
disj2type_term([TypeCode|R], PosDsO, PosDsI, NegDsO, NegDsI) :-
	get_type_name(TypeCode, Pos, Neg),
        match_type(Pos, Neg, PosDs1, PosDsI, NegDs1, NegDsI),
	disj2type_term(R, PosDsO, PosDs1, NegDsO, NegDs1).

get_type_name(number(N), (N,number(N)), 'N O') :- !.
get_type_name(string(N), (N,string(N)), 'N O') :- !.
get_type_name(afs_symbol(N), (N,afs_symbol(N)), 'N O') :- !.
get_type_name(-Term, 'N O', Neg) :-  %% only ofr -number(N), -string(S) ...
	get_type_name(Term, Neg, _), !.
get_type_name(TypeCode, (Type,TypeCode), 'N O') :-
	type_code(TypeCode, Type), !.
get_type_name(TypeCode, 'N O', (Type,TypeCode1)) :-
	TypeCode1 is 0-TypeCode,
	type_code(TypeCode1, Type), !.
get_type_name(_TypeCode, (top,[[1,-1]]), 'N O').

match_type('N O', Neg, Poss, Poss, [Neg|Negs], Negs) :- !.
match_type(Pos, 'N O', [Pos|Poss], Poss, Negs, Negs).

minimize_disj([], NegDs, DsO, DsI, PosCs, PosCs, NegCsO, NegCsI) :- !,
       sort(NegDs, NegL),
       ( NegL = [Neg] -> 
	     match_disj(DsO, DsI, Neg, NegCsI, NegCsO)
       ; match_disj(NegCsO, NegCsI, d([], NegL), DsI, DsO)
       ).
minimize_disj(PosDs, [], DsO, DsI, PosCsO, PosCsI, NegCs, NegCs) :- !,
       sort(PosDs, SPosDs),
       ( SPosDs = [Pos] ->
	     match_disj(DsO, DsI, Pos, PosCsI, PosCsO)
       ; memberchk((top,TID), SPosDs) ->               % top ; T --> top
             match_disj(DsO, DsI, (top,TID), PosCsI, PosCsO)
       ; match_disj(PosCsO, PosCsI, d(SPosDs, []), DsI, DsO)
       ).
minimize_disj(PosDs, NegDs, DsO, DsI, PosCsO, PosCsI, NegCs, NegCs) :-
       sort(PosDs, SPosDs),
       sort(NegDs, SNegDs),
       ( member(X, SPosDs), memberchk(X, SNegDs) ->     % t ; ~t --> top
	     match_disj(DsO, DsI, (top,[[1,-1]]), PosCsI, PosCsO)
       ; memberchk((top,TID), SPosDs) ->                % top ; T --> top
             match_disj(DsO, DsI, (top,TID), PosCsI, PosCsO)
       ; match_disj(PosCsO, PosCsI, d(SPosDs, SNegDs), DsI, DsO)
       ).

match_disj(Xs, Xs, Elem, In, [Elem|In]).

%%%  minimize_tt(+Ds, +PosCs, +NegCs, -MinimizedTT)
%%%  Replacing type terms by the following rules:
%%%  top & T --> T
%%%  ~t & (t ; T) --> ~t & T
%%%  t & (~t ; T) --> t & T
minimize_tt([], PosCs, NegCs, TTOut) :- !,
	minimize_end([], PosCs, NegCs, TTOut).
minimize_tt(Ds, PosCs, NegCs, TTOut) :- 
	minimize_tt(Ds, DsO, PosCs, PosCsO, NegCs, NegCsO),
	( Ds == DsO ->               % stop recursion
	      minimize_end(Ds, PosCsO, NegCsO, TTOut)
	; minimize_tt(DsO, PosCsO, NegCsO, TTOut)
	).

minimize_end(Ds, PosCs, NegCs, TTOut) :-
	ds2list(Ds, DsL),
	( NegCs == [] ->
	      NegL = [], MFlag = no
	; sort(NegCs, NegCsS),
	  neg_list(NegCsS, NegL),
	    MFlag = yes
	),
	( PosCs == [] ->
	      PosL =[]
	; sort(PosCs, PosCsS),
	  minimize_conj(PosCsS, PosL, MFlag)
	),
	append(PosL, NegL, PNL),
	append(PNL, DsL, IDs),
	minimize_new(IDs, NewIDs),
	convertIDs(NewIDs,List),
	sort(List,SList),
	list2tt(SList, TTOut).

minimize_tt([], [], PosCs, PosCs, NegCs, NegCs).
minimize_tt([d(PosDs, NegDs)|R], DsOut, PosCsI, PosCsO, NegCsI, NegCsO) :-
%       ( NegCsI == [] ->
	     minimize_ds_new(PosDs, NegDs, PosDs1, NegDs1),
%       ; PosDs=PosDs1, NegDs=NegDs1
%       ),
       minimize_ds(PosDs1, PosDsO, NegDs1, NegDsO,
                   PosCsI, PosCs1, NegCsI, NegCs1, Flag),
       ( Flag == del ->
	     DsOut = R, PosCs1 = PosCsO, NegCs1 = NegCsO
       ; Flag == new ->
             DsOut = [d(PosDsO, NegDsO)|R],
	     PosCs1 = PosCsO, NegCs1 = NegCsO
       ; /* Flag == same -> */
             DsOut = [d(PosDsO, NegDsO)|RR],
	     minimize_tt(R, RR, PosCs1, PosCsO, NegCs1, NegCsO)
       ).

%%  minimize_ds_new/4 was necessary to reduce type terms generated
%%  by polyfeature constraints in an optimal way. It has to be called
%%  before minimize_ds/9!
minimize_ds_new([], NegDsI, [], NegDsO) :- !,
	minimize_ds_new(NegDsI, NegDsO).
minimize_ds_new(PosDsI, [], PosDsO, []) :- !,
	minimize_ds_new(PosDsI, PosDsO).
minimize_ds_new(PosDs, NegDs, PosDs, NegDs).

%%  realizes (t <--> T) --> (t ; T) --> T
minimize_ds_new(IDs, DsOut) :-
	length(IDs,L), L > 2,
	delete((_,TID),IDs,IDsR),
	ds2tt(IDsR, TT),
%	check_formulas([TT],[[TID]],1), !,
	type_subsumes([TT],[[TID]],1),
	type_subsumes([[TID]],[TT],1), !,
	minimize_ds_new(IDsR, DsOut).
minimize_ds_new(Ds, Ds).

minimize_ds([], [], NegDs, NegDsO, PosCs, PosCs, NegCsI, NegCsO, Flag) :- !,
       minimize_NegDs(NegDs, NegDsO, PosCs, NegCsI, NegCsO),
       ( NegDsO == [] ->
	    Flag = del
       ; NegCsO == NegCsI ->
	    Flag = same
       ; Flag = new
       ).
minimize_ds(PosDs, PosDsO, [], [], PosCsI, PosCsO, NegCs, NegCs, Flag) :- !,
       minimize_PosDs(PosDs, PosDsO, PosCsI, PosCsO, NegCs),
       ( PosDsO == [] ->
	    Flag = del
       ; PosCsO == PosCsI ->
	    Flag = same
       ; Flag = new
       ).
minimize_ds(PosDsI, PosDsO, NegDsI, NegDsO, PosCs, PosCs, NegCs, NegCs,
	    Flag) :-
       ( delete(D, PosDsI, PosDsO), memberchk(D, NegCs) ->
             NegDsO = NegDsI, Flag = new
       ; delete(D, NegDsI, NegDsO), memberchk(D, PosCs) ->
             PosDsO = PosDsI, Flag = new
       ; PosDsO = PosDsI, NegDsO = NegDsI, Flag = same
       ).

minimize_NegDs([D], [], _PosCs, NegCs, [D|NegCs]) :- !.
minimize_NegDs(NegDsI, NegDsO, PosCs, NegCsI, NegCsO) :-
       delete(D, NegDsI, NegDsR),
       memberchk(D, PosCs), !,
       minimize_NegDs(NegDsR, NegDsO, PosCs, NegCsI, NegCsO).
minimize_NegDs(NegDs, NegDs, _PosCs, NegCs, NegCs).

minimize_PosDs([D], [], PosCsI, [D|PosCsI], _NegCs) :- !.
minimize_PosDs(PosDsI, PosDsO, PosCsI, PosCsO, NegCs) :-
       delete(D, PosDsI, PosDsR),
       memberchk(D, NegCs), !,
       minimize_PosDs(PosDsR, PosDsO, PosCsI, PosCsO, NegCs).
minimize_PosDs(PosDs, PosDs, PosCs, PosCs, _NegCs).

minimize_conj([C], [C], no) :- !.
minimize_conj(PosCsI, PosCsO, _) :-
    ( delete((top,_), PosCsI, PosCsO) -> true
    ; PosCsI=PosCsO
    ).

%%% minimize_new(+IDs, -List)
%%%  subsumption tests: if t < t', then
%%%      t & t' --> t
%%%      t ; t' --> t'
minimize_new([], []) :- !.
minimize_new(IDs, List) :-
	minimize_new(IDs, List, _).

minimize_new([], [], _).
minimize_new([ID|IDs], ListOut, Flag) :-
	minimize_new(ID, IDs, ListOut, Flag).

minimize_new((T,TID), IDs, ListOut, Flag) :-
	minimize_new(IDs, T, TID, NewList, Flag),
	( Flag == again ->
	      minimize_new(NewList, ListOut, _)
	; NewList = ListOut
	).
minimize_new(d(Ds,TT), IDs, ListOut, Flag) :-
	minimize_new(IDs, Ds, TT, NewList, Flag),
	( Flag == again ->
	      minimize_new(NewList, ListOut, _)
	; NewList=ListOut
	).

minimize_new([], T, TID, [Out], _) :-
	minimize_out(T, TID, Out).
minimize_new([F|R], P, PID, NewList, Again) :-
	minimize_new(F, R, P, PID, NewList, Again).

minimize_new((T,TID), R, P, PID, NewList, Again) :-
	( functor(PID, '.', 2) ->
	      check_formulas(PID,[[TID]],Flag)
	; check_formulas([[PID]],[[TID]],Flag)
	),
	( Flag == 1 ->
	      Again = again,
	      minimize_out(P, PID, Out),
	      NewList = [Out|R]
	; Flag == 2 ->
	      Again = again,
	      NewList = [(T,TID)|R]
	; /* Flag == 3 */
	      NewList = [(T,TID)|List],
	      minimize_new(R, P, PID, List, Again)
	).
minimize_new(d(Ds,TT), R, T, TID, NewList, Again) :-
	( functor(TID, '.', 2) ->
	      check_formulas(TID,TT,Flag)
	; check_formulas([[TID]],TT,Flag)
	),
	( Flag == 1 ->
	      Again = again,
	      minimize_out(T, TID, Out),
	      NewList = [Out|R]
	; Flag == 2 ->
	      Again = again,
	      NewList = [d(Ds,TT)|R]
	; /* Flag == 3 */
	      NewList = [d(Ds,TT)|List],
	      minimize_new(R, T, TID, List, Again)
	).

minimize_out(T, TID, Out) :- 
	( functor(T,'.',2) ->  %% bug fixed (missing arity),12.6.95,jd
	                       %% lead to fail for printing afs_symbol('.')
	      Out = d(T,TID)
	; Out = (T,TID)
	).

find_ids([], []).
find_ids([T|R],[(T,TID)|TIDs]) :-
	( T = ~(T0) ->
	      symbol_table(T0,0,TID0,_,_),
	      negate_TID(TID0,TID)
	; symbol_table(T,0,TID,_,_)
	),
	find_ids(R,TIDs).

convertIDs([], []).
convertIDs([F|R],[P|RR]) :-
	convertIDs(F,R,[P|RR]).

convertIDs((P,_),R,[P|RR]) :- !,
	convertIDs(R,RR).
convertIDs(d(Ds,_),R, [NewDs|RR]) :-
	convertIDs(Ds, NewDs),
	convertIDs(R, RR).

ds2list([], []).
ds2list([d(PosDs,NegDs)|R], [DOut|TTOut]) :-
      ( NegDs == [] ->
	    Ds=PosDs
      ; neg_list(NegDs, NegL),
        append(PosDs, NegL, Ds)
      ),
      minimize_new_ds(Ds,NewDs),
      ( NewDs = [T] ->
	    DOut=T
      ; ds2tt(NewDs,TT),
	DOut=d(NewDs,[TT])
      ),
      ds2list(R, TTOut).

ds2tt([],[]).
ds2tt([(_,TID)|R],[TID|RR]) :-
	ds2tt(R,RR).

minimize_new_ds([], []).
minimize_new_ds([(T,TID)|IDs], ListOut) :-
	minimize_new_ds(IDs, T, TID, ListOut).

minimize_new_ds([], T, TID, [(T,TID)]).
minimize_new_ds([(T,TID)|R], P, PID, NewList) :- !,
	check_formulas([[PID]],[[TID]],Flag),
	( Flag == 1 ->
	      minimize_new_ds(R, T, TID, NewList)
	; Flag == 2 ->
	      minimize_new_ds(R, P, PID, NewList)
	; /* Flag == 3 */
	      NewList = [(T,TID)|List],
	      minimize_new_ds(R, P, PID, List)
	), !.

neg_list([], []).
neg_list([(T,TID)|R], [(~(T0),TID0)|NR]) :-
   ( T == top ->
	 T0 = bottom, TID0 = [[1],[-1]]
   ; T == bottom ->
	 T0 = top, TID0 = [[1,-1]]
   ; T=T0, negate_TID(TID,TID0)
   ),
   neg_list(R, NR).

list2tt([D], D_Out) :- !,
   list2disj(D, D_Out).
list2tt([D|Cs], '&'(D_Out, CsOut)) :-
   list2disj(D, D_Out),
   list2tt(Cs, CsOut).

list2disj([D], D) :- !.
list2disj([D|Ds], ';'(D, D_Out)) :- !,
   list2disj(Ds, D_Out).
list2disj(D, D).

/*
%%% test environment
:-op(610, xfy, &).
:-op(590,  fx, ~).

type_code(0, top) :- !.
type_code(I, I) :- I > 0.

append([], L, L).
append([A|B], C, [A|D]) :-
	append(B, C, D).

memberchk(A, [A|_]) :- !.
memberchk(A, [_|R]) :-
	memberchk(A, R).

member(A, [A|_]).
member(A, [_|R]) :-
	member(A, R).

delete(A, [A|L], L) :- !.
delete(A, [B|C], [B|D]) :-
	delete(A, C, D).

t(CNF) :-
	nl, write(CNF), nl, cnf2type_term(CNF, TT), write(TT), nl.

l :- [cnf2type_term].

ls :- listing.
ls(X) :- listing(X).

t1 :- t([[-1],[1,2]]).       % 2& ~1
t2 :- t([[1,-1]]).           % top
t3 :- t([[1],[3,-1,2]]).     % 1&(2;3)
t4 :- t([[0],[1]]).          % 1
t5 :- t([[0,1]]).            % top

t :- t1,t2,t3,t4,t5.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose:                                               %%%
%%%      Created: Tue Sep 14 16:07:06 1993                      %%%
%%%     Modified: Tue Sep 19 19:15:05 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.7  1995/09/19 17:24:10  jochen
% convert4fed made mode robust
%
% Revision 1.6  1994/07/01  13:26:17  jochen
% bug fix convert4fed
%
% Revision 1.5  1994/06/30  17:49:26  jochen
% convert4fed now switches off debug
%
% Revision 1.4  1994/06/24  17:28:20  jochen
% bug fixes, more robustness in tree-pp
% * kill_restlist_sharings/2 removed
% * handling of empty dtrs lists in get_dtrs_list
% * only convert_local for complex structures
%
% Revision 1.3  1994/06/21  15:46:19  michel
% new internal structures in prepare printing made some minor changes
% neccessary in get_dtrs_list/3, fetch_feature_paths/2,
% extract_maxpath/2, remove_tree_dtrs/3
%
% Revision 1.2  1994/05/10  16:04:05  jochen
% major changes due to new pp-preparation (prepare_printing.pl)
%
% Revision 1.1  1993/11/18  14:44:26  jochen
% check in cuf2.28d.
%




%% To be called before preparing unless xmfed_output flag is no or
%% tree_display_dtr_pred is not a current_predicate.
%% convert4fed recursively walks through structure, converting local
%% trees as specified by user
%% synopsis: convert4fed(Structure, OccList, OutputMode, OutStruc)
%%           OccList for cycle check
%%           OutputMode is xmfed(TriggerTypeCode,CUFPredicate) where:
%%           TriggerTypeCode is the code of the type of the
%%                           tree_display_dtr_pred entry
%%           CUFPredicate is the conversion predicate given in that entry


convert4fed(FS, Out) :-
	get_flag(debug,DbgFlag),
	set_flag(debug,no),
	convert4fed_(FS, Out, [], _),
	flag(debug,DbgFlag).


convert4fed_([TT|_FS], _, Occs, Occs) :-
	memberchk_eq(TT,Occs), !.
convert4fed_([TT|FS], Output, Occs, Occs_o) :-
	convert_local_tree([TT|FS], Output),
	convert4fed_fs(FS, Output, [TT|Occs], Occs_o).

convert4fed_fs(R, _, Occs, Occs) :- var(R), !.
convert4fed_fs([], _, Occs, Occs).
convert4fed_fs([F:Val|Rest], Output, Occs, Occs_o) :- !,
	( ( F = '$DTRS' ; F = '$IGNORESKEL' ) -> Occs1 = Occs
	; convert4fed_(Val, Output, Occs, Occs1)
	),
	convert4fed_fs(Rest, Output, Occs1, Occs_o).
convert4fed_fs([_A], _, Occs, Occs). %% for atoms


convert_local_tree([TT|FS], xmfed(TCode,Func/Arity)) :-
	FS = [_:_|_],     %% only for complex structures
%%	current_predicate(tree_display_dtr_pred,tree_display_dtr_pred(_,_)),
%%	tree_display_dtr_pred(Type,Func/Arity),
%%	symbol_table(Type,0,TCode,_,type), 
	get_type(TT, Type1, _TRest),
	cuf_is_complex([TT|FS]),      %% ignore variables of tree type
	nonvar(Type1),
	type_subsumes([[TCode]],Type1,1),
	Ar1 is Arity+1,
	functor(Pred,Func,Ar1),
	arg(2,Pred,[TT|FS]),
	wrap_goal(Pred,WPred),
        cuf_prove(undelayed_only,[WPred],_,[],_,_),
	arg(1,Pred,DtrsList),
	arg(3,Pred,IgnoreSkel),
%	kill_restlist_sharings(DtrsList, DtrsList1),
	!,
	unify([TT|FS],[_,'$DTRS':DtrsList,'$IGNORESKEL':IgnoreSkel|_]).
% 	fetch_feature_paths(IgnoreSkel,IgnorePaths),
% 	remove_tree_dtrs(IgnorePaths, FS, FSOut).
convert_local_tree(_, _).

% kill_restlist_sharings(['R':[TT,[]]|FS], ['R':[TT,[]]|FS]) :- !.
% kill_restlist_sharings(['R':[_TT|RFS]|FS], ['R':[_|RFS_o]|FS]) :- !,
% 	kill_restlist_sharings(RFS, RFS_o).
% kill_restlist_sharings([E|FS],[E|FS_o]) :-
% 	kill_restlist_sharings(FS, FS_o).
	

%% To be called during preparation of FS for Xmfed.
%% convert4fed has to have converted FS before, so that get_dtrs_list
%% need only check for $DTRS and $IGNORESKEL features

%% THE FOLLOWING CODE DEPENDS ON THE INTERNAL FORMAT AFTER 
%% ELIMINATE_AND_SORT..!!

get_dtrs_list(FS, DtrsFSList, FSOut) :-
	delete('$DTRS':DtrsStruc, FS, FS1),
	( DtrsStruc = '$STRUC'(_Var, _TT,DtrsFSList) ->
	      true
	; DtrsFSList = [] ),
	delete('$IGNORESKEL':IgnoreSkel, FS1, FS2),
	fetch_feature_paths(IgnoreSkel,IgnorePaths),
	remove_tree_dtrs(IgnorePaths, FS2, FSOut).


% 
% cuf_list_to_prolog([_|FS], [F|R]) :-
% 	member('R':RR, FS),!,
% 	(member('F':F, FS) -> true ; true),
% 	cuf_list_to_prolog(RR, R).
% cuf_list_to_prolog(_, []).


%% extract list of feature paths (list of list of features) from
%% skeletal FS
fetch_feature_paths('$STRUC'(_,_,FS), IgnorePaths) :-
	findall(P, extract_maxpath(FS, P), IgnorePaths).


extract_maxpath([], []).
extract_maxpath(FS, [F|P]) :-
	member(F:'$STRUC'(_,_,RFS1), FS),
	extract_maxpath(RFS1,P).


% memb_nonvar(_,X) :- var(X), !, fail.
% memb_nonvar(E,[E|_]).
% memb_nonvar(E,[_|R]) :- memb_nonvar(E,R).


%% remove_tree_dtrs(PathsList, FS, NewFS)
%% remove paths PathList from FS
remove_tree_dtrs([], F, F).
remove_tree_dtrs([Path|Paths], FS, NewTTFS) :-
	remove_tree_dtrs_path(Path, FS, FS1),
	remove_tree_dtrs(Paths, FS1, NewTTFS).
	
remove_tree_dtrs_path([F|RPath], FS, NewFS) :-
	delete(F:V, FS, FS1),
	nonvar(V), !, 
	(RPath == [] -> NewFS = FS1
	; NewFS = [F:V1|FS1],
	  remove_tree_dtrs_path(RPath, V, V1)
	).
remove_tree_dtrs_path(_, FS, FS).  %% path not there or trivial


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose: translate structure into Xmfed format,        %%%
%%%      Created: Tue May  3 16:42:36 1994                      %%%
%%%     Modified: Tue Nov  7 17:46:57 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO: 
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.7  1995/11/07 16:54:11  jochen
% adapted to work in Sicstus3 as well (\ in 'atom', unix/1->shell/1)
%
% Revision 1.6  1994/07/01 13:24:08  jochen
% printing of !-terms (type prolog) fixed
%
% Revision 1.5  1994/06/24  17:29:26  jochen
% bug fix; couldn't handle DIFFs on atoms
%
% Revision 1.4  1994/06/21  16:13:09  michel
% small bug fix in layout2fed/4, 'POLY' case
%
% Revision 1.3  1994/06/13  12:15:39  jochen
% test/1 commented out
%
% Revision 1.2  1994/05/24  15:03:03  jochen
% type_term2atom/2 conversion added, due to change in prepare_printing
% using write_atom/1
%
% Revision 1.1  1994/05/10  16:00:04  jochen
% initial check-in
%

%% pp2fed/2 transforms fs /cs-pair to Xmfed format, writes output
%% into tmp file and calls xmfed
	

%:- ensure_loaded(library(system)).

pp2fed(FS,Cs) :-
	fs_layout( FS, LayoutFS, 1, _N),
        cs_layout( Cs, LayoutCs, []), 
        cuf_tmp_file_name( File ),
	tell( File), % open stream to file
        write( ' [ #b ( % ' ),   %% nl, darf hier nicht stehen!
	layout2fed(LayoutFS, 1, TagList, [] ), % Tab = 0
	write( ' ) ' ),nl,
	( LayoutCs = [] -> true
	; layout2fed_nelist(LayoutCs, 1, _, [] )
	),
        singles( TagList, Singles ), 
	( Singles = [] -> true
	; write( ' ( % [ #l #h ' ), nl,
          write_singles( Singles ),               % hidden duplicates
	  write( ' ) ' ),nl
        ),
        told,                          % close stream
	concat_atom( ['(xmfed -nc ',File, '>& /dev/null;',
		      (\),'rm ', File, ') &'],
                     Command ), 
	( predicate_property(shell(_),_) ->  %% hack for Sicstus3
	      shell(Command)
	; unix(shell(Command))
	).




fs_layout( 'ATOM'(A), atom(A1), I, I ) :-
   ( A == [] -> A1 = '<>' 
   ; number(A) -> number_chars(A, ChA1), atom_chars(A1, ChA1)
   ; A = A1
   ).
fs_layout( 'REF'(Tag), reentr(Tag), I, I ).
fs_layout( 'CYCLE'(Tag), reentr(Tag), I, I ).
fs_layout('STRUC'(Multi,Tag,TT,FS), Content, I0, In ) :-
   ( Multi == 'MULTI' ->
	 ( var(Tag) ->      % first occurrence
	       Tag = I0,
	       I1 is I0+1,
	       fs_layout(FS, TT, ContFS, I1, In),
	       Content = flaggedfs(reentr(Tag),ContFS)
	 ; In = I0, Content = reentr(Tag) % should not occur
	 )
   ; fs_layout(FS, TT, Content, I0, In)
   ).
fs_layout('TREE'(RootFS,DtrsListFS), Content, I0, In) :-
   ( convert2list(DtrsListFS, ConsedDtrs, _),
     fs_layout(RootFS, CRoot, I0, I1),
     fs_layout_list(ConsedDtrs, CDtrs, I1, In) ->
			Content = tree([CRoot|CDtrs])
   ; cuf_out(error, ['failed to print tree']),
     RootFS = 'STRUC'(Multi, Tag, TT, RootFeats),
     fs_layout('STRUC'(Multi, Tag, TT, ['$DTRS':'STRUC'(_,_,top,DtrsListFS)|RootFeats]),
	       Content, I0, In)
   ).
fs_layout('PROLOG'(P), prolog(P), I, I).


fs_layout([], TT, fs(TT_atom,[]), I, I) :- !,
   type_term2atom(TT,TT_atom).
fs_layout(FS, TT, Content, I0, In) :-
   ( cuf_list(FS), get_flag(print_lists, yes) ->
	 FS=['F':FV,'R':RV],
	 fs_layout(FV, FCont, I0, I1),
	 ( RV = 'ATOM'([]) -> 
	       Content = nelist([FCont]), In = I1
	 ; fs_layout(RV, RCont, I1, In),
	   ( RCont = nelist(L) ->
		 Content = nelist([FCont|L])
	   ; Content = nelist([FCont|rlist(RCont)])
	   )
	 )
   ; ( ( get_flag(print_types, no) ; TT == top ) ->
	   Content = fs_no_lbl(ContFS)
     ;  type_term2atom(TT,TT_atom),
	Content = fs(TT_atom,ContFS)
     ),
     fs_layout_(FS, ContFS, I0, In)
   ).



fs_layout_list([], [], I, I).
fs_layout_list([F|R], [Cont|Conts], I0, In) :-
   fs_layout(F, Cont, I0, I1),
   fs_layout_list(R, Conts, I1, In).

fs_layout_([], [], I, I).
fs_layout_([F:V|R], [feature(F,ContV)|Conts], I0, In) :-
   fs_layout(V, ContV, I0, I1),
   fs_layout_(R, Conts, I1, In).



cs_layout([]) --> [].
cs_layout([C|R]) -->
	cs_layout_constraint(C),
	cs_layout(R).

cs_layout_constraint('DIFF'(FS1, FS2)) -->
   { layout_get_tag(FS1, T1),
     layout_get_tag(FS2, T2) },
   ( { sort([T1,T2],[S1,S2]) } ->
	 ['DIFF'(S1,S2)]
   ; []
   ).
cs_layout_constraint('POLY'(_ID,TI1,DTA,TI2,RTA)) -->
   { layout_get_tag(TI1, T1),
     layout_get_tag(TI2, T2) },
   ['POLY'(T1, atom(DTA), T2, atom(RTA))].

layout_get_tag('STRUC'(_,Tag,_,_), reentr(Tag)) :-
	nonvar(Tag), !.
layout_get_tag('STRUC'(_,_,_,_), atom('_')).
layout_get_tag('ATOM'(Atom), atom(Atom)).
layout_get_tag('REF'(Tag), reentr(Tag)).
layout_get_tag('PROLOG'(P), prolog(P)).

% layout_get_tag(Tag, LayoutTag) :-
% 	pp_get_tag(Tag, PPTag),
% 	( PPTag = '_' -> LayoutTag = atom('_')
% 	; LayoutTag = reentr(PPTag)
% 	).


layout2fed_parens( Cont, Tab ) -->
	{ nl, tab( Tab ), write( ' ( % ' )  },
	layout2fed( Cont, Tab ),
        { nl, tab( Tab ), write( ' ) ' ) }.

layout2fed( atom(Atom), _Tab ) --> [],
        { write_atom( Atom ) }.
%        { writeq( Atom ) }.
layout2fed( nelist(List), Tab ) -->
        { tab( Tab ), write( '[ #l #b ( % "<" ) ' ),
          nl,  NewTab is Tab + 8 },
	layout2fed_nelist( List, NewTab ),
        { tab( NewTab ), write( ' ( % ">" ) ' ) }.
layout2fed( reentr(Tag), _Tab ) --> [Tag],
        { write( '#' ),
          write( Tag ) }.
layout2fed( flaggedfs(Tag,Cont), Tab ) -->
	layout2fed( Tag, Tab ),
        { write( '= ' ), NewTab is Tab + 3 },
	layout2fed( Cont, NewTab ).
layout2fed( tree(List), Tab ) -->
        { format( '[ #s~n', [] ),
          NewTab is Tab + 3 },
        layout2fed_nelist( List, NewTab ).
layout2fed( fs_no_lbl(Cont), Tab ) -->
	{ write( '[ ' ),
          ( Cont == [] -> true ; write( '#f ' ) ),
          nl, NewTab is Tab + 3 },
	layout2fed_fslist( Cont, NewTab ).
layout2fed( fs(Lbl,Cont), Tab ) -->
        { format( '[ #f ( % "~a" #g=bold )~n', Lbl ),
          NewTab is Tab + 5 },
	layout2fed_fslist( Cont, NewTab ).
layout2fed( 'DIFF'(S1, S2), Tab ) --> 
	{ write(' [ #l #b ( % ') }, 
        layout2fed(S1, Tab),
        { write(' ) ( % "=/=" ) ( % ') },
        layout2fed(S2, Tab),
        { write(' )'),nl }.
layout2fed( 'POLY'(T1, DTA, T2, RTA), Tab ) --> [],
	{ write(' [ #l #b ( % ') }, 
        layout2fed(T1, Tab),
        { write(' ) ( % "&" ) ( % "') },
        layout2fed(DTA, Tab),
        { write('" #g=bold ) ( % "-->" ) ( % ') },
        layout2fed(T2, Tab),
        { write(' ) ( % "&" ) ( % "') },
        layout2fed(RTA, Tab),
        { write('" #g=bold )'),nl }.
layout2fed( prolog(P), _Tab ) --> [],
	{ write('"!'), writeq(P), write('"') }.


layout2fed_nelist( rlist(Tag), Tab ) --> 
	{ nl, tab( Tab ), write( ' ( % . ) ' )  },
	layout2fed_parens( Tag, Tab).
layout2fed_nelist( [], _Tab ) --> [].
layout2fed_nelist( [F|R], Tab ) -->
	layout2fed_parens( F, Tab ),
        layout2fed_nelist( R, Tab ).

layout2fed_fslist( [], _Tab ) --> [].
layout2fed_fslist( [feature(Name,Val)|R], Tab ) -->
	{ nl, tab( Tab ), write( ' ( ' ),
          writeq( Name ),         % FName
          write( ' ' ),
          NewTab is Tab + 7 },
	layout2fed( Val, NewTab ),   % FVal
        { nl, tab( Tab ),
          write( ' ) ' ) },
        layout2fed_fslist( R, Tab ).

        

singles( [], [] ).
singles( [F|R], RR ) :-
        member( F, R ),
        !,
        delete_all( F, R, R1 ),
        singles( R1, RR ).
singles( [F|R], [F|RR] ) :-
	singles(R, RR).

write_singles( [] ).
write_singles( [F|R] ) :-
        write( ' ( % #' ), write( F ), %% F is a singl. tag
        write( ' ) ' ), nl,        
        write_singles( R ).


cuf_tmp_file_name( File ) :-
        ( environ( 'CUFTMPDIR', TmpDir ) -> true ; TmpDir = '/tmp' ),
        concat_atom( [TmpDir,'/cufXXXXXX'],
                     Template),
        cuf_mktemp(Template,File).

% 
% test(Extern) :-
% 	cuf_extern2intern(Extern, Intern, Constraints, Result),
%         set_initial_goalIDs(Intern),       %% necessary for debug
% 	proof_init,                        %% necessary for debug
% 	statistics(runtime,[TOff,_]),
% 	( cuf_prove(all, Intern, [], Constraints, ConsOut,
% 		    RuleNoList),
% 	  write_success_node(RuleNoList),  %% necessary for debug
% 	  statistics(runtime,[TAbs,_]),
% 	  TRel is TAbs-TOff,
% 	  cuf_out(message, ['used time (msec):',TRel]),
% 	  set_flag(xmfed_output,yes),
% 	  prepare_printing(Result,ConsOut,PrepResult,PrepConsOut),
% 	  pp2fed(PrepResult,PrepConsOut),
% 	  ( yn_question(['other results? (y/n)']) ->
% 		fail
% 	  ; true )
% 	; statistics(runtime,[TEnd,_]),
% 	  Total is TEnd-TOff,
% 	  cuf_out(message, ['total time for', cuf(Extern),
% 	                    '(msec):',Total]),
% 	  ( depth_check(depth_exhausted) ->   %% in prooftree.pl
% 		cuf_out(message,
% 			['search incomplete (depth limit exhausted)'])
% 	  ; true
% 	  )
% 	).

%%%
%%% eof: pp2fed.pl
%%%

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%       Author: Jochen Doerre                                 %%%
%%%      Purpose:                                               %%%
%%%      Created: Fri Feb 19 12:42:57 1993                      %%%
%%%     Modified: Fri Mar 24 13:20:23 1995 (jochen)             %%%
%%%    Copyright: Institut fuer maschinelle Sprachverarbeitung  %%%
%%%               Universitaet Stuttgart                        %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% RCS-INFO:
%
% $Source: /cvs/cTI/cTI/benchterm/large/lg_sys.pl,v $
% $Id: lg_sys.pl,v 1.1.1.1 2002/11/26 10:40:18 roberto Exp $

% $Log: lg_sys.pl,v $
% Revision 1.1.1.1  2002/11/26 10:40:18  roberto
% Initial version from Fred.
%
% Revision 1.1.1.1  2000/07/26 15:45:41  roberto
% China 0.9
%
% Revision 1.2  2000/02/12 22:47:57  roberto
% First go at the improved handling of setof, bagof, findall, on_exception, and once.
%
% Revision 1.1  2000/01/16 16:29:10  roberto
% Moved.
%
% Revision 1.1  1999/04/11 19:54:41  roberto
% New files.
%
% Revision 1.8  1996/01/17 13:17:18  junger
% - arguments swapped in nth to match new version of nth
%
% Revision 1.7  1995/03/24  14:53:08  jochen
% undo/1 eliminated (was not correct in Sicstus version)
%
% Revision 1.6  1994/06/14  09:32:00  junger
% - get_clause_list/2 adapted to handle foreign calls.
%
% Revision 1.5  1994/02/03  11:48:30  jochen
%   Adaptations to new goal and clause formats
%   set_flag(interactive,no)...  on backtracking in skip
%
% Revision 1.4  1993/12/21  10:20:58  michel
% show_clauses_dbg flag renamed to show_clauses
%
% Revision 1.3  1993/12/08  13:01:19  michel
% clause selection bug fixed
% clause output can be switched on/off by the show_clauses_dbg flag
%
% Revision 1.2  1993/11/18  14:43:05  jochen
% check in cuf2.28d.
%



% History (before RCS):
% Thu Aug 12 12:03:35 1993 (jochen) : in calls for resolve/9 +MaxDepth argument corrected (twice)
% Tue Jul 27 15:43:56 1993 (jochen) : use '$GOAL$' wrapper for goals in cuf_out


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% contains:
%%%           resolve_interactive/8
%%%           resolve_interactive1/7
%%%           get_clause_list/2
%%%           display_choice_list/3
%%%           display_clause_list/3
%%%           read_choice/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%  resolve_interactive(+GoalList, -SubGoals, +ConsIn, -ConsOut
%%%%%%%                      +Indentation, +MaxDepth, -SolDepth, -RuleNo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_interactive(GoalList, SubGoals, ConsIn, ConsOut,
                    Ind, MaxDepth, NewInd, ClauseID) :- 
     get_flag(graphic_dbg,yes),
     current_predicate(cuf_dbg_scrolled_window,_), !,
     add_delay_flags(GoalList, GoalInfoList),
     cuf_dbg_window_selection(GoalInfoList,GoalNo, ClauseID),
     (GoalNo > 0
     -> nth_del(GoalNo, GoalList, Goal, RestGoals),
        NewMaxDepth is Ind+1,                 %% ignore MaxDepth here
	resolve(Goal, SubGoals, RestGoals, ConsIn, ConsOut,
		Ind, NewMaxDepth, NewInd, ClauseID)
     ;  %% else: skip
        !,  %% cut window_selection, no more for this list
	(set_flag(interactive, no)
	; set_flag(interactive, yes), fail),   %% on backtr.
	resolve(GoalList, SubGoals, ConsIn, ConsOut,
		Ind, MaxDepth, NewInd, ClauseID)
     ).


resolve_interactive(GoalList, SubGoals, ConsIn, ConsOut,
                    Ind, MaxDepth, NewInd, ClauseID) :- 
     display_choice_list(GoalList, 1, Max),
     read_choice(['Goal No.',(0-Max):''], Max, No),
     ( No == Max ->
	   resolve_interactive(GoalList, SubGoals, ConsIn, ConsOut,
			       Ind, MaxDepth, NewInd, ClauseID)
     ; No == 0 ->
	   set_flag(interactive, no),
% 	   undo((set_flag(interactive,yes),fail)),
	   ( true ; set_flag(interactive,yes),fail),
	   resolve(GoalList, SubGoals, ConsIn, ConsOut,
		   Ind, MaxDepth, NewInd, ClauseID)
     ; nth_del(No, GoalList, SelectedGoal, RestGoals),
       resolve_interactive1(SelectedGoal, SubGoals, RestGoals,
			    ConsIn, ConsOut, Ind, NewInd, ClauseID)
     ).

%%%%%%%  resolve_interactive1(+Goal, -SubGoals, +RestGoals, +ConsIn,
%%%%%%%           -ConsOut, +Indentation, -NewInd, -ClauseID) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_interactive1(Goal, SubGoals, RestGoals, ConsIn, ConsOut,
                     Ind, NewInd, ClauseID) :-
    get_clause_list(Goal, ClauseList),
    wrap_goal(Goal0,Goal),
    repeat,
    ( get_flag(show_clauses,yes) ->
	  functor(Goal0,F,A),
	  A1 is A-1,
	  display_clause_list(ClauseList, 1, F, A1)
    ; cuf_out(message, ['Goal:','$GOAL$'(Goal0),'$NL$'])
    ),
    length(ClauseList,Max),
    read_choice(['Clause No. (1 -',Max,'or 0=fail):'], Max, RuleNo),
%     undo((cuf_out(message, ['Retry ...','(last choice was:',RuleNo,')']),
% 	  set_flag(interactive, yes))
% 	),
    ( true
    ; cuf_out(message, ['Retry ...','(last choice was:',RuleNo,')']),
      set_flag(interactive, yes),
      fail
    ),
    ( RuleNo > 0 ->
	  nth(RuleNo,'CUF clause'(FID, EID, No, _, _, _), ClauseList),
	  ClauseID = rule_id(FID, EID, No),
	  NewMaxDepth is Ind+1,                 %% ignore MaxDepth here
	  resolve(Goal, SubGoals, RestGoals, ConsIn, ConsOut,
		  Ind, NewMaxDepth, NewInd, ClauseID, non_det)
    ; !, fail
    ).

get_clause_list(WGoal, ClauseList) :-
    wrap_goal(Goal,WGoal),
    copy_term(Goal, Copy),
    bagof('CUF clause'(FID, EID, No, Copy, SubGoals, Cons),
	  check_for_clause(Copy, FID, EID, No, SubGoals, Cons),
	  ClauseList).
%    bagof('CUF clause'(FID, EID, No, Copy, SubGoals, Cons),
%	   'CUF clause'(Copy, FID, EID, No, [], SubGoals, [], Cons, []),
%	   ClauseList).

check_for_clause(Goal, FID, EID, No, SubGoals, Cons) :-
	functor(Goal, Func, Arity),
	(cuf_foreign(Func, Arity, FID) ->
	     wrap_goal(Goal, WGoal),
	     goal_flag(WGoal, foreign),
	     SubGoals = [WGoal],
	     Cons = []
	; 'CUF clause'(Goal, FID, EID, No, [], SubGoals, [], Cons, [])
	).

%%%%%%%  display_choice_list(+ChoiceList, +NumberIn, -NumberOut)
%%%%%%%  display_clause_list(+ClauseList, +Functor, +Arity)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  output of choices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_choice_list([], No, No) :-
    cuf_out(message, [[No]:' redisplay']),
    cuf_out(message, [[0],': skip','$NL$']).
display_choice_list([WGoal|Goals], No, Max) :-
    wrap_goal(Goal, WGoal),
    ( is_undelayed(Goal) -> 
	  DelayFlag = ' OK ' 
    ; DelayFlag ='WAIT' ),
    cuf_out(message, [[No]:(DelayFlag:''), '$GOAL$'(Goal)]),
    No1 is No+1,
    display_choice_list(Goals, No1, Max).

display_clause_list(ClauseList, No, Sort, Arity) :-
	trans_ClauseList(ClauseList, NewList),
	print_rules(NewList, No, Sort, Arity).

trans_ClauseList([], []).
trans_ClauseList(['CUF clause'(_,_,_,Head,Body,Cons)|R],['CUF clause'(Head,Body,Cons)|RR]) :-
	trans_ClauseList(R,RR).


%%%%%%%  read_choice(+Message, +Range, -ChoiceNumber)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  input of choice number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_choice(Output, Range, No) :-
    repeat,
    cuf_out(message, Output),
    read(No),
    ( integer(No), 
      No >= 0,
      No =< Range, !
    ; cuf_out(message, ['Type a number between 0 and',Range,'please!']),
      fail
    ).


add_delay_flags([],[]).
add_delay_flags([WGoal|Goals],[Goal:DelayFlag|GoalInfos]) :-
	wrap_goal(Goal,WGoal),
	(is_undelayed(Goal) -> DelayFlag = ' OK ' 
	; DelayFlag ='WAIT'),
	add_delay_flags(Goals,GoalInfos).


