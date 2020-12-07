/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:49:17 1994 */
/* Updated: Mon May 29 15:02:48 2000 */ 
/* Filename: op.pl */
/* Abstract: Operator declarations. Comments on built-in predicates. */

%% Special characters (ISO Prolog): # $ & * + - . / : < = > ? @ ^ ~ \

%% Since the backslash \ behaves differently in quoted names in different
%% implementations we take ! as escape character for creating TeX
%% output. For instance, the Prolog atom '!neq' stands for the TeX control
%% sequence \neq (cf. proofmacros.tex).

:- op(980,xfy,by).		% ... by ...
:- op(970,xfy,:).		% two Peano dots, right associative
:- op(960,yfx,<=>).		% equivalence
:- op(950,xfy,=>).		% implication (-> is an operator of Prolog)
:- op(940,yfx,\/).		% disjunction (v can be used as name)
:- op(930,yfx,&).		% conjunction
:- op(900,fy,~).		% negation (cf. not, \+)  
:- op(900,fy,not).		% negation
:- op(900,xfy,'!,').		% empty operator (TeX)
:- op(900,fy,def).		% def
:- op(900,fy,succeeds).		% succeeds
:- op(900,fy,fails).		% fails
:- op(900,fy,terminates).	% terminates
:- op(800,fy,all).		% universal quantifier
:- op(800,fy,ex).		% existential quantifier
:- op(700,yfx,=).		% equality
:- op(700,yfx,:=).		% assignment
:- op(700,yfx,'!eq').		% equality (TeX)
:- op(700,xfy,<>).		% different
:- op(700,xfy,'!neq').		% different (TeX)
:- op(700,xfy,<).		% less (built-in)
:- op(700,xfy,=<).		% less than or equal (built-in)
:- op(700,xfy,@<).		% less (nat)
:- op(700,xfy,@=<).		% less than or equal (nat)
:- op(700,xfy,#<).		% less (int)
:- op(700,xfy,#=<).		% less than or equal (int)
:- op(700,xfy,'!leq').		% less than or equal (TeX)
:- op(700,xfy,'!prec').         % less than (TeX)
:- op(700,xfy,'!preceq').       % less than or equal (TeX)
:- op(700,xfy,'!sub').		% subset (TeX)
:- op(700,xfx,'!is').		% is (TeX)
:- op(600,yfx,//).		% application of substitutions
:- op(600,yfx,'!apply').	% application of substitutions (TeX)
:- op(600,yfx,**).		% concatenation
:- op(600,yfx,'!app').		% concatenation (TeX)
:- op(550,xfy,imp).
:- op(500,yfx,@+).		% sum (nat)
:- op(500,yfx,#+).		% sum (int)
:- op(500,yfx,or).
:- op(500,yfx,#-).		% subtraction (int)
:- op(400,yfx,@*).		% product (nat)
:- op(400,yfx,and).
:- op(400,yfx,#*).		% product (int)
:- op(300,fy,#-).		% minus (int)
:- op(300,fy,neg).
:- op(100,fy,?).		% variables: ?x, ?y, ?z, ?v, ?0, ?1, ?2

%%
%% We use fhe following built-in predicates:
%%
%% In the pure, declarative kernel only the negation as failure operator is 
%% used. We use (almost) no cuts. We use (G1 -> G2; G3). Note, that
%% (G1 -> G2 ; G3 -> G4 ; G5) is parsed as (G1 -> G2 ; (G3 -> G4 ; G5)).
%%
%% Negation:
%%
%%   \+ G: Negation as failure.
%%
%% If-then-else:
%%
%%   (G1 -> G2 ; G3): Tries to execute goal G1, and, if successful, proceeds
%%   to goal G2; otherwise proceeds to goal G3.
%%
%% Arithmetic:
%%
%%   N1 is N2 + N3: N1 is the sum of N2 and N3.
%%   N1 is N2 - N3: N1 is the difference of N2 and N3.
%%   N1 is N2 mod N3: N1 is the rest of dividing N2 by N3.
%%   N1 < N2: N1 is less than N2.
%%   N1 =< N2: N1 is less than or equal to N2.
%%
%% Unification:
%%
%%   T1 = T2: Equality is defined by the fact =(X,X).
%%
%% Type tests:
%%
%%   atom(X): X is an atom.
%%   atomic(X): X is an atom or an integer.
%%   integer(X): X is an integer.
%% 
%% Creating and decomposing terms:
%%
%%   T =.. L: L is the list consisting of the function symbol and the
%%     arguments of T in order, e.g. f(c,d) =.. [f,c,d].
%%   functor(T,F,N): F is the function sumbol of T and N is its arity,
%%     e.g. functor(f(c,d),f,2).
%%
%% Character operations:
%%
%%   concat_atom(L,A): A is the concatenation of the atoms in L,
%%     e.g. concat_atom([pro,log],prolog).
%%   name(A,L): L ist the list of ASCII codes of the atom A,
%%     e.g. name(prolog,[112,114,111,108,111,103]).
%%   atomic_length(A,N): N is the number of characters of the atom A.
%%     e.g. atomic_length(prolog,6).
%%
%% List Predicates:
%%
%%   length(L,N): N is the lenght of the list L.
%%
%% Input and Output:
%%
%%   write(T): Write T to the current output stream.
%%   writeq(T): Write T to the current output stream in a form readable
%%     by the parser.
%%   nl: Write a newline character to the current output stream.
%%   read(T): Read a term from the current input stream.
%%   close(S): Close stream S.
%%
%% The following I/O predicates are defined in the files
%% {cprolog,eclipse,open,quintus,sicstus}.pl
%%
%%   io__get_stream(F,write,S): Open file F to write and return a stream S.
%%   io__get_stream(F,read,S): Open file F to read and return a stream S.
%%   io__set_output(S): Set the current output stream to S.
%%   io__set_input(S): Set the current input stream to S.
%%   io__original_user(S): S is the original input stream (user).
%%
%% Manipulating the knowledge base:
%%
%%   dynamic(P/N): Predicate P with arity N is dynamic. Facts can be added to
%%     the knowledge base and removed.
%%   abolish(P/N): Remove the definition of predicate P with arity N.
%%   assert(F): Add the fact F to the knwoledge base.
%%   retract(F): Remove the fact F from the knowledge base.
%%
%% Finding all solutions (only used for creating the ground representation of
%% a Prolog program, in gnd.pl):
%%
%%   setof(Term,Goal,List)
%%   bagof(Term,Goal,List)
%%
%% Non-logical properties of terms (only used for creating the ground
%% representation of a Prolog program, in gnd.pl):
%%
%%   var(X): X is an unbound variable.
%%

% op.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:49:51 1994 */
% Updated: Wed Jul 21 17:10:49 1999
/* Filename: sicstus.pl */
/* Abstract: System predicates for SICStus 3, SunOS. */

% Compiling with SICStus Prolog:
%
%	?- prolog_flag(compiling,_,fastcode). 
% 	?- consult('op.pl').
% 	?- fcompile('lptp.pl').
% 	?- halt.

%%d io__lptp_home(gr::out)

io__lptp_home('/home/fred/lptp').

%%d io__path_sep(gr::out)

io__path_sep(/).

%%d once(gr::in)

once(Goal) :- call(Goal), !.

%%d concat_atom(grL::in,gr::out)

concat_atom(AtomL,Atom) :-
	concat_atomL(AtomL,CharL),
	name(Atom,CharL).

%%d concat_atomL(grL::in,grL::out)

concat_atomL([],[]).
concat_atomL([Atom|AtomL],Char3L) :-
	concat_atomL(AtomL,Char1L),
	name(Atom,Char2L),
	lst__concat(Char2L,Char1L,Char3L).

%%d atomic_length(gr::in,int::out)

atomic_length(Atom,N) :-
	name(Atom,CharL),
	length(CharL,N).

%%d io__get_stream(gr::in,gr::in,gr::out)

io__get_stream(File,Mode,Stream) :-
	open(File,Mode,Stream).

%%d io__set_output(gr::in)

io__set_output(Stream) :- set_output(Stream).

%%d io__set_input(gr::in)

io__set_input(Stream) :- set_input(Stream).

%%d db__user_stream(gr::out)

:- dynamic(db__user_stream/1).

db__user_stream(user).

%%d io__original_user(gr::out)

io__original_user(user).

%%d read_with_variables(any,any)

read_with_variables(Term,VarL) :-
	read_term(Term,[variable_names(VarL)]).

%%d io__exec_file(gr::in)

io__exec_file(File) :- consult(File).

% sicstus.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:54:26 1994 */
/* Updated: Tue Feb 16 14:38:24 1999 */
/* Filename: obj.pl */
/* Abstract: Data types for the object logic. The internal representation of
   expressions, terms, atomic goals, queries, formulas, clauses, derivations,
   derivation steps, substitutions, and second order substitutions. */

%%c
%%c Expressions are Prolog terms of the following form:
%%c
%%c   $(Name)				variables
%%c   [Tag,Expr,...,Expr]		applications
%%c   @(Tag,[Name,...,Name],Expr)	abstractions
%%c
%%c Terms, atomic goals, queries and formulas are all expressions.
%%c
%%c Terms are Prolog terms of the following form:
%%c
%%c   $(Name)
%%c   [n(Name,N),Term,...,Term]
%%c   [f(Name,N),Term,...,Term]
%%c
%%c Atomic goals are Prolog terms of the following form:
%%c
%%c   [n(Name,N),Term,...,Term]
%%c
%%c Goals are Prolog terms of the following form:
%%c
%%c   Atom
%%c   [=,Term,Term]
%%c   [~,Goal]
%%c   [&,Goal,...,Goal]
%%c   [\/,Goal,...,Goal]
%%c 
%%c Formulas are Prolog terms of the following form:
%%c
%%c   [d(Name,N),Term,...,Term]
%%c   [n(Name,N),Term,...,Term]
%%c   [=,Term,...,Term]
%%c   [<>,Term,Term]
%%c   gr(Term)
%%c   [succeeds,Atom]
%%c   [fails,Atom]
%%c   [terminates,Goal]
%%c   [def,Form]
%%c   [&,Form,...,Form]
%%c   [\/,Form,...,Form]
%%c   [~,Form]
%%c   [=>,Form,Form]
%%c   [<=>,Form,Form]
%%c   @(all,[Name,...,Name],Form)
%%c   @(ex,[Name,...,Name],Form)
%%c
%%c Clauses are Prolog terms of the following form:
%%c
%%c   clause(Atom,Goal,VarL/N) 
%%c
%%c Condition: The variables of Atom and Goal are in VarL or among 
%%c $(0), $(1),...,$(N - 1).
%%c
%%c Program predicate definitions are Prolog facts of the following form:
%%c
%%c   db__clauses(n(Name,N),[Clause,...,Clause]).
%%c
%%c Defined predicate definitions are Prolog facts of the following form:
%%c
%%c   db__pred([d(Name,N),$(1),...,$(n)],Form).
%%c
%%c Defined function definitions are Prolog facts of the following form:
%%c
%%c   db__fun([f(Name,N),$(1),...,$(n)],Form,Form).
%%c

% %%d obj__expression(gr::in)
% 
% obj__expression($(Name)) :-			% variable
% 	atomic(Name).
% obj__expression([_|ExprL]) :-			% compound expression
% 	obj__expressionL(ExprL).
% obj__expression(@(_,VarL,Expr)) :-		% abstraction
% 	obj__varL(VarL),
% 	obj__expression(Expr).
% 
% %%d obj__expressionL(grL::in)
% 
% obj__expressionL([]).				% lists of expressions
% obj__expressionL([Expr|ExprL]) :-
% 	obj__expression(Expr),
% 	obj__expressionL(ExprL).

%%d obj__varL(grL::in)

obj__varL([]).					% lists of variables
obj__varL([Name|NameL]) :-
	atomic(Name),
	obj__varL(NameL),
	\+ lst__member_check(Name,NameL).

%%d obj__term(gr::in)

obj__term($(Name)) :-				% variables
	atomic(Name).
obj__term([n(Name,N)|TermL]) :-			% constructor
	atomic(Name),
	integer(N),
	obj__termL(TermL).
obj__term([f(Name,N)|TermL]) :-			% defined function symbol
	atomic(Name),
	integer(N),
	obj__termL(TermL).

%%d obj__termL(grL::in)

obj__termL([]).					% lists of terms
obj__termL([Term|TermL]) :-
	obj__term(Term),
	obj__termL(TermL).

%%d obj__pure_term(gr::in)

obj__pure_term($(_)).				% functions symbols
obj__pure_term([n(_,_)|TermL]) :-	
	obj__pure_termL(TermL).

%%d obj__pure_termL(grL::in)

obj__pure_termL([]).				% lists of pure terms
obj__pure_termL([Term|TermL]) :-
	obj__pure_term(Term),
	obj__pure_termL(TermL).

%%d obj__atomic_goal(gr::in)

obj__atomic_goal([n(Name,N)|TermL]) :-		% atomic goals
	atomic(Name),
	integer(N),
	obj__termL(TermL).

%%d obj__goal(gr::in)

obj__goal([=,Term1,Term2]) :-			% equations
	obj__term(Term1),
	obj__term(Term2).
obj__goal([n(Name,N)|TermL]) :-			% atomic goals
	atomic(Name),
	integer(N),
	obj__termL(TermL).
obj__goal([~,Goal]) :-				% negated goal: \+ Goal
	obj__goal(Goal).
obj__goal([&|GoalL]) :-				% conjunction: (Goal1, Goal2)
	obj__goalL(GoalL).			% [&] is true
obj__goal([\/|GoalL]) :-			% disjunction: (Goal1; Goal2)
	obj__goalL(GoalL).			% [\/] is fail

%%d obj__goalL(grL::in)

obj__goalL([]).					% lists of goals
obj__goalL([Goal|GoalL]) :-
	obj__goal(Goal),
	obj__goalL(GoalL).

%%d obj__sft_atom(gr::in)

obj__sft_atom([succeeds,Atom]) :-
	obj__atomic_goal(Atom).
obj__sft_atom([fails,Atom]) :-
	obj__atomic_goal(Atom).
obj__sft_atom([terminates,Atom]) :-
	obj__atomic_goal(Atom).

%%d obj__formula(gr::in)

obj__formula([n(Name,N)|TermL]) :-		% atomic formulas
	atomic(Name),
	integer(N),
	obj__termL(TermL).
obj__formula([d(Name,N)|TermL]) :-		% atomic formulas
	atomic(Name),
	integer(N),
	obj__termL(TermL).
obj__formula([=|TermL]) :-			% equations
	obj__termL(TermL).
obj__formula([<>,Term1,Term2]) :-		% inequations
	obj__term(Term1),
	obj__term(Term2).
obj__formula([gr,Term]) :-			% gr
	obj__term(Term).
obj__formula([succeeds,Atom]) :-		% sft formulas, operator S
	obj__atomic_goal(Atom).
obj__formula([fails,Atom]) :-			% sft formulas, operator F
	obj__atomic_goal(Atom).
obj__formula([terminates,Goal]) :-		% sft formulas, operator T
	obj__goal(Goal).	
obj__formula([def,Form]) :-			% def, the definition form
	obj__sft_atom(Form).
obj__formula([~,Form]) :-			% negation
	obj__formula(Form).
obj__formula([&|FormL]) :-			% conjunction, [&] ~~ true
	obj__formulaL(FormL).
obj__formula([\/|FormL]) :-			% disjunction, [\/] ~~ false
	obj__formulaL(FormL).
obj__formula([=>,Form1,Form2]) :-		% implication
	obj__formula(Form1),
	obj__formula(Form2).
obj__formula([<=>,Form1,Form2]) :-		% equivalence
	obj__formula(Form1),
	obj__formula(Form2).
obj__formula(@(all,NameL,Form)) :-		% universial quantifier
	obj__varL(NameL),
	obj__formula(Form).
obj__formula(@(ex,NameL,Form)) :-		% existential quantifier
	obj__varL(NameL),
	obj__formula(Form).

%%d obj__formulaL(grL::in)

obj__formulaL([]).				% lists of formulas
obj__formulaL([Form|FormL]) :-
	obj__formula(Form),
	obj__formulaL(FormL).

%%d obj__sft_op(gr::out)

obj__sft_op(succeeds).
obj__sft_op(fails).
obj__sft_op(terminates).

%%d obj__var_form(gr::in)
%%d obj__equation_form(gr::in)
%%d obj__negation_form(gr::in)
%%d obj__conjunction_form(gr::in)
%%d obj__disjunction_form(gr::in)
%%d obj__implication_form(gr::in)
%%d obj__equivalence_form(gr::in)
%%d obj__forall_form(gr::in)
%%d obj__exists_form(gr::in)

obj__var_form($(_)).
obj__equation_form([=|_]).
obj__negation_form([~,_]).
obj__conjunction_form([&|_]).
obj__disjunction_form([\/|_]).
obj__implication_form([=>,_,_]).
obj__equivalence_form([<=>,_,_]).
obj__forall_form(@(all,_,_)).
obj__exists_form(@(ex,_,_)).

%%d obj__term_form(gr::in)

obj__term_form($(_)).
obj__term_form([n(_,_)|_]).
obj__term_form([f(_,_)|_]).

%%d obj__clause(gr::in)

obj__clause(clause(Head,Body,U)) :-
	obj__atomic_goal(Head),
	obj__goal(Body),
	obj__free_vars(U).

%%d obj__free_vars(gr::in)

obj__free_vars(NameL/N) :-
	obj__varL(NameL),
	integer(N).

%%d obj__clauseL(grL::in)

obj__clauseL([]).
obj__clauseL([Clause|ClauseL]) :-
	obj__clause(Clause),
	obj__clauseL(ClauseL).

%%d obj__derivation(grL::in)

obj__derivation([]).
obj__derivation([Step|Deriv]) :-
	obj__derivation_step(Step),
	obj__derivation(Deriv).

%%d obj__derivation_step(gr::in)

obj__derivation_step(by(Form,_)) :-
	obj__formula(Form).
obj__derivation_step([Tag|FormL]) :-
	obj__formula([Tag|FormL]).
obj__derivation_step(@(Tag,VL,Form)) :-
	obj__formula(@(Tag,VL,Form)).
obj__derivation_step(assume(Form1,Deriv,Form2)) :-
	obj__formula(Form1),
	obj__derivation(Deriv),
	obj__formula(Form2).
obj__derivation_step(cases(CaseL,Form)) :-
	obj__caseL(CaseL),
	obj__formula(Form).
obj__derivation_step(exist(NameL,Form1,Deriv,Form2)) :-
	obj__varL(NameL),
	obj__formula(Form1),
	obj__derivation(Deriv),
	obj__formula(Form2).
obj__derivation_step(induction(FormL,StepL)) :-
	obj__formulaL(FormL),
	obj__induction_stepL(StepL).
obj__derivation_step(contra(Form,Deriv)) :-
	obj__formula(Form),
	obj__derivation(Deriv).
obj__derivation_step(indirect([~,Form],Deriv)) :-
	obj__formula(Form),
	obj__derivation(Deriv).

%%d obj__caseL(grL::in)

obj__caseL([]).
obj__caseL([case(Form,Deriv)|CaseL]) :-
	obj__formula(Form),
	obj__derivation(Deriv),
	obj__caseL(CaseL).

%%d obj__induction_step(gr::in)

obj__induction_step(step(NameL,FormL,Deriv,Form)) :-
	obj__varL(NameL),
	obj__formulaL(FormL),
	obj__derivation(Deriv),
	obj__formula(Form).
	
%%d obj__induction_stepL(grL::in)

obj__induction_stepL([]).
obj__induction_stepL([Step|StepL]) :-
	obj__induction_step(Step),
	obj__induction_stepL(StepL).

%%d obj__substitution(gr::in)

obj__substitution([]).
obj__substitution([Name => Term|Sub]) :-
	atomic(Name),
	obj__term(Term),
	\+ obj__domain(Name,Sub),
	obj__substitution(Sub).

%%d obj__domain(gr::in,grL::in)

obj__domain(Name,Sub) :- lst__member(Name => _,Sub).

%%d obj__substitution2(grL::in)

obj__substitution2([]).
obj__substitution2([sub(Name,N,VarL,Form)|U]) :-
	atomic(Name),
	integer(N),
	obj__varL(VarL),
	obj__formula(Form), 
	\+ obj__domain2(Name,N,U),
	obj__substitution2(U).

%%d obj__domain2(gr::in,gr::in,grL::in)

obj__domain2(Name,N,U) :- lst__member(sub(Name,N,_,_),U).

%%d obj__make_varL(varL::in,tmL::out)
%%d obj__make_varL(varL::out,tmL::in)

obj__make_varL([],[]).
obj__make_varL([Name|NameL],[$(Name)|TermL]) :-
	obj__make_varL(NameL,TermL).

%%d obj__theorem(gr::in,gr::in,gr::in)

% obj__theorem(theorem(Ref,Form,Deriv)) :-
%	ctl__reference(Ref),
% 	obj__formula(Form),
% 	obj__derivation(Deriv).
% obj__theorem(lemma(Ref,Form,Deriv)) :-
%	ctl__reference(Ref),
% 	obj__formula(Form),
% 	obj__derivation(Deriv).
% obj__theorem(corollary(Ref,Form,Deriv)) :-
%	ctl__reference(Ref),
% 	obj__formula(Form),
% 	obj__derivation(Deriv).

% obj.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 6/11/95, 9:11 PM */
/* Updated: Tue Feb 16 14:38:50 1999 */
/* Filename: e2i.pl */
/* Abstract: From the external to the internal representation. */

%%c
%%c External representations:
%%c
%%c We use the following meta symbols:
%%c
%%c <c> <x> <x1> ... <xn>	for arbitrary Prolog constants.
%%c <f>				for Prolog constants different from
%%c				= <> ~ & \/ => <=> ? all ex tt ff
%%c				succeeds fails terminates def by gr
%%c
%%c External terms are Prolog terms of the following form:
%%c
%%c <c>				[n(<c>,0)]
%%c ?<x>			$(<x>)
%%c <f>(ETerm,...,ETerm)	[n(<f>,n),ETerm^,...,ETerm^]
%%c
%%c External atomic goals are Prolog terms of the following form:
%%c
%%c <c>				[n(<c>,0)]
%%c <f>(ETerm,...,ETerm)	[n(<f>,n),ETerm^,...,ETerm^]
%%c
%%c External goals are Prolog terms of the following form:
%%c
%%c ETerm = ETerm		[=,ETerm^,ETerm^]
%%c EAtom		        EAtom^
%%c ~ EGoal			[~,EGoal^]
%%c EGoal & ... & EGoal		[&,EGoal^,...,EGoal^]
%%c EGoal \/ ... \/ EGoal	[\/,EGoal^,...,EGoal^]
%%c
%%c External formulas are Prolog terms of the following form:
%%c
%%c tt				[&]
%%c ff				[\/]
%%c <c>				[n(<c>,0)]
%%c ETerm = ... = ETerm		[=,ETerm^,...,Eterm^]
%%c Eterm <> Eterm		[<>,ETerm^,ETerm^]
%%c EAtom		        EAtom^
%%c succeeds EAtom		[succeeds,EAtom^]
%%c fails EAtom			[fails,EAtom^]
%%c terminates EGoal		[terminates,EGoal^]
%%c def succeeds EAtom		[def,[succeeds,EAtom^]]
%%c def fails EAtom 		[def,[fails,EAtom^]]
%%c def terminates EGoal	[def,[terminates,EGoal^]]
%%c ~ EForm			[~,EForm^]
%%c EForm & ... & EForm		[&,EForm^,...,EForm^]
%%c EForm \/ ... \/ EForm	[\/,EForm^,...,EForm^]
%%c EForm => EForm		[=>,EForm^,EForm^]
%%c EForm <=> EForm		[<=>,EForm^,EForm^]
%%c all <x>: EForm		@(all,[<x>],EForm^)
%%c ex <x>: EForm		@(ex,[<x>],EForm^)
%%c all[<x1>,...,<xn>]: EForm	@(all,[<x1>,...,<xn>],EForm^)
%%c ex[<x1>,...,<xn>]: EForm	@(ex,[<x1>,...,<xn>],EForm^)
%%c
%%c External derivations are Prolog terms of the following form:
%%c
%%c EStep
%%c [EStep,...,EStep]
%%c
%%c External derivation steps are Prolog terms:
%%c 
%%c EForm
%%c
%%c assume(EForm,
%%c  EDeriv,
%%c  EForm)
%%c
%%c contra(EForm,
%%c  EDeriv)
%%c
%%c indirect(~ EFform,
%%c  EDeriv)
%%c
%%c cases(EForm,
%%c  EDeriv,
%%c  EForm,
%%c  EDeriv,
%%c  EForm)
%%c
%%c cases([
%%c  case(EForm,
%%c   EDeriv),
%%c  ...,
%%c  case(EForm,
%%c   EDeriv)],
%%c  EForm)
%%c
%%c exist([<x1>,...,<xn>],EForm,
%%c  EDeriv,
%%c  EForm)
%%c
%%c exist(<x>,EForm,
%%c  EDeriv,
%%c  EForm)
%%c
%%c induction([EForm,..,EForm],
%%c  [step([<x1>,...,<xn>],[EForm,...,EForm],
%%c    EDerv,
%%c    EForm),
%%c   ...,
%%c   step([<x1>,...,<xn>],[EForm,...,EForm],
%%c    EDerv,
%%c    EForm)])
%%c
%%c EForm by lemma(Ref)
%%c EForm by theorem(Ref)
%%c EForm by corollary(Ref)
%%c EForm by axiom(Ref)
%%c EForm by completion
%%c EForm by sld
%%c EForm by introduction(Name,N)
%%c EForm by elimination(Name,N)
%%c EForm by existence(Name,N)
%%c EForm by uniqueness(Name,N)
%%c EForm by concatenation
%%c EForm by builtin
%%c EForm by addition
%%c EForm by gap
%%c EForm by because
%%c EForm by comment(_)
%%c EForm by new(Ref)
%%c EForm by [debug,fact,ind,indqf,unfold,case,ex,elim,tot,comp,auto(N)]
%%c


%%d e2i__formula_tag(gr::out)

e2i__formula_tag(=>).
e2i__formula_tag(~).
e2i__formula_tag(succeeds).
e2i__formula_tag(fails).
e2i__formula_tag(terminates).
e2i__formula_tag(gr).
e2i__formula_tag(def).
e2i__formula_tag(<>).
e2i__formula_tag(<=>).

%%d e2i__flat_op(gr::out)

e2i__flat_op(&).
e2i__flat_op(\/).
e2i__flat_op(=).

%%d e2i__keyword(gr::out)

e2i__keyword(?).
e2i__keyword(tt).
e2i__keyword(ff).
e2i__keyword(all).
e2i__keyword(ex).
e2i__keyword(assume).
e2i__keyword(contra).
e2i__keyword(cases).
e2i__keyword(case).
e2i__keyword(indirect).
e2i__keyword(exist).
e2i__keyword(by).
e2i__keyword(induction).
e2i__keyword(step).


% The predicate `e2i__expression(X,Expr)' transforms a Prolog term `X'
% into an expression `Expr' of the following kind:
%
%     $(_)
%     [Tag,Expr,...,Expr]
%     @(all,_,Expr)
%     @(ex,_,Expr)
%
% `Tag' satisfies:
%
%     `e2i__formula(Tag)' or 
%     `e2i__flat_op(Tag)' or
%     `Tag = n(_,_)' or 
%     `Tag = d(_,_)' or
%     `Tag = f(_,_)'.
%
% The predicate `e2i__expression(X,Expr)' checks whether `X' is a ground
% term, i.e., does not contain Prolog variables.
%
% Conjunctions `&', disjunctions `\/' and equations `=' are flattened.
%
% Nested quantifiers of the same kind are flattened, too.

%%d e2i__expression(gr:in,exp::out)

e2i__expression(X,Result) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Result = [n(undef,0)]
	;	X = (? Y) ->
		Result = $(Y)
	;	X = (A & B) ->
		e2i__con(B,[],Expr1L),
		e2i__con(A,Expr1L,Expr2L),
		e2i__add_op(Expr2L,&,Result)
	;	X = (A \/ B) ->
		e2i__dis(B,[],Expr1L),
		e2i__dis(A,Expr1L,Expr2L),
		e2i__add_op(Expr2L,\/,Result)
	;	X = (A = B) ->
		e2i__eq(B,[],Expr1L),
		e2i__eq(A,Expr1L,Expr2L),
		Result = [=|Expr2L]
	;	X = (all Y: A) ->
		e2i__forall(A,Var1L,Expr),
		e2i__concat_variables(Y,Var1L,Var2L),
		Result = @(all,Var2L,Expr)
	;	X = (ex Y: A) ->
		e2i__exists(A,Var1L,Expr),
		e2i__concat_variables(Y,Var1L,Var2L),
		Result = @(ex,Var2L,Expr)
	;	X = ff ->
		Result = [\/]
	;	X = tt ->
		Result = [&]
	;	functor(X,Name,N),
		(	e2i__formula_tag(Name) ->
			Tag = Name
		;	def__defined_fun(Name,N) -> 
			Tag = f(Name,N)
		;	def__defined_pred(Name,N) ->
			Tag = d(Name,N)
		; 	Tag = n(Name,N)
		),
		X =.. [Name|XL],
		e2i__expressionL(XL,ExprL),
		Result = [Tag|ExprL]
	).

%%d e2i__expressionL(grL::in,expL::out)

e2i__expressionL([],[]).
e2i__expressionL([X|XL],[Expr|ExprL]) :-
	e2i__expression(X,Expr),
	e2i__expressionL(XL,ExprL).

%%d e2i__con(gr::in,expL::in,expL::out)

e2i__con(X,Expr1L,Expr3L) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Expr3L = []
	;	X = (A & B) ->
		e2i__con(B,Expr1L,Expr2L),
		e2i__con(A,Expr2L,Expr3L)
	;	X = tt ->
		Expr3L = Expr1L
	;	e2i__expression(X,Expr),
		Expr3L = [Expr|Expr1L]
	).

%%d e2i__dis(gr::in,expL::in,expL::out)
 
e2i__dis(X,Expr1L,Expr3L) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Expr3L = []
	;	X = (A \/ B) ->
		e2i__dis(B,Expr1L,Expr2L),
		e2i__dis(A,Expr2L,Expr3L)
	;	X = ff ->
		Expr3L = Expr1L
	;	e2i__expression(X,Expr),
		Expr3L = [Expr|Expr1L]
	).

%%d e2i__add_op(expL::in,gr::in,exp::out)

e2i__add_op(ExprL,Op,Expr) :-
	(	ExprL = [Expr] -> 
		true
	; 	Expr = [Op|ExprL]
	).

%%d e2i__forall(gr::in,varL::out,exp::out)

e2i__forall(X,Var2L,Expr) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Expr = [&]
	;	X = (all Y: A) ->
		e2i__forall(A,Var1L,Expr),
		e2i__concat_variables(Y,Var1L,Var2L)
	;	Var2L = [],
		e2i__expression(X,Expr)
	).

%%d e2i__exists(gr::in,varL::out,exp::out)

e2i__exists(X,Var2L,Expr) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Expr = [&]
	;	X = (ex Y: A) ->
		e2i__exists(A,Var1L,Expr),
		e2i__concat_variables(Y,Var1L,Var2L)
	;	Var2L = [],
		e2i__expression(X,Expr)
	).

%%d e2i__concat_variables(gr::in,grL::in,grL::out)

e2i__concat_variables(X,L1,L2) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,as,bound,variable]),
		L2 = []
	;	lst__append(X,L1,L2) ->
		true
	;	L2 = [X|L1]
	).


%%d e2i__eq(gr::in,expL::in,expL::out)

e2i__eq(X,Expr1L,Expr3L) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Expr3L = []
	;	X = (A = B) ->
		e2i__eq(B,Expr1L,Expr2L),
		e2i__eq(A,Expr2L,Expr3L)
	;	e2i__expression(X,Expr),
		Expr3L = [Expr|Expr1L]	
	).

%%d e2i__check_name(gr::in)

e2i__check_name(Name) :-
	(	e2i__keyword(Name) ->
		ctl__syntax([keyword,q(Name),at,wrong,place])
	;	true
	).

%%d e2i__check_term(gr::in)

e2i__check_term($(Name)) :-
	(	var(Name) ->
		ctl__syntax([prolog,variable,in,term])
	;	atomic(Name),
		e2i__check_name(Name)
	).
e2i__check_term([n(Name,_)|TermL]) :-
	e2i__check_name(Name),
	e2i__check_termL(TermL).
e2i__check_term([f(_,_)|TermL]) :-
	e2i__check_termL(TermL).
e2i__check_term($(Name)) :-
	\+ atomic(Name),
	ctl__syntax([q(Name),is,not,an,atomic,variable,name]).
e2i__check_term([d(Name,N)|_]) :-
	ctl__syntax([defined,predicate,Name/N,not,allowed,in,term]).
e2i__check_term(@(Quant,_,_)) :-
	ctl__syntax([quantifier,q(Quant),not,allowed,in,term]).
e2i__check_term([Tag|_]) :-
	atomic(Tag),
	ctl__syntax([q(Tag),not,allowed,in,term]).

%%d e2i__check_termL(grL::in)

e2i__check_termL([]).
e2i__check_termL([Term|TermL]) :-
	e2i__check_term(Term),
	e2i__check_termL(TermL).

%%d e2i__check_atomic_goal(gr::in)

e2i__check_atomic_goal([n(Name,_)|TermL]) :-
	e2i__check_name(Name),
	e2i__check_termL(TermL).
e2i__check_atomic_goal([d(Name,N)|_]) :-
	ctl__syntax([defined,predicate,Name/N,not,allowed,as,atomic,goal]).
e2i__check_atomic_goal([f(Name,N)|_]) :-
	ctl__syntax([defined,function,Name/N,not,allowed,as,atomic,goal]).
e2i__check_atomic_goal($(Name)) :-
	ctl__syntax([variable,q(? Name),not,allowed,as,atomic,goal]).
e2i__check_atomic_goal(@(Quant,_,_)) :-
	ctl__syntax([quantifier,q(Quant),not,allowed,in,atomic,goal]).
e2i__check_atomic_goal([Tag|_]) :-
	atomic(Tag),
	ctl__syntax([q(Tag),not,allowed,as,atomic,goal]).

%%d e2i__check_goal(gr::in)

e2i__check_goal([=,Term1,Term2]) :-		% equations
	e2i__check_term(Term1),
	e2i__check_term(Term2).
e2i__check_goal([n(Name,_)|TermL]) :-		% atomic goals
	e2i__check_name(Name),
	e2i__check_termL(TermL).
e2i__check_goal([~,Goal]) :-			% negated goals
	e2i__check_goal(Goal).
e2i__check_goal([&|GoalL]) :-			% conjunction: `true' is [&]
	e2i__check_goalL(GoalL).
e2i__check_goal([\/|GoalL]) :-			% disjunctions: `fail' is [\/]
	e2i__check_goalL(GoalL).
e2i__check_goal([d(Name,N)|_]) :-		% no defined predicates
	ctl__syntax([defined,predicate,Name/N,not,allowed,as,goal]).
e2i__check_goal([f(Name,N)|_]) :-		% no defined functions
	ctl__syntax([defined,function,Name/N,not,allowed,as,goal]).
e2i__check_goal($(Name)) :-			% no variables
	ctl__syntax([variable,q(? Name),not,allowed,as,goal]).
e2i__check_goal(@(Quant,_,_)) :-		% no quantifiers
	ctl__syntax([quantifier,q(Quant),not,allowed,in,goal]).
e2i__check_goal([=|ExprL]) :-			% `=' is binary
	\+ lst__two_elements(ExprL),
	ctl__syntax([q(=),is,binary,in,goals]).
e2i__check_goal([~|ExprL]) :-			% `~' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(~),is,unary]).
e2i__check_goal([Tag|_]) :-			% unknown tags
	atomic(Tag),
	\+ lst__member(Tag,[=,~,&,\/]),
	ctl__syntax([q(Tag),not,allowed,as,goal]).

%%d e2i__check_goalL(grL::in)

e2i__check_goalL([]).
e2i__check_goalL([Goal|GoalL]) :-
	e2i__check_goal(Goal),
	e2i__check_goalL(GoalL).

%%d e2i__check_sft_atom(gr::in)

e2i__check_sft_atom([succeeds,Atom]) :-		% succeeds
	e2i__check_atomic_goal(Atom).
e2i__check_sft_atom([fails,Atom]) :-		% fails
	e2i__check_atomic_goal(Atom).
e2i__check_sft_atom([terminates,Atom]) :-	% terminates
	e2i__check_atomic_goal(Atom).
e2i__check_sft_atom($(Name)) :-			% no variables
	ctl__syntax([variable,q(? Name),not,allowed,after,q(def)]).
e2i__check_sft_atom(@(Quant,_,_)) :-		% no quantifiers
	ctl__syntax([quantifier,q(Quant),not,allowed,after,q(def)]).
e2i__check_sft_atom([succeeds|ExprL]) :-	% `succeeds' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(succeeds),is,unary]).
e2i__check_sft_atom([fails|ExprL]) :-		% `fails' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(fails),is,unary]).
e2i__check_sft_atom([terminates|ExprL]) :-	% `terminates' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(terminates),is,unary]).
e2i__check_sft_atom([Tag|_]) :-			% unknown tags
	\+ lst__member(Tag,[succeeds,fails,terminates]),
	ctl__syntax([q(Tag),not,allowed,after,q(def)]).

%%d e2i__check_formula(gr::in)

e2i__check_formula([n(Name,_)|TermL]) :-	% predicate with clauses
	e2i__check_name(Name),
	e2i__check_termL(TermL).
e2i__check_formula([d(_,_)|TermL]) :-		% predicate as abbreviation
	e2i__check_termL(TermL).
e2i__check_formula([=|TermL]) :-		% equation
	e2i__check_termL(TermL).
e2i__check_formula([<>,Term1,Term2]) :-		% inequation
	e2i__check_term(Term1),
	e2i__check_term(Term2).
e2i__check_formula([gr,Term]) :-		% gr
	e2i__check_term(Term).
e2i__check_formula([succeeds,Atom]) :-		% succeeds
	e2i__check_atomic_goal(Atom).
e2i__check_formula([fails,Atom]) :-		% fails
	e2i__check_atomic_goal(Atom).
e2i__check_formula([terminates,Goal]) :-	% terminates
	e2i__check_goal(Goal).
e2i__check_formula([def,Form]) :-		% def
	e2i__check_sft_atom(Form).
e2i__check_formula([~,Form]) :-			% negation
	e2i__check_formula(Form).
e2i__check_formula([&|FormL]) :-		% conjunction
	e2i__check_formulaL(FormL).
e2i__check_formula([\/|FormL]) :-		% disjunction
	e2i__check_formulaL(FormL).
e2i__check_formula([=>,Form1,Form2]) :-		% implication
	e2i__check_formula(Form1),
	e2i__check_formula(Form2).
e2i__check_formula([<=>,Form1,Form2]) :-	% equivalence
	e2i__check_formula(Form1),
	e2i__check_formula(Form2).
e2i__check_formula(@(all,NameL,Form)) :-	% universial quantifier
	e2i__check_formula(Form),
	obj__varL(NameL).
e2i__check_formula(@(ex,NameL,Form)) :-		% existential quantifier
	e2i__check_formula(Form),
	obj__varL(NameL).
e2i__check_formula($(Name)) :-			% no variables
	ctl__syntax([variable,q(? Name),not,allowed,as,formula]).
e2i__check_formula([f(Name,N)|_]) :-		% no defined functions
	ctl__syntax([defined,function,Name/N,not,allowed,as,predicate]).
e2i__check_formula([<>|ExprL]) :-		% `<>' is binary
	\+ lst__two_elements(ExprL),
	ctl__syntax([q(<>),is,binary]).
e2i__check_formula([gr|ExprL]) :-		% `gr' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(gr),is,unary]).
e2i__check_formula([succeeds|ExprL]) :-		% `succeeds' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(succeeds),is,unary]).
e2i__check_formula([fails|ExprL]) :-		% `fails' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(fails),is,unary]).
e2i__check_formula([terminates|ExprL]) :-	% `terminates' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(terminates),is,unary]).
e2i__check_formula([def|ExprL]) :-		% `def' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(def),is,unary]).
e2i__check_formula([~|ExprL]) :-		% `~' is unary
	\+ lst__singleton(ExprL),
	ctl__syntax([q(~),is,unary]).
e2i__check_formula([=>|ExprL]) :-		% `=>' is binary
	\+ lst__two_elements(ExprL),
	ctl__syntax([q(=>),is,binary]).
e2i__check_formula([<=>|ExprL]) :-		% `<=>' is binary
	\+ lst__two_elements(ExprL),
	ctl__syntax([q(<=>),is,binary]).
e2i__check_formula(@(all,NameL,_)) :-		% bound variables
	\+ obj__varL(NameL),
	ctl__syntax([NameL,is,not,a,list,of,bound,variables]).
e2i__check_formula(@(ex,NameL,_)) :-		% bound variables
	\+ obj__varL(NameL),
	ctl__syntax([NameL,is,not,a,list,of,bound,variables]).
e2i__check_formula([Tag|_]) :-			% unknown tags
	atomic(Tag),
	\+ e2i__formula_tag(Tag),
	\+ e2i__flat_op(Tag),
	ctl__syntax([q(Tag),cannot,be,used,as,a,formula]).

%%d e2i__check_formulaL(grL::in)

e2i__check_formulaL([]).
e2i__check_formulaL([Form|FormL]) :-
	e2i__check_formula(Form),
	e2i__check_formulaL(FormL).

%%d e2i__formula(gr::in,fml::out)

e2i__formula(X,Form) :-
	(	e2i__expression(X,Form),
		(	\+ db__flag(check_everything)
		;	e2i__check_formula(Form)
		) -> true
	;	ctl__syntax([p(X),cannot,be,parsed]),
		Form = [&]
	).

%%d e2i__formulaL(gr::in,fmlL::out)

e2i__formulaL(X,Result) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,formula]),
		Result = []
	;	X = [] ->
		Result = []
	;	X = [Y|YL] ->
		e2i__formula(Y,Form),
		e2i__formulaL(YL,FormL),
		Result = [Form|FormL]
	).


%%d e2i__derivation(gr::in,drv::out)

e2i__derivation(X,Deriv) :-
	(	e2i__derivation_steps(X,Deriv) ->
		true
	;	ctl__syntax([p(X),cannot,be,parsed])
	).


%%d e2i__derivation_steps(gr::in,drv::out)

e2i__derivation_steps(X,Result) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,derivation]),
		Result = []
	;	lst__list_form(X) ->
		e2i__derivation_list(X,Result)
	;	e2i__derivation_step(X,Step),
		Result = [Step]
	).

%%d e2i__derivation_steps(grL::in,drv::out)

e2i__derivation_list(X,Result) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,derivation]),
		Result = []
	;	X = [] ->
		Result = []
	;	X = [Y|YL] ->
		e2i__derivation_step(Y,Step),
		e2i__derivation_list(YL,Deriv),
		Result = [Step|Deriv]
	;	ctl__syntax([p(X),is,not,a,list,of,derivation,steps]),
		Result = []
	).

%%d e2i__derivation_step(gr::in,dstp::out)

e2i__derivation_step(S,Result) :-
	(	var(S) ->
		ctl__syntax([prolog,variable,in,derivation]),
		Result = [&]
	;	S = assume(X,Y,Z) ->
		e2i__formula(X,Form1),
		e2i__derivation(Y,Deriv),
		e2i__formula(Z,Form2),
		Result = assume(Form1,Deriv,Form2)
	;	S = contra(X,Y) ->
		e2i__formula(X,Form1),
		e2i__derivation(Y,Deriv),
		Result = contra(Form1,Deriv)
	;	S = cases(X1,Y1,X2,Y2,X) ->
	 	e2i__formula(X1,Form1),
		e2i__derivation(Y1,Deriv1),
		e2i__formula(X2,Form2),
		e2i__derivation(Y2,Deriv2),
		e2i__formula(X,Form),
		Result = cases([case(Form1,Deriv1),case(Form2,Deriv2)],Form)
	;	S = cases(XL,X) ->
		e2i__cases(XL,CaseL),
		e2i__formula(X,Form),
		Result = cases(CaseL,Form)
	; 	S = indirect(X,Y) ->
		e2i__formula(X,Form),
		e2i__derivation(Y,Deriv),
		Result = indirect(Form,Deriv)
	; 	S = exist(X,X1,Y,X2) ->
		e2i__bound_variables(X,VL),
		e2i__formula(X1,Form1),
		e2i__derivation(Y,Deriv),
		e2i__formula(X2,Form2),
		Result = exist(VL,Form1,Deriv,Form2)
	;	S = by(X,Y) ->
		e2i__formula(X,Form),
		Result = by(Form,Y)
	;	S = induction(XL,YL) ->
		e2i__formulaL(XL,FormL),
		e2i__inductionL(YL,StepL),
		Result = induction(FormL,StepL)
	; 	e2i__formula(S,Result)
	).

%%d e2i__bound_variables(gr::in,varL::out)

e2i__bound_variables(X,VarL) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,derivation]),
		VarL = []
	;	atomic(X) ->
		VarL = [X]
	;	obj__varL(X) ->
		VarL = X
	;	ctl__syntax([X,is,not,a,list,of,bound,variables]),
		VarL = []
	).
	
%%d e2i__cases(grL::in,casL::out)

e2i__cases(C,Result) :-
	(	var(C) ->
		ctl__syntax([prolog,variable,in,derivation]),
		Result = []
	;	C = [] ->
		Result = []
	; 	C = [case(X,Y)|XL] ->
		e2i__formula(X,Form),
		e2i__derivation(Y,Deriv),
		e2i__cases(XL,CaseL),
		Result = [case(Form,Deriv)|CaseL]
	;	ctl__syntax([p(C),is,not,a,case,list]),
		Result = []
	).

%%d e2i__inductionL(grL::in,istpL::out)

e2i__inductionL(X,Result) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,derivation]),
		Result = []
	;	X = [] ->
		Result = []
	;	X = [step(VL,YL,Y,Z)|XL] ->
		e2i__formulaL(YL,FormL),
		e2i__derivation(Y,Deriv),
		e2i__formula(Z,Form),
		e2i__inductionL(XL,StepL),
		Result = [step(VL,FormL,Deriv,Form)|StepL]
	;	ctl__syntax([p(X),is,not,a,list,of,induction,steps]),
		Result = []
	).

% e2i.pl ends here
/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 6/11/95, 9:15 PM */
/* Filename: i2e.pl */
/* Abstract: From the internal to the external representation. */

%%d i2e__arity_form(gr::in)

i2e__arity_form(n(_,_)).
i2e__arity_form(d(_,_)).
i2e__arity_form(f(_,_)).

%%d i2e__assoc_tag(gr::in)

i2e__assoc_tag(=).
i2e__assoc_tag(&).
i2e__assoc_tag(\/).

%%d i2e__expression(exp::in,gr::out)

i2e__expression($(X),? X).
i2e__expression([=|ExprL],X) :- i2e__eq(ExprL,X).
i2e__expression([&|ExprL],X) :- i2e__con(ExprL,X).
i2e__expression([\/|ExprL],X) :- i2e__dis(ExprL,X).
i2e__expression([n(Name,_)|ExprL],X) :-
	i2e__expressionL(ExprL,XL),
	X =.. [Name|XL].
i2e__expression([d(Name,_)|ExprL],X) :-
	i2e__expressionL(ExprL,XL),
	X =.. [Name|XL].
i2e__expression([f(Name,_)|ExprL],X) :-
	i2e__expressionL(ExprL,XL),
	X =.. [Name|XL].
i2e__expression([Tag|ExprL],X) :-		% ~, <>, =>, <=>, def, gr, 
	\+ i2e__arity_form(Tag),		% succeeds, fails, terminates
	\+ i2e__assoc_tag(Tag),
	i2e__expressionL(ExprL,XL),
	X =.. [Tag|XL].
i2e__expression(@(all,VarL,Expr),all X1: X2) :-
	i2e__flatten_singleton(VarL,X1),
	i2e__expression(Expr,X2).
i2e__expression(@(ex,VarL,Expr),ex X1: X2) :-
	i2e__flatten_singleton(VarL,X1),
	i2e__expression(Expr,X2).

%%d i2e__expressionL(expL::in,grL::out)

i2e__expressionL([],[]).
i2e__expressionL([Expr|ExprL],[X|XL]) :-
	i2e__expression(Expr,X),
	i2e__expressionL(ExprL,XL).

%%d i2e__eq(expL::in,gr::out)

% i2e__eq([$(1),$(2),$(3)], (?1 = ?2) = ?3).

i2e__eq([], tt).
i2e__eq([Expr|ExprL],X2) :-
	i2e__expression(Expr,X1),
	i2e__eqL(ExprL,X1,X2).

%%d i2e__eqL(expL::in,gr::in,gr::out)

i2e__eqL([],X,X).
i2e__eqL([Expr|ExprL],X1,X3) :-
	i2e__expression(Expr,X2),
	i2e__eqL(ExprL,X1 = X2,X3).

%%d i2e__con(expL::in,gr::out)

i2e__con([], tt).
i2e__con([Expr|ExprL],X2) :-
	i2e__expression(Expr,X1),
	i2e__conL(ExprL,X1,X2).

%%d i2e__conL(expL::in,gr::in,gr::out)

i2e__conL([],X,X).
i2e__conL([Expr|ExprL],X1,X3) :-
	i2e__expression(Expr,X2),
	i2e__conL(ExprL,X1 & X2,X3).

%%d i2e__dis(expL::in,gr::out)

i2e__dis([], ff).
i2e__dis([Expr|ExprL],X2) :-
	i2e__expression(Expr,X1),
	i2e__disL(ExprL,X1,X2).

%%d i2e__disL(expL::in,gr::in,gr::out)

i2e__disL([],X,X).
i2e__disL([Expr|ExprL],X1,X3) :-
	i2e__expression(Expr,X2),
	i2e__disL(ExprL,X1 \/ X2,X3).

%%d i2e__flatten_singleton(gr::in,gr::out)

i2e__flatten_singleton(X,Z) :-
	(	X = [Y] -> 
		Z = Y
	; 	Z = X
	).

%%d i2e__derivation(drv::in,gr::out)

i2e__derivation(Deriv,X) :-
	i2e__derivationL(Deriv,XL),
	i2e__flatten_singleton(XL,X).

%%d i2e__derivationL(drv::in,grL::out)

i2e__derivationL([],[]).
i2e__derivationL([Step|Deriv],[X|XL]) :-
	i2e__derivation_step(Step,X),
	i2e__derivationL(Deriv,XL).

%%d i2e__proof_form(gr::in)

i2e__proof_form(assume(_,_,_)).
i2e__proof_form(contra(_,_)).
i2e__proof_form(cases(_,_)).
i2e__proof_form(indirect(_,_)).
i2e__proof_form(exist(_,_,_,_)).
i2e__proof_form(by(_,_)).
i2e__proof_form(induction(_,_)).

%%d i2e__derivation_step(dstp::in,gr::out)

i2e__derivation_step(assume(Form1,Deriv,Form2),assume(X1,Y,X2)) :-
	i2e__expression(Form1,X1),
	i2e__expression(Form2,X2),
	i2e__derivation(Deriv,Y).
i2e__derivation_step(contra(Form,Deriv),contra(X,Y)) :-
	i2e__expression(Form,X),
	i2e__derivation(Deriv,Y).
i2e__derivation_step(indirect(Form,Deriv),indirect(X,Y)) :-
	i2e__expression(Form,X),
	i2e__derivation(Deriv,Y).
i2e__derivation_step(cases([case(Form1,Deriv1),case(Form2,Deriv2)],Form),
	cases(X1,Y1,X2,Y2,X)) :-
	i2e__expression(Form1,X1),
	i2e__expression(Form2,X2),
	i2e__expression(Form,X),
	i2e__derivation(Deriv1,Y1),
	i2e__derivation(Deriv2,Y2).
i2e__derivation_step(cases(CaseL,Form),cases(XL,X)) :-
	\+ lst__two_elements(CaseL),
	i2e__expression(Form,X),
	i2e__cases(CaseL,XL).
i2e__derivation_step(exist(VarL,Form1,Deriv,Form2),exist(X,X1,Y,X2)) :-
	i2e__flatten_singleton(VarL,X),
	i2e__expression(Form1,X1),
	i2e__expression(Form2,X2),
	i2e__derivation(Deriv,Y).
i2e__derivation_step(by(Form,Y),by(X,Y)) :-
	i2e__expression(Form,X).
i2e__derivation_step(induction(FormL,StepL),induction(XL,YL)) :-
	i2e__expressionL(FormL,XL),
	i2e__induction_steps(StepL,YL).
i2e__derivation_step(F,X) :-			% in the case of formulas
	\+ i2e__proof_form(F),
	i2e__expression(F,X).

%%d i2e__cases(casL::in,grL::out)

i2e__cases([],[]).
i2e__cases([case(Form,Deriv)|CaseL],[case(X,Y)|XL]) :-
	i2e__expression(Form,X),
	i2e__derivation(Deriv,Y),
	i2e__cases(CaseL,XL).

%%d i2e__induction_steps(istpL::in,grL::out)

i2e__induction_steps([],[]).
i2e__induction_steps([step(VarL,FormL,Deriv,Form)|StepL],
	[step(VarL,XL,X,Y)|YL]) :-
	i2e__expressionL(FormL,XL),
	i2e__derivation(Deriv,X),
	i2e__expression(Form,Y),
	i2e__induction_steps(StepL,YL).

%%d i2e__subL(sub::in,grL::out)

i2e__subL([],[]).
i2e__subL([X => Term|Sub],[X => Y|Z]) :-
	i2e__expression(Term,Y),
	i2e__subL(Sub,Z).

%%d i2e__class(cl::in,gr::out)

i2e__class(cl(Term,Part),cl(X,Y)) :-
	i2e__expression(Term,X),
	i2e__partition(Part,Y).

%%d i2e__partition(ptn::in,gr::out)

i2e__partition([],[]).
i2e__partition([Class|Part],[X|Y]) :-
	i2e__class(Class,X),
	i2e__partition(Part,Y).

%%d i2e__theorem(thm::in,gr::out)

i2e__theorem(theorem(Ref,Form,Deriv),theorem(Ref,EForm,EDeriv)) :-
	i2e__expression(Form,EForm),
	i2e__derivation(Deriv,EDeriv).
i2e__theorem(lemma(Ref,Form,Deriv),lemma(Ref,EForm,EDeriv)) :-
	i2e__expression(Form,EForm),
	i2e__derivation(Deriv,EDeriv).
i2e__theorem(corollary(Ref,Form,Deriv),corollary(Ref,EForm,EDeriv)) :-
	i2e__expression(Form,EForm),
	i2e__derivation(Deriv,EDeriv).

% i2e.el ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:54:48 1994 */
/* Updated: Mon Feb 22 12:53:28 1999 */
/* Filename: pr.pl (proofs) */
/* Abstract: The kernel of the system. What is a proof? */


%% Variable conventions in this file:
%%
%% Deriv	derivation
%% Form		formula
%% Gamma	list of formulas
%% V		list of variables
%% BV		list of bound variables
%% Step		step in a derivation or induction

%%c
%%c Derivations are Prolog terms of the following forms:
%%c
%%c [Step,...,Step] where each Step has of one of the following forms:
%%c 
%%c Form
%%c assume(Form,Deriv,Form)
%%c contra(Form,Deriv)
%%c cases([case(Form,Deriv),...],Form)
%%c indirect([~,Form],Deriv)
%%c exist(BV,Form,Deriv,Form)
%%c induction(FormL,[step(BV,FormL,Deriv,Form),...])
%%c by(Form,comment(_))
%%c by(Form,completion)
%%c by(Form,gap)
%%c by(Form,elimination(Name,N))
%%c by(Form,introduction(Name,N))
%%c by(Form,existence(Name,N))
%%c by(Form,uniqueness(Name,N))
%%c by(Form,theorem(_))
%%c by(Form,lemma(_))
%%c by(Form,corollary(_))
%%c by(Form,[l(N),r(N),ind,cmp,uni,thm,dis,ex])
%%c


%% The predicates in this file are correct with respect to the following
%% interpretation (Gamma1 |- Gamma2 means that every formula from the list
%% Gamma2 is derivable in classical logic from the formulas in Gamma1):
%%
%% pr__proof(Deriv,V,Gamma1,Gamma2) has to be understood as a function,
%% that given Deriv, V, Gamma1 checks Deriv and computes Gamma2.
%% 
%% pr__proof(Deriv,V,Gamma1,Gamma2) => for all Gamma3, if FV(Gamma3) is
%%   contained in V and Gamma3 |- Gamma1 then Gamma3 |- Gamma2.
%% 
%% pr__step(Deriv,V,Gamma1,Form) => for all Gamma2, if FV(Gamma2) is
%%   contained in V and Gamma2 |- Gamma1 then Gamma2 |- Form.
%% 
%% pr__derivable(V,Gamma1,Form,_) => for all Gamma2, if FV(Gamma2) is
%%   contained in V and Gamma2 |- Gamma1 then Gamma2 |- Form.
%% 
%% pr__by(_,V,Gamma1,Form1,Form2) => for all Gamma2, if FV(Gamma2) is
%%   contained in V and Gamma2 |- Gamma1 then
%%   Gamma2 |- Form1 and Gamma2 |- Form2.
%% 
%% pr__axiom(Form) => |- Form
%% 
%% pr__inconsistent(Gamma) => Gamma |- ff
%% 
%% pr__equivalent(Form1,Form2) => |- Form1 <=> Form2
%% 

%%d pr__proof(drv::in,varL::in,fmlL::in,fmlL::out)

pr__proof([],_,Gamma,Gamma).
pr__proof([Step|Deriv],V,Gamma1,Gamma2) :-
	pr__step_err(Step,V,Gamma1,Form),
	pr__proof(Deriv,V,[Form|Gamma1],Gamma2).

%%d pr__step(dstp::in,varL::in,fmlL::in,fml::out)

pr__step(Form1 by X,V,Gamma,Form2) :-			% step: by
	obj__formula(Form1),
	pr__by(X,V,Gamma,Form1,Form2),
	pr__statistics(step_by).
pr__step([Tag|FormL],V,Gamma,[Tag|FormL]) :-		% step: trivial 
	obj__formula([Tag|FormL]),
	pr__derivable_once(V,Gamma,[Tag|FormL]),
	pr__statistics(step_trivial).
pr__step(@(Tag,BV,Form),V,Gamma,@(Tag,BV,Form)) :-
	obj__formula(@(Tag,BV,Form)),
	pr__derivable_once(V,Gamma,@(Tag,BV,Form)),
	pr__statistics(step_trivial).
pr__step(assume(Form1,Deriv,Form2),V1,Gamma1,		% step: assume
	[=>,Form1,Form2]) :-
	obj__formula(Form1),
	obj__formula(Form2),
	eq__add_free(Form1,V1,V2),
	pr__proof(Deriv,V2,[Form1|Gamma1],Gamma2),
	pr__derivable_err(V2,Gamma2,Form2),
	pr__statistics(step_assume).
pr__step(cases(CaseL,Form),V,Gamma,Form) :-		% step: cases
	obj__formula(Form),
	pr__cases_formula(CaseL,FormL),
	pr__derivable_err(V,Gamma,[\/|FormL]),
	pr__cases(CaseL,V,Gamma,Form),
	pr__statistics(step_cases).
pr__step(exist(BV,Form1,Deriv,Form2),V1,Gamma,Form2) :-	% step: exist
	obj__formula(Form1),
	obj__formula(Form2),
	obj__varL(BV),
	lst__disjoint(V1,BV),
	eq__free(Form2,BV),
	pr__derivable_err(V1,Gamma,@(ex,BV,Form1)),
	eq__add_free(Form1,V1,V2),
	pr__proof(Deriv,V2,[Form1|Gamma],Gamma1),
	pr__derivable_err(V2,Gamma1,Form2),
	pr__statistics(step_exist).
pr__step(induction(FormL,StepL),V,Gamma,Form) :- 	% step: induction
	ind__pr_induction(V,Gamma,FormL,StepL),
	pr__conjunction(FormL,Form),
	pr__statistics(step_induction).
pr__step(contra(Form,Deriv),V1,Gamma1,[~,Form]) :-	% step: contra
	obj__formula(Form),
	eq__add_free(Form,V1,V2),
	pr__proof(Deriv,V2,[Form|Gamma1],Gamma2),
	pr__inconsistent(Gamma2),
	pr__statistics(step_contra).
pr__step(indirect([~,Form],Deriv),V1,Gamma1,Form) :-	% step: indirect
	obj__formula(Form),
	eq__add_free(Form,V1,V2),
	pr__proof(Deriv,V2,[[~,Form]|Gamma1],Gamma2),
	pr__inconsistent(Gamma2),
	pr__statistics(step_indirect).

%%d pr__cases_formula(casL::in,fmlL::out)

pr__cases_formula([],[]).
pr__cases_formula([case(Form,_)|CaseL],[Form|FormL]) :-
	obj__formula(Form),
	pr__cases_formula(CaseL,FormL).

%%d pr__cases(varL::in,fmlL::in,casL::in,fml::out)

pr__cases([],_,_,_).
pr__cases([case(Form1,Deriv)|CaseL],V1,Gamma,Form2) :-
	eq__add_free(Form1,V1,V2),
	pr__proof(Deriv,V2,[Form1|Gamma],Gamma1),
	pr__derivable_err(V2,Gamma1,Form2),
	pr__cases(CaseL,V1,Gamma,Form2).

%%d pr__add_assumptionL(fmlL::in,varL::in,fmlL::in,varL::out,fmlL::out)

pr__add_assumptionL(FormL,V1,Gamma1,V2,Gamma2) :-
	eq__add_free_boundL(FormL,[],V1,V2),
	lst__concat(FormL,Gamma1,Gamma2).

%%d pr__conjunction(fmlL::in,fml::out)

pr__conjunction(FormL,Form) :-
	(	lst__singleton(FormL) ->
		FormL = [Form]
	;	Form = [&|FormL]
	).

%%d pr__derivable_once(varL::in,fmlL::in,fml::in)

% DEPTH OF THINKING = 6

pr__derivable_once(V,Gamma,Form) :- 
	once(pr__derivable(V,Gamma,Form,s(s(s(s(s(s(0)))))))).


%%d pr__derivable_once(varL::in,fmlL::in,fml::in,nat::in)

pr__derivable_once(V,Gamma,Form,N) :-
	once(pr__derivable(V,Gamma,Form,N)).

%%d pr__derivable(varL::in,fmlL::in,fml::in,nat::in)

pr__derivable(V,Gamma,Form,N) :-
	db__flag(debug),
	once(pr__debug(V,Gamma,Form,N)),
	fail.

pr__derivable(_,Gamma,Form,_) :-			% identity axiom
	lst__member_con_check(Form,Gamma),
	pr__statistics(rule_identity_axiom).
pr__derivable(V,Gamma,[&|FormL],s(N)) :-		% and intro
	pr__derivableL(FormL,V,Gamma,N),
	pr__statistics(rule_and_intro).
pr__derivable(V,Gamma,@(all,BV,Form),s(N)) :-		% forall intro
	lst__disjoint(V,BV),
	pr__derivable_once(V,Gamma,Form,N),
	pr__statistics(rule_forall_intro).
pr__derivable(_,Gamma,Form1,_) :-			% modus ponens match
	lst__member_con(Form2,Gamma),
	(	obj__forall_form(Form2)
	;	obj__implication_form(Form2)
	),
	pr__match_kernel(Form2,Form1,[],[],Sub,FormL),
	once(pr__match_args(FormL,Gamma,Sub)),
	pr__statistics(rule_modus_ponens_match).
pr__derivable(V,Gamma,[\/|FormL],s(N)) :-		% or intro
	lst__member(Form,FormL),
	pr__derivable_once(V,Gamma,Form,N),
	pr__statistics(rule_or_intro).
pr__derivable(V,Gamma,Form2,s(N)) :-			% modus ponens
	lst__member_con([=>,Form1,Form2],Gamma),
	pr__derivable_once(V,Gamma,Form1,N),
	pr__statistics(rule_modus_ponens_plain).
pr__derivable(_,Gamma,_,_) :-				% inconsistent
	pr__inconsistent(Gamma),
	pr__statistics(rule_inconsistent).
pr__derivable(V,Gamma,Form1,s(N)) :-			% modus ponens deriv
	lst__member_con(@(all,BV,[=>,Form2,Form3]),Gamma),
	pr__match_conclusion(Form3,Form1,BV,Sub),
	eq__apply_plain(Form2,Sub,Form4),
	pr__derivable_once(V,Gamma,Form4,N),
	pr__statistics(rule_modus_ponens_deriv).
pr__derivable(_,Gamma,@(ex,BV,Form1),_) :-		% exists intro
	lst__member_con(Form2,Gamma),
	\+ obj__exists_form(Form2),
	eq__match_constrained(Form1,Form2,BV,[],_),
	pr__statistics(rule_exists_intro).
pr__derivable(_,Gamma,@(ex,BV,Form),_) :-		% exists intro match
	pr__match_assumption(Gamma,Form,BV,[],_),
	pr__statistics(rule_exists_intro_match).
pr__derivable(_,_,Form,_) :-				% logical axiom
	pr__axiom(Form),
	pr__statistics(rule_special_axiom).
pr__derivable(V,Gamma,[=>,_,Form],s(N)) :-		% implication intro r
	pr__derivable_once(V,Gamma,Form,N),
	pr__statistics(rule_implication_intro_right).
pr__derivable(_,Gamma,[gr,$(X)],_) :-			% gr intro variable
	lst__member_con([gr,[n(_,_)|TermL]],Gamma),
	lst__member(Term,TermL),
	eq__occurs_qf(Term,X),
	obj__pure_term(Term),
	pr__statistics(rule_gr_intro_variable).
pr__derivable(V,Gamma,[gr,[n(_,_)|TermL]],s(N)) :- 	% gr intro term
	obj__pure_termL(TermL),
	eq__add_free_boundL(TermL,[],[],XL),
	cmp__map_gr(XL,[],FormL),
	pr__derivableL(FormL,V,Gamma,N),
	pr__statistics(rule_gr_intro_term).
pr__derivable(V,Gamma,[terminates,[&,Goal|GoalL]],s(N)) :-
	cmp__terminates_goal(Goal,Form1),
	pr__derivable_once(V,Gamma,Form1,N),
	(	cmp__terminates_goal([&|GoalL],Form2), 	% t intro t
		pr__derivable_once(V,Gamma,Form2,N),
		pr__statistics(rule_t_intro_t)
	;	cmp__succeeds_goal(Goal,Form2),		% t intro s
		cmp__terminates_goal([&|GoalL],Form3),
		pr__derivable_once(V,Gamma,[=>,Form2,Form3],N),
		pr__statistics(rule_t_intro_s)
	;	cmp__fails_goal(Goal,Form2),		% t intro f
		pr__derivable_once(V,Gamma,Form2,N),
		pr__statistics(rule_t_intro_f)
	).
pr__derivable(_,Gamma,Form,_) :-			% cet unification
	mgu__unify_gamma(Gamma,Result),
	pr__derivable_cet(Gamma,Result,Form),
	pr__statistics(rule_cet_unification).
pr__derivable(_,Gamma,Form,_) :-			% equality trivial
	pr__extract_eqs(Gamma,[],TermLL),
	TermLL = [_|_],
	pr__derivable_eq(Gamma,TermLL,Form),
	pr__statistics(rule_equality_trivial).
pr__derivable(_,Gamma,[succeeds,[Tag|TermL]],_) :- 	% sld step
	cmp__match_clause(Tag,TermL,Gamma),
	pr__statistics(rule_sld_step).
pr__derivable(V,Gamma,[Op,Atom],s(N)) :-		% user defined intro
	obj__sft_op(Op),
	bi__user_defined_atom(Atom),
	cmp__sft_formula(Op,Atom,Form),
	pr__derivable_once(V,Gamma,Form,N),
	pr__statistics(rule_user_defined_intro).
pr__derivable(_,Gamma,[\/,[succeeds,Goal],[fails,Goal]],_) :-
	lst__member_con_check([terminates,Goal],Gamma),
	pr__statistics(rule_totality).			% totality
pr__derivable(_,Gamma,[\/,[fails,Goal],[succeeds,Goal]],_) :-
	lst__member_con_check([terminates,Goal],Gamma),
	pr__statistics(rule_totality).
pr__derivable(V,Gamma,[Op,Atom],s(N)) :-		% built-in intro
	obj__sft_op(Op),
	bi__builtin_atom(Atom),
	bi__typed(Atom),
	bi__eval(Atom,Goal),
	cmp__op_goal(Op,Goal,Form),
	pr__derivable_once(V,Gamma,Form,N),
	pr__statistics(rule_built_in_intro).
pr__derivable(_,Gamma,[<=>,Form1,Form2],_) :-		% equivalence intro
	lst__member_con_check([=>,Form1,Form2],Gamma),
	lst__member_con_check([=>,Form2,Form1],Gamma),
	pr__statistics(rule_equivalence_intro).
pr__derivable(V,Gamma,[=>,Form,_],s(N)) :-		% implication intro l
	pr__derivable_once(V,Gamma,[~,Form],N),
	pr__statistics(rule_implication_intro_left).
pr__derivable(_,Gamma,Form1,_) :-			% trivial equivalencies
	pr__equivalent(Form1,Form2),
	lst__member_con_check(Form2,Gamma),
	pr__statistics(rule_trivial_equivalencies).
pr__derivable(_,Gamma,[=,Term1,Term2],_) :-		% cet injective
	lst__member_con([=,[n(R,N)|Term1L],[n(R,N)|Term2L]],Gamma),
	pr__cet_injective(Term1L,Term2L,Term1,Term2),
	pr__statistics(rule_cet_injective).
pr__derivable(_,Gamma,[\/|Form1L],_) :-			% or intro subset
	lst__member_con([\/|Form2L],Gamma),
	lst__subset(Form2L,Form1L),
	pr__statistics(rule_or_intro_subset).
pr__derivable(V,Gamma,@(all,BV1,Form1),_) :-		% forall intro alpha
	lst__member_con(Form2,Gamma),
	\+ obj__forall_form(Form2),
	eq__match(Form2,Form1,Sub),
	eq__sub_forall(Sub,V,BV1,[]),
	pr__statistics(rule_forall_intro_alpha).
pr__derivable(_,Gamma,Form2,_) :-			% equivalence elim
	(	lst__member_con([<=>,Form1,Form2],Gamma)
	;	lst__member_con([<=>,Form2,Form1],Gamma)
	),
	lst__member_con_check(Form1,Gamma),
	pr__statistics(rule_equivalence_elim).
pr__derivable(V1,Gamma,[=>,Form1,Form2],s(N)) :- 	% implication intro
	eq__add_free(Form1,V1,V2),
	pr__derivable_once(V2,[Form1|Gamma],Form2,N),
	pr__statistics(rule_implication_intro).
pr__derivable(_,Gamma,Form1,_) :-			% alpha conversion
	lst__member_con(Form2,Gamma),
	eq__alpha(Form1,Form2),
	pr__statistics(rule_alpha_conversion).

%%d pr__derivableL(fmlL::in,varL::in,fmlL::in,nat::in)

pr__derivableL([],_,_,_).
pr__derivableL([Form|FormL],V,Gamma,N) :-
	pr__derivable_once(V,Gamma,Form,N),
	pr__derivableL(FormL,V,Gamma,N).

%%d pr__axiom(fml::in)

pr__axiom([=,Term,Term]) :-				% equal terms
	pr__statistics(axiom_equal_terms).
pr__axiom([gr,[n(_,0)]]) :-				
	pr__statistics(axiom_gr_constant).		% ground constants
pr__axiom([~,[=,Term1,Term2]]) :-			% not unifiable terms
	obj__pure_term(Term1),
	obj__pure_term(Term2),
	\+ mgu__unifiable_terms(Term1,Term2),
	pr__statistics(axiom_not_unifiable_terms).
pr__axiom([<>,[n(Name1,N1)|_],[n(Name2,N2)|_]]) :-	% function clash
	\+ (Name1 = Name2, N1 = N2),
	pr__statistics(axiom_function_clash).
pr__axiom([\/|FormL]) :-				% tertium non datur
	(	lst__member([~,Form],FormL),
		lst__member_check(Form,FormL)
	;	lst__member([<>,Term1,Term2],FormL),
		lst__member_check([=,Term1,Term2],FormL)
	),
	pr__statistics(axiom_tertium_non_datur).
pr__axiom([<>,Term1,Term2]) :-				% different terms
	obj__pure_term(Term1),
	obj__pure_term(Term2),
	\+ mgu__unifiable_terms(Term1,Term2),
	pr__statistics(axiom_different_terms).
pr__axiom([<=>,Form,Form]) :-				% equivalence
	pr__statistics(axiom_equivalence).

%%d pr__inconsistent(fmlL::in)

pr__inconsistent(Gamma) :-				% falsum
	lst__member_con_check([\/],Gamma),
	pr__statistics(inconsistent_falsum).
pr__inconsistent(Gamma) :-				% fails and succeeds
	lst__member_con([fails,Goal],Gamma),
	lst__member_con_check([succeeds,Goal],Gamma),
	pr__statistics(inconsistent_fails_and_succeeds).
pr__inconsistent(Gamma) :-				% pos and neg
	lst__member_con([~,Form],Gamma),
	lst__member_con_check(Form,Gamma),
	pr__statistics(inconsistent_pos_and_neg).
pr__inconsistent(Gamma) :-				% equal and different
	lst__member_con([<>,Term1,Term2],Gamma),
	lst__member_con_check([=,Term1,Term2],Gamma),
	pr__statistics(inconsistent_equal_and_different).
pr__inconsistent(Gamma) :-				% not self different
	lst__member_con([<>,Term,Term],Gamma),
	pr__statistics(inconsistent_not_self_different).

%%d pr__equivalent(fml::in,fml::out)
%%d pr__equivalent(fml::out,fml::in)

pr__equivalent([<>,Term1,Term2],[~,[=,Term1,Term2]]).
pr__equivalent([~,[=,Term1,Term2]],[<>,Term1,Term2]).
pr__equivalent([<>,Term1,Term2],[<>,Term2,Term1]).


%%d pr__by(gr::in,varL::in,fmlL::in,fml::in,fml::out)

pr__by(lemma(Ref),V,Gamma1,Form1,Form1) :-		% ref to lemma
	db__lemma(Ref,Form2),
	pr__derivable_once(V,[Form2|Gamma1],Form1),
	pr__statistics(by_reference).
pr__by(theorem(Ref),V,Gamma1,Form1,Form1) :-		% ref to theorem
	db__theorem(Ref,Form2),
	pr__derivable_once(V,[Form2|Gamma1],Form1),
	pr__statistics(by_reference).
pr__by(corollary(Ref),V,Gamma1,Form1,Form1) :-		% ref to corollary
	db__corollary(Ref,Form2),
	pr__derivable_once(V,[Form2|Gamma1],Form1),
	pr__statistics(by_reference).
pr__by(axiom(Ref),V,Gamma1,Form1,Form1) :-		% ref to axiom
	db__axiom(Ref,Form2),
	pr__derivable_once(V,[Form2|Gamma1],Form1),
	pr__statistics(by_reference).
pr__by(completion,V,Gamma,[def,[Op,Atom]],Form) :-	% fixed point
	pr__derivable_once(V,Gamma,[Op,Atom]),
	cmp__sft_formula(Op,Atom,Form),
	pr__statistics(by_fixed_point).
pr__by(sld,_,Gamma,Form,Form) :-			% sld step
	Form = [succeeds,[Tag|TermL]],
	cmp__match_clause(Tag,TermL,Gamma),
	pr__statistics(by_sld_step).
pr__by(completion,V,Gamma,[Op,Atom],[Op,Atom]) :-	% closure
	cmp__sft_formula(Op,Atom,Form),
	pr__derivable_once(V,Gamma,Form),
	pr__statistics(by_closure).
pr__by(introduction(Name,N),V,Gamma,Form1,Form1) :- 	% predicate intro
	Form1 = [d(Name,N)|_],
	def__pred_formula(Form1,Form2),
	pr__derivable_once(V,Gamma,Form2),
	pr__statistics(by_predicate_intro).
pr__by(elimination(Name,N),V,Gamma,Form1,Form1) :- 	% predicate elim
	def__pred_atom(Name,N,Form1,Form2),
	pr__derivable_once(V,Gamma,Form2),
	pr__statistics(by_predicate_elim).
pr__by(existence(Name,N),V,Gamma,Form1,Form1) :-	% function existence
	def__fun_existence(Name,N,Form1,Form2),
	pr__derivable_once(V,Gamma,Form2),
	pr__statistics(by_function_existence).
pr__by(uniqueness(Name,N),V,Gamma,Form1,Form1) :-	% function uniqueness
	def__fun_uniqueness(Name,N,Form1,Form2),
	pr__derivable_once(V,Gamma,Form2),
	pr__statistics(by_function_uniqueness).
pr__by(concatenation,V,Gamma,Form,Form) :-		% concatenation
	bi__concatenation(V,Gamma,Form),
	pr__statistics(by_concatenation).
pr__by(builtin,V,Gamma,[Op,Atom],[Op,Atom]) :-		% built-in closure
	obj__sft_op(Op),
	bi__typed(Atom),
	bi__eval(Atom,Goal),
	cmp__op_goal(Op,Goal,Form),
	pr__derivable_once(V,Gamma,Form),
	pr__statistics(by_built_in_closure).
pr__by(builtin,V,Gamma,[def,[Op,Atom]],Form) :-		% built-in fixed pt
	obj__sft_op(Op),
	bi__typed(Atom),
	pr__derivable_once(V,Gamma,[Op,Atom]),
	bi__eval(Atom,Goal),
	cmp__op_goal(Op,Goal,Form),
	pr__statistics(by_built_in_fixed_point).
pr__by(addition,V,Gamma,Form,Form) :-			% addition
	bi__addition(V,Gamma,Form),
	pr__statistics(by_addition).
pr__by(gap,_,_,Form,Form) :-				% gap with warning
	ctl__warning([there,is,a,gap,in,the,proof]).
pr__by(because,_,_,Form,Form) :-			% gap without warning
	(	db__flag(report_because) ->
		ctl__warning([there,is,a,gap,in,the,proof])
	;	true
	).
pr__by(new(Ref),V,Gamma,Form,Form) :-			% new theorem
	tac__new(Ref,V,Gamma,Form).
pr__by(Opt,V,Gamma,Form,Form) :-			% tactics, see tac.pl
	lst__list_form(Opt),
	tac__proof_prt(Opt,V,Gamma,Form).
pr__by(comment(_),V,Gamma,Form,Form) :-			% comment
	pr__derivable_once(V,Gamma,Form).
pr__by(X,_,_,Form,Form) :-				% unknown tactic
	\+ pr__by_tag(X),
	ctl__error([unknown,tactic,q(X)]).

%%d pr__by_tag(gr::in)

pr__by_tag(lemma(_)).
pr__by_tag(theorem(_)).
pr__by_tag(corollary(_)).
pr__by_tag(axiom(_)).
pr__by_tag(completion).
pr__by_tag(sld).
pr__by_tag(introduction(_,_)).
pr__by_tag(elimination(_,_)).
pr__by_tag(existence(_,_)).
pr__by_tag(uniqueness(_,_)).
pr__by_tag(concatenation).
pr__by_tag(builtin).
pr__by_tag(addition).
pr__by_tag(gap).
pr__by_tag(because).
pr__by_tag(comment(_)).
pr__by_tag(new(_)).
pr__by_tag([_|_]).
pr__by_tag([]).

%%d pr__match_conclusion(fml::in,fml::in,varL::in,sub::out)

pr__match_conclusion(Form1,Form2,XL,Sub) :-
	eq__match_constrained(Form1,Form2,XL,[],Sub).
pr__match_conclusion([&|FormL],Form2,XL,Sub) :-
	lst__member(Form1,FormL),
	eq__match_constrained(Form1,Form2,XL,[],Sub).

%%d pr__match_assumption(fmlL::in,fml::in,varL::in,sub::in,sub::out)

pr__match_assumption(Gamma,Form1,XL,Sub1,Sub2) :-
	(	obj__conjunction_form(Form1) ->
		Form1 = [&|FormL],
		pr__match_assumptionL(FormL,Gamma,XL,Sub1,Sub2)
	;	lst__member_con(Form2,Gamma),
		eq__match_constrained(Form1,Form2,XL,Sub1,Sub2)
	).

%%d pr__match_assumptionL(fmlL::in,fmlL::in,varL::in,sub::in)

pr__match_assumptionL([],_,_,Sub,Sub).
pr__match_assumptionL([Form1|FormL],Gamma,XL,Sub1,Sub3) :-
	lst__member_con(Form2,Gamma),
	eq__match_constrained(Form1,Form2,XL,Sub1,Sub2),
	pr__match_assumptionL(FormL,Gamma,XL,Sub2,Sub3). % once ??

%%d pr__match_kernel(fml::in,fml::in,varL::in,varL::insub::out,fmlL::out)

pr__match_kernel(Form1,Form2,BV,_,Sub,[]) :-
	eq__match_constrained(Form1,Form2,BV,[],Sub).
pr__match_kernel([&|FormL1],Form2,BV,V,Sub,FormL2) :-
	lst__member(Form1,FormL1),
	pr__match_kernel(Form1,Form2,BV,V,Sub,FormL2).
pr__match_kernel([=>,Form3,Form4],Form2,BV,V1,Sub,FormL1) :-
	eq__add_free_bound(Form3,BV,V1,V2),
	pr__match_kernel(Form4,Form2,BV,V2,Sub,FormL2),
	FormL1 = [@(all,BV,Form3)|FormL2].
pr__match_kernel(@(all,BV2,Form3),Form2,BV1,V,Sub,FormL1) :-
	lst__disjoint(BV2,V),
	lst__append_set(BV2,BV1,BV3),
	pr__match_kernel(Form3,Form2,BV3,V,Sub,FormL1).

%%d pr__match_args(fmlL::in,fmlL::in,Sub)

pr__match_args([],_,_).
pr__match_args([@(all,BV,Form)|FormL],Gamma,Sub1) :-
	pr__match_assumption(Gamma,Form,BV,Sub1,Sub2),
	pr__match_args(FormL,Gamma,Sub2).


%%d pr__cet_injective(tmL::in,tmL::in,tm::in,tm::in)

pr__cet_injective([Term1|_],[Term2|_],Term1,Term2).
pr__cet_injective([_|Term1L],[_|Term2L],Term1,Term2) :-
	pr__cet_injective(Term1L,Term2L,Term1,Term2).

%%d pr__derivable_cet(fmlL::in,ptn::in,fml::in)

pr__derivable_cet(_,no,_).
pr__derivable_cet(_,yes(Part),[=,Term1,Term2]) :-
	mgu__term_eq(Term1,Term2,Part).
pr__derivable_cet(Gamma,yes(Part),Form1) :-
	lst__member_con(Form2,Gamma),
	mgu__expr_eq(Form1,Form2,Part).

%%d pr__extract_eqs(fmlL::in,tmLL::in,tmLL::out)

pr__extract_eqs([],TermLL,TermLL).
pr__extract_eqs([Form|Gamma],Term1LL,Term3LL) :-
	(	obj__equation_form(Form) ->
		Form = [=|TermL],
		pr__extract_eqs(Gamma,[TermL|Term1LL],Term3LL)
	;	obj__conjunction_form(Form) ->
		Form = [&|FormL],
		pr__extract_eqs(FormL,Term1LL,Term2LL),
		pr__extract_eqs(Gamma,Term2LL,Term3LL)
	;	pr__extract_eqs(Gamma,Term1LL,Term3LL)
	).

%%d pr__derivable_eq(fmlL::in,tmLL::in,fml::in)

pr__derivable_eq(_,TermLL,[=|TermL]) :-
	eq__termL_eq(TermL,TermLL).
pr__derivable_eq(Gamma,TermLL,Form1) :-
	lst__member_con(Form2,Gamma),
	once(eq__expr_eq(Form1,Form2,TermLL)).

%%d pr__step_err(drv::in,varL::in,fmlL::in,fml::out)

pr__step_err(Step,V,Gamma,Form) :-
	(	pr__step(Step,V,Gamma,Form) -> 
		true
	;	i2e__derivation_step(Step,X),
		ctl__error([incorrect,derivation,step,p(X)])
	).

%%d pr__derivable_err(varL::in,fmlL::in,fml::in)

pr__derivable_err(V,Gamma,Form) :-
	(	pr__derivable_once(V,Gamma,Form) ->
		true
	;	i2e__expression(Form,X),
		ctl__error([underivable,formula,p(X)])
	).

%%d pr__debug(varL::in,fmlL::in,fml::in)

pr__debug(V,Gamma,Form,_) :-
	i2e__expressionL(Gamma,XL),
	i2e__expression(Form,X),
	io__tell_user, nl,
	write('Protected Variables: '),
	write(V), nl,
	write('Available Formulas: '), nl,
	prt__write(XL), nl,
	write('To show: '), nl,
	prt__write(X), nl.


%%d pr__statistics(in::gr)

pr__statistics(_).

% The following definition is used for statistics.

%pr__statistics(X) :- bb__inc(X).

% pr.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 6/11/95, 10:37 PM */
/* Updated: Mon Feb 15 14:25:53 1999 */
/* Filename: cmp.pl */
/* Abstract: Predicates for computing the success-, failure- and 
   termination formula with and without unification. */

% We assume that the bodies of clauses are quantifier-free goals.

%%d db__clauses(prd::in,clsL::out)

:- dynamic(db__clauses/2).

db__clauses(n(fail,0),[]).

%%d assert_clauses(prd::in,clsL::in)

assert_clauses(n(Name,N),ClauseL) :-
	(	obj__clauseL(ClauseL) ->
		(	db__clauses(n(Name,N),_) ->
			ctl__warning([predicate,Name/N,is,already,defined]),
			retract(db__clauses(n(Name,N),_)),
			assert(db__clauses(n(Name,N),ClauseL))
		;	bi__predicate(Name,N) ->
			ctl__warning([builtin,predicate,Name/N,is,
				already,defined]),
			assert(db__clauses(n(Name,N),ClauseL))
		;	assert(db__clauses(n(Name,N),ClauseL))
		)
	;	ctl__error([clauses,for,Name/N,are,corrupted])
	).

%%d cmp__clauses_err(prd::in,clsL::out)

cmp__clauses_err(n(Name,N),ClauseL) :-
	(	db__clauses(n(Name,N),ClauseL) -> 
		true
	;	ctl__error([there,are,no,clauses,for,Name/N]),
		ClauseL = []
	).

%%d cmp__sft_formula(gr::in,atm::in,fml::out)

cmp__sft_formula(Op,[Tag|TermL],Form) :-
	(	obj__make_varL(V,TermL),
		obj__varL(V) ->
		cmp__plain_gen(Tag,TermL,ClauseL)
	;	cmp__uni_gen(Tag,TermL,ClauseL)
	),
	cmp__op_clauseL(ClauseL,Op,FormL),
	(	Op = (succeeds) ->
		cmp__disjunction(FormL,Form)
	;	cmp__conjunction(FormL,Form)
	).

%%d cmp__op_clauseL(gr::in,clsL::in,fml::out)

cmp__op_clauseL([],_,[]).
cmp__op_clauseL([Clause|ClauseL],Op,[Form|FormL]) :-
	cmp__op_clause(Op,Clause,Form),
	cmp__op_clauseL(ClauseL,Op,FormL).

%%d cmp__op_clause(gr::in,cls::in,fml::out)

cmp__op_clause(succeeds,clause(EqL,Goal,VL),Form3) :-
	cmp__succeeds_goal(Goal,Form1),
	lst__append(EqL,[Form1],Form1L),
	cmp__conjunction(Form1L,Form2),
	cmp__abstraction(ex,VL,Form2,Form3).
cmp__op_clause(fails,clause(EqL,Goal,VL),Form4) :-
	cmp__conjunction_equations(EqL,Form1),
	cmp__fails_goal(Goal,Form2),
	cmp__implication(Form1,Form2,Form3),
	cmp__abstraction(all,VL,Form3,Form4).
cmp__op_clause(terminates,clause(EqL,Goal,VL),Form4) :-
	cmp__conjunction_equations(EqL,Form1),
	cmp__terminates_goal(Goal,Form2),
	cmp__implication(Form1,Form2,Form3),
	cmp__abstraction(all,VL,Form3,Form4).

%%d cmp__gen(gr::in,atm::in,clsL::out)

cmp__gen(plain,[Tag|TermL],ClauseL) :-
	cmp__plain_gen(Tag,TermL,ClauseL).
cmp__gen(uni,[Tag|TermL],ClauseL) :-
	cmp__uni_gen(Tag,TermL,ClauseL).

%%d cmp__plain_gen(prd::in,tmL::in,clsL::out)

cmp__plain_gen(Tag,TermL,Clause2L) :-
	cmp__clauses_err(Tag,Clause1L),
	eq__add_max_free_qfL(TermL,0,[],K,VL),
	cmp__plain_genL(Clause1L,TermL,VL,K,Clause2L).

%%d cmp__plain_genL(clsL::in,tmL::in,varL::in,int::in,clsL::out)

cmp__plain_genL([],_,_,_,[]).
cmp__plain_genL([Clause1|Clause1L],TermL,VL,K,[Clause3|Clause2L]) :-
	cmp__rename(Clause1,VL,K,Clause2),
	cmp__plain_gen_clause(Clause2,TermL,Clause3),
	cmp__plain_genL(Clause1L,TermL,VL,K,Clause2L).

%%d cmp__plain_gen_clause(cls::in,tmL::in,cls::out)

cmp__plain_gen_clause(clause([_|Term1L],Goal1,V1L),Term2L,
	clause(Eq2L,Goal2,V2L)) :-
	cmp__equationL(Term2L,Term1L,V1L,V2L,Eq1L,Sub),
	% One has to apply the substitution also to the equations
	eq__apply_sub_qfL(Eq1L,Sub,Eq2L),
	eq__apply_sub_qf(Goal1,Sub,Goal2).

%%d cmp__equationL(tmL::in,tmL::in,varL::in,varL::out,fmlL::out,sub::out)

cmp__equationL([],[],VL,VL,[],[]).
cmp__equationL([Term|Term1L],[[Tag|TermL]|Term2L],V1L,V2L,
	[[=,Term,[Tag|TermL]]|EqL],Sub) :-
	cmp__equationL(Term1L,Term2L,V1L,V2L,EqL,Sub).
cmp__equationL([Term|Term1L],[$(X)|Term2L],V1L,V3L,Eq2L,Sub2) :-
	% $(X) does not occur in Term, since the clause is renamed.
	(	lst__delete(X,V1L,V2L) ->
		Sub2 = [X => Term|Sub1],
		cmp__equationL(Term1L,Term2L,V2L,V3L,Eq2L,Sub1)
	;	Eq2L = [[=,Term,$(X)]|Eq1L],
		cmp__equationL(Term1L,Term2L,V1L,V3L,Eq1L,Sub2)
	).

%%d cmp__rename(cls::in,varL::in,int::in,cls::out)

cmp__rename(clause(Atom1,Goal1,V1L/N1),V2L,N2,clause(Atom2,Goal2,V3L)) :-
	cmp__maximum(N1,N2,K1),
	eq__apply_extend(V1L,V2L,[],K1,V3L,_,Sub,_),
	eq__apply_sub_qf(Atom1,Sub,Atom2),
	eq__apply_sub_qf(Goal1,Sub,Goal2).

%%d cmp__maximum(int::in,int::in,int::out)

cmp__maximum(N1,N2,N3) :-
	(	N1 < N2 -> 
		N3 = N2
	;	N3 = N1
	).

%%d cmp__uni_gen(prd::in,tmL::in,clsL::out)

cmp__uni_gen(Tag,Term1L,Clause3L) :-
	cmp__clauses_err(Tag,Clause1L),
	eq__add_max_free_qfL(Term1L,0,[],K1,V1L),
	cmp__pure_partL(Term1L,K1,[],Term2L,K2,Sub),
	eq__add_free_qfL(Term2L,V1L,V2L),	% Why? Oh, yes.
	cmp__uni_genL(Clause1L,Term2L,V2L,K2,Clause2L),
	(	Sub = [] ->
		Clause3L = Clause2L
	;	cmp__apply_clauseL(Clause2L,Sub,Clause3L)
	).

%%d cmp__uni_genL(clsL::in,tmL::in,varL::in,int::in,clsL::out)

cmp__uni_genL([],_,_,_,[]).
cmp__uni_genL([Clause1|Clause1L],Term1L,V1L,K,Clause3L) :-
	cmp__rename(Clause1,V1L,K,clause([_|Term2L],Goal1,V2L)),
	(	mgu__unify_termL_sub(Term1L,Term2L,Sub1) ->
		cmp__uni_equationL(Sub1,V2L,V3L,Sub2,Sub3,Eq1L),
		eq__apply_sub_qf(Goal1,Sub2,Goal2),
		eq__apply_sub_qf(Goal2,Sub3,Goal3),
		eq__apply_sub_qfL(Eq1L,Sub3,Eq2L),
		Clause3L = [clause(Eq2L,Goal3,V3L)|Clause2L],
		cmp__uni_genL(Clause1L,Term1L,V1L,K,Clause2L)
	;	% otherwise the terms are not unifiable
		cmp__uni_genL(Clause1L,Term1L,V1L,K,Clause3L)
	).

%%d cmp__uni_equationL(sub::in,varL::in,varL::out,sub::out,sub::out,fmlL::out)

cmp__uni_equationL([],VL,VL,[],[],[]).
cmp__uni_equationL([X => Term|Sub1],V1L,V3L,Sub3,Sub5,Eq2L) :-
    (   Term = $(X) ->
        cmp__uni_equationL(Sub1,V1L,V3L,Sub3,Sub5,Eq2L)
    ;   (   lst__delete(X,V1L,V2L) ->
	    % $(X) does not occur in Term, since the mgu is idempotent
            Sub3 = [X => Term|Sub2],
            cmp__uni_equationL(Sub1,V2L,V3L,Sub2,Sub5,Eq2L)
        ;   (   Term = $(Y) ->
                (   lst__delete(Y,V1L,V2L) ->
                    Sub3 = [Y => $(X)|Sub2],
                    Sub5 = [Y => $(X)|Sub4],
                    cmp__uni_equationL(Sub1,V2L,V3L,Sub2,Sub4,Eq2L)
                ;   Eq2L = [[=,$(Y),$(X)]|Eq1L],
		    cmp__uni_equationL(Sub1,V1L,V3L,Sub3,Sub5,Eq1L)
                    
                )
            ;   Eq2L = [[=,$(X),Term]|Eq1L],
	        cmp__uni_equationL(Sub1,V1L,V3L,Sub3,Sub5,Eq1L)
                
            )
        )
    ).

%%d cmp__pure_part(tm::in,int::in,sub::in,tm::out,int::out,sub::out)

cmp__pure_part($(X),K,Sub,$(X),K,Sub).
cmp__pure_part([n(Name,N)|Term1L],K1,Sub1,[n(Name,N)|Term2L],K2,Sub2) :-
	cmp__pure_partL(Term1L,K1,Sub1,Term2L,K2,Sub2).
cmp__pure_part([f(Name,N)|Term1L],K1,Sub1,$(K2),K3,Sub2) :-
	% We want a special term and not a general one.
	(	lst__member(K2 => [f(Name,N)|Term1L],Sub1) ->
		K3 = K1,
		Sub2 = Sub1
	;	K2 = K1, 
		K3 is K1 + 1, 
		Sub2 = [K2 => [f(Name,N)|Term1L]|Sub1]).

%%d cmp__pure_partL(tmL::in,int::in,sub::in,tmL::out,int::out,sub::out)

cmp__pure_partL([],K,Sub,[],K,Sub).
cmp__pure_partL([Term1|Term1L],K1,Sub1,[Term2|Term2L],K3,Sub3) :-
	cmp__pure_part(Term1,K1,Sub1,Term2,K2,Sub2),
	cmp__pure_partL(Term1L,K2,Sub2,Term2L,K3,Sub3).

%%d cmp__apply_clauseL(clsL::in,sub::in,clsL::out)

cmp__apply_clauseL([],_,[]).
cmp__apply_clauseL([clause(Eq1L,Goal1,VL)|Clause1L],Sub,
	[clause(Eq2L,Goal2,VL)|Clause2L]) :-
	eq__apply_sub_qfL(Eq1L,Sub,Eq2L),
	eq__apply_sub_qf(Goal1,Sub,Goal2),
	cmp__apply_clauseL(Clause1L,Sub,Clause2L).

% In applying the operators 'succeeds', 'fails' and 'terminates' to goals
% the following simplifications are made:
%
% tt & Form ===> Form
% ff & Form ===> ff
% tt \/ Form ===> tt
% ff \/ Form ===> Form
% tt => Form ===> Form
% ff => Form ===> tt
% Form => tt ===> tt
% Form => ff ===> ~ Form
% fails (?x = ?y & Goal) ===> ?x = ?y => fails Goal
% terminates (?x = ?y & Goal) ===> ?x = ?y => terminates Goal

%%d cmp__op_goal(gr::in,goal::in,fml::out)

cmp__op_goal(succeeds,Goal,Form) :-			% succeeds
	cmp__succeeds_goal(Goal,Form).
cmp__op_goal(fails,Goal,Form) :-			% fails
	cmp__fails_goal(Goal,Form).
cmp__op_goal(terminates,Goal,Form) :-			% terminates
	cmp__terminates_goal(Goal,Form).

%%d cmp__succeeds_goal(goal::in,fml::out)

cmp__succeeds_goal([=,Term1,Term2],[=,Term1,Term2]).	% =
cmp__succeeds_goal([n(Name,N)|TermL],		% predicate
	[succeeds,[n(Name,N)|TermL]]).
cmp__succeeds_goal([~,Goal],Form) :-		% negation
	cmp__fails_goal(Goal,Form).
cmp__succeeds_goal([&|GoalL],Form) :-		% conjunction
	cmp__succeeds_goalL(GoalL,FormL),
	cmp__conjunction(FormL,Form).
cmp__succeeds_goal([\/|GoalL],Form) :-		% disjunction
	cmp__succeeds_goalL(GoalL,FormL),
	cmp__disjunction(FormL,Form).
cmp__succeeds_goal(@(ex,XL,Goal),	% existential
	@(ex,XL,Form)) :-
	cmp__succeeds_goal(Goal,Form).

%%d cmp__succeeds_goalL(goalL::in,fmlL::out)

cmp__succeeds_goalL([],[]).
cmp__succeeds_goalL([Goal|GoalL],[Form|FormL]) :-
	cmp__succeeds_goal(Goal,Form),
	cmp__succeeds_goalL(GoalL,FormL).

%%d cmp__fails_goal(goal::in,fml::out)

cmp__fails_goal([=,Term1,Term2],[<>,Term1,Term2]).	% =
cmp__fails_goal([n(Name,N)|TermL],		% predicate
	[fails,[n(Name,N)|TermL]]).
cmp__fails_goal([~,Goal],Form) :-		% negation
	cmp__succeeds_goal(Goal,Form).
cmp__fails_goal([&|GoalL],Form) :-		% conjunction
	cmp__fails_goalL(GoalL,FormL),
	cmp__disjunction(FormL,Form).
cmp__fails_goal([\/|GoalL],Form) :-		% disjunction
	cmp__fails_goalL(GoalL,FormL),
	cmp__conjunction(FormL,Form).
cmp__fails_goal(@(ex,XL,Goal),		% existential
	@(all,XL,Form)) :-
	cmp__fails_goal(Goal,Form).

%%d cmp__fails_goalL(goalL::in,fmlL::out)

cmp__fails_goalL([],[]).
cmp__fails_goalL([Goal|GoalL],[Form|FormL]) :-
	cmp__fails_goal(Goal,Form),
	cmp__fails_goalL(GoalL,FormL).

%%d cmp__terminates_goal(goal::in,fml::out)

cmp__terminates_goal([=,_,_],[&]).		% =
cmp__terminates_goal([n(Name,N)|TermL],		% predicate
	[terminates,[n(Name,N)|TermL]]).
cmp__terminates_goal([~,Goal],Form2) :-		% negation
	cmp__pure_goal(Goal),
	cmp__terminates_goal(Goal,Form1),
	cmp__gr_goal(Goal,FormL),
	cmp__conjunction([Form1|FormL],Form2).
cmp__terminates_goal([&|GoalL],Form) :-		% conjunction
	(	GoalL = [] ->
		Form = [&]
	;	lst__singleton(GoalL) ->
		GoalL = [Goal],
		cmp__terminates_goal(Goal,Form)
	;	Form = [terminates,[&|GoalL]]
	).
cmp__terminates_goal([\/|GoalL],Form) :-	% disjunction
	cmp__terminates_goalL(GoalL,FormL),
	cmp__conjunction(FormL,Form).
cmp__terminates_goal(@(ex,XL,Goal),	% existential
	@(all,XL,Form)) :-
	cmp__terminates_goal(Goal,Form).

%%d cmp__terminates_goalL(goalL::in,fmlL::out)

cmp__terminates_goalL([],[]).
cmp__terminates_goalL([Goal|GoalL],[Form|FormL]) :-
	cmp__terminates_goal(Goal,Form),
	cmp__terminates_goalL(GoalL,FormL).

%%d cmp__gr_goal(exp::in,fmlL::out)

cmp__gr_goal(Goal,FormL) :-
	eq__add_free(Goal,[],XL),
	cmp__map_gr(XL,[],FormL).

%%d cmp__map_gr(grL::in,fmlL::in,fmlL::out)

cmp__map_gr([],FormL,FormL).
cmp__map_gr([X|XL],Form1L,Form2L) :-
	cmp__map_gr(XL,[[gr,$(X)]|Form1L],Form2L).

%%d cmp__disjunction(fmlL::in,fml::out)

cmp__disjunction(Form1L,Form2) :-
	cmp__flatten_dis(Form1L,Form2L),
	(	lst__singleton(Form2L) ->
		Form2L = [Form2]
	;	lst__member_check([&],Form2L) ->
		Form2 = [&]
	;	Form2 = [\/|Form2L]
	).

%%d cmp__conjunction(fmlL::in,fml::out)

cmp__conjunction(Form1L,Form2) :-
	cmp__flatten_con(Form1L,Form2L),
	(	lst__singleton(Form2L) ->
		Form2L = [Form2]
	;	lst__member_check([\/],Form2L) ->
		Form2 = [\/]
	;	Form2 = [&|Form2L]
	).

%%d cmp__flatten_con(fmlL::in,fmlL::out)

cmp__flatten_con([],[]).
cmp__flatten_con([Form|Form1L],Form4L) :-
	cmp__flatten_con(Form1L,Form2L),
	(	obj__conjunction_form(Form) ->
		Form = [&|Form3L],
		lst__concat(Form3L,Form2L,Form4L)
	;	Form4L = [Form|Form2L]
	).
		
%%d cmp__flatten_dis(fmlL::in,fmlL::out)

cmp__flatten_dis([],[]).
cmp__flatten_dis([Form|Form1L],Form4L) :-
	cmp__flatten_dis(Form1L,Form2L),
	(	obj__disjunction_form(Form) ->
		Form = [\/|Form3L],
		lst__concat(Form3L,Form2L,Form4L)
	;	Form4L = [Form|Form2L]
	).

%%d cmp__implication(fml::in,fml::in,fml::out)

cmp__implication(Form1,Form2,Form3) :-
	(	Form1 = [&] ->
		Form3 = Form2
	;	Form1 = [\/] -> 
		Form3 = [&]
	;	Form2 = [\/] -> 
		Form3 = [~,Form1]
	;	Form2 = [&] -> 
		Form3 = [&]
	;	Form3 = [=>,Form1,Form2]
	).

%%d cmp__conjunction_equations(fmlL::in,fml::out)

cmp__conjunction_equations(FormL,Form) :-
	(	lst__singleton(FormL) ->
		FormL = [Form]
	;	Form = [&|FormL]
	).

%%d cmp__abstraction(gr::in,varL::in,fml::in,fml::out)


cmp__abstraction(Tag,VL,Form1,Form2) :-
	(	VL = [] ->
		Form2 = Form1
	;	eq__free(Form1,VL) ->
		Form2 = Form1
	;	Form2 = @(Tag,VL,Form1)
	).

% What happens if Form1 is already an abstraction???


%%d cmp__match_clause(prd::in,tmL::in,fmlL::in)

cmp__match_clause(Tag,Term1L,Gamma) :-
	db__clauses(Tag,ClauseL),
	lst__member(clause([Tag|Term2L],Goal,_),ClauseL),
	eq__match_qfL(Term2L,Term1L,[],Sub),
	cmp__succeeds_goal(Goal,Form),
	cmp__body_match(Form,Gamma,Sub,_).

%%d cmp__body_match(fml::in,fmlL::in,sub::in,sub::out)

cmp__body_match([=,Term1,Term2],Gamma,Sub1,Sub3) :-
	lst__member_con([=,Term3,Term4],Gamma),
	(	eq__match_qf(Term1,Term3,Sub1,Sub2),
		eq__match_qf(Term2,Term4,Sub2,Sub3) ->
		true
	;	eq__match_qf(Term1,Term4,Sub1,Sub2),
		eq__match_qf(Term2,Term3,Sub2,Sub3)
	).
cmp__body_match([<>,Term1,Term2],Gamma,Sub1,Sub3) :-
	lst__member_con([<>,Term3,Term4],Gamma),
	(	eq__match_qf(Term1,Term3,Sub1,Sub2),
		eq__match_qf(Term2,Term4,Sub2,Sub3) ->
		true
	;	eq__match_qf(Term1,Term4,Sub1,Sub2),
		eq__match_qf(Term2,Term3,Sub2,Sub3)
	).
cmp__body_match([succeeds,Atom1],Gamma,Sub1,Sub2) :-
	lst__member_con([succeeds,Atom2],Gamma),
	eq__match_qf(Atom1,Atom2,Sub1,Sub2).
cmp__body_match([fails,Atom1],Gamma,Sub1,Sub2) :-
	lst__member_con([fails,Atom2],Gamma),
	eq__match_qf(Atom1,Atom2,Sub1,Sub2).
cmp__body_match([&|FormL],Gamma,Sub1,Sub2) :-
	cmp__body_matchL(FormL,Gamma,Sub1,Sub2).
cmp__body_match([\/|FormL],Gamma,Sub1,Sub2) :-
	lst__member(Form,FormL),
	cmp__body_match(Form,Gamma,Sub1,Sub2).

%%d cmp__body_matchL(fmlL::in,fmlL::in,sub::in,sub::out)

cmp__body_matchL([],_,Sub,Sub).
cmp__body_matchL([Form|FormL],Gamma,Sub1,Sub3) :-
	cmp__body_match(Form,Gamma,Sub1,Sub2),
	cmp__body_matchL(FormL,Gamma,Sub2,Sub3).

%%d cmp__pure_goal(goal::in)

cmp__pure_goal([=,Term1,Term2]) :-
	obj__pure_term(Term1),
	obj__pure_term(Term2).
cmp__pure_goal([n(_,_)|TermL]) :-
	obj__pure_termL(TermL).
cmp__pure_goal([~,Goal]) :-
	cmp__pure_goal(Goal).
cmp__pure_goal([&|GoalL]) :-
	cmp__pure_goalL(GoalL).
cmp__pure_goal([\/|GoalL]) :-
	cmp__pure_goalL(GoalL).

%%d cmp__pure_goalL(goalL::in)

cmp__pure_goalL([]).
cmp__pure_goalL([Goal|GoalL]) :-
	cmp__pure_goal(Goal),
	cmp__pure_goalL(GoalL).

% cmp.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 8/11/95, 2:13 PM */
/* Updated: Sat Dec 26 15:12:10 1998 */
/* Filename: bi.pl */
/* Abstract: Built-in predicates. Library predicates. */

%%d bi__predicate(gr::out,int::out)

bi__predicate(atom,1).				% atom/1
bi__predicate(integer,1).			% integer/1
bi__predicate(atomic,1).			% atomic/1
bi__predicate(is,2).				% is/2
bi__predicate(<,2).				% </2
bi__predicate(=<,2).				% =</2
bi__predicate(current_op,3).			% current_op/3
bi__predicate(once,1).				% once/1
bi__predicate(call,_).				% call/n+1

%%d bi__arithmetic_constructor(gr::in)

bi__arithmetic_constructor(+,2).
bi__arithmetic_constructor(-,2).
bi__arithmetic_constructor(*,2).
bi__arithmetic_constructor(/,2).
bi__arithmetic_constructor(Name,0) :- integer(Name).

%%d bi__typed(atm::in)

bi__typed([n(atom,1),Term]) :-			% atom/1
	bi__ground(Term).
bi__typed([n(integer,1),Term]) :-		% integer/1
	bi__ground(Term).
bi__typed([n(atomic,1),Term]) :-		% atomic/1
	bi__ground(Term).
bi__typed([n(is,2),_,Term]) :-			% is/2
	bi__ground_arithmetic(Term).
bi__typed([n(<,2),Term1,Term2]) :-		% </2
	bi__ground_arithmetic(Term1),
	bi__ground_arithmetic(Term2).
bi__typed([n(=<,2),Term1,Term2]) :-		% =</2
	bi__ground_arithmetic(Term1),
	bi__ground_arithmetic(Term2).
bi__typed([n(current_op,3),_,_,[n(_,0)]]).	% current_op/3
bi__typed([n(once,1),Term]) :-			% once/1
	bi__ground(Term).
bi__typed([n(call,_),[n(_,0)]|_]).		% call/n+1

%%d bi__eval(atm::in,goal::out)

bi__eval([n(atom,1),Term],Goal) :-		% atom/1
	(	Term = [n(Name,0)],
		atom(Name) ->
		Goal = [&]
	;	Goal = [\/]
	).
bi__eval([n(integer,1),Term],Goal) :-		% integer/1
	(	Term = [n(Name,0)],
		integer(Name) ->
		Goal = [&]
	;	Goal = [\/]
	).
bi__eval([n(atomic,1),Term],Goal) :-		% atomic/1
	(	Term = [n(Name,0)],
		atomic(Name) ->
		Goal = [&]
	;	Goal = [\/]
	).
bi__eval([n(is,2),Term1,Term2],Goal) :-		% is/2
	bi__eval_arithm(Term2,Name),
	Goal = [=,Term1,[n(Name,0)]].
bi__eval([n(<,2),Term1,Term2],Goal) :-		% </2
	(	bi__eval_arithm(Term1,N1),
	 	bi__eval_arithm(Term2,N2),
	 	N1 < N2 ->
		Goal = [&]
	;	Goal = [\/]
	).
bi__eval([n(=<,2),Term1,Term2],Goal) :-		% </2
	(	bi__eval_arithm(Term1,N1),
	 	bi__eval_arithm(Term2,N2),
	 	N1 =< N2 ->
		Goal = [&]
	;	Goal = [\/]
	).
bi__eval([n(once,1),Term],Term).		% once/1
bi__eval([n(call,N1),[n(Name,0)]|TermL],Goal) :- % call/n+1
	N2 is N1 - 1,
	Goal = [n(Name,N2)|TermL].

%%d bi__ground(tm::in)

bi__ground([_|TermL]) :-
	bi__groundL(TermL).

%%d bi__groundL(tmL::in)

bi__groundL([]).
bi__groundL([Term|TermL]) :-
	bi__ground(Term),
	bi__groundL(TermL).

%%d bi__ground_arithmetic(tm::in)

bi__ground_arithmetic([n(Name,N)|TermL]) :-
	bi__arithmetic_constructor(Name,N),
	bi__ground_arithmeticL(TermL).

%%d bi__ground_arithmeticL(tmL::in)

bi__ground_arithmeticL([]).
bi__ground_arithmeticL([Term|TermL]) :-
	bi__ground_arithmetic(Term),
	bi__ground_arithmeticL(TermL).

%%d bi__eval_arithm(tm::in,gr::out)

bi__eval_arithm([n(+,2),Term1,Term2],N3) :-
	bi__eval_arithm(Term1,N1),
	bi__eval_arithm(Term2,N2),
	N3 is N1 + N2.
bi__eval_arithm([n(*,2),Term1,Term2],N3) :-
	bi__eval_arithm(Term1,N1),
	bi__eval_arithm(Term2,N2),
	N3 is N1 * N2.
bi__eval_arithm([n(-,2),Term1,Term2],N3) :-
	bi__eval_arithm(Term1,N1),
	bi__eval_arithm(Term2,N2),
	N3 is N1 - N2.
bi__eval_arithm([n(/,2),Term1,Term2],N3) :-
	bi__eval_arithm(Term1,N1),
	bi__eval_arithm(Term2,N2),
	N3 is N1 / N2.
bi__eval_arithm([n(N,0)],N) :-
	integer(N).

%%d bi__builtin_atom(atm::gr)

bi__builtin_atom([n(Name,N)|_]) :-
	bi__predicate(Name,N).

%%d bi__user_defined_atom(atm::int)

bi__user_defined_atom([Tag|_]) :-
	db__clauses(Tag,_).


%% Concatenation of lists

% The following predicates are only correct under the assumption that
% list/1 and **/2 have their standard definitions.
%
% Example: The list normal form of [?x|?l1 ** [] ** ?l2] ** (?l3 ** ?l4)
% is [ [?x|?l1],?l2,?l3,?l4 ].

%%d bi__list_form(tm::in)

bi__list_form([n([],0)]).
bi__list_form([n('.',2),_,_]).
bi__list_form([f(**,2),_,_]).

%%d bi__list_nf(tm::in,tmL::in,tmL::out)

bi__list_nf([f(**,2),Term1,Term2],L1,L3) :-
	bi__list_nf(Term2,L1,L2),
	bi__list_nf(Term1,L2,L3).
bi__list_nf([n('.',2),Term1,Term2],L1,L3) :-
	bi__list_nf(Term2,L1,L2),
	bi__list_cons(Term1,L2,L3).
bi__list_nf([n([],0)],L,L).
bi__list_nf(Term,L,[Term|L]) :-
 	\+ bi__list_form(Term).

%%d bi__list_cons(tm::in,tmL::in,tmL::out)

bi__list_cons(Term,[],[[n('.',2),Term,[n([],0)]]]).
bi__list_cons(Term1,[Term2|L],[[n('.',2),Term1,Term2]|L]).

%%d bi__list_definedL(tmL::in,fmlL::in)

bi__list_definedL([],_,_).	% [] = [] ** [] ** []
bi__list_definedL([_],_,_).	% The type of the last element is arbitrary
bi__list_definedL([Term1,Term2|L],V,Gamma) :-
	once(bi__list_defined(Term1,V,Gamma)),
	bi__list_definedL([Term2|L],V,Gamma).

%%d bi__list_defined(tm::in,fmlL::in)

bi__list_defined([n('.',2),_,Term],V,Gamma) :-
	bi__list_defined(Term,V,Gamma).
bi__list_defined(Term,V,Gamma) :-
	pr__derivable_once(V,Gamma,[succeeds,[n(list,1),Term]]).

%%d bi__concatenation(fmlL::in,fml::in)

% Term1 and Term2 have the same normal form.

bi__concatenation(V,Gamma,[=,Term1,Term2]) :-
	bi__list_nf(Term1,[],L),
	bi__list_definedL(L,V,Gamma),
	bi__list_nf(Term2,[],L).

%% Sums of natural numbers

% The following predicates are only correct under the assumption that
% nat/1 and @+/2 have their standard definitions.
%
% Example: The normal form of s(?m @+ s(?n) @+ 0) @+ ?i consists of
% [?m,?n,?i] and s(s(0)).

%%d bi__sum_form(tm::in)

bi__sum_form([n(0,0)]).
bi__sum_form([n(s,1),_]).
bi__sum_form([f(@+,2),_,_]).

%%d bi__sum_nf(tm::in,tmL::in,tmL::out)

bi__sum_nf([n(0,0)],L,L,N,N).
bi__sum_nf([n(s,1),Term],L1,L2,N1,s(N2)) :-
	bi__sum_nf(Term,L1,L2,N1,N2).
bi__sum_nf([f(@+,2),Term1,Term2],L1,L3,N1,N3) :-
	bi__sum_nf(Term2,L1,L2,N1,N2),
	bi__sum_nf(Term1,L2,L3,N2,N3).
bi__sum_nf(Term,L,[Term|L],N,N) :-
	\+ bi__sum_form(Term).

%%d bi__nat_defined(tm::in,varL::in,fmlL::in)

bi__nat_defined(Term,V,Gamma) :-
	pr__derivable_once(V,Gamma,[succeeds,[n(nat,1),Term]]).

bi__nat_definedL([],_,_).
bi__nat_definedL([Term|TermL],V,Gamma) :-
	bi__nat_defined(Term,V,Gamma),
	bi__nat_definedL(TermL,V,Gamma).

%%d bi__addition(varL::in,fmlL::in,fml::in)

bi__addition(V,Gamma,[=,Term1,Term2]) :-
	bi__sum_nf(Term1,[],L1,0,N),
	bi__nat_definedL(L1,V,Gamma),
	bi__sum_nf(Term2,[],L2,0,N),
	lst__permutation(L1,L2).

% bi.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 7/19/95, 4:33 PM */
/* Updated: Mon Feb 15 14:39:02 1999 */
/* Filename: def.pl */
/* Abstract: Defined predicate and defined function symbols. */

%%d db__pred(gr::out,fml::out)

:- dynamic(db__pred/2).

db__pred([d('DUMMY',0)],[&]).

%%d def__defined_pred(gr::in,int::in)

def__defined_pred(Name,N) :-
	db__pred([d(Name,N)|_],_).

%%d db__fun(tm::out,fml::out,fml::out)

:- dynamic(db__fun/3).

db__fun([f('DUMMY',0)],[&],[&]).

%%d def__defined_fun(gr::in,int::in)

def__defined_fun(Name,N) :-
	db__fun([f(Name,N)|_],_,_).

%% add a predicate definition to the database

%%d assert_pred(atm::in,fml::in)

assert_pred(Atom,Form) :-
	Atom = [d(Name,N)|_],
	(	def__defined_pred(Name,N) ->
		ctl__warning([Name/N,is,already,a,defined,predicate,symbol])
	;	def__defined_fun(Name,N) ->
		ctl__warning([Name/N,is,already,a,defined,function,symbol])
	;	assert(db__pred(Atom,Form))
	).

%% process a predicate definition

%%d definition_pred(gr::in,gr::in)

definition_pred(Name,N,X) :-
	ctl__unset_flag(global_error),
	e2i__formula(X,Form1),
	(	def__pred_check(Name,N,Form1,Form2,Form3,Form4) ->
		assert_pred(Form2,Form3),
		(	db__is_open(thm), 
			db__flag(thm_output) -> 
			io__tell(thm),
			write(':- '),
			writeq(assert_pred(Form2,Form3)),
			write('.'), nl
		; 	true
		),
		(	db__is_open(tex), 
			db__flag(tex_output) ->
			io__tell(tex),
			'TeX__write_def'(Name,N,Form4)
		; 	true
		),
		(	db__is_open(prt) ->
			io__tell(prt),
			prt__pred_def(Name,N,X)
		;	true
		),
		dep__write_assert(definition_pred(Name,N),[])	
	;	ctl__error([incorrect,predicate,definition,p(X)])
	).

%%d def__pred_check(gr::in,int::in,fml::in,fml::out,fml::out)

def__pred_check(Name,N,Form1,Form5,Form4,Form6) :-
	eq__add_free(Form1,[],[]),
	Form1 = @(all,BV,Form2),
	(	Form2 = [<=>,Form3,Form4]
	;	Form2 = [<=>,Form4,Form3]
	),
	(	Form3 = [n(Name,N)|TermL]
	;	Form3 = [d(Name,N)|TermL]
	),
	obj__make_varL(BV,TermL),
	(	def__defined_pred(Name,N) ->
		ctl__error([Name/N,is,already,a,defined,predicate,symbol])
	;	def__defined_fun(Name,N) ->
		ctl__error([Name/N,is,already,a,defined,function,symbol])
	;	Form5 = [d(Name,N)|TermL],
		Form6 = @(all,BV,[<=>,Form5,Form4])
	).

%%d def__pred_formula(fml::in,fml::out)

def__pred_formula([d(Name,N)|Term1L],Form2) :-
	db__pred([d(Name,N)|Term2L],Form1),
	obj__make_varL(V1,Term2L),
	eq__make_sub(V1,Term1L,V2,Sub),
	eq__max_qfL(Term1L,0,K1),
	eq__max(Form1,K1,K2),
	eq__apply(Form1,V2,Sub,K2,Form2).

%%d def__pred_atom(gr::in,int::in,fml::in,atm::out)

def__pred_atom(Name,N,Form1,Atom) :-
	db__pred([d(Name,N)|TermL],Form2),
	eq__match(Form2,Form1,Sub),
	eq__apply_sub_qf([d(Name,N)|TermL],Sub,Atom).

%% add a function definition to the database

%%d assert_fun(tm::in,fml::in,fml::in)

assert_fun(Term,Form1,Form2) :-
	Term = [f(Name,N)|_],
	(	def__defined_fun(Name,N) ->
		ctl__warning([Name/N,is,already,a,defined,function,symbol])
	;	def__defined_pred(Name,N) ->
		ctl__warning([Name/N,is,already,a,defined,predicate,symbol])
	;	assert(db__fun(Term,Form1,Form2))
	).

%% process a function definition

%%d definition_fun(gr::in,int::in,gr::in,gr::in,gr::in,gr::in)

definition_fun(Name,N,X,
		existence by Refex,
		uniqueness by Refuni) :-
	ctl__unset_flag(global_error),
	e2i__formula(X,Form1),
	(	def__fun_check(Name,N,Form1,Term,BV,Y,Form2,Form3,Form4) ->
		(	def__fun_check_existence(Refex,BV,Y,Form2,Form3) ->
			true
		;	ctl__error([incorrect,existence,in,definition,
				of,Name/N])
		),
		(	def__fun_check_uniqueness(Refuni,BV,Y,Form2,Form3) ->
			true
		;	ctl__error([incorrect,uniqueness,in,definition,
				of,Name/N])
		),
		Form5 = @(ex,[Y],Form3),
		assert_fun(Term,Form2,Form5),
		(	db__is_open(thm), 
			db__flag(thm_output) -> 
			io__tell(thm),
			write(':- '),
			writeq(assert_fun(Term,Form2,Form5)),
			write('.'), nl
		; 	true
		),
		(	db__is_open(tex), 
			db__flag(tex_output) ->
			io__tell(tex),
			'TeX__write_def'(Name,N,Form4)
		; 	true
		),
		(	db__is_open(prt) ->
			io__tell(prt),
			prt__definition_fun(Name,N,X,Refex,Refuni)
		;	true
		),
		dep__write_assert(definition_fun(Name,N),[Refex,Refuni])
	;	ctl__error([incorrect,function,definition,p(X)])
	).

%%d def__fun_check(gr::in,int::in,fml::in,tm::out,grL::out,gr::out,fml::out,fml::out)

def__fun_check(Name,N,Form1,Term2,BV2,Y,Form2,Form4,Form5) :-
	Form1 = @(all,BV1,[=>,Form2,Form3]),
	(	Form3 = [<=>,[=,Term1,$(Y)],Form4]
	;	Form3 = [<=>,[=,$(Y),Term1],Form4]
	;	Form3 = [<=>,Form4,[=,Term1,$(Y)]]
	;	Form3 = [<=>,Form4,[=,$(Y),Term1]]
	),
	(	Term1 = [n(Name,N)|TermL]
	;	Term1 = [f(Name,N)|TermL]
	),
	Term2 = [f(Name,N)|TermL],
	obj__make_varL(BV2,TermL),
	lst__delete(Y,BV1,BV2),
	eq__add_free(Form1,[],[]),
	eq__free(Form2,[Y]),
	Form5 = @(all,BV1,[=>,Form2,[<=>,[=,Term2,$(Y)],Form4]]).

%%d def__fun_check_existence(gr::in,varL::in,var::in,fml::in,fml::in)

def__fun_check_existence(Ref,BV,Y,Form1,Form2) :-
	Form3 = @(all,BV,[=>,Form1,@(ex,[Y],Form2)]),
	def__implies_thm(Ref,Form3).

%%d def__fun_check_uniqueness(gr::in,varL::in,var::in,fml::in,fml::in)

def__fun_check_uniqueness(Ref,BV1,Y,Form1,Form2) :-
	eq__max(Form2,0,Z),
	N is Z + 1,
	eq__apply(Form2,[Z],[Y => $(Z)],N,Form3),
	cmp__conjunction([Form2,Form3,Form1],Form4),
	lst__concat(BV1,[Y,Z],BV2),
	Form5 = @(all,BV2,[=>,Form4,[=,$(Y),$(Z)]]),
	def__implies_thm(Ref,Form5).

%%d def__map_gr(varL::in,fmlL::out)

def__map_gr([],[]).
def__map_gr([X|XL],[[gr,$(X)]|FormL]) :-
	def__map_gr(XL,FormL).

%d def__implies_thm(gr::in,fml::in)

def__implies_thm(theorem(Ref),Form1) :-
	db__theorem(Ref,Form2),
	def__derivable(Form2,Form1).
def__implies_thm(lemma(Ref),Form1) :-
	db__lemma(Ref,Form2),
	def__derivable(Form2,Form1).
def__implies_thm(corollary(Ref),Form1) :-
	db__corollary(Ref,Form2),
	def__derivable(Form2,Form1).
def__implies_thm(axiom(Ref),Form1) :-
	db__axiom(Ref,Form2),
	def__derivable(Form2,Form1).

%%d def__derivable(fml::in,fml::in)

def__derivable(Form1,Form2) :-
	eq__alpha(Form1,Form2).
def__derivable(Form1,@(all,BV,[=>,Form2,Form3])) :-
	pr__derivable_once(BV,[Form1,Form2],Form3).

%%d def__fun_existence(gr::in,int::in,fml::in,fml::out)

def__fun_existence(Name,N,Form1,Form4) :-
	db__fun([f(Name,N)|TermL],Form2,@(ex,[Y],Form3)),
	eq__match(Form3,Form1,Sub),
	eq__apply_sub_qf([f(Name,N)|TermL],Sub,Term),
	lst__member_check(Y => Term,Sub),
	eq__apply_plain(Form2,Sub,Form4).

%%d def__fun_uniqueness(gr::in,int::in,fml::in,fml::out)

def__fun_uniqueness(Name,N,Form1,Form6) :-
	db__fun([f(Name,N)|TermL],Form2,@(ex,[Y],Form3)),
	(	Form1 = [=,[f(Name,N)|Term2L],Term]
	;	Form1 = [=,Term,[f(Name,N)|Term2L]]
	),
	obj__make_varL(V1,TermL),
	eq__make_sub(V1,Term2L,V2,Sub),
	eq__max_qfL(Term2L,0,N1),
	eq__max(Form2,N1,N2),
	eq__apply(Form2,V2,Sub,N2,Form4),
	eq__max(Form3,N1,N3),
	eq__max_qf(Term,N3,N4),
	eq__add_free_qf(Term,V2,V3),
	eq__apply(Form3,V3,[Y => Term|Sub],N4,Form5),
	cmp__conjunction([Form4,Form5],Form6).

% def.pl ends here

%   Author: Robert F. Staerk <robert.staerk@unifr.ch> 
%  Created: Tue Mar 24 16:10:41 1998 
% Updated: Mon Feb 15 11:35:10 1999
% Filename: dep.pl 
% Abstract: Dependencies between formal proofs (the information flow)

% If the flag `write_dependencies' is set with the command
%
%    :- set(write_dependencies)
%
% then the dependencies between theorems, lemmas, corollaries and
% axioms are written on the `.thm' file.
%
% If the file is later used with the command `:- needs_thm(...)'
% the dependencies are added to the internal database.
%
% We can ask about the dependencies as follows:
%
%    ?- depends(theorem(_)).
%    ?- depends(lemma(_)).
%    ?- depends(corollary(_)).
%    ?- depends(definition_fun(_,_)).
%
% We can ask about unused facts:
%
%    ?- dep__unused(X).


%%d dep__print_dependencies(gr::in)

dep__print_dependencies(Fact) :-
	dep__depends(Fact,L1),
	dep__topological_ordering(L1,[],[],L2),
	io__tell_user,
	dep__print_facts(L2).

%% dep__write_dependencies(gr::in,gr::in,drv::in)

dep__write_fact(Kind,Ref,Deriv) :-
	(	db__flag(write_dependencies),
		db__is_open(thm) ->
		dep__derivation(Deriv,[],L),
		Fact =.. [Kind,Ref],
		io__tell(thm),
		write(':- '),
		writeq(assert(db__depends(Fact,L))),
		write('.'), nl
	;	true
	).

%%d dep__write_assert(gr::in,grL::in)

dep__write_assert(X,L) :-
	(	db__flag(write_dependencies),
		db__is_open(thm) ->
		io__tell(thm),
		write(':- '),
		writeq(assert(db__depends(X,L))),
		write('.'), nl
	;	true
	).

%%d dep__derivation(drv::in,grL::in,grL::out)

dep__derivation([],L,L).
dep__derivation([Step|Deriv],L1,L3) :-
	dep__derivation_step(Step,L1,L2),
	dep__derivation(Deriv,L2,L3).

%%d dep__derivation_step(dstp::in,grL::in,grL::out)

dep__derivation_step(by(_,X),L1,L2) :-
	(	dep__by(X,Y) ->
		lst__add_element(Y,L1,L2)
	;	L2 = L1
	).
dep__derivation_step([_|_],L,L).
dep__derivation_step(@(_,_,_),L,L).
dep__derivation_step(assume(_,Deriv,_),L1,L2) :-
	dep__derivation(Deriv,L1,L2).
dep__derivation_step(cases(CaseL,_),L1,L2) :-
	dep__caseL(CaseL,L1,L2).
dep__derivation_step(exist(_,_,Deriv,_),L1,L2) :-
	dep__derivation(Deriv,L1,L2).
dep__derivation_step(induction(_,StepL),L1,L2) :-
	dep__induction_stepL(StepL,L1,L2).
dep__derivation_step(contra(_,Deriv),L1,L2) :-
	dep__derivation(Deriv,L1,L2).
dep__derivation_step(indirect(_,Deriv),L1,L2) :-
	dep__derivation(Deriv,L1,L2).

%%d dep__caseL(casL::in,grL::in,grL::out)

dep__caseL([],L,L).
dep__caseL([case(_,Deriv)|CaseL],L1,L3) :-
	dep__derivation(Deriv,L1,L2),
	dep__caseL(CaseL,L2,L3).

%%d dep__induction_stepL(istpL::in,grL::in,grL::out)

dep__induction_stepL([],L,L).
dep__induction_stepL([step(_,_,Deriv,_)|StepL],L1,L3) :-
	dep__derivation(Deriv,L1,L2),
	dep__induction_stepL(StepL,L2,L3).

%%d dep_by(gr::in,gr::out)

dep__by(X,X) :- dep__fact_form(X).
dep__by(elimination(Name,N),definition_pred(Name,N)).
dep__by(introduction(Name,N),definition_pred(Name,N)).
dep__by(existence(Name,N),definition_fun(Name,N)).
dep__by(uniqueness(Name,N),definition_fun(Name,N)).

%%d dep__fact_form(any::in)

dep__fact_form(axiom(_)).
dep__fact_form(lemma(_)).
dep__fact_form(theorem(_)).
dep__fact_form(corollary(_)).

%%d dep__topological_ordering(grL::in,grL::in,grL::in,grL::out)

dep__topological_ordering([],_,WF,WF).
dep__topological_ordering([X|L1],Path,WF1,WF3) :-
	(	lst__member_check(X,Path) ->
		ctl__error([there,is,a,cycle,through,X])
	;	lst__member_check(X,WF1) ->
		dep__topological_ordering(L1,Path,WF1,WF3)
	;	dep__depends(X,L2),
		dep__topological_ordering(L2,[X|Path],WF1,WF2),
		dep__topological_ordering(L1,Path,[X|WF2],WF3)
	).

%%d dep__depends(gr::in,grL::out)

dep__depends(X,L) :-
	(	db__depends(X,L) ->
		true
	;	ctl__warning([no,dependencies,for,X]),
		L = []
	).

%%d dep__print_facts(grL::in)

dep__print_facts([]).
dep__print_facts([X|L]) :-
	write(X), nl,
	dep__print_facts(L).

%%d dep__used(gr::in)

dep__used(X) :-
	db__depends(_,L),
	lst__member_check(X,L).

%%d dep_unused(gr::in)

dep__unused(X) :-
	db__depends(X,_),
	\+ dep__used(X).

% dep.pl ends here
/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:52:53 1994 */
/* Updated: Mon Feb 15 14:40:52 1999 */
/* Filename: ind.pl */
/* Abstract: The induction scheme. */

% %%d ind__tactic_arg(gr::in)
% 
% ind__tactic_arg(arg(Tactic,Sub2,VarL,N)) :-
% 	(	Tactic = plain
% 	; 	Tactic = tactic(V,Gamma),
% 	 	obj__varL(V),
% 		obj__formulaL(Gamma)
% 	),
% 	obj__substitution2(Sub2),
% 	obj__varL(VarL),
% 	integer(N).

%%d ind__gen_err(gr::in,fmlL::in,istpL::out)

% Tactic in [plain,tactic(V,Gamma)]

ind__gen_err(Tactic,FormL,StepL) :-
	(	ind__make_sub(FormL,U,V,K) ->
		(	ind__steps(FormL,arg(Tactic,U,V,K),[],StepL) ->
			true
		;	ctl__error([cannot,create,induction,scheme]),
			StepL = []
		)
	;	i2e__expressionL(FormL,XL),
		ctl__error([XL,cannot,be,used,for,induction]),
		StepL = []
	).

%%d ind__make_sub(fmlL::in,sub2::out,varL::out,int::out)

% ind__make_sub(_,U,V,K) and sub(Name,N,BV,Form) is in U then
%   if $(X) is free in Form and X is not in BV then X is in V,
%   if $(I) is free in Form and I is not in BV then I < K.

ind__make_sub([],[],[],0).
ind__make_sub([@(all,BV,[=>,[succeeds,[n(Name,N)|TermL]],Form])|FormL],
		[sub(Name,N,BV,Form)|U],V2,K2) :-
	obj__make_varL(BV,TermL),
	ind__make_sub(FormL,U,V1,K1),
	\+ obj__domain2(Name,N,U),
	eq__add_max_free_bound(Form,BV,K1,V1,K2,V2).

%%d ind__steps(fmlL::in,itac::in,istpL::in,istpL::out)

ind__steps([@(all,_,[=>,[succeeds,[Tag|_]],_])|FormL],X,Step1L,Step3L) :-
	cmp__clauses_err(Tag,ClauseL),
	ind__clauses(ClauseL,X,Step2L,Step3L),
	ind__steps(FormL,X,Step1L,Step2L).
ind__steps([],_,StepL,StepL).

%%d ind__clauses(clsL::in,itac::in,istpL::in,istpL::out)

ind__clauses([Clause|ClauseL],X,Step1L,Step3L) :-
	ind__clause(Clause,X,Step2L,Step3L),
	ind__clauses(ClauseL,X,Step1L,Step2L).
ind__clauses([],_,StepL,StepL).

%%d ind__clause(cl::in,itac::in,istpL::in,istp::out)

ind__clause(clause(Atom,Goal,V/K),X,Step1L,Step2L) :-
	cmp__succeeds_goal(Goal,Form),
	(	ind__normal_body(Form) ->
		ind__formula_to_list(Form,FormL),
		ind__normal_clause(FormL,Atom,V,K,X,Step),
		Step2L = [Step|Step1L]
	;	X = arg(_,U,_,_),
		ind__dnf_formula(Form,U,FormLL),
		ind__normal_clauses(FormLL,Atom,V,K,X,Step1L,Step2L)
	).

%%d ind__normal_clauses(fmlLL::in,atm::in,varL::in,int::in,itac::in,istpL::in,istpL::out)

ind__normal_clauses([FormL|FormLL],Atom,V,K,X,Step1L,[Step|Step2L]) :-
	ind__normal_clause(FormL,Atom,V,K,X,Step),
	ind__normal_clauses(FormLL,Atom,V,K,X,Step1L,Step2L).
ind__normal_clauses([],_,_,_,_,StepL,StepL).

%%d ind__normal_clause(fmlL::in,atm::in,varL::in,int::in,itac::in,istp::out)

ind__normal_clause(Form1L,Atom1,V1,K1,arg(Tactic,U,V2,K2),Step) :-
	cmp__maximum(K1,K2,K3),
	(	Tactic = tactic(V3,_) ->
		lst__append_set(V3,V2,V4)
	;	V4 = V2
	),
	eq__apply_extend(V1,V4,[],K3,BV,_,Sub,K4),
	eq__apply_sub_qf(Atom1,Sub,Atom2),
	ind__apply([succeeds,Atom2],U,K4,Form),
	eq__apply_sub_qfL(Form1L,Sub,Form2L),
	ind__apply_add(Form2L,U,K4,Form2L,Form3L),
	ind__tactic(Tactic,Form3L,Form,Deriv),
	Step = step(BV,Form3L,Deriv,Form).

%%d ind__tactic(gr::in,fml::in,drv::out)

ind__tactic(plain,_,_,[by([\/],gap)]).
ind__tactic(tactic(V1,Gamma1),FormL,Form,Deriv) :-
	pr__add_assumptionL(FormL,V1,Gamma1,V2,Gamma2),
	tac__conclusion(Form,V2,Gamma2,d,Deriv).

%%d ind__apply(fml::in,sub2::in,int::in,fml::out)

ind__apply([succeeds,[n(Name,N)|TermL]],U,K,Form2) :-
	lst__member(sub(Name,N,V1,Form1),U),
	eq__make_sub(V1,TermL,V2,Sub),
	eq__apply(Form1,V2,Sub,K,Form2).
	
%%d ind__apply_add(fmlL::in,sub2::in,int::in,fmlL::in,fmlL::out)

ind__apply_add([],_,_,FormL,FormL).
ind__apply_add([Form|FormL],U,K,Form2L,Form4L) :-
	(	ind__apply(Form,U,K,Form2) ->
		Form4L = [Form2|Form3L]
	;	Form4L = Form3L
	),
	ind__apply_add(FormL,U,K,Form2L,Form3L).

%%d ind__normal_body(fml::in)

ind__normal_body([&|FormL]) :- 
	ind__normalL(FormL).
ind__normal_body(Form) :- 
	ind__literal_form(Form).

%%d ind__normalL(fmlL::in)

ind__normalL([]).
ind__normalL([Form|FormL]) :-
	ind__literal_form(Form),
	ind__normalL(FormL).

%%d ind__literal_form(fml::in)

ind__literal_form([succeeds,_]).
ind__literal_form([fails,_]).
ind__literal_form([=,_,_]).
ind__literal_form([<>,_,_]).

%%d ind__formula_to_list(fml::in,fmlL::out)
	
ind__formula_to_list(Form,FormL) :-
	(	obj__conjunction_form(Form) ->
		Form = [&|FormL]
	;	FormL = [Form]
	).

%%d ind__free(fml::in,sub2::in)

ind__free([=,_,_],_).
ind__free([<>,_,_],_).
ind__free([succeeds,[n(Name,N)|_]],U) :- 
	\+ obj__domain2(Name,N,U).
ind__free([fails,_],_).
ind__free([&|FormL],U) :-
	ind__freeL(FormL,U).
ind__free([\/|FormL],U) :-
	ind__freeL(FormL,U).

%%d ind__freeL(fmlL::in,sub2::in)

ind__freeL([],_).
ind__freeL([Form|FormL],U) :-
	ind__free(Form,U),
	ind__freeL(FormL,U).

%%d ind__dnf_formula(fml::in,sub2::in,fmlLL::out)

ind__dnf_formula(Form,U,FormLL) :-
	ind__dnf_add_formula(Form,U,[[]],FormLL).

%%d ind__dnf_add_formula(fml::in,sub2::in,fmlLL::in,fmlLL::out)

% ind__dnf_add_formula(F1,_,[[F2,...],...],[[F3,...],...]) =>
% 
% F1 & ((F2 & ... ) \/ ... ) <=> ((F3 & ... ) \/ ... )

ind__dnf_add_formula(Form1,U,Form1LL,Form2LL) :-
	(	(	ind__literal_form(Form1)
		; 	ind__free(Form1,U)
		) ->
		ind__dnf_add_atom(Form1,Form1LL,Form2LL)
	;	obj__conjunction_form(Form1) ->
		Form1 = [&|FormL],
		ind__dnf_add_con(FormL,U,Form1LL,Form2LL)
	;	obj__disjunction_form(Form1) ->
		Form1 = [\/|FormL],
		ind__dnf_add_dis(FormL,U,Form1LL,Form2LL)
	).

%%d ind__dnf_add_atom(fml::in,fmlLL::in,fmlL::out)

% ind__dnf_add_atom(F1,_,[[F2,...],...],[[F3,...],...]) =>
% 
% F1 & ((F2 & ... ) \/ ... ) <=> ((F3 & ... ) \/ ... )

ind__dnf_add_atom(_,[],[]).
ind__dnf_add_atom(Form,[Form1L|Form1LL],[Form3L|Form2LL]) :-
	(	obj__conjunction_form(Form) ->
		Form = [&|Form2L],
		lst__append(Form2L,Form1L,Form3L)
	;	Form3L = [Form|Form1L]
	),
	ind__dnf_add_atom(Form,Form1LL,Form2LL).

%%d ind__dnf_add_con(fmlL::in,sub2::in,fmlLL::in,fmlLL::out)

% ind__dnf_add_atom([F1,...],_,[[F2,...],...],[[F3,...],...]) =>
% 
% (F1 & ... ) & ((F2 & ... ) \/ ... ) <=> ((F3 & ... ) \/ ... )

ind__dnf_add_con([],_,FormLL,FormLL).
ind__dnf_add_con([Form|FormL],U,Form1LL,Form3LL) :-
	ind__dnf_add_con(FormL,U,Form1LL,Form2LL),
	ind__dnf_add_formula(Form,U,Form2LL,Form3LL).

%%d ind__dnf_add_dis(fmlL::in,sub2::in,fmlLL::in,fmlLL::out)

% ind__dnf_add_atom([F1,...],_,[[F2,...],...],[[F3,...],...]) =>
% 
% (F1 \/ ... ) & ((F2 & ... ) \/ ... ) <=> ((F3 & ... ) \/ ... )

ind__dnf_add_dis([],_,_,[]).
ind__dnf_add_dis([Form|FormL],U,FormLL,Form3LL) :-
	ind__dnf_add_formula(Form,U,FormLL,Form1LL),
	ind__dnf_add_dis(FormL,U,FormLL,Form2LL),
	lst__append(Form1LL,Form2LL,Form3LL).

%%d ind__pr_induction(varL::in,fmlL::in,fml::in,istpL::in)

ind__pr_induction(V,Gamma,FormL,DerivL) :-
	ind__gen_err(plain,FormL,StepL),
	ind__pr_induction_steps(DerivL,StepL,V,Gamma).

%%d ind__pr_induction_steps(istpL::in,istpL::in,varL::in,fmlL::in)

ind__pr_induction_steps([],[],_,_).
ind__pr_induction_steps([step(BV1,Form1L,Deriv,Form1)|DerivL],
	[step(BV2,Form2L,_,Form2)|StepL],V1,Gamma1) :-
	lst__disjoint(BV1,V1),
	(	eq__alpha(@(all,BV1,[&,Form1|Form1L]),
			  @(all,BV2,[&,Form2|Form2L])) ->
		true
	;	i2e__expressionL([Form1|Form1L],[X1|X1L]),
		i2e__expressionL([Form2|Form2L],[X2|X2L]),
		ctl__warning([mismatch,in,induction,step,
			p(X1),p(X2),p(X1L),p(X2L)])
	),
	pr__add_assumptionL(Form1L,V1,Gamma1,V2,Gamma2),
	pr__proof(Deriv,V2,Gamma2,Gamma3),
	pr__derivable_err(V2,Gamma3,Form1),
	ind__pr_induction_steps(DerivL,StepL,V1,Gamma1).

% ind.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:52:06 1994 */
/* Updated: Tue Feb 16 10:46:46 1999 */
/* Filename: eq.pl */
/* Abstract: Equality predicates for terms with abstractions. */



%%% alpha equality between expressions 



%%d eq__alpha(exp::in,exp::in)

eq__alpha(Expr1,Expr2) :- eq__alpha_bound(Expr1,Expr2,[]).

%%d eq__alpha_bound(exp::in,exp::in,ass::in)

eq__alpha_bound($(X1),$(X2),Ass) :-		% Ass is a list of pairs of
	eq__alpha_correspond(Ass,X1,X2).	% the form X1 - X2
eq__alpha_bound([Tag|Expr1L],[Tag|Expr2L],Ass) :-
	eq__alpha_boundL(Expr1L,Expr2L,Ass).
eq__alpha_bound(@(Tag,V1,Expr1),@(Tag,V2,Expr2),Ass1) :-
	eq__alpha_extend(V1,V2,Ass1,Ass2),
	eq__alpha_bound(Expr1,Expr2,Ass2).

%%d eq__alpha_extend(varL::in,varL::in,ass::in,ass::out)

eq__alpha_extend([],[],Ass,Ass).
eq__alpha_extend([X1|V1],[X2|V2],A1L,[X1 - X2|A2L]) :-
	eq__alpha_extend(V1,V2,A1L,A2L).

%%d eq__alpha_boundL(expL::in,expL::in,ass::in)

eq__alpha_boundL([],[],_).
eq__alpha_boundL([Expr1|Expr1L],[Expr2|Expr2L],Ass) :-
	eq__alpha_bound(Expr1,Expr2,Ass),
	eq__alpha_boundL(Expr1L,Expr2L,Ass).

%%d eq__alpha_correspond(ass::in,gr::in,gr::in)

eq__alpha_correspond([X - Y|_],X,Y).
eq__alpha_correspond([U - V|Ass],X,Y) :-
	\+ (X = U),
	\+ (Y = V),
	eq__alpha_correspond(Ass,X,Y).
eq__alpha_correspond([],X,X).



%%% maximum integer variable of an expression



%%d eq__max(exp::in,int::in,int::out)

% eq__max(Expr,N1,N2) => 
%   N2 = max( {N1} union {M +1 | $(M) free in Expr, integer(M)} )

eq__max(Expr,N1,N2) :-
	eq__max_bound(Expr,[],N1,N2).

%%d eq__max_bound(exp::in,varL::in,int::in,int::out)

eq__max_bound($(X),BV,N1,N2) :-
	(	lst__member_check(X,BV) -> 
		N2 = N1
	; 	eq__max_var(X,N1,N2)
	).
eq__max_bound([_|ExprL],BV,N1,N2) :-
	eq__max_boundL(ExprL,BV,N1,N2).
eq__max_bound(@(_,BV1,Expr),BV2,N1,N2) :-
	lst__append_set(BV1,BV2,BV3),
	eq__max_bound(Expr,BV3,N1,N2).

%%d eq__max_boundL(expL::in,varL::in,int::in,int::out)

eq__max_boundL([],_,N,N).
eq__max_boundL([Expr|ExprL],BV,N1,N3) :-
	eq__max_bound(Expr,BV,N1,N2),
	eq__max_boundL(ExprL,BV,N2,N3).

%%d eq__max_var(gr::in,int::in,int::out)

eq__max_var(X,N,N) :-
	atom(X).
eq__max_var(X,N1,N2) :-
	integer(X),
	(	X < N1 -> 
		N2 = N1
	; 	N2 is X + 1
	).

%%d eq__max_varL(varL::in,int::in,int::out)

eq__max_varL([],N,N).
eq__max_varL([X|XL],N1,N3) :-
	eq__max_var(X,N1,N2),
	eq__max_varL(XL,N2,N3).

%%d eq__max_qf(exp::in,int::in,int::out)

eq__max_qf($(X),N1,N2) :-
	eq__max_var(X,N1,N2).
eq__max_qf([_|ExprL],N1,N2) :-
	eq__max_qfL(ExprL,N1,N2).

%%d eq__max_qfL(expL::in,int::in,int::out)

eq__max_qfL([],N,N).
eq__max_qfL([Expr|ExprL],N1,N3) :-
	eq__max_qf(Expr,N1,N2),
	eq__max_qfL(ExprL,N2,N3).



%%% alpha norm of an expression



%%d eq__norm(exp::in,exp::out)

eq__norm(Expr1,Expr2) :-
	eq__max(Expr1,0,N),
	eq__norm_bound(Expr1,N,[],Expr2).

%%d eq__norm_bound(exp::in,int::in,ass::in,exp::out)

eq__norm_bound($(V1),_,Ass,$(V2)) :-
	eq__assoc(Ass,V1,V2).
eq__norm_bound([Tag|Expr1L],N,Ass,[Tag|Expr2L]) :-
	eq__norm_boundL(Expr1L,N,Ass,Expr2L).
eq__norm_bound(@(Tag,V,Expr1),N1,Ass1,@(Tag,NL,Expr2)) :-
	eq__norm_extend(V,N1,Ass1,N2,Ass2,NL),
	eq__norm_bound(Expr1,N2,Ass2,Expr2).

%%d eq__norm_extend(varL::in,int::in,ass::in,int::out,ass::out,intL::out)

eq__norm_extend([],N,Ass,N,Ass,[]).
eq__norm_extend([X|V],N1,Ass1,N3,[X - N1|Ass2],[N1|NL]) :-
	N2 is N1 + 1,
	eq__norm_extend(V,N2,Ass1,N3,Ass2,NL).

%%d eq__norm_boundL(expL::in,int::in,ass::in,expL::out)

eq__norm_boundL([],_,_,[]).
eq__norm_boundL([Expr1|Expr1L],N,Ass,[Expr2|Expr2L]) :-
	eq__norm_bound(Expr1,N,Ass,Expr2),
	eq__norm_boundL(Expr1L,N,Ass,Expr2L).

%%d eq__assoc(ass::in,gr::in,gr::out)

eq__assoc([],X,X).
eq__assoc([X1 - Y1|Ass],X2,Y2) :-
	(	X1 = X2 -> 
		Y2 = Y1
	; 	eq__assoc(Ass,X2,Y2)
	).

%%d eq__norm_equal(exp::in,exp::in)

eq__norm_equal(Expr1,Expr2) :-			% alpha equality
	eq__norm(Expr1,Expr3),
	eq__norm(Expr2,Expr3).



%%% free variables of an expression



%%d eq__occurs_qf(tm::in,gr::in)

eq__occurs_qf($(X),X).
eq__occurs_qf([_|TermL],X) :-
	lst__member(Term,TermL),
	eq__occurs_qf(Term,X).

%%d eq__free_qf(exp::in,varL::in)

eq__free_qf($(X),V) :-
	\+ lst__member_check(X,V).
eq__free_qf([_|ExprL],V) :-
	eq__free_qfL(ExprL,V).

%%d eq__free_qfL(expL::in,varL::in)

eq__free_qfL([],_).
eq__free_qfL([Expr|ExprL],V) :-
	eq__free_qf(Expr,V),
	eq__free_qfL(ExprL,V).

%%d eq__free(exp::in,varL::in)

eq__free(Expr,V) :- eq__free_bound(Expr,V,[]).

%%d eq__free_bound(exp::in,varL::in,varL::in)

eq__free_bound($(X),V,BV) :-
	(	lst__member_check(X,BV) ->
		true
	; 	\+ lst__member_check(X,V)
	).
eq__free_bound([_|ExprL],V,BV) :-
	eq__free_boundL(ExprL,V,BV).
eq__free_bound(@(_,BV1,Expr),V,BV2) :-
	lst__append_set(BV1,BV2,BV3),
	eq__free_bound(Expr,V,BV3).

%%d eq__free_boundL(expL::in,varL::in,varL::in)

eq__free_boundL([],_,_).
eq__free_boundL([Expr|ExprL],V,BV) :-
	eq__free_bound(Expr,V,BV),
	eq__free_boundL(ExprL,V,BV).

%%d eq__add_free_qf(exp::in,varL::in,varL::out)

eq__add_free_qf($(X),V1,V2) :-
	lst__add_element(X,V1,V2).
eq__add_free_qf([_|ExprL],V1,V2) :-
	eq__add_free_qfL(ExprL,V1,V2).

%%d eq__add_free_qfL(expL::in,varL::in,varL::out)

eq__add_free_qfL([],V,V).
eq__add_free_qfL([Expr|ExprL],V1,V3) :-
	eq__add_free_qf(Expr,V1,V2),
	eq__add_free_qfL(ExprL,V2,V3).

%%d eq__add_free(exp::in,varL::in,varL::out)

eq__add_free(Expr,V1,V2) :-
	eq__add_free_bound(Expr,[],V1,V2).

%%d eq__add_free_bound(exp::in,varL::in,varL::in,varL::out)

eq__add_free_bound($(X),BV,V1,V2) :-
	(	lst__member_check(X,BV) -> 
		V2 = V1 
	; 	lst__add_element(X,V1,V2)
	).
eq__add_free_bound([_|ExprL],BV,V1,V2) :-
	eq__add_free_boundL(ExprL,BV,V1,V2).
eq__add_free_bound(@(_,BV1,Expr),BV2,V1,V2) :-
	lst__append_set(BV1,BV2,BV3),
	eq__add_free_bound(Expr,BV3,V1,V2).

%%d eq__add_free_boundL(expL::in,varL::in,varL::in,varL::out)

eq__add_free_boundL([],_,V,V).
eq__add_free_boundL([Expr|ExprL],BV,V1,V3) :-
	eq__add_free_bound(Expr,BV,V1,V2),
	eq__add_free_boundL(ExprL,BV,V2,V3).

%%d eq__add_max_free_qf(exp::in,int::in,varL::in,int::out,varL::out)

eq__add_max_free_qf($(X),N1,V1,N2,V2) :-
	eq__max_var(X,N1,N2),
	lst__add_element(X,V1,V2).
eq__add_max_free_qf([_|ExprL],N1,V1,N2,V2) :-
	eq__add_max_free_qfL(ExprL,N1,V1,N2,V2).

%%d eq__add_max_free_qfL(expL::in,int::in,varL::in,int::out,varL::out)

eq__add_max_free_qfL([],N,V,N,V).
eq__add_max_free_qfL([Expr|ExprL],N1,V1,N3,V3) :-
	eq__add_max_free_qf(Expr,N1,V1,N2,V2),
	eq__add_max_free_qfL(ExprL,N2,V2,N3,V3).

%%d eq__add__free_max(exp::in,int::in,varL::in,int::out,varL::out)

eq__add_max_free(Expr,N1,V1,N2,V2) :-
	eq__add_max_free_bound(Expr,[],N1,V1,N2,V2).

%%d eq__add_max_free_bound(exp::in,varL::in,int::in,varL::in,int::out,varL::out)

eq__add_max_free_bound($(X),BV,N1,V1,N2,V2) :-
	(	lst__member_check(X,BV) ->
		N2 = N1,
		V2 = V1
	;	lst__add_element(X,V1,V2),
		eq__max_var(X,N1,N2)
	).
eq__add_max_free_bound([_|ExprL],BV,N1,V1,N2,V2) :-
	eq__add_max_free_boundL(ExprL,BV,N1,V1,N2,V2).
eq__add_max_free_bound(@(_,BV1,Expr),BV2,N1,V1,N2,V2) :-
	lst__append_set(BV1,BV2,BV3),
	eq__add_max_free_bound(Expr,BV3,N1,V1,N2,V2).

%%d eq__add_max_free_boundL(expL::in,varL::in,int::in,varL::in,int::out,varL::out)

eq__add_max_free_boundL([],_,N,V,N,V).
eq__add_max_free_boundL([Expr|ExprL],BV,N1,V1,N3,V3) :-
	eq__add_max_free_bound(Expr,BV,N1,V1,N2,V2),
	eq__add_max_free_boundL(ExprL,BV,N2,V2,N3,V3).




%%% matching of expressions




%%d eq__match_extend(gr::in,tm::in,sub::in,sub::out)

eq__match_extend(X,Term1,Sub1,Sub2) :-
	(	lst__member(X => Term2,Sub1) ->
			Term1 = Term2, 
			Sub2 = Sub1
	; 	Sub2 = [X => Term1|Sub1]
	).

%%d eq__match_qf(exp::in,exp::in,sub::in,sub::out)

eq__match_qf($(X),Term,Sub1,Sub2) :-
	eq__match_extend(X,Term,Sub1,Sub2).
eq__match_qf([Tag|Expr1L],[Tag|Expr2L],Sub1,Sub2) :-
	eq__match_qfL(Expr1L,Expr2L,Sub1,Sub2).

%%d eq__match_qfL(expL::in,expL::in,sub::in,sub::out)

eq__match_qfL([],[],Sub,Sub).
eq__match_qfL([Expr1|Expr1L],[Expr2|Expr2L],Sub1,Sub3) :-
	eq__match_qf(Expr1,Expr2,Sub1,Sub2),
	eq__match_qfL(Expr1L,Expr2L,Sub2,Sub3).

%%d eq__match(exp::in,exp::in,sub::out)

eq__match(Expr1,Expr2,Sub) :-
	eq__match_bound(Expr1,Expr2,[],[],[],[],Sub).

%%d eq__match_bound(exp::in,exp::in,varL::in,varL::in,ass::in,sub::in,sub::out)

eq__match_bound($(X1),Term,BV1,BV2,Ass,Sub1,Sub2) :-
	(	lst__member_check(X1,BV1) -> 
		Term = $(X2),
		eq__assoc(Ass,X1,X2),
		Sub2 = Sub1
	;	eq__free_qf(Term,BV2),
		eq__match_extend(X1,Term,Sub1,Sub2)
	).
eq__match_bound([Tag|Expr1L],[Tag|Expr2L],BV1,BV2,Ass,Sub1,Sub2) :-
	eq__match_boundL(Expr1L,Expr2L,BV1,BV2,Ass,Sub1,Sub2).
eq__match_bound(@(Tag,V1,Expr1),@(Tag,V2,Expr2),
	BV1,BV2,Ass1,Sub1,Sub2) :-
	eq__alpha_extend(V1,V2,Ass1,Ass2),
	lst__append_set(V1,BV1,BV3),
	lst__append_set(V2,BV2,BV4),
	eq__match_bound(Expr1,Expr2,BV3,BV4,Ass2,Sub1,Sub2).

%%d eq__match_boundL(expL::in,expL::in,varL::in,varL::in,ass::in,sub::in,sub::out)

eq__match_boundL([],[],_,_,_,Sub,Sub).
eq__match_boundL([Expr1|Expr1L],[Expr2|Expr2L],BV1,BV2,Ass,Sub1,Sub3) :-
	eq__match_bound(Expr1,Expr2,BV1,BV2,Ass,Sub1,Sub2),
	eq__match_boundL(Expr1L,Expr2L,BV1,BV2,Ass,Sub2,Sub3).

%%d eq__match_constrained(exp::in,exp::in,varL::in,sub::in,sub::out)

eq__match_constrained(Expr1,Expr2,XL,Sub1,Sub2) :-
	(eq__match_constrained_bound(Expr1,Expr2,[],[],[],XL,Sub1,Sub2) ->
		true
	;	fail
	).

%%d eq__match_constrained_bound(exp::in,exp::in,varL::in,varL::in,ass::in,varL::in,sub::in,sub::out)

eq__match_constrained_bound($(X1),Term,BV1,BV2,Ass,XL,Sub1,Sub2) :-
	(	lst__member_check(X1,BV1) -> 
		Term = $(X2),
		eq__assoc(Ass,X1,X2),
		Sub2 = Sub1
	;	(	Term = $(X1) ->
			true
		; 	lst__member_check(X1,XL)
		),
		eq__free_qf(Term,BV2),
		eq__match_extend(X1,Term,Sub1,Sub2)
	).
eq__match_constrained_bound([Tag|Expr1L],[Tag|Expr2L],
	BV1,BV2,Ass,XL,Sub1,Sub2) :-
	eq__match_constrained_boundL(Expr1L,Expr2L,BV1,BV2,Ass,XL,Sub1,Sub2).
eq__match_constrained_bound(@(Tag,V1,Expr1),@(Tag,V2,Expr2),
	BV1,BV2,Ass1,XL,Sub1,Sub2) :-
	eq__alpha_extend(V1,V2,Ass1,Ass2),
	lst__append_set(V1,BV1,BV3),
	lst__append_set(V2,BV2,BV4),
	eq__match_constrained_bound(Expr1,Expr2,BV3,BV4,Ass2,XL,Sub1,Sub2).

%%d eq__match_constrained_boundL(expL::in,expL::in,varL::in,varL::in,ass::in,varL::in,sub::in,sub::out)

eq__match_constrained_boundL([],[],_,_,_,_,Sub,Sub).
eq__match_constrained_boundL([Expr1|Expr1L],[Expr2|Expr2L],
	BV1,BV2,Ass,XL,Sub1,Sub3) :-
	eq__match_constrained_bound(Expr1,Expr2,BV1,BV2,Ass,XL,Sub1,Sub2),
	eq__match_constrained_boundL(Expr1L,Expr2L,BV1,BV2,Ass,XL,Sub2,Sub3).



%%% application of substitutions to expressions



%%d eq__apply(exp::in,varL::in,sub::in,int::in,exp::out)

% Preconditions for eq__apply(Expr1,V,Sub,N,Expr2):
%
%  o If $(K) is free in Expr1 then K < N.
%  o If X => Term is in Sub, $(X) <> Term and $(Y) is in Term then
%    Y is in V.
%  o If X => Term is in Sub and $(K) is in Term then K < N.

eq__apply($(X),_,Sub,_,Term) :-
	eq__apply_var(Sub,X,Term).
eq__apply([Tag|Expr1L],V,Sub,N,[Tag|Expr2L]) :-
	eq__applyL(Expr1L,V,Sub,N,Expr2L).
eq__apply(@(Tag,BV1,Expr1),V1,Sub1,N1,@(Tag,BV2,Expr2)) :-
	eq__apply_extend(BV1,V1,Sub1,N1,BV2,V2,Sub2,N2),
	eq__apply(Expr1,V2,Sub2,N2,Expr2).

%%d eq__applyL(expL::in,varL::in,sub::in,int::in,expL::out)

eq__applyL([],_,_,_,[]).
eq__applyL([Expr1|Expr1L],V,Sub,N,[Expr2|Expr2L]) :-
	eq__apply(Expr1,V,Sub,N,Expr2),
	eq__applyL(Expr1L,V,Sub,N,Expr2L).

%%d eq__apply_extend(varL::in,varL::in,sub::in,int::in,varL::out,varL::out,sub::out,int::out)

eq__apply_extend([],V,Sub,N,[],V,Sub,N).
eq__apply_extend([X|BV1],V1,Sub1,N1,BV3,V3,Sub3,N3) :-
	(	lst__member_check(X,V1) -> 
		N2 is N1 + 1,
		BV3 = [N1|BV2],
		V3 = [N1|V2],
		Sub3 = [X => $(N1)|Sub2],
		eq__apply_extend(BV1,V1,Sub1,N2,BV2,V2,Sub2,N3)
	;	BV3 = [X|BV2],
		Sub3 = [X => $(X)|Sub2],
		eq__max_var(X,N1,N2),
		eq__apply_extend(BV1,V1,Sub1,N2,BV2,V3,Sub2,N3)
	).

%%d eq__apply_var(sub::in,gr::in,tm::out)

eq__apply_var([],X,$(X)).
eq__apply_var([Y => Term1|Sub],X,Term2) :-
	(	X = Y -> 
		Term2 = Term1 
	; 	eq__apply_var(Sub,X,Term2)
	).

%%d eq__apply_sub_qf(exp::in,sub::in,exp::in)

eq__apply_sub_qf(Expr1,Sub,Expr2) :-
	(	Sub = [] ->
		Expr2 = Expr1
	;	eq__apply_qf(Expr1,Sub,Expr2)
	).

%%d eq__apply_sub_qfL(exp::in,sub::in,exp::in)

eq__apply_sub_qfL(Expr1L,Sub,Expr2L) :-
	(	Sub = [] ->
		Expr2L = Expr1L
	;	eq__apply_qfL(Expr1L,Sub,Expr2L)
	).

%%d eq__apply_qf(exp::in,sub::in,exp::in)

eq__apply_qf($(X),Sub,Term) :-
	eq__apply_var(Sub,X,Term).
eq__apply_qf([Tag|Expr1L],Sub,[Tag|Expr2L]) :-
	eq__apply_qfL(Expr1L,Sub,Expr2L).

%%d eq__apply_qfL(tmL::in,sub::in,tmL::in)

eq__apply_qfL([],_,[]).
eq__apply_qfL([Expr1|Expr1L],Sub,[Expr2|Expr2L]) :-
	eq__apply_qf(Expr1,Sub,Expr2),
	eq__apply_qfL(Expr1L,Sub,Expr2L).

%%d eq__apply_plain(exp::in,sub::in,exp::out)

eq__apply_plain(Expr1,Sub,Expr2) :-
	(	Sub = [] ->
		Expr2 = Expr1
	;	eq__max(Expr1,0,N1),
		eq__max_free_sub(Sub,N1,[],N2,V),
		eq__apply(Expr1,V,Sub,N2,Expr2)
	).

%%d eq__max_free_sub(sub::in,int::in,varL::in,int::out,varL::out)

eq__max_free_sub([],N,V,N,V).
eq__max_free_sub([_ => Term|Sub],N1,V1,N3,V3) :-
	eq__add_max_free_qf(Term,N1,V1,N2,V2),
	eq__max_free_sub(Sub,N2,V2,N3,V3).




%%% Predicates used in other files




%%d eq__make_sub(varL::in,tmL::in,varL::out,sub::out)

eq__make_sub([],[],[],[]).
eq__make_sub([X|XL],[Term|TermL],V2,[X => Term|Sub]) :-
	eq__make_sub(XL,TermL,V1,Sub),
	eq__add_free_qf(Term,V1,V2).

%%d eq__term_eq(tm::in,tm::in,tmLL::in)

eq__term_eq(Term1,Term2,TermLL) :-
	lst__member(TermL,TermLL),
	lst__member_check(Term1,TermL),
	lst__member_check(Term2,TermL).
eq__term_eq($(X),$(X),_).
eq__term_eq([Tag|Term1L],[Tag|Term2L],TermLL) :-
	eq__term_eqL(Term1L,Term2L,TermLL).

%%d eq__term_eqL(tmL::in,tmL::in,tmLL::in)

eq__term_eqL([],[],_).
eq__term_eqL([Term1|Term1L],[Term2|Term2L],TermLL) :-
	once(eq__term_eq(Term1,Term2,TermLL)),
	eq__term_eqL(Term1L,Term2L,TermLL).

%%d eq__termL_eq(tmLL::in,tmLL::in)

eq__termL_eq([_],_).
eq__termL_eq([Term1,Term2|TermL],TermLL) :-
	once(eq__term_eq(Term1,Term2,TermLL)),
	eq__termL_eq([Term2|TermL],TermLL).

%%d eq__expr_eq(exp::in,exp::in,tmLL::in)

eq__expr_eq(Expr1,Expr2,TermLL) :-
	once(eq__expr_eq_bound(Expr1,Expr2,[],[],[],TermLL)).

%%d eq__expr_eq_bound(exp::in,exp::in,varL::in,varL::in,ass::in,tmLL::in)

eq__expr_eq_bound($(X1),$(X2),_,_,Ass,_) :-
	eq__alpha_correspond(Ass,X1,X2).
eq__expr_eq_bound(Term1,Term2,BV1,BV2,_,TermLL) :-
	obj__term_form(Term1),
	obj__term_form(Term2),
	(	eq__free_qf(Term1,BV1), 
		eq__free_qf(Term2,BV2) ->
		once(eq__term_eq(Term1,Term2,TermLL))
	; 	fail
	).
eq__expr_eq_bound([Tag|Form1L],[Tag|Form2L],BV1,BV2,Ass,TermLL) :-
	eq__expr_eq_boundL(Form1L,Form2L,BV1,BV2,Ass,TermLL).
eq__expr_eq_bound(@(Tag,BV1,Form1),@(Tag,BV2,Form2),
	BV3,BV4,Ass1,TermLL) :-
	eq__alpha_extend(BV1,BV2,Ass1,Ass2),
	lst__append_set(BV1,BV3,BV5),
	lst__append_set(BV2,BV4,BV6),
	once(eq__expr_eq_bound(Form1,Form2,BV5,BV6,Ass2,TermLL)).

%%d eq__expr_eq_boundL(expL::in,expL::in,varL::in,varL::in,ass::in,tmLL::in)

eq__expr_eq_boundL([],[],_,_,_,_).
eq__expr_eq_boundL([Form1|Form1L],[Form2|Form2L],BV1,BV2,Ass,TermLL) :-
	once(eq__expr_eq_bound(Form1,Form2,BV1,BV2,Ass,TermLL)),
	eq__expr_eq_boundL(Form1L,Form2L,BV1,BV2,Ass,TermLL).

%%d eq__sub_forall(sub::in,varL::in,varL::in,varL::in)

eq__sub_forall(Sub,V,BV1,BV2) :-
	\+ eq__sub_forall_not(Sub,V,BV1,BV2).

%%d eq__sub_forall_not(sub::in,varL::in,varL::in,varL::in)

eq__sub_forall_not(Sub,V,BV1,BV2) :-
	lst__member(X => Term,Sub),
	\+ lst__member_check(X,BV2),
	lst__member_check(X,V),
	(	Term = $(X) -> 
		lst__member_check(X,BV1)
	;	 true
	).

% eq.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:53:39 1994 */
/* Filename: mgu.pl */
/* Abstract: A union-find based, quadratic unification algorithm. */
%
% The algorithm uses equivalence relations on terms represented as
% lists of classes, called partitions.
%
% %%d mgu__class(gr::in)
% 
% mgu__class(cl(Term,Part)) :-
% 	obj__term(Term),
% 	mgu__partition(Part),
% 	\+ mgu__partition_member(Term,Part),
% 
% %%d mgu__partition(grL::in)
% 
% mgu__partition([]).
% mgu__partition([Class|Part]) :-
% 	mgu__class(Class),
% 	mgu__partition(Part),
% 	mgu__disjoint(Class,Part).
%
% %%d mgu__disjoint(cl::in,ptn::in)
% 
% mgu__disjoint(Class,Part) :- \+ mgu__not_disjoint(Class,Part).
% 
% %%d mgu__not_disjoint(cl::in,ptn::in)
% 
% mgu__not_disjoint(Class,Part) :-
% 	mgu__class_member(Term,Class),
% 	mgu__partition_member(Term,Part).
%
% %%d mgu__result(gr::in)
% 
% mgu__result(yes(Part)) :- mgu__partition(Part).
% mgu__result(no).

%%d mgu__class_member(gr::in,cl::in)

mgu__class_member(Term,cl(Term,_)).
mgu__class_member(Term,cl(_,Part)) :-
	mgu__partition_member(Term,Part).

%%d mgu__partition_member(gr::in,ptn::in)

mgu__partition_member(Term,[Class|_]) :-
	mgu__class_member(Term,Class).
mgu__partition_member(Term,[_|Part]) :-
	mgu__partition_member(Term,Part).

%%d mgu__find(ptn::in,gr::in,tm::out)

mgu__find([],Term,Term).
mgu__find([Class|Part],Term1,Term2) :-
	(	mgu__class_member(Term1,Class) ->
		Class = cl(Term2,_)
	;	mgu__find(Part,Term1,Term2)
	).

%%d mgu__find_delete(ptn::in,tm::in,ptn::out,cl::out)

mgu__find_delete([],Term,[],cl(Term,[])).
mgu__find_delete([Class1|Part1],Term,Part3,Class2) :-
	(	mgu__class_member(Term,Class1) -> 
		Class2 = Class1,
		Part3 = Part1
	;	mgu__find_delete(Part1,Term,Part2,Class2),
		Part3 = [Class1|Part2]
	).

%%d mgu__union_find(tm::in,tm::in,ptn::in,ptn::out)

mgu__union_find(Term1,Term2,Part1,Part4) :-
	mgu__find_delete(Part1,Term1,Part2,Class1),
	(	mgu__class_member(Term2,Class1) -> 
		Part4 = Part1
	;	mgu__find_delete(Part2,Term2,Part3,Class2),
		Class1 = cl(Term3,Q1),
		Class2 = cl(Term4,Q2),
		(	obj__var_form(Term3) ->
			Part4 = [cl(Term4,[Class1|Q2])|Part3]
		;	obj__var_form(Term4) ->
			Part4 = [cl(Term3,[Class2|Q1])|Part3]
		;	Term3 = [Tag|Term1L], 
			Term4 = [Tag|Term2L],
			mgu__union_findL(Term1L,Term2L,
				[cl(Term4,[Class1|Q2])|Part3],Part4)
	    	)
	).

%%d mgu__union_findL(tmL::in,tmL::in,ptn::in,ptn::out)

mgu__union_findL([],[],Part,Part).
mgu__union_findL([Term1|Term1L],[Term2|Term2L],Part1,Part3) :-
	mgu__union_find(Term1,Term2,Part1,Part2),
	mgu__union_findL(Term1L,Term2L,Part2,Part3).

%%d mgu__cycle_free(ptn::in)

mgu__cycle_free(Part) :-
	mgu__roots(Part,TermL),
	mgu__cycle_freeL(TermL,Part,[],[],_).

%%d mgu__roots(ptn::in,tmL::out)

mgu__roots([],[]).
mgu__roots([cl(Term,_)|Part],[Term|TermL]) :-
	mgu__roots(Part,TermL).

%%d mgu__cycle_freeL(tmL::in,ptn::in,tmL::in,tmL::in,tmL::out)

mgu__cycle_freeL([],_,_,TermL,TermL).
mgu__cycle_freeL([Term1|Term1L],Part,Term2L,Term3L,TermL) :-
	mgu__find(Part,Term1,Term2),
	(	lst__member_check(Term2,Term2L) ->
		fail
	;	lst__member_check(Term2,Term3L) ->
		mgu__cycle_freeL(Term1L,Part,Term2L,Term3L,TermL)
	;	obj__var_form(Term2) ->
		mgu__cycle_freeL(Term1L,Part,Term2L,[Term2|Term3L],TermL)
	;	Term2 = [_|Term4L],
		mgu__cycle_freeL(Term4L,Part,[Term2|Term2L],Term3L,Term5L),
		mgu__cycle_freeL(Term1L,Part,Term2L,[Term2|Term5L],TermL)
	).

%%d mgu__unify_terms_part(tm::in,tm::in,ptn::out)

mgu__unify_terms_part(Term1,Term2,Part) :- 
	mgu__union_find(Term1,Term2,[],Part),
	mgu__cycle_free(Part).

%%d mgu__unifiable_terms(tm::in,tm::in)

mgu__unifiable_terms(Term1,Term2) :- mgu__unify_terms_part(Term1,Term2,_).

%%d mgu__unify_terms_sub(tm::in,tm::in,sub::out)

mgu__unify_terms_sub(Term1,Term2,Sub) :-
	mgu__unify_terms_part(Term1,Term2,Part),
	mgu__partition_sub(Part,Part,[],Sub).

%%d mgu__unify_termL_sub(tmL::in,tmL::in,sub::out)

mgu__unify_termL_sub(Term1L,Term2L,Sub) :-
	mgu__union_findL(Term1L,Term2L,[],Part),
	mgu__cycle_free(Part),
	mgu__partition_sub(Part,Part,[],Sub).

%%d mgu__partition_sub(ptn::in,ptn::in,sub::in,sub::out)

mgu__partition_sub([],_,Sub,Sub).
mgu__partition_sub([Class|Part1],Part2,Sub1,Sub3) :-
	mgu__class_sub(Class,Part2,Sub1,Sub2),
	mgu__partition_sub(Part1,Part2,Sub2,Sub3).

%%d mgu__class_sub(cl::in,ptn::in,sub::in,sub::out)

mgu__class_sub(cl($(X),Part1),Part2,Sub1,Sub2) :-
	mgu__partition_term($(X),Part2,Term),
	mgu__partition_sub(Part1,Part2,[X => Term|Sub1],Sub2).
mgu__class_sub(cl([_|_],Part1),Part2,Sub1,Sub2) :-
	mgu__partition_sub(Part1,Part2,Sub1,Sub2).

%%d mgu__partition_term(tm::in,ptn::in,tm::out)

mgu__partition_term(Term1,Part,Term3) :-
	mgu__find(Part,Term1,Term2),
	(	obj__var_form(Term2) ->
		Term3 = Term2
	;	Term2 = [Tag|Term1L],
		mgu__partition_termL(Term1L,Part,Term2L),
		Term3 = [Tag|Term2L]
	).

%%d mgu__partition_termL(tmL::in,ptn::in,tmL::out)

mgu__partition_termL([],_,[]).
mgu__partition_termL([Term1|Term1L],Part,[Term2|Term2L]) :-
	mgu__partition_term(Term1,Part,Term2),
	mgu__partition_termL(Term1L,Part,Term2L).

%%d mgu__unify_gamma(fmlL::in,mgu::out)

mgu__unify_gamma(Gamma,X) :-
	mgu__filter_equations(Gamma,[],TermLL),
	(	mgu__unify_termLL(TermLL,[],Part), 
		mgu__cycle_free(Part) ->
		X = yes(Part)
	;	X = no
	).

%%d mgu__filter_equations(fmlL::in,tmLL::out)

mgu__filter_equations([],TermLL,TermLL).
mgu__filter_equations([Form|Gamma],Term1LL,Term3LL) :-
	(	Form = [=|TermL],
		obj__pure_termL(TermL) ->
		mgu__filter_equations(Gamma,[TermL|Term1LL],Term3LL)
	;	Form = [&|FormL] ->
		mgu__filter_equations(FormL,Term1LL,Term2LL),
		mgu__filter_equations(Gamma,Term2LL,Term3LL)
	;	mgu__filter_equations(Gamma,Term1LL,Term3LL)
	).

%%d mgu__unify_termLL(tmLL::in,ptn::in,ptn::out)

mgu__unify_termLL([],Part,Part).
mgu__unify_termLL([TermL|TermLL],Part1,Part3) :-
	mgu__unify_termL(TermL,Part1,Part2),
	mgu__unify_termLL(TermLL,Part2,Part3).

%%d mgu__unify_termL(tmL::in,ptn::in,ptn::out)

mgu__unify_termL([],Part,Part).
mgu__unify_termL([Term|TermL],Part1,Part2) :-
	mgu__unify_term_termL(TermL,Term,Part1,Part2).

%%d mgu__unify_term_termL(tmL::in,tm::in,ptn::in,ptn::out)

mgu__unify_term_termL([],_,Part,Part).
mgu__unify_term_termL([Term1|TermL],Term2,Part1,Part3) :-
	mgu__union_find(Term1,Term2,Part1,Part2),
	mgu__unify_term_termL(TermL,Term2,Part2,Part3).

%%d mgu__term_eq(tm::in,tm::in,ptn::in)

mgu__term_eq(Term1,Term2,Part) :-
	mgu__term_eq(Term1,Term2,Part,_).

%%d mgu__term_eq(tm::in,tm::in,ptn::in,ptn::out)

mgu__term_eq(Term1,Term2,Part1,Part5) :-
	mgu__find_delete(Part1,Term1,Part2,Class1),
	(	mgu__class_member(Term2,Class1) ->
		Part5 = Part1
	;	mgu__find_delete(Part2,Term2,Part3,Class2),
		Class1 = cl(Term3,_),
		Class2 = cl(Term4,Q2),
		(	Term3 = [Tag|Term1L], Term4 = [Tag|Term2L] -> 
			mgu__term_eqL(Term1L,Term2L,Part3,Part4),
			Part5 = [cl(Term4,[Class1|Q2])|Part4]
		;	fail
		)
	).

%%d mgu__term_eqL(tmL::in,tmL::in,ptn::in,ptn::out)

mgu__term_eqL([],[],Part,Part).
mgu__term_eqL([Term1|Term1L],[Term2|Term2L],Part1,Part3) :-
	mgu__term_eq(Term1,Term2,Part1,Part2),
	mgu__term_eqL(Term1L,Term2L,Part2,Part3).

%%d mgu__expr_eq(exp::in,exp::in,ptn::in)

mgu__expr_eq(Exp1,Exp2,Part) :- mgu__expr_bound_eq(Exp1,Exp2,[],[],[],Part).

%%d mgu__expr_bound_eq(exp::in,exp::in,varL::in,varL::in,ass::in,ptn::in)

mgu__expr_bound_eq($(X1),Term,Bnd1L,Bnd2L,Ass,Part) :-
	(	lst__member_check(X1,Bnd1L) -> 
		Term = $(X2),
		eq__assoc(Ass,X1,X2)
	;	eq__free_qf(Term,Bnd2L),
		mgu__term_eq($(X1),Term,Part)
	).
mgu__expr_bound_eq([Tag|TermL],$(X),Bnd1L,Bnd2L,_,Part) :-
	\+ lst__member_check(X,Bnd2L),
	eq__free_qf([Tag|TermL],Bnd1L),
	mgu__term_eq([Tag|TermL],$(X),Part).
mgu__expr_bound_eq([Tag|Expr1L],[Tag|Expr2L],Bnd1L,Bnd2L,Ass,Part) :-
	mgu__expr_bound_eqL(Expr1L,Expr2L,Bnd1L,Bnd2L,Ass,Part).
mgu__expr_bound_eq(@(TAG,Bnd1L,Expr1),@(TAG,Bnd2L,Expr2),
		Bnd3L,Bnd4L,Ass1,Part) :-
	eq__alpha_extend(Bnd1L,Bnd2L,Ass1,Ass2),
	lst__append_set(Bnd1L,Bnd3L,Bnd5L),
	lst__append_set(Bnd2L,Bnd4L,Bnd6L),
	mgu__expr_bound_eq(Expr1,Expr2,Bnd5L,Bnd6L,Ass2,Part).

%%d mgu__expr_bound_eqL(expL::in,expL::in,varL::in,varL::in,ass::in,ptn::in)

mgu__expr_bound_eqL([],[],_,_,_,_).
mgu__expr_bound_eqL([Expr1|Expr1L],[Expr2|Expr2L],Bnd1L,Bnd2L,Ass,Part) :-
	mgu__expr_bound_eq(Expr1,Expr2,Bnd1L,Bnd2L,Ass,Part),
	mgu__expr_bound_eqL(Expr1L,Expr2L,Bnd1L,Bnd2L,Ass,Part).

% mgu.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 7/19/95, 4:47 PM */
/* Updated: Mon Feb 15 10:29:11 1999 */
/* Filename: tac.pl */
/* Abstract: Some simple tactics. */


% tac__conclusion(Form,V,Gamma,m,Deriv) =>
%   exist Gamma1 such that pr__proof(Deriv,V,Gamma,Gamma1) and
%   lst__member(Form,Gamma1).
% 
% tac__conclusion(Form,V,Gamma,d,Deriv) =>
%   exist Gamma1 such that pr__proof(Deriv,V,Gamma,Gamma1) and
%   pr__derivable(V,Gamma1,Form).
% 
% tac__conclusion_acc(Form,V,Gamma,m,Deriv1,Deriv2) => 
%   Deriv2 = Deriv3 ** Deriv1 and 
%   there exist Gamma3 such that pr__proof(Deriv3,V,Gamma,Gamma3)
%   and lst__member(Form,Gamma3).
% 
% tac__conclusion_acc(Form,V,Gamma,d,Deriv1,Deriv2) =>
%   Deriv2 = Deriv3 ** Deriv1 and 
%   there exist Gamma3 such that pr__proof(Deriv3,V,Gamma,Gamma3)
%   and pr__derivable(V,Gamma1,Form).
% 
% tac__hypothesis(Form1,Form2,V,Gamma,m,Deriv) =>
%   if add_free(Form1,V,V1) then there exist a Gamma1 such that 
%   pr__proof(Deriv,V1,[Form1|Gamma],Gamma1)
%   and lst__member(Form2,Gamma1).
% 
% tac__hypothesis(Form1,Form2,V,Gamma,d,Deriv) =>
%   if add_free(Form1,V,V1) then there exist a Gamma1 such that 
%   pr__proof(Deriv,V1,[Form1|Gamma],Gamma1)
%   and pr__derivable(V1,Gamma1,Form2).


%%d tac__conclusion(fml::in,varL::in,fmlL::in,gr::in,drv::out)

tac__conclusion(Form,VL,Gamma,Opt,Deriv) :-
	tac__conclusion_acc(Form,VL,Gamma,Opt,[],Deriv).

%%d tac__conclusion_acc(fml::in,varL::in,fmlL::in,gr::in,drv::in,drv::out)

tac__conclusion_acc(Form,V,Gamma,Opt,Deriv1,Deriv5) :-
	(	pr__derivable_once(V,Gamma,Form,s(s(s(s(s(0)))))) ->
		tac__add(Opt,Form,Deriv1,Deriv5)
	;	Form = @(all,BV1,Form1) ->
		tac__add(Opt,@(all,BV1,Form1),Deriv1,Deriv2),
		(	lst__disjoint(BV1,V) ->
			tac__conclusion_acc(Form1,V,Gamma,m,Deriv2,Deriv5)
		;	tac__rename(BV1,Form1,V,BV2,Form2),
			tac__conclusion_acc(Form2,V,Gamma,m,
				[@(all,BV2,Form2)|Deriv2],Deriv5)
		)
	;	Form = [=>,Form1,Form2] ->
		tac__hypothesis(Form1,Form2,V,Gamma,d,Deriv2),
		Deriv5 = [assume(Form1,Deriv2,Form2)|Deriv1]
	;	Form = [<=>,Form1,Form2] ->
		tac__hypothesis(Form1,Form2,V,Gamma,d,Deriv2),
		tac__hypothesis(Form2,Form1,V,Gamma,d,Deriv3),
		tac__add(Opt,[<=>,Form1,Form2],Deriv1,Deriv4),
		Deriv5 = [assume(Form1,Deriv2,Form2),
		  	  assume(Form2,Deriv3,Form1)|Deriv4]
	;	Form = [~,Form1] ->
		tac__hypothesis(Form1,[\/],V,Gamma,d,Deriv2),
		Deriv5 = [contra(Form1,Deriv2)|Deriv1]
	;	Form = [&|FormL] ->
		tac__add(Opt,[&|FormL],Deriv1,Deriv2),
		tac__conclusion_accL(FormL,V,Gamma,Deriv2,Deriv5)
	;	Deriv5 = [Form by gap|Deriv1]
	).

%%d tac__conclusion_accL(fmlL::in,varL::in,fmlL::in,drv::in,drv::out)

tac__conclusion_accL([],_,_,Deriv,Deriv).
tac__conclusion_accL([Form|FormL],VL,Gamma,Deriv1,Deriv3) :-
	tac__conclusion_accL(FormL,VL,Gamma,Deriv1,Deriv2),
	tac__conclusion_acc(Form,VL,Gamma,d,Deriv2,Deriv3).

%%d tac__hypothesis(fml::in,fml::in,varL::in,fmlL::in,gr::in,drv::out)

tac__hypothesis(Form1,Form2,V1,Gamma,Opt,Deriv2) :-
	(	Form1 = @(ex,BV1,Form3) ->
		eq__add_free(Form2,V1,V2),
		(	lst__disjoint(V2,BV1) ->
			BV2 = BV1,
			Form4 = Form3
		;	tac__rename(BV1,Form3,V2,BV2,Form4)
		),
		eq__add_free(Form4,V1,V3),
		tac__hypothesis(Form4,Form2,V3,Gamma,d,Deriv1),
		Deriv2 = [exist(BV2,Form4,Deriv1,Form2)]
	;	Form1 = [\/|FormL],
		\+ (FormL = []) ->
		tac__caseL(FormL,Form2,V1,Gamma,CaseL),
		Deriv2 = [cases(CaseL,Form2)]
	;	Form1 = [=>,Form3,Form4],
		lst__member_con_check(Form3,Gamma) ->
		tac__hypothesis(Form4,Form2,V1,Gamma,Opt,Deriv1),
		Deriv2 = [Form4|Deriv1]
	;	eq__add_free(Form1,V1,V2),
		tac__conclusion(Form2,V2,[Form1|Gamma],Opt,Deriv2)
	).

%%d tac__caseL(fmlL::in,fml::in,varL::in,fmlL::in,casL::out)

tac__caseL([],_,_,_,[]).
tac__caseL([Form1|FormL],Form2,V,Gamma,[case(Form1,Deriv)|CaseL]) :-
	tac__hypothesis(Form1,Form2,V,Gamma,d,Deriv),
	tac__caseL(FormL,Form2,V,Gamma,CaseL).

%%d tac__add(gr::in,fml::in,drv::in,drv::out)

tac__add(m,Form,Deriv,[Form|Deriv]).
tac__add(d,_,Deriv,Deriv).

%%d tac__rename(varL::in,fml::in,varL::in,varL::out,fml::out)

tac__rename(BV1L,Form1,V1L,BV2L,Form2) :-
	eq__max(Form1,0,N1),
	eq__max_varL(V1L,N1,N2),
	eq__apply_extend(BV1L,V1L,[],N2,BV2L,V2L,Sub,N3),
	eq__apply(Form1,V2L,Sub,N3,Form2).

%%d tac__options(grL::in,fml::in,varL::in,fmlL::in,drv::out)

tac__options(Opt,Form,V,Gamma,Deriv) :-
	(	lst__member(debug,Opt) ->			% debug
		pr__debug(V,Gamma,Form,N)
	;	true
	),
		tac__derivable(Form,V,Gamma,Deriv)
	;	lst__member_check(fact,Opt),			% fact
		tac__fact(Form,V,Gamma,Deriv)
	;	lst__member_check(ind,Opt),			% ind
		tac__induction(Form,V,Gamma,Deriv)
	;	lst__member_check(indqf,Opt),			% indqf
		tac__qf_induction(Form,V,Gamma,Deriv)
	;	lst__member_check(unfold,Opt),			% unfold
		tac__unfold(Form,V,Gamma,Deriv)
	;	lst__member_check(case,Opt),			% case
		tac__case_split(Form,V,Gamma,Deriv)
	;	lst__member_check(ex,Opt),			% ex
		tac__existence_elim(Form,V,Gamma,Deriv)
	;	lst__member_check(elim,Opt),			% elim
		tac__definition_expand(Form,V,Gamma,Deriv)
	;	lst__member_check(tot,Opt),			% tot
		tac__totality(Form,V,Gamma,Deriv)
	;	lst__member_check(comp,Opt),			% comp
		tac__comp(Form,V,Gamma,Deriv)
	;	lst__member(auto(N),Opt),
		ai__automatic(N,Form,V,Gamma,Deriv)		% auto
	;	tac__conclusion(Form,V,Gamma,d,Deriv).

%%d tac__derivable(fml::in,varL::in,fmlL::in,drv::out)

tac__derivable(Form,V,Gamma,[Form]) :-
	pr__derivable_once(V,Gamma,Form).

%%d tac__fact(fml::in,varL::in,fmlL::in,drv::out)

tac__fact(Form1,V,Gamma,[Form2 by X]) :-
	tac__db_select(X,Form3),
	Form3 = @(all,BV,[=>,_,Form4]),
	(	Form2 = Form1
	;	Form1 = [=,Term1,Term2],
		Form2 = [=,Term2,Term1]
	),
	pr__match_kernel(Form4,Form2,BV,[],_,_),
	pr__derivable_once(V,[Form3|Gamma],Form2).
tac__fact(Form1,V,Gamma,[Form1 by X]) :-
	tac__db_select(X,Form2),
	pr__derivable_once(V,[Form2|Gamma],Form1).

%%d tac__db_select(gr::out,fml::out)

tac__db_select(X,Form) :-
		db__theorem(Ref,Form),
		X = theorem(Ref)
	;	db__lemma(Ref,Form),
		X = lemma(Ref)
	;	db__corollary(Ref,Form),
		X = corollary(Ref)
	;	db__axiom(Ref,Form),
		X = axiom(Ref).

%%d tac__induction(fml::in,varL::in,fmlL::in,drv::out)

tac__induction(Form,V,Gamma,[induction(Form2L,StepL)]) :-
	ind__formula_to_list(Form,Form1L),
	tac__induction_formulaL(Form1L,Form2L),
	ind__gen_err(tactic(V,Gamma),Form2L,StepL).

%%d tac__induction_formula(fml::in,fml::out)

tac__induction_formula(Form1,Form4) :-
	Form1 = @(all,ZL,[=>,Form2,Form3]),
	Form4 = @(all,XL,[=>,Form5,Form6]),
	(	Form2 = [succeeds,[n(_,_)|TermL]] ->
		Form5 = Form2,
		obj__make_varL(XL,TermL),
		lst__set_minus(ZL,XL,YL),
		cmp__abstraction(all,YL,Form3,Form6)
	;	Form2 = [&,Form5|FormL],	% FormL is not empty
		Form5 = [succeeds,[n(_,_)|TermL]],
		obj__make_varL(XL,TermL),
		lst__set_minus(ZL,XL,YL),
		cmp__conjunction(FormL,Form7),
		cmp__abstraction(all,YL,[=>,Form7,Form3],Form6)
	).

%%d tac__induction_formulaL(fmlL::in,fmlL::out)

tac__induction_formulaL([],[]).
tac__induction_formulaL([Form1|Form1L],[Form2|Form2L]) :-
	tac__induction_formula(Form1,Form2),
	tac__induction_formulaL(Form1L,Form2L).

%%d tac__qf_induction(fml::in,varL::in,fmlL::in,drv::out)

tac__qf_induction(@(all,_,[=>,Form1,Form2]),VL,Gamma,Deriv) :-
	Form1 = [succeeds,[n(_,_)|TermL]],
	obj__make_varL(XL,TermL),
	Form3 = @(all,XL,[=>,Form1,Form2]),
	ind__gen_err(tactic(VL,Gamma),[Form3],StepL),
	Deriv = [induction([Form3],StepL)].
tac__qf_induction(@(all,_,[=>,[&,Form1|Form1L],Form2]),VL,Gamma,Deriv) :-
	Form1 = [succeeds,[n(_,_)|TermL]],
	obj__make_varL(XL,TermL),
	cmp__conjunction(Form1L,Form3),
	Form4 = [=>,Form3,Form2],
	Form5 = @(all,XL,[=>,Form1,Form4]),
	ind__gen_err(tactic(VL,Gamma),[Form5],StepL),
	Deriv = [induction([Form5],StepL)].

%%d tac__unfold(fml::in,varL::in,fmlL::in,drv::out)

tac__unfold(Form1,VL,Gamma,Deriv) :-
	Form1 = [Op,Atom],
	obj__sft_op(Op), 
	obj__atomic_goal(Atom),
	cmp__sft_formula(Op,Atom,Form2),
	tac__conclusion_acc(Form2,VL,Gamma,m,
		[Form1 by completion],Deriv).
tac__unfold(Form1,VL,Gamma,Deriv) :-
	Form1 = [d(Name,N)|_],
	def__pred_formula(Form1,Form2),
	tac__conclusion_acc(Form2,VL,Gamma,d,
		[Form1 by introduction(Name,N)],Deriv).
tac__unfold(Form1,VL,Gamma,Deriv) :-
	Form1 = [=|_],
	def__fun_uniqueness(Name,N,Form1,Form2),
	tac__conclusion_acc(Form2,VL,Gamma,d,
		[Form1 by uniqueness(Name,N)],Deriv).
tac__unfold(Form1,VL,Gamma,Deriv) :-
	Form1 = [terminates,[&,Goal|GoalL]],
	cmp__terminates_goal(Goal,Form2),
	cmp__terminates_goal([&|GoalL],Form3),
	tac__conclusion_accL([Form2,Form3],VL,Gamma,[Form1],Deriv).
tac__unfold(Form1,VL,Gamma,Deriv) :-
	Form1 = [terminates,[&,Goal|GoalL]],
	cmp__terminates_goal(Goal,Form2),
	cmp__succeeds_goal(Goal,Form3),
	cmp__terminates_goal([&|GoalL],Form4),
	Form5 = [=>,Form3,Form4],
	tac__conclusion_accL([Form2,Form5],VL,Gamma,[Form1],Deriv).
tac__unfold(Form1,VL,Gamma,Deriv) :-
	def__fun_existence(Name,N,Form1,Form2),
	tac__conclusion_acc(Form2,VL,Gamma,d,
		[Form1 by existence(Name,N)],Deriv).

%%d tac__case_split(fml::in,varL::in,fmlL::in,drv::out)

tac__case_split(Form,V,Gamma,Deriv) :-
	tac__choose_assumption([\/|FormL],Gamma),
	tac__hypothesis([\/|FormL],Form,V,Gamma,d,Deriv).

%%d tac__existence_elim(fml::in,varL::in,fmlL::in,drv::out)

tac__existence_elim(Form1,V,Gamma,Deriv) :-
	tac__choose_assumption(@(ex,BV,Form2),Gamma),
	tac__hypothesis(@(ex,BV,Form2),Form1,V,Gamma,d,Deriv).

%%d tac__definition_expand(fml::in,varL::in,fmlL::in,drv::out)

tac__definition_expand(Form1,VL,Gamma,Deriv2) :-
	tac__choose_assumption([d(Name,N)|TermL],Gamma),
	def__pred_formula([d(Name,N)|TermL],Form2),
	tac__hypothesis(Form2,Form1,VL,Gamma,m,Deriv1),
	Deriv2 = [Form2 by elimination(Name,N)|Deriv1].

%%d tac__totality(fml::in,varL::in,fmlL::in,drv::out)

tac__totality(Form1,VL,Gamma,Deriv2) :-
	tac__choose_assumption([terminates,Atom],Gamma),
	Atom = [n(_,_)|_],
	Form2 = [\/,[succeeds,Atom],[fails,Atom]],
	tac__hypothesis(Form2,Form1,VL,Gamma,d,Deriv1),
	Deriv2 = [Form2|Deriv1].

%%d tac__comp(fml::in,varL::in,fmlL::in,drv::out)

tac__comp(Form1,VL,Gamma,Deriv2) :-
	tac__choose_assumption([Op,Atom],Gamma),
	obj__sft_op(Op),
	bi__user_defined_atom(Atom),
	cmp__sft_formula(Op,Atom,Form2),
	tac__hypothesis(Form2,Form1,VL,Gamma,m,Deriv1),
	Deriv2 = [[def,[Op,Atom]] by completion|Deriv1].

%%d tac__choose_assumption(fml::out,fmlL::in)

tac__choose_assumption(Form,Gamma) :-
	db__marked_assumption(Form),
	lst__member_con_check(Form,Gamma),
	retract(db__marked_assumption(Form)),
	assert(db__marked_assumption([&])).
tac__choose_assumption(Form,Gamma) :-
	lst__member_con(Form,Gamma).

%%d tac__proof_prt(grL::in,varL::in,fmlL::in,fml::in)

tac__proof_prt(Opt,VL,Gamma,Form) :-
	tac__options(Opt,Form,VL,Gamma,Deriv),
	once((
		i2e__derivation(Deriv,Y),
		(	lst__member(l(Left),Opt) -> 
			true 
		;	 Left = 0
		),
		(	lst__member(r(Right),Opt) -> 
			true 
		; 	prt__text_width(Right)
		),
		io__tell_user,
		nl, write('======'), nl,
		prt__write(Y,Left,Right), nl,
		write('======'), nl
	)),
	(	lst__member_check(more,Opt) ->
		write('more (y/n)? '),
		get(Char),
		(	Char = 110 ->		% answer: n
			true
		;	fail			% answer: y
		)
	; 	true).

%%d tac__by(gr::in,grL::in)

tac__by(X,Opt) :-
	e2i__formula(X,Form),
	tac__proof_prt(Opt,[],[],Form).

%%d tac__new(gr::in,varL::in,fmlL::in,fml::in)

tac__new(Ref,V1,Gamma1,Form1) :-
	lst__reverse(V1,V2),
	lst__reverse(Gamma1,Gamma2),
	cmp__conjunction(Gamma2,Form2),
	cmp__implication(Form2,Form1,Form3),
	cmp__abstraction(all,V2,Form3,Form4),
	tac__conclusion(Form4,[],[],d,Deriv),
	i2e__expression(Form4,X),
	i2e__derivation(Deriv,Y),
	prt__fact(lemma,Ref,X,Y).

% tac.pl ends here

%   Author: Robert Staerk <staerk@saul.cis.upenn.edu> 
%  Created: Mon Mar 11 14:46:08 1996 
% Updated: Thu Feb 11 10:39:07 1999
% Filename: ai.pl 
% Abstract: Artificial intelligence? --- Backward and forward search.

%%d ai__automatic(int::in,fml::in,varL::in,fmlL::in,drv::out)

ai__automatic(I,Form,V,Gamma,Deriv) :-
	ai__integer_numeral(I,K),
	ai__backward_acc(Form,V,Gamma,d,[],Deriv,K).

%%d ai__backward_acc(fml::in,varL::in,fmlL::in,gr::in,drv::in,drv::out,gr::in)

ai__backward_acc(Form,V,Gamma,Opt,Deriv1,Deriv2,_) :-
	pr__derivable_once(V,Gamma,Form),
	tac__add(Opt,Form,Deriv1,Deriv2).
ai__backward_acc(Form1,V,Gamma1,Opt,Deriv1,Deriv2,s(K)) :-
	lst__delete_con([=>,Form2,Form1],Gamma1,Gamma2),
	ai__backward_acc(Form2,V,Gamma2,Opt,Deriv1,Deriv2,K).
ai__backward_acc(Form1,_,Gamma,_,Deriv1,Deriv2,_) :-
	tac__db_select(X,@(all,BV,Form2)),
	pr__match_kernel(Form2,Form1,BV,[],Sub,FormL),
	pr__match_args(FormL,Gamma,Sub),
	Deriv2 = [Form1 by X|Deriv1].
ai__backward_acc(Form1,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form1 = @(all,BV1,Form2),
	tac__add(Opt,Form1,Deriv1,Deriv2),
	(	lst__disjoint(BV1,V) ->
		ai__backward_acc(Form2,V,Gamma,m,Deriv2,Deriv3,K)
	;	tac__rename(BV1,Form2,V,BV2,Form3),
		Form4 = @(all,BV2,Form3),
		ai__backward_acc(Form3,V,Gamma,m,[Form4|Deriv2],Deriv3,K)
	).
ai__backward_acc([=>,Form1,Form2],V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	ai__backward_add(Form1,Form2,V,Gamma,d,Deriv2,K),
	Deriv3 = [assume(Form1,Deriv2,Form2)|Deriv1].
ai__backward_acc(Form1,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form1 = [<=>,Form2,Form3],
	tac__add(Opt,Form1,Deriv1,Deriv2),
	FormL = [[=>,Form2,Form3],[=>,Form3,Form2]],
	ai__backward_accL(FormL,V,Gamma,Opt,Deriv2,Deriv3,K).
ai__backward_acc([~,Form],V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	ai__backward_add(Form,[\/],V,Gamma,d,Deriv2,K),
	Deriv3 = [contra(Form,Deriv2)|Deriv1].
ai__backward_acc([&|FormL],V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	tac__add(Opt,[&|FormL],Deriv1,Deriv2),
	ai__backward_accL(FormL,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form,_,Gamma,_,Deriv1,Deriv2,_) :-
	Form = [succeeds,[Tag|TermL]],
	cmp__match_clause(Tag,TermL,Gamma),
	Deriv2 = [Form by sld|Deriv1].
ai__backward_acc([Op,Atom],V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	obj__sft_op(Op),
	bi__user_defined_atom(Atom),
	cmp__sft_formula(Op,Atom,Form),
	Deriv2 = [[Op,Atom] by completion|Deriv1],
	ai__backward_acc(Form,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc([Op,Atom],V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	obj__sft_op(Op),
	bi__builtin_atom(Atom),
	bi__typed(Atom),
	bi__eval(Atom,Goal),
	cmp__op_goal(Op,Goal,Form),
	Deriv2 = [[Op,Atom] by builtin|Deriv1],
	ai__backward_acc(Form,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form1,V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	Form1 = [d(Name,N)|_],
	def__pred_formula(Form1,Form2),
	Deriv2 = [Form1 by introduction(Name,N)|Deriv1],
	ai__backward_acc(Form2,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form1,V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	Form1 = [=|_],
	def__fun_uniqueness(Name,N,Form1,Form2),
	Deriv2 = [Form1 by uniqueness(Name,N)|Deriv1],
	ai__backward_acc(Form2,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form1,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form1 = [terminates,[&,Goal|GoalL]],
	cmp__terminates_goal(Goal,Form2),
	cmp__terminates_goal([&|GoalL],Form3),
	tac__add(Opt,Form1,Deriv1,Deriv2),
	FormL = [Form2,Form3],
	ai__backward_accL(FormL,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form1,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form1 = [terminates,[&,Goal|GoalL]],
	cmp__terminates_goal(Goal,Form2),
	cmp__succeeds_goal(Goal,Form3),	
	cmp__terminates_goal([&|GoalL],Form4),
	tac__add(Opt,Form1,Deriv1,Deriv2),
	FormL = [Form2,[=>,Form3,Form4]],
	ai__backward_accL(FormL,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form = [gr,[n(_,_)|TermL]],
	obj__pure_termL(TermL),
	tac__add(Opt,Form,Deriv1,Deriv2),
	eq__add_free_boundL(TermL,[],[],XL),
	cmp__map_gr(XL,[],FormL),
	ai__backward_accL(FormL,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	Form = [gr,$(X)],
	tac__add(Opt,Form,Deriv1,Deriv2),
	(	lst__member_con([=,$(X),Term],Gamma)
	;	lst__member_con([=,Term,$(X)],Gamma)
	),
	ai__backward_acc([gr,Term],V,Gamma,m,Deriv2,Deriv3,K).
ai__backward_acc(Form,V,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	ai__choose_hypothesis(V,Gamma,Opt,Form,Deriv2,K),
	lst__append(Deriv2,Deriv1,Deriv3).
ai__backward_acc(Form1,V,Gamma,_,Deriv1,Deriv3,s(K)) :-
	ai__names_expressionL(Gamma,[],L1),
	tac__db_select(X,@(all,BV,[=>,Form4,Form3])),
	pr__match_conclusion(Form3,Form1,BV,Sub),
	ai__names_expression(Form4,[],L2),
	lst__subset(L2,L1),
	Deriv2 = [Form1 by X|Deriv1],
	eq__apply_plain(Form4,Sub,Form5),
	ai__backward_acc(Form5,V,Gamma,d,Deriv2,Deriv3,K).
ai__backward_acc(Form1,V1,Gamma,Opt,Deriv1,Deriv3,s(K)) :-
	tac__db_select(X,@(all,BV,[=>,Form2,Form3])),
	pr__match_assumption(Gamma,Form2,BV,[],Sub),
	eq__apply_plain(Form3,Sub,Form4),
	\+ lst__member_con(Form4,Gamma),
	eq__add_free(Form4,V1,V2),
	ai__backward_acc(Form1,V2,[Form4|Gamma],Opt,Deriv1,Deriv2,K),
	Deriv3 = [Form4 by X|Deriv2].

%%d ai__backward_accL(fmlL::in,varL::in,fmlL::in,gr::in,drv::in,drv::out,gr::in)

ai__backward_accL([],_,_,_,Deriv,Deriv,_).
ai__backward_accL([Form|FormL],V,Gamma,Opt,Deriv1,Deriv3,K) :-
	ai__backward_accL(FormL,V,Gamma,Opt,Deriv1,Deriv2,K),
	ai__backward_acc(Form,V,Gamma,Opt,Deriv2,Deriv3,K).


%%d ai__choose_hypothesis(varL::in,fmlL::in,gr::in,fml::in,drv::out,gr::in)

ai__choose_hypothesis(V,Gamma1,Opt,Form1,Deriv,K) :-
	lst__delete_con(Form2,Gamma1,Gamma2),
	ai__forward(Form2,Form1,V,Gamma2,Opt,Deriv,K).

%%d ai__forward(fml::in,varL::in,fmlL::in,gr::in,fml::in,drv::out,gr::in)

ai__forward(@(ex,BV1,Form1),Form2,V1,Gamma,_,Deriv2,s(K)) :-
	eq__add_free(Form2,V1,V2),
	(	lst__disjoint(V2,BV1) ->
		BV2 = BV1,
		Form3 = Form1
	;	tac__rename(BV1,Form1,V2,BV2,Form3)
	),
	eq__add_free(Form3,V1,V3),
	ai__backward_add(Form3,Form2,V3,Gamma,d,Deriv1,K),
	Deriv2 = [exist(BV2,Form3,Deriv1,Form2)].
ai__forward([\/|FormL],Form,V,Gamma,_,Deriv,s(K)) :-
	\+ (FormL = []),
	ai__cases(FormL,Form,V,Gamma,CaseL,K),
	Deriv = [cases(CaseL,Form)].
ai__forward([d(Name,N)|TermL],Form1,V,Gamma,Opt,Deriv2,s(K)) :-
	def__pred_formula([d(Name,N)|TermL],Form2),
	ai__backward_add(Form2,Form1,V,Gamma,Opt,Deriv1,K),
	Deriv2 = [Form2 by elimination(Name,N)|Deriv1].
ai__forward([succeeds,Atom],Form1,V,Gamma,Opt,Deriv2,s(K)) :-
	bi__user_defined_atom(Atom),
	cmp__sft_formula(succeeds,Atom,Form2),
	ai__backward_add(Form2,Form1,V,Gamma,Opt,Deriv1,K),
	Deriv2 = [[def,[succeeds,Atom]] by completion|Deriv1].
ai__forward([succeeds,Atom],Form1,V,Gamma,Opt,Deriv2,s(K)) :-
	bi__builtin_atom(Atom),
	bi__typed(Atom),
	bi__eval(Atom,Goal),
	cmp__op_goal(succeeds,Goal,Form2),
	ai__backward_add(Form2,Form1,V,Gamma,Opt,Deriv1,K),
	Deriv2 = [[def,[succeeds,Atom]] by builtin|Deriv1].
ai__forward([terminates,Atom],Form1,V,Gamma,_,Deriv,s(K)) :-
	obj__atomic_goal(Atom),
	FormL = [[succeeds,Atom],[fails,Atom]],
	ai__cases(FormL,Form1,V,Gamma,CaseL,K),
	Deriv = [cases(CaseL,Form1)].
ai__forward([=>,Form1,Form2],Form3,V,Gamma,Opt,Deriv2,s(K)) :-
	pr__derivable_once(V,Gamma,Form1),
	ai__backward_add(Form2,Form3,V,Gamma,Opt,Deriv1,K),
	Deriv2 = [Form2|Deriv1].
ai__forward(@(all,BV,[=>,Form1,Form2]),Form3,V,Gamma,Opt,Deriv2,s(K)) :-
	pr__match_assumption(Gamma,Form1,BV,[],Sub),
	eq__apply_plain(Form2,Sub,Form4),
	ai__backward_add(Form4,Form3,V,Gamma,Opt,Deriv1,K),
	Deriv2 = [Form4|Deriv1].

%%d ai__cases(fmlL::in,fml::in,varL::in,fmlL::in,casL::out,gr::int).

ai__cases([],_,_,_,[],_).
ai__cases([Form1|FormL],Form2,V,Gamma,[case(Form1,Deriv)|CaseL],K) :-
	ai__backward_add(Form1,Form2,V,Gamma,d,Deriv,K),
	ai__cases(FormL,Form2,V,Gamma,CaseL,K).

%%d ai__backward_add(fml::in,fml::in,varL::in,fmlL::in,drv::out,gr::in)

ai__backward_add(Form1,Form2,V1,Gamma,Opt,Deriv,K) :-
	\+ lst__member_con(Form1,Gamma),
	eq__add_free(Form1,V1,V2),
	ai__backward_acc(Form2,V2,[Form1|Gamma],Opt,[],Deriv,K).

%%d ai__integer_numeral(int::in,gr::out)

ai__integer_numeral(I1,K2) :-
	(	I1 > 0 ->
		I2 is I1 - 1,
		K2 = s(K1),
		ai__integer_numeral(I2,K1)
	;	K2 = 0
	).


%%d ai__names_expression(expr::in,grL::in,grL::out)

ai__names_expression($(_),L,L).
ai__names_expression([Tag|ExprL],L1,L3) :-
	(	ai__name(Tag) ->
		lst__add_element(Tag,L1,L2),
		ai__names_expressionL(ExprL,L2,L3)
	;	ai__names_expressionL(ExprL,L1,L3)
	).
ai__names_expression(@(_,_,Expr),L1,L2) :-
	ai__names_expression(Expr,L1,L2).

%%d ai__names_expressionL(exprL::in,grL::in,grL::out)

ai__names_expressionL([],L,L).
ai__names_expressionL([Expr|ExprL],L1,L3) :-
	ai__names_expression(Expr,L1,L2),
	ai__names_expression(ExprL,L2,L3).


%%d ai__name(gr::in)

ai__name(n(_,_)).
ai__name(f(_,_)).
ai__name(d(_,_)).

% ai.pl ends here

/*   Author: Robert Staerk <staerk@saul.cis.upenn.edu> */
/*  Created: Fri Jan 19 12:01:32 1996 */
/* Updated: Mon Feb 15 16:10:13 1999 */
/* Filename: ctl.pl */
/* Abstract: Flags, warnings, error messages, initialization, theorems,
   lemmas, corollaries, axioms. */

%%c
%%c The proof files (.pr) contain the following commands:
%%c
%%c :- initialize.
%%c
%%c :- needs_thm(Path).
%%c :- needs_gr(Path).
%%c
%%c :- thm_file(Path).
%%c :- tex_file(Path).
%%c :- prt_file(Path).
%%c :- ma_file(Path).
%%c
%%c :- axiom(Ref,EForm).
%%c
%%c :- theorem(Ref,EForm,EDeriv).
%%c :- lemma(Ref,EForm,EDeriv).
%%c :- corollary(Ref,EForm,EDeriv).
%%c
%%c :- definition_pred(<r>,n,
%%c  all [<x1>,...,<xn>]: <r>(?<x1>,...,?<xn>) <=> Eform).
%%c
%%c :- definition_fun(<f>,n,
%%c  all [<x1>,...,<xn>,<y>]: EForm1 =>
%%c   (<f>(?<x1>,...,?<xn>) = ?<y> <=> EForm2),
%%c  existence by <Thm>(Ref1),
%%c  uniqueness by <Thm>(Ref2)
%%c ).
%%c
%%c :- bye(File).
%%c
%%c The theorem files (.thm) contain the following commands:
%%c
%%c :- assert_fact(theorem,Ref,Form).
%%c :- assert_fact(lemma,Ref,Form).
%%c :- assert_fact(corollary,Ref,Form).
%%c :- assert_fact(axiom,Ref,Form).
%%c
%%c :- assert_pred(Atom,Form).
%%c :- assert_fun(Term,Form,Form).
%%c
%%c The program files (.gr) contain the following commands:
%%c
%%c :- assert_clauses(Pred,ClauseL)
%%c
%%c The internal database contains the following facts:
%%c
%%c db__theorem(Ref,Form).
%%c db__lemma(Ref,Form).
%%c db__corollary(Ref,Form).
%%c db__axiom(Ref,Form).
%%c db__pred(Atom,Form).
%%c db__fun(Term,Form,Form).
%%c db__clauses(Pred,ClauseL).
%%c


%% Flags

%%d db__flag(gr::out)

:- dynamic(db__flag/1).

db__flag('DUMMY').
db__flag(check_everything).			% cautious
db__flag(fail_on_error).			% fail in case of an error
db__flag(unique_names).				% unique names in theorems
db__flag(assert_facts).				% assert facts
db__flag(report_because).			% report "because"
db__flag(tex_output).				% create TeX output
db__flag(thm_output).				% write the .thm files

%db__flag(print_names).				% print names of theorems
%db__flag(debug).				% debug mode
%db__flag(write_dependencies).			% write dependencies on .thm

% The following flags are set by the system. Do not change them.

%db__flag(global_error).			% there is a global error

%%d ctl__set_flag(gr::in)

ctl__set_flag(Flag) :-
	(	db__flag(Flag) -> 
		true
	; 	assert(db__flag(Flag))
	).

%%d ctl__unset_flag(gr::in)

ctl__unset_flag(Flag) :-
	(	db__flag(Flag) -> 
		retract(db__flag(Flag))
	; 	true
	).

%%d ctl__plain
%%d ctl__pedantic
%%d ctl__draft
%%d ctl__show

ctl__pedantic :-
	ctl__set_flag(check_everything),
	ctl__set_flag(fail_on_error),
	ctl__set_flag(unique_names),
	ctl__set_flag(assert_facts),
	ctl__set_flag(report_because),
	ctl__set_flag(tex_output),
	ctl__set_flag(thm_output),
	ctl__unset_flag(print_names).

ctl__plain :-
	ctl__unset_flag(check_everything),
	ctl__set_flag(fail_on_error),
	ctl__unset_flag(unique_names),
	ctl__set_flag(assert_facts),
	ctl__unset_flag(report_because),
	ctl__unset_flag(tex_output),
	ctl__set_flag(thm_output),
	ctl__unset_flag(print_names).

ctl__draft :-
	ctl__set_flag(check_everything),
	ctl__unset_flag(fail_on_error),
	ctl__unset_flag(unique_names),
	ctl__unset_flag(assert_facts),
	ctl__unset_flag(report_because),
	ctl__unset_flag(tex_output),
	ctl__unset_flag(thm_output),
	ctl__unset_flag(print_names).

ctl__show :-
	ctl__unset_flag(check_everything),
	ctl__unset_flag(fail_on_error),
	ctl__set_flag(unique_names),
	ctl__set_flag(assert_facts),
	ctl__unset_flag(report_because),
	ctl__unset_flag(tex_output),
	ctl__unset_flag(thm_output),
	ctl__set_flag(print_names).

%%d ctl__warning(grL::in)

ctl__warning(L) :-
	io__tell_user,
	write('! LPTP-Warning:'),
	ctl__write_phrase(L).

%%d ctl__message(grL::in)

ctl__message(L) :-
	io__tell_user,
	write('! LPTP-Message:'),
	ctl__write_phrase(L).

%%d ctl__syntax(grL::in)

ctl__syntax(L) :-
	ctl__set_flag(global_error),
	io__tell_user,
	write('! LPTP-Syntax:'),
	ctl__write_phrase(L).

%%d ctl__error(grL::in)

ctl__error(L) :-
	(	db__flag(global_error) ->
		true
	;	ctl__set_flag(global_error),
		io__tell_user,
		write('! LPTP-Error:'),
		ctl__write_phrase(L)	
	),
	\+ db__flag(fail_on_error).

%%d ctl__write_phrase(grL::in)

ctl__write_phrase([]) :-
	write('.'), nl.
ctl__write_phrase([X|L]) :-
	write(' '), 
	(	var(X) ->
		write(X)
	;	X = p(Y) ->
		nl, prt__write(Y)
	;	X = q(Y) ->
		write('`'), write(Y), write('\'')
	;	X = e(Expr) ->
		i2e__expression(Expr,Y), nl, prt__write(Y)
	;	write(X)
	),
	ctl__write_phrase(L).

%% Theorems, lemmas, corollaries, axioms

%%d db__theorem(gr::out,fml::out)
%%d db__lemma(gr::out,fml::out)
%%d db__corollary(gr::out,fml::out)
%%d db__axiom(gr::out,fml::out)

:- dynamic(db__theorem/2).
:- dynamic(db__lemma/2).
:- dynamic(db__corollary/2).
:- dynamic(db__axiom/2).

db__theorem('DUMMY',[&]).
db__lemma('DUMMY',[&]).
db__corollary('DUMMY',[&]).
db__axiom('DUMMY',[&]).

%%d theorem(gr::in,gr::in,gr::in)
%%d lemma(gr::in,gr::in,gr::in)
%%d corollary(gr::in,gr::in,gr::in)

theorem(X,Y,Z)   :- ctl__module(theorem,X,Y,Z).
lemma(X,Y,Z)     :- ctl__module(lemma,X,Y,Z).
corollary(X,Y,Z) :- ctl__module(corollary,X,Y,Z).

%%d axiom(gr::in,gr::in)

axiom(Ref,X) :-
	ctl__unset_flag(global_error),
	ctl__reference_err(Ref),
	e2i__formula(X,Form),
	ctl__assert_and_print_fact(axiom,Ref,Form,[],X,'DUMMY').

%%d ctl__exists_fact(gr::in,gr::in)

ctl__exists_fact(theorem,Ref)   :- db__theorem(Ref,_).
ctl__exists_fact(lemma,Ref)     :- db__lemma(Ref,_).
ctl__exists_fact(corollary,Ref) :- db__corollary(Ref,_).
ctl__exists_fact(axiom,Ref)     :- db__axiom(Ref,_).

%%d ctl__do_assert(gr::in,gr::in,fml::in)

ctl__do_assert(theorem,Ref,Form)   :- assert(db__theorem(Ref,Form)).
ctl__do_assert(lemma,Ref,Form)     :- assert(db__lemma(Ref,Form)).
ctl__do_assert(corollary,Ref,Form) :- assert(db__corollary(Ref,Form)).
ctl__do_assert(axiom,Ref,Form)     :- assert(db__axiom(Ref,Form)).

%%d ctl__module(gr::in,gr::in,gr::in,gr::in)

ctl__module(Kind,Ref,X,Y) :-
	ctl__unset_flag(global_error),
	ctl__reference_err(Ref),
	(	db__flag(print_names) ->
		io__tell_user,
		write(Kind), write(' '), write(Ref), nl
	;	true
	),
	e2i__formula(X,Form),
	e2i__derivation(Y,Deriv),
	(	db__flag(global_error) ->
		Fact =.. [Kind,Ref],
		ctl__warning([Fact,is,not,checked])
	;	pr__proof(Deriv,[],[],Gamma),
		pr__derivable_err([],Gamma,Form),
		ctl__assert_and_print_fact(Kind,Ref,Form,Deriv,X,Y),
		dep__write_fact(Kind,Ref,Deriv)
	).

%%d ctl__assert_and_print_fact(gr::in,gr::in,fml::in,drv::in,gr::in,gr::in)

ctl__assert_and_print_fact(Kind,Ref,Form,Deriv,X,Y) :-
	(	db__flag(global__error) ->
		Fact =.. [Kind,Ref],
		ctl__warning([Fact,is,not,asserted])
	;	ctl__assert_new_fact(Kind,Ref,Form),
		eq__add_free(Form,[],VarL),
		(	VarL = [] ->
			true
		;	Fact = [Kind,Ref],
			ctl__warning([free,variables,VarL,in,Fact])
		),
		ctl__write_tex(Kind,Ref,Form,Deriv),
		ctl__write_thm(Kind,Ref,Form),
		(	db__is_open(prt) ->
			io__tell(prt),
			prt__fact(Kind,Ref,X,Y)
		;	true
		)
	).

%% ctl__write_tex(gr::in,gr::in,fml::in,drv::in)

ctl__write_tex(Kind,Ref,Form,Deriv) :-
	(	db__is_open(tex), 
		db__flag(tex_output) ->
		io__tell(tex),
		'TeX__write_fact'(Kind,Ref,Form,Deriv)
	; 	true
	).

%%d ctl__write_thm(gr::in,gr::in,fml::in)

ctl__write_thm(Kind,Ref,Form) :-
	(	db__is_open(thm), 
		\+ ctl__tmp_name(Ref),
		db__flag(thm_output) -> 
		io__tell(thm),
		write(':- '),
		writeq(assert_fact(Kind,Ref,Form)),
		write('.'), nl
	; 	true
	).

%%d ctl__tmp_name(gr::in)

ctl__tmp_name(tmp:_).

%%d assert_fact(gr::in,gr::in,fml::in)

assert_fact(Kind,Ref,Form) :- 
	(	db__flag(check_everything) ->
		(	obj__formula(Form) ->
			true
		;	Fact =.. [Kind,Ref],
			ctl__warning([corrupted,formula,in,Kind,Fact])
		)
	;	true),
	ctl__assert_new_fact(Kind,Ref,Form).

%%d ctl__assert_new_fact(gr::in,gr::in,fml::in)

ctl__assert_new_fact(Kind,Ref,Form) :-
	(	db__flag(unique_names),
		ctl__exists_fact(Kind,Ref) ->
		Fact =.. [Kind,Ref],
		ctl__warning([Fact,already,exists])
	;	db__flag(assert_facts) ->
		ctl__do_assert(Kind,Ref,Form)
	;	true
	).

%%d initialize

initialize :-
	abolish(db__theorem/2),
	abolish(db__lemma/2),
	abolish(db__corollary/2),
	abolish(db__axiom/2),
	abolish(db__pred/2),
	abolish(db__fun/3),
	abolish(db__clauses/2),
	abolish(db__depends/2),
	assert(db__theorem('DUMMY',[&])),
	assert(db__lemma('DUMMY',[&])),
	assert(db__corollary('DUMMY',[&])),
	assert(db__axiom('DUMMY',[&])),
	assert(db__pred([d('DUMMY',0)],[&])),
	assert(db__fun([f('DUMMY',0)],[&],[&])),
	assert(db__clauses(n(fail,0),[])),
	assert(db__depends('DUMMY',[])).

%% Print all theorems, lemmas, corollaries and axioms

%%d ctl__print_facts(gr::in)

ctl__print_facts(Ref) :-
	io__tell_user,
	ctl__unset_flag(global_error),
	ctl__reference_err(Ref),
	\+ ctl__print_all_facts(Ref).

%%d ctl__print_all_factss(gr::in)

ctl__print_all_facts(Ref1) :-
	db__theorem(Ref2,Form),
	ctl__print_fact(theorem,Ref1,Ref2,Form),
	fail.
ctl__print_all_facts(Ref1) :-
	db__lemma(Ref2,Form),
	ctl__print_fact(lemma,Ref1,Ref2,Form),
	fail.
ctl__print_all_facts(Ref1) :-
	db__corollary(Ref2,Form),
	ctl__print_fact(corollary,Ref1,Ref2,Form),
	fail.
ctl__print_all_facts(Ref1) :-
	db__axiom(Ref2,Form),
	ctl__print_fact(axiom,Ref1,Ref2,Form),
	fail.

%%d ctl__print_fact(gr::in,gr::in,gr::in,fml::in)

ctl__print_fact(Kind,Ref1,Ref2,Form) :-
	ctl__reference_subset(Ref1,Ref2),
	nl, write(Kind), 
	write('('),
	write(Ref2),
	write(')'), nl,
	i2e__expression(Form,Y),
	prt__write(Y),
	write('.'), nl.

%%d ctl__prt_definition(gr::in)

ctl__prt_definition(X1) :-
	io__tell_user,
	ctl__unset_flag(global_error),
	e2i__formula(X1,Form1),
	e2i__check_sft_atom(Form1),
	(	Form1 = [Op,Atom], 
		obj__sft_op(Op),
		bi__user_defined_atom(Atom) ->
		cmp__sft_formula(Op,Atom,Form2)
	;	Form1 = [Op,Atom], 
		obj__sft_op(Op),
		bi__typed(Atom) ->
		bi__eval(Atom,Goal),
		cmp__op_goal(Op,Goal,Form2)
	;	Form1 = [d(_,_)|_] ->
		def__pred_formula(Form1,Form2)
	),
	i2e__expression(Form2,X2),
	nl, prt__write(X2), nl.

%%d ctl__reference_err(gr::in)

ctl__reference_err(Ref) :-
	(	ctl__reference(Ref) ->
		true
	;	ctl__syntax([q(Ref),is,not,a,correct,reference])
	).
%%d ctl__reference(gr::in)

ctl__reference(X) :-
	(	var(X) ->
		ctl__syntax([prolog,variable,in,reference])
	;	X = (Y:Z) ->
		atomic(Y),
		ctl__reference(Z)
	;	atomic(X)
	).

%%d ctl__reference_member(gr::out,gr:;in)
%%d ctl__reference_member(gr::in,gr:;in)

ctl__reference_member(Ref,Ref) :-
	atomic(Ref).
ctl__reference_member(Ref,Ref:_).
ctl__reference_member(Ref1,_:Ref2) :-
	ctl__reference_member(Ref1,Ref2).

%%d ctl__reference_subset(gr::in,gr::in)

ctl__reference_subset(Ref1,Ref2) :-
	\+ ctl__reference_subset_not(Ref1,Ref2).

%%d ctl__reference_subset_not(gr::in,gr::in)

ctl__reference_subset_not(Ref1,Ref2) :-
	ctl__reference_member(Ref,Ref1),
	\+ ctl__reference_member(Ref,Ref2).

%%d db__marked_assumption(gr::out)

:- dynamic(db__marked_assumption/1).

db__marked_assumption([&]).

%%d ctl__assert_marked_assumption(gr::in)

ctl__assert_marked_assumption(X) :-
	e2i__formula(X,Form),
	retract(db__marked_assumption(_)),
	assert(db__marked_assumption(Form)),
	io__tell_user,
	nl,
	ctl__message([X,is,marked]).

%%d db__depends(gr::out,grL::out)

:- dynamic(db__depends/2).

db__depends('DUMMY',[]).

% ctl.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:55:00 1994 */
/* Updated: Thu Jul 22 10:20:54 1999 */
/* Filename: prt.pl */
/* Abstract: Pretty print (pp). */

%%d prt__indent(int::out)

prt__indent(1).

%%d prt__text_width(int::out)

prt__text_width(76).
%prt__text_width(60).

%%d prt__no_break(gr::in)

prt__no_break(=).
prt__no_break(all).
prt__no_break(ex).
prt__no_break(?).
prt__no_break(succeeds).
prt__no_break(fails).
prt__no_break(terminates).
prt__no_break(:).
prt__no_break(~).
prt__no_break(by).
prt__no_break(<>).
prt__no_break(#<).
prt__no_break(#=<).

%%d prt__prefix_nolayout(gr::in)

prt__prefix_nolayout(?).

%%d prt__indent_infix(gr::in)

prt__indent_infix(:).
prt__indent_infix(by).
prt__indent_infix(=>).

%%d prt__break_args(gr::in)

prt__break_args(assume).
prt__break_args(contra).
prt__break_args(cases).
prt__break_args(cases).
prt__break_args(case).
prt__break_args(indirect).
prt__break_args(exist).
prt__break_args(induction).
prt__break_args(step).
prt__break_args(theorem).
prt__break_args(lemma).
prt__break_args(corollary).

%%d prt__write(gr::in,int::in)

prt__write(X) :-
	prt__text_width(Right),
	prt__write(X,0,Right).

%%d prt__write(gr::in,int::in,int::in)

prt__write(X,Left) :- 
	prt__text_width(Right),
	prt__write(X,Left,Right).

%%d prt__write(gr::in,int::in,int::in)

prt__write(X,Left,Right) :-
	(	prt__term_tree(X,Tree,_) ->
		tab(Left),
		Pos is Left + 1,
		prt__write_tree(Tree,Left,Pos,Right,_)
	;	ctl__error([pretty,print,cannot,print,X])
	).

% %%d prt__tree(gr::in)
% 
% prt__tree(node(Label,N)) :-			% N is the size of the linear
% 	integer(N),				% formatted version of Label
% 	prt__label(Label).
% 
% %%d prt__label(gr::in)
% 
% prt__label(constant(Name)) :-
% 	atomic(Name).
% prt__label(structure(Name,N,TreeL)) :-
% 	atomic(Name),
% 	integer(N),				% N is the length of Name
% 	prt__treeL(TreeL).
% 
% % [nobreak(Tree1,','),nobreak(Tree2,','),nobreak(Tree3,')')]
% % [break(Tree1,','),break(Tree2,','),nobreak(Tree3,')')]
% 
% prt__label(list(TreeL)) :-
% 	prt__treeL(TreeL).
% 
% % [nobreak(T1,','),nobreak(T2,'|'),nobreak(T3,']')]
% 
% prt__label(parenthesis(Tree)) :-
% 	prt__tree(Tree).
% prt__label(infix(Name,N,Tree1,Tree2)) :-
% 	atomic(Name),
% 	integer(N),				% N is the length of Name
% 	prt__tree(Tree1),
% 	prt__tree(Tree2).
% prt__label(prefix(Name,N,Tree)) :-
% 	atomic(Name),
% 	integer(N),				% N is the length of Name
% 	prt__tree(Tree).
% prt__label(postfix(Name,N,Tree)) :-
% 	atomic(Name),
% 	integer(N),				% N is the length of Name
% 	prt__tree(Tree).
% 
% %%d prt__treeL(grL::in)
% 
% prt__treeL([]).
% prt__treeL([X|TreeL]) :-
% 	prt__pair(X),
% 	prt__treeL(TreeL).
% 
% %%d prt__pair(gr::in)
% 
% prt__pair(break(Tree,Sep)) :-
% 	prt__tree(Tree),
% 	atomic(Sep).				% Sep in {',','|',']',')'}
% prt__pair(nobreak(Tree,Sep)) :-
% 	prt__tree(Tree),
% 	atomic(Sep).				% Sep in {',','|',']',')'}

%%d prt__term_tree(gr::in,tree::in,int::in)

prt__term_tree(X,Tree,N) :-
	prt__term_tree(X,1200,Tree,N).

%%d prt__term_tree(gr::in,int::in,tree::in,int::in)

prt__term_tree(X,Prec,Tree2,N3) :-
	(	var(X) ->			% logical variables
		fail
	;	atomic(X) ->			% atoms
		atomic_length(X,N3),
		Tree2 = node(constant(X),N3)
	;	X = [Y|YL] ->			% non-nil lists
		prt__term_tree(Y,999,Tree,N1),
		prt__list_tree(YL,Tree,N1,TreeL,N2),
		N3 is N2 + 1,			% 1 for '['
		Tree2 = node(list(TreeL),N3)
	;	X =.. XL,			% general case ==> univ
		prt__termL_tree(XL,Prec,Tree2,N3)
	).

%%d prt__termL_tree(grL::in,int::in,tree::in,int::in)

prt__termL_tree(L,Prec1,Tree4,N5) :-
	(	L = [Name,X1,X2],
		prt__infix(Name,Prec2,Prec3,Prec4) ->
		atomic_length(Name,N1),
		prt__term_op_tree(X1,Prec3,Tree1,N2),
		prt__term_op_tree(X2,Prec4,Tree2,N3),
		N4 is N1 + 2 + N2 + N3,			% 2 around operator
		Tree3 = node(infix(Name,N1,Tree1,Tree2),N4),
		prt__parenthesize(Prec1,Prec2,Tree3,N4,Tree4,N5)
	;	L = [Name,X1],
		prt__prefix(Name,Prec2,Prec3) ->
		atomic_length(Name,N1),
		prt__term_op_tree(X1,Prec3,Tree1,N2),
		(	prt__prefix_nolayout(Name) ->
			N3 is N1 + N2
		;	N3 is N1 + 1 + N2		% 1 after operator
		),
		Tree2 = node(prefix(Name,N1,Tree1),N3),
		prt__parenthesize(Prec1,Prec2,Tree2,N3,Tree4,N5)
	;	L = [Name,X1],
		prt__postfix(Name,Prec2,Prec3) ->
		atomic_length(Name,N1),
		prt__term_op_tree(X1,Prec3,Tree1,N2),
		N3 is N1 + 1 + N2,			% 1 before operator
		Tree2 = node(postfix(Name,N1,Tree1),N3),
		prt__parenthesize(Prec1,Prec2,Tree2,N3,Tree4,N5)
	;	L = [Name|XL],
		atomic_length(Name,N1),
		(	prt__break_args(Name) ->
			prt__break_args(XL,TreeL,N2)
		;	prt__nobreak_args(XL,TreeL,N2)
		),
		N5 is N1 + N2 + 1,			% 1 for '('
		Tree4 = node(structure(Name,N1,TreeL),N5)
	).

%%d prt__operator(gr::in)

prt__operator(Name) :- current_op(_,_,Name).

%%d prt__term_op_tree(gr::in,int::in,tree::out,int::out)

prt__term_op_tree(X,Prec,Tree,N) :-
	(	atom(X),
		\+ X = (','),
		current_op(_,_,X) ->
		atomic_length(X,N1),
		Tree = node(parenthesis(node(constant(X),N1)),N),
		N is N1 + 2
	;	prt__term_tree(X,Prec,Tree,N)
	).

%%d prt__nobreak_args(grL::in,int::in,grL::out,int::out)

prt__nobreak_args([X],[nobreak(Tree,')')],N2) :-
	prt__term_tree(X,999,Tree,N1),
	N2 is N1 + 1.
prt__nobreak_args([X1,X2|XL],[nobreak(Tree,',')|TreeL],N3) :-
	prt__term_tree(X1,999,Tree,N1),
	prt__nobreak_args([X2|XL],TreeL,N2),
	N3 is N1 + N2 + 1.

%%d prt__break_args(gr::in,int::in,treeL::out,int::out)

prt__break_args([X],[nobreak(Tree,')')],N2) :-
	prt__term_tree(X,999,Tree,N1),
	N2 is N1 + 1.
prt__break_args([X1,X2|XL],[break(Tree,',')|TreeL],N3) :-
	prt__term_tree(X1,999,Tree,N1),
	prt__break_args([X2|XL],TreeL,N2),
	N3 is N1 + N2 + 1.

%%d prt__list_tree(grL::in,fr::in,int::in,grL::out,int::out)

prt__list_tree([],Tree,N1,[nobreak(Tree,']')],N2) :-
	N2 is N1 + 1.
prt__list_tree([X|XL],Tree1,N1,[break(Tree1,',')|TreeL],N4) :-
	prt__term_tree(X,999,Tree2,N2),
	prt__list_tree(XL,Tree2,N2,TreeL,N3),
	N4 is N1 + N3 + 1.
prt__list_tree(X,Tree1,N1,TreeL,N3) :-
	\+ lst__list_form(X),
	prt__term_tree(X,999,Tree2,N2),
	TreeL = [break(Tree1,'|'),nobreak(Tree2,']')],
	N3 is N1 + N2 + 2.

%%d prt__infix(gr::in,int::out,int::out,int::out)

prt__infix(Op,Prec1,Prec2,Prec1) :-
	current_op(Prec1,xfy,Op),
	Prec2 is Prec1 - 1.
prt__infix(Op,Prec1,Prec1,Prec2) :- 
	current_op(Prec1,yfx,Op),
	Prec2 is Prec1 - 1.
prt__infix(Op,Prec1,Prec2,Prec2) :-
	current_op(Prec1,xfx,Op),
	Prec2 is Prec1 - 1.

%%d prt__prefix(gr::in,int::out,int::out)

prt__prefix(Op,Prec,Prec) :-
	current_op(Prec,fy,Op).
prt__prefix(Op,Prec1,Prec2) :-
	current_op(Prec1,fx,Op),
	Prec2 is Prec1 - 1.

%%d prt__postfix(gr::in,int::out,int::out)

prt__postfix(Op,Prec,Prec) :-
	current_op(Prec,yf,Op).
prt__postfix(Op,Prec1,Prec2) :-
	current_op(Prec1,xf,Op),
	Prec2 is Prec1 - 1.

%%d prt__parenthesize(int::in,int::in,tree::in,int::in,tree::out,int::out)

prt__parenthesize(Prec1,Prec2,Tree1,N1,Tree2,N2) :-
	(	Prec2 =< Prec1 ->
		Tree2 = Tree1,
		N2 = N1
	;	N2 is N1 + 2,			% 2 for '(' and ')'
		Tree2 = node(parenthesis(Tree1),N2)
	).

%%d prt__linear(tree::in)

prt__linear(node(constant(Name),_)) :-
	writeq(Name).
prt__linear(node(structure(Name,_,TreeL),_)) :-
	writeq(Name),
	write('('),
	prt__linearL(TreeL).
prt__linear(node(list(TreeL),_)) :-
	write('['),
	prt__linearL(TreeL).
prt__linear(node(parenthesis(Tree),_)) :-
	write('('),
	prt__linear(Tree),
	write(')').
prt__linear(node(infix(Name,_,Tree1,Tree2),_)) :-
	prt__linear(Tree1),
	(	Name = (:) ->
		write(:),
		(	Tree1 = node(constant(_),_) ->
			true
		;	write(' ')
		)
	;	write(' '),
		writeq(Name),
		write(' ')
	),
	prt__linear(Tree2).
prt__linear(node(prefix(Name,_,Tree),_)) :-
	writeq(Name), 
	(	prt__prefix_nolayout(Name) ->
		true
	;	write(' ')
	),
	prt__linear(Tree).
prt__linear(node(postfix(Name,_,Tree),_)) :-
	prt__linear(Tree),
	write(' '),
	writeq(Name).

%%d prt__linearL(treeL::in)

prt__linearL([]).
prt__linearL([nobreak(Tree,Sep)|TreeL]) :-
	prt__linear(Tree),
	write(Sep),
	prt__linearL(TreeL).
prt__linearL([break(Tree,Sep)|TreeL]) :-
	prt__linear(Tree),
	write(Sep),
	prt__linearL(TreeL).

%
% The model of the screen:
%
% +---+---+--------------------------------------------------------+---+
% | 0 | 1 |                                                        | R |
% +---+---+--------------------------------------------------------+---+
%      <--------- screen ----------------------------------------->
%
% +---+---+--------+---+-------+------------+---+------------------+---+
% | 0 | 1 |        | L | L + 1 |            | P |                  | R |
% +---+---+--------+---+-------+------------+---+------------------+---+
%                       <----- screen ---------------------------->
%                                            <---- write --------->
%
% At the beginning: L = 0 and P = 1
%

%%d prt__write_tree(tree::in,int::in,int::in,int::in,int::out)

prt__write_tree(node(Tree,N),L,P1,R,P2) :-
	(	N =< R - P1 ->
		prt__linear(node(Tree,N)),
		P2 is P1 + N
	;	prt__non_linear(node(Tree,N),L,P1,R,P2)
	).

%%d prt__non_linear(tree::in,int::in,int::in,int::in,int::out)

prt__non_linear(node(constant(Name),N),L,P1,_,P2) :-
	prt__nl(L,P1),
	writeq(Name),
	P2 is L + N + 1.
prt__non_linear(node(structure(Name,N,TreeL),_),L1,P1,R,P3) :-
    	prt__nl(L1,P1),
    	writeq(Name), write('('),
	prt__indent(I),
	L2 is (L1 + I) mod R,
	P2 is L1 + N + 2,
	prt__write_treeL(TreeL,L2,P2,R,P3).
prt__non_linear(node(list(TreeL),_),L1,P1,R,P3) :-
	prt__nl(L1,P1),
    	write('['),
	P2 is L1 + 2,
	prt__indent(I),
	L2 is (L1 + I) mod R,
	prt__write_treeL(TreeL,L2,P2,R,P3).
prt__non_linear(node(parenthesis(Tree),_),L1,P1,R,P4) :-
	prt__nl(L1,P1),
    	write('('),
	prt__indent(I),
	L2 is (L1 + I) mod R,
	P2 is L1 + 2,
	prt__write_tree(Tree,L2,P2,R,P3),
	write(')'),
	P4 is P3 + 1.
prt__non_linear(node(infix(Name,N,Tree1,Tree2),_),L1,P1,R,P7) :-
	(	prt__no_break(Name) ->	% Try it on the next line.
		prt__nl(L1,P1),
    		P2 is (L1 + 1) mod R
	;	P2 = P1
	),
	prt__write_tree(Tree1,L1,P2,R,P3),
	(	Name = (:) ->		% Colon is a special case.
		write(:),
		P5 is P3 + 1
	;	N =< 2 ->		% Small operators on the same line.
		write(' '),
		writeq(Name),
		P5 is P3 + N + 1
	;	prt__write_space(P3,R,P4),
		prt__writeq_atom(Name,N,L1,P4,R,P5)
	),
	(	current_op(_,yfx,Name),	% Force a line break.
		Tree2 = node(_,Size),
		R - P5 < Size ->
		P6 = R
	;	prt__write_space(P5,R,P6)
	),
	(	prt__indent_infix(Name) -> 
		prt__indent(I),
		L2 is (L1 + I) mod R
	;	L2 = L1
	),
	prt__write_tree(Tree2,L2,P6,R,P7).
prt__non_linear(node(prefix(Name,N,Tree),_),L,P1,R,P5) :-
	(	prt__no_break(Name) ->
		prt__nl(L,P1),
		P2 is (L + 1) mod R
	;	P2 = P1
	),
	prt__writeq_atom(Name,N,L,P2,R,P3),
	(	prt__prefix_nolayout(Name) ->
		P4 = P3
	;	prt__write_space(P3,R,P4)
	),
	prt__write_tree(Tree,L,P4,R,P5).
prt__non_linear(node(postfix(Name,N,Tree),_),L,P1,R,P4) :-
	prt__write_tree(Tree,L,P1,R,P2),
	prt__write_space(P2,R,P3),
	prt__writeq_atom(Name,N,L,P3,R,P4).

%%d prt__write_treeL(treeL::in,int::in,int::in,int::in,int::out)

prt__write_treeL([],_,P,_,P).
prt__write_treeL([nobreak(Tree,Sep)|TreeL],L,P1,R,P4) :-
	prt__write_tree(Tree,L,P1,R,P2),
	write(Sep),
	P3 is P2 + 1,
	prt__write_treeL(TreeL,L,P3,R,P4).
prt__write_treeL([break(Tree,Sep)|TreeL],L,P1,R,P2) :-
	prt__write_tree(Tree,L,P1,R,_),
	write(Sep),
	prt__write_treeL(TreeL,L,R,R,P2).	% force a line break

%%d prt__write_space(int::in,int::in,int::out)

prt__write_space(P1,R,P2) :-
	(	P1 < R ->
		write(' '),
		P2 is P1 + 1
	;	P2 = P1
	).

%%d prt__writeq_atom(gr::in,int::in,int::in,int::in,int::in,int::out)

prt__writeq_atom(Name,N,L,P1,R,P2) :-
	(	R - P1 < N ->
		prt__nl(L,P1),
		writeq(Name),
		P2 is L + N + 1
	;	writeq(Name),
		P2 is P1 + N
	).

%%d prt__write_atom(gr::in,int::in,int::in,int::in,int::in,int::out)

prt__write_atom(Name,N,L,P1,R,P2) :-
	(	R - P1 < N ->
		prt__nl(L,P1),
		write(Name),
		P2 is L + N + 1
	;	write(Name),
		P2 is P1 + N
	).

%%d prt__nl(int::in,int::in)

prt__nl(L,P) :- 
	(	P is L + 1 -> 
		true
	; 	nl, 
		tab(L)
	).

%%d prt__fact(gr::in,gr::in,gr::in,gr::in)

prt__fact(Kind,Ref,X,Y) :-
	nl, write(':- '),
	write(Kind),
	write('('),
	writeq(Ref),
	write(','), nl,
	prt__write(X),
	(	Y = 'DUMMY' ->
		true
	;	write(','), nl,
		prt__write(Y)
	),
	nl, write(').'), nl.

%%d prt__pred_def(gr::in,int::in,gr::in)

prt__pred_def(Name,N,X) :-
	nl, write(':- definition_pred('),
	write(Name), write(','), write(N), write(','), nl,
	prt__write(X,1), nl,
	write(').'), nl.

%%d prt__definition_fun(gr::in,gr::in,gr::in,gr::in)

prt__definition_fun(Name,N,X,Refex,Refuni) :-
	nl, write(':- definition_fun('),
	write(Name), write(','), write(N), write(','), nl,
	prt__write(X,1), write(','), nl,
	write(' existence by '), write(Refex), write(','), nl,
	write(' uniqueness by '), write(Refuni), nl,
	write(').'), nl.

% prt.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 6/11/95, 9:58 PM */
/* Updated: Mon Dec 28 20:24:14 1998 */
/* Filename: tex.pl */
/* Abstract: TeX output. */

% We use a TeX-hack for breaking logical formulas at the right places. A
% formula should be broken at positions which are not very deep in the
% formula. But TeX does not know about the depth of a formula. Therefore we
% have to inform TeX about the depth of a formula. For instance, the formula
% "a & b -> c \/ d" is translated into "\1 a\land b\2\to\1 c\lor d\2". The
% macro \1 tells TeX that it goes down one level; the macro \2 tells TeX
% that it goes up one level (cf. proofmacros.tex and 'TeX__bp_formula').
% Thus the formula "a & b -> c \/ d" is broken after -> and not around & or
% \/. Terms are treated differently (cf. 'TeX_term').

%%d 'TeX__non_tt_symbol'(gr::in)

% These symbols are printed in math mode and not in typewriter style.

'TeX__non_tt_symbol'(.).
'TeX__non_tt_symbol'(<).
'TeX__non_tt_symbol'(*).
'TeX__non_tt_symbol'(/).
'TeX__non_tt_symbol'(+).
'TeX__non_tt_symbol'(-).
'TeX__non_tt_symbol'([]).

%%d 'TeX__special_op'(gr::in,gr::out)

% A symbol '!xxx' must be declared as operator in op.pl and as TeX macro \xxx
% in proofmacros.tex!

'TeX__special_op'(=<,2,'!leq').
'TeX__special_op'(sub,2,'!sub').
'TeX__special_op'(**,2,'!app').
'TeX__special_op'(is,2,'!is').
'TeX__special_op'($,2,'!$').
'TeX__special_op'(@<,2,<).
'TeX__special_op'(@=<,2,'!leq').
'TeX__special_op'(@+,2,+).
'TeX__special_op'(@*,2,*).
'TeX__special_op'(//,2,'!apply').
'TeX__special_op'(#+,2,+).
'TeX__special_op'(#-,2,-).
'TeX__special_op'(#-,1,-).
'TeX__special_op'(#*,2,*).
'TeX__special_op'(#<,2,<).
'TeX__special_op'(#=<,2,'!leq').

%%d 'TeX__special_pred'(gr::out,gr::out)

'TeX__special_pred'(<>,'!neq').
'TeX__special_pred'(gr,gr).

%%d 'TeX__term'(tm::in,gr::out)

'TeX__term'($(Name),X) :-			% variables
	'TeX__var'(Name,X).
'TeX__term'([n(Name,_)|TermL],X) :-		% constructors in typewriter
	(	'TeX__special_op'(Name,_,Y) ->
		true
	;	'TeX__atom_tt'(Name,Y)
	),
	'TeX__termL'(TermL,XL),
	X =.. [Y|XL].
'TeX__term'([d(Name,N)|TermL],X) :-		% defined predicates in italic
	(	'TeX__special_op'(Name,N,Y) ->
		true
	;	'TeX__atom_it'(Name,Y)
	),
	'TeX__termL'(TermL,XL),
	X =.. [Y|XL].
'TeX__term'([f(Name,N)|TermL],X) :-		% defined functions in italic
	(	'TeX__special_op'(Name,N,Y) ->
		true
	;	'TeX__atom_it'(Name,Y)
	),
	'TeX__termL'(TermL,XL),
	X =.. [Y|XL].
'TeX__term'([=,Term|TermL],X2) :-		% equations
	'TeX__term'(Term,X1),
	'TeX__term_eq'(TermL,X1,X2).
'TeX__term'([Tag|TermL],X) :-			% special symbols in terms
	'TeX__special_pred'(Tag,Y),
	'TeX__termL'(TermL,XL),
	X =.. [Y|XL].


%%d 'TeX__termL'(tmL::in,grL::out)

'TeX__termL'([],[]).
'TeX__termL'([Term|TermL],[X|XL]) :-
	'TeX__term'(Term,X),
	'TeX__termL'(TermL,XL).

%%d 'TeX__term_eq'(gr::in,expL::in,gr::in,gr::out)

'TeX__term_eq'([],X,X).
'TeX__term_eq'([Term|TermL],X1,X4) :-
	'TeX__term'(Term,X2),
	X3 =.. ['!eq',X1,X2],
	'TeX__term_eq'(TermL,X3,X4).

%%d 'TeX__atom_tt'(gr::in,gr::out)

'TeX__atom_tt'(X,Y) :- 
	(	'TeX__non_tt_symbol'(X) ->
		Y = X
	;	concat_atom(['!Tt{',X ,'}'],Y)
	).

%%d 'TeX__atom_it'(gr::in,gr::out)

'TeX__atom_it'(X,Y) :- 
	(	'TeX__non_tt_symbol'(X) ->
		Y = X
	;	 concat_atom(['!It{',X ,'}'],Y)
	).

%%d 'TeX__is_digit'(int::in)

'TeX__is_digit'(X) :- 48 =< X, X =< 57.

% name('_{}',[95,123,125]).
% name('!It{',[33,73,116,123]).

%%d 'TeX__var'(gr::in,gr::out)

'TeX__var'(X,Y) :-
	(	integer(X) -> 
		concat_atom(['!v{',X,'}'],Y)
	;	name(X,L1),
		lst__reverse(L1,L2),
		L2 = [X1|L3],
		'TeX__is_digit'(X1) ->
		(	L3 = [X2|L4],
			'TeX__is_digit'(X2) ->
			lst__reverse([125,X1,X2,123,95|L4],L5)
		;	lst__reverse([X1,95|L3],L5)		
		),
		name(Y,L5)
	;	Y = X
	).

%%d 'TeX__varL'(grL::in,grL::out)

'TeX__varL'([],[]).
'TeX__varL'([X|XL],[Y|YL]) :-
	'TeX__var'(X,Y),
	'TeX__varL'(XL,YL).

%%d 'TeX__bp_formula'(fml::in)

'TeX__bp_formula'(Form) :-
	write('!0'),
	'TeX__bp_formula'(Form,1200).

%%d 'TeX__bp_formula'(fml::in,int::in)

'TeX__bp_formula'([Op1,Form],N1) :-
	'TeX__bp_unary_op'(Op1,Op2),
	prt__prefix(Op1,N2,N3),
	'TeX__bp_left'(N1,N2),
	write(Op2),
	'TeX__bp_formula'(Form,N3),	
	'TeX__bp_right'(N1,N2).
'TeX__bp_formula'([Op1,Form1,Form2],N1) :-
	'TeX__bp_binary_op'(Op1,Op2),
	prt__infix(Op1,N2,N3,N4),
	'TeX__bp_left'(N1,N2),
	'TeX__bp_formula'(Form1,N3),
	write(Op2),
	'TeX__bp_formula'(Form2,N4),	
	'TeX__bp_right'(N1,N2).
'TeX__bp_formula'([&],_) :- write('!top').
'TeX__bp_formula'([\/],_) :- write('!bot').
'TeX__bp_formula'([Op1,Form|FormL],N1) :-
	'TeX__bp_associative_op'(Op1,Op2),
	current_op(N2,_,Op1),
	'TeX__bp_left'(N1,N2),
	N3 is N2 - 1,
	'TeX__bp_formula'(Form,N3),
	'TeX__bp_list'(FormL,Op2,N3),
	'TeX__bp_right'(N1,N2).
'TeX__bp_formula'([Tag|FormL],_) :-
	\+ 'TeX__bp_special'(Tag),
	'TeX__term'([Tag|FormL],X),
	write(X).
'TeX__bp_formula'(@(Tag1,Var1L,Form),N) :-
	'TeX__bp_quantifier'(Tag1,Tag2),
	'TeX__varL'(Var1L,Var2L),
	'TeX__bp_left'(N,900),
	write(Tag2), write(Var2L), write('!,'),
	'TeX__bp_formula'(Form,900),	
	'TeX__bp_right'(N,900).

%%d 'TeX__bp_list'(fmlL::in,gr::in,int::in)

'TeX__bp_list'([],_,_).
'TeX__bp_list'([Form|FormL],Op,N) :-
	write(Op),
	'TeX__bp_formula'(Form,N),
	'TeX__bp_list'(FormL,Op,N).

%%d 'TeX__bp_left'(int::in,int::in)

'TeX__bp_left'(N1,N2) :-
	write('!1'),
	(	N1 < N2 -> 
		write('(')
	; 	true
	).

%%d 'TeX__bp_right'(int::in,int::in)

'TeX__bp_right'(N1,N2) :-
	(	N1 < N2 -> 
		write(')')
	; 	true
	),
	write('!2').

%%d 'TeX__bp_quantifier'(gr::in,gr::out)

'TeX__bp_quantifier'(all,'!all').
'TeX__bp_quantifier'(ex,'!ex').

%%d 'TeX__bp_unary_op'(gr::in,gr::out)

'TeX__bp_unary_op'(~,'!lnot ').
'TeX__bp_unary_op'(def,'!D ').
'TeX__bp_unary_op'(succeeds,'!S ').
'TeX__bp_unary_op'(fails,'!F ').
'TeX__bp_unary_op'(terminates,'!T ').

%%d 'TeX__bp_binary_op'(gr::in,gr::out)

'TeX__bp_binary_op'(=>,'!to ').
'TeX__bp_binary_op'(<=>,'!eqv ').

%%d 'TeX__bp_associative_op'(gr::in,gr::out)

'TeX__bp_associative_op'(&,'!land ').
'TeX__bp_associative_op'(\/,'!lor ').

%%d 'TeX__bp_special'(gr::in)

'TeX__bp_special'(X) :- 'TeX__bp_unary_op'(X,_).
'TeX__bp_special'(X) :- 'TeX__bp_binary_op'(X,_).
'TeX__bp_special'(X) :- 'TeX__bp_associative_op'(X,_).

%%d 'TeX__preamble'(gr::in)

'TeX__preamble'(Path) :-
	write(\),
	write('input '),
	io__expand($(tex)/'proofmacros.tex',Name1),
	write(Name1), nl,
	io__path_last(Path,Name2),
	write('!title{'),
	write(Name2),
	write('}'), nl.

%%d 'TeX__postamble'

'TeX__postamble' :-
	nl, write('!end'), nl.

%%d 'TeX__write_dollar'(fml::in)

'TeX__write_dollar'(Form) :-
	write('$'),
	'TeX__bp_formula'(Form),
	write('$').

%%d 'TeX__write_dollar_dot'(fml::in)

'TeX__write_dollar_dot'(Form) :-
	'TeX__write_dollar'(Form),
	write('.').

%%d 'TeX__write_braces'(fml::in)

'TeX__write_braces'(Form) :-
	write('{'),
	'TeX__bp_formula'(Form),
	write('}').

%%d 'TeX__write_dollarL'(fmlL::in)

'TeX__write_dollarL'([]) :-
	write('{none}').
'TeX__write_dollarL'([Form|FormL]) :-
	write('{'),
	'TeX__write_dollar'(Form),
	'TeX__write_dollarL_comma'(FormL).

%%d 'TeX__write_dollarL_comma'(fml::in)

'TeX__write_dollarL_comma'([]) :-
	write('}').
'TeX__write_dollarL_comma'([Form|FormL]) :-
	write(' and '),
	'TeX__write_dollar'(Form),
	'TeX__write_dollarL_comma'(FormL).

%%d 'TeX__write_fact'(gr::in,gr::in,fml::in,drv::in)
	
'TeX__write_fact'(Kind,Ref,Form,Deriv) :-
	nl, write(!), write(Kind),
	write('{'), write(Ref), write('}'),
	'TeX__write_braces'(Form), nl,
	(	Deriv = [] ->
		true
	;	write('!Pr{'),
		'TeX__write_derivation'(Deriv),
		write('}'), nl,
		write('!Epr'), nl
	).

%%d 'TeX__write_derivation'(drv::in)

'TeX__write_derivation'([]).
'TeX__write_derivation'([Step|Deriv]) :-
	'TeX__write_derivation_step'(Step),
	'TeX__write_derivation'(Deriv).

%%d 'TeX__write_derivation_step'(dstp::in)

'TeX__write_derivation_step'([Tag|FormL]) :-
	nl, 'TeX__write_dollar_dot'([Tag|FormL]).
'TeX__write_derivation_step'(@(Tag,VarL,Form)) :-
	nl, 'TeX__write_dollar_dot'(@(Tag,VarL,Form)).
'TeX__write_derivation_step'(assume(Form1,Deriv,Form2)) :-
	nl, write('!Ass'),
	'TeX__write_braces'(Form1),
	'TeX__write_derivation'(Deriv), nl,
	write('!Eass'),
	'TeX__write_braces'([=>,Form1,Form2]).
'TeX__write_derivation_step'(contra(Form,Deriv)) :-
	nl, write('!Con'),
	'TeX__write_braces'(Form),
	'TeX__write_derivation'(Deriv), nl,
	write('!Econ'),
	'TeX__write_braces'([~,Form]).
'TeX__write_derivation_step'(cases(CaseL,Form)) :-
	'TeX__write_cases'(CaseL), nl,
	write('!Fin'),
	'TeX__write_braces'(Form).
'TeX__write_derivation_step'(indirect([~,Form],Deriv)) :-
	nl, write('!Dir'),
	'TeX__write_braces'([~,Form]),
	'TeX__write_derivation'(Deriv), nl,
	write('!Edir'),
	'TeX__write_braces'(Form).
'TeX__write_derivation_step'(exist(VarL,Form1,Deriv,Form2)) :-
	nl, write('!Ex'),
	'TeX__varL'(VarL,XL),
	write(XL),
	'TeX__write_braces'(Form1),
	'TeX__write_derivation'(Deriv), nl,
	write('!Eex'),
	'TeX__write_braces'(Form2).
'TeX__write_derivation_step'(by(Form,X)) :-
	nl, 'TeX__write_dollar'(Form),
	'TeX__write_by'(X).
'TeX__write_derivation_step'(induction(FormL,StepL)) :-
	nl, write('!Ind'),
	'TeX__write_dollarL'(FormL),
	'TeX__write_induction_steps'(StepL),
	nl,write('!Eind').

%%d 'TeX__write_cases'(casL::in)

'TeX__write_cases'([]).
'TeX__write_cases'([case(Form,Case)|CaseL]) :-
	nl, write('!Cas'),
	'TeX__write_braces'(Form),
	'TeX__write_derivation'(Case), nl,
	write('!Ecas'),
	'TeX__write_cases'(CaseL).

%%d 'TeX__write_induction_steps'(istpL::in)

'TeX__write_induction_steps'([]).
'TeX__write_induction_steps'([step(_,FormL,Deriv,Form)|StepL]) :-
	nl, write('!Stp'),
	'TeX__write_dollarL'(FormL),
	'TeX__write_derivation'(Deriv), nl,
	write('!Estp'),
	'TeX__write_braces'(Form),
	'TeX__write_induction_steps'(StepL).

%%d 'TeX__write_by'(gr::in)

'TeX__write_by'(X) :-
	(	X = comment(Y) ->
		write('! by~'),
		write(Y),
		write('.')
	;	X = gap ->
		write('! by~{!bf GAP}.')
	;	X = theorem(Ref) ->
		write('!by{Theorem}{'),
		write(Ref),
		write('}.')
	;	X = lemma(Ref) ->
		write('!by{Lemma}{'),
		write(Ref),
		write('}.')
	;	X = corollary(Ref) ->
		write('!by{Corollary}{'),
		write(Ref),
		write('}.')
	;	X = axiom(Ref) ->
		write('!by{Axiom}{'),
		write(Ref),
		write('}.')
	;	(	X = elimination(Name,N)
		; 	X = introduction(Name,N)
		;	X = existence(Name,N)
		;	X = uniqueness(Name,N)
		) ->
		write('!by{Definition}{'),
		write(Name),
		write(/),
		write(N),
		write('}.')
	;	write('! by~'),
		write(X), 
		write('.')
	).

%%d 'TeX__write_def'(gr::in,int::in,fml::in)

'TeX__write_def'(Name,N,Form) :-
	nl, write('!definition{'),
	write(Name),
	write(/), 
	write(N), 
	write('}'),
	'TeX__write_braces'(Form), nl.

% tex.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:53:24 1994 */
/* Filename: lst.pl */
/* Abstract: List processing predicates. */

%%d lst__member(any,any)

lst__member(X,[X|_]).
lst__member(X,[_|L]) :- lst__member(X,L).

%%d lst__member_check(any,any)

lst__member_check(X,[Y|L]) :-
	(	X = Y ->
		true
	;	lst__member_check(X,L)
	).

%%d lst__concat(any,any,any)

lst__concat(L1,L2,L3) :-
	(	L2 = [] ->
		L3 = L1
	;	lst__append(L1,L2,L3)
	).

%%d lst__append(any,any,any)

lst__append([],L,L).
lst__append([X|L1],L2,[X|L3]) :- lst__append(L1,L2,L3).

%%d lst__append_set(grL::in,grL::in,grL::out)

lst__append_set([],L,L).
lst__append_set([X|L1],L2,L4) :-
	lst__append_set(L1,L2,L3),
	lst__add_element(X,L3,L4).

%%d lst__add_element(gr::in,grL::in,grL::out)

lst__add_element(X,L1,L2) :- 
	(	lst__member_check(X,L1) -> 
		L2 = L1
	; 	L2 = [X|L1]
	).

%%d lst__delete(any,any,any)

lst__delete(X,[X|L],L).
lst__delete(X,[Y|L1],[Y|L2]) :-
	lst__delete(X,L1,L2).

%%d lst__permutation(any,any)

lst__permutation([],[]).
lst__permutation(L1,[X|L3]) :-
	lst__delete(X,L1,L2),
	lst__permutation(L2,L3).

%%d lst__singleton(gr::in)

lst__singleton([_]).

%%d lst__two_elements(gr::in)

lst__two_elements([_,_]).

%%d lst__subset(grL::in,grL::in)

lst__subset(L1,L2) :- \+ lst__not_subset(L1,L2).

%%d lst__not_subset(grL::in,grL::in)

lst__not_subset(L1,L2) :-
	lst__member(X,L1),
	\+ lst__member(X,L2).

%%d lst__reverse(any,any)

lst__reverse(L1,L2) :-
	lst__reverse(L1,[],L2).

%%d lst__reverse(any,any,any)

lst__reverse([],L,L).
lst__reverse([X|L1],L2,L3) :- lst__reverse(L1,[X|L2],L3).

%%d lst__disjoint(grL::in,grL::in)

lst__disjoint(L1,L2) :-
	\+ lst__not_disjoint(L1,L2).

%%d lst__not_disjoint(grL::in,grL::in)

lst__not_disjoint(L1,L2) :-
	lst__member(X,L1), lst__member(X,L2).

%%d lst__list_form(gr::in)

lst__list_form([]).
lst__list_form([_|_]).

%%d lst__set_minus(grL::in,grL::in,grL::out)

lst__set_minus([],_,[]).
lst__set_minus([X|L1],L2,L4) :-
	(	lst__member_check(X,L2) ->
		L4 = L3
	;	L4 = [X|L3]
	),
	lst__set_minus(L1,L2,L3).

%%d lst__member_con(any,any)

lst__member_con(X,[[&|L]|_]) :- lst__member(X,L).
lst__member_con(X,[X|_]).
lst__member_con(X,[_|L]) :- lst__member_con(X,L).

%%d lst__member_con_check(any,any)

lst__member_con_check(X,[Y|L1]) :-
	(	X = Y ->
		true
	;	Y = [&|L2],
		lst__member_check(X,L2) ->
		true
	;	lst__member_con_check(X,L1)
	).

%%d lst__delete_con(any,any,any)

lst__delete_con(X,[X|L],L).
lst__delete_con(X,[[&|M1]|L],[[&|M2]|L]) :-
	lst__delete(X,M1,M2).
lst__delete_con(X,[Y|L1],[Y|L2]) :-
	lst__delete_con(X,L1,L2).

% lst.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:53:06 1994 */
/* Updated: Wed Jul 21 10:11:11 1999 */
/* Filename: io.pl */
/* Abstract: Input and output. */

% For every suffix X in [thm,tex,err,gr], a stream can be generated with
% 
% 	io__open(X,Path)	side effect: io__preamble(X,Path)
% 	
% Then, the following predicates are available:
% 
% 	db__is_open(X)
% 	io__tell(X)
% 	io__close(X)		side effect: io__postamble(X)


%%d db__is_open(gr::in)
%%d db__stream(gr::in,gr::out)

:- dynamic(db__is_open/1).
:- dynamic(db__stream/2).

db__is_open('DUMMY').
db__stream('DUMMY','DUMMY').

%%d io__open(gr::in,gr::in)

io__open(Suffix,Path) :-
	(	db__is_open(Suffix) ->
		io__close(Suffix)
	;	true
	),
	(	Suffix = tex,
		\+ db__flag(tex_output) ->
		true
	;	Suffix = thm,
		\+ db__flag(thm_output) ->
		true
	;	io__expand(Path,File),
		concat_atom([File,'.',Suffix],File_Suffix),
		io__get_stream(File_Suffix,write,Stream),
		assert(db__stream(Suffix,Stream)),
		assert(db__is_open(Suffix)),
		io__set_output(Stream),
		io__preamble(Suffix,Path)
	).

%%d io__tell(gr::in)

io__tell(Suffix) :-
	db__stream(Suffix,Stream),
	io__set_output(Stream).

%%d io__close(gr::in)

io__close(Suffix) :-
	(	db__is_open(Suffix) ->
		db__stream(Suffix,Stream),
		io__set_output(Stream),
		io__postamble(Suffix),
		close(Stream),
		retract(db__is_open(Suffix)),
		retract(db__stream(Suffix,Stream))
	;	true
	).

%%d io__closeL(grL::in)

io__closeL([]).
io__closeL([Suffix|SuffixL]) :-
	io__close(Suffix),
	io__closeL(SuffixL).

%%d io__close_output(gr::in)

io__close_output(File) :-
	io__close_output,
	ctl__message([File,'o.k']).

%%d io__close_output

io__close_output :-
	(	bagof(Suffix,
			(db__is_open(Suffix), \+ Suffix = 'DUMMY'),
			SuffixL) ->
		io__closeL(SuffixL)
	;	true
	).

%%d db__alias(gr::out,gr::out)

:- dynamic(db__alias/2).

db__alias(lptp,Path) :- io__lptp_home(Path).
db__alias(lib,$(lptp)/lib).
db__alias(tmp,$(lptp)/tmp).
db__alias(tex,$(lptp)/tex).
db__alias(test,$(lptp)/test).
db__alias(examples,$(lptp)/examples).
db__alias(parser,$(lptp)/examples/parser).

%%d io__defined_alias(gr::in)

io__defined_alias(Alias) :- db__alias(Alias,_).

%%d io__set_alias(gr::in,gr::in)

io__set_alias(Alias,Path) :-
	(	io__defined_alias(Alias) ->
		ctl__warning([alias,q(Alias),is,redefined]),
		retract(db__alias(Alias,_)),
		assert(db__alias(Alias,Path))
	;	atomic(Alias) ->
		assert(db__alias(Alias,Path))
	;	ctl__error([q(Alias),is,not,atomic])
	).

%%d io__expand(gr::in,gr::out)

io__expand(Path1,Name) :-
	(	io__substitute(Path1,Path2) ->
		io__insert_sep(Path2,NameL),
		concat_atom(NameL,Name)
	;	ctl__error([cannot,expand,q(Path1),into,a,filename])
	).

%%d io__substitute(gr::in,gr::out)

io__substitute(Name,Name) :-
	atom(Name).
io__substitute(Path1/Name,Path2/Name) :-
	atom(Name),
	io__substitute(Path1,Path2).
io__substitute(Path1/ $(Alias),Path4/Path3) :-
	db__alias(Alias,Path2),
	io__expand(Path2,Path3),
	io__substitute(Path1,Path4).
io__substitute($(Alias),Path2) :-
	db__alias(Alias,Path1),
	io__expand(Path1,Path2).

%%d io__insert_sep(gr::in,grL::out).

io__insert_sep(Path/Name,NameL) :-
	io__insert_sep(Path,[Name],NameL).
io__insert_sep(Name,[Name]) :-
	atom(Name).

%%d io__insert_sep(gr::in,grL::in,grL::out)

io__insert_sep(Name,NameL,[Name,Sep|NameL]) :-
	atom(Name),
	io__path_sep(Sep).
io__insert_sep(Path/Name,Name1L,Name2L) :-
	io__path_sep(Sep),
	io__insert_sep(Path,[Name,Sep|Name1L],Name2L).

%%d io__path_last(gr::in,gr::in)

io__path_last(Name,Name) :-
	atom(Name).
io__path_last(_/Name,Name).

%%d io__preamble(gr::in,gr::in)

io__preamble(Suffix,Path) :-
	(	Suffix = tex -> 
		'TeX__preamble'(Path)
	;	true
	).

%%d io__postamble(gr::in)

io__postamble(Suffix) :-
	(	Suffix = tex ->
		'TeX__postamble'
	;	true
	).

% the user stream is treated differently


%%d io__open_user(gr::in)

io__open_user(Path) :-
	io__expand(Path,File),
	abolish(db__user_stream/1),
	concat_atom([File,'.err'],File_err),
	io__get_stream(File_err,write,Stream),
	assert(db__user_stream(Stream)).

%%d io__tell_user

io__tell_user :-
	db__user_stream(Stream),
	io__set_output(Stream).

%%d io__close_user

io__close_user :-
	db__user_stream(Stream),
	close(Stream),
	abolish(db__user_stream/1),
	io__original_user(User),
	assert(db__user_stream(User)).

%%d io__consult(gr::in,gr::in)

io__consult(Path,Suffix) :-
	io__expand(Path,File1),
	concat_atom([File1,'.',Suffix],File2),
	io__exec_file(File2).

%%d io__consult_thm(gr::in)

io__consult_thm(Path) :-
	io__consult(Path,thm),
	(	db__is_open(tex) ->
		io__tell(tex),
		write('!inputaux{'),
		io__path_last(Path,Name),
		write(Name), write('.aux}'), nl
	;	true
	).

% io.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: 6/11/95, 8:32 PM */
/* Updated: Wed Jul 21 13:41:22 1999 */
/* Filename: gnd.pl */
/* Abstract: Ground representation of programs. */

%% The predicate gnd__compile(F) transforms the clauses of the File <F>.pl into
%% ground facts of the form clauses(_,_) and writes them on the File <F>.gr.
%% It is assumed that the clauses do not contain subterms of the form 'VAR'(_).
%% Op declarations are ignored.

:- dynamic(tmp_clause/4).
:- dynamic(tmp_predicate_name/1).

%%d gnd__compile(gr::in)

gnd__compile(Path) :-
	io__expand(Path,File),
	concat_atom([File,'.pl'],File_pl),	% append '.pl' to File
	io__get_stream(File_pl,read,Stream_pl),	% open the file File_pl
	io__set_input(Stream_pl),		% set input to File_pl
	abolish(tmp_clause/4),
	abolish(tmp_predicate_name/1),
	once(gnd__read_clauses),		% read the file File_pl
	close(Stream_pl),
	concat_atom([File,'.gr'],File_gr),	% append '.gr' to File
	io__get_stream(File_gr,write,Stream_gr),% open the file File_gr
	io__set_output(Stream_gr),		% set output to File_gr
	setof(R,tmp_predicate_name(R),RL),	% collect all predicates
	gnd__write_clauses(RL),			% write clauses to File_gr
	close(Stream_gr),			% close the file File_gr
	ctl__message([new,file,File_gr]).

%%d gnd__read_clauses

gnd__read_clauses :-
	repeat,
	read_with_variables(Clause,NameL),
	gnd__instantiate_vars(NameL),
	(	Clause = end_of_file ->
		true
	;	once(gnd__convert_clause(Clause)), 
		fail
	).

%%d gnd__instantiate_vars(any)

gnd__instantiate_vars([]).
gnd__instantiate_vars([A = Variable|L]) :-
	name(A,[N|NL]),
	(	65 =< N, 
		N =< 90 ->			% ascii(N) is A-Z
		M is N + 32,			% ascii(M) is a-z
		name(B,[M|NL])
	;	B = A
	),
	Variable = 'VAR'(B),
	gnd__instantiate_vars(L).

%%d gnd__convert_clause(any)

gnd__convert_clause(:-(X,Y)) :-
	gnd__term(X,0,Atom,N1),
	gnd__goal(Y,N1,Goal,N2),
	gnd__assert_clause(X,Atom,Goal,N2).
gnd__convert_clause(X) :-
	\+ gnd__rule_form(X),
	gnd__term(X,0,Atom,N),
	gnd__assert_clause(X,Atom,[&],N).
	
%% op(1100,xfy,;)
%% op(1050,xfy,->)
%% op(100,xfy,',')

%%d gnd__rule_form(any)
%%d gnd__comma_form(any)
%%d gnd__semicolon_form(any)
%%d gnd__implication_form(any)

gnd__rule_form((_ :- _)).
gnd__comma_form((_ , _)).
gnd__comma_form(true).
gnd__semicolon_form((_ ; _)).
gnd__semicolon_form(fail).
gnd__implication_form((_ -> _)).

%%d gnd__goal_form(any)

gnd__goal_form(true).
gnd__goal_form(fail).
gnd__goal_form(_ = _).
gnd__goal_form(not _).
gnd__goal_form(\+ _).
gnd__goal_form((_ , _)).
gnd__goal_form((_ ; _)).

%%d gnd__goal(any,int::in,goal::out,int::out)

gnd__goal(true,N,[&],N).
gnd__goal(fail,N,[\/],N).
gnd__goal(X1 = X2,N1,[=,E1,E2],N3) :-
	gnd__term(X1,N1,E1,N2),
	gnd__term(X2,N2,E2,N3).
gnd__goal(not X,N1,[~,Goal],N2) :-
	gnd__goal(X,N1,Goal,N2).
gnd__goal(\+ X,N1,[~,Goal],N2) :-
	gnd__goal(X,N1,Goal,N2).
gnd__goal((X,Y),N1,[&|Goal2L],N3) :-
	gnd__conjunction(Y,N1,[],N2,Goal1L),
	gnd__conjunction(X,N2,Goal1L,N3,Goal2L).
gnd__goal(X,N1,[\/|GoalL],N2) :-
	gnd__semicolon_form(X),
	gnd__disjunction(X,N1,[],N2,GoalL).
gnd__goal(X,N1,Atom,N2) :-
	\+ gnd__goal_form(X),
	gnd__term(X,N1,Atom,N2).

%%d gnd__conjunction(any,int::in,goalL::out,int::out)

gnd__conjunction((X,Y),N1,Goal1L,N3,Goal3L) :-
	gnd__conjunction(Y,N1,Goal1L,N2,Goal2L),
	gnd__conjunction(X,N2,Goal2L,N3,Goal3L).
gnd__conjunction(true,N,GoalL,N,GoalL).
gnd__conjunction(X,N1,GoalL,N2,[Goal|GoalL]) :-
	\+ gnd__comma_form(X),
	gnd__goal(X,N1,Goal,N2).

%%d gnd__disjunction(any,int::in,goalL::out,int::out)

gnd__disjunction((X;Y),N1,Goal1L,N3,Goal3L) :-
	\+ gnd__implication_form(X),
	gnd__disjunction(Y,N1,Goal1L,N2,Goal2L),
	gnd__disjunction(X,N2,Goal2L,N3,Goal3L).
gnd__disjunction(fail,N,GoalL,N,GoalL).
gnd__disjunction(X,N1,GoalL,N2,[Goal|GoalL]) :-
	\+ gnd__semicolon_form(X),
	gnd__goal(X,N1,Goal,N2).
gnd__disjunction((X -> Y ; Z),N1,Goal1L,N4,Goal2L) :-
	gnd__goal(X,N1,Goal1,N2),
	gnd__goal(Y,N2,Goal2,N3),
	gnd__goal(Z,N3,Goal3,N4),
	gnd__join_conjunctions(Goal1,Goal2,Goal4),
	gnd__join_conjunctions([~,Goal1],Goal3,Goal5),
	Goal2L = [Goal4,Goal5|Goal1L].

%%d gnd__join_conjunctions(goal::in,goal::in,goal::out)

gnd__join_conjunctions(Goal1,Goal2,Goal3) :-
	(	Goal1 = [&|Goal1L] ->
		(	Goal2 = [&|Goal2L] ->
			lst__append(Goal1L,Goal2L,Goal3L),
			Goal3 = [&|Goal3L]
		;	lst__append(Goal1L,[Goal2],Goal3L),
			Goal3 = [&|Goal3L]
		)
	;	(	Goal2 = [&|Goal2L] ->
			Goal3 = [&,Goal1|Goal2L]
		;	Goal3 = [&,Goal1,Goal2]
		)
	).

%%d gnd__term(any,int::in,fml::out,int::out)

gnd__term(X,N1,$(N1),N2) :-		% X is bound to 'VAR'(N1)
	var(X),
	X = 'VAR'(N1),
	N2 is N1 + 1, !.
gnd__term('VAR'(X),N,$(X),N) :-		% 'VAR'(N)
	atomic(X), !.
gnd__term(X,N1,[n(Tag,N)|TermL],N2) :-	% f(T1,...,Tn)
	functor(X,Tag,N),
	X =.. [Tag|XL],
	gnd__termL(XL,N1,TermL,N2).

%%d gnd__termL(any,int::in,tmL::in,int::out)

gnd__termL([],N,[],N).
gnd__termL([X|XL],N1,[Term|TermL],N3) :-
	gnd__term(X,N1,Term,N2),
	gnd__termL(XL,N2,TermL,N3).

%%d gnd__assert_clause(gr::in,atm::in,goal::in,int::in)

gnd__assert_clause(X,Atom,Goal,N) :-
	eq__add_free_qf(Atom,[],V1L),
	eq__add_free_qf(Goal,V1L,V2L),
	lst__reverse(V2L,V3L),
	functor(X,Functor,Arity),
	assert(tmp_clause(n(Functor,Arity),Atom,Goal,V3L/N)),
	assert(tmp_predicate_name(n(Functor,Arity))).

%%d gnd__write_clauses(grL::in)

gnd__write_clauses([]).
gnd__write_clauses([Tag|TagL]) :-
	bagof(clause(Atom,Goal,N),tmp_clause(Tag,Atom,Goal,N),ClauseL),
	nl, write(':- assert_clauses('),
	writeq(Tag),
	write(',['), nl,
	gnd__write_clauseL(ClauseL),
	nl, write(']).'), nl,
	gnd__write_clauses(TagL).

%%d gnd__write_clauseL(clsL::in)

gnd__write_clauseL([]).
gnd__write_clauseL([Clause]) :-
	gnd__write_clause(Clause).
gnd__write_clauseL([Clause1,Clause2|ClauseL]) :-
	gnd__write_clause(Clause1),
	gnd__write_clause_comma([Clause2|ClauseL]).

%%d gnd__write_clause_comma(clsL::in)

gnd__write_clause_comma([]).
gnd__write_clause_comma([Clause|ClauseL]) :-
	write(','), nl,
	gnd__write_clause(Clause),
	gnd__write_clause_comma(ClauseL).

%%d gnd__write_clause(cls::in)

gnd__write_clause(clause(Atom,Goal,N)) :-
	write(' clause('),
	writeq(Atom), write(','), nl,
	write('  '),
	writeq(Goal),
	write(','), nl,
	write('  '),
	writeq(N),
	write(')').
	
% gnd.pl ends here

/*   Author: Robert Staerk <staerk@math.stanford.edu> */
/*  Created: Fri Dec  2 15:51:07 1994 */
/*  Updated: Wed Jul 21 14:06:12 1999 */
/* Filename: alias.pl */
/* Abstract: Some handy abbreviations. */

draft :-		ctl__draft.
pedantic :-		ctl__pedantic.
plain :-		ctl__plain.
show :-			ctl__show.

set(Flag) :-		ctl__set_flag(Flag).
unset(Flag) :-		ctl__unset_flag(Flag).

bye :- 			io__close_output.
bye(File) :- 		io__close_output(File).
needs_gr(Path) :- 	io__consult(Path,gr).
needs_thm(Path) :- 	io__consult_thm(Path).
prt_file(Path) :- 	io__open(prt,Path).
tex_file(Path) :- 	io__open(tex,Path).
thm_file(Path) :- 	io__open(thm,Path).
set(Alias,Path) :-	io__set_alias(Alias,Path).
exec(File) :-		io__exec_file(File).
check(Path) :-		once(io__consult(Path,pr)).

def(EForm) :-		once(ctl__prt_definition(EForm)).
facts(PTerm) :- 	ctl__print_facts(PTerm).
by(EForm,Opt) :-	tac__by(EForm,Opt).
compile_gr(Path) :- 	once(gnd__compile(Path)).
mark(EForm) :-          ctl__assert_marked_assumption(EForm).

e2i(EForm,Form) :- 	e2i__formula(EForm,Form).
e2id(EDeriv,Deriv) :- 	e2i__derivation(EDeriv,Deriv).
i2e(Form,EForm) :- 	i2e__expression(Form,EForm).
ipp(Form) :-		i2e__expression(Form,X), prt__write(X), nl.
pp(PTerm) :- 		prt__write(PTerm).

depends(Fact) :-	dep__print_dependencies(Fact).

banner :- 
	write('LPTP, Version 1.06, July 21, 1999.'), nl,
	write('Copyright (C) 1999 by Robert F. Staerk'), nl.

:- initialization(banner).

% alias.pl ends here

