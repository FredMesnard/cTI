%% Unknown predicates

on_statistics.

%% Dynamic predicates

pp_wid(X) :- '$top'(X).
save_continue.
abbrev_(X) :- '$top'(X).
abbrev__(X) :- '$top'(X).
proving(X) :- '$top'(X).
@(X) :- '$top'(X).
sw(X) :- '$top'(X).
continue.
expl_rewrite.
expl_expand.
vt100_.
expand_depth(X) :- '$top'(X).

%% Dynamic predicates, even if not declared as such

#(X) :- '$top'(X).
abbrev_(X,Y) :- '$top'(X,Y).
definition(X,Y) :- '$top'(X,Y).
jtuples(X) :- '$top'(X).
constructor(X) :- '$top'(X).
tp(X) :- '$top'(X).
accessor(X) :- '$top'(X).
type_prescription(X,Y) :- '$top'(X,Y).
machine(X,Y) :- '$top'(X,Y).
recursive(X) :- '$top'(X).
nonrecursive(X) :- '$top'(X).
recognizer(X) :- '$top'(X).
bottom_object(X) :- '$top'(X).
nasty_function(X) :- '$top'(X).
lexrel(X,Y) :- '$top'(X,Y).
note(X,Y,Z,W,V,U,T) :- '$top'(X,Y,Z,W,V,U,T).
measured_subset(X,Y) :- '$top'(X,Y).
induction_lemma(X,Y,Z,W) :- '$top'(X,Y,Z,W).
recognizer_constructor(X) :- '$top'(X).
recognizer_bottom(X) :- '$top'(X).
type_of_A(X) :- '$top'(X).
rewrite_lemma(X,Y,Z,W,V,U) :- '$top'(X,Y,Z,W,V,U).
bit_place(X,Y) :- '$top'(X,Y).
elimination_lemma(X,Y,Z) :- '$top'(X,Y,Z).
part(X,Y) :- '$top'(X,Y).
e.
rewrite_env(X) :- '$top'(X).
common(X) :- '$top'(X).
g.
generalization_lemma(X,Y,Z) :- '$top'(X,Y,Z).
scheme(X) :- '$top'(X).
pool(X,Y) :- '$top'(X,Y).
changeables(X,Y,Z) :- '$top'(X,Y,Z).
subsume_C(X,Y) :- '$top'(X,Y).
subsume_S(X,Y) :- '$top'(X,Y).
merge_S(X,Y,Z) :- '$top'(X,Y,Z).
merge_C(X,Y,Z) :- '$top'(X,Y,Z).
defun_loaded.
used_variable(X) :- '$top'(X).
constructor_TRs(X,Y) :- '$top'(X,Y).
lessp(X,Y) :- '$top'(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- op( 1199,  fx, (@)).
:- op( 1199,  fx, (#)).
:- op( 1150,  fx, (mode)).
:- op(  701,  fx, (note_env)).
:- op(  701,  fx, (make_env)).
:- op(  701,  fx, (shell)).
:- op(  701,  fx, (definition)).
:- op(  701,  fx, (axiom)).
:- op(  701,  fx, (lemma)).
:- op(  701,  fx, (theorem)).
:- op(  701,  fx, (abbrev)).
:- op(  101,  fx, ($)).
:- op(  100, xfx, (:)).
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

%
%	Converting a Formula into a Conjunction of Clauses
%

:- public formula_to_clauses/2.
:- mode formula_to_clauses(+,-). 
formula_to_clauses(F,C) :- if_expr(F,I),if_norms(I,A),if_elims([A],B),!,
	cleanup(B,C),!.

%
%	Converting Clauses into "implies" Formula
%

:- public clauses_to_implies/2.
:- mode clauses_to_implies(+,-).
clauses_to_implies([H|T],[HH|TT]) :- !,clause_to_implies(H,HH),
	clauses_to_implies(T,TT),!.
clauses_to_implies(X,X).

:- public clause_to_implies/2.
:- mode clause_to_implies(+,-).
clause_to_implies([X],X).
clause_to_implies(X,X) :- atomic(X),!. 
clause_to_implies(C,I) :- c2i([],C,I),!.

:- mode c2i(+,+,-).
c2i(H,[H1,H2|C],I) :- negate(H1,NH1),!,append(H,[NH1],NH),!,c2i(NH,[H2|C],I),!.
c2i([H],[C],[implies,H,C]).
c2i([],[C],C).
c2i(H,[C],[implies,[and|H],C]).

%
%	Expanding "not","implies","and" and "or"-term into "if"-term
%

:- public if_expr/2.
:- mode if_expr(+,-).

if_expr([$,X],[$,X]).

if_expr([not,X],Z) :- !,if_expr(X,Y),!,
	((Y=t,!,Z=f); (Y=f,!,Z=t); negate(Y,Z)),!.

if_expr([implies,P,Q],Z) :- !,if_expr(P,P1),!,if_expr(Q,Q1),!,
	((P1=f,!,Z=t);
	 (Q1=t,!,Z=t);
	 (P1=t,!,((Q1=f,!,Z=f);
		  Z=[if,Q1,t,f]));
	 (Q1=f,!,Z=[if,P1,f,t]);
	 Z=[if,P1,[if,Q1,t,f],t]),!.

if_expr([and,P],Z) :- !,if_expr(P,Z),!.
if_expr([and,P|Qs],Z) :- !,if_expr(P,P1),!,if_expr([and|Qs],Q1),!,
	((P1=f,!,Z=f);
	 (Q1=f,!,Z=f);
	 (P1=t,!,((Q1=t,!,Z=t);
		  Z=[if,Q1,t,f]));
	 (Q1=t,!,Z=[if,P1,t,f]);
	 Z=[if,P1,[if,Q1,t,f],f]),!.

if_expr([or,P],Z) :- !,if_expr(P,Z),!.
if_expr([or,P|Qs],Z) :- !,if_expr(P,P1),!,if_expr([or|Qs],Q1),!,
	((P1=t,!,Z=t);
	 (Q1=t,!,Z=t);
	 (P1=f,!,((Q1=f,!,Z=f);
		  Z=[if,Q1,t,f]));
	 (Q1=f,!,Z=[if,P1,t,f]);
	 Z=[if,P1,t,[if,Q1,t,f]]),!.

if_expr([if,t,L,_],Z) :- !,if_expr(L,Z),!.
if_expr([if,f,_,R],Z) :- !,if_expr(R,Z),!.
if_expr([if,T,L,R],Z) :- !,if_expr(T,TT),!,if_expr(L,LL),!,if_expr(R,RR),!,
	((T=t,!,Z=LL); (T=f,!,Z=RR); Z=[if,TT,LL,RR]),!.

if_expr([F|A],[F|B]) :- !,map_if_expr(A,B),!.
if_expr(X,X).

:- mode map_if_expr(+,-).
map_if_expr([A1|A],[B1|B]) :- if_expr(A1,B1),!,map_if_expr(A,B),!.
map_if_expr([],[]).

%
%	Repeat to apply the (wired-in-)rewrite rule:
%
%	[equal,[F,X1,...,[if,P,Q,R],...,Xn],
%	       [if,P,[F,X1,...,Q,...,Xn],
%		     [F,X1,...,R,...,Xn]]]
%
%	until all the "if"s are outside all other function symbols.
%

:- public if_norms/2.
:- mode if_norms(+,-).
if_norms(A,C) :- if_norm(A,B),!,((A=B,!,B=C); if_norms(B,C)),!.

:- mode if_norm(+,-).
if_norm([$,X],[$,X]).
if_norm([if,t,L,_],LL) :- !,if_norm(L,LL),!.
if_norm([if,f,_,R],RR) :- !,if_norm(R,RR),!.
if_norm([if,T,L,R],[if,TT,LL,RR]) :- !,
	if_norm(T,TT),!,if_norm(L,LL),!,if_norm(R,RR),!.
if_norm([F|A],C) :- !,map_if_norms(A,B),!,if_norm1([F],B,C),!.
if_norm(X,X).

:- mode if_norm1(+,+,-).
if_norm1(A,[[if,t,L,_]|Tail],B) :- !,if_norm1(A,[L|Tail],B),!.
if_norm1(A,[[if,f,_,R]|Tail],B) :- !,if_norm1(A,[R|Tail],B),!.
if_norm1(A,[[if,T,L,R]|Tail],[if,T,LL,RR]) :- !,
	reverse([L|A],L1),!,append(L1,Tail,LL),!,
	reverse([R|A],R1),!,append(R1,Tail,RR),!.
if_norm1(A,[X|Tail],B) :- if_norm1([X|A],Tail,B),!.
if_norm1([t,not],[],f).
if_norm1([f,not],[],t).
if_norm1(A,[],B) :- reverse(A,B),!.

:- mode map_if_norms(+,-).
map_if_norms([A1|A],[B1|B]) :- if_norms(A1,B1),!,map_if_norms(A,B),!.
map_if_norms([],[]).

%
%	Repeat to apply the (wired-in-)rewrite rule for a clause:
%
%	[L1', ..., L<n-1>', [if,P,Q,R], L<n+1>, ..., Lm]
%
%  ==>  [L1', ..., L<n-1>', [not,P], Q, L<n+1>, ..., Lm]
%	  AND
%	[L1', ..., L<n-1>',      P,  R, L<n+1>, ..., Lm]
%
%	until none of the "if"s introduced by literal Ln remain.
%

:- mode if_elims(+,-).
if_elims(Clause,Clauses) :- if_elim([[]],Clause,C),!,if_elims1(t,C,Clauses),!.

:- mode if_elims1(+,+,-).
if_elims1(_,[],t).
if_elims1(X,X,X).
if_elims1(_,Y,Z) :- map_if_elim(Y,Y1),!,if_elims1(Y,Y1,Z),!.

:- mode if_elim(+,+,-).
if_elim(_,[t|_],[]).
if_elim(Heads,[f|Tail],Clauses) :- !,if_elim(Heads,Tail,Clauses),!.
if_elim(Heads,[[if,t,L,_]|Tail],Clauses) :- !,
	if_elim(Heads,[L|Tail],Clauses),!.
if_elim(Heads,[[if,f,_,R]|Tail],Clauses) :- !,
	if_elim(Heads,[R|Tail],Clauses),!.
if_elim(Heads,[[if,T,L,R]|Tail],Clauses) :- !,
	if_norms([not,T],NT),!,if_expr(NT,N),!,
	new_heads(Heads,[N,L],[T,R],Heads1),!,
	((Heads1=[],!,Clauses=t);
	 (if_elim(Heads1,Tail,Clauses))),!.
if_elim(Heads,[X|Tail],Clauses) :-
	map_cons(X,Heads,Heads1),!,
	if_elim(Heads1,Tail,Clauses),!.
if_elim(A,[],C) :- map_reverse(A,C),!.

:- mode map_if_elim(+,-).
map_if_elim([C1|C],D) :- if_elim([[]],C1,C1s),!,map_if_elim(C,D1),!,
	append(C1s,D1,D),!.
map_if_elim([],[]).

:- mode new_heads(+,+,+,-).
new_heads(X,Y,Z,W) :- member(t,Y),!,new_heads(X,[],Z,W),!.
new_heads(X,Y,Z,W) :- member(t,Z),!,new_heads(X,Y,[],W),!.
new_heads(_,[],[],[]).
new_heads(X,Y,Z,W) :- new_heads1(X,Y,Z,W),!.

:- mode new_heads1(+,+,+,-).
new_heads1([X1|X],Y,[],[YY|W]) :- !,
	((Y=[f,Y1],!,YY=[Y1|X1]);
	 (Y=[Y1,f],!,YY=[Y1|X1]);
	 (Y=[Y1,Y2],!,YY=[Y2,Y1|X1])),!,
	new_heads1(X,Y,[],W),!.
new_heads1([X1|X],[],Y,[YY|W]) :- !,
	((Y=[f,Y1],!,YY=[Y1|X1]);
	 (Y=[Y1,f],!,YY=[Y1|X1]);
	 (Y=[Y1,Y2],!,YY=[Y2,Y1|X1])),!,
	new_heads1(X,[],Y,W),!.
new_heads1([X1|X],Y,Z,[YY,ZZ|W]) :-
	((Y=[f,Y1],!,YY=[Y1|X1]);
	 (Y=[Y1,f],!,YY=[Y1|X1]);
	 (Y=[Y1,Y2],!,YY=[Y2,Y1|X1])),!,
	((Z=[f,Z1],!,ZZ=[Z1|X1]);
	 (Z=[Z1,f],!,ZZ=[Z1|X1]);
	 (Z=[Z1,Z2],!,ZZ=[Z2,Z1|X1])),!,
	new_heads1(X,Y,Z,W),!.
new_heads1([],_,_,[]).

:- mode map_cons(+,+,-).
map_cons(f,X,X).
map_cons(X,[Y1|Y],[[X|Y1]|Z]) :- map_cons(X,Y,Z),!.
map_cons(_,[],[]).

:- mode map_reverse(+,-).
map_reverse([X1|X],[Y1|Y]) :- reverse(X1,Y1),!,map_reverse(X,Y),!.
map_reverse([],[]).

%
%	Logical Simplification of Clauses
%

:- public cleanup/2.
:- mode	cleanup(+,-).
cleanup(C1,C3) :- trivial_cleanup(C1,C2),!,
	( C2==f,C3=f ; C2==[],C3=t ; further_cleanup(C2,C3) ),!.
cleanup(X,X).

:- mode	trivial_cleanup(+,-).
trivial_cleanup(Cls,NCls) :- map_cleanup_clause(Cls,Cls1),
	( simply_false(Cls1),NCls=f ; remove_clauses(Cls1,NCls) ),!.

:- mode map_cleanup_clause(+,-).
map_cleanup_clause([C1|Clauses],[NC1|NC]) :- cleanup_clause(C1,NC1),
	map_cleanup_clause(Clauses,NC),!.
map_cleanup_clause([],[]).

:- mode simply_false(+).
simply_false([f|_]) :- !.
simply_false([[L]|Clauses]) :- negate(L,NL),member([NL],Clauses),!.
simply_false([_|Clauses]) :- simply_false(Clauses),!.

:- mode remove_clauses(+,-).
remove_clauses([t|Clauses],C) :- !,remove_clauses(Clauses,C),!.
remove_clauses([C1|Clauses],C) :- clause_member(C1,Clauses),!,
	remove_clauses(Clauses,C),!.
remove_clauses([C1|Clauses],[C1|C]) :- !,remove_clauses(Clauses,C),!.
remove_clauses([],[]).

:- mode	cleanup_clause(+,-).
cleanup_clause(C,NC) :- negin_all(C,C1),
	( simply_true(C1),NC=t ; remove_literals(C1,NC) ),!.

:- mode simply_true(+).
simply_true([t|_]) :- !.
simply_true([L|Lits]) :- negate(L,NL),member(NL,Lits),!.
simply_true([_|Lits]) :- simply_true(Lits),!.

:- mode remove_literals(+,-).
remove_literals([f|Lits],Cls) :- !,remove_literals(Lits,Cls),!.
remove_literals([L|Lits],Cls) :- member(L,Lits),!,remove_literals(Lits,Cls),!.
remove_literals([L|Lits],[L|Cls]) :- remove_literals(Lits,Cls),!.
remove_literals([],[]).

%
%	Further Reductions of Clauses
%

:- mode further_cleanup(+,-).
further_cleanup([Cl1|Cls],NewClauses) :- robinson_replace(Cl1,Cls,Cls1),!,
	further_cleanup(Cls1,NewClauses),!.
further_cleanup([Cl1|Cls],NewClauses) :- resolve(Cl1,Cls,Cls1),!,
	further_cleanup(Cls1,NewClauses),!.
further_cleanup([Cl1|Cls],[Cl1|NewClauses]) :- !,
	further_cleanup(Cls,NewClauses),!.
further_cleanup(X,X).

%	Robinson's "replacement principle"
%
%	[P,Q]
%	 and		==>	[P]
%	[P,[not,Q]]

:- mode robinson_replace(+,+,-).
robinson_replace(Cl1,[Cl2|Cls],[Qr|Cls]) :-
	one_and_rest(Cl1,P,Pr),one_and_rest(Cl2,Q,Qr),
	(negate(P,R),negin(Q,S) ; negin(P,R),negate(Q,S)),R==S,
	equ_clause(Pr,Qr),!.
robinson_replace(Cl1,[Cl2|Cls],[Cl2|Cls1]) :- robinson_replace(Cl1,Cls,Cls1),!.

%	Resolution
%
%	[P,Q]			[P,Q]
%	 and		==>	 and
%	[P,[not,Q],R]		[P,R] (resolvent)

:- mode resolve(+,+,-).
resolve(Cl1,[Cl2|Cls],NewCls) :-
	one_and_rest(Cl1,P,Pr),one_and_rest(Cl2,Q,Qr),
	(negate(P,R),negin(Q,S) ; negin(P,R),negate(Q,S)),R==S,
	( subset_literals(Pr,Qr),NewCls=[Cl1,Qr|Cls] ;
	  subset_literals(Qr,Pr),NewCls=[Pr,Cl2|Cls] ),!.
resolve(Cl1,[Cl2|Cls],[Cl2|Cls1]) :- resolve(Cl1,Cls,Cls1),!.

%
%	       i	       k
%	\/ /\ P	   ==>	/\ \/ Q
%	 i  j  j	 k  l  l
%

:- mode andor_to_clauses(+,-).
andor_to_clauses(Ors,Clauses) :-
	setof(Ands,distribution(Ors,Ands),Clauses).

:- public distribution/2.
:- mode distribution(+,-).
distribution([Ands|Ors],[L1|Literals]) :- one_of(L1,Ands),
	distribution(Ors,Literals).
distribution([],[]).

:- public negate/2.
:- mode negate(+,-).
negate(t,f).
negate(f,t).
negate([not,X],X) :- !,negin(X,_),!.
negate(X,[not,X]).

:- mode negin(+,-).
negin([not,X],X1) :- !,negate(X,X1),!.
negin(X,X).

:- public neg_all/2.
:- mode neg_all(+,-).
neg_all([T|Ts],[NT|NTs]) :- negate(T,NT),neg_all(Ts,NTs),!.
neg_all([],[]).

:- public negin_all/2.
:- mode negin_all(+,-).
negin_all([T|Ts],[NT|NTs]) :- negin(T,NT),negin_all(Ts,NTs),!.
negin_all([],[]).

:- public neg_alls/2.
:- mode neg_alls(+,-).
neg_alls([T|Ts],[NT|NTs]) :- neg_all(T,NT),neg_alls(Ts,NTs),!.
neg_alls([],[]).

:- public clause_member/2.
:- mode clause_member(+,+).
clause_member(A,[B|_]) :- equ_clause(A,B),!.
clause_member(A,[_|B]) :- clause_member(A,B),!.

:- public equ_clause/2.
:- mode equ_clause(+,+).
equ_clause([A1|A],B) :- !,delete_literal(B,A1,B1),B\==B1,equ_clause(A,B1),!.
equ_clause(X,X).

:- public delete_literal/3.
:- mode delete_literal(+,+,-).
delete_literal([A1|A],B,C) :- A1==B,!,delete_literal(A,B,C),!.
delete_literal([A1|A],B,[A1|C]) :- delete_literal(A,B,C),!.
delete_literal([],_,[]).

:- public subset_literals/2.
:- mode subset_literals(+,+).
subset_literals([A1|A],B) :- member(A1,B),subset_literals(A,B),!.
subset_literals([],_).

% EOF clause.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- mode	definition_(+).
definition_(FA/J=B) :-
	assert(jtuples(J)), definition_(FA=B), abolish(jtuples,1),!.
definition_(FA:T=B) :-
	assert(tp(T)), definition_(FA=B), abolish(tp,1),!.
definition_(_=undefined).
definition_(FA=B) :-
	FA=..[F|Formals],length(Formals,N),mtos(B,Body),
	nl,pp_def(FA,Body),
	nl,write('Analyzing...'),nl,
	definition_principle_A([F|Formals]),
	definition_principle_B(Formals),
	definition_principle_C(Formals,Body),
	definition_principle_D(F/N,Formals,Body),
	p_vars(Formals,Body,P_Formals,P_Body),
	((tp(_),assert(type_prescription([F|P_Formals],tp(8'377777,[]))));
	 compute_TP([F|Formals],P_Formals,Body)),
	assertz(definition([F|P_Formals],P_Body)),
	((machine([F|Formals],_),
	  nl,write('    ... terminative recursive function'),nl,
	  assertz(recursive([F|P_Formals])));
	 assertz(nonrecursive([F|P_Formals]))),
	abolish(machine,2),
	nl,pp_TP([F|Formals]),nl,
	nl,pp_def(FA,Body),
	nl,write('    has been accepted.'),nl,!.
%definition_(_) :- nl,write('The definition is rejected.'),nl,!.
definition_(_) :- nl,write('\arejected.'),nl,!.

%
%	The Definition Principle
%

:- mode	definition_principle_A(+).
definition_principle_A(FA) :-
	definition(FA,Body),stom(FA,FAm),
	error('The function has been already defined as:'),
	nl,pp_def(FAm,Body),nl,!,fail.
definition_principle_A(X) :-
	(constructor(X),error('Name confliction with the constructor.'));
	(recognizer(X),error('Name confliction with the recognizer.'));
	(accessor(X),error('Name confliction with the accessor.')),!,fail.
definition_principle_A(_).

:- mode	definition_principle_B(+).
definition_principle_B([F|Formals]) :- \+member(F,Formals),
	definition_principle_B(Formals),!.
definition_principle_B([]).
definition_principle_B(_) :-
	error('Formal variables should be distinct.'),!,fail.

:- mode	definition_principle_C(+,+).
definition_principle_C(Formals,Body) :- legal_body(Formals,Body),!.
definition_principle_C(_,_) :- error('Illegal definition body.'),!,fail.

:- mode	legal_body(+,+).
legal_body(_,t).
legal_body(_,f).
legal_body(_,X) :- integer(X),!.
legal_body(_,[pack,_]).
legal_body(_,[list,_]).
legal_body(_,[$,_]).
legal_body(_,Body) :- bottom_object(Body),!.
legal_body(Formals,Body) :- atomic(Body),!,
	(member(Body,Formals); constructor(Body)),!.
legal_body(Formals,[_|Args]) :- legal_body_args(Formals,Args),!.

:- mode legal_body_args(+,+).
legal_body_args(Formals,[A1|Args]) :- legal_body(Formals,A1),
	legal_body_args(Formals,Args),!.
legal_body_args(_,[]).

:- mode	definition_principle_D(+,+,+).
definition_principle_D(F/N,Formals,Body) :-
	machine_([F/N|Formals],[],[],Body),
%listing(machine),
	((\+machine([F|Formals],_),true);
         analysis_for_induction([F|Formals])),!.
definition_principle_D(_,_,_) :-
	error('Termination cannot be certified.'),!,fail.

%
%	Machine
%

:- mode machine_(+,+,+,+).
machine_(_,_,_,[$,_]).
machine_([F/N|Formals],C,G,[if,T,L,R]) :- negate(T,NT),
	(((recursive_call(F/N,T,_);
	   (setof(LC,recursive_call(F/N,L,LC),LCs),
	    setof(RC,recursive_call(F/N,R,RC),RCs),
	    union(LCs,RCs,U),
	    (U=LCs; U=RCs))),
	  machine_([F/N|Formals],C,G,T),
	  machine_([F/N|Formals],C,[T|G],L),
	  machine_([F/N|Formals],C,[NT|G],R));
	 (machine_([F/N|Formals],[T|C],[T|G],L),
	  machine_([F/N|Formals],[NT|C],[NT|G],R))),!.
machine_([F/N|Formals],C,G,T) :-
	setof(X, (recursive_call(F/N,T,X),
		  machine__([F|Formals],C,G,X)),_),!.
machine_(_,_,_,_).

:- public machine__/4.
:- mode machine__(+,+,+,+).
machine__([F|Formals],C,G,X) :-
	((C=[],CC=t); C=[CC]; (reverse(C,RC),CC=[and|RC])),!,
	((G=[],GG=t); G=[GG]; (reverse(G,RG),GG=[and|RG])),!,
	(machine([F|Formals],CC:(GG->X));
	 (p_vars(Formals,CC:(GG->X),P_Formals,P_X),
	  assertz(machine([F|P_Formals],P_X)))),!.

%
%	Definition Time Analysis for Induction
%

:- mode analysis_for_induction(+).
analysis_for_induction([F|Formals]) :-
	nastiness_check([F|Formals]),
	abolish(note,7),
	setof(Case,machine([F|Formals],Case),Cases),
	look_at_induction_lemmas([F|Formals]),
%listing(note),
	find_lexicographic_relations([F|Formals],Cases),
%listing(lexrel),
	setof(MS,R^(lexrel(R,MS), measured_subset_([F|Formals],MS)),
%	      MSs),
	      _),
%listing(measured_subset),
	make_templates([F|Formals],Cases),
%listing(induction_template),
	abolish(note,7),
	abolish(lexrel,2),!.

:- mode measured_subset_(+,+).
measured_subset_([F|Formals],MS) :-
	(measured_subset([F|Formals],MS);
	 (p_vars(Formals,MS,P_Formals,P_MS),
	  assertz(measured_subset([F|P_Formals],P_MS)))),!.

%
%	Nastiness of a Function
%

:- mode nastiness_check(+).
nastiness_check([F|Formals]) :- machine([F|Formals],_:(_->[F|Args])),
	\+primitive_recursive(Formals,Args),
	p_vars(Formals,[],P_Formals,_),
	assertz(nasty_function([F|P_Formals])),!.
nastiness_check(_).

:- public primitive_recursive/2.
:- mode primitive_recursive(+,+).
primitive_recursive([V|Vs],[V|As]) :- primitive_recursive(Vs,As).
primitive_recursive([_|Vs],[A|As]) :- accessor(A), primitive_recursive(Vs,As).
primitive_recursive([],[]).

%
%	Looking at Induction Lemmas
%

:- mode look_at_induction_lemmas(+).
look_at_induction_lemmas([F|Formals]) :-
	setof(J_tuple,j_tuple(Formals,J_tuple),J_tuples),
%nl,write(J_tuples),nl,
	setof(Case,machine([F|Formals],Case),Cases),
%nl,write(Cases),nl,
	lail(J_tuples,Cases,Formals),!.

:- mode lail(+,+,+).
lail([J|Js],C,F) :- lail1(J,C,F), lail(Js,C,F),!.
lail([],_,_).

:- mode lail1(+,+,+).
lail1(J,[C|Cs],F) :- lail2(J,C,F), lail1(J,Cs,F),!.
lail1(_,[],_).

:- mode lail2(+,+,+).
lail2(J,C,F) :- setof([WFR,M],not_increasing(C,F,J,WFR,M),_),!.
lail2(_,_,_).

:- public j_tuple/2.
:- mode j_tuple(+,-).
j_tuple(_,J) :- jtuples(Js),!,one_of(J,Js).
j_tuple(X,Z) :- j1(X,Y),j2(X,Y,Z),Z\==[].

:- mode j1(+,-).
j1([_|X],Y) :- j1(X,Y).
j1(X,X).

:- mode j2(+,+,-).
j2(X,[_|Ys],[Z|Zs]) :- j3(X,Z), j2(X,Ys,Zs), j4(Z,Zs).
j2(_,[],[]).

:- mode j3(+,-).
j3([X|_],X).
j3([_|X],Y) :- j3(X,Y).

:- mode j4(+,+).
j4(X,[Y|_]) :- X==Y,!,fail.
j4(X,[_|Z]) :- j4(X,Z).
j4(_,[]).

:- public not_increasing/5.
:- mode not_increasing(+,+,+,-,-).
not_increasing(Case:(Test->[_|Actuals]),Formals,J_tuple,WFR,M) :-
	subst_j_tuple(J_tuple,Formals,Actuals,S_tuple),!,
	transitive_rel(S_tuple,J_tuple,Hyp,WFR,M,Name),
	or_forms(Hyp,OH),
	prove_hyp(Test,OH),
%nl,write(note(J_tuple,Case:(Test->Actuals),S_tuple,Hyp,WFR,M,Name)),nl,
	assertz(note(J_tuple,Case:(Test->Actuals),S_tuple,Hyp,WFR,M,Name)).

:- mode subst_j_tuple(+,+,+,-).
subst_j_tuple([],_,_,[]).
subst_j_tuple([J1|J_tuple],Formals,Actuals,[S1|S_tuple]) :-
	sjt(J1,Formals,Actuals,S1),
	subst_j_tuple(J_tuple,Formals,Actuals,S_tuple).

:- mode sjt(+,+,+,-).
sjt(J,[F|_],[A|_],A) :- J==F.
sjt(J,[_|F],[_|A],S) :- sjt(J,F,A,S).

:- mode prove_hyp(+,+).
prove_hyp(_,P) :- has_free_var(P),!,fail.
prove_hyp(Test,[H|Hs]) :- jtuples(_),!,
	nl,tab(8),pp(8,[implies,Test,H]),nl,
	prove_hyp(Test,Hs).
prove_hyp(Test,[H|Hs]) :-
%	asserta(dumbly),
	prove_short(['induction lemma hypothesis']:[implies,Test,H]),!,
%	abolish(dumbly,0),
	prove_hyp(Test,Hs).
prove_hyp(Test,[H|Hs]) :-
	nl,write('The conjecture'),nl,
	nl,tab(8),pp(8,[implies,Test,H]),nl,
	nl,write('is provable? (y./n.) '),ttyflush,read(Ans),
	( (Ans=y, prove_hyp(Test,Hs)); !,fail).
prove_hyp(_,[]).
prove_hyp(_,_) :- abolish(dumbly,0),!,fail.

%
%	Transitive Closure of a Well Founded Relation
%

:- mode transitive_rel(+,+,-,-,-,-).
transitive_rel(S_tuple,S_tuple,[],equal,(@),(@)).
transitive_rel(S_tuple,J_tuple,Hyp,WFR,M,Name) :-
	induction_lemma([WFR1,[M1|S_tuple],[M1|M_tuple]],Hyp1,_,Name1),
	((M_tuple=J_tuple, Hyp=Hyp1, WFR=WFR1, M=M1, Name=[Name1]);
	 (transitive_rel(M_tuple,J_tuple,Hyp2,WFR2,M2,Name2),
	  append(Hyp1,Hyp2,Hyp),
	  ((WFR1=equal,WFR=WFR2);(WFR2=equal,WFR=WFR1);(WFR1=WFR2,WFR=WFR1)),
	  M1=M2, M=M1,
	  Name=[Name2|Name1])).

%
%	Lexicographic Relations
%

:- mode find_lexicographic_relations(+,+).
find_lexicographic_relations([F|Formals],Calls) :-
	setof(wfr(R,M,J_tuple),wfr_(J_tuple,R,M),WFRs),
%nl,write(WFRs),nl,
	setof(lexrel(LexRel,MS),
		(cover([F|Formals],Calls,WFRs,LexRel,MS),
		 assertz(lexrel(LexRel,MS))),
%		LexRels),!.
		_),!.

:- public wfr_/3.
wfr_(J_tuple,R,M) :- note(J_tuple,_,_,_,R,M,_), R\==equal.

:- public cover/6.
:- mode cover(+,+,+,+,-,-).
cover([F|Formals],Calls,WFRs,[R1|LexRel],MS) :-
	one_of_rels(R1,WFRs,Rs),
	not_decreasing(Calls,R1,Cs),
	Calls \== Cs,
	R1=wfr(_,_,J_tuple),
	cover([F|Formals],Cs,Rs,LexRel,MS1),
	merge_MS(Formals,J_tuple,MS1,MS).
cover(_,[],_,[],[]).

:- mode one_of_rels(-,+,-).
one_of_rels(X,[X|Y],Y).
one_of_rels(X,[W|Z],[W|Y]) :- one_of_rels(X,Z,Y).

:- mode not_decreasing(+,+,-).
%not_decreasing([Case:(Test->[F|Actuals])|Calls],wfr(R,M,J_tuple),Cs) :-
not_decreasing([Case:(Test->[_|Actuals])|Calls],wfr(R,M,J_tuple),Cs) :-
	note(J_tuple,Case:(Test->Actuals),_,_,R,M,_),
	not_decreasing(Calls,wfr(R,M,J_tuple),Cs),!.
not_decreasing([Case:(Test->[F|Actuals])|Calls],wfr(R,M,J_tuple),
		[Case:(Test->[F|Actuals])|Cs]) :-
	(note(J_tuple,Case:(Test->Actuals),_,_,equal,M,_);
	 note(J_tuple,Case:(Test->Actuals),_,_,equal,(@),(@))),
	not_decreasing(Calls,wfr(R,M,J_tuple),Cs),!.
not_decreasing([],_,[]).

:- mode merge_MS(+,+,+,-).
merge_MS([X|Xs],Y,Z,[X|Q]) :- (member(X,Y); member(X,Z)), merge_MS(Xs,Y,Z,Q),!.
merge_MS([_|Xs],Y,Z,Q) :- merge_MS(Xs,Y,Z,Q),!.
merge_MS([],_,_,[]).

%
%	Composing Induction Templates
%

:- mode make_templates(+,+).
%make_templates([F|Formals],Calls) :-
make_templates([F|Formals],_) :-
	setof(Case,TC^machine([F|Formals],Case:TC),Cases),
	setof((MS,RM,P_Formals,P_Template),
		(measured_subset([F|Formals],MS),
		 make_template1(Cases,[F|Formals],MS,RM),
		 p_vars(Formals,template(Formals/MS,RM),
			P_Formals,P_Template),
		 assertz(induction_template([F|P_Formals],P_Template))),
	_).

:- public make_template1/4.
:- mode make_template1(+,+,+,-).
make_template1([Case|Cases],[F|Formals],MS,RMout) :-
	lexrel(LexRel,MS),
	make_template([Case|Cases],[F|Formals],LexRel,MS,[],RMout),!.

:- mode make_template(+,+,+,+,+,-).
make_template([Case|Cases],[F|Formals],LexRel,MS,RMin,RMout) :-
	setof(Call,Test^machine([F|Formals],Case:(Test->Call)),Calls),
	mt(Case:Calls,LexRel,Formals,MS,[],Hyp,Substs),
	cleanup(Hyp,Hyp1),
	merge_RM_case(Hyp1,Substs,RMin,RM1),
	make_template(Cases,[F|Formals],LexRel,MS,RM1,RMout),!.
make_template([],_,_,_,RM,RM).

:- mode merge_RM_case(+,+,+,-).
merge_RM_case(Hyp,Substs,RMin,RM1) :-
	merge_RM_case1(Hyp,Substs,RMin,RM1),!.
merge_RM_case(Hyp,Substs,RMin,[(Hyp->Substs)|RMin]).

:- mode merge_RM_case1(+,+,+,-).
merge_RM_case1(Hyp,Substs,[(H1->S1)|RMin],[(H1->S2)|RMin]) :-
	set_equalp(Hyp,H1),append(Substs,S1,S2),!.
merge_RM_case1(Hyp,Substs,[(H1->S1)|RMin],[(H1->S1)|RMout]) :-
	merge_RM_case(Hyp,Substs,RMin,RMout),!.

:- mode mt(+,+,+,+,+,-,-).
mt(Case:[[_|Actuals]|Calls],LexRel,Formals,MS,Hin,Hout,[Actuals/S_tuple|S]) :-
	one_of(wfr(_,M,J_tuple),LexRel),
	note(J_tuple,Case:(_->Actuals),_,Hyp,_,M,_),
	append(Hyp,Hin,H1),
	subst_j_tuple(MS,Formals,Actuals,S_tuple),
	mt(Case:Calls,LexRel,Formals,MS,H1,Hout,S).
mt(_:[],_,_,_,H,H,[]).

%
%	Computing Definition Type Set
%

:- mode definition_type_set(+,-,+).

definition_type_set(X,DTS,Ass) :- dts_assumed(X,DTS,Ass),!.

:- mode dts_assumed(+,-,+).
dts_assumed(X,DTS,[X=DTS|_]).
dts_assumed(X,DTS,[_|Ass]) :- dts_assumed(X,DTS,Ass),!.

definition_type_set([equal,_,_],dts(3,[]),_) :- !.
definition_type_set(X,dts(TS,[]),_) :-
	(recognizer_constructor([R,X]);
	 recognizer_bottom([R,X]);
	 type_of_A([R,X]);
	 R=[X];
	 (rewrite_lemma([R,X],t,P,no,_,_),
	  establish(P,[[],[],[],0,tf],[],_,[],_,[],_))),
	bit_place(R,TS),!.	

definition_type_set(X,dts(TS,[]),_) :-
	rewrite_lemma([or,[R1,X]|Y],t,P,no,_,_),
	establish(P,[[],[],[],0,tf],[],_,[],_,[],_),
	tsa([[R1,X]|Y],0,X,TS),!.

definition_type_set([if,T,L,R],DTS,Ass) :- !,
	dts_assumption(T,Mustbe,Ass),!,
	((Mustbe=t,!,definition_type_set(L,DTS,Ass));
	 (Mustbe=f,!,definition_type_set(R,DTS,Ass));
	 (Mustbe=or(L_Ass,R_Ass),!,
	  definition_type_set(L,LDTS,L_Ass),!,LDTS=dts(LTS,LVars),!,
	  definition_type_set(R,RDTS,R_Ass),!,RDTS=dts(RTS,RVars),!,
	  ts_union(LTS,RTS,TS),!,
	  union(LVars,RVars,Vars),!,
	  DTS=dts(TS,Vars))),!.

definition_type_set(X,dts(TS,[]),Ass) :- recognizer(X),!,X=[R,Y],!,
	(bit_place(R,RTS);
	 (recognizer_constructor([R,C]),bit_place([C],RTS))),!,
	definition_type_set(Y,YDTS,Ass),!,
	(YDTS=dts(YTS,[]); YTS=8'377777),!,
	((RTS=YTS,!,TS=2);
	 (ts_intersection(RTS,YTS,0),!,TS=1);
	 TS=3),!.
%			[implies,[R,X],[not,[R',X]]]

definition_type_set([F|Args],dts(TS,Vars),Ass) :-
	type_prescription([F|Args],tp(TS_F,Terms)),!,
	dts_args(Terms,TS_F,[],TS,Vars,Ass),!.

	:- mode dts_args(+,+,+,-,-,+).
	dts_args([T1|Terms],TSp,Varsp,TS,Vars,Ass) :-
		definition_type_set(T1,DTS1,Ass),!,DTS1=dts(TS1,Vars1),!,
		ts_union(TSp,TS1,TSn),!,
		union(Varsp,Vars1,Varsn),!,
		dts_args(Terms,TSn,Varsn,TS,Vars,Ass),!.
	dts_args([],TS,Vars,TS,Vars,_).

definition_type_set(X,dts(0,[X]),_) :- variable(X),!.
definition_type_set(_,dts(8'377777,[]),_).

%
%	Assuming Expressions true or false
%

:- mode dts_assumption(+,-,+).

dts_assumption([not,X],A,Ass) :- !,dts_assumption(X,B,Ass),!,
	((B=t,!,A=f);
	 (B=f,!,A=t);
	 (B=or(L,R),!,A=or(R,L))),!.

dts_assumption([equal,T1,T2],Mustbe,Ass) :-
	definition_type_set(T1,DTS1,Ass),!,DTS1=dts(S1,V1),!,
	definition_type_set(T2,DTS2,Ass),!,DTS2=dts(S2,V2),!,
	((V1=[],SV1=S1); SV1=8'377777),!,
	((V2=[],SV2=S2); SV2=8'377777),!,
	ts_intersection(SV1,SV2,S),!,
	((S=0,!,Mustbe=f);
	 (SV1=SV2,bit_place([_],S),!,Mustbe=t);
	 (add_ts_assumption([[equal,T1,T2]=dts(2,[]),[equal,T2,T1]=dts(2,[]),
			     T1=dts(S,[]),T2=dts(S,[])],
			Ass,L_Ass),!,
	  ((SV1=S,bit_place([_],S),!,
	    ts_difference(SV2,SV1,S21),!,
	    add_ts_assumption([[equal,T1,T2]=dts(1,[]),[equal,T2,T1]=dts(1,[]),
			       T2=dts(S21,[])],
			Ass,R_Ass)) ;
	   (SV2=S,bit_place([_],S),!,
	    ts_difference(SV1,SV2,S12),!,
	    add_ts_assumption([[equal,T1,T2]=dts(1,[]),[equal,T2,T1]=dts(1,[]),
			       T1=dts(S12,[])],
			Ass,R_Ass)) ;
	   add_ts_assumption([[equal,T1,T2]=dts(1,[]),[equal,T2,T1]=dts(1,[])],
			Ass,R_Ass)),!,
	  Mustbe=or(L_Ass,R_Ass))),!.

dts_assumption(X,Mustbe,Ass) :- recognizer(X),!,X=[R,Y],!,
	(bit_place(R,RTS);
	 (recognizer_constructor([R,C]), bit_place([C],RTS))),!,
	definition_type_set(Y,DTS,Ass),!,
	(DTS=dts(YTS,[]); YTS=8'377777),!,
	((YTS=RTS,!,Mustbe=t);
	 (ts_intersection(YTS,RTS,0),!,Mustbe=f);
	 (ts_difference(YTS,RTS,Sd),!,
	  add_ts_assumption([Y=dts(RTS,[])],Ass,L_Ass),!,
	  add_ts_assumption([Y=dts(Sd,[])],Ass,R_Ass),!,
	  Mustbe=or(L_Ass,R_Ass))),!.

dts_assumption(X,Mustbe,Ass) :-
	definition_type_set(X,DTS,Ass),!,DTS=dts(TS,V),!,
	((TS=1,V=[],!,Mustbe=f);
	 (ts_intersection(TS,1,0),V=[],!,Mustbe=t);
	 (ts_difference(TS,1,Sd),!,
	  add_ts_assumption([X=dts(Sd,V)],Ass,L_Ass),!,
	  add_ts_assumption([X=dts(1,[])],Ass,R_Ass),!,
	  Mustbe=or(L_Ass,R_Ass))),!.

%
%	Computing Type Prescription
%

:- mode compute_TP(+,+,+).
compute_TP([F|Formals],P_Formals,Body) :-
	asserta(type_prescription([F|P_Formals],tp(0,[]))),
	expand_nonrecursive_test(Body,B1), if_expr(B1,B2), if_norms(B2,B3),
	if_norms2(B3,B4),
	compute_TP([F|Formals],B4),!.

:- mode if_norms2(+,-).
if_norms2([if,T,L,R],[if,TT,LL,RR]) :-
	if_norms2(T,T1),
	((T1=[if,TT,TL,TR],
	  if_norms2([if,TL,L,R],LL),
	  if_norms2([if,TR,L,R],RR));
	 (T1=TT,
	  if_norms2(L,LL),
	  if_norms2(R,RR))),!.
if_norms2(X,X).

:- mode expand_nonrecursive_test(+,-).
expand_nonrecursive_test([if,T,L,R],[if,TT,LL,RR]) :-
	expand_nonrecursive(T,TT),
	expand_nonrecursive_test(L,LL),
	expand_nonrecursive_test(R,RR),!.
expand_nonrecursive_test(X,X).

:- mode expand_nonrecursive(+,-).
expand_nonrecursive(X,Z) :- nonrecursive(X), definition(X,Y),
	expand_nonrecursive(Y,Z),!.
expand_nonrecursive([F|Args],[F|A]) :- expand_nonrecursive_args(Args,A),!.
expand_nonrecursive(X,X).

:- mode expand_nonrecursive_args(+,-).
expand_nonrecursive_args([X|Xs],[Y|Ys]) :-
	expand_nonrecursive(X,Y), expand_nonrecursive_args(Xs,Ys),!.
expand_nonrecursive_args([],[]).

:- mode compute_TP(+,+).
compute_TP([F|Formals],Body) :-
	definition_type_set(Body,dts(TS,Vars),[]),
	(type_prescription([F|Formals],tp(TS,Vars));
	 (p_vars(Formals,Vars,P_Formals,P_Vars),
	  retract(type_prescription([F|Formals],tp(_,_))),
	  asserta(type_prescription([F|P_Formals],tp(TS,P_Vars))),
	  compute_TP([F|Formals],Body))),!.

% EOF defun.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public elim1/1.
:- mode elim1(+).
elim1([ID|Path]:Clause) :- !,
%	nl,write('Eliminating Destructors...'),nl,!,
%	elimination_lemma(_,LemmaName,[implies,Hyp,[equal,[C|Ds],X]]),
	elimination_lemma(_,_,[implies,Hyp,[equal,[C|Ds],X]]),
	call_in_terms([D|Args],Clause),
	distinct_variables(Args),
	member_destructor_([D|Args],Ds),
	setof(T,destructor_term_(T,Ds),DTs),
	(elim1_no_loop(Path,Args); dt_in_not_eq(DTs,Clause)),
	assume_false(Clause,[],_,_,_,[],_,[],Context),
	negate(Hyp,NHyp),
	perform_generalization([NHyp|Clause],Context,DTs,Vars,C2),
	gen_subst(Ds,DTs,Vars,Ds1),
	apply_subst(C2,[(X->[C|Ds1])],C3),
	map_elim1_simp(C3,Cls2),
	expl_elim1(X,[C|Ds1],DTs),!,
	formula_to_clauses([or,Hyp|Clause],Cls1),
%	( dumbly; (nl,write('Generalization Base Case:'),nl)),
	pool_(simplification,[c(Vars,ID)|Path]:Cls1,z),
%	( dumbly; (nl,write('Generalization Case:'),nl)),
	pool_(simplification,[d(Vars,ID)|Path]:Cls2,z),!.

:- mode member_destructor(+,+).
member_destructor(X,X).
member_destructor(X,[_|A]) :- member_destructor_(X,A).
:- mode member_destructor_(+,+).
member_destructor_(X,[Y|_]) :- nonvar(Y),member_destructor(X,Y).
member_destructor_(X,[_|Y]) :- member_destructor_(X,Y).

:- mode destructor_term(-,+).
destructor_term([F|A],[F|A]) :- distinct_variables(A).
destructor_term(X,[_|A]) :- destructor_term_(X,A).
:- public destructor_term_/2.
:- mode destructor_term_(-,+).
destructor_term_(X,[Y|_]) :- nonvar(Y),destructor_term(X,Y).
destructor_term_(X,[_|Y]) :- destructor_term_(X,Y).

:- mode map_elim1_simp(+,-).
map_elim1_simp([C|Cs],[NC|NCs]) :- elim1_simp(C,NC),map_elim1_simp(Cs,NCs),!.
map_elim1_simp([],[]).

:- mode elim1_simp(+,-).
elim1_simp([[not,X]|C],C1) :- recognizer_constructor(X), elim1_simp(C,C1),!.
elim1_simp([X|C],C1) :- member(X,C), elim1_simp(C,C1),!.
elim1_simp([X|C],[X|C1]) :- elim1_simp(C,C1),!.
elim1_simp([],[]).

:- mode elim1_no_loop(+,+).
elim1_no_loop([d(Vars,_)-_|_],Args) :- one_of(V,Vars), member(V,Args),!,fail.
elim1_no_loop([d(Vars,_)|_],Args) :- one_of(V,Vars), member(V,Args),!,fail.
elim1_no_loop([b(_)-_|_],_).
elim1_no_loop([b(_)|_],_).
elim1_no_loop([i(_)-_|_],_).
elim1_no_loop([i(_)|_],_).
elim1_no_loop([_|Path],V) :- !,elim1_no_loop(Path,V),!.
elim1_no_loop(_,_).

:- mode dt_in_not_eq(+,+).
dt_in_not_eq([DT|_],Cl) :- dt_in_not_eq1(DT,Cl),!.
dt_in_not_eq([_|DTs],Cl) :- dt_in_not_eq(DTs,Cl),!.

:- mode dt_in_not_eq1(+,+).
dt_in_not_eq1(DT,[[not,[equal,L,R]]|_]) :-
	((explicit_value(R), call_in_term(DT,L));
	 (explicit_value(L), call_in_term(DT,R))),!.
dt_in_not_eq1(DT,[_|Cl]) :- dt_in_not_eq1(DT,Cl),!.

:- mode expl_elim1(+,+,+).
expl_elim1(_,_,_) :- dumbly,!.
expl_elim1(X,LHS,Destructors) :-
	nl,wwrite('We replace '),wwrite(X,reverse),wwrite(' by '),
	name(X,XL),length(XL,XN),I is XN+19,pp(I,LHS),nl,
	nl,wwrite('    to eliminate '),
	((Destructors=[D],!,pp(13,D));
	 (pp(13,[''|Destructors]))),nl,!.

% EOF elim1.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public elim2/1.
:- mode elim2(+).
elim2([ID|Path]:Clause) :- !,
%	nl,wwrite('Eliminating Irrelevance...'),nl,!,
	abolish(part,2),!,
	partitioning(Clause),!,
	((setof(L,P^(falsifiable_part(P),one_of(L,P)),DL),
	  abolish(part,2),
	  delete_DL(Clause,DL,Cl),
	  Cl\==[],
	  expl_elim2(DL),
	  pool_(simplification,[l(ID)|Path]:[Cl],a),!);
	 (abolish(part,2),!,fail)).

:- mode partitioning(+).
partitioning([L|Ls]) :-
	(setof(V,var_in_term(V,L),Vs); Vs=[]),!,
	sharing_check([L],Vs),!,
	partitioning(Ls),!.
partitioning([]).

sharing_check(Ls1,Vs1) :-
	( (part(Ls2,Vs2),
	   sharing(Vs1,Vs2),
	   setof(V, (one_of(V,Vs1); one_of(V,Vs2)), Vs3),
	   retract(part(Ls2,Vs2)),
	   append(Ls1,Ls2,Ls3),
	   sharing_check(Ls3,Vs3)) ;
	  assertz(part(Ls1,Vs1)) ),!.

:- mode sharing(+,+).
sharing([V|_],[V|_]).
sharing([_|U],V) :- sharing(U,V).
sharing(U,[_|V]) :- sharing(U,V).

:- public falsifiable_part/1.
:- mode falsifiable_part(-).
falsifiable_part(Ls) :- part(Ls,_), falsifiable(Ls).

:- mode falsifiable(+).
falsifiable([L]) :- (L=[not,X]; L=X), recursive(X),!,X=[_|Args],!,
	distinct_variables(Args),!.
falsifiable(X) :- falsifiable1(X),!.

:- mode falsifiable1(+).
falsifiable1([X|Ls]) :- not_mention_recursive_function(X),!,falsifiable1(Ls),!.
falsifiable1([]).

:- mode not_mention_recursive_function(+).
not_mention_recursive_function(X) :- recursive(X),!,fail.
not_mention_recursive_function([_|Args]) :- !,
	not_mention_recursive_functions(Args),!.
not_mention_recursive_function(_).

:- mode not_mention_recursive_functions(+).
not_mention_recursive_functions([X|Xs]) :-
	not_mention_recursive_function(X),!,
	not_mention_recursive_functions(Xs),!.
not_mention_recursive_functions([]).

:- mode delete_DL(+,+,-).
delete_DL([L|Ls],D,Ls1) :- delete_DL(D,L),!,delete_DL(Ls,D,Ls1),!.
delete_DL([L|Ls],D,[L|Ls1]) :- delete_DL(Ls,D,Ls1),!.
delete_DL([],_,[]).

:- mode delete_DL(+,+).
delete_DL([X|_],X).
delete_DL([_|X],Y) :- delete_DL(X,Y),!.

:- mode expl_elim2(+).
expl_elim2(_) :- dumbly,!.
expl_elim2(Del) :-
	nl,wwrite('We eliminate the irrelevant term(s)'),nl,
	neg_all(Del,ND),
	nl,tab(8),((ND=[D],pp(8,D)); pp(8,[''|ND])),nl,!.

% EOF elim2.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public equ/1.
:- mode equ(+).
equ([ID|Path]:Clause) :-
%	nl,wwrite('Using Equalities...'),nl,!,
	((one_of([not,[equal,S,T]],Clause),
	  cross_fertilize(Path:Clause,S,T,Cls));
	 (one_of([not,[equal,S,T]],Clause),
	  uniform_subst(Clause,S,T,Cls))),
	pool_(simplification,[e(ID)|Path]:[Cls],a),!.

:- mode cross_fertilize(+,+,+,-).
cross_fertilize(Path:Clause,S,T,Cls) :-
	induction_step(Path),!,
	setof([Sid,X,C],cross_fertilize0(Clause,S,T,Sid,X,C),L),
	decide_side(L,Side,Cls),
	expl_fertilize(S,T,Side),!.

:- mode decide_side(+,-,-).
decide_side([[Side,_,Cls]],Side,Cls).
decide_side([[Sid1,A,Cls1],[Sid2,B,Cls2]],Side,Cls) :-
	((less_complex([A-B],0,0), Side=Sid1, Cls=Cls1);
	 (Side=Sid2, Cls=Cls2)).

:- mode induction_step(+).
induction_step([b(_)-_|_]) :-!,fail.
induction_step([b(_)|_]) :-!,fail.
induction_step([i(_)-_|_]).
induction_step([i(_)|_]).
induction_step([_|Path]) :- induction_step(Path).

:- public cross_fertilize0/6.
:- mode cross_fertilize0(+,+,+,-,-,-).
cross_fertilize0(Clause,S,T,Sid,X,C) :-
	cross_fertilize1(Clause,S,T,Sid,C),
	((Sid=right,\+explicit_value(S),\+explicit_value_template(T),X=S);
	 (Sid=left, \+explicit_value(T),\+explicit_value_template(S),X=T)).

:- mode cross_fertilize1(+,+,+,?,-).
cross_fertilize1([[not,[equal|U]]|Ls],S,T,Side,Ls2) :- !,
	cross_fertilize1(Ls,S,T,Side,Ls1),
	((U=[S,T], Ls2=Ls1);
	 (U\==[S,T],
	  ((Side == right,
	    substitute_term_nonfail(S,T,[equal|U],U1)) ;
	   (Side == left,
	    substitute_term_nonfail(T,S,[equal|U],U1))),
	  Ls2=[[not,U1]|Ls1])).
cross_fertilize1([[equal,U,V]|Ls],S,T,Side,[[equal,U1,V1]|Ls1]) :- !,
	((substitute_term(S,T,V,V1), U1=U, Side=right);
	 (substitute_term(T,S,U,U1), V1=V, Side=left);
	 (U1=U, V1=V)),
	cross_fertilize1(Ls,S,T,Side,Ls1).
cross_fertilize1([L|L1],S,T,Side,[L|L2]) :-
	cross_fertilize1(L1,S,T,Side,L2).
cross_fertilize1([],_,_,Side,[]) :- nonvar(Side).

:- mode substitute_term(+,+,+,-).
substitute_term(New,Old,Old,New).
substitute_term(New,Old,[F|Old_args],[F|New_args]) :-
	substitute_terms(New,Old,Old_args,New_args),!,
	Old_args\==New_args,!.

:- mode substitute_terms(+,+,+,-).
substitute_terms(New,Old,[O1|O],[N1|N]) :-
	substitute_term_nonfail(New,Old,O1,N1),
	substitute_terms(New,Old,O,N),!.
substitute_terms(_,_,[],[]).

:- mode substitute_term_nonfail(+,+,+,-).
substitute_term_nonfail(New,Old,Old,New).
substitute_term_nonfail(New,Old,[F|Old_args],[F|New_args]) :-
	substitute_terms(New,Old,Old_args,New_args),!.
substitute_term_nonfail(_,_,X,X).

:- mode uniform_subst(+,+,+,-).
uniform_subst(Ls,S,T,Ls1) :-
	((\+explicit_value_template(T),Subst=(S->T),
	  uniform_subst1(Ls,Subst,Ls1));
	 (\+explicit_value_template(S),Subst=(T->S),
	  uniform_subst1(Ls,Subst,Ls1))),
	Ls\==Ls1,
	expl_uniform_subst(Subst),!.

:- mode uniform_subst1(+,+,-).
uniform_subst1([L|Ls],(S->T),[L|Ls1]) :-
	(L=[not,[equal,S,T]];L=[not,[equal,T,S]]),
	uniform_subst1(Ls,(S->T),Ls1),!.
uniform_subst1([L|Ls],(S->T),[L1|Ls1]) :-
	substitute_term(S,T,L,L1),
	uniform_subst1(Ls,(S->T),Ls1),!.
uniform_subst1([],_,[]).

:- mode expl_uniform_subst(+).
expl_uniform_subst(_) :- dumbly,!.
expl_uniform_subst((S->T)) :-
	nl,wwrite('We now use the above equality hypothesis'),
	wwrite(' by uniformly substituting'),nl,
	nl,tab(8),pp(8,S),
	nl,wwrite('    for'),
	nl,tab(8),pp(8,T),nl,!.

:- mode expl_fertilize(+,+,+).
expl_fertilize(_,_,_) :- dumbly,!.
expl_fertilize(S,T,Side) :-
	nl,wwrite('We use the above equality hypothesis'),
	wwrite(' by cross-fertilizing'),nl,
	((Side==left,SS=T,TT=S);(Side==right,SS=S,TT=T)),
	nl,tab(8),pp(8,SS),
	nl,wwrite('    for'),
	nl,tab(8),pp(8,TT),nl,
	nl,wwrite('    and throwing away the equality.'),nl,!.

% EOF equ.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- mode expand(+,+,-,+,+,-,+,-,+,-).

expand(_,[and|P],New,_,U,U,V,V,W,W) :- !,if_expr([and|P],New),!.
expand(_,[or|P],New,_,U,U,V,V,W,W) :- !,if_expr([or|P],New),!.
expand(_,[implies|P],New,_,U,U,V,V,W,W) :- !,if_expr([implies|P],New),!.

expand(_,X,New,[TS_alist,N_list,_,Lev,H],U,U1,V,V1,W,W1) :-
	(nonrecursive(X); (measured_subset(X,MS),explicit_values(MS))),
	definition(X,Body),!,expl_expand(X,Lev),!,Lev1 is Lev+1,!,
	rewrite(Body,New,[TS_alist,N_list,[],Lev1,H],
		U,U1,V,V1,[e(Lev,X)|W],W1),!.

expand(_,X,Y,_,U,U1,V,[X|V],W,W) :-
	X=[F|Args], length(Args,N), member(F/N,U),!,X=Y,!,U=U1,!.

expand(_,X,Y,I,U,U1,V,V1,W,W1) :- rewrite_val(X,Y,I,U,U1,V,V1,W,W1),!.
expand(_,X,X,[_,_,L,_],U,U,V,[X|V],W,W) :- expand_depth(D),L>D,!.

%expand(O,X,New,[TS_alist,N_list,_,Lev,H],U,U,V,V1,W,W1) :-
expand(_,X,New,[TS_alist,N_list,_,Lev,H],U,U,V,V1,W,W1) :-
	definition(X,Body), expl_expand(X,Lev),
	X=[F|Args], length(Args,N), Lev1 is Lev+1,
	rewrite(Body,New,[TS_alist,N_list,[],Lev1,H],
		[F/N|U],_,V,V1,[e(Lev,X)|W],W1),
	recursive_calls(F/N,New,Fs),
	rewrite_env(Path:Terms),
%	better(Fs,O,Path:Terms),!.
	better(Fs,X,Path:Terms),!.

expand(_,X,X,_,U,U,V,[X|V],W,W).

:- mode better(+,+,+).
better([[_|Args]|Fs],F,Path:Ts) :-
	no_new_terms(Args,Ts),!,better(Fs,F,Path:Ts),!.
better([[F|A1]|Fs],[F|A2],Path:Ts) :-
	more_explicit_values(A1,A2,0,0),!,better(Fs,[F|A2],Path:Ts),!.
better([[F|A1]|Fs],[F|A2],Path:Ts) :-
	proper_induction_step(Path),
	make_pairs(A1,A2,P), measured_subset([F|P],P1),	less_complex(P1,0,0),!,
	better(Fs,[F|A2],Path:Ts),!.
better([],_,_).

:- mode proper_induction_step(+).
proper_induction_step([i-_|_]).
proper_induction_step([i|_]).

:- mode no_new_terms(+,+).
no_new_terms([A|Args],Terms) :- (variable(A); member(A,Terms)),!,
	no_new_terms(Args,Terms),!.
no_new_terms([],_).

:- mode more_explicit_values(+,+,+,+).
more_explicit_values([A|As],[B|Bs],M,N) :-
	((explicit_value(A), M1 is M+1); M1 is M),!,
	((explicit_value(B), N1 is N+1); N1 is N),!,
	more_explicit_values(As,Bs,M1,N1),!.
more_explicit_values([],[],M,N) :- M>N,!.

:- mode make_pairs(+,+,-).
make_pairs([A|As],[B|Bs],[A-B|P]) :- make_pairs(As,Bs,P),!.
make_pairs([],[],[]).

:- mode less_complex(+,+,+).
less_complex([A-B|P],I1,I2) :- complexity(A,CA), complexity(B,CB),
	I1n is I1+CA, I2n is I2+CB, less_complex(P,I1n,I2n),!.
less_complex([],C1,C2) :- C1<C2,!.

:- mode complexity(+,-).
complexity([if,_,X,Y],C) :- complexity(X,XC), complexity(Y,YC),
	((XC>YC, C is XC); C is YC),!.
complexity([_|Args],C) :- complexity_args(Args,0,CA), C is CA+1,!.
complexity(_,0).

:- mode complexity_args(+,+,-).
complexity_args([A|Args],I,O) :- complexity(A,CA), I1 is I+CA,
	complexity_args(Args,I1,O),!.
complexity_args([],C,C).

:- mode expl_expand(+,+).
expl_expand(_,_) :- (dumbly; \+expl_expand),!.
expl_expand(X,Lev) :-
	I is Lev*3, tab(I), write('expanding '), I1 is I+10, pp(I1,X),nl,!.

% EOF expand.pl

{F} :- [F].
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public generalize/1.
:- mode generalize(+).
generalize([ID|Path]:Clause) :- !,
%	nl,wwrite('Generalizing...'),nl,!,
	induction_step(Path),
	abolish(common,1),
	setof(T,(common_term(T,Clause),
		 generalizable(T,Clause),
		 assertz(common(T))),_),
	minimal_common_subterms,
	setof(GT,common(GT),GTM),
	abolish(common,1),
	assume_false(Clause,[],_,_,_,[],_,[],Context),
	perform_generalization(Clause,Context,GTM,Vars,NCl),
	expl_gen(GTM,Vars),
	pool_(simplification,[g(Vars,ID)|Path]:NCl,a),!.

:- public common_term/2.
:- mode common_term(?,+).
common_term(T,Clause) :-
	(common_term_lit(T,Clause) ; common_term_equ(T,Clause)).

:- mode common_term_lit(?,+).
common_term_lit(T,[L1|Clause]) :- one_of(L2,Clause),
	call_in_term(T,L1), call_in_term(T,L2).
common_term_lit(T,[_|Clause]) :- common_term_lit(T,Clause).

:- mode common_term_equ(?,+).
common_term_equ(T,Clause) :-
	one_of(L,Clause), (L=[equal,A,B];L=[not,[equal,A,B]]),
	call_in_term(T,A), call_in_term(T,B).

:- public generalizable/2.
:- mode generalizable(+,+).
generalizable(X,_) :-
	\+explicit_value_template(X),
	\+X=[equal,_,_],
	\+accessor(X),!.

minimal_common_subterms :- common(T1), common(T2), T1\==T2,
	call_in_term(T1,T2), retract(common(T2)),
	minimal_common_subterms,!.
minimal_common_subterms.

:- mode perform_generalization(+,+,+,-,-).
perform_generalization(Clause,Context,GTM,Vars,C3) :-
	(setof(R,generalization_restriction(Context,GTM,R),Rs);
	 Rs=[]),
	length(GTM,N),
	gen_vars(N,Vars),
	append(Rs,Clause,C1),
	gen_subst(C1,GTM,Vars,C2),
	formula_to_clauses([or|C2],C3),!.

:- public generalization_restriction/3.
:- mode generalization_restriction(+,+,-).
generalization_restriction(Context,GTM,B) :-
	one_of(T,GTM),
	((generalization_lemma(_,_,X),
	  call_in_term(T,X),
	  B=[not,X]);
	 (type_set(T,TS,Context),
	  bit_place(R,TS),
	  B=[not,[R,T]])).

:- mode gen_subst(+,+,+,-).
gen_subst([L|Ls],Ts,Vs,[L1|Ls1]) :-
	gen_subst1(L,Ts,Vs,L1),	gen_subst(Ls,Ts,Vs,Ls1),!.
gen_subst([],_,_,[]).

:- mode gen_subst1(+,+,+,-).
gen_subst1(L,[T1|Ts],[V1|Vs],L2) :-
	gen_subst2(L,T1,V1,L1),	gen_subst1(L1,Ts,Vs,L2),!.
gen_subst1(L,[],[],L).

:- mode gen_subst2(+,+,+,-).
gen_subst2(T,T,V,V).
gen_subst2([F|Args],T,V,[F|Args1]) :- gen_subst2s(Args,T,V,Args1),!.
gen_subst2(T,_,_,T).

:- mode gen_subst2s(+,+,+,-).
gen_subst2s([A|As],T,V,[A1|As1]) :-
	gen_subst2(A,T,V,A1), gen_subst2s(As,T,V,As1),!.
gen_subst2s([],_,_,[]).

:- mode expl_gen(+,+).
expl_gen(_,_) :- dumbly,!.
expl_gen(Terms,Vars) :-
	nl,wwrite('We generalize the conjecture by replacing'),nl,
	nl,tab(8),((Terms=[T],pp(8,T)); pp(8,[''|Terms])),
	nl,wwrite('    by'),
	nl,tab(8),((Vars=[V],wwrite(V,reverse)); pp(8,[''|Vars])),nl,!.

% EOF gen.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public heuristics/0.
heuristics :- retract(pool(heuristics,X)),!,
	pp_clause(X,'Trying heuristic rewrite...'),
	heuristics(X),!.
heuristics.

:- mode heuristics(+).
heuristics(X) :- elim1(X),!.
heuristics(X) :- equ(X),!.
heuristics(X) :- generalize(X),!.
heuristics(X) :- elim2(X),!.
heuristics(P:C) :- pool_(induction,P:[C],z),!.

% EOF heuris.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public induction/0.
induction :- retract(pool(induction,X)),!,
	pp_clause(X,'Trying induction...'),!,
	induction(X),!.
induction.
	
:- mode	induction(+).
induction(Path:Clause) :-
	abolish(scheme,1), abolish(changeables,3),
	collect_schemes(Clause),
%listing(scheme),
	sift_schemes,
	perform_induction(Path:Clause),
	abolish(scheme,1), abolish(changeables,3),!.
induction(X) :-
	abolish(scheme,1), abolish(changeables,3),!,
	assertz(pool(fail,X)),!.

%
%	Collecting Induction Schemes for a Clause
%

:- mode collect_schemes(+).
collect_schemes(Clause) :-
	setof([Call,Template,UC,Cases],
		(call_in_terms(Call,Clause),
		 induction_template(Call,Template),
		 applies(Template,Call,UC),
		 delete_illegal_pairs(Template,UC,Cases),
		 make_scheme(Call,Cases,UC)),
	_),!.

%
%	applies(+Template,+Term,-Unchangeables) :-
%
%	A Template applies to a Term
%	and return Unchangeables w.r.t. the Term and the Template
%

:- public applies/3.
:- mode	applies(+,+,-).
applies(template(X/MS,Cases),Call,UC) :-
	setof(Subst,subst_in_cases(Subst,Cases),Substs),
%nl,write(Substs),nl,
	transpose([MS|Substs],S),
	scan_substs(S,C,UC),
	assertz(changeables(Call,template(X/MS,Cases),C)),
	distinct_variables(C),
	\+setof(V,X^(one_of(X,UC),var_in_term(V,X),member(V,C)),UC),!.

:- public subst_in_cases/2.
:- mode subst_in_cases(-,+).
subst_in_cases(Subst,[(_->S)|_]) :- subst_in_cases(Subst,S).
subst_in_cases(Subst,[_/Subst|_]).
subst_in_cases(Subst,[_|S]) :- subst_in_cases(Subst,S).

:- mode transpose(+,-).
transpose([[]|_],[]).
transpose(A,[AT|B]) :- cars(A,AT), cdrs(A,AD), transpose(AD,B),!.

:- mode cars(+,-).
cars([[A|_]|B],[A|C]) :- cars(B,C),!.
cars([],[]).

:- mode cdrs(+.-).
cdrs([[_|D]|B],[D|C]) :- cdrs(B,C),!.
cdrs([],[]).

:- mode scan_substs(+,-,-).
scan_substs([[X|Xs]|S],C,[X|UC]) :- unchangeable(X,Xs),
	scan_substs(S,C,UC),!.
scan_substs([[X|_]|S],[X|C],UC) :-
	scan_substs(S,C,UC),!.
scan_substs([],[],[]).

:- mode unchangeable(+,+).
unchangeable(X,[X|Xs]) :- unchangeable(X,Xs),!.
unchangeable(_,[]).

%
%	Deleting Illegal Substitution Pairs
%

:- public delete_illegal_pairs/3.
:- mode delete_illegal_pairs(+,+,-).
delete_illegal_pairs(template(Actuals/MS,[(T->S)|SS]),UC,[(T->SD)|SSD]) :-
	delete_illegal_pairs(Actuals/MS,S,UC,SD),
	delete_illegal_pairs(template(Actuals/MS,SS),UC,SSD),!.
delete_illegal_pairs(template(_,[]),_,[]).

:- mode delete_illegal_pairs(+,+,+,-).
delete_illegal_pairs(Actuals/MS,[Subst/_|S],UC,[AD1|SD]) :-
	delete_illegal_pairs1(Actuals,Subst,UC,AD),
	delete_ambiguous_pairs(AD,MS,AD1),
	delete_illegal_pairs(Actuals/MS,S,UC,SD),!.
delete_illegal_pairs(_,[],_,[]).

:- mode delete_illegal_pairs1(+,+,+,-).
delete_illegal_pairs1([A|Actuals],[S|Subst],UC,[(A->S)|AD]) :-
	variable(A), \+member(A,UC), A\==S,
	delete_illegal_pairs1(Actuals,Subst,UC,AD),!.
delete_illegal_pairs1([_|Actuals],[_|Subst],UC,AD) :-
	delete_illegal_pairs1(Actuals,Subst,UC,AD),!.
delete_illegal_pairs1([],_,_,[]).

:- mode delete_ambiguous_pairs(+,+,-).
delete_ambiguous_pairs([(X->_)|AD],MS,AD1) :-
	ambiguous(X,AD), \+member(X,MS),
	delete_ambiguous_pairs(AD,MS,AD1),!.
delete_ambiguous_pairs([S|AD],MS,[S|AD1]) :-
	delete_ambiguous_pairs(AD,MS,AD1),!.
delete_ambiguous_pairs([],_,[]).

:- mode ambiguous(+,+).
ambiguous(X,[(X->_)|_]).
ambiguous(X,[_|Y]) :- ambiguous(X,Y).

%
%	Composing an Induction Scheme
%

:- public make_scheme/3.
:- mode make_scheme(+,+,+).
%make_scheme([F|Args],Cases,UC) :-
make_scheme([F|Args],Cases,_) :-
	setof(CV,changing_variable(CV,Cases),CVs),
	(setof(UCV,(var_in_term(UCV,[F|Args]),\+member(UCV,CVs)),UCVs);
	 UCVs=[]),
	length(Args,D),
	max_subst_length(Cases,0,N),
	assertz(scheme([Cases,[[F|Args]],CVs,UCVs,N/D])),!.

:- public changing_variable/2.
:- mode changing_variable(-,+).
changing_variable(CV,[(_->S)|_]) :- changing_variable1(CV,S).
changing_variable(CV,[_|S]) :- changing_variable(CV,S).

:- mode changing_variable1(-,+).
changing_variable1(CV,[S|_]) :- changing_variable2(CV,S).
changing_variable1(CV,[_|S]) :- changing_variable1(CV,S).

:- mode changing_variable2(-,+).
changing_variable2(CV,[(CV->_)|_]).
changing_variable2(CV,[_|S]) :- changing_variable2(CV,S).

:- mode max_subst_length(+,+,-).
max_subst_length([(_->S)|HS],N,M) :- max_subst_length1(S,0,M1),
	max([N,M1],M2),
	max_subst_length(HS,M2,M),!.
max_subst_length([],M,M).

:- mode max_subst_length1(+,+,-).
max_subst_length1([S|SS],N,M) :- length(S,M1),
	max([N,M1],M2),
	max_subst_length1(SS,M2,M),!.
max_subst_length1([],M,M).

:- mode max(+,-).
max([N1|Ns],M) :- max(Ns,M1), (N1>=M1, M=N1; M=M1),!.
max([N],N).

%
%	Sifting Induction Scheme Candidates
%

sift_schemes :-	
	scheme(S1), scheme(S2), S1\==S2,
	subsume_scheme(S1,S2,S3),
	retract(scheme(S1)), retract(scheme(S2)), asserta(scheme(S3)),
	sift_schemes,!.
sift_schemes :-	
	scheme(S1), scheme(S2), S1\==S2,
	merge_scheme(S1,S2,S3),
	retract(scheme(S1)), retract(scheme(S2)), asserta(scheme(S3)),
	sift_schemes,!.
sift_schemes :-	flaw_check,!,sift_schemes,!.
sift_schemes :-
	scheme(S1), scheme(S2), S1\==S2,
	has_heigher_score(S1,S2),
	retract(scheme(S2)),
	sift_schemes,!.
sift_schemes :-
	scheme(S1), scheme(S2), S1\==S2,
	has_nastier_terms(S1,S2),
	retract(scheme(S2)),
	sift_schemes,!.
sift_schemes.

%
%	subsume_scheme(+S1,+S2,-S3) :-
%
%	S2 subsumes S1 to give a new scheme S3
%

:- mode subsume_scheme(+,+,-).
subsume_scheme(Sc1,Sc2,Sc3) :-
	Sc1=[S1,T1,CV1,UCV1,P1],
	Sc2=[S2,T2,CV2,UCV2,P2],
	subsetp(CV1,CV2),
	subsetp(UCV1,UCV2),
	abolish(subsume_C,2),
	setof([C1,C2],(one_of(C1,S1),one_of(C2,S2),
		subsume_case(C1,C2),assertz(subsume_C(C1,C2))),_),
	subsume_C_check1(S1),
	subsume_C_check2(S2),
	abolish(subsume_C,2),
	union(T1,T2,T3),
	add_score(P1,P2,P3),
	Sc3=[S2,T3,CV2,UCV2,P3],
	expl_subsume(Sc1,Sc2,Sc3),!.
subsume(_,_,_) :-
	abolish(subsume_C,2),!,fail.

:- mode subsume_C_check1(+).
subsume_C_check1([C1|C]) :- subsume_C(C1,_), subsume_C_check1(C),!.
subsume_C_check1([]).

:- mode subsume_C_check2(+).
subsume_C_check2([C1|_]) :- subsume_C(C2,C1), subsume_C(C3,C1), C2\==C3,!,fail.
subsume_C_check2([_|C]) :- subsume_C_check2(C),!.
subsume_C_check2([]).

:- public subsume_case/2.
:- mode subsume_case(+,+).
subsume_case((T1->S1),(T2->S2)) :-
	subsetp(T1,T2),
	abolish(subsume_S,2),
	setof([C1,C2],(one_of(C1,S1),one_of(C2,S2),
		subsume_subst(C1,C2),assertz(subsume_S(C1,C2))),_),
	subsume_S_check1(S1),
	subsume_S_check2(S2),
	abolish(subsume_S,2),!.
subsume_case(_,_) :-
	abolish(subsume_S,2),!,fail.

:- mode subsume_S_check1(+).
subsume_S_check1([S1|S]) :- subsume_S(S1,_), subsume_S_check1(S),!.
subsume_S_check1([]).

:- mode subsume_S_check2(+).
subsume_S_check2([S1|_]) :- subsume_S(S2,S1), subsume_S(S3,S1), S2\==S3,!,fail.
subsume_S_check2([_|S]) :- subsume_S_check2(S),!.
subsume_S_check2([]).

:- public subsume_subst/2.
:- mode subsume_subst(+,+).
subsume_subst([S1_1|S1s],S2) :-
	subsume_subst1(S1_1,S2),
	subsume_subst(S1s,S2),!.
subsume_subst([],_).

:- mode subsume_subst1(+,+).
subsume_subst1((V->T1),[(V->T2)|_]) :- occur(T1,T2).
subsume_subst1(S1,[_|S2]) :- subsume_subst1(S1,S2).

:- mode occur(+,+).
occur(T,T).
occur(T1,[_|T2]) :- occur_in_args(T1,T2).

:- mode occur_in_args(+,+).
occur_in_args(T1,[T2|_]) :- occur(T1,T2).
occur_in_args(T1,[_|T2]) :- occur_in_args(T1,T2).

:- mode expl_subsume(+,+,+).
expl_subsume(_,_,_) :- !.
expl_subsume(_,_,_) :- dumbly,!.
expl_subsume(S1,S2,S3) :-
	nl,write('The scheme:'),
	nl,pp_scheme(S2),nl,
	nl,write('is subsumed by the scheme:'),
	nl,pp_scheme(S1),nl,
	nl,write('getting a new scheme:'),
	nl,pp_scheme(S3),nl,brk,!.

%
%	merge_scheme(+S1,+S2,-S3) :-
%
%	merge S1 and S2 into S3
%

:- mode merge_scheme(+,+,-).
merge_scheme(Sc1,Sc2,Sc3) :-
	Sc1=[S1,T1,CV1,UCV1,P1],
	Sc2=[S2,T2,CV2,UCV2,P2],
	intersection(CV1,CV2,X), X\==[],
	intersection(UCV1,CV2,Y), Y=[],
	intersection(CV1,UCV2,Z), Z=[],
	abolish(merge_C,3),
	setof([C1,C2,C3],(one_of(C1,S1),one_of(C2,S2),
		merge_case(C1,C2,C3),assertz(merge_C(C1,C2,C3))),_),
	merge_C_check1(S1),
	merge_C_check2(S2,S3),
	union(CV1,CV2,CV3),
	union(UCV1,UCV2,UCV3),
	union(T1,T2,T3),
	add_score(P1,P2,P3),
	abolish(merge_C,3),
	Sc3=[S3,T3,CV3,UCV3,P3],
	expl_merge(Sc1,Sc2,Sc3),!.
merge_scheme(_,_,_) :-
	abolish(merge_C,3),!,fail.

:- mode merge_C_check1(+).
merge_C_check1([C1|C]) :- merge_C(C1,_,_), merge_C_check1(C),!.
merge_C_check1([]).

:- mode merge_C_check2(+,-).
merge_C_check2([C1|_],_) :- merge_C(C2,C1,_), merge_C(C3,C1,_), C2\==C3,!,fail.
merge_C_check2([C1|C],[C2|C3]) :- merge_C(_,C1,C2), merge_C_check2(C,C3),!.
merge_C_check2([C1|C],[C1|C3]) :- merge_C_check2(C,C3),!.
merge_C_check2([],[]).

:- public merge_case/3.
:- mode merge_case(+,+,-).
merge_case((T1->S1),(T2->S2),(T3->S3)) :-
	abolish(merge_S,3),
	setof([C1,C2,C3],(one_of(C1,S1),one_of(C2,S2),
		merge_subst(C1,C2,C3),assertz(merge_S(C1,C2,C3))),_),
	merge_S_check1(S1),
	merge_S_check2(S2,S3),
	union(T1,T2,T12),
	cleanup(T12,T3),
	abolish(merge_S,3),!.
merge_case(_,_,_) :-
	abolish(merge_S,3),!,fail.

:- mode merge_S_check1(+).
merge_S_check1([S1|S]) :- merge_S(S1,_,_), merge_S_check1(S),!.
merge_S_check1([]).

:- mode merge_S_check2(+,-).
merge_S_check2([S1|_],_) :- merge_S(S2,S1,_), merge_S(S3,S1,_), S2\==S3,!,fail.
merge_S_check2([S1|S],[S2|S3]) :- merge_S(_,S1,S2), merge_S_check2(S,S3),!.
merge_S_check2([S1|S],[S1|S3]) :- merge_S_check2(S,S3),!.
merge_S_check2([],[]).

:- public merge_subst/3.
:- mode merge_subst(+,+,-).
merge_subst(S1,S2,S3) :-
	setof([C1,C2],(one_of(C1,S1),one_of(C2,S2),common_var(C1,C2)),CS),
	check_CS(CS),
	union(S1,S2,S3),!.

:- public common_var/2.
:- mode common_var(+,+).
common_var((V->_),(V->_)).

:- mode check_CS(+).
check_CS([[S,S]|CS]) :- check_CS(CS),!.
check_CS([]).

:- mode expl_merge(+,+,+).
%expl_merge(_,_,_) :- !.
%expl_merge(_,_,_) :- dumbly,!.
expl_merge(S1,S2,S3) :-
	nl,write('The scheme:'),
	nl,pp_scheme(S2),nl,
	nl,write('and the scheme:'),
	nl,pp_scheme(S1),nl,
	nl,write('are merged into a new scheme:'),
	nl,pp_scheme(S3),nl,brk,!.

%
%	flawed_scheme(+S2,+S1) :-
%
%	scheme S1 is flawed by S2
%

flaw_check :- setof([S1,S2],
	(scheme(S1), scheme(S2), S1\==S2, flawed_scheme(S1,S2)),
	FS),!,
	setof(Sc1,Y^one_of([Sc1,Y],FS),S1s),
	setof(Sc2,scheme(Sc2),S2s),
	\+set_equalp(S1s,S2s),
	setof(S,(one_of(S,S1s),retract(scheme(S)),expl_flawed(scheme(S))),_),!.

:- mode flawed_scheme(+,+).
flawed_scheme([_,_,CV2,UCV2,_],[_,T1,_,_,_]) :-
	induction_variable(IV1,T1),
	(member(IV1,CV2); member(IV1,UCV2)).

:- mode induction_variable(-,+).
%induction_variable(IV,T) :- one_of(Term,T), changeables(Term,Temp,C),
induction_variable(IV,T) :- one_of(Term,T), changeables(Term,_,C),
	one_of(IV,C).

:- mode expl_flawed(+).
expl_flawed(_) :- !.
expl_flawed(_) :- dumbly,!.
expl_flawed(S) :-
	nl,write('The scheme:'),
	nl,pp_scheme(S),nl,
	nl,write('is flawed'),nl,brk,!.

%
%	Tie Breaking among Remaining Induction Scheme Candidates
%

:- mode has_heigher_score(+,+).
has_heigher_score([_,_,_,_,P1],[_,_,_,_,P2]) :-
	heigher_score(P1,P2).

:- mode heigher_score(+,+).
heigher_score(N1/D1,N2/D2) :- M1 is N1*D2, M2 is N2*D1, M1>M2.

:- mode add_score(+,+,-).
add_score(N1/D1,N2/D2,N3/D3) :-
	D3 is D1*D2, N31 is N1*D2, N32 is N2*D1, N3 is N31+N32.

:- mode has_nastier_terms(+,+).
has_nastier_terms([_,T1,_,_,_],[_,T2,_,_,_]) :-
	nastiness(T1,0,N1), nastiness(T2,0,N2), N1>N2.

:- mode nastiness(+,+,-).
nastiness([X|R],N,M) :-
	nasty_function(X), N1 is N+1, nastiness(R,N1,M).
nastiness([_|R],N,M) :- nastiness(R,N,M).
nastiness([],N,N).

%
%	Performing Induction Rewrite
%

:- mode perform_induction(+).
perform_induction([ID|Path]:Clause) :-
	scheme([M,Terms,CV,_,_]),
	superimpose_machine(M,S),
	(dumbly;
	 (nl,write('We will induct according to the following scheme:'),nl,
	  pp_scheme([S,Terms,CV,_,_]),nl)),
	base_case(S,[ID|Path]:Clause),
	((S=[C],induction_step(C,[i(ID)|Path]:Clause));
	 ( S=SS,%reverse(S,SS),
	   induction_steps(SS,[i(ID)-1|Path]:Clause))),!.

:- mode superimpose_machine(+,-).
superimpose_machine([(T->S)|M],[C|Ms]) :-
	superimpose_machine1((T->S),M,C,M1),
	superimpose_machine(M1,Ms),!.
superimpose_machine([],[]).

:- mode superimpose_machine1(+,+,-,-).
superimpose_machine1((T1->S1),[(T2->S2)|M],C,M1) :-
	set_equalp(T1,T2),
	union(S1,S2,S3),
	superimpose_machine1((T1->S3),M,C,M1),!.
superimpose_machine1(C1,[C2|M],C,[C2|M1]) :-
	superimpose_machine1(C1,M,C,M1),!.
superimpose_machine1(C,[],C,[]).

:- mode base_case(+,+).
base_case(M,[ID|Path]:Clause) :-
%	( dumbly; (nl,write('Base Case:'),nl)),
	base_tests(M,Tests),
	formula_to_clauses([or|Tests],Cls1),
	append_cls(Cls1,Clause,Cls),
	cleanup(Cls,ClsC),
	( (ClsC=t,nl,write(trivial),nl);
	  pool_(simplification,[b(ID)|Path]:ClsC,z)),!.

:- mode append_cls(+,+,-).
append_cls([C1|Cls1],C2,[C3|Cls3]) :- append(C1,C2,C3),
	append_cls(Cls1,C2,Cls3),!.
append_cls([],_,[]).

:- mode base_tests(+,-).
base_tests([(Tests->_)|M],[BT|T]) :-
	((Tests=[BT1],or_form(BT1,BT));
	 (or_forms(Tests,BT2),BT=[and|BT2])),!,
	base_tests(M,T).
base_tests([],[]).

:- public or_forms/2.
:- mode or_forms(+,-).
or_forms([A|B],[OA|OB]) :- or_form(A,OA), or_forms(B,OB),!.
or_forms([],[]).

:- public or_form/2.
:- mode or_form(+,-).
or_form([T],T).
or_form(T,[or|T]).

:- mode induction_step(+,+).
induction_step((Tests->Substs),NPath:Clause) :-
%	(dumbly; (nl,write('Induction Step:'),nl)),
	neg_alls(Tests,NTests),
	neg_all(Clause,NClause),
	apply_substs(NClause,Substs,NIhyps),
	append(NTests,NIhyps,Hyps),
	andor_to_clauses(Hyps,HCls),
	append_cls(HCls,Clause,ICls),
	cleanup(ICls,ClsC),
	qsort(ClsC,[],CCC),
	pool_(simplification,NPath:CCC,z),!.

:- mode induction_steps(+,+).
induction_steps([C|M],[i(ID)-N|Path]:Clause) :-
	induction_step(C,[i(ID)-N|Path]:Clause),
	N1 is N+1,
	induction_steps(M,[i(ID)-N1|Path]:Clause),!.
induction_steps([],_).

:- mode apply_substs(+,+,-).
apply_substs(Conj,[S|Substs],[IHyp|IHyps]) :-
	apply_subst(Conj,S,IHyp),
	apply_substs(Conj,Substs,IHyps),!.
apply_substs(_,[],[]).

:- mode apply_subst(+,+,-).
apply_subst(X,S,XS) :- (variable(X);atomic(X)),!, subst_var(X,S,XS),!.
apply_subst([A|D],S,[AS|DS]) :-	apply_subst(A,S,AS), apply_subst(D,S,DS),!.
apply_subst(X,_,X).

:- mode subst_var(+,+,-).
subst_var(X,[(X->T)|_],T).
subst_var(X,[_|S],XS) :- subst_var(X,S,XS),!.
subst_var(X,[],X).

%
%	Printing Induction Schemes
%

pp_schemes.
pp_schemes :- listing(scheme),!.

:- mode pp_scheme(+).
pp_scheme([Cases,Terms,CV,_,_]) :-
	sort(CV,CVS),
	steps(Cases,CVS,BaseTest,Steps),
	(BaseTest=[BT]; BT=[and|BaseTest]),
	Scheme=[and,[implies,BT,['P'|CVS]]|Steps],
	nl,tab(8),pp(8,Scheme),nl,
	nl,write('    which accounts for'),nl,
	nl,tab(8),((Terms=[T],pp(8,T)); pp(8,[''|Terms])),!.

:- mode steps(+,+,-,-).
steps([(Test->Substs)|Cases],CV,[NT|BaseTest],
	[[implies,[and,T|Ihyps],['P'|CV]]|Steps]) :-
	((Test=[T1],or_form(T1,T)); (or_forms(Test,T1),T=[and|T1])),
	negate(T,NT),
	ihyps(Substs,CV,Ihyps),
	steps(Cases,CV,BaseTest,Steps),!.
steps([],_,[],[]).

:- mode ihyps(+,+,-).
ihyps([S|Substs],CV,[['P'|SCV]|Ihyps]) :-
	apply_subst(CV,S,SCV),
	ihyps(Substs,CV,Ihyps),!.
ihyps([],_,[]).

% EOF induct.pl

:-
	on(continue),
	off(expl_rewrite),
	off(expl_expand),
	off(vt100_).
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public append/3.
:- mode append(+,+,-).
append([H|A],B,[H|C]) :- append(A,B,C),!.
append([],B,B).

:- public reverse/2.
:- mode reverse(+,-), rev_(+,+,-).
reverse(X,Y) :- rev_(X,[],Y),!.

	rev_([A|X],Y,Z) :- rev_(X,[A|Y],Z),!.
	rev_([],X,X).

:- public member/2.
:- mode member(+,+).
member(A,[B|C]) :- var(A),!,var(B),!,(A==B; member(A,C)),!.
member(A,[A|_]).
member(A,[_|C]) :- member(A,C),!.

:- public one_of/2.
:- mode one_of(?,+).
one_of(A,[A|_]).
one_of(A,[_|C]) :- one_of(A,C).

:- public delete/3.
:- mode delete(+,+,-).
delete(X,[X|L],L) :- !.
delete(X,[Y|L],[Y|M]) :- delete(X,L,M),!.
delete(_,[],[]).

:- public one_and_rest/3.
:- mode one_and_rest(+,-,-).
one_and_rest([X|L],X,L).
one_and_rest([Y|L],X,[Y|M]) :- one_and_rest(L,X,M).

:- mode qsort(+,+,-).
qsort([X|L],R0,R) :-
   partition(L,X,L1,L2),
   qsort(L2,R0,R1),
   qsort(L1,[X|R1],R).
qsort([],R,R).

:- mode partition(+,+,-,-).
partition([X|L],Y,[X|L1],L2) :- size(X,M), size(Y,N), M < N, !,
   partition(L,Y,L1,L2).
partition([X|L],Y,[X|L1],L2) :- size(X,M), size(Y,N), M = N, !, X @< Y,
   partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :- !,
   partition(L,Y,L1,L2).
partition([],_,[],[]).

:- mode size(+,-).
size([A|B],L) :- size(A,M), size(B,N), L is M+N,!.
size(_,1).

% EOF list.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

% Type Set Representation for "true" and "false"

:-
	assert(bit_place([t],2)),
	assert(bit_place([f],1)).

% Wired-in Function Definitions

:-
	assert((@definition not(p) = if(p,f,t))),
	assert(type_prescription([not,_],tp(3,[]))),

	assert((@definition and(p,q) = if(p,if(q,t,f),f))),
	assert(type_prescription([and|_],tp(3,[]))),

	assert((@definition or(p,q) = if(p,t,if(q,t,f)))),
	assert(type_prescription([or|_],tp(3,[]))),

	assert((@definition implies(p,q) = if(p,if(q,t,f),t))),
	assert(type_prescription([implies,_,_],tp(3,[]))).

% EOF logic.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- op(1150,fx,dynamic).

:- dynamic pp_wid/1.

:- public pp_width/1.
pp_width(W) :- var(W),!, (pp_wid(W); (assert(pp_wid(70)),pp_wid(W))),!.
pp_width(W) :- abolish(pp_wid,1), asserta(pp_wid(W)),!.

:- public pp/1, pp/2.
:- mode	pp(+), pp(+,+).
pp(X) :- pp(0,X),!.
pp(Indent,X) :- pp(0,Indent,X),!.

:- mode pp(+,+,+).
pp(Level,Indent,T) :- 
	fold_abbrev(T,Term),
	pp_term_length(Term,TL),
	pp_width(Width),
	Space is Width-Level-Indent,
	pp(Level,Indent,Term,TL,Space),!.

:- mode fold_abbrev(+,-).
fold_abbrev(X,Y) :- abbrev__(Y=X),!.
fold_abbrev([A|B],[X|Y]) :- fold_abbrev(A,X), fold_abbrev(B,Y),!.
fold_abbrev(X,X).

:- mode pp(+,+,+,+,+).
pp(_,_,{X},_,_) :- wwrite({X}),!.
pp(_,_,[F],TL,Space) :- integer(TL), TL =< Space,!,
	wwrite(F,reverse),!.
pp(_,_,Term,TL,Space) :- integer(TL), TL =< Space,!,
	((Term=[$,X], wwrite($X,reverse)); wwrite(Term,reverse)),!.
pp(_,_,[$,List],_=TL,Space) :- TL+1 =< Space,!,
	wwrite($List,reverse),!.
pp(Level,Indent,[$,List],(AT=M)=_,Space) :- M+3 =< Space,!,
	wwrite('$[',reverse),
		Indent2 is Indent+2,
		Space2 is Space-3,
		pp_list_args(Level,Indent2,List,AT,Space2),!,
		wwrite(']',reverse),!.
pp(Level,Indent,[F|Args],(FL,AT=_)=TL,Space) :- F\==($), TL =< Space,!,
	wwrite(F,reverse), wwrite('(',reverse),
	Indent2 is Indent+FL+1, Space2 is Space-FL-1,
	pp_args(Level,Indent2,Args,AT,Space2),!.
pp(Level,Indent,[F|Args],(FL,AT=ATL)=_,Space) :- F\==($), FL+2+ATL =< Space,!,
	wwrite(F,reverse),wwrite('(',reverse),
		Indent2 is Indent+FL+1,
		Space2 is Space-FL-2,
		pp_args_nl(Level,Indent2,Args,AT,Space2),!,
		wwrite(')',reverse),!.
pp(Level,Indent,[F|Args],(FL,AT=_)=_,Space) :- F\==($), FL+1 =< Space,!,
	wwrite(F,reverse),wwrite('(',reverse),
		Indent2 is Indent+3,
		(FL<3;(nl, pp_level(Level), tab(Indent2))),
		Space2 is Space-4,
		pp_args_nl(Level,Indent2,Args,AT,Space2),!,
		wwrite(')',reverse),!.
pp(Level,Indent,T,TL,Space) :- nl,
	Level1 is Level+1,
	Space1 is Space+Indent-4,
	pp_level(Level1), tab(3),
	pp(Level1,3,T,TL,Space1),!.

:- mode pp_args(+,+,+,+,+).
pp_args(Level,Indent,[A],[AT],Space) :-
	pp(Level,Indent,A,AT,Space), wwrite(')',reverse),!.
pp_args(Level,Indent,[A|Args],[AT|ATs],Space) :-
	pp(Level,Indent,A,AT,Space), wwrite(',',reverse),
	(AT=(_=L); AT=L),
	Indent2 is Indent+L+1, Space2 is Space-L-1,
	pp_args(Level,Indent2,Args,ATs,Space2),!.

:- mode pp_args_nl(+,+,+,+,+).
pp_args_nl(Level,Indent,[A],[AT],Space) :- !,pp(Level,Indent,A,AT,Space),!.
pp_args_nl(Level,Indent,[A|Args],[AT|ATs],Space) :-
	pp(Level,Indent,A,AT,Space), wwrite(',',reverse),nl,
	pp_level(Level), tab(Indent),
	pp_args_nl(Level,Indent,Args,ATs,Space),!.

:- mode pp_list(+,+,+,+,+).
pp_list(_,_,Term,TL,Space) :- integer(TL), TL =< Space,!,
	wwrite(Term,reverse),!.
pp_list(_,_,List,_=TL,Space) :- TL =< Space,!,
	wwrite(List,reverse),!.
pp_list(Level,Indent,List,(AT=M)=_,Space) :- M+2 =< Space,!,
	wwrite('[',reverse),
		Indent2 is Indent+1,
		nl, pp_level(Level), tab(Indent2),
		Space2 is Space-2,
		pp_list_args(Level,Indent2,List,AT,Space2),!,
		wwrite(']',reverse),!.
pp_list(Level,Indent,List,TL,Space) :- nl,
	Level1 is Level+1,
	Space1 is Space+Indent-4,
	pp_level(Level1), tab(3),
	pp_list(Level1,3,List,TL,Space1),!.

:- mode pp_list_args(+,+,+,+,+).
pp_list_args(Level,Indent,Args,N,Space) :-
	integer(N), pp_list(Level,Indent,Args,N,Space),!.
pp_list_args(Level,Indent,[A],[AT],Space) :- !,
	pp_list(Level,Indent,A,AT,Space),!.
pp_list_args(Level,Indent,[A|Args],[AT|ATs],Space) :-
	pp_list(Level,Indent,A,AT,Space), wwrite(',',reverse),nl,
	pp_level(Level), tab(Indent),
	pp_list_args(Level,Indent,Args,ATs,Space),!.

:- mode pp_level(+).
pp_level(0).
pp_level(L) :- wwrite('|'), L1 is L-1, pp_level(L1),!.

:- mode pp_term_length(+,-).
pp_term_length({_},0).
pp_term_length([$,X],AT) :- pp_term_length_list(X,Y),!,
	((Y=((T=M)=N), N1 is N+1, AT=((T=M)=N1));
	 AT is Y+1),!.
pp_term_length([F],N) :- name(F,M), length(M,N),!.
pp_term_length([F|A],(N1,AT=M)=N) :- F\==($),
	pp_term_length(F,N1),!,
	pp_term_length_args(A,AT,M,N2),!,
	N is N1+N2+1,!.
pp_term_length(X,N) :- name(X,XN),!,length(XN,N),!.

:- mode pp_terms_length_args(+,+,-,-).
pp_term_length_args([A|As],[AX|ATs],Max,Total) :-
	pp_term_length(A,AX),
	(AX=(_=AN); AN=AX), 
	pp_term_length_args(As,ATs,Max1,ANs),
	((Max1 >= AN, Max is Max1); Max is AN),
	Total is AN+ANs+1.
pp_term_length_args([],[],0,0).

:- mode pp_terms_length_list(+,-).
pp_term_length_list(X,N) :- atomic(X), name(X,M), length(M,N),!.
pp_term_length_list([],3).
pp_term_length_list([X|Y],N) :- atomic(Y), name(X,XN), name(Y,YN),
	length(XN,XNN), length(YN,YNN), N is XNN+YNN+3,!.
pp_term_length_list([X|Y],(AT=M)=N1) :-
	pp_term_length_list_args([X|Y],AT,M,N),!,
	N1 is N+1,!.

:- mode pp_terms_length_list_args(+,+,-,-).
pp_term_length_list_args([A|As],[AX|ATs],Max,Total) :-
	pp_term_length_list(A,AX),
	(AX=(_=AN); AN=AX), 
	pp_term_length_list_args(As,ATs,Max1,ANs),
	((Max1 >= AN, Max is Max1); Max is AN),
	Total is AN+ANs+1.
pp_term_length_list_args([],[],0,0).

% EOF pp.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public prove/1.
:- mode	prove(+).
prove(Path:Formula) :-
	(dumbly; defun_loaded; (nl,wwrite('Proving...'),nl)),
	abolish(goal_ID,1),
	abolish(pool,2),
	abolish(var_seed,1),
	abolish(used_variable,1),
	((setof(V,var_in_term(V,Formula),Vs),
	  setof(W,(one_of(W,Vs),assertz(used_variable(W))),_));
	 true),
	formula_to_clauses(Formula,Clauses),
	pool_(simplification,[#(0=Path)]:Clauses,z),!.

:- mode new_goal(-).
new_goal(N) :- (retract(goal_ID(M)); M=0),
	N is M+1, asserta(goal_ID(N)),!.

:- dynamic save_continue/0.
:- public prove_short/1.
:- mode	prove_short(+).
prove_short(Path:Formula) :-
	( continue -> assert(save_continue) ; true ),
	abolish(continue,0), assert(continue),
	prove(Path:Formula),!,qq,!,
	( save_continue -> assert(continue) ; abolish(continue,0) ),!.

:- public error/1.
:- mode error(+).
error(X) :- nl,wwrite('\aError! ---> '),wwrite(X),nl,!.

:- public pool_/3.
:- mode pool_(+,+,+).
pool_(_,_:t,_) :- defun_loaded,!.
%pool_(_,_:t,_) :- nl,wwrite(trivial),nl,!.
pool_(_,_:t,_) :- !.
pool_(P,Path:[t|Cs],AZ) :- pool_(P,Path:Cs,AZ),!.
pool_(P,Path:[C1|Cs],AZ) :-
	((Cs=[],!,
%	  (dumbly;
%	   (nl,wwrite('Poured a conjecture into the '),
%	    wwrite(P),wwrite('-pool.'),nl)),
	  pool1_(P,Path:C1,AZ)) ;
%	 (length([C1|Cs],N),
%	  (dumbly;
%	   (nl,wwrite('Poured '),wwrite(N),wwrite(' conjectures into the '),
%	    wwrite(P),wwrite('-pool.'),nl)),
	  pooln_(P,Path:[C1|Cs],1,AZ)),!.

:- mode pool1_(+,+,+).
pool1_(_,_:t,_) :- (defun_loaded; (nl,wwrite(trivial),nl)),!.
pool1_(P,Path:Clause,a) :- (Path=[#(N)|Pa]; (new_goal(N),Pa=Path)),
	asserta(pool(P,[#(N)|Pa]:Clause)),!,
	nl,wwrite('Conjecture ',bold),pp_path(11,[#(N)]),nl,
	nl,wwrite('    has been placed on the top of the '),
	wwrite(P),wwrite('-pool.'),nl,!.
pool1_(P,Path:Clause,z) :- (Path=[#(N)|Pa]; (new_goal(N),Pa=Path)),
	assertz(pool(P,[#(N)|Pa]:Clause)),!,
	nl,wwrite('Conjecture ',bold),pp_path(11,[#(N)]),nl,
	nl,wwrite('    has been placed at the bottom of the '),
	wwrite(P),wwrite('-pool.'),nl,!.

:- mode pooln_(+,+,+,+).
pooln_(P,[M|Path]:[C1|Cs],N,AZ) :-
	pool1_(P,[M-N|Path]:C1,AZ),
	N1 is N+1,
	pooln_(P,[M|Path]:Cs,N1,AZ),!.
pooln_(_,_:[],_,_).

:- mode add_(+,+).
add_(Type,NL=B) :- NL=..[Name|Labels],mtos(B,Body),
	( dumbly ;
	  ( nl,pp_(Type,Name,[],Body),
	    ( Type\== (theorem) ; (nl,wwrite('    has been proved.'),nl)))),!,
	add_(Type,Name,Labels,Body),!.

:- mode	add_(+,+,+,+).
add_(Type,Name,[Label|Labels],Body) :-
	add_(Type,Name,Label,Body),
	add_(Type,Name,Labels,Body),!.
add_(_,_,[],_).
add_(Type,Name,rewrite,Body) :-
	formulate_rewrite_lemma(Body,L,R,Hyp,P),
	asserta(rewrite_lemma(L,R,Hyp,P,Type,Name)),
	( dumbly ;
	  ( %nl,pp_(Type,Name,rewrite,Body),
	    nl,wwrite('    is registered as a rewrite lemma.'),nl)),!.
add_(Type,Name,induction,Body) :-
	formulate_induction_lemma(Body,B,Hyp),
	asserta(induction_lemma(B,Hyp,Type,Name)),
	( dumbly ;
	  ( %nl,pp_(Type,Name,induction,Body),
	    nl,wwrite('    is registered as an induction lemma.'),nl)),!.
add_(Type,Name,elimination,Body) :-
	(setof(V,var_in_term(V,Body),Vars); Vars=[]),
	p_vars(Vars,Body,_,P_Body),
	assertz(elimination_lemma(Type,Name,P_Body)),
	( dumbly ;
	  ( %nl,pp_(Type,Name,Label,Body),
	    nl,wwrite('    is registered as an elimination lemma.'),nl)),!.
add_(Type,Name,generalize,Body) :-
	(setof(V,var_in_term(V,Body),Vars); Vars=[]),
	p_vars(Vars,Body,_,P_Body),
	assertz(generalization_lemma(Type,Name,P_Body)),
	( dumbly ;
	  ( %nl,pp_(Type,Name,Label,Body),
	    nl,wwrite('    is registered as a generalization lemma.'),nl)),!.
add_(_,_,_,_) :- error('Invalid lemma.').

:- mode formulate_rewrite_lemma(+,-,-,-,-).
formulate_rewrite_lemma(Body,P_L,P_R,P_H,P) :-
	(setof(V,var_in_term(V,Body),Vars); Vars=[]),
	((Body=[implies,Hyp,Conc], formula_to_clauses(Hyp,H));
	 (Body=Conc,H=t)),
	((Conc=[equal,L,R],permutative(L,R,P));
	 (Conc=[not,L],R=f,P=no);
	 (Conc=L,R=t,P=no)),
	p_vars(Vars,[L,R,H],_,PLRH),
	PLRH=[P_L,P_R,P_H],!.

:- mode formulate_induction_lemma(+,-,-).
formulate_induction_lemma(Body,P_Conc,P_H) :-
	(setof(V,var_in_term(V,Body),Vars); Vars=[]),
	((Body=[implies,Hyp,Conc], formula_to_clauses(Hyp,H));
	 (Body=Conc,H=[])),
	p_vars(Vars,[Conc,H],_,PCH),
	PCH=[P_Conc,P_H],!.

:- mode permutative(+,+,-).
permutative(L,R,permutative) :- variable(L),variable(R),!.
permutative([F|A1],[F|A2],permutative) :- permutative1(A1,A2),!.
permutative(_,_,no).

:- mode permutative1(+,+).
permutative1([A1|A],[B1|B]) :- permutative(A1,B1,P),!,P=permutative,!,
	permutative1(A,B),!.
permutative1([],[]).

:- mode pp_(+,+).
pp_(Type,NL=B) :- NL=..[Name|Labels],mtos(B,Body),
	pp_(Type,Name,Labels,Body),!.

:- public pp_/4.
:- mode pp_(+,+,+,+).
pp_(Type,Name,Labels,Body) :-
	((Type = (axiom),   wwrite('Axiom ',bold)) ;
	 (Type = (lemma),   wwrite('Lemma ',bold)) ;
	 (Type = (theorem), wwrite('Theorem ',bold)) ),
	wwrite(Name),
	(Labels=[];
	 (atomic(Labels),wwrite(' ('),wwrite(Labels),wwrite(')'));
	 (wwrite(' '),Lab=..[''|Labels],wwrite(Lab))),nl,
	nl,tab(8),pp(8,Body),nl,!.

:- public pp_clause/2.
:- mode pp_clause(+,+).
pp_clause(_,_) :- dumbly,!.
pp_clause(Path:Clause,Mess) :-
	clause_to_implies(Clause,Implies),
	nl,wwrite('Conjecture ',bold),pp_path(11,Path),nl,
	nl,tab(8),pp(8,Implies),nl,
	(Mess=''; (nl,wwrite(Mess),nl)),!.

:- mode pp_path(+,+).
pp_path(I,[Anc|Dec]) :- pp_node(Anc),!,
	(Dec=[]; (nl,tab(I),pp_path(I,Dec))),!.
pp_path(_,P) :- pp_node(P),!.

:- mode pp_node(+).
pp_node(s(ID)) :- wwrite('simplified '),wwrite(ID),!.
pp_node(c(X,ID)) :- wwrite('eliminating destructor(s) '),wwrite(X),
	wwrite(' base case for '),wwrite(ID),!.
pp_node(d(X,ID)) :- wwrite('destructor(s) replaced by '),wwrite(X),
	wwrite(' in '),wwrite(ID),!.
pp_node(e(ID)) :- wwrite('used an equality in '),wwrite(ID),!.
pp_node(g(X,ID)) :- wwrite('generalized '),wwrite(ID),wwrite(' with '),wwrite(X),!.
pp_node(l(ID)) :- wwrite('irrelevance(s) eliminated in '),wwrite(ID),!.
pp_node(b(ID)-N) :- wwrite('base case-'),wwrite(N),wwrite(' for '),wwrite(ID),!.
pp_node(b(ID)) :- wwrite('base case for '),wwrite(ID),!.
pp_node(i(ID)-N) :- wwrite('induction step-'),wwrite(N),
	wwrite(' for '),wwrite(ID),!.
pp_node(i(ID)) :- wwrite('induction step for '),wwrite(ID),!.
pp_node(X-N) :- pp_node(X), wwrite(' -'), wwrite(N),!.
pp_node(X) :- wwrite(X),!.

pp_pool :-
	(setof(P:C,(pool(simplification,P:C),
		pp_clause(['waiting simplification'|P]:C,'')),_); true),
	(setof(P:C,(pool(heuristics,P:C),
		pp_clause(['waiting heuristic rewrite'|P]:C,'')),_); true),
	(setof(P:C,(pool(induction,P:C),
		pp_clause(['waiting induction'|P]:C,'')),_); true),
	(setof(P:C,(pool(fail,P:C),
		pp_clause(['failed'|P]:C,'')),_); true),!.

% EOF prove.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- mode	rewrite(+,-,+,+,-,+,-,+,-).
rewrite(X,X,_,U,U,V,V,W,W) :- var(X),!.
rewrite(X,X,_,U,U,V,V,W,W) :- integer(X),!.
rewrite([$,X],[$,X],_,U,U,V,V,W,W).
rewrite(X,Z,[I,N,P,L,H],U,U1,V,V1,W,W1) :-
	rewrite0(X,Y,[I,N,P,L,H],U,U1,V,V1,W,W1),
	((H=tf,type_set(Y,YTS,I),ts_intersection(YTS,1,YTS1),YTS1=0,Z=t);
	 Z=Y),!.

:- mode	rewrite0(+,-,+,+,-,+,-,+,-).
rewrite0(t,t,_,U,U,V,V,W,W).
rewrite0(f,f,_,U,U,V,V,W,W).
rewrite0([quote,X],[quote,X],_,U,U,V,V,W,W).
rewrite0(X,Y,_,U,U,V,V1,W,W) :- member(X,V),!,V=V1,!,X=Y,!.
rewrite0(X,Y,[TS_alist|_],U,U,V,V1,W,W) :-
	member(X=TS,TS_alist),bit_place([Y],TS),!,V=V1,!.
rewrite0(X,t,_,U,U,V,V,W,W) :- recognizer_constructor(X),!.
rewrite0(X,t,_,U,U,V,V,W,W) :- recognizer_bottom(X),!.
rewrite0(X,Y,_,U,U,V,[Y|V],W,W) :- variable(X),!,X=Y,!.
rewrite0(X,Y,_,U,U,V,[Y|V],W,W) :- explicit_value(X),!,X=Y,!.
rewrite0(X,Z,[TS_alist|Info],U,U1,V,[Z|V1],W,W1) :-
	rewrite1(X,Y,[TS_alist|Info],U,U1,V,V1,W,W1),!,
	((type_set(Y,TS,TS_alist),bit_place([Z],TS)); Y=Z),!.

:- mode	rewrite1(+,-,+,+,-,+,-,+,-).

rewrite1([if,_,X,X],X1,I,U,U1,V,V1,W,W1) :- !,rewrite(X,X1,I,U,U1,V,V1,W,W1),!.
rewrite1([if,t,L,_],L1,I,U,U1,V,V1,W,W1) :- !,rewrite(L,L1,I,U,U1,V,V1,W,W1),!.
rewrite1([if,f,_,R],R1,I,U,U1,V,V1,W,W1) :- !,rewrite(R,R1,I,U,U1,V,V1,W,W1),!.
rewrite1([if,T,L,R],New,[TS_alist,N_list,_,Lev,H],U,U2,V,V2,W,W2) :-
	rewrite(T,T1,[TS_alist,N_list,[],Lev,tf],U,U1,V,V1,W,W1),!,
	rewrite_if(T1,L,R,New,[TS_alist,N_list,_,Lev,H],
			U1,U2,V1,V2,W1,W2),!.

:- mode rewrite_if(+,+,+,-,+,+,-,+,-,+,-).
rewrite_if(t,L,_,New,[TS_alist,N_list,_,Lev,H],U,U1,V,V1,W,W1) :- !,
	rewrite(L,New,[TS_alist,N_list,[],Lev,H],U,U1,V,V1,W,W1),!.
rewrite_if(f,_,R,New,[TS_alist,N_list,_,Lev,H],U,U1,V,V1,W,W1) :- !,
	rewrite(R,New,[TS_alist,N_list,[],Lev,H],U,U1,V,V1,W,W1),!.
rewrite_if(T,L,R,New,[TS_alist,N_list,_,Lev,H],U,U,V,V,W,W2) :-
	type_set(T,TS,TS_alist), TS=3,
	ts_assumption(T,A,TS_alist), A=or(L_Ass,R_Ass),!,
	(((expl_rewrite; expl_expand),
	  write('------------------------------------------------------------------------------'),nl); true),
	rewrite(L,L1,[L_Ass,N_list,[],Lev,H],U,_,[],_,W,W1),!,
	(((expl_rewrite; expl_expand),
	  write('------------------------------------------------------------------------------'),nl); true),
	rewrite(R,R1,[R_Ass,N_list,[],Lev,H],U,_,[],_,W1,W2),!,
	(((expl_rewrite; expl_expand),
	  write('------------------------------------------------------------------------------'),nl); true),
	rewrite_if1([if,T,L1,R1],New,TS_alist),!.
rewrite_if(T,L,R,[if,T,L,R],_,U,U,V,V,W,W).

%
%   Use the following (wired-in-)rewrite rules if applicable:
%
%	[equal,[if,X,Y,Y],Y]
%	[equal,[if,X,X,f],X]
%	[equal,[if,X,t,f],X]	(applied only if X is Boolean)
%

:- mode	rewrite_if1(+,-,+).
rewrite_if1([if,_,Y,Y],Y,_).
rewrite_if1([if,X,X,f],X,_).
rewrite_if1([if,X,t,f],X,TS_alist) :- type_set(X,TS,TS_alist),TS=3,!.
rewrite_if1([if,[not,X],f,t],X,TS_alist) :- type_set(X,TS,TS_alist),TS=3,!.
rewrite_if1(X,X,_).

rewrite1([equal,X,X],t,_,U,U,V,V,W,W).
rewrite1([equal,L,R],New,[TS_alist,N_list,P_list,Lev,H],U,U3,V,V3,W,W3) :-
	rewrite(L,L1,[TS_alist,N_list,[],Lev,strict],U,U1,V,V1,W,W1),!,
	rewrite(R,R1,[TS_alist,N_list,[],Lev,strict],U1,U2,V1,V2,W1,W2),!,
	rewrite_equal(L1,R1,New,[TS_alist,N_list,P_list,Lev,H],
			U2,U3,V2,V3,W2,W3),!.

:- mode rewrite_equal(+,+,-,+,+,-,+,-,+,-).
rewrite_equal(X,X,t,_,U,U,V,V,W,W).
rewrite_equal(L,R,f,[TS_alist,_,_,_,_],U,U,V,V,W,W) :-
	type_set(L,L_ts,TS_alist), type_set(R,R_ts,TS_alist),
	ts_intersection(L_ts,R_ts,0),!.
rewrite_equal(L,R,f,_,U,U,V,V,W,W) :- explicit_value(L), explicit_value(R),!.
rewrite_equal(L,R,f,_,U,U,V,V,W,W) :- bottom_object(L), constructor(R),!.
rewrite_equal(L,R,f,_,U,U,V,V,W,W) :- bottom_object(R), constructor(L),!.
rewrite_equal(L,R,f,[TS_alist,_,_,_,_],U,U,V,V,W,W) :- constructor_TRs(R,TRs),
	R=[_|Args], component(L,Args,TRs,TS_alist),!.
rewrite_equal(L,R,f,[TS_alist,_,_,_,_],U,U,V,V,W,W) :- constructor_TRs(L,TRs),
	L=[_|Args], component(R,Args,TRs,TS_alist),!.
rewrite_equal(L,R,New,I,U,U1,V,V1,W,W1) :-
	rewrite_equal1([equal,L,R],New,I,U,U1,V,V1,W,W1),!.
rewrite_equal(L,R,New,I,U,U1,V,V1,W,W1) :-
	rewrite_equal1([equal,R,L],New,I,U,U1,V,V1,W,W1),!.
rewrite_equal(L,R,New,I,U,U1,V,V1,W,W1) :-
	if_expr([equal,L,R],P), if_norms(P,Q),
	((Q\==[equal,L,R],!,
	  rewrite(Q,New,I,U,U1,V,V1,W,W1));
	 rewrite_val([equal,L,R],New,I,U,U1,V,V1,W,W1)),!.
rewrite_equal(L,R,[equal,L,R],_,U,U,V,V,W,W).

:- mode component(+,+,+,+).
component(T,[T|_],t,_).
component(T,[T|_],[TR|_],TS_alist) :- type_set([TR,T],TS,TS_alist),TS=2,!.
component(T,[_|Args],[_|TRs],TS_alist) :- component(T,Args,TRs,TS_alist),!.

%
%   Use the following (wired-in-)rewrite rules if applicable:
%
%	[equal,[equal,X,t],X]	(applied only if X is Boolean)
%
%	[equal,[equal,X,[equal,Y,Z]],
%	       [if,[equal,Y,Z],
%		   [equal,X,t],
%		   [equal,X,f]]]
%
%	[equal,[equal,X,f],[if,X,f,t]]
%

:- mode rewrite_equal1(+,-,+,+,-,+,-,+,-).
rewrite_equal1([equal,X,t],New,[TS_alist|Info],U,U1,V,V1,W,W1) :-
	type_set(X,TS,TS_alist),TS=3,!,
	rewrite_equal2(X,New,[TS_alist|Info],U,U1,V,V1,W,W1),!.
rewrite_equal1([equal,X,[equal,Y,Z]],New,I,U,U1,V,V1,W,W1) :-
	rewrite_equal2([if,[equal,Y,Z],[equal,X,t],[equal,X,f]],
		    New,I,U,U1,V,V1,W,W1),!.
rewrite_equal1([equal,X,f],New,I,U,U1,V,V1,W,W1) :-
	rewrite_equal2([if,X,f,t],New,I,U,U1,V,V1,W,W1),!.

:- mode rewrite_equal2(+,-,+,+,-,+,-,+,-).
rewrite_equal2(X,Y,I,U,U1,V,V1,W,W1) :- rewrite_val(X,Y,I,U,U1,V,V1,W,W1),!.
rewrite_equal2(X,X,_,U,U,V,V,W,W).

rewrite1(X,New,[TS_alist,N_list,P_list,Lev,H],U,U2,V,V2,W,W2) :-
	recognizer(X),!,X=[R,Y],!,
	(bit_place(R,RTS);
	 (recognizer_constructor([R,C]), bit_place([C],RTS))),!,
	rewrite(Y,Z,[TS_alist,N_list,[],Lev,strict],U,U1,V,V1,W,W1),!,
	type_set(Z,ZTS,TS_alist),!,
	((RTS=ZTS,!,New=t,!,U1=U2,!,V1=V2,!,W1=W2);
	 (ts_intersection(RTS,ZTS,0),!,New=f,!,U1=U2,!,V1=V2,!,W1=W2);
	 rewrite_val([R,Z],New,[TS_alist,N_list,P_list,Lev,H],
			U1,U2,V1,V2,W1,W2);
	 (New=[R,Z],!,U1=U2,!,V1=V2,!,W1=W2)),!.

rewrite1([not,X],New,[TS_alist,N_list,_,Lev,H],U,U2,V,V2,W,W2) :- !,
	((H=t,!,H1=f); (H=f,!,H1=t); H1=H),!,
	rewrite(X,Y,[TS_alist,N_list,[],Lev,H1],U,U1,V,V1,W,W1),!,
	((Y=t,!,New=f,!,U1=U2,!,V1=V2,!,W1=W2);
	 (Y=f,!,New=t,!,U1=U2,!,V1=V2,!,W1=W2);
	 (if_norms([not,Y],Z),Z\==[not,Y],!,
	  rewrite(Z,New,[TS_alist,N_list,p_list,Lev,H],U1,U2,V1,V2,W1,W2));
	 (negate(Y,New),!,U1=U2,!,V1=V2,!,W1=W2)),!.
/*
rewrite1(X,New,_,U,U,V,V,W,W) :- (integer(X); X=[list|_]; X=[$|_]),!,
	((H=tf,!,New=t); expand_abbrev(X,New)),!.

rewrite1(X,New,_,U,U,V,V,W,W) :- expand_abbrev1(X,New),!.
*/
rewrite1([F|A],New,[TS_alist,N_list,P_list,Lev,H],U,U2,V,V2,W,W2) :-
	rewrite_args(A,B,[TS_alist,N_list,[],Lev,strict],U,U1,V,V1,W,W1),!,
	if_expr([F|B],P), if_norms(P,Q),
	((Q\==[F|B],!,
	  rewrite(Q,New,
	       [TS_alist,N_list,P_list,Lev,H],U1,U2,V1,V2,W1,W2));
	 expand([F|A],[F|B],New,
	       [TS_alist,N_list,P_list,Lev,H],U1,U2,V1,V2,W1,W2)),!.

:- mode rewrite_args(+,-,+,+,-,+,-,+,-).
rewrite_args([A|As],[B|Bs],[TS_alist,N_list,_,Lev,H],U,U2,V,V2,W,W2) :-
	rewrite(A,B,[TS_alist,N_list,[],Lev,H],U,U1,V,V1,W,W1),!,
	rewrite_args(As,Bs,[TS_alist,N_list,[],Lev,H],U1,U2,V1,V2,W1,W2),!.
rewrite_args([],[],_,U,U,V,V,W,W).

%
%	Rewriting Terms using axioms and lemmas
%

:- mode rewrite_val(+,-,+,+,-,+,-,+,-).
rewrite_val(X,New,I,U,U1,V,V1,W,W1) :-
	rewrite_lemma(X,Y,Hyp,P,Type,Name),
	rewrite_val1(X,Y,Hyp,P,Type,Name,New,I,U,U1,V,V1,W,W1),!.
rewrite_val(X,New,[T,N,Q,L,H],U,U1,V,V1,W,W1) :-
	negate(X,NX),
	rewrite_lemma(NX,Y,Hyp,P,Type,Name),
	((H=t,H1=f); (H=f,H1=t); H1=H),
	rewrite_val1(NX,Y,Hyp,P,Type,Name,Z,[T,N,Q,L,H1],U,U1,V,V1,W,W1),
	negate(Z,New),!.

	% "rewrite_val" may fail

:- mode rewrite_val1(+,+,+,+,+,+,-,+,+,-,+,-,+,-).
rewrite_val1(L,R,Hyp,P,Type,Name,New,[TS_alist,N_list,_,Lev,H],
		U,U2,V,V2,W,W2) :-
	expl_rewrite_lemma(Type,Name,Lev),!,
	L1 is Lev+1,!,
	establish(Hyp,[TS_alist,N_list,[],L1,H],
		U,_,V,_,[r(Lev,Type,Name)|W],W1),!,
/*
	(P\==permutative; \+member(R,P_list)),
	rewrite(R,New,[TS_alist,[],[L|P_list],Lev,H],U1,U2,V1,V2,W1,W2),!,
	(P\==permutative; New @< L;
	 (measured_subset(New,MS),explicit_values(MS))),!.
*/
	(P\==permutative; alphalessp(R,L)),
	rewrite(R,New,[TS_alist,[],[],Lev,H],U,U2,V,V2,W1,W2),!.

:- mode alphalessp(+,+).
alphalessp([X|Xs],[Y|Ys]) :- !,length([X|Xs],L),length([Y|Ys],M),!,
	(L<M;
	 (L=M,(alphalessp(X,Y); (alphalesseqp(X,Y),alphalessp(Xs,Ys))))),!.
alphalessp(X,[_|_]) :- atomic(X),!.
alphalessp(X,Y) :- atomic(X), atomic(Y), alpha_lessp(X,Y),!.

:- mode alphalesseqp(+,+).
alphalesseqp([X|Xs],[Y|Ys]) :- !,length([X|Xs],L),length([Y|Ys],M),!,
	(L<M; (L=M,alphalesseqp(X,Y),alphalesseqp(Xs,Ys))),!.
alphalesseqp(X,[_|_]) :- atomic(X),!.
alphalesseqp(X,Y) :- atomic(X), atomic(Y), \+alpha_lessp(Y,X),!.

:- public alpha_lessp/2.
:- mode alpha_lessp(+,+).
alpha_lessp(X,Y) :- lessp_transitive(X,Y),!.
alpha_lessp(X,Y) :- \+lessp_transitive(Y,X), X @< Y,!.

:- public lessp_transitive/2.
:- mode lessp_transitive(+,+).
lessp_transitive(X,Y) :- lessp(X,Y),!.
lessp_transitive(X,Y) :- lessp(X,Z),lessp_transitive(Z,Y),!.

% Preventing from Infinite Backward Chaining

:- public establish/8.
:- mode establish(+,+,+,-,+,-,+,-).
establish(t,_,U,U,V,V,W,W).
establish([Hyp|Hyps],[TS_alist,N_list,_,Lev,H],U,U2,V,V2,W,W2) :-
	establish1(Hyp,[TS_alist,N_list,[],Lev,H],U,U1,V,V1,W,W1),
	establish(Hyps,[TS_alist,N_list,[],Lev,H],U1,U2,V1,V2,W1,W2).
establish([],_,U,U,V,V,W,W).

:- mode establish1(+,+,+,-,+,-,+,-).
establish1([Hyp|_],I,U,U1,V,V1,W,W1) :- establish2(Hyp,I,U,U1,V,V1,W,W1).
establish1([_|Hyp],I,U,U1,V,V1,W,W1) :- establish1(Hyp,I,U,U1,V,V1,W,W1).

:- mode establish2(+,+,+,-,+,-,+,-).
establish2({},_,U,U,V,V,W,W).
establish2({X},_,U,U,V,V,W,W) :- call(X),!.
%establish2({X},_,U,U,V,V,W,W).
establish2(Hyp,[TS_alist,_,_,_,_],U,U,V,V,W,W) :- has_free_var(Hyp),!,
	fix_free_var(Hyp,TS_alist).
establish2(Hyp,[_,N_list,_,_,_],U,U,V,V,W,W) :- member(Hyp,N_list),!.
establish2(Hyp,[_,N_list,_,_,_],U,U,V,V,W,W) :-
	find_looping(Hyp,N_list),!,fail.
establish2(Hyp,[TS_alist,N_list,_,Lev,_],U,U1,V,V1,W,W1) :-
	add_negation(Hyp,N_list,N_list1),!,
	rewrite(Hyp,X,[TS_alist,N_list1,[],Lev,t],U,U1,V,V1,W,W1),!,
	X = t,!.

% Fixing Free Variables

:- mode fix_free_var(+,+).
fix_free_var(Hyp,TS_alist) :- assume_t(Hyp,HypTS),!,member(HypTS,TS_alist),!.

:- public has_free_var/1.
has_free_var(X) :- var(X),!.
has_free_var([A|D]) :- (has_free_var(A); has_free_var(D)),!.

:- mode assume_t(+,-).
assume_t([not,X],Ass) :- !,assume_f(X,Ass),!.
assume_t(X,X=2).

:- mode assume_f(+,-).
assume_f([not,X],Ass) :- !,assume_t(X,Ass),!.
assume_f(X,X=1).

:- mode find_looping(+,+).
find_looping(_,[]) :- !,fail.  
find_looping(New,N_list) :- negate(New,NN), member(NN,N_list),!.
find_looping(New,N_list) :- one_of(Old,N_list),
	atom_of(New,NewAtom),
	atom_of(Old,OldAtom),
	elaboration(OldAtom,NewAtom),!.

:- mode add_negation(+,+,-).
add_negation([not,Hyp],N_list,[Hyp|N_list]).
add_negation(Hyp,N_list,[[not,Hyp]|N_list]).

:- mode atom_of(+,-).
atom_of([not,X],Y) :- !,atom_of(X,Y),!.
atom_of(X,X).

:- mode elaboration(+,+).
elaboration(X,X).
elaboration(Old,New) :-
	(bagof(X,fn_in_term(X,Old),Oldlist); Oldlist=[]),!,
	(bagof(Y,fn_in_term(Y,New),Newlist); Newlist=[]),!,
	length(Oldlist,Oldnum), length(Newlist,Newnum),	Oldnum =< Newnum,!,
	worse_than(Old,New),!.

:- mode fn_in_term(?,+).
fn_in_term(F,[F|_]) :- atomic(F).
fn_in_term(F,[_|Args]) :- fn_in_args(F,Args).

:- mode fn_in_args(?,+).
fn_in_args(F,[Arg|_]) :- fn_in_term(F,Arg).
fn_in_args(F,[_|Args]) :- fn_in_args(F,Args).

:- mode worse_than(+,+).
worse_than(Old,New) :- variable(Old), Old\==New, var_in_term(Old,New),!.

worse_than(Old,New) :- \+variable(Old), \+variable(New),
	worse_than1(Old,New),!.

worse_than1([F1|Args1],[F2|Args2]) :- F1 \== F2,
   subterm_of(Subnew,[F2|Args2]),
  ([F1|Args1] = Subnew; worse_than([F1|Args1],Subnew)),!.

:- mode subterm_of(?,+).
subterm_of(X,[_|Args]) :- one_of(X,Args).
subterm_of(X,[_|Args]) :- subterm_of_args(X,Args).

:- mode subterm_of_args(?,+).
subterm_of_args(X,[Top|_]) :- subterm_of(X,Top).
subterm_of_args(X,[_|Rest]) :- subterm_of_args(X,Rest).

worse_than1([F|OldArgs],[F|NewArgs]) :-
	some_arg_worse(OldArgs,NewArgs),
	non_var_or_exp(OldArgs,NewArgs),
	\+some_arg_worse(NewArgs,OldArgs),!.

:- public some_arg_worse/2.
:- mode some_arg_worse(+,+).
some_arg_worse([OldArg|_],[NewArg|_]) :- worse_than(OldArg,NewArg).
some_arg_worse([_|OldArgs],[_|NewArgs]) :- some_arg_worse(OldArgs,NewArgs).

:- mode non_var_or_exp(+,+).
non_var_or_exp([OldArg|OldArgs],[NewArg|NewArgs]) :-
	((\+var_or_exp(NewArg), true); var_or_exp(OldArg)),!,
	non_var_or_exp(OldArgs,NewArgs),!.
non_var_or_exp([],[]).

:- public var_or_exp/1.
:- mode var_or_exp(+).
var_or_exp(Term) :- (variable(Term) ; explicit_value(Term)),!.

:- mode expl_rewrite_lemma(+,+,+).
expl_rewrite_lemma(_,_,_) :- (dumbly; \+expl_rewrite),!.
expl_rewrite_lemma(Type,Name,Lev) :-
	I is Lev*3, tab(I), write('rewriting with the '),
	write(Type),write(': '), wwrite(Name,reverse),nl,!.

% EOF rewrite.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public set_equalp/2.
:- mode set_equalp(+,+).
set_equalp([A1|A],B) :- !,set_delete(B,A1,B1),set_equalp(A,B1),!.
set_equalp(X,X).

:- public set_memberp/2.
:- mode set_memberp(+,+).
set_memberp(A,[B|_]) :- set_equalp(A,B),!.
set_memberp(A,[_|B]) :- set_memberp(A,B),!.

:- public set_delete/3.
:- mode set_delete(+,+,-).
set_delete([A1|A],B,C) :- set_equalp(A1,B),!, set_delete(A,B,C),!.
set_delete([A1|A],B,[A1|C]) :- set_delete(A,B,C),!.
set_delete([],_,[]).

:- public subsetp/2.
:- mode subsetp(+,+).
subsetp([A1|A],B) :- set_memberp(A1,B),subsetp(A,B),!.
subsetp([],_).

:- public union/3.
:- mode union(+,+,-).
union([A1|A],B,C) :- set_memberp(A1,B),!,union(A,B,C),!.
union([A1|A],B,[A1|C]) :- union(A,B,C),!.
union([],B,B).

:- public intersection/3.
:- mode intersection(+,+,-).
intersection([A1|A],B,[A1|C]) :- set_memberp(A1,B),!,intersection(A,B,C),!.
intersection([_|A],B,C) :- intersection(A,B,C),!.
intersection([],_,[]).

:- public set_equal/2.
:- mode set_equal(+,+), set_equal1(+,+).
set_equal(A,B) :- var(A), var(B),!, A==B.
set_equal(A,_) :- var(A),!, fail.
set_equal(_,B) :- var(B),!, fail.
set_equal(A,A) :- atomic(A).
set_equal(X,Y) :- X=..[F|A],Y=..[F|A].
set_equal(A,[_|_]) :- atomic(A),!, fail.
set_equal([_|_],B) :- atomic(B),!, fail.
set_equal(A,B) :- set_equal1(A,B),!, set_equal1(B,A).

	set_equal1([A1|A],B) :- set_member(A1,B),!, set_equal1(A,B).
	set_equal1([],_).

:- public set_member/2.
:- mode set_member(+,+).
set_member(A,[B|C]) :- var(A), var(B),!, (A==B; set_member(A,C)).
set_member(A,[B|_]) :- set_equal(A,B).
set_member(A,[_|B]) :- set_member(A,B).

:- public subset/2.
:- mode subset(?,+).
subset([A1|A],B) :- set_member(A1,B), subset(A,B).
subset([],_).

:- public var_union/3.
:- mode var_union(+,+,-).
var_union([A1|A],B,C) :- set_member(A1,B),!,var_union(A,B,C),!.
var_union([A1|A],B,[A1|C]) :- var_union(A,B,C),!.
var_union([],B,B).

% EOF set.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- mode shell_(+).
shell_(S:R/BTM) :- bit_place(_,BP),
	( (BP=8'200000,nl,error('Too many shells.'),!,fail);
	  ( BP1 is BP<<1,
	    S=..[C|Args], length(Args,N), generate_vars(1,N,Xs),
	    ((N=0, asserta(bit_place([S],BP1))); asserta(bit_place(R,BP1))),
	    arg_list(Args,Args1,As,TRs,DVs,AXs,X),
	    add_shell( C/N,Args1,R,BTM,As,TRs,DVs,AXs,X,Xs ),
	    pp_shell(S:R/BTM),nl,write('is accepted.'),nl)),!.
shell_(S:R) :- shell_(S:R/{}),!.
shell_(X) :-
	error('Wrong shell definition.'),
	pp_shell(X),nl,write('is rejected.'),nl,!.

:- mode arg_list(+,-,-,-,-,+).
arg_list([ATR/DV|Args],[ATR/DV|Args1],
	 [A|As],[TR|TRs],[DV|DVs],[[A,X]|AXs],X) :-
	(ATR=A:TR; (ATR=A,TR=t)),
	arg_list(Args,Args1,As,TRs,DVs,AXs,X),!.
arg_list([],[],[],[],[],[],_).

:- mode add_shell(+,+,+,+,+,+,+,+,+,+).
add_shell( C/N,Args,R,BTM,As,TRs,DVs,AXs,X,Xs ) :-
	nl,write('Adding the following axiom(s)...'),nl,
/* 0 */ add_axiom_A_C(			C/N,Args,Xs,[C|Xs]),
/* 1 */ add_axiom_A_not_R(		C/N,Args,R),
/* 2 */ add_axiom_A_type_restriction(	C/N,Args,[],Xs,[C|Xs]),
/* 3 */ add_axiom_A_BTM(		C/N,Args,BTM),
/* 4 */ add_axiom_A_lessp(		C/N,Args,R,BTM),
/* 5 */ add_axiom_C_equal(		C/N,Args,Xs),
/* 6 */ add_axiom_C_A(			C/N,As,DVs,AXs,X,R,BTM),
/* 7 */ add_axiom_A_elim(		C/N,As,DVs,AXs,X,R,BTM),
/* 8 */ add_axiom_count_C(		C/N,Args,Xs),
/* 9 */ add_axiom_count_BTM(		C/N,BTM),
	((N=0,
	  assertz( constructor(C) ),
	  assertz( recognizer_constructor([R,C]) ));
	 (assertz( constructor([C|Xs]) ),
	  assertz( constructor_TRs([C|Xs],TRs) ),
	  assertz( recognizer_constructor([R,[C|Xs]]) ))),
	assertz( recognizer([R,X]) ),
	assertz_A(Args),
	(BTM={};
	 (assertz( bottom_object(BTM) ),
	  assertz( recognizer_bottom([R,BTM]) ),
	  assertz( constructor_bottom([C|Xs],BTM) ))),!.

:- mode add_axiom_A_C(+,+,+,+).
/* 0 */ add_axiom_A_C(C/N,[ATR/DV|As],[X|Xs],CX) :-
	((ATR=A:TR,RHS=[if,[TR,X],X,DV]); (ATR=A,RHS=X)),
	asserta(rewrite_lemma([A,CX],RHS,t,no,
		(axiom),[A,C])),
	Body=[equal,[A,CX],RHS],
	numbervars(Body,0,_),
	nl,pp_((axiom),[A,C],rewrite,Body),
	add_axiom_A_C(C/N,As,Xs,CX),!.
%
% e.g.		[equal,[sub1,[add1,X]],
%		       [if,[numberp,X],X,0]]
%
add_axiom_A_C(_,[],[],_).

:- mode add_axiom_A_not_R(+,+,+).
/* 1 */ add_axiom_A_not_R(C/N,[ATR/DV|As],R) :-
%	(ATR=A:TR; ATR=A),
	(ATR=A:_; ATR=A),
	asserta(rewrite_lemma([A,X],DV,[[[not,[R,X]]]],no,
		(axiom),[A,not,R])),
	Body=[implies,[not,[R,X]],[equal,[A,X],DV]],
	numbervars(Body,0,_),
	nl,pp_((axiom),[A,not,R],rewrite,Body),
	add_axiom_A_not_R(C/N,As,R),!.
%
% e.g.		[implies,[not,[listp,X]],
%			 [equal,[car,X],nil]]
%
add_axiom_A_not_R(_,[],_).

:- mode add_axiom_A_type_restriction(+,+,+,+,+).
/* 2 */ add_axiom_A_type_restriction(C/N,[ATR/DV|As],Xh,[X|Xt],CX) :-
	((ATR=A:TR,
	append(Xh,[DV|Xt],DVx),
	asserta(rewrite_lemma(CX,[C|DVx],[[[not,[TR,X]]]],no,
		(axiom),type_restriction(A))),
	Body=[implies,[not,[TR,X]],[equal,CX,[C|DVx]]],
	numbervars(Body,0,_),
	nl,pp_((axiom),type_restriction(A),rewrite,Body)); true),
	append(Xh,[X],Xh1),
	add_axiom_A_type_restriction(C/N,As,Xh1,Xt,CX),!.
%
% e.g.		[implies,[not,[numberp,X]],
%			 [equal,[add1,X],
%				[add1,0]]]
%
add_axiom_A_type_restriction(_,[],_,[],_).

:- mode add_axiom_A_BTM(+,+,+).
/* 3 */ add_axiom_A_BTM(_,_,{}).
	add_axiom_A_BTM(C/N,[ATR/DV|As],BTM) :-
%	(ATR=A:TR; ATR=A),
	(ATR=A:_; ATR=A),
	asserta(rewrite_lemma([A,BTM],DV,t,no,
		(axiom),[A,BTM])),
	Body=[equal,[A,BTM],DV],
	numbervars(Body,0,_),
	nl,pp_((axiom),[A,BTM],rewrite,Body),
	add_axiom_A_BTM(C/N,As,BTM),!.
%
% e.g.		[equal,[sub1,0],0]
%
add_axiom_A_BTM(_,[],_).

:- mode add_axiom_A_lessp(+,+,+,+).
%/* 4 */ add_axiom_A_lessp(C/N,[ATR/DV|As],R,BTM) :-
%	(ATR=A:TR; ATR=A),
/* 4 */ add_axiom_A_lessp(C/N,[ATR/_|As],R,BTM) :-
	(ATR=A:_; ATR=A),
	((BTM={},LHS=[R,x]); LHS=[and,[R,x],[not,[equal,x,BTM]]]),
	Body=[lessp,[count,[A,x]],[count,x]],
	numbervars(Body,0,_),
	formula_to_clauses(LHS,LHSC),
	p_vars([x],[LHSC,Body],_,P), P=[P_LHS,P_Body],
	asserta(induction_lemma(P_Body,P_LHS,(axiom),lessp(A))),
	nl,pp_((axiom),lessp(A),induction,[implies,LHS,Body]),
	add_axiom_A_lessp(C/N,As,R,BTM),!.
%
% e.g.		[implies,[and,[numberp,X],
%			      [not,[equal,X,0]]],
%			 [lessp,[count,[sub1,X]],[count,[sub1,X]]]]
%
add_axiom_A_lessp(_,[],_,_).

:- mode add_axiom_C_equal(+,+,+).
/* 5 */ add_axiom_C_equal(C/N,Args,Xs) :- (N=0; (
	generate_vars(1,N,Ys),
	tr_list(Xs,Ys,Args,TRlist),
	(TRlist=[RHS]; RHS=[and|TRlist]),
	asserta(rewrite_lemma([equal,[C|Xs],[C|Ys]],RHS,t,no,
		(axiom),equal(C))),
	Body=[equal,[equal,[C|Xs],[C|Ys]],RHS],
	numbervars(Body,0,_),
	nl,pp_((axiom),equal(C),rewrite,Body))),!.
%
% e.g.		[equal,[equal,[add1,X1],
%			      [add1,Y1]].
%		       [if,[numberp,X1],
%			   [if,[numberp,Y1],
%			       [equal,X1,Y1],
%			       [equal,X1,0]],
%			   [if,[numberp,Y1],
%			       [equal,0,Y1],
%			       t]]]]
%

:- mode tr_list(+,+,+,-).
%tr_list([X|Xs],[Y|Ys],[A:TR/DV|As],
tr_list([X|Xs],[Y|Ys],[_:TR/DV|As],
	[[if,[TR,X],
	     [if,[TR,Y],
		 [equal,X,Y],
		 [equal,X,DV]],
	     [if,[TR,Y],
		 [equal,DV,Y],
		 t]]|TRlist]) :- !,
	tr_list(Xs,Ys,As,TRlist),!.
%tr_list([X|Xs],[Y|Ys],[A/DV|As],[[equal,X,Y]|TRlist]) :- !,
tr_list([X|Xs],[Y|Ys],[_|As],[[equal,X,Y]|TRlist]) :- !,
	tr_list(Xs,Ys,As,TRlist),!.
tr_list([],[],[],[]).

:- mode add_axiom_C_A(+,+,+,+,+,+,+).
/* 6 */ add_axiom_C_A(C/N,As,DVs,AXs,X,R,BTM) :- (N=0; (
	((BTM={},LHS=[R,X]); LHS=[and,[R,X],[not,[equal,X,BTM]]]),
	asserta(rewrite_lemma([C|AXs],[if,LHS,X,[C|DVs]],t,no,
		(axiom),[C,As])),
	Body=[equal,[C|AXs],[if,LHS,X,[C|DVs]]],
	numbervars(Body,0,_),
	nl,pp_((axiom),[C,As],rewrite,Body))),!.
%
% e.g.		[equal,[add1,[sub1,X]],
%		       [if,[and,[numberp,X],
%				[not,[equal,X,0]]],
%			   X,
%			   [add1,0]]]
%

:- mode add_axiom_A_elim(+,+,+,+,+,+,+).
%/* 7 */ add_axiom_A_elim(C/N,As,DVs,AXs,X,R,BTM) :- (N=0; (
/* 7 */ add_axiom_A_elim(C/N,As,_,AXs,X,R,BTM) :- (N=0; (
	((BTM={},LHS=[R,X]); LHS=[and,[R,X],[not,[equal,X,BTM]]]),
	asserta(elimination_lemma((axiom),elim(As),
		[implies,LHS,
			 [equal,[C|AXs],X]])),
	Body=	[implies,LHS,
			 [equal,[C|AXs],X]],
	numbervars(Body,0,_),
	nl,pp_((axiom),elim(As),elimination,Body))),!.
%
% e.g.		[implies,[and,[numberp,X],
%			      [not,[equal,X,0]]],
%			 [equal,[add1,[sub1,X]],X]]).
%

:- mode add_axiom_count_C(+,+,+).
/* 8 */ add_axiom_count_C(C/N,Args,Xs) :-
	((N=0,LHS=C); LHS=[C|Xs]),
	count_list(Args,Xs,Counts),
	((Counts=[],RHS=zero); Counts=[RHS]; RHS=[plus|Counts]),
	asserta(rewrite_lemma([count,LHS],[add1,RHS],t,no,
		(axiom),count(C))),
	Body=[equal,[count,LHS],[add1,RHS]],
	numbervars(Body,0,_),
	nl,pp_((axiom),count(C),rewrite,Body),!.
%
% e.g.		[equal,[count,[cons,X1,X2]],
%		       [add1,[plus,[count,X1],[count,X2]]]]
%

:- mode count_list(+,+,-).
%count_list([A:TR/DV|As],[X|Xs],[[if,[TR,X],[count,X],zero]|Counts]) :- !,
count_list([_:TR/_|As],[X|Xs],[[if,[TR,X],[count,X],zero]|Counts]) :- !,
	count_list(As,Xs,Counts),!.
count_list([_|As],[X|Xs],[[count,X]|Counts]) :- !,
	count_list(As,Xs,Counts),!.
count_list([],[],[]).

:- mode add_axiom_count_BTM(+,+).
%/* 9 */ add_axiom_count_BTM(C/N,BTM) :-
/* 9 */ add_axiom_count_BTM(_,BTM) :-
	(BTM={};
	 (asserta(rewrite_lemma([count,BTM],zero,t,no,
		(axiom),count(BTM))),
	  Body=[equal,[count,BTM],zero],
	  numbervars(Body,0,_),
	  nl,pp_((axiom),count(BTM),rewrite,Body))),!.
%
% e.g.		[equal,[count,0],0]
%

:- mode assertz_A(+).
%assertz_A([ATR/DV|As]) :-
assertz_A([ATR/_|As]) :-
	((ATR=A:TR,assertz(type_of_A([TR,[A,X]]))); ATR=A),
	assertz(accessor([A,X])),
	assertz_A(As),!.
assertz_A([]).

:- mode pp_shell(+).
pp_shell(S:R/BTM) :-
	nl,wwrite('Shell ',bold),S=..[C|Args],wwrite(C,reverse),
	(Args=[];
	 (wwrite('(',reverse),pp_C(Args),wwrite(')',reverse),nl,
	  tab(8),write('Accessor(:Type-restriction)/Default-value: '),
	  ((Args=[A],write(A),nl);
	   (nl,pp_As(Args))))),
	tab(8),write('Recognizer: '),wwrite(R,reverse),nl,
	(BTM={};(tab(8),write('Bottom-object: '),wwrite(BTM,reverse),nl)),!.
pp_shell(S) :- \+S=_:_,
	nl,wwrite('Shell ',bold),wwrite(S,reverse),nl,!.

:- mode pp_C(+).
pp_C([_|Args]) :-
	wwrite('_',reverse),(Args=[];(wwrite(',',reverse),pp_C(Args))),!.

:- mode pp_As(+).
pp_As([A|As]) :-
	tab(16),wwrite(A,reverse),nl,(As=[];pp_As(As)),!.

% EOF shell.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public simplify/0.
simplify :- retract(pool(simplification,X)),!,
	pp_clause(X,'Simplifying...'),
	simplify_clause(X),!.
simplify.

:- mode simplify_clause(+).
simplify_clause([ID|Path]:Clause) :-
	((Clause=[_],!,C2=Clause);
	 (delete_notequal([],Clause,[],C1,_),!,reverse(C1,C2))),
	setof(C,call_in_terms(C,C2),Terms),
	asserta(rewrite_env(Path:Terms)),
	simplify_step([],C2,C3),!,
	abolish(rewrite_env,1),
	cleanup(C3,C4),!,
	((C4=f,!,fail);
%	 (C4=t,(dumbly; nl,wwrite('The conjecture is true.',bold),nl));
	 (C4=t,(dumbly;
		(nl,wwrite('Conjecture ',bold),pp_path(11,[ID]),nl,
		 nl,wwrite('    is true.'),nl)));
	 (C4=[Clause],
	  pool_(heuristics,[ID|Path]:[Clause],z));
	 pool_(simplification,[s(ID)|Path]:C4,a)),!.

:- mode	delete_notequal(+,+,+,-,-).
delete_notequal(H,[[not,[equal|LR]]|Cl],T,NH,NT) :-
	(LR=[X,Y]; LR=[Y,X]), variable(X), \+var_in_term(X,Y),!,
	replace_var(X,Y,[['H'|H],['C'|Cl],['T'|T]],HCT),!,
	HCT=[['H'|NewH],['C'|NewCl],['T'|NewT]],!,
	delete_notequal(NewH,NewCl,NewT,NH,NT),!.
delete_notequal(H,[Top|Cl],T,NH,NT) :- delete_notequal([Top|H],Cl,T,NH,NT),!.
delete_notequal(H,[],T,H,T).

:- mode simplify_step(+,+,-).
simplify_step(Head,[L1|Tail],Result) :-
	assume_false(Head,Tail,NewHead,NewTail,XT,[],Hyp,[],TS_alist),!,
	((XT=(_=t),
	  nl,wwrite(trivial),nl,
	  Result=t);
	 (
	  (((expl_expand;expl_rewrite),pp_hyp(Hyp),nl,
	     wwrite('rewriting '),pp(10,L1),nl); true),!,
	  rewrite(L1,NewL1,[TS_alist,[],[],0,tf],[],_,[],_,[],W),!,
	  (((expl_expand;expl_rewrite),wwrite('--------------------------------------------------')); true),!,
	  expl_simp_step(Hyp,L1,NewL1,W),!,
	  ((NewL1=t,!,Result=t);
	   (NewL1=f,!,simplify_step(NewHead,NewTail,Result));
	   (formula_to_clauses(NewL1,Cls),!,
	    ((Cls=[[L]],!,
	      simplify_step([L|NewHead],NewTail,Result));
	     map_simplify_step(NewHead,Cls,NewTail,t,Result)))))),!.
simplify_step(X,[],[Y]) :- !,reverse(X,Y),!.

:- mode assume_false(+,+,-,-,-,+,-,+,-).
assume_false([X|H],T,NH,NT,XT,Hin,Hout,ASin,TS_alist) :-
	assume_false1(X,A,ASin),!,
	((A=t,!,XT=(X=t),!,TS_alist=[]);
	 (A=f,!,assume_false(H,T,NH,NT,XT,Hin,Hout,ASin,TS_alist));
	 (A=or(_,AS1),!,add_ts_assumption(AS1,ASin,AS2),!,
	  NH=[X|NH1],!,Hout=[X|H1],!,
	  assume_false(H,T,NH1,NT,XT,Hin,H1,AS2,TS_alist))),!.
assume_false([],[],[],[],[],H,H,A,A).
assume_false([],T,[],NT,XT,Hin,Hout,ASin,TS_alist) :-
	assume_false(T,[],NT,_,XT,Hin,Hout,ASin,TS_alist),!.

:- mode assume_false1(+,-,+).
assume_false1([not,X],A,Ass) :- !,assume_true1(X,B,Ass),!,
	((B=t,!,A=f); (B=f,!,A=t); (B=or(L,R),!,A=or(R,L))),!.
assume_false1(X,A,Ass) :- ts_assumption(X,A,Ass),!.

:- mode assume_true1(+,-,+).
assume_true1([not,X],A,Ass) :- !,assume_false1(X,B,Ass),!,
	((B=t,!,A=f); (B=f,!,A=t); (B=or(L,R),!,A=or(R,L))),!.
assume_true1(X,A,Ass) :- ts_assumption(X,A,Ass),!.

:- mode map_simplify_step(+,+,+,+,-).
map_simplify_step(Head,[Cl|Cls],Tail,R0,Result) :-
	delete_notequal(Head,Cl,Tail,Head1,Tail1),!,
	simplify_step(Head1,Tail1,R1),!,
	((R1=f,!,fail);
	 (((R0=t,R2=R1); (R1=t,R2=R0); append(R0,R1,R2)),!,
	  map_simplify_step(Head,Cls,Tail,R2,Result))),!.
map_simplify_step(_,[],_,R,R).

:- mode expl_simplify_step(+,+,+,+).
expl_simp_step(_,_,_,_) :- dumbly,!.
expl_simp_step(Hyp,L1,NewL1,W) :- L1\==NewL1,brk,
	pp_hyp(Hyp),
	(W=[]; (nl,reverse(W,W1),pp_W(W1))),
	(( NewL1=t,
	  nl,wwrite('literal'),nl,
	  nl,tab(8),pp(8,L1),nl,
	  nl,wwrite('    is true.'),nl) ;
	 ( NewL1=f,
	  nl,wwrite('literal'),nl,
	  nl,tab(8),pp(8,L1),nl,
	  nl,wwrite('    is false.'),nl) ;
	 (nl,wwrite('we rewrite literal'),nl,
	  nl,tab(8),pp(8,L1),
	  nl,wwrite('    to'),
	  nl,tab(8),pp(8,NewL1),nl)),!.
expl_simp_step(_,_,_,_).

:- mode pp_hyp(+).
pp_hyp(Hyp) :- neg_all(Hyp,NHyp),!,
	( NHyp=[] ;
	  (NHyp=[NH],nl,wwrite('Under the assumption'),nl,
	   nl,tab(8),pp(8,NH),nl) ;
	  (nl,wwrite('Under the assumption'),nl,
	   nl,tab(8),pp(8,[and|NHyp]),nl) ),!.

:- mode pp_W(+).
pp_W([e(L,X)|W]) :- I is L*4+4,tab(I),
	wwrite('expanding '),I1 is I+10,pp(I1,X),nl,
	pp_W(W),!.
pp_W([r(L,T,N)|W]) :- I is L*4+4,tab(I),
	wwrite('applying '),wwrite(T),
	wwrite(': '),wwrite(N),nl,pp_W(W),!.
pp_W([]).

% EOF simp.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public explicit_value/1.
:- mode explicit_value(+).
explicit_value(t).
explicit_value(f).
explicit_value([$,_]).
explicit_value(X) :- bottom_object(X),!.
explicit_value(X) :- constructor_TRs(X,TRs),!,X=[_|As],!,satisfy_TR(TRs,As),!.

:- public explicit_values/1.
:- mode explicit_values(+).
explicit_values([V|Vs]) :- explicit_value(V),!,explicit_values(Vs),!.
explicit_values([]).

:- mode satisfy_TR(+,+).
satisfy_TR([R|TRs],[X|Xs]) :-
	((recognizer_constructor([R,X]),!,
	  constructor_TRs(X,TRs1),!,X=[_|As1],!,satisfy_TR(TRs1,As1),!);
	 recognizer_bottom([R,X])),!,
	satisfy_TR(TRs,Xs),!.
satisfy_TR([],[]).

:- mode explicit_value_templates(+).
explicit_value_templates([A|As]) :-
	(variable(A); explicit_value_template(A)),!,
	explicit_value_templates(As),!.
explicit_value_templates([]).

:- public explicit_value_template/1.
:- mode explicit_value_template(+).
explicit_value_template(X) :- bottom_object(X),!.
explicit_value_template(X) :- constructor(X),!,X=[_|As],!,
	explicit_value_templates(As),!.

:- public call_in_terms/2.
call_in_terms(_,T) :- var(T),!,fail.
call_in_terms(X,[T|_]) :- call_in_term(X,T).
call_in_terms(X,[_|Ts]) :- call_in_terms(X,Ts).

:- public call_in_term/2.
call_in_term(_,T) :- var(T),!,fail.
call_in_term([F|Ts],[F|Ts]).
call_in_term(X,[_|Ts]) :- call_in_terms(X,Ts).

:- public recursive_calls/3.
:- mode recursive_calls(+,+,-).
recursive_calls(F,X,R) :- setof(C,recursive_call(F,X,C),R),!.
recursive_calls(_,_,[]).

:- public recursive_call/3.
:- mode	recursive_call(+,+,-).
recursive_call(_,X,_) :- var(X),!,fail.
recursive_call(_,[pack,_],_) :- !,fail.
recursive_call(_,[quote,_],_) :- !,fail.
recursive_call(F/N,[F|As],[F|As]) :- length(As,N).
recursive_call(F,[_|As],C) :- recursive_call_args(F,As,C).

:- mode	recursive_call_args(+,+,-).
recursive_call_args(F,[A|_],C) :- recursive_call(F,A,C).
recursive_call_args(F,[_|As],C) :- recursive_call_args(F,As,C).

%
%	mtos(+M_expression,-S_expression).
%

:- public mtos/2.
:- mode mtos(?,-).
mtos(X,X) :- var(X),!.
mtos("nil",nil).
mtos([X|Y],[pack,N]) :- name(N,[X|Y]),!.
mtos($X,[$,X]).
mtos({X},{X}).
mtos(X,Z) :- abbrev_(X=Y), mtos(Y,Z),!.
mtos(M,[F|AS]) :- M=..[F,A1|A], map_mtos([A1|A],AS),!.
mtos(X,X).

:- mode map_mtos(+,-).
map_mtos([A1|A],[A1S|AS]) :- mtos(A1,A1S),!, map_mtos(A,AS),!.
map_mtos([],[]).

:- public mtos1/2.
:- mode mtos1(?,-).
mtos1(X,X) :- var(X),!.
mtos1({X},{X}).
mtos1(M,[F|AS]) :- M=..[F,A1|A], map_mtos1([A1|A],AS),!.
mtos1(X,X).

:- mode map_mtos1(+,-).
map_mtos1([A1|A],[A1S|AS]) :- mtos1(A1,A1S),!, map_mtos1(A,AS),!.
map_mtos1([],[]).

%
%	stom(+S_expression,-M_expression).
%

:- public stom/2.
:- mode stom(?,-).
stom(X,X) :- var(X),!.
stom({X},{X}).
stom([F|A],M) :- map_stom(A,AM),!, M=..[F|AM],!.
stom(X,X).

:- mode map_stom(+,-).
map_stom([A1|A],[A1M|AM]) :- stom(A1,A1M),!, map_stom(A,AM),!.
map_stom([],[]).

% EOF term.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public time/1, time/4.

time(P) :- statistics(runtime,[_,_]),call(P),
	statistics(runtime,[_,E]),time(E,Min,Sec,MSec),
	write_time(Min,Sec,MSec).

:- mode time(+,-,-,-).
time(xwd(T1,T2),Min,Sec,MSec) :- T2 < 0,!,
	T1new is T1 + 1,!,
	time_posi(xwd(T1new,0),Min1,Sec1,MSec1),!,
	T2new is T2 * -1,!,
	time_posi(xwd(0,T2new),Min2,Sec2,MSec2),!,
	time_diff(Min1,Sec1,MSec1,Min2,Sec2,MSec2,Min,Sec,MSec).
time(xwd(T1,T2),Min,Sec,MSec) :- !,time_posi(xwd(T1,T2),Min,Sec,MSec),!.
time(E,Min,Sec,MSec) :- S1 is E/1000,
	Min is S1/60, Sec is S1 mod 60, MSec is E mod 1000,!.

:- mode time_posi(+,-,-,-).
time_posi(xwd(T1,T2),Min,Sec,MSec) :- !,
	T3 is T2 mod 1000,
	T4 is 144*T1+T3,
	MSec is T4 mod 1000,
	S1 is 22*T1+T2/1000+T4/1000,
	Sec is S1 mod 60,
	Min is 4*T1 + S1/60.

:- mode time_diff(+,+,+,+,+,+,-,-,-).
time_diff(Min1,Sec1,MSec1,Min2,Sec2,MSec2,Min,Sec,MSec) :-
	MSec1 >= MSec2,!,
	MSec is MSec1 - MSec2,
	time_diff1(Min1,Sec1,Min2,Sec2,Min,Sec).
time_diff(Min1,Sec1,MSec1,Min2,Sec2,MSec2,Min,Sec,MSec) :-
	MSec is MSec1 + 1000 - MSec2,
	Sec2new is Sec2 + 1,
	time_diff1(Min1,Sec1,Min2,Sec2new,Min,Sec).

:- mode time_diff1(+,+,+,+,-,-).
time_diff1(Min1,Sec1,Min2,Sec2,Min,Sec) :-
	Sec1 >= Sec2,!,
	Sec is Sec1 - Sec2,
	Min is Min1 - Min2.
time_diff1(Min1,Sec1,Min2,Sec2,Min,Sec) :-
	Sec is Sec1 + 1000 - Sec2,
	Min is Min1 - Min2 - 1.

:- mode write_time(+,+,+).
write_time(0,Sec,MSec) :-
	nl,write('                             '),
	write(Sec),write('.'),
	write_MSec(MSec),
	write('  seconds'),nl,!.

write_time(Min,Sec,MSec) :- 
	nl,write('                '),
	write(Min),write('  minuites  '),
	write(Sec),write('.'),
	write_MSec(MSec),
	write('  seconds'),nl,!.

:- mode write_MSec(+).
write_MSec(MS) :- MS>99,write(MS).
write_MSec(MS) :- MS>9,write(0),write(MS).
write_MSec(MS) :- write(00),write(MS).

% EOF time.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public banner/0.
banner :- wwrite('BMTP in Prolog V4.0 (Aug. 1985)').

:- dynamic abbrev_/1, abbrev__/1.

:- public (abbrev)/1.
:- mode abbrev(+).
abbrev(X=Y) :-
	(setof(V,var_in_pterm(V,X),Vs); Vs=[]),!,
	p_vars(Vs,X=Y,_,Z),
	mtos1(X,XX), mtos(Y,YY),
	p_vars(Vs,XX=YY,_,W),
	asserta(abbrev_(Z)),
	asserta(abbrev__(W)),
	nl,wwrite('Abbreviation ',[reverse,bold]),wwrite(X=Y),nl,
	nl,wwrite('is accepted.'),nl.

:- public (shell)/1.
:- mode shell(+).
%%RB: is a built-in in SICStus. Not called anyway.
%shell(X) :- shell_(X).

:- public (definition)/1.
:- mode definition(+).
definition(X) :- definition_(X).

:- public (axiom)/1.
:- mode axiom(+).
axiom(X) :- add_((axiom),X).

:- public (lemma)/1.
:- mode lemma(+).
lemma(X) :- add_((lemma),X).

:- dynamic proving/1.

:- public (theorem)/1.
:- mode	theorem(+).
theorem(X) :-
	nl,pp_((theorem),X),asserta(proving(X)),!,b,!,
	((proving(X),abolish(dumbly,0),X=(NL=B),NL=..[Name|_],mtos(B,Body),!,
	  prove([theorem(Name)]:Body));
	 true),!.

:- public p/0, pr/0.
p :- pr.
pr :- pool(_,_),!, pp_pool.
pr :- (#X),pp_problem(X),fail.
pr.

:- public qq/0.
qq :- retract(pool(_,_:f)),!,abolish(continue,0),!,fail.
qq :- retract(pool(_,_:t)),!,qq,!.
qq :- remove_subsumed,!,qq,!.
qq :- pool(simplification,_),!,simplify,!,qq,!.
qq :- pool(heuristics,_),!,heuristics,!,qq,!.
qq :- retract(pool(_,_)),!,abolish(continue,0),!,fail.
qq :- (defun_loaded; (nl,wwrite('OK',bold),nl)),!,abolish(continue,0),!.

fail_ :- (dumbly; (nl,wwrite('\aFail!'),nl)),!,fail.

remove_subsumed :-
	pool(Pool,P1:C1), pool(Pool,P2:C2), P1\==P2, C1==C2,
%	pp_clause(P1:C1,'is subsumed by'),
%	pp_clause(P2:C2,''),
	retract(pool(Pool,P1:C1)),
	remove_subsumed,!.

%:- public r/0.
%r :- s,proving(_),!,r.
%r.

%:- public x/0.
%x :- listing((#)).
%x :- (#X),pp_problem(X),fail.
%x.

:- mode pp_problem(+).
pp_problem(definition(H=B)) :- mtos(B,Body),
	nl,pp_def(H,Body),!.
pp_problem(axiom(H=B)) :- H=..[Name|Labels],mtos(B,Body),
	nl,pp_(axiom,Name,Labels,Body),!.
pp_problem(lemma(H=B)) :- H=..[Name|Labels],mtos(B,Body),
	nl,pp_(lemma,Name,Labels,Body),!.
pp_problem(theorem(H=B)) :- H=..[Name|Labels],mtos(B,Body),
	nl,pp_(theorem,Name,Labels,Body),!.
pp_problem(X) :- nl,wwrite(X),nl.

:- dynamic (@)/1.

%:- public y/0.
%y :- (#X),!,y(X),((retract((#X)),assertz((@X))); true),!.

%:- public s/0.
%s :- retract(pool(_,_:f)),!,fail_.
%s :- retract(pool(_,_:t)),!.
%s :- remove_subsumed,!.
%s :- pool(simplification,_),!,simplify,!.
%s :- pool(heuristics,_),!,heuristics,!.
%s :- pool(induction,_),!,induction,!.
%s :- pool(fail,_),!,fail_.
%s :- proving(X),!,(dumbly; (nl,wwrite('Q.E.D.',bold),nl)),!,
%	add_((theorem),X),abolish(proving,1),!.

:- public n/0, next/0.
n:- next.
next :- proving(_),!, next1.
next :- (#X),!,next_com(X),((retract((#X)),assertz((@X))); true),!.

next1 :- retract(pool(_,_:f)),!,fail_.
next1 :- retract(pool(_,_:t)),!.
next1 :- remove_subsumed,!.
next1 :- pool(simplification,_),!,simplify,!.
next1 :- pool(heuristics,_),!,heuristics,!.
next1 :- pool(induction,_),!,induction,!.
next1 :- pool(fail,_),!,fail_.
next1 :- proving(X),!,(dumbly; (nl,wwrite('Q.E.D.',bold),nl)),!,
	add_((theorem),X),abolish(proving,1),!.

:- dynamic sw/1.

:- public next_com/1.
:- mode next_com(+).
next_com(pause) :- assert(sw(pause)).
next_com(X) :- integer(X),!,(X<1; (X1 is X-1, next,!,next_com(X1))),!.
next_com(make_env(X)) :- retract((#make_env(X))),make_env(X),!.
next_com(X) :- call(X),!.

%:- public z/0.
%z :- retract(sw(pause)).
%z :- proving(_).
%z :- next,!,z.

:- public b/0.
b :- dumbly,!.
b :- ((on_statistics,nl,statistics); true), brk,!.

:- public continue/0.
:- dynamic continue/0.
:- public c/0.
c :- off_(continue).
c :- on(continue).

:- dynamic expl_rewrite/0, expl_expand/0.
:- public t/0.
t :- off_(expl_rewrite),off_(expl_expand).
t :- on(expl_rewrite),on(expl_expand).

:- public q/0, quit/0.
q :- quit.
quit :- abolish(pool,2),
	((proving(X),add_((theorem),X),abolish(proving,1)); true).

:- public h/0.
h :- help.

:- dynamic vt100_/0.
:- public v/0, vt100/0.
v :- vt100.
vt100 :- off_(vt100_).
vt100 :- on(vt100_).

:- public brk/0.
brk :- continue.
brk :- nl,wwrite('Option (h for help): ',[blink,bold]),ttyflush,
	get_com(C),do_com(C),!.

:- mode off(+), off_(+), on(+).
off(SW) :- abolish(SW,0),nl,wwrite(SW),wwrite(' disabled.'),nl.
off_(SW) :- SW,abolish(SW,0),nl,wwrite(SW),wwrite(' disabled.'),nl.
on(SW) :- (SW; asserta(SW)),nl,wwrite(SW),wwrite(' enabled.'),nl.

:- mode get_com(-).
get_com(C) :- ttyget0(X),!,((X<32,C=[]);(get_com(Cs),C=[X|Cs])),!.

:- dynamic expand_depth/1.

:- mode do_com(+).
do_com("a") :- abort.
do_com("b") :- break.
do_com("c") :- c.
do_com("d") :- nl,prompt(_,'expansion depth: '),read(X),
	integer(X),abolish(expand_depth,1),asserta(expand_depth(X)).
do_com("e") :- e.
do_com("g") :- g.
do_com("h") :- help_com,brk.
do_com("p") :- pp_pool,brk.
do_com("t") :- trace.
do_com("v") :- v,brk.
do_com("@") :- !,nl,prompt(_,'| :- '),read(X),call(X).
do_com(_).

:-public help_com/0.
help_com :- wwrite('\
a	abort\
b	break\
c	toggle continue mode\
e	toggle tracing (rewrite,expand)\
g	give up the proof (but store as lemma)\
h	help\
p	print remaining conjectures\
t	trace\
v	toggle VT100 mode\
@	accept command\
'),!.

:- public (note_env)/1.
:- mode note_env(+).
note_env([F|Fs]) :- note_env(F),!,note_env(Fs),!.
note_env([]).
note_env(-F) :- name_concat(F,env,FE),reconsult(FE),!.
note_env(F) :- name_concat(F,env,FE),consult(FE),!.
note_env(-F) :- name_concat(F,env,FE),see(FE),!, abolish((@)/1),
	do_note_env, seen,
	nl,wwrite(FE),wwrite('has been loaded '),nl.
note_env(F) :- name_concat(F,env,FE),see(FE),!,
	do_note_env, seen,
	nl,wwrite(FE),wwrite(' has been loaded '),nl.

do_note_env :- read(X), (X=end_of_file ; assert(X), do_note_env).

:- public (make_env)/1.
:- mode make_env(+).
make_env([F|Fs]) :- make_env(F),!,make_env(Fs),!.
make_env([]).
make_env(F) :- name_concat(F,env,FE),tell(FE),env,told,
	nl,wwrite(FE),wwrite(' told '),nl,!.

:- mode name_concat(+,+,-).
name_concat(A,B,C) :- name(A,As), name(B,Bs), append(As,Bs,Cs), name(C,Cs),!.

:- public env/0.
env :-
	listing(abbrev_),
	listing(abbrev__),
	listing(bit_place),
	listing(constructor),
	listing(constructor_bottom),
	listing(constructor_TRs),
	listing(bottom_object),
	listing(accessor),
	listing(type_of_A),
	listing(recognizer),
	listing(recognizer_constructor),
	listing(recognizer_bottom),
	listing(rewrite_lemma),
	listing(induction_lemma),
	listing(elimination_lemma),
	listing((definition)/2),
	listing(type_prescription),
	listing(nonrecursive),
	listing(recursive),
	listing(nasty_function),
	listing(measured_subset),
	listing(induction_template),
	listing((@)),
	listing(pool),
	listing(proving),
	listing(var_seed),
	listing(used_variable),
	listing((#)),
!.

%
%	Pretty Printing
%

:- public pp_def/2.
:- mode	pp_def(+,+).
pp_def(FA,Body) :-
	wwrite('Definition ',bold),wwrite(FA),nl,
	nl,wwrite('    =   '),pp(8,Body),nl,!.

:- public pp_TP/1.
:- mode pp_TP(+).
pp_TP(FA) :-
	type_prescription(FA,tp(TS,Vars)),
	type_list(TS,Vars,TL),
	wwrite('    ... type is '),
%	((TL=[T],!,pp(8,T)); pp(12,['OR'|TL])),!,
	pp_TPs(TL),!,
	wwrite(' '),!.

:- mode pp_TPs(+).
pp_TPs([T]) :- wwrite(T,reverse),!.
pp_TPs([T|Ts]) :- wwrite(T,reverse),wwrite(' or '),pp_TPs(Ts),!.


:- mode type_list(+,+,-).
type_list(8'377777,_,[universe]).
type_list(TS,Vars,[boolean|Types]) :- bit_place([t],B1), bit_place([f],B2),
	ts_intersection(TS,B1,B1),
	ts_intersection(TS,B2,B2),
	ts_difference(TS,B1,TS1),
	ts_difference(TS1,B2,TS2),
	type_list(TS2,Vars,Types),!.
type_list(TS,Vars,[T1|Types]) :- bit_place(T1,B1),
	ts_intersection(TS,B1,B1),
	ts_difference(TS,B1,TS1),
	type_list(TS1,Vars,Types),!.
type_list(0,[V1|Vars],[type_of(V1)|Ts]) :-
	type_list(0,Vars,Ts),!.
type_list(0,[],[]).

wwrite(X) :- write(X).

:- public user_help/0.

user_help :-
	see('help.txt'),
	do_help,
	seen.

do_help :- get0(C), (C= -1 ; put(C), do_help).

{F,Files} :- read_prob(F), {Files}.
{F} :- read_prob(F).

read_prob(File) :- name_concat(File,'.bm',InFile),
	see(InFile), do_read_prob, seen.

do_read_prob :- read(X), (X=end_of_file ; assert(X), do_read_prob).

% EOF top.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public type_set/3.
:- mode type_set(+,-,+).

type_set(X,TS,Ass) :- ts_assumed(X,TS,Ass),!.

	:- mode ts_assumed(+,-,+).
	ts_assumed(X,TS,[X=TS|_]).
	ts_assumed(X,TS,[_|Ass]) :- ts_assumed(X,TS,Ass),!.

type_set([equal,_,_],3,_) :- !.
type_set(X,TS,Ass) :-
	(recognizer_constructor([R,X]);
	 recognizer_bottom([R,X]);
	 type_of_A([R,X]);
	 R=[X];
	 (rewrite_lemma([R,X],t,P,no,_,_),
	  establish(P,[Ass,[],[],0,tf],[],_,[],_,[],_))),
	bit_place(R,TS),!.

type_set(X,TS,Ass) :-
	rewrite_lemma([or,[R1,X]|Y],t,P,no,_,_),
	establish(P,[Ass,[],[],0,tf],[],_,[],_,[],_),
	tsa([[R1,X]|Y],0,X,TS),!.

:- public tsa/4.
:- mode tsa(+,+,+,-).
tsa([[R1,X]|Y],TSin,X,TS) :-
	bit_place(R1,TS1),
	ts_union(TSin,TS1,TSout),
	tsa(Y,TSout,X,TS),!.
tsa([],TS,_,TS).
	
type_set([if,T,L,R],TS,Ass) :- !,
	ts_assumption(T,Mustbe,Ass),!,
	((Mustbe=t,!,type_set(L,TS,Ass));
	 (Mustbe=f,!,type_set(R,TS,Ass));
	 (Mustbe=or(L_Ass,R_Ass),!,
		type_set(L,L_TS,L_Ass),!,
		type_set(R,R_TS,R_Ass),!,
		ts_union(L_TS,R_TS,TS))),!.

type_set(X,TS,Ass) :- recognizer(X),!,
	X=[R,Y],!,
	(bit_place(R,RTS);
	 (recognizer_constructor([R,C]), bit_place([C],RTS))),!,
	type_set(Y,YTS,Ass),!,
	((RTS=YTS,!,TS=2);
	 (ts_intersection(RTS,YTS,0),!,TS=1);
	 TS=3),!.
%			[implies,[R,X],[not,[R',X]]]

type_set([F|Args],TS,Ass) :-
	type_prescription([F|Args],tp(TS_F,Terms)),!,
	ts_args(Terms,TS_F,TS,Ass),!.

	:- mode ts_args(+,+,-,+).
	ts_args([T1|Terms],TSp,TS,Ass) :-
		type_set(T1,TS1,Ass),!,
		ts_union(TSp,TS1,TSn),!,
		ts_args(Terms,TSn,TS,Ass),!.
	ts_args([],TS,TS,_).

type_set(_,8'377777,_).

%
%	Assuming Expressions true or false
%

:- mode ts_assumption(+,-,+).

ts_assumption([not,X],A,Ass) :- !,ts_assumption(X,B,Ass),!,
	((B=t,!,A=f);
	 (B=f,!,A=t);
	 (B=or(L,R),!,A=or(R,L))),!.

ts_assumption(X,A,Ass) :- type_set(X,S,Ass), 
	((S=1,!,A=f) ; (S=2,!,A=t)),!.

ts_assumption([equal,T1,T2],Mustbe,Ass) :- !,
	type_set(T1,S1,Ass),!,
	type_set(T2,S2,Ass),!,
	ts_intersection(S1,S2,S),!,
	((S=0,!,Mustbe=f);
	 (S1=S2,bit_place([_],S),!,Mustbe=t);
	 (add_ts_assumption([[equal,T1,T2]=2,[equal,T2,T1]=2,T1=S,T2=S],
		Ass,L_Ass),!,
	  ((S1=S,bit_place([_],S),!,
	    ts_difference(S2,S1,S21),!,
	    add_ts_assumption([[equal,T1,T2]=1,[equal,T2,T1]=1,T2=S21],
		 	Ass,R_Ass)) ;
	   (S2=S,bit_place([_],S),!,
	    ts_difference(S1,S2,S12),!,
	    add_ts_assumption([[equal,T1,T2]=1,[equal,T2,T1]=1,T1=S12],
		 	Ass,R_Ass)) ;
	   add_ts_assumption([[equal,T1,T2]=1,[equal,T2,T1]=1],
			Ass,R_Ass)),
	  Mustbe=or(L_Ass,R_Ass))),!.

ts_assumption(X,Mustbe,Ass) :- recognizer(X),!,
	X=[R,Y],!,
	(bit_place(R,RTS);
	 (recognizer_constructor([R,C]), bit_place([C],RTS))),!,
	type_set(Y,YTS,Ass),!,
	((RTS=YTS,!,Mustbe=t);
	 (ts_intersection(RTS,YTS,0),!,Mustbe=f);
	 (ts_difference(YTS,RTS,Sd),!,
	  add_ts_assumption([Y=RTS],Ass,L_Ass),!,
	  add_ts_assumption([Y=Sd],Ass,R_Ass),!,
	  Mustbe=or(L_Ass,R_Ass))),!.

ts_assumption(X,Mustbe,Ass) :-
	type_set(X,S,Ass),!,
	((S=1,!,Mustbe=f);
	 (ts_intersection(S,1,0),!,Mustbe=t);
	 (ts_difference(S,1,Sd),!,
	  add_ts_assumption([X=Sd],Ass,L),!,
	  add_ts_assumption([X=1],Ass,R),!,
	  Mustbe=or(L,R))),!.

:- public add_ts_assumption/3.
:- mode add_ts_assumption(+,+,-).
add_ts_assumption([A1|As],Ass,NewAss) :-
	add_ass1(A1,Ass,N1),!,
	add_ts_assumption(As,N1,NewAss),!.
add_ts_assumption([],A,A).

	:- mode add_ass1(+,+,-).
	add_ass1(T=New,[T=_|Ass],[T=New|Ass]).
	add_ass1(A,[A1|Ass],[A1|Newass]) :- add_ass1(A,Ass,Newass),!.
	add_ass1(A,[],[A]).

:- public ts_subset/2.
:- mode ts_subset(+,+).
ts_subset(X,Y) :- XY is X/\Y,!,X=XY,!.

:- public ts_union/3.
:- mode ts_union(+,+,?).
ts_union(X,Y,Z) :- XY is X\/Y,!,Z=XY,!.

:- public ts_intersection/3.
:- mode ts_intersection(+,+,?).
ts_intersection(X,Y,Z) :-  XY is X/\Y,!,Z=XY,!.

:- public ts_difference/3.
:- mode ts_difference(+,+,?).
ts_difference(X,Y,Z) :- XY is X-(X/\Y),!,Z=XY,!.

:- public boolean/2.
:- mode boolean(+,?).
boolean(Term,Ass) :- type_set(Term,Y,Ass),!,(Y=1;Y=2).

% EOF type.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public ll/0, ll/1, lrl/0, lgl/0, lel/0, lil/0.

ll :- lrl, lgl, lel, lil.

lrl :-	clear(ll_num), nl,write('rewrite lemma'),nl, ll(rewrite).
lgl :-	clear(ll_num), nl,write('generalize lemma'),nl, ll(generalize).
lel :-	clear(ll_num), nl,write('elimination lemma'),nl, ll(elimination).
lil :-	clear(ll_num), nl,write('induction lemma'),nl, ll(induction).

:- mode ll(+).
ll(rewrite) :- rewrite_lemma(_,_,_,_,Type,Name), llemma(Type,Name), fail.
ll(generalize) :- generalization_lemma(Type,Name,_), llemma(Type,Name), fail.
ll(elimination) :- elimination_lemma(Type,Name,_), llemma(Type,Name), fail.
ll(induction) :- induction_lemma(_,_,Type,Name), llemma(Type,Name), fail.
ll(_) :- clear(ll_num).

:- mode llemma(+).
llemma(Type,Name) :- new(ll_num,N),
	tab(3),write(N),write('	'),write(Type),write('	'),write(Name),nl,!.

clear(Counter) :- abolish(Counter,1), C=..[Counter,0], asserta(C),!.

new(Counter,N1) :- O=..[Counter,N], retract(O), N1 is N+1,
	C=..[Counter,N1], asserta(C),!.
new(Counter,1) :- C=..[Counter,1], asserta(C),!.

:- public lp/0, lp/1, lsp/0, lhp/0, lip/0.

lp :- lsp, lhp, lip.

lsp :- clear(lp_num), nl,write('simplification pool'),nl, lp(simplification).
lhp :- clear(lp_num), nl,write('heuristics pool'),nl, lp(heuristics).
lip :- clear(lp_num), nl,write('induction pool'),nl, lp(induction).

:- mode lp(+).
lp(Pool) :- pool(Pool,Name:_), lpool(Name), fail.
lp(_) :- clear(lp_num).

:- mode lpool(+).
lpool(Name) :- new(lp_num,N),
	tab(3),write(N),write('	'),write(Name),nl,!.

:- public rsp/0, rhp/0, rip/0, hpsa/0, hpsz/0, ipsa/0, ipsz/0.

rsp :- retract(pool(simplification,C)), assertz(pool(simplification,C)).
rhp :- retract(pool(heuristics,C)), assertz(pool(heuristics,C)).
rip :- retract(pool(induction,C)), assertz(pool(induction,C)).

hpsa :- retract(pool(heuristics,C)), asserta(pool(simplification,C)).
hpsz :- retract(pool(heuristics,C)), assertz(pool(simplification,C)).
ipsa :- retract(pool(induction,C)), asserta(pool(simplification,C)).
ipsz :- retract(pool(induction,C)), assertz(pool(simplification,C)).

% EOF util.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public variable/1.
:- mode variable(+).
variable([]) :- !,fail.
variable(X) :- atom(X),!,X\==t,!,X\==f,!,\+bottom_object(X),!,\+abbrev_(X,_),!.

:- public var_in_term/2.
%:- mode var_in_term(?,+).
var_in_term(V,V) :- variable(V).
%var_in_term(V,{X}) :- var_in_pterm(V,X).
var_in_term(V,[_|As]) :- var_in_args(V,As).

:- public var_in_args/2.
:- mode var_in_args(?,+).
var_in_args(V,[A|_]) :- var_in_term(V,A).
var_in_args(V,[_|As]) :- var_in_args(V,As).

:- mode var_in_pterm(?,+).
var_in_pterm(V,V) :- variable(V).
var_in_pterm(V,T) :- T=..[_|A], var_in_pterm_args(V,A).

:- mode var_in_pterm_args(?,+).
var_in_pterm_args(V,[A|_]) :- var_in_pterm(V,A).
var_in_pterm_args(V,[_|As]) :- var_in_pterm_args(V,As).

:- public vars_in_term/2.
:- mode	vars_in_term(+,?).
vars_in_term(X,[X]) :- variable(X),!.
vars_in_term([_|As],V) :- map_vars_in_term(As,V),!.
vars_in_term(_,[]).

:- public map_vars_in_term/2.
:- mode map_vars_in_term(+,?).
map_vars_in_term([H|T],V) :-
	vars_in_term(H,HV),
	map_vars_in_term(T,TV),
	union(HV,TV,V),!.
map_vars_in_term([],[]).

:- public distinct_variables/1.
:- mode distinct_variables(+).
distinct_variables([V|Vs]) :- (variable(V); var(V)),!,\+member(V,Vs),!, 
	distinct_variables(Vs),!.
distinct_variables([]).

:- public prolog_vars/1.
:- mode prolog_vars(+).
prolog_vars([V|Vs]) :- var(V),!,prolog_vars(Vs),!.
prolog_vars([]).

:- public new_vars/2.
:- mode new_vars(+,-).
new_vars(X,Y) :- assert(hack(X)), retract(hack(Y)),!.

:- public instantiate/2.
:- mode instantiate(+,-).
instantiate(X,Y) :- assert(hack(X)), retract(hack(Y)),!.

:- public p_vars/4.
:- mode p_vars(+,+,-,-).
p_vars(X,Y,U,V) :- length(X,N), generate_vars(1,N,U),!, p_vars0(X,Y,U,V),!.

:- mode p_vars0(+,+,+,-).
p_vars0(_,Y,_,Y) :- var(Y),!.
p_vars0(X,Y,U,V) :- atomic(Y),!, p_vars1(X,Y,U,V),!.
p_vars0(X,Y1:Y2,U,V1:V2) :- p_vars0(X,Y1,U,V1), p_vars0(X,Y2,U,V2),!.
p_vars0(X,Y1-Y2,U,V1-V2) :- p_vars0(X,Y1,U,V1), p_vars0(X,Y2,U,V2),!.
p_vars0(X,Y1=Y2,U,V1=V2) :- p_vars0(X,Y1,U,V1), p_vars0(X,Y2,U,V2),!.
p_vars0(X,(Y1->Y2),U,(V1->V2)) :- p_vars0(X,Y1,U,V1), p_vars0(X,Y2,U,V2),!.
p_vars0(_,[pack,X],_,[pack,X]).
p_vars0(X,[YA|YD],U,[VA|VD]) :- p_vars0(X,YA,U,VA),!, p_vars0(X,YD,U,VD),!.
p_vars0(_,[],_,[]) :- !.
p_vars0(X,{Y},U,{V}) :- Y=..[F|A], p_vars0(X,A,U,B), V=..[F|B],!.
p_vars0(X,Y,U,V) :- Y=..[F|A], p_vars0(X,A,U,B), V=..[F|B],!.
p_vars0(_,Y,_,Y) :- !.

:- mode p_vars1(+,+,+,-).
p_vars1([X|_],X,[U|_],U) :- !.
p_vars1([_|X],Y,[_|U],V) :- !, p_vars1(X,Y,U,V),!.
p_vars1(_,Y,_,Y) :- !.

:- public generate_vars/3.
:- mode generate_vars(+,+,-).
generate_vars(N,M,[_|Xs]) :- N=<M, !, N1 is N+1, generate_vars(N1,M,Xs),!.
generate_vars(_,_,[]) :- !.

:- public gen_var/1, gen_vars/2.
:- mode gen_var(?), gen_vars(+,-).
gen_var(V) :- next_var(VS), name(V,VS),
	\+bottom_object(V), V\==t, V\==f, \+abbrev_(V,_),
	\+used_variable(V),
	assertz(used_variable(V)),!.
gen_var(V) :- gen_var(V).

:- mode next_var(-).
next_var([V]) :- retract(var_seed([V])),
	((V1 is V+1, V1=<90, asserta(var_seed([V1])));
	 asserta(var_seed([66,49]))),!.
next_var([VS,VN]) :- retract(var_seed([VS,VN])),
	((VS1 is VS+1, VS1=<90, asserta(var_seed([VS1,VN])));
	 (VN1 is VN+1, asserta(var_seed([66,VN1])))),!.
next_var([65]) :- asserta(var_seed([65])),!.

gen_vars(0,[]) :- !.
gen_vars(N,[Vn|V]) :- gen_var(Vn), N1 is N-1, gen_vars(N1,V),!.

:- public rep_var/4.
:- mode rep_var(+,+,+,?).
rep_var(X,T,X,T):-!.
rep_var(X,T,[F|Args],[F|NewArgs]):- !,replace_var(X,T,Args,NewArgs).
rep_var(_,_,T,T).

:- public replace_var/4.
:- mode replace_var(+,+,+,?).
replace_var(X,T,[Top|Rest],[NewTop|NewRest]):-
	rep_var(X,T,Top,NewTop),
	replace_var(X,T,Rest,NewRest).
replace_var(_,_,[],[]).

% EOF var.pl
% (C)1992 Institute for New Generation Computer Technology
% (Read COPYRIGHT for detailed information.)

:- public
	tab/0, bell/0, home/0, set_cursor/2, set_region/2,
	erase_/1, attrib/1, wwrite/2.

tab :- write('	').
bell :- write('\a').
home :- vt100_,write('\e[H').

:- mode set_cursor(+,+).
set_cursor(Line,Column) :- vt100_,
	write('\e['),write(Line),write(';'),write(Column),write('H').
set_cursor(_,_).
:- mode set_region(+,+).
set_region(Top,Bottom) :- vt100_,
	write('\e['),write(Top),write(';'),write(Bottom),write('r').
set_region(_,_).

:- mode erase_(+,+).
erase_([cursor,eos]) :- vt100_,write('\e[0J').
erase_([bos,cursor]) :- vt100_,write('\e[1J').
erase_([bos,eos])    :- vt100_,write('\e[2J').
erase_([cursor,eol]) :- vt100_,write('\e[0K').
erase_([bol,cursor]) :- vt100_,write('\e[1K').
erase_([bol,eol])    :- vt100_,write('\e[2K').
erase_(screen)       :- vt100_,write('\e[2J').
erase_(line)         :- vt100_,write('\e[2K').
erase_(_).

:- mode attrib(+).
attrib([A1|As]) :- attrib(A1),attrib(As).
attrib(normal)		:- write('\e[0m').
attrib(bold)		:- write('\e[1m').
attrib(underline)	:- write('\e[4m').
attrib(blink)		:- write('\e[5m').
attrib(reverse)		:- write('\e[7m').
attrib(graphic)		:- write('\e(0').
attrib(ascii)		:- write('').
attrib(big)		:- write('\e#3').
attrib(_).

:- mode wwrite(+,+).
wwrite(X,A) :- vt100_,attrib(A),write(X),
	(((A=big;member(big,A)),nl,write('\e#4'),write(X)); true),
	attrib(normal).
wwrite(X,_) :- write(X).

% EOF vt100.pl
