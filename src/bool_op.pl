:- module(bool_op,[
		   true/1,
		   false/1,
		   conjunction/3,
		   satisfiable/1,
		   entail/4,
		   equivalent/4,
		   union/6,
		   widening/6,
		   project/4,
           simplify_constraint/3,
           simplify_constraint_else_false/3
		  ]).

:- use_module(library(clpb)).

:- use_module(compat_swi).

:- dynamic(bool_copy/2).

%%%%
true(1).

%%%%
false(0).

%%%%
conjunction(C1,C2,C1*C2).

%%%%
check(G):- \+ \+ call(G).
	
%%%%%
satisfiable( T ):- check(sat(T)).

%%%%%
entail(Xs, S ,Ys, T ):- check( (Xs=Ys,taut(S =< T,1)) ).

%%%%%
equivalent(Xs, S ,Ys, T ):- check( (Xs=Ys, taut(S =:= T, 1)) ).
	
%%%%%
union(Xs, S ,Ys, T ,Zs, U ):-
	copy_term(e(Ys,T),e(Xs,Tcxs)),
	project(Xs,S+Tcxs,Zs,U).

%%%%% 
widening(Xs,S,Ys,T,Zs,U):-union(Xs,S,Ys,T,Zs,U).

%%%%%

project(Xs,S,_,_):-
	sat(S),
    % F
    copy_term(Xs,Ys,Bs),
    strip_bool_constraints(Bs,Cs),
    %copy_term(Xs-S,Ys-Cs),
    %asserta(bool_copy(Xs,1)),call_residue(retract(bool_copy(Ys,1)),C),gather(C,Cs),
	normalize(Ys,X1s,[],Cs,C2s),
	asserta(bool_copy(X1s,C2s)),
	fail.
project(_,_,Ys, T ):-
	retract(bool_copy(Ys,T1)), 
 	free_vars(T1,Ys,FreeVars),
	eliminate(FreeVars,T1,T).

strip_bool_constraints([],1).
strip_bool_constraints([clpb:sat(C)],C) :- !. 
strip_bool_constraints([clpb:sat(C)|Bs],C*Cs) :- strip_bool_constraints(Bs,Cs).
    
normalize([],[],_,Cs,Cs).
normalize([X|Xs],[Y|Ys],Vars,C1s,C2s) :-
	var(X),member_var(Vars,X),!,normalize(Xs,Ys,Vars,(Y=:=X)*C1s,C2s).
normalize([X|Xs],[X|Ys],Vars,C1s,C2s) :-
	var(X),!,normalize(Xs,Ys,[X|Vars],C1s,C2s).
normalize([X|Xs],[Y|Ys],Vars,C1s,C2s) :-
	(X=0 -> Z= ~Y ;  Z=Y),
	normalize(Xs,Ys,Vars,Z*C1s,C2s).	
	

member_var([Y|_],X) :- X==Y,!.
member_var([_|Ys],X) :- member_var(Ys,X).
	
free_vars(Term,Vars,FreeVars) :- fv(Term,Vars,[],FreeVars).

fv(X,Vars,FV,FV) :- var(X), (member_var(Vars,X) ; member_var(FV,X)),!.
fv(X,_,FV,[X|FV]) :- var(X), !.
fv(X,_,FV,FV) :- atomic(X), !.
fv(X^T,Vars,FV1,FV2) :- !, fv(T,[X|Vars],FV1,FV2).
fv(~T,Vars,FV1,FV2) :- !, fv(T,Vars,FV1,FV2).
fv(T,Vars,FV1,FV2) :- T=..[_,T1,T2], fv(T1,Vars,FV1,FV3), fv(T2,Vars,FV3,FV2).
		
eliminate([],T,T).
eliminate([X|Xs],T,U):-eliminate(Xs,(X^T),U).
	

/* 

examples
	project([X,Y],(X=:=0)*(X=:=Y),A,B).
	project([X,Y],X=:=Y+Z,A,B).
	project([X,Y],(X=:=Y+Z)*(X=:=Y),A,B).
	project([X,Y],X=:=Y+a,A,B).
	project([X,Y,Z],X*Z+(Y=:=1),A,B).

*/


%simplify_constraint(A1s,A2s,_TimeOut) :- !, A2s = A1s.
simplify_constraint(A1s,A2s,TimeOut) :-    
    (satisfiable(A1s)
    -> 
        term_variables(A1s,Vars), 
        cti_time_out(bool_op:project(Vars,A1s,Vars,A2s),TimeOut,Result),
        (Result=success -> true ; writeln('% time_out bool simplify_constraint!'),A2s=A1s)
    ;
        false(A2s)).


%simplify_constraint_else_false(A1s,A2s,_TimeOut) :- !, A2s = A1s.
simplify_constraint_else_false(A1s,A2s,TimeOut) :-    
    (satisfiable(A1s)
    -> 
        term_variables(A1s,Vars), 
        cti_time_out(bool_op:project(Vars,A1s,Vars,A2s),TimeOut,Result),
        (Result=success -> true ; writeln('% time_out bool simplify_constraint!'),false(A2s))
    ;
        false(A2s)).







