%% Fred 
%% juillet 1998, octobre 1999
%% fevrier 2000, avril 2000, juillet 2000
%% mai 2002 avec Roberto

:- module(num_op,[
		   true/1,
		   false/1,
		   conjunction/3,
		   satisfiable/1,
		   entail/4,
		   equivalent/4,
		   union/6,
		   widening/6,
		   project/4,
		   tell_cs/1,
		   entail_conj/1,
           simplify_constraint/3
	       ]).

:- dynamic(widening78_result/1).
:- dynamic(widening92_result/1).

:- dynamic(num_copy/2).

:-use_module(library(lists),[append/3,select/3]).
:-use_module(library(assoc)).
:-use_module(library(clpq)).


%%%%
true([]).

%%%%
false([X=Y]) :- {X=0,Y=1}.

%%%%
conjunction(C1,C2,C3) :- append(C1,C2,C3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% satisfiable(+[C1,...,Cn]) iff 	
%% 		C1&...&Cn is satisfiable 
%% precond: 	none

satisfiable(T) :- check(callc(T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tell_cs(Cs) :- callc(Cs).
callc([]).
callc([C|Cs]) :- {C}, callc(Cs).

check(G) :- \+ \+ call(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% entail(+Xs,+S,+Ys,+T) iff 
%%		env(Xs,S) => env(Ys,T)
%% precond: 	var(env(Xs,S)) inter Ys = O 
%%				var(T) included in Ys

entail(Xs,S,Ys,T) :-
	check(
	      (Xs=Ys,
		  (callc(S) ->
		      entail_conj(T)
		  ;
		      true
		  )
	      )
	     ).

%% if the intended model is Q={rational numbers} then
entail_conj([]).
entail_conj([C|Cs]) :-entailed(C),entail_conj(Cs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  equivalent(+Xs,+S,+Ys,+T) iff 
%%		env(Xs,S) <=> env(Ys,T)
%% precond: 	Xs inter Ys = O 
%%		var(S) included in Xs 
%%		var(T) included in Ys

equivalent(Xs,S,Ys,T) :-
	entail(Xs,S,Ys,T), entail(Ys,T,Xs,S).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%  meta_object(+Cs) iff 	
%%		Cs is satisfiable and 'tell' Cs to the current store
%% precond: 	none

meta_object(Cs) :-callc(Cs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  object_meta(+Xs,-Ys,-Cs) iff 	
%%		env(Ys,Cs) is a projected copy of Xs 
%%		and its related constraints in the current store
%% precond: 	none


object_meta(Xs,_,_) :-
	normalize(Xs,X1s,X2s,Cons,Eqns),
	dump(X1s,X3s,Cons),
	X2s=X3s,
	asserta(copy(X2s,Eqns)),
	fail.
object_meta(_,Ys,Cs) :-
	retract(copy(Ys,Cs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  project(+Xs,S,-Ys,-T) iff 	
%%		env(Ys,T) is a projected copy of env(Xs,S)
%% precond: 	S satisfiable

/*
project_ppl(Vars,C3s,Ws,Cs) :-
	term_variables(Vars,Vs),
	norm(Vars,ListVarsDist,Eqns),
	poly_ppl:create_dont_know(Vs,Poly1),
	poly_ppl:extend_and_add_constraints(Poly1,C3s,Poly2),
	length(Vs,Long),
	poly_ppl:restrict(Long,Poly2,Poly3),
	poly_ppl:get_constraints(Poly3,C4s),
	poly_ppl:dispose(Poly3),
	append(Eqns,C4s,C5s),
	copy_term(ListVarsDist-C5s,Ws-Cs).

% norm(+Vars,-ListVarsDist,-Eqns)
norm(Vars,ListVarsDist,Eqns) :- norm(Vars,[],ListVarsDist,Eqns).
norm([],_,[],[]).
norm([X|Xs],SVs,[X|L],E) :- notmembereq(SVs,X),!,norm(Xs,[X|SVs],L,E).
norm([X|Xs],SVs,[Y|L],[X=Y|E]) :- norm(Xs,SVs,L,E).

notmembereq([],_).
notmembereq([X|Xs],Y) :-X \== Y,notmembereq(Xs,Y).
*/

%%%%
project(Xs,S,Ys,T) :- 
    project_clpq(Xs,S,Ys,T).
%	(main:current_cti_flag(poly_lib,poly_ppl) ->
%	    project_ppl(Xs,S,Ys,T)
%	;
%	    project_clpq(Xs,S,Ys,T)).


%%%%
project_clpq(Xs,S,_,_) :-
	(callc(S) ->
	    normalize(Xs,X1s,X2s,Cons,Eqns),
	    dump(X1s,X3s,Cons),
	    X2s=X3s,
	    asserta(num_copy(X2s,Eqns))
	;
	    false(C),
	    asserta(num_copy(Xs,C))),
	fail.
project_clpq(_,_,Ys,T) :-
	retract(num_copy(Ys,T)).


%% relation capitale : correction et temps de calcul
normalize(Xs,Ys,Zs,Cons,Eqns) :-
	empty_assoc(Env),
	normalize(Xs,Ys,Zs,Env,Cons,Eqns).

normalize([],[],[],_,Tail,Tail).
normalize([X|Xs],Ys2,Zs2,Vars,Tail,Eqns2) :-
	(var(X) ->
	    (get_assoc(X,Vars,Z) ->
		(Eqns2=[Y=Z|Eqns],Vars2=Vars,Ys2=[Y|Ys],Zs2=[Y|Zs])
	    ;
		(Eqns2=Eqns,put_assoc(X,Vars,Z,Vars2),Ys2=[X|Ys],Zs2=[Z|Zs]))
	; % ground(X)
	    (Eqns2=[Y=X|Eqns],Vars2=Vars,Ys2=[Y|Ys],Zs2=[Y|Zs])),
	normalize(Xs,Ys,Zs,Vars2,Tail,Eqns).

/*
REMARK2:  project does simplify the constraint. The projected
constraint SEEMS to be of the following form:

		E+ >= rat(N,D)	E+ commence par un monome positif (une var) 
						et ne contient pas de constante
		E+ =< rat(N,D)	E commence par un monome positif (une var)
						et ne contient pas de constante
		X = rat(N,D)	

		X = E		E ne contient pas de constante (evt E var)	

		X = rat(N,D)+E	tq X n'apparait pas dans E, 
				E ne contient pas de constante, N>0 ou N<0

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  convex_hull(+Xs,+Cxs,+Ys,+Cys,-Zs,-Czs)) iff 	
%%		env(Zs,Czs) is the convex hull of env(Xs,Cxs) and env(Ys,Cys)
%%  precond: 	Xs inter Ys = O 
%%				var(Cxs) included in Xs 
%%				var(Cys) included in Ys
%%				Cxs and Cys satisfiable

convex_hull(Xs,Cxs,Ys,Cys,Zs,Czs) :-
	scale(Xs,Cxs,S1,Vs,[],Cvs),
	scale(Ys,Cys,S2,Ws,Cvs,Cvws),
	add_vect(Vs,Ws,Us,Cvws,Cs),
	project(Us,[S1>=0,S2>=0,S1+S2=1|Cs],Zs,Czs).

union(Xs,Cxs,Ys,Cys,Zs,Czs) :-
	convex_hull(Xs,Cxs,Ys,Cys,Zs,Czs).

add_vect([],[],[],Cs,Cs).
add_vect([X|Xs],[Y|Ys],[Z|Zs],C1s,C2s) :-
	add_vect(Xs,Ys,Zs,[X+Y=Z|C1s],C2s).

scale(Xs,Cxs,S,Ys,Cws,Cyws) :-
	copy_term(env(Xs,Cxs),env(Ys,Cys)),
	multis(Cys,S,Cws,Cyws).

multis([],_,Cs,Cs).
multis([C1|C1s],S,C2s,C3s) :-
	multi(C1,S,C2),multis(C1s,S,[C2|C2s],C3s).
	
multi(C1,S,C2) :-
	C1=..[OpRel,A1,B1],C2=..[OpRel,A2,B2],
	multExp(A1,S,A2),multExp(B1,S,B2).
		
multExp(X,_,X) :-var(X),!.
multExp(-X,S,-Y) :-!,multExp(X,S,Y). 
multExp(N,S,N*S) :-ground(N),!.
multExp(N*X,_,N*X) :-ground(N),var(X),!.			
multExp(A+B,S,C+D) :-!,multExp(A,S,C),multExp(B,S,D).
multExp(A-B,S,C-D) :-!,multExp(A,S,C),multExp(B,S,D).
multExp(X,_,_) :-raise_exception(module(q_op,multExp(X,_,_))).  % oops !
	

/*
| ?-  convex_hull([X],[X= -1],[Y],[Y=1],A,B).

A = [_A],
B = [_A>= -1,_A=<1] ? ;

no
| ?-  convex_hull([X],[X=1+Z,-1=<Z,Z=<1],[Y],[Y=4],A,B).

A = [_A],
B = [_A=<4,_A>=0] ? ;

no
| ?-  convex_hull([X,Y],[X=1,Y=1],[X,Y],[X=2,Y=2],V,C).

C = [_A>=1,_A=<2,_B=_A],
V = [_B,_A] ? ;

no
| ?-  convex_hull([X,Y],[X=<1,Y>=0,Y=<2],[U,V],[2=<U,U=<3,0=<V,V=<1],A,B).

A = [_A,_B],
B = [_A+2*_B=<5,_A=<3,_B=<2,_B>=0] ? ;

no
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  widening(+Xs,+Cxs,+Ys,+Cys,-Zs,-Czs)) iff 	
%%		env(Zs,Czs) is the widening of env(Xs,Cxs) and env(Ys,Cys)
%%		where 	Czs={cnstrnts from Cxs which are logical consequences of Cys}
%%			     once Xs=Ys.
%% precond: 	Xs inter Ys = O 
%%		var(Cxs) included in Xs 
%%		var(Cys) included in Ys
%%		Cxs and Cys satisfiable

widening(V1, C1, V2, C2, V3, C3) :-
	widening78(V1, C1, V2, C2, V3, C3).
	
tousineq([],[]).
tousineq([C|Cs],Fs) :-ineq(C,Ds),append(Ds,Es,Fs),tousineq(Cs,Es).

ineq(A=B,[A>=B,B>=A]).
ineq(A=<B,[B>=A]).
ineq(A>=B,[A>=B]).

touspos([],[]).
touspos([X|Xs],[X>=0|Ys]) :- touspos(Xs,Ys).

% le widening Cousot-Halbwachs 78
% cf peephole 
widening78(V1, C11, V2, C2, _, _) :-
	tousineq(C11,C1),
	%\+ \+ (
		V1 = V2,
		filter78(C1, C2, C3, V1),
		assertz(widening78_result(V1-C3)),
	 %     ),
	fail.
widening78(_, _, _, _, V3, C3posp) :-
	retract(widening78_result(V3-C3)),!,
	touspos(V3,Cpos),
	append(Cpos,C3,C3pos),
	project(V3,C3pos,V3,C3posp).

% filter78(C1, C2, C3, Vs):
%   C1 and C2 constrain the same variables.
%   C1 only contains inequalitites.
filter78([], _C2, [], _Vs).
filter78([C|Cs], C2, Ds, Vs) :-
	copy_term(C-Vs, D-Ws),
	(entail(Vs, C2, Ws, [D]) ->
	    Ds = [C|Es]
	;
	    Ds = Es
	),
	filter78(Cs, C2, Es, Vs).

/*
% le widening Cousot^2 92
widening92(V1, C11, V2, C22, _, _) :-
	tousineq(C11,C1),
	tousineq(C22,C2),
	\+ \+ (
		V1 = V2,
		widening78_2(C1, C2, C1_prime, V1),
		widening92_2(C1, C2, C2_prime, V1),
		conjunction(C1_prime, C2_prime, C3),
		assertz(widening92_result(V1-C3))
	      ),
	fail.
widening92(_, _, _, _, V3, C3posp) :-
	retract(widening92_result(V3-C3)),!,
	touspos(V3,Cpos),
	append(Cpos,C3,C3pos),
	project(V3,C3pos,V3,C3posp).
	%C3posp=C3pos.
	%sort(C3pos,C3posp).


%%
widening92_2(C1, C2, C3, Vs) :-
	filter92_2(C2, C1, C3, Vs).

filter92_2([], _, [], _Vs).
filter92_2([Gamma|Gammas], C1, C3, Vs) :-
	(aux_filter92_2(Gamma, C1, Vs) ->
	    C3 = [Gamma|D3]
	;
	    C3 = D3
	),
	filter92_2(Gammas, C1, D3, Vs).

aux_filter92_2(Gamma, C1, Vs) :-
	select(_Beta, C1, RestC1),
	copy_term(Vs-[Gamma|RestC1], Ws-GRCs),
	equivalent(Vs, C1, Ws, GRCs).


%%%%%%%%%%%%%%%%% test
widening78F(V1,C11,V2,C2,V3,C4):-
	(satisfiable(C2) ->
	    tousineq(C11,C1),
	    widening1(V1,C1,V2,C2,V3,C4)
	;
	    copy_term(V1-C11,V3-C4)).

widening1(V1,C1,V2,C2,_,_):-
        copy_term(env(V1,C1),env(V3,C3)),
        V1=V2,callc(C2),tell_entailed(C1,C3),
        normalize(V3,X1s,X2s,Cons,Eqns),
        dump(X1s,X3s,Cons),
        X2s=X3s,
        bb_put(copy,env(X2s,Eqns)),
        fail.
widening1(_,_,_,_,Vs,Cs):-
        bb_delete(copy,env(Vs,C0s)),
	touspos(Vs,Cpos),
	append(Cpos,C0s,Cs).
	

tell_entailed([],[]).                                                                                
tell_entailed([C1|C1s],[C2|C2s]):-                                                                   
       negate_rat(C1, Cn), \+ { Cn },{ C2 },                                                         
       !,tell_entailed(C1s,C2s).                                                                     
tell_entailed([_|C1s],[_|C2s]):-                                                                     
       tell_entailed(C1s,C2s).                                                                       

negate_rat(A<B,        A>=B).                                                                        
negate_rat(A=<B,       A>B).                                                                         
negate_rat(A>B,        A=<B).                                                                        
negate_rat(A>=B,       A<B).                                                                         
negate_rat(A=B,        A=\=B). 

*/



	

%remettre exemples a jour 
/*
| ?-  widening1([A,B],[A>=1,B<0],[X,Y],[X>2,Y>=1],Zs,Czs).

Zs = [_A,_B],
Czs = [_A>=1] ? ;

no
| ?- widening1([X,Y],[0=<X,X=<1,Y=0],[A,B],[0=<B,B=<A,A=<2],Vs,Cs).

Cs = [_A>=0],
Vs = [_A,_B] ? ;

no
| ?- widening1([X,Y],[0=<X,X=<1,Y=<0,0=<Y],[A,B],[0=<B,B=<A,A=<2],Vs,Cs).

Cs = [_A>=0,_B>=0],
Vs = [_B,_A] ? ;
no
| ?- widening([X,Y],[0=<X,X=<1,Y=<0,0=<Y],[A,B],[0=<B,B=<A,A=<2],Vs,Cs).

Cs = [_A-_B>=0,_B>=0],
Vs = [_A,_B] ? ;

no
no
*/


simplify_constraint(A1s,A2s,_TO) :- !, A2s = A1s.
simplify_constraint(A1s,A2s,_TO) :-        
    (satisfiable(A1s)
    ->
        term_variables(A1s,Vars), 
        project(Vars,A1s,Vars,A2s)
    ;
        false(A2s)).
            
