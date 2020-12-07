%% Fred
%% Juillet 97-99

:-module(cmc,[calcul_mat_col/4]).

:-use_module(library(lists),[same_length/2]).
:-use_module(library(clpq)).

%%%
calcul_mat_col(Cons,V,M,C):-
	calcul_mat_col(Cons,V,[],M,[],C).
	
calcul_mat_col([],_,M,M,C,C).
calcul_mat_col([Cons|Cs],V,M1,M2,C1,C2):-
	traite(Cons,V,M1,M3,C1,C3),
	calcul_mat_col(Cs,V,M3,M2,C3,C2).

/*
| ?- cmc:calcul_mat_col([X >=rat(0,1), Y =< rat(-1,1),Z=T],[X,Y,Z,T],M,C).

C = [0,0,1,0],
M = [[0, 0, 1, -1],  % Z-T  >=0  2 ineqs pour Z=T
     [0, 0,-1,	1],  % -Z+T >=0
     [0,-1, 0,	0],  % -Y   >=1	 l'ineq pour Y =< -1
     [1, 0, 0,	0]]  % X    >=0	 idem X>=0
ie
pre: 	var(Cons) \subseteq V
	calcul_mat_col(Cons,V,M,C) 
post:	Cons <=> M*V >= C
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Notations :		
%%		rat(N,D) 	{N entier relatif, evt 0, D entier naturel non nul}
%%		X, -(X) 	{X variable} 
%%
%% !!!	associativite a gauche :
%%		X+Y+Z => (X+Y)+Z
%%		X-Y+Z => (X-Y)+Z
%%		X+Y-Z => (X+Y)-Z
%%		X-Y-Z => (X-Y)-Z
%%
%% a priori, 3 cas pour les contraintes numeriques,
%% une fois simplifiees par le solveur clpq de SP :
%%
%% 	E+ >= rat(N,D)	E+ (commence par un monome positif et) ne contient pas de cste
%%			a reecrire en	E >= rat(N,D)
%%
%% 	E+ =< rat(N,D)	E (commence par un monome positif et) ne contient pas de cste
%%			a reecrire en	-E >= rat(-N,D) 	
%%
%%      X = rat(N,D)	a reecrire en 	X >= rat(N,D) 	
%%	                               -X >= rat(-N,D) 	
%%	
%% 	X = rat(N,D)+E	X n'apparait pas dans E, E ne contient pas de cste, evt N=0
%%			a reecrire en 	X - E >= rat(N,D) 	
%%	                                E - X >= rat(-N,D) 	
%%	
%% E est de la forme (en gros ...) :
%%
%%		E	->  rat(N,D) R_Exp | -(X) R_Exp | M R_Exp 
%%		R_Exp	->  vide | + M R_Exp | -(X) R_Exp
%%		M	->  X | rat(N,D)*X 


traite(E >= Num,V,M,[L|M],C,[Cste|C]):-
	{Cste=Num},
	construit_ligne(E,V,L).
traite(E =< Num,V,M,[Lm|M],C,[Opp_Cste|C]):-
	{Opp_Cste = - Num},
	construit_ligne(E,V,L), 
	opposes(L,Lm).
traite(X = E,V,M,[L1,L2|M],C,[Cste,Opp_Cste|C]):-
	separe_cste_exp(E,Cste,Exp),!,
	{ Opp_Cste = - Cste },
	construit_ligne(X,V,Lx),
	construit_ligne(Exp,V,Le),
	opposes(Lx,LxOpp),
	opposes(Le,LeOpp),
	ajoute_ligne(Lx,LeOpp,L1),
	ajoute_ligne(Le,LxOpp,L2).
traite(X = rat(N,D),V,M,[L1,L2|M],C,[Cste,Opp_Cste|C]):-
	Cste=rat(N,D), {Opp_Cste = - Cste},
	construit_ligne(X,V,L1),
	opposes(L1,L2).
	
ajoute_ligne([],[],[]).
ajoute_ligne([X|Xs],[Y|Ys],[Z|Zs]):-{ Z=X+Y },ajoute_ligne(Xs,Ys,Zs).

opposes([],[]).
opposes([X|Xs],[Y|Ys]):-{ Y = -X },opposes(Xs,Ys).

separe_cste_exp(X,Z,X):- var(X),is_zero(Z),!.
separe_cste_exp(-(X),Z,-(X)):- var(X),is_zero(Z),!.
separe_cste_exp(Num*X,Z,Num*X):- !,var(X),is_num(Num),is_zero(Z).
separe_cste_exp(X+E,Z,X+E):- var(X),is_zero(Z),!.
separe_cste_exp(X-E,Z,X-E):- var(X),is_zero(Z),!.
separe_cste_exp(Num+E2,Num,E2):- is_num(Num),!.
separe_cste_exp(Num-E2,Num,-E2):- is_num(Num),!.
separe_cste_exp(E1+E2,Cste,Exp+E2) :- !,separe_cste_exp(E1,Cste,Exp).
separe_cste_exp(E1-E2,Cste,Exp-E2):-!,separe_cste_exp(E1,Cste,Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% construit_ligne(Exp,Var,Ligne)
%% 		precond : Exp ne contient pas de cste
%% 		Ligne est la ligne des coeffs de Exp wrt Var

construit_ligne(E,Var,L):-
	same_length(Var,Coeff),
	construit_ligne2(E,Var,Coeff),
	instan(Coeff,L).
	
instan([],[]).
instan([X|Xs],[0|Ys]):-var(X),!,instan(Xs,Ys).
instan([X|Xs],[X|Ys]):-instan(Xs,Ys).

construit_ligne2(X,Var,Coeff):-
	var(X),!,num_one(One),maj_coeff(Var,X,One,Coeff).
construit_ligne2(-(X),Var,Coeff):-
	var(X),!,num_minus_one(MO),maj_coeff(Var,X,MO,Coeff).
construit_ligne2(Num*X,Var,Coeff):-
	is_num(Num),var(X),!,maj_coeff(Var,X,Num,Coeff).
construit_ligne2(-Num*X,Var,Coeff):-
	is_num(Num),var(X),!,{R1 = -Num},maj_coeff(Var,X,R1,Coeff).
construit_ligne2(E1+E2,Var,Coeff):-
	!,construit_ligne2(E2,Var,Coeff),
	construit_ligne2(E1,Var,Coeff).
construit_ligne2(E1-E2,Var,Coeff):-
	!,construit_ligne2(-(E2),Var,Coeff),
	construit_ligne2(E1,Var,Coeff).
	
maj_coeff([X|_],Y,Num,[Num|_]):-X==Y,!.
maj_coeff([_|Xs],Y,Num,[_|Coeff]):-maj_coeff(Xs,Y,Num,Coeff).
	
%%%%%%%%%
is_num(R):-nonvar(R),R=rat(N,D),integer(N),integer(D).
is_pos(rat(N,_)):-N>0.
is_neg(rat(N,_)):-N<0.
is_zero(rat(0,1)).

num_one(rat(1,1)).
num_minus_one(rat(-1,1)).











