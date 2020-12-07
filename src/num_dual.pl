:- module(dual,[dual/5]).

:- use_module(library(terms)).
:- use_module(library(lists),[append/3,same_length/2]).

:- use_module(num_op,[]).
:- use_module(utils,[tous_pos/2]).


%%%%
transpose(M,[]):-lignes_nulles(M),!. % sinon bug !! :-transpose([],L),fail.
transpose(M,[R|N]):-construit_ligne(M,R,K),transpose(K,N).
	
construit_ligne([],[],[]).
construit_ligne([[H|T]|U],[H|V],[T|W]):-construit_ligne(U,V,W).
	
lignes_nulles([]).
lignes_nulles([[]|Z]):-lignes_nulles(Z).
	
%%%%
mul_vect_vect([],[],0).
mul_vect_vect([E|F],[V|W],T+E*V):-mul_vect_vect(F,W,T).

%%%%
mul_mat_vect([],_,[]).
mul_mat_vect([L|M],V,[E|F]):-mul_vect_vect(L,V,E),mul_mat_vect(M,V,F).

%%%%
tous_inferieurs([],[],[]).
tous_inferieurs([X|Xs],[Y|Ys],[X=<Y|Cs]):-tous_inferieurs(Xs,Ys,Cs).

%%%%
dual(Parametres,Matrice,Colonne,Valeur,C4s):-
	same_length(Matrice,Nouveaux_Parametres),
	tous_pos(Nouveaux_Parametres,C1s),
	transpose(Matrice,Matrice_Transposee),
	mul_mat_vect(Matrice_Transposee,Nouveaux_Parametres,Parametres2),
	tous_inferieurs(Parametres2,Parametres,C2s),
	mul_vect_vect(Colonne,Nouveaux_Parametres,Max),
	append([Max >= Valeur|C1s],C2s,C3s),
	term_variables(Parametres,Vars),
    num_op:project(Vars,C3s,Vars,C4s).

/*
dual:dual([A,B,-A,-B],[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1],[1,2,-3,-4]],[0,0,0,0,1],1,Cs).
Cs = [4*A+ -1*B>=0,1*A>=1,1*B>=2, -2*A+3*B>=0] 

dual:dual([A,B,-A,-B],[[1,0,-1,0],[0,1,0,-1],[1,1,-1,-1]],[-1,-1,1],1,Cs).
Cs = [A-2*B=< -1,A-1/2*B>=1/2] 
*/
	


	
