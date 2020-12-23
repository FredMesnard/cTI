:- module(cti_term_size,[cti_term_size/2]).

cti_term_size(X,X):-var(X),!.
cti_term_size(Cte,0):-atomic(Cte),!.
cti_term_size(Terme,Exp):-functor(Terme,_F,N),N>0,size(1,N,Terme,1,Exp).

% list-size:
%term_size([],0).
%term_size([_X|Xs],1+N):-term_size(Xs,N).

size(P,N,_Terme,Exp,Exp):-P is N+1,!.
size(I,N,Terme,E1,E2):-
	I=<N,J is I+1,
	arg(I,Terme,Ai),
	cti_term_size(Ai,Si),
	size(J,N,Terme,E1+Si,E2).

