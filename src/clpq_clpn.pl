:- module(clpq_clpn,[clpq_clpn/2]).

:- use_module(library(lists), [member/2]).

clpq_clpn([],[]).
clpq_clpn([Cq|Cs],[Cn|Ds]) :-
	Cq=..[_Oprel,A1,A2],
	recup_denoms(A1,[],L2),
	recup_denoms(A2,L2,Lds),
	ppcm(Lds,Ppcm),
	rel_q_n(Ppcm,Cq,Cn),
	%utils:write_list([Ppcm-Cq-Cn]),
	clpq_clpn(Cs,Ds).

rel_q_n(1,C1,C2)   :- !, rel_q_n_aux1(C1,C2).
rel_q_n(P,C1,C2) :- P > 1, rel_q_n_aux(C1,C2,P).

%% ppcm = 1
rel_q_n_aux1(E1 >= E2, F1 >= F2) :-term_q_n(E1,F1),term_q_n(E2,F2).
rel_q_n_aux1(E1 =< E2, F1 =< F2) :-term_q_n(E1,F1),term_q_n(E2,F2).
rel_q_n_aux1(E1 =  E2, F1 =  F2) :-term_q_n(E1,F1),term_q_n(E2,F2).

term_q_n(X,X) :- var(X),!.
term_q_n(N,N) :- integer(N),!.
term_q_n(-(X),-(X)) :- var(X),!.
term_q_n(0*_X,0) :-!.
term_q_n(N*X,N*X) :- integer(N),!.
term_q_n(rat(N,_),N) :- !.
term_q_n(rat(0,_)*_X,0) :-!.
term_q_n(rat(N,_)*X,N*X) :-var(X),!.
term_q_n(E+G,F+H) :-!,term_q_n(E,F),term_q_n(G,H).
term_q_n(E-G,F-H) :-!,term_q_n(E,F),term_q_n(G,H).
term_q_n(E,_) :-raise_exception(module(poly_ppl,term_q_n(E,_))).  % oops !

% ppcm >=2
rel_q_n_aux(E1 >= E2, F1 >= F2, Ppcm) :-term_q_n(E1,F1,Ppcm),term_q_n(E2,F2,Ppcm).
rel_q_n_aux(E1 =< E2, F1 =< F2, Ppcm) :-term_q_n(E1,F1,Ppcm),term_q_n(E2,F2,Ppcm).
rel_q_n_aux(E1 =  E2, F1 =  F2, Ppcm) :-term_q_n(E1,F1,Ppcm),term_q_n(E2,F2,Ppcm).

term_q_n(X,A,Ppcm) :-var(X),!,A=Ppcm*X.
term_q_n(N,A,Ppcm) :-integer(N),!,A=Ppcm*N.
term_q_n(-(X),A,Ppcm) :-var(X),!,A= -Ppcm*X.
term_q_n(0*_,0,_) :-!.
term_q_n(N*X,A*X,Ppcm) :-integer(N),!,A is N*Ppcm.
term_q_n(rat(N,D),A,Ppcm) :-!,A is N*Ppcm // D.
term_q_n(rat(N,D)*X,A,Ppcm) :-var(X),!,Coeff is N*Ppcm // D,A=Coeff*X.
term_q_n(E+G,F+H,Ppcm) :-!,term_q_n(E,F,Ppcm),term_q_n(G,H,Ppcm).
term_q_n(E-G,F-H,Ppcm) :-!,term_q_n(E,F,Ppcm),term_q_n(G,H,Ppcm).
term_q_n(E,_,_) :-raise_exception(module(poly_ppl,term_q_n(E,_,_))).  % oops !

%%%%
recup_denoms(E,Lds) :-recup_denoms(E,[],Lds).
recup_denoms(X,L1,L2) :-var(X),!,L1=L2.
recup_denoms(N,L1,L2) :-integer(N),!,L1=L2.
recup_denoms(-(X),L1,L2) :-var(X),!,L1=L2.
recup_denoms(N*X,L1,L2) :-integer(N),var(X),!,L1=L2.
recup_denoms(rat(_N,D),L1,L2) :-!,((D=1;member(D,L1)) -> L1=L2 ; [D|L1]=L2).
recup_denoms(rat(_N,D)*X,L1,L2) :-var(X),!,(D=1 -> L1=L2 ; [D|L1]=L2).
recup_denoms(E+G,L1,L3) :-!,recup_denoms(E,L1,L2),recup_denoms(G,L2,L3).
recup_denoms(E-G,L1,L3) :-!,recup_denoms(E,L1,L2),recup_denoms(G,L2,L3).
recup_denoms(E,_,_) :- raise_exception(module(poly_ppl,recup_denoms(E,_,_))).  % oops !

ppcm([],1).
ppcm([N],N) :- !.
ppcm([N1,N2|Ns],M) :- !,ppcm(N1,N2,N3),ppcm([N3|Ns],M).

ppcm(N1,N2,N3) :- Pgcd is gcd(N1,N2), C1 is N1 // Pgcd, C2 is N2 // Pgcd, N3 is C1*C2*Pgcd.
