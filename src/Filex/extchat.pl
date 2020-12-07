possessive(B,C,D,[],E,F,G,H,I,J,K,L,M,N) :-
   gen_case(K,O,M,P),
   np_head0(Q,R,S,O,T,P,U),
   possessive(Q,R,S,V,[pp(poss,np(C,B,E))|V],F,G,H,I,J,T,L,U,N).
possessive(B,C,D,E,F,B,C,D,E,F,G,G,H,H).

gen_case(A,B,C,D) :-
	cti:{A-B=<2,A-B>=0,B>=0,D>=1,C-D>= -1,A-B+C-D>=0}.
np_head0(A,B,C,D,E,F,G):-
	cti:{D-E>=0,A-1/2*D+1/2*E>=0,
	     A-2/3*D+2/3*E>= -1/3,A+1/3*C-2/3*D+2/3*E>=0,A+2*C-D+E>=0,
	     A-D+E>= -2,A+2*B-D+E>=0,A+1/3*B-2/3*D+2/3*E>=0,
	     B-1/2*C+1/2*D-1/2*E+1/2*F>=1,B-1/3*C+1/3*D-1/3*E+1/3*F-1/3*G>=2/3,
	     A+7/3*B-2/3*C-2/3*D+2/3*E+2/3*F-2/3*G>=4/3,
	     A+8*B-2*C-D+E+2*F-2*G>=4,C-F+G=<2,C-D+E-F+G=<1,
	     C+1/3*F-1/3*G>=2/3,C+1/2*D-1/2*E+1/2*F-1/2*G>=1,
	     C+1/2*F>=1,C-F=<1,B-1/3*C+1/3*F>=2/3,A-1/3*C+1/3*F>=2/3,
	     A-1/2*C+1/2*D-1/2*E+1/2*F>=1,C+D-E+F>=2,C-D+E-F=<0,B>=0,
	     A-2*C-D+E+2*F-2*G>= -4,A+6*C-D+E+2*F-2*G>=4,
	     A+5/3*C-2/3*D+2/3*E+2/3*F-2/3*G>=4/3,
	     A-2/3*C-2/3*D+2/3*E+2/3*F-2/3*G>= -1,
	     A-1/3*C+1/3*D-1/3*E+1/3*F-1/3*G>=2/3,
	     G>=0,C>=0,E>=0,A-1/5*C-1/5*D+1/5*E+1/5*F-1/5*G>=2/5,
	     B-1/4*C+1/4*F-1/4*G>=1/2}.