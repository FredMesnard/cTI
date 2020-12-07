%------------------------------------------------------------------------------
%	Benchmark Program - matrix*matrix multiplication
%
%	Copyright by Manuel Hermenegildo
%	Date: January 17 1986
%
%------------------------------------------------------------------------------

:-use_module(library(clpq)).


'{}'(_).  % car CTI ne connait pas clpq


%    predicate_term_condition(mmultiply(A,B,C),B*C+A*C+A*B)   !!!!!
%    mmultiply/3-([A,B,C]-[A>=0,B>=0,C>=0]-(precision=0)-(nb_ite=3))
mmultiply([],_,[]).
mmultiply([V0|Rest], V1, [Result|Others]):-  
            mmultiply(Rest, V1, Others),
    	    multiply(V1,V0,Result).

%    predicate_term_condition(multiply(A,B,C),A+B*C)
%    multiply/3-([A,B,C]-[A>=0,B>=0,C>=0]-(precision=0)-(nb_ite=3))
multiply([],_,[]).
multiply([V0|Rest], V1, [Result|Others]):-  
            multiply(Rest, V1, Others),
    	    vmul(V0,V1,Result).

%    predicate_term_condition(vmul(A,B,C),B+A)
%    vmul/3-([A,B,C]-[A>=0,B>=0,C>=0]-(precision=0)-(nb_ite=4))
vmul([],[],Z):-{Z=0}.
vmul([H1|T1], [H2|T2], Result):- 
	vmul(T1,T2, Newresult), 
	{Product= H1*H2,Result=Product+Newresult}.

%    predicate_term_condition(trans_m(A,B),B)        !!!!!
%    trans_m/2-([A,B]-[B>=0,A>=1]-(precision=0)-(nb_ite=3))
trans_m([[]|_],[]).
trans_m(M,[C1|Cn]):- trans_v(M,C1,R), trans_m(R,Cn).

%    predicate_term_condition(trans_v(A,B,C),C+B+A)
%    trans_v/3-([A,B,C]-[C>=0,A=B+C,B>=0]-(precision=0)-(nb_ite=3))
trans_v([],[],[]).
trans_v([[C11|C1n]|C],[C11|X],[C1n|Y]):- trans_v(C,X,Y).

%    predicate_term_condition(makematrix(A,B),A)
%    makematrix/2-([A,B]-[A-B=<1,A>=1]-(precision=0)-(nb_ite=3))
makematrix(N,Matrix):- makevector(N,Vec), makematrix(N,Vec,Matrix).

%    predicate_term_condition(makematrix(A,B,C),C+A)
%    makematrix/3-([A,B,C]-[A-C=<1,A>=1]-(precision=0)-(nb_ite=4))
makematrix(Z,_,[]):-cti:{Z=0}.	% on dit : Z naturel=0
makematrix(Rows,Vector,[Vector|T]):-
	cti:{Rows>0, Nextrow = Rows-1},    % Rows nat >= 1, Nextrow nat = Rows-1
	makematrix(Nextrow,Vector,T).

%    predicate_term_condition(makevector(A,B),B+A)
%    makevector/2-([A,B]-[A-1/3*B=<1]-(precision=0)-(nb_ite=5))
makevector(Z,[]):-cti:{Z=0}.
makevector(Cols,[Cols|T]):-
	cti:{Cols>0,Nextcol = Cols-1},
	makevector(Nextcol,T).













































