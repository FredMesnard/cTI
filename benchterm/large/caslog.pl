%:- entry caslog(Complexity,File).
%:- entry caslog(File).

log2(X,Y) :-
  float(X),
  float(Y).
pow(X,Y,Z) :-
  float(X),
  float(Y),
  float(Z).

%
%  algebraic.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for performing algebraic computations
%  in normal-form.
%

%
%  add two normal-form expressions.
%
add_expr(_,bot,bot).
add_expr(bot,A2,bot) :-
	A2 \== bot.
add_expr(A1,inf,inf) :-
	A1 \== bot.
add_expr(inf,A2,inf) :-
	A2 \== bot,
	A2 \== inf.
add_expr(expr(T1,F1),expr(T2,F2),expr(T,F)) :-
	add_terms(T1,T2,T),
	add_factors(F1,F2,F).

%
%  add two normal-form terms.
%
add_terms(T1,[],T1).
add_terms([],T2,T2) :-
	T2 \== [].
add_terms([T|T1],T2,NT) :-
	T2 \== [],
	add_terms(T1,T2,Ts),
	add_term(Ts,T,NT).

add_term([],T,[T]).
add_term([term(T1,C1)|Ts],term(T2,C2),NT) :-
	T1 == T2,
	add_factors(C1,C2,C),
	(C == [] ->
		NT = Ts;
		NT = [term(T1,C)|Ts]).
add_term([term(T1,C1)|Ts],term(T2,C2),[term(T2,C2),term(T1,C1)|Ts]) :-
	T1 @< T2.
add_term([term(T1,C1)|Ts1],term(T2,C2),[term(T1,C1)|Ts]) :-
	T1 @> T2,
	add_term(Ts1,term(T2,C2),Ts).
	
%
%  add two normal-form factors.
%
add_factors(F1,[],F1).
add_factors([],F2,F2) :-
	F2 \== [].
add_factors([F|F1],F2,NF) :-
	F2 \== [],
	add_factors(F1,F2,Fs),
	add_factor(Fs,F,NF).

add_factor([],F,[F]).
add_factor([factor(F1,C1)|Fs],factor(F2,C2),NF) :-
	F1 == F2,
	C is C1+C2,
	(C =:= 0 ->
		NF = Fs;
		NF = [factor(F1,C)|Fs]).
add_factor([factor(F1,C1)|Fs],factor(F2,C2),[factor(F2,C2),factor(F1,C1)|Fs]) :-
	F1 @< F2.
add_factor([factor(F1,C1)|Fs1],factor(F2,C2),[factor(F1,C1)|Fs]) :-
	F1 @> F2,
	add_factor(Fs1,factor(F2,C2),Fs).

%
%  Subtract two normal-form expressions.
%
subtract_expr(_,bot,bot).
subtract_expr(bot,A2,bot) :-
	A2 \== bot.
subtract_expr(A1,inf,bot) :-
	A1 \== bot.
subtract_expr(inf,A2,inf) :-
	A2 \== bot,
	A2 \== inf.
subtract_expr(E1,E2,E) :-
	Minus1 is -1,
	normal_form(Minus1,MinusOne),
	multiply_expr(MinusOne,E2,E3),
	add_expr(E1,E3,E).

%
%  multiply two normal-form expressions.
%
multiply_expr(_,bot,bot).
multiply_expr(bot,A2,bot) :-
	A2 \== bot.
multiply_expr(A1,inf,inf) :-
	A1 \== bot.
multiply_expr(inf,A2,inf) :-
	A2 \== bot,
	A2 \== inf.
multiply_expr(expr(T1,F1),expr(T2,F2),expr(T,F)) :-
	multiply_terms(T1,T2,T3),
	multiply_term_factor(T1,F2,T4),
	multiply_term_factor(T2,F1,T5),
	add_terms(T3,T4,T6),
	add_terms(T5,T6,T),
	multiply_factors(F1,F2,F).
	
%
%  multiply two normal-form terms.
%
multiply_terms(_,[],[]).
multiply_terms([],T2,[]) :-
	T2 \== [].
multiply_terms([T|T1],T2,NT) :-
	T2 \== [],
	multiply_term(T2,T,T3),
	multiply_terms(T1,T2,T4),
	add_terms(T3,T4,NT).

multiply_term([],_,[]).
multiply_term([term(P1,F1)|Ts1],term(P2,F2),[term(P,F)|Ts]) :-
	multiply_factors(F1,F2,F),
	multiply_primaries(P1,P2,P),
	multiply_term(Ts1,term(P2,F2),Ts).

%
%  multiply two normal-form primaries.
%
multiply_primaries(P1,[],P1).
multiply_primaries([],P2,P2) :-
	P2 \== [].
multiply_primaries([P|P1],P2,NP) :-
	P2 \== [],
	multiply_primaries(P1,P2,Ps),
	multiply_primary(Ps,P,NP).

multiply_primary(Ps,P,NP) :-
	multiply_primary(Ps,P,Done,TP),
	(Done == 1 ->
		NP = TP;
		insert_order_list(Ps,P,NP)).

multiply_primary([],_,0,[]).
multiply_primary([P1|Ps],P1,1,[exp(expr([P1],[factor([],1)]),
	      expr([],[factor([],2)]))|Ps]) :-
	functor(P1,F,N),
	(F,N) \== (exp,2).
multiply_primary([exp(P1,C1)|Ps],exp(P1,C2),1,[exp(P1,C)|Ps]) :-
	add_expr(C1,C2,C).
multiply_primary([P1|Ps],exp(Expr,Exp),1,[exp(Expr,NExp)|Ps]) :-
	Expr = expr([P1],[factor([],1)]),
	normal_form(1,One),
	add_expr(One,Exp,NExp).
multiply_primary([exp(Expr,Exp)|Ps],P1,1,[exp(Expr,NExp)|Ps]) :-
	Expr = expr([P1],[factor([],1)]),
	normal_form(1,One),
	add_expr(One,Exp,NExp).
multiply_primary([P1|Ps1],P2,Done,[P1|Ps]) :-
	functor(P1,F1,N1),
	(F1,N1) \== (exp,2),
	functor(P2,F2,N2),
	(F2,N2) \== (exp,2),
	P1 \== P2,
	multiply_primary(Ps1,P2,Done,Ps).
multiply_primary([exp(P1,C1)|Ps1],exp(P2,C2),Done,[exp(P1,C1)|Ps]) :-
	P1 \== P2,
	multiply_primary(Ps1,exp(P2,C2),Done,Ps).
multiply_primary([P1|Ps1],exp(Expr,Exp),Done,[P1|Ps]) :-
	Expr \== expr([P1],[factor([],1)]),
	multiply_primary(Ps1,exp(Expr,Exp),Done,Ps).
multiply_primary([exp(Expr,Exp)|Ps1],P1,Done,[exp(Expr,Exp)|Ps]) :-
	Expr \== expr([P1],[factor([],1)]),
	multiply_primary(Ps1,P1,Done,Ps).


%
%  multiply a normal-form term and a normal-form factor.
%
multiply_term_factor(_,[],[]).
multiply_term_factor([],F2,[]) :-
	F2 \== [].
multiply_term_factor([term(T,F1)|Ts1],F2,[term(T,F)|Ts]) :-
	F2 \== [],
	multiply_factors(F1,F2,F),
	multiply_term_factor(Ts1,F2,Ts).

%
%  multiply two normal-form factors.
%
multiply_factors(_,[],[]).
multiply_factors([],F2,[]) :-
	F2 \== [].
multiply_factors([F|F1],F2,NF) :-
	F2 \== [],
	multiply_factor(F2,F,F3),
	multiply_factors(F1,F2,F4),
	add_factors(F3,F4,NF).

multiply_factor([],_,[]).
multiply_factor([factor(I1,C1)|Fs1],factor(I2,C2),[factor(I,C)|Fs]) :-
	C is C1*C2,
	multiply_items(I1,I2,I),
	multiply_factor(Fs1,factor(I2,C2),Fs).

%
%  multiply two normal-form items.
%
multiply_items(I1,[],I1).
multiply_items([],I2,I2) :-
	I2 \== [].
multiply_items([I|I1],I2,NI) :-
	I2 \== [],
	multiply_items(I1,I2,Is),
	multiply_item(Is,I,NI).

multiply_item(Is,I,NI) :-
	multiply_item(Is,I,Done,TI),
	(Done == 1 ->
		NI = TI;
		insert_order_list(Is,I,NI)).

multiply_item([],_,0,[]).
multiply_item([I1|Is],I1,1,[exp(expr([],[factor([I1],1)]),
	      expr([],[factor([],2)]))|Is]) :-
	functor(I1,F,N),
	(F,N) \== (exp,2).
multiply_item([exp(I1,C1)|Is],exp(I1,C2),1,[exp(I1,C)|Is]) :-
	add_expr(C1,C2,C).
multiply_item([I1|Is],exp(Expr,Exp),1,[exp(Expr,NExp)|Is]) :-
	Expr = expr([],[factor([I1],1)]),
	normal_form(1,One),
	add_expr(One,Exp,NExp).
multiply_item([exp(Expr,Exp)|Is],I1,1,[exp(Expr,NExp)|Is]) :-
	Expr = expr([],[factor([I1],1)]),
	normal_form(1,One),
	add_expr(One,Exp,NExp).
multiply_item([I1|Is1],I2,Done,[I1|Is]) :-
	functor(I1,F1,N1),
	(F1,N1) \== (exp,2),
	functor(I2,F2,N2),
	(F2,N2) \== (exp,2),
	I1 \== I2,
	multiply_item(Is1,I2,Done,Is).
multiply_item([exp(I1,C1)|Is1],exp(I2,C2),Done,[exp(I1,C1)|Is]) :-
	I1 \== I2,
	multiply_item(Is1,exp(I2,C2),Done,Is).
multiply_item([I1|Is1],exp(Expr,Exp),Done,[I1|Is]) :-
	Expr \== expr([],[factor([I1],1)]),
	multiply_item(Is1,exp(Expr,Exp),Done,Is).
multiply_item([exp(Expr,Exp)|Is1],I1,Done,[exp(Expr,Exp)|Is]) :-
	Expr \== expr([],[factor([I1],1)]),
	multiply_item(Is1,I1,Done,Is).

%
%  Divide two normal-form expressions.
%
divide_expr(_,bot,bot).
divide_expr(bot,A2,bot) :-
	A2 \== bot.
divide_expr(A1,inf,Zero) :-
	A1 \== bot,
	normal_form(0,Zero).
divide_expr(inf,A2,inf) :-
	A2 \== bot,
	A2 \== inf.
divide_expr(E1,E2,E) :-
	general_form(E2,E3),
	(number(E3) ->
		(X is 1/E3,
		 normal_form(X,E4));
		(Minus1 is -1,
		 normal_form(Minus1,MinusOne),
		 exp_expr(E2,MinusOne,E4))),
	multiply_expr(E1,E4,E).

%
%  compute the exponent in normal-form.
%
exp_expr(_,bot,bot).
exp_expr(bot,A2,bot) :-
	A2 \== bot.
exp_expr(A1,inf,inf) :-
	A1 \== bot.
exp_expr(inf,A2,inf) :-
	A2 \== bot,
	A2 \== inf.
exp_expr(_,E2,expr([],[factor([],1)])) :-
	E2 == expr([],[]),!.
exp_expr(E1,E2,E1) :-
	E2 = expr([],[factor([],I)]),
	I =:= 1,!.
exp_expr(E1,_,expr([],[])) :-
	E1 == expr([],[]),!.
exp_expr(E1,_,expr([],[factor([],1)])) :-
	E1 = expr([],[factor([],I)]),
	I =:= 1,!.
exp_expr(E1,E2,expr([],[factor([],E)])) :-
	E1 = expr([],[factor([],I1)]),
	E2 = expr([],[factor([],I2)]),!,
	F1 is float(I1),
	F2 is float(I2),
	pow(F1,F2,E).
exp_expr(E1,E2,expr([],[factor([exp(E1,E2)],1)])).

%
%  compute the logarithm in normal-form.
%
log_expr(_,bot,bot).
log_expr(bot,A2,bot) :-
	A2 \== bot.
log_expr(A1,inf,inf) :-
	A1 \== bot.
log_expr(inf,A2,bot) :-
	A2 \== bot,
	A2 \== inf.
log_expr(E1,E1,expr([],[factor([],1)])).
log_expr(E1,E2,expr([],[])) :-
	E1 \== E2,
	E2 = expr([],[factor([],I)]),
	I =:= 1,!.
log_expr(E1,E2,expr([],[factor([],F)])) :-
	E1 \== E2,
	E1 = expr([],[factor([],I1)]),
	E2 = expr([],[factor([],I2)]),!,
	F1 is float(I1),
	log2(F1,NF1),
	F2 is float(I2),
	log2(F2,NF2),
	F is NF2/NF1.
log_expr(E1,E2,expr([],[factor([log(E1,E2)],1)])) :-
	E1 \== E2.

%
%  compute the factorial of a normal-form expression.
%
factorial_expr(bot,bot).
factorial_expr(inf,inf).
factorial_expr(E,expr([],[factor([],1)])) :-
	E == expr([],[]),!.
factorial_expr(E,expr([],[factor([],1)])) :-
	E = expr([],[factor([],I)]),
	I =:= 1,!.
factorial_expr(E,expr([],[factor([fact(E)],1)])).

%
%  Insert an element into a list based on standard order.
%
insert_order_list([],I2,[I2]).
insert_order_list([I1|Is],I2,[I2,I1|Is]) :-
	I1 @< I2.
insert_order_list([I1|Is1],I2,[I1|Is]) :-
	I1 @> I2,
	insert_order_list(Is1,I2,Is).
%
%  diff_equ.pl			Nai-Wei Lin			February, 1992
%
%  This file contains the procedures for solving linear difference equations.
%

%
%  Solve a difference equation.
%
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	first_order_con_diff_equ(Equ,Var,Ivalue,An,Bn),!,
	solve_focde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	first_order_var_diff_equ(Equ,Var,Ivalue,An,Bn),!,
	solve_fovde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	second_order_con_diff_equ(Equ,Var,Ivalue,R1,R2,A1n,A2n,Bn),!,
	solve_socde(Var,Ivalue,R1,R2,A1n,A2n,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	divide_conquer_diff_equ(Equ,Var,Ivalue,A,C,Bn),!,
	solve_dcde(Var,Ivalue,A,C,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
	mutual_size_diff_equ(Equ,Var,Pred,Ivalue,An,Bn),!,
	solve_msde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
	mutual_struct_diff_equ(Equ,Var,Pred,Ivalue,Bn),!,
	solve_mstde(Var,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
	mutual_list_diff_equ(Equ,Var,Pred,Ivalue,Bn),!,
	solve_mlsde(Var,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
	size_struct_diff_equ(Equ,Var,Pred,Ivalue,Bn),!,
	solve_ssde(Var,Bn,Sol).
solve_diff_equ(_,_,_,_,inf).

%
%  Test if a difference equation is linear first-order with constant 
%  coefficient.
%
first_order_con_diff_equ(Equ,Var,Ivalue,An,Bn) :-
	first_order_diff_equ(Equ,Var,Ivalue,A,B),
	A = [factor([],_)],
	An = expr([],A),
	const_coeff_solvable(B,Var),
	Bn = expr([],B).
	
%
%  Test if a difference equation is linear first-order with variable 
%  coefficient.
%
first_order_var_diff_equ(Equ,Var,Ivalue,An,Bn) :-
	first_order_diff_equ(Equ,Var,Ivalue,A,B),
	An = expr([],A),
	Bn = expr([],B).
	
%
%  Test if a difference equation is linear first-order.
%
first_order_diff_equ(Equ,Var,Ivalue,An,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,_,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	I =:= -1,
	Ivalue = [val(_,_)].

%
%  Solve a linear first-order constant coefficient difference equation.
%
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	normal_form(1,A),!,
	solve_fovde(Var,Ivalue,A,Bn,Sol).
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	Bn = expr([],BN),
	first_order_par_sol(BN,Var,A,Sol1),
	first_order_gen_sol(Var,Ivalue,A,Sol1,Sol).

%
%
first_order_par_sol([],_,_,Sol) :-
	normal_form(0,Sol).
first_order_par_sol([F|Fs],Var,A,Sol) :-
	first_order_par_sol1(F,Var,A,Sol1),
	first_order_par_sol(Fs,Var,A,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
first_order_par_sol1(factor([],C),_,An,Sol) :-
	general_form(An,A),
	normal_form(C/(1-A),Sol).
first_order_par_sol1(factor([Var],C),Var,An,Sol) :-
	general_form(An,A),
	normal_form((C/(1-A))*Var-(A*C)/exp(1-A,2),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	general_form(An,A),
	normal_form((C/(1-A))*exp(Var,2)-((2*A*C)/exp(1-A,2))*Var+
		(exp(A,2)*C+A*C)/exp(1-A,3),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =\= D,
	normal_form((C*D/(D-A))*exp(D,Var),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =:= D,
	normal_form(C*Var*exp(D,Var),Sol).

%
%
first_order_gen_sol(Var,Ivalue,An,PSol,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	general_form(PSol,PSol1),
	general_form(Iindex,Iindex1),
	substitute(PSol1,Var,Iindex1,PSol2),
	normal_form(PSol2,PSol3),
	subtract_expr(Ival,PSol3,N),
	exp_expr(An,Iindex,D),
	divide_expr(N,D,C),
	normal_form(Var,Var1),
	exp_expr(An,Var1,G1),
	multiply_expr(C,G1,G2),
	add_expr(G2,PSol,Sol).

%
%  Solve a linear first-order variable coefficient difference equation.
%
solve_fovde(Var,Ivalue,An,Bn,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	substitute(An,Var,$(i),AN),
	substitute(Bn,Var,$(j),BN),
	normal_form(1,One),
	add_expr(Iindex,One,Lower1),
	normal_form(Var,Upper),
	prod_expr($(i),Lower1,Upper,AN,P1),
	multiply_expr(Ival,P1,S1),
	normal_form(($(j))+1,Lower2),
	prod_expr($(i),Lower2,Upper,AN,P2),
	multiply_expr(BN,P2,P3),
	sum_expr($(j),Lower1,Upper,P3,S2),
	add_expr(S1,S2,Sol).

%
%  Test if a difference equation is linear second-order with constant 
%  coefficient.
%
second_order_con_diff_equ(Equ,Var,Ivalue,R1,R2,A1,A2,Bn) :-
	second_order_diff_equ(Equ,Var,Ivalue,An1,An2,B),
	An1 = [factor([],A1)],
	An2 = [factor([],A2)],
	real_quadratic_roots(A1,A2,R1,R2),
	const_coeff_solvable(B,Var),
	Bn = expr([],B).
	
%
%  Test if a difference equation is linear second-order.
%
second_order_diff_equ(Equ,Var,Ivalue,A1,A2,Bn) :-
	Equ = expr([term([Dvar1],A1),term([Dvar2],A2)],Bn),!,
	userfunc(Dvar1),
	functor(Dvar1,F,1),
	arg(1,Dvar1,Arg1),
	Arg1 = expr([],[factor([Var],1),factor([],I1)]),
	First is -1,
	I1 =:= First,
	userfunc(Dvar2),
	functor(Dvar2,F,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	Second is -2,
	I2 =:= Second,
	Ivalue = [val(Iindex1,_),val(Iindex2,_)],
	general_form(Iindex1,Index1),
	integer(Index1),
	general_form(Iindex2,Index2),
	integer(Index2),
	(Index1 > Index2 ->
		D is Index1-Index2;
		D is Index2-Index1),
	D =:= 1.
second_order_diff_equ(Equ,Var,Ivalue,A1,A2,Bn) :-
	Equ = expr([term([Dvar2],A2)],Bn),
	userfunc(Dvar2),
	functor(Dvar2,_,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	Second is -2,
	I2 =:= Second,
	A1 = [factor([],0)],
	Ivalue = [val(Iindex1,_),val(Iindex2,_)],
	general_form(Iindex1,Index1),
	integer(Index1),
	general_form(Iindex2,Index2),
	integer(Index2),
	(Index1 > Index2 ->
		D is Index1-Index2;
		D is Index2-Index1),
	D =:= 1.

%
%  Solve a linear second-order constant coefficient difference equation.
%
solve_socde(Var,Ivalue,R1,R2,A1,A2,Bn,GSol) :-
	Bn = expr([],BN),
	second_order_par_sol(BN,Var,R1,R2,A1,A2,PSol),
	second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol).

%
%
second_order_par_sol([],_,_,_,_,_,Sol) :-
	normal_form(0,Sol).
second_order_par_sol([F|Fs],Var,R1,R2,A1,A2,Sol) :-
	second_order_par_sol1(F,Var,R1,R2,A1,A2,Sol1),
	second_order_par_sol(Fs,Var,R1,R2,A1,A2,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
second_order_par_sol1(factor([],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
		normal_form(C/(1-A1-A2),Sol);
		(U =:= 1 ->
			normal_form((C/(A1+2*A2))*Var,Sol);
			normal_form((C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([Var],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
/*
		(normal_form(C/(1-A1-A2),D1),
		 normal_form(-(A1+2*A2)/(1-A1-A2),D2),
		 multiply_expr(D1,D2,D0),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E1,D0,Sol));
*/
		normal_form((C/(1-A1-A2))*Var+
			    (-C*(A1+2*A2)/exp(1-A1-A2,2)),Sol);
		(U =:= 1 ->
			normal_form((C/(2*(A1+2*A2)))*exp(Var,2)+
			     ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*Var,Sol);
			normal_form((C/6)*exp(Var,3)+(C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	unit_count(R1,R2,U),
	(U =:= 0 ->
		(normal_form(C/(1-A1-A2),D2),
		 normal_form(-2*(A1+2*A2)/(1-A1-A2),TD1),
		 multiply_expr(TD1,D2,D1),
		 normal_form((A1+2*A2)/(1-A1-A2),TD01),
		 multiply_expr(TD01,D1,TD03),
		 normal_form((A1+4*A2)/(1-A1-A2),TD02),
		 multiply_expr(TD02,D2,TD04),
		 subtract_expr(TD04,TD03,D0),
		 normal_form(exp(Var,2),Var2),
		 multiply_expr(D2,Var2,E2),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E2,E1,Sol1),
		 add_expr(Sol1,D0,Sol));
		(U =:= 1 ->
			normal_form((C/(3*(A1+2*A2)))*exp(Var,3)+
			   ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*exp(Var,2)+
			   ((C*(A1*A1+4*A1*A2+16*A2*A2))/(6*exp(A1+2*A2,3)))*
				Var,Sol);
			normal_form((C/12)*exp(Var,4)+(C/3)*exp(Var,3)+
			   (5*C/12)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	R1 =\= D,
	R2 =\= D,
	normal_form(((C*exp(D,2))/(exp(D,2)-A1*D-A2))*exp(D,Var),Sol).

%
%
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =\= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = exp(R2,Index2)*exp(R1,Index1)-exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*exp(R2,Var)+GPSol,GSol).
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =:= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = Index2*exp(R2,Index2)*exp(R1,Index1)-
	     Index1*exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*Index1*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*Var*exp(R2,Var)+GPSol,GSol).

%
%
divide_conquer_diff_equ(Equ,Var,Ivalue,A,C,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,_,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],I)]),
	I < 1,
	C is 1/I,
	An = [factor([],A)],
	divide_conquer_solvable(Bn,Var),
	Ivalue = [val(Iindex,_)],
	normal_form(1,Iindex).

%
%
solve_dcde(Var,[val(_,Ival)],A,C,Bn,Sol) :-
	divide_conquer_par_sol(Bn,Var,A,C,PSol),
	normal_form(exp(Var,log(C,A)),E),
	multiply_expr(Ival,E,GSol),
	add_expr(GSol,PSol,Sol).

%
%
divide_conquer_par_sol([],_,_,_,Sol) :-
	normal_form(0,Sol).
divide_conquer_par_sol([factor(I,D)|F],Var,A,C,Sol) :-
	divide_conquer_order(I,Var,O),
	simplification(exp(C,O),X),
	divide_conquer_par_sol1(O,X,Var,A,C,Sol1),
	normal_form(D,D1),
	multiply_expr(D1,Sol1,Sol2),
	divide_conquer_par_sol(F,Var,A,C,Sols),
	add_expr(Sol2,Sols,Sol).

%
%
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =:= E,
	normal_form(exp(Var,O)*log(C,Var),Sol).
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =\= E,
	N = exp(Var,log(C,A))-exp(Var,O),
	D = A-exp(C,O),
	Exp = exp(C,O)*(N/D),
	normal_form(Exp,Sol).

%
%
divide_conquer_order([],_,0).
divide_conquer_order([Var],Var,1).
divide_conquer_order([exp(E1,E2)],Var,I) :-
	normal_form(Var,E1),
	general_form(E2,E),
	I is integer(E).

%
%  Test if an equation is solvable using constant coefficient solving method.
%
const_coeff_solvable([],_).
const_coeff_solvable([factor(I,_)|F],Var) :-
	const_coeff_solvable1(I,Var),
	const_coeff_solvable(F,Var).

const_coeff_solvable1([],_).
const_coeff_solvable1([Var],Var).
const_coeff_solvable1([exp(E1,E2)],Var) :-
	normal_form(Var,E1),
	normal_form(2,E2).
const_coeff_solvable1([exp(E1,E2)],Var) :-
	E1 = expr([],[factor([],_)]),
	normal_form(Var,E2).

%
%
exp_only_expr([],_,_,_).
exp_only_expr([factor(I,_)|F],Var,R1,R2) :-
	exp_only_expr1(I,Var,R1,R2),
	exp_only_expr(F,Var,R1,R2).

exp_only_expr1([exp(E1,E2)],Var,R1,R2) :-
	E1 = expr([],[factor([],C)]),
	C =\= R1,
	C =\= R2,
	normal_form(Var,E2).

%
%
real_quadratic_roots(A1,A2,R1,R2) :-
	D is A1*A1+4*A2,
	(D >= 0 ->
		normal_form(exp(D,0.5),ND),
		general_form(ND,D1),
		R1 is (A1+D1)/2,
		R2 is (A1-D1)/2).

%
%
unit_count(R1,R2,I) :-
	unit_count(R1,I1),
	unit_count(R2,I2),
	I is I1+I2.

unit_count(R,1) :-
	R =:= 1.
unit_count(R,0) :-
	R =\= 1.

%
%
divide_conquer_solvable([],_).
divide_conquer_solvable([factor(I,_)|F],Var) :-
	divide_conquer_solvable1(I,Var),
	divide_conquer_solvable(F,Var).

divide_conquer_solvable1([],_).
divide_conquer_solvable1([Var],Var).
divide_conquer_solvable1([exp(E1,E2)],Var) :-
	normal_form(Var,E1),
	general_form(E2,I),
	I1 is integer(I),
	I =:= I1.

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_msde(Var,Ivalue,An,Bn,Sol) :-
	normal_form(1,A),
	solve_fovde(Var,Ivalue,A,Bn,Sol1),
	substitute(An,Var,$(i),AN),
	normal_form(Var,Upper),
	sum_expr($(i),A,Upper,AN,Sol2),
	add_expr(Sol1,Sol2,Sol).

%
%  Test if a difference equation, obtained from an auxiliary predicate
%  based on measure term-size and builtins functor/3 and arg/3, is 
%  linear first-order with coefficient 1. That is,
%  f(n) = f(n-1) + g(arg(x,n)).
%
mutual_size_diff_equ(Equ,Var,Pred,Ivalue,A,B) :-
	Equ = expr(Terms,Bn),
	primary_term(Terms,Pred,Var,MTerm),
	mutual_size_solvable(Bn,Var),
	normal_form(0,Zero),
	Ivalue = [val(Zero,_)],
	A = expr([MTerm],[]),
	B = expr([],Bn).

%
%
primary_term([T1,T2],Pred,Var,T2) :-
	primary_term(T1,Pred,Var),
	nonprimary_term(T2).
primary_term([T1,T2],Pred,Var,T1) :-
	primary_term(T2,Pred,Var),
	nonprimary_term(T1).

primary_term(term([P],[factor([],1)]),Pred,Var) :-
	userfunc(P),
	functor(P,F,1),
	Pred = F/_,
	arg(1,P,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	I =:= -1.
nonprimary_term(term([P],[factor([],1)])) :-
	userfunc(P),
	functor(P,_,N),
	N =\= 1.

%
%
mutual_size_solvable([],_).
mutual_size_solvable([factor(I,_)|F],Var) :-
	const_coeff_solvable(I,Var),
	mutual_size_solvable(F,Var).
mutual_size_solvable([factor(I,_)|F],Var) :-
	I = [arg(E1,E2)],
	general_form(E1,Var1),
	variable(Var1),
	normal_form(Var,E2),
	mutual_size_solvable(F,Var).

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_mstde(Var,Bn,Sol) :-
	solve_mstde1(Bn,Var,Sol).

solve_mstde1([],_,Zero) :-
	normal_form(0,Zero).
solve_mstde1([B|Bn],Var,Sol) :-
	solve_mstde2(B,Var,Sol1),
	solve_mstde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_mstde2(factor([],C),Var,Sol) :-
	normal_form(C*Var,Sol).
solve_mstde2(factor([Var],C),Var,Sol) :-
	normal_form((C/2)*Var*(Var+1),Sol).
solve_mstde2(factor([arity(E)],C),Var,Sol) :-
	normal_form(Var,E),
	normal_form(C*(Var-1),Sol).

%
%  Test if a mutual size difference equation is linear first-order with coeff 1.
%
mutual_struct_diff_equ(Equ,Var,F/_,Ivalue,Bn) :-
	Equ = expr(Term,Bn),
	NTerm = expr(Term,[]),
	general_form(NTerm,GTerm),
	GTerm = sum(Index,1,arity(Var),Expr),
	functor(Expr,F,1),
	arg(1,Expr,arg(Var,Index)),
	normal_form(0,Zero),
	normal_form(1,One),
	Ivalue = [val(Iindex,Ival)],
	(Iindex == Zero; Iindex == One),
	general_form(Ival,GIval),
	mutual_struct_solvable(Bn,Var,GIval).

%
%
mutual_struct_solvable([],_,_).
mutual_struct_solvable([B|Bn],Var,GIval) :-
	mutual_struct_solvable1(B,Var,GIval),
	mutual_struct_solvable(Bn,Var,GIval).

mutual_struct_solvable1(factor([],Val),_,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([Var],Val),Var,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([arity(Expr)],_),Var,_) :-
	normal_form(Var,Expr).

%
%  Test if a difference equation based on measure term-size on a list
%  is linear ``first-order'' with coefficient 1. That is,
%  f(n) = f(head(n)) + f(tail(n)) + g(n), where
%  g(n) is either d, d*n, head(n), or tail(n).
%
mutual_list_diff_equ(Equ,Var,F/_,Ivalue,Bn) :-
	Equ = expr(Term,Bn),
	mutual_list_term(Term,Var,F),
	normal_form(1,One), Ivalue = [val(One,Ival)],
	general_form(Ival,GIval),
	mutual_list_solvable(Bn,Var,GIval).

mutual_list_term([Term1,Term2],Var,Pred) :-
	mutual_list_head(Term2,Var,Pred),
	mutual_list_tail(Term1,Var,Pred).

mutual_list_head(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,head(Var)).

mutual_list_tail(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,tail(Var)).

%
%
mutual_list_solvable([],_,_).
mutual_list_solvable([B|Bn],Var,GIval) :-
	mutual_list_solvable1(B,Var,GIval),
	mutual_list_solvable(Bn,Var,GIval).

mutual_list_solvable1(factor([],Val),_,GIval) :- Val >= GIval.	% d
mutual_list_solvable1(factor([Var],Val),Var,GIval) :- Val >= GIval.	%d*n
mutual_list_solvable1(factor([head(Expr)],_),Var,_) :- normal_form(Var,Expr).
mutual_list_solvable1(factor([tail(Expr)],_),Var,_) :- normal_form(Var,Expr).

%
%  Solve a linear first-order difference equation with coefficient 1 based
%  on measure term-size on a list.
%
solve_mlsde(Var,[],Sol) :- normal_form(Var,Sol).
solve_mlsde(Var,Bn,Sol) :- Bn \== [], solve_mlsde1(Bn,Var,Sol).

solve_mlsde1([],_,Zero) :- normal_form(0,Zero).
solve_mlsde1([B|Bn],Var,Sol) :-
	solve_mlsde2(B,Var,Sol1),
	solve_mlsde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_mlsde2(factor([],C),Var,Sol) :- normal_form(C*Var,Sol).
solve_mlsde2(factor([Var],C),Var,Sol) :- normal_form((C/2)*Var*(Var+1),Sol).
solve_mlsde2(factor([head(E)],C),Var,Sol) :-
	normal_form(Var,E), normal_form(C*exp(Var-1,2)/2,Sol).
solve_mlsde2(factor([tail(E)],C),Var,Sol) :-
	normal_form(Var,E), normal_form(C*exp(Var-1,2)/2,Sol).

%
%  Test if a difference equation based on measure term-size on a structure
%  is linear ``first-order'' with coefficient 1. That is,
%  f(n) = f(arg(n,m)) + f(arg(n,m-1)) + ... + g(n), where
%  g(n) is either d, d*n, or arg(n,i).
%
size_struct_diff_equ(Equ,Var,F/_,Ivalue,Bn) :-
	Equ = expr(Term,Bn),
	size_struct_terms(Term,Var,F),
	normal_form(0,Zero), normal_form(1,One), 
	Ivalue = [val(Index,Ival)],
	(Index == Zero; Index == One),
	general_form(Ival,GIval),
	size_struct_solvable(Bn,Var,GIval).

size_struct_terms([],_,_).
size_struct_terms([Term|Terms],Var,Pred) :-
	size_struct_term(Term,Var,Pred),
	size_struct_terms(Terms,Var,Pred).

size_struct_term(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,arg(Var,I)),
	integer(I).

%
size_struct_solvable([],_,_).
size_struct_solvable([B|Bn],Var,GIval) :-
	size_struct_solvable1(B,Var,GIval),
	size_struct_solvable(Bn,Var,GIval).

size_struct_solvable1(factor([],Val),_,GIval) :- Val >= GIval.	% d
size_struct_solvable1(factor([Var],Val),Var,GIval) :- Val >= GIval.	%d*n
size_struct_solvable1(factor([arg(Expr1,Expr2)],_),Var,_) :- 	% arg(n,i)
	normal_form(Var,Expr1), general_form(Expr2,I), integer(I).

%
%  Solve a linear first-order difference equation with coefficient 1 based
%  on measure term-size on a structure.
%
solve_ssde(Var,[],Sol) :- normal_form(Var,Sol).
solve_ssde(Var,Bn,Sol) :- Bn \== [], solve_mlsde1(Bn,Var,Sol).

solve_ssde1([],_,Zero) :- normal_form(0,Zero).
solve_ssde1([B|Bn],Var,Sol) :-
	solve_ssde2(B,Var,Sol1),
	solve_ssde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_ssde2(factor([],C),Var,Sol) :- normal_form(C*Var,Sol).
solve_ssde2(factor([Var],C),Var,Sol) :- normal_form((C/2)*Var*(Var+1),Sol).
solve_ssde2(factor([arg(_,_)],C),Var,Sol) :-
	normal_form((C/2)*Var*(Var+1),Sol).	% an approx. - same as d*n

%
%  general_form.pl		Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for transforming the normal-form
%  expressions into the general-form expressions.
%

%
%  Transform a normal-form expression into a general-form expression.
%
general_form(bot,bot).
general_form(inf,inf).
general_form(expr(T,F),Y) :-
	terms_general_form(T,0,Y1),
	factors_general_form(F,Y1,Y).

%
%  Transform the normal-form terms into a general-form expression.
%
terms_general_form([],Y,Y).
terms_general_form([term(P,F)|Ts],X,Y) :-
	primaries_general_form(P,P1),
	factors_general_form(F,0,F1),
	((integer(F1), F1 < 0) ->
		(MF1 is -F1,
		 multiply(MF1,P1,Y1),
		 subtraction(X,Y1,Y2));
		(multiply(F1,P1,Y1),
		 addition(X,Y1,Y2))),
	terms_general_form(Ts,Y2,Y).

%
%  Transform the normal-form factors into a general-form expression.
%
factors_general_form([],Y,Y).
factors_general_form([factor(I,C)|Fs],X,Y) :-
	items_general_form(I,1,I1),
	(C > 0 ->
		(multiply(C,I1,Y1),
		 addition(X,Y1,Y2));
		(MC is -C,
		 multiply(MC,I1,Y1),
		 subtraction(X,Y1,Y2))),
	factors_general_form(Fs,Y2,Y).

%
%  Transform the normal-form primaries into a general-form expression.
%
primaries_general_form(P,Y) :-
	items_general_form(P,1,Y).

%
%  Transform the normal-form items into a general-form expression.
%
items_general_form([],Y,Y).
items_general_form([I|Is],X,Y) :-
	item_general_form(I,Y1),
	multiply(X,Y1,Y2),
	items_general_form(Is,Y2,Y).

%
%  Transform a normal-form primary or item into a general-form expression.
%
item_general_form(I,I) :-
	variable(I).
item_general_form(exp(E1,E2),exp(G1,G2)) :-
	general_form(E1,G1),
	general_form(E2,G2).
item_general_form(log(E1,E2),log(G1,G2)) :-
	general_form(E1,G1),
	general_form(E2,G2).
item_general_form(fact(E),fact(G)) :-
	general_form(E,G).
item_general_form(I,Y) :-
	functor(I,sum,4),
	functor(Y,sum,4),
	function_general_form(4,I,Y).
item_general_form(I,Y) :-
	functor(I,prod,4),
	functor(Y,prod,4),
	function_general_form(4,I,Y).
item_general_form(I,Y) :-
	functor(I,arg,2),
	functor(Y,arg,2),
	function_general_form(2,I,Y).
item_general_form(I,Y) :-
	functor(I,arity,1),
	functor(Y,arity,1),
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	functor(I,head,1),
	functor(Y,head,1),
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	functor(I,tail,1),
	functor(Y,tail,1),
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	userfunc(I),
	functor(I,F,N),
	functor(Y,F,N),
	function_general_form(N,I,Y).

%
%  Transform a normal-form user-defined function into a general-form expression.
%
function_general_form(0,_,_).
function_general_form(N,I,Y) :-
	N > 0,
	arg(N,I,Arg),
	general_form(Arg,Arg1),
	arg(N,Y,Arg1),
	N1 is N-1,
	function_general_form(N1,I,Y).
foreign_file('math.o',[pow,log2]).
foreign(pow,pow(+float,+float,[-float])).
foreign(log2,log2(+float,[-float])).
:- load_foreign_files(['math.o'],['-lm']).
%
%  maxmin.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for performing maximum and minimum
%  functions in normal-form.
%

%
%  Perform the maximum function in normal-form.
%
max_expr(_,bot,bot).
max_expr(bot,E2,bot) :-
	E2 \== bot.
max_expr(E1,inf,inf) :-
	E1 \== bot.
max_expr(inf,E2,inf) :-
	E2 \== bot,
	E2 \== inf.
max_expr(E1,E2,E) :-
	Minus1 is -1,
	normal_form(Minus1,Y1),
	multiply_expr(Y1,E2,Y2),
	add_expr(E1,Y2,Y),
	max_sign_expr(E1,E2,Y,E).

%
%  Determine the maximum based on the sign of the difference.
%
max_sign_expr(E1,_,Y,E1) :-
	positive_expr(Y).
max_sign_expr(_,E2,Y,E2) :-
	negative_expr(Y).
max_sign_expr(E1,E2,Y,E) :-
	((positive_expr(Y);negative_expr(Y)) ->
		fail;
		max_term_by_term(E1,E2,E)).

%
%  Perform the maximum function in normal-form.
%
min_expr(_,bot,bot).
min_expr(bot,E2,bot) :-
	E2 \== bot.
min_expr(E1,inf,E1) :-
	E1 \== bot.
min_expr(inf,E2,E2) :-
	E2 \== bot,
	E2 \== inf.
min_expr(E1,E2,E) :-
	Minus1 is -1,
	normal_form(Minus1,Y1),
	multiply_expr(Y1,E2,Y2),
	add_expr(E1,Y2,Y),
	min_sign_expr(E1,E2,Y,E).

%
%  Determine the minimum based on the sign of the difference.
%
min_sign_expr(E1,_,Y,E1) :-
	negative_expr(Y).
min_sign_expr(_,E2,Y,E2) :-
	positive_expr(Y).
min_sign_expr(E1,E2,Y,E) :-
	((positive_expr(Y);negative_expr(Y)) ->
		fail;
		(ground_size(E1,S1),
		 ground_size(E2,S2),
		 (S1 =< S2 ->
			E = E1;
			E = E2))).

%
%  Test if a normal-form expression is positive.
%
positive_expr(expr(T,F)) :-
	positive_terms(T),
	positive_factors(F).

%
%  Test if the normal-form terms are positive.
%
positive_terms([]).
positive_terms([term(_,F)|Ts]) :-
	positive_factors(F),
	positive_terms(Ts).

%
%  Test if the normal-form factors are positive.
%
positive_factors([]).
positive_factors([F|Fs]) :-
	positive_factor(F),
	positive_factors(Fs).

positive_factor(factor(I,C)) :-
	pos_factor(I,C).

pos_factor([],C) :-
	C > 0.
pos_factor([I|Is],C) :-
	positive_item(I),
	pos_factor(Is,C).

%
%  Test if a normal-form item is positive.
%
positive_item(V) :- variable(V).
positive_item(exp(_,expr([],[factor([],_)]))).
positive_item(exp(expr([],[factor([],C)]),_)) :-
	C > 0.

%
%  Test if a normal-form expression is negative.
%
negative_expr(expr(T,F)) :-
	negative_terms(T),
	negative_factors(F).

%
%  Test if the normal-form terms are negative.
%
negative_terms([]).
negative_terms([term(_,F)|Ts]) :-
	negative_factors(F),
	negative_terms(Ts).

%
%  Test if the normal-form factors are negative.
%
negative_factors([]).
negative_factors([F|Fs]) :-
	negative_factor(F),
	negative_factors(Fs).

negative_factor(factor(I,C)) :-
	neg_factor(I,C).

neg_factor([],C) :-
	C < 0.
neg_factor([I|Is],C) :-
	positive_item(I),
	neg_factor(Is,C).

%
%  Determine the maximum of two expressions term by term.
%
max_term_by_term(expr(T1,F1),expr(T2,F2),expr(T,F)) :-
	%nl,
	%write(T1),nl,
	%write(T2),nl,
	max_terms(T1,T2,T),
	%write(T),nl,
	max_factors(F1,F2,F).

%
max_terms([term([P1],C1)],[term([P2],C2)],Term) :-
	userfunc(P1), functor(P1,F,N),
	userfunc(P2), functor(P2,F,N),
	arg(1,P1,A), arg(1,P2,A), !,
	max_terms(N,P1,P2,C1,C2,Term).
max_terms(T1,T2,T) :-
	add_terms(T1,T2,T).

%
max_terms(_,P1,P2,C1,C2,[term([P1],C)]) :-
	P1 == P2,!,
	max_factors(C1,C2,C).
max_terms(Arity,P1,P2,C1,C2,[term([P1],C1)]) :-
	P1 \== P2,
	larger_terms(3,Arity,P1,P2),
	larger_factors(C1,C2),!.
max_terms(Arity,P1,P2,C1,C2,[term([P2],C2)]) :-
	P1 \== P2,
	larger_terms(3,Arity,P2,P1),
	larger_factors(C2,C1),!.
max_terms(_,P1,P2,C1,C2,T) :-
	P1 \== P2,
	add_terms([term([P1],C1)],[term([P2],C2)],T).

%
larger_terms(I,Arity,_,_) :-
	I > Arity.
larger_terms(I,Arity,P1,P2) :-
	I =< Arity,
	arg(Arity,P1,Arg1),
	arg(Arity,P2,Arg2),
	normal_form(-1,Minus1),
	multiply_expr(Minus1,Arg2,MArg2),
	add_expr(Arg1,MArg2,Arg),
	positive_expr(Arg),
	Arity1 is Arity-1,
	larger_terms(I,Arity1,P1,P2).
	
%
larger_factors(F1,F2) :-
	normal_form(-1,Minus1),
	multiply_expr(Minus1,expr([],F2),E2),
	add_expr(expr([],F1),E2,E),
	positive_expr(E).
	
%
max_factors(F1,F2,F) :-
	add_factors(F1,F2,F).
%
%  normal_form.pl		Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for transforming general-form 
%  expressions into normal-form expressions, and performing algebraic
%  simplification.
%

%
%  Simplify a list of general-form expressions.
%
list_simplification([],[]).
list_simplification([X|Xs],[Y|Ys]) :-
	simplification(X,Y),
	list_simplification(Xs,Ys).

%
%  Simplify a general-form expression.
%
simplification(X,Y) :-
	normal_form(X,Z),
	general_form(Z,Y).

%
%  Transform a general-form expression into a normal-form expression.
%
normal_form(bot,bot).
normal_form(inf,inf).
normal_form(X,Y) :-
	number(X),
	number_normal_form(X,Y).
normal_form(X,Y) :-
	compound(X),
	comp_normal_form(X,Y).

%
%  Transform a general-form number into a normal-form number.
%
number_normal_form(X,expr([],[factor([],X)])) :-
	X =\= 0.
number_normal_form(X,expr([],[])) :-
	X =:= 0.

%
%  Transform a general-form compound expression into a normal-form compound 
%  expression.
%
comp_normal_form(X,Y) :-
	variable(X),
	var_normal_form(X,Y).
comp_normal_form(X+Y,Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	add_expr(X1,Y1,Z).
comp_normal_form(X-Y,Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	subtract_expr(X1,Y1,Z).
comp_normal_form(X*Y,Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	multiply_expr(X1,Y1,Z).
comp_normal_form(X/Y,Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	divide_expr(X1,Y1,Z).
comp_normal_form(-X,Y) :-
	Minus1 is -1,
	normal_form(Minus1,X1),
	normal_form(X,X2),
	multiply_expr(X1,X2,Y).
comp_normal_form(exp(X,Y),Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	exp_expr(X1,Y1,Z).
comp_normal_form(log(X,Y),Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	log_expr(X1,Y1,Z).
comp_normal_form(fact(X),Y) :-
	normal_form(X,X1),
	factorial_expr(X1,Y).
comp_normal_form(max(X,Y),Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	max_expr(X1,Y1,Z).
comp_normal_form(min(X,Y),Z) :-
	normal_form(X,X1),
	normal_form(Y,Y1),
	min_expr(X1,Y1,Z).
comp_normal_form(X,Y) :-
	functor(X,sum,4),
	sum_normal_form(X,Y).
comp_normal_form(X,Y) :-
	functor(X,prod,4),
	prod_normal_form(X,Y).
comp_normal_form(X,Y) :-
	functor(X,arg,2),
	function_expr(X,Y).
comp_normal_form(X,Y) :-
	functor(X,arity,1),
	function_expr(X,Y).
comp_normal_form(X,Y) :-
	functor(X,head,1),
	function_expr(X,Y).
comp_normal_form(X,Y) :-
	functor(X,tail,1),
	function_expr(X,Y).
comp_normal_form(X,Y) :-
	userfunc(X),
	userfunc_normal_form(X,Y).

%
%  Transform a general-form variable into a normal-form variable.
%
var_normal_form(X,expr([],[factor([X],1)])).

sum_normal_form(X,Y) :-
	functor(X,sum,4),
	arg(1,X,Index),
	arg(2,X,1),
	arg(3,X,arity(Var)),
	arg(4,X,arg(Var,Index)),!,
	normal_form(Var-1,Y).
sum_normal_form(X,Y) :-
	functor(X,sum,4),
	arg(1,X,Var),
	arg(2,X,Lower),
	normal_form(Lower,NLower),
	arg(3,X,Upper),
	normal_form(Upper,NUpper),
	arg(4,X,Expr),
	normal_form(Expr,NExpr),
	sum_expr(Var,NLower,NUpper,NExpr,Y).

prod_normal_form(X,Y) :-
	functor(X,prod,4),
	arg(1,X,Var),
	arg(2,X,Lower),
	normal_form(Lower,NLower),
	arg(3,X,Upper),
	normal_form(Upper,NUpper),
	arg(4,X,Expr),
	normal_form(Expr,NExpr),
	prod_expr(Var,NLower,NUpper,NExpr,Y).

%
%  Transform a general-form sum or prod function into a normal-form 
%  sum or product function.
%
function_expr(X,expr([],[factor([Y],1)])) :-
	functor(X,F,N),
	functor(Y,F,N),
	function_normal_form(N,X,Y).

function_normal_form(0,_,_).
function_normal_form(N,X,Y) :-
	N > 0,
	arg(N,X,Arg),
	normal_form(Arg,NArg),
	arg(N,Y,NArg),
	N1 is N-1,
	function_normal_form(N1,X,Y).

%
%  Transform a general-form user-defined function into a normal-form 
%  user-defined function.
%
userfunc_normal_form(X,expr([term([Y],[factor([],1)])],[])) :-
	functor(X,F,N),
	functor(Y,F,N),
	function_normal_form(N,X,Y).

%
%  Test if a term is a variable.
%
variable($(_)).
variable($(_,_)).


%
%  Test if a term is a user-defined function.
%
userfunc(X) :-
	functor(X,F,N),
	(F,N) \== ('$',1), (F,N) \== ('$',2), (F,N) \== ((+),2),
	(F,N) \== ((-),2), (F,N) \== ((*),2), (F,N) \== ((/),2),
	(F,N) \== ((-),1), (F,N) \== (exp,2), (F,N) \== (log,2),
	(F,N) \== (fact,1),(F,N) \== (max,2), (F,N) \== (min,2),
	(F,N) \== (arg,2), (F,N) \== (arity,1),(F,N) \== (head,1),
	(F,N) \== (tail,1),(F,N) \== (sum,4), (F,N) \== (prod,4).
%
%  Sumprod.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for performing sum and product 
%  operations.
%
%  Assume both lower and upper bounds are greater than or equal to 1.
%

%
%  Simplify the sum expression.
%
sum_expr(_,_,_,bot,bot).
sum_expr(_,_,bot,Expr,bot) :-
	Expr \== bot.
sum_expr(_,bot,Upper,Expr,bot) :-
	Expr \== bot,
	Upper \== bot.
sum_expr(_,Lower,Upper,inf,inf) :-
	Upper \== bot,
	Lower \== bot.
sum_expr(_,Lower,inf,Expr,inf) :-
	Expr \== bot,
	Expr \== inf,
	Lower \== bot.
sum_expr(_,inf,Upper,Expr,bot) :-
	Expr \== bot,
	Expr \== inf,
	Upper \== bot,
	Upper \== inf.
sum_expr(Var,Low,Upper,expr(T,F),Sum) :-
	sum_factors(Var,Low,Upper,F,FSum),
	sum_terms(Var,Low,Upper,T,TSum),
	add_expr(TSum,FSum,Sum).

%
%  Simplify the sum factors.
%
sum_factors(_,_,_,[],Sum) :-
	normal_form(0,Sum).
sum_factors(Var,Low,Upper,[F|Fs],Sum) :-
	sum_factor(Var,Low,Upper,F,Sum1),
	sum_factors(Var,Low,Upper,Fs,Sum2),
	add_expr(Sum1,Sum2,Sum).

sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	rel_const_items(I,Var),!,
	const_sum_factor(Low,Upper,Sum1),
	CExpr = expr([],[factor(I,C)]),
	multiply_expr(CExpr,Sum1,Sum).
sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	rel_linear_items(I,Var),!,
	linear_items_coeff(I,Var,Coeff),
	normal_form(1,One),
	poly_sum_factor(Var,Low,Upper,One,Sum1),
	CExpr = expr([],[factor(Coeff,C)]),
	multiply_expr(CExpr,Sum1,Sum).
sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	rel_poly_items(I,Var),!,
	poly_items_coeff(I,Var,Coeff,Exp),
	poly_sum_factor(Var,Low,Upper,Exp,Sum1),
	CExpr = expr([],[factor(Coeff,C)]),
	multiply_expr(CExpr,Sum1,Sum).
sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	rel_exp_items(I,Var,0),!,
	exp_items_coeff(I,Var,Coeff,Base,ExpCoe,ExpCon),
	exp_sum_factor(Low,Upper,Base,ExpCoe,ExpCon,Sum1),
	CExpr = expr([],[factor(Coeff,C)]),
	multiply_expr(CExpr,Sum1,Sum).
sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	rel_log_items(I,Var,0),!,
	log_items_coeff(I,Var,Coeff,Base,Exp),
	log_sum_factor(Var,Low,Upper,Base,Exp,Sum1),
	CExpr = expr([],[factor(Coeff,C)]),
	multiply_expr(CExpr,Sum1,Sum).
sum_factor(Var,Low,Upper,factor(I,C),Sum) :-
	normal_form(Var,Var1),
	Expr = expr([],[factor(I,C)]),
	Sum = expr([],[factor([sum(Var1,Low,Upper,Expr)],1)]).

%
%  Simplify the sum terms.
%
sum_terms(_,_,_,[],Sum) :-
	normal_form(0,Sum).
sum_terms(Var,Low,Upper,[T|Ts],Sum) :-
	sum_term(Var,Low,Upper,T,Sum1),
	sum_terms(Var,Low,Upper,Ts,Sum2),
	add_expr(Sum1,Sum2,Sum).

sum_term(Var,Low,Upper,T,Sum) :-
	normal_form(Var,Var1),
	Sum = expr([term([sum(Var1,Low,Upper,expr([T],[]))],[factor([],1)])],
	      []).

%
%  Simplify the constant sum factor.
%
const_sum_factor(Low,Upper,Sum) :-
	subtract_expr(Upper,Low,E1),
	normal_form(1,One),
	add_expr(E1,One,Sum).

%
%  Simplify the polynomial sum factor.
%
poly_sum_factor(Var,Lower,Upper,Order,Sum) :-
	general_form(Order,Order1),
	poly_indef_sum(Order1,Var,Upper,USum),
	(default_lower_bound(Lower) ->
		Sum = USum;
		(normal_form(1,One),
		 subtract_expr(Lower,One,Lower1),
		 poly_indef_sum(Order1,Var,Lower1,LSum),
		 subtract_expr(USum,LSum,Sum))).

%
%  Compute the polynomial indefinite sum.
%
poly_indef_sum(1,_,Upper,Sum) :-
	normal_form(1,One),
	add_expr(Upper,One,E1),
	multiply_expr(Upper,E1,E2),
	normal_form(2,Two),
	divide_expr(E2,Two,Sum).
poly_indef_sum(I,Var,Upper,Sum) :-
	I > 1,
	gen_subsum_expr(I,Var,E1),
	normal_form(exp(Var,I),E2),
	subtract_expr(E1,E2,Expr1),
	normal_form(1,One),
	sum_expr(Var,One,Upper,Expr1,Sum1),
	gen_subsum(I,Upper,Sum2),
	subtract_expr(Sum2,Sum1,Sum).

%
%  Generate the polynomial indefinite subsum expressions.
%
gen_subsum_expr(1,Var,Expr) :-
	normal_form(Var,Expr).
gen_subsum_expr(I,Var,Expr) :-
	I > 1,
	I1 is I-1,
	gen_subsum_expr(I1,Var,Expr1),
	normal_form(Var-I1,Expr2),
	multiply_expr(Expr1,Expr2,Expr).

%
%  Generate the polynomial indefinite subsum.
%
gen_subsum(I,Upper,Sum) :-
	gen_subsum_seq(I,Upper,Sum1),
	normal_form(I+1,Sum2),
	divide_expr(Sum1,Sum2,Sum).

gen_subsum_seq(0,Upper,Prod) :-
	normal_form(1,S1),
	add_expr(Upper,S1,Prod).
gen_subsum_seq(I,Upper,Prod) :-
	I > 0,
	I1 is I-1,
	normal_form(I1,S1),
	subtract_expr(Upper,S1,P1),
	gen_subsum_seq(I1,Upper,P2),
	multiply_expr(P1,P2,Prod).

%
%  Simplify an exponential sum factor.
%
exp_sum_factor(Lower,Upper,Base,ExpCoe,ExpCon,Sum) :-
	normal_form(1,One),
	add_expr(Upper,One,S1),
	multiply_expr(S1,ExpCoe,S2),
	exp_expr(Base,S2,E1),
	multiply_expr(Lower,ExpCoe,S3),
	exp_expr(Base,S3,E2),
	subtract_expr(E1,E2,E),
	exp_expr(Base,ExpCoe,S4),
	subtract_expr(S4,One,D),
	divide_expr(E,D,S5),
	exp_expr(Base,ExpCon,Coeff),
	multiply_expr(Coeff,S5,Sum).

%
%  Simplify a logarithm sum factor.
%
log_sum_factor(Var,Lower,Upper,Base,Exp,Sum) :-
	prod_expr(Var,Lower,Upper,Exp,Prod),
	log_expr(Base,Prod,Sum).


%
%  Simplify the product expression.
%
prod_expr(_,_,_,bot,bot).
prod_expr(_,_,bot,Expr,bot) :-
	Expr \== bot.
prod_expr(_,bot,Upper,Expr,bot) :-
	Expr \== bot,
	Upper \== bot.
prod_expr(_,Lower,Upper,inf,inf) :-
	Upper \== bot,
	Lower \== bot.
prod_expr(_,Lower,inf,Expr,inf) :-
	Expr \== bot,
	Expr \== inf,
	Lower \== bot.
prod_expr(_,inf,Upper,Expr,bot) :-
	Expr \== bot,
	Expr \== inf,
	Upper \== bot,
	Upper \== inf.
prod_expr(Var,Lower,Upper,Expr,Prod) :-
	Expr = expr([],[factor(I,C)]),!,
	prod_items(Var,Lower,Upper,I,Prod1),
	normal_form(C,C1),
	const_prod_expr(Lower,Upper,C1,Prod2),
	multiply_expr(Prod1,Prod2,Prod).
prod_expr(Var,Lower,Upper,Expr,Prod) :-
	Expr = expr([],F),
	rel_const_factor(F,Var),!,
	const_prod_expr(Lower,Upper,Expr,Prod).
prod_expr(Var,Lower,Upper,Expr,Prod) :-
	normal_form(Var,Var1),
	Prod = expr([],[factor([prod(Var1,Lower,Upper,Expr)],1)]).

%
%  Simplify the product items.
%
prod_items(_,_,_,[],Prod) :-
	normal_form(1,Prod).
prod_items(Var,Lower,Upper,[I|Is],Prod) :-
	prod_item(Var,Lower,Upper,I,Prod1),
	prod_items(Var,Lower,Upper,Is,Prod2),
	multiply_expr(Prod1,Prod2,Prod).

%
%  Simplify the product items.
%
prod_item(Var,Lower,Upper,Item,Prod) :-
	rel_const_item(Item,Var),!,
	Expr = expr([],[factor([Item],1)]),
	const_prod_expr(Lower,Upper,Expr,Prod).
prod_item(Var,Lower,Upper,Var,Prod) :-
	!,
	linear_prod_expr(Lower,Upper,Prod).
prod_item(Var,Lower,Upper,Item,Prod) :-
	rel_poly_item1(Item,Var),!,
	poly_items_coeff([Item],Var,_,Order),
	linear_prod_expr(Lower,Upper,P1),
	exp_expr(P1,Order,Prod).
prod_item(Var,Lower,Upper,Item,Prod) :-
	rel_exp_item1(Item,Var),!,
	Item = exp(E1,E2),
	sum_expr(Var,Lower,Upper,E2,Sum),
	exp_expr(E1,Sum,Prod).
prod_item(Var,Lower,Upper,Item,Prod) :-
	normal_form(Var,Var1),
	Expr = expr([],[factor([Item],1)]),
	Prod = expr([],[factor([prod(Var1,Lower,Upper,Expr)],1)]).

%
%  Simplify the constant product expression.
%
const_prod_expr(Low,Upper,Const,Prod) :-
	subtract_expr(Upper,Low,E1),
	normal_form(1,One),
	add_expr(E1,One,E2),
	exp_expr(Const,E2,Prod).

%
%  Simplify the linear product expression.
%
linear_prod_expr(Low,Upper,Prod) :-
	factorial_expr(Upper,E1),
	normal_form(1,One),
	subtract_expr(Low,One,E2),
	factorial_expr(E2,E3),
	divide_expr(E1,E3,Prod).

%
%  Test if an expression is constant w.r.t. a variable.
%
rel_const_expr(Var,expr([],F)) :-
	rel_const_factor(F,Var).

%
%  Test if a factor is constant w.r.t. a variable.
%
rel_const_factor([],_).
rel_const_factor([factor(I,_)|Fs],Var) :-
	rel_const_items(I,Var),
	rel_const_factor(Fs,Var).

rel_const_items([],_).
rel_const_items([I|Is],Var) :-
	rel_const_item(I,Var),
	rel_const_items(Is,Var).

%
%  Test if an item is constant w.r.t. a variable.
%
rel_const_item(Var1,Var) :-
	variable(Var1),
	Var1 \== Var.
rel_const_item(exp(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_const_item(log(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_const_item(fact(E),Var) :-
	rel_const_expr(Var,E).

%
%  Test if an expression is linear w.r.t. a variable.
%
rel_linear_expr(Var,expr([],F)) :-
	rel_linear_factor(F,Var).

%
%  Test if a factor is linear w.r.t. a variable.
%
rel_linear_factor([],_).
rel_linear_factor([factor(I,_)|F],Var) :-
	rel_linear_items(I,Var),
	rel_linear_factor(F,Var).

rel_linear_items([],_).
rel_linear_items([I|Is],Var) :-
	rel_linear_item(I,Var),
	rel_linear_items(Is,Var).

%
%  Test if an item is linear w.r.t. a variable.
%
rel_linear_item(Var1,Var) :-
	variable(Var1),
	Var1 \== Var.
rel_linear_item(exp(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_linear_item(log(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_linear_item(fact(E),Var) :-
	rel_const_expr(Var,E).
rel_linear_item(Var,Var).

%
%  Get the coefficient of a linear expression w.r.t. a variable.
%
linear_expr_coeff(Var,expr([],F),expr([],NCoeF),expr([],NConF)) :-
	linear_factor_coeff(F,Var,NCoeF,NConF).

linear_factor_coeff([],_,[],[]).
linear_factor_coeff([factor(I,C)|F],Var,NCoeF,NConF) :-
	linear_factor_coeff(F,Var,CoeF,ConF),
	linear_items_coeff(I,Var,Coeff),
	(I == Coeff ->
		(add_factor(ConF,factor(Coeff,C),NConF),
		 NCoeF = CoeF);
		(add_factor(CoeF,factor(Coeff,C),NCoeF),
		 NConF = ConF)).

linear_items_coeff([],_,[]).
linear_items_coeff([I|Is],Var,Coeff) :-
	(I == Var ->
		Coeff = NCoeff;
		Coeff = [I|NCoeff]),
	linear_items_coeff(Is,Var,NCoeff).

%
%  Test if an expression is polynomial w.r.t. a variable.
%
rel_poly_expr(Var,expr([],F)) :-
	rel_poly_factor(F,Var).

%
%  Test if a factor is polynomial w.r.t. a variable.
%
rel_poly_factor([factor(I,_)],Var) :-
	rel_poly_items(I,Var).

rel_poly_items([],_).
rel_poly_items([I|Is],Var) :-
	rel_poly_item(I,Var),
	rel_poly_items(Is,Var).

%
%  Test if an item is polynomial w.r.t. a variable.
%
rel_poly_item(Var1,Var) :-
	variable(Var1),
	Var1 \== Var.
rel_poly_item(exp(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_poly_item(log(E1,E2),Var) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_poly_item(fact(E),Var) :-
	rel_const_expr(Var,E).
rel_poly_item(exp(E1,E2),Var) :-
	normal_form(Var,E1),
	general_form(E2,E),
	integer(E).

rel_poly_item1(exp(Var1,E),Var) :-
	normal_form(Var,Var1),
	general_form(E,E1),
	integer(E1).

%
%  Get the coefficient and the order of a polynomial expression w.r.t. 
%  a variable.
%
poly_expr_coeff(Var,expr([],F),expr([],Coeff),Exp) :-
	poly_factor_coeff(F,Var,Coeff,Exp).

poly_factor_coeff([factor(I,C)],Var,[factor(Coeff,C)],Exp) :-
	poly_items_coeff(I,Var,Coeff,Exp).

poly_items_coeff([],_,[],_).
poly_items_coeff([I|Is],Var,Coeff,Exp) :-
	(I = exp(E1,E2),normal_form(Var,E1),general_form(E2,E),integer(E) ->
		(Coeff = NCoeff,
		 Exp = E2);
		(Coeff = [I|NCoeff],
		 Exp = Exp1)),
	poly_items_coeff(Is,Var,NCoeff,Exp1).

%
%  Test if an expression is exponential w.r.t. a variable.
%
rel_exp_expr(Var,expr([],F)) :-
	rel_exp_factor(F,Var).

%
%  Test if a factor is exponential w.r.t. a variable.
%
rel_exp_factor([factor(I,_)],Var) :-
	rel_exp_items(I,Var,0).

rel_exp_items([],_,_).
rel_exp_items([I|Is],Var,Flag) :-
	rel_exp_item(I,Var,Flag,NFlag),
	rel_exp_items(Is,Var,NFlag).

%
%  Test if an item is exponential w.r.t. a variable.
%
rel_exp_item(Var1,Var,Flag,Flag) :-
	variable(Var1),
	Var1 \== Var.
rel_exp_item(exp(E1,E2),Var,Flag,Flag) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_exp_item(log(E1,E2),Var,Flag,Flag) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_exp_item(fact(E),Var,Flag,Flag) :-
	rel_const_expr(Var,E).
rel_exp_item(exp(E1,E2),Var,0,1) :-
	rel_const_expr(Var,E1),
	rel_linear_expr(Var,E2).

rel_exp_item1(exp(E1,_),Var) :-
	rel_const_expr(Var,E1).

%
%  Get the coefficient, the base and the exponent of an exponential 
%  expression w.r.t. a variable.
%
exp_expr_coeff(Var,expr([],F),expr([],Coeff),Base,ExpCoe,ExpCon) :-
	exp_factor_coeff(F,Var,Coeff,Base,ExpCoe,ExpCon).

exp_factor_coeff([factor(I,C)],Var,[factor(Coeff,C)],Base,ExpCoe,ExpCon) :-
	exp_items_coeff(I,Var,Coeff,Base,ExpCoe,ExpCon).

exp_items_coeff([],_,[],_,_,_).
exp_items_coeff([I|Is],Var,Coeff,Base,ExpCoe,ExpCon) :-
	(I = exp(E1,E2),rel_const_expr(Var,E1),rel_linear_expr(Var,E2) ->
		(Base = E1,
		 linear_expr_coeff(Var,E2,ExpCoe,ExpCon),
		 Coeff = NCoeff);
		(Base = NBase,
		 ExpCoe = NExpCoe,
		 ExpCon = NExpCon,
		 Coeff = [I|NCoeff])),
	exp_items_coeff(Is,Var,NCoeff,NBase,NExpCoe,NExpCon).

%
%  Test if an expression is logarithmic w.r.t. a variable.
%
rel_log_expr(Var,expr([],F)) :-
	rel_log_factor(F,Var).

%
%  Test if a factor is logarithmic w.r.t. a variable.
%
rel_log_factor([factor(I,_)],Var) :-
	rel_log_items(I,Var,0).

rel_log_items([],_,_).
rel_log_items([I|Is],Var,Flag) :-
	rel_log_item(I,Var,Flag,NFlag),
	rel_log_items(Is,Var,NFlag).

%
%  Test if an item is logarithmic w.r.t. a variable.
%
rel_log_item(Var1,Var,Flag,Flag) :-
	variable(Var1),
	Var1 \== Var.
rel_log_item(exp(E1,E2),Var,Flag,Flag) :-
	rel_const_expr(Var,E1),
	rel_const_expr(Var,E2).
rel_log_item(fact(E),Var,Flag,Flag) :-
	rel_const_expr(Var,E).
rel_log_item(log(E1,_),Var,0,1) :-
	rel_const_expr(Var,E1).

%
%  Get the coefficient, the base and the exponent of a logarithmic 
%  expression w.r.t. a variable.
%
log_expr_coeff(Var,expr([],F),expr([],Coeff),Base,Exp) :-
	log_factor_coeff(F,Var,Coeff,Base,Exp).

log_factor_coeff([factor(I,C)],Var,[factor(Coeff,C)],Base,Exp) :-
	log_items_coeff(I,Var,Coeff,Base,Exp).

log_items_coeff([],_,[],_,_).
log_items_coeff([I|Is],Var,Coeff,Base,Exp) :-
	(I = log(E1,E2),rel_const_expr(Var,E1) ->
		(Base = E1,
		 Exp = E2,
		 Coeff = NCoeff);
		(Base = NBase,
		 Exp = NExp,
		 Coeff = [I|NCoeff])),
	log_items_coeff(Is,Var,NCoeff,NBase,NExp).

%
%  Test if the lower bound of a sum or product is 1.
%
default_lower_bound(Lower) :-
	general_form(Lower,Lower1),
	number(Lower1),
	Lower1 =:= 1.
%
%  diff_equ.pl			Nai-Wei Lin			February, 1992
%
%  This file contains the procedures for solving linear difference equations.
%

%
%  Solve a difference equation.
%
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	first_order_con_diff_equ(Equ,Var,Ivalue,An,Bn),!,
	solve_focde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	first_order_var_diff_equ(Equ,Var,Ivalue,An,Bn),!,
	solve_fovde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	second_order_con_diff_equ(Equ,Var,Ivalue,R1,R2,A1n,A2n,Bn),!,
	solve_socde(Var,Ivalue,R1,R2,A1n,A2n,Bn,Sol).
solve_diff_equ(Equ,Var,_,Ivalue,Sol) :-
	divide_conquer_diff_equ(Equ,Var,Ivalue,A,C,Bn),!,
	solve_dcde(Var,Ivalue,A,C,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
	mutual_size_diff_equ(Equ,Var,Pred,Ivalue,An,Bn),!,
	solve_msde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
%	write('go1'),nl,
	mutual_struct_diff_equ(Equ,Var,Pred,Ivalue,Bn),!,
%	write('go'),nl,
	solve_mstde(Var,Bn,Sol).
solve_diff_equ(Equ,Var,Pred,Ivalue,Sol) :-
%	write('go2'),nl,
	mutual_list_diff_equ(Equ,Var,Pred,Ivalue,Bn),!,
%	write('go'),nl,
	solve_mlsde(Var,Bn,Sol).
solve_diff_equ(_,_,_,_,inf).

%
%  Test if a difference equation is linear first-order.
%  f(n) = a(n) * f(n-1) + b(n).
%
first_order_diff_equ(Equ,Var,An,Bn) :-
	Equ = expr([term([FuncTerm],Coeff)],ConstTerm),
	userfunc(FuncTerm),
	functor(FuncTerm,_,1),
	arg(1,FuncTerm,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	I =:= -1,
	An = expr([],Coeff),
	Bn = expr([],ConstTerm).

%
%  Solve a linear first-order difference equation.
%
solve_fode(Var,BEqus,An,Bn,Sol) :-
	%%% Fred
	%first_order_boundary_condition(BEqus,BCond),
	(first_order_const_solvable(An,Bn,Var) ->
		solve_focde(Var,BCond,An,Bn,Sol);
		solve_fovde(Var,BCond,An,Bn,Sol)).
	
%
%  Test if a linear first-order difference equation is solvable using
%  constant coefficient solving methods.
%
first_order_const_solvable(An,Bn,Var) :-
	first_order_const_diff_equ(An),
	const_coeff_solvable_terms(Bn,Var).
%
%  Test if a linear first-order difference equation has constant coefficient.
%
first_order_const_diff_equ(An) :-
	An = expr([],Coeff),
	Coeff = [factor([],_)].

%
%  Test if the terms of a linear first-order difference equation 
%  with constant coefficient are solvable by using constant coefficient
%  solving methods.
%
const_coeff_solvable_terms(Bn,Var) :-
	Bn = expr([],Factor),
	const_coeff_solvable_factors(Factor,Var).

%
%  Test if the factors of an equation is solvable using (1-order or 2-order)
%  constant coefficient solving methods. These methods handle several common
%  patterns efficiently.
%
const_coeff_solvable_factors([],_).
const_coeff_solvable_factors([F|Fs],Var) :-
	const_coeff_solvable_factor(F,Var),
	const_coeff_solvable_factors(Fs,Var).

const_coeff_solvable_factor(Factor,Var) :- constant_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- linear_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- quadratic_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- constant_base_exp_factor(Factor,Var).

constant_factor(factor([],_),_).
linear_factor(factor([Var],_),Var).
quadratic_factor(factor([exp(E1,E2)],_),Var) :-
	normal_form(Var,E1),
	normal_form(2,E2).
constant_base_exp_factor(factor([exp(E1,E2)],_),Var) :-
	E1 = expr([],[factor([],_)]),
	normal_form(Var,E2).

%
%  Solve a linear first-order constant coefficient difference equation.
%
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	normal_form(1,A),!,
	solve_fovde(Var,Ivalue,A,Bn,Sol).
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	Bn = expr([],BN),
	first_order_par_sol(BN,Var,A,Sol1),
	first_order_gen_sol(Var,Ivalue,A,Sol1,Sol).

%
%
%
first_order_par_sol([],_,_,Sol) :-
	normal_form(0,Sol).
first_order_par_sol([F|Fs],Var,A,Sol) :-
	first_order_par_sol1(F,Var,A,Sol1),
	first_order_par_sol(Fs,Var,A,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
first_order_par_sol1(factor([],C),_,An,Sol) :-
	general_form(An,A),
	normal_form(C/(1-A),Sol).
first_order_par_sol1(factor([Var],C),Var,An,Sol) :-
	general_form(An,A),
	normal_form((C/(1-A))*Var-(A*C)/exp(1-A,2),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	general_form(An,A),
	normal_form((C/(1-A))*exp(Var,2)-((2*A*C)/exp(1-A,2))*Var+
		(exp(A,2)*C+A*C)/exp(1-A,3),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =\= D,
	normal_form((C*D/(D-A))*exp(D,Var),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =:= D,
	normal_form(C*Var*exp(D,Var),Sol).

%
%
first_order_gen_sol(Var,Ivalue,An,PSol,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	general_form(PSol,PSol1),
	general_form(Iindex,Iindex1),
	substitute(PSol1,Var,Iindex1,PSol2),
	normal_form(PSol2,PSol3),
	subtract_expr(Ival,PSol3,N),
	exp_expr(An,Iindex,D),
	divide_expr(N,D,C),
	normal_form(Var,Var1),
	exp_expr(An,Var1,G1),
	multiply_expr(C,G1,G2),
	add_expr(G2,PSol,Sol).

%
%  Solve a linear first-order variable coefficient difference equation.
%
solve_fovde(Var,Ivalue,An,Bn,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	substitute(An,Var,$(i),AN),
	substitute(Bn,Var,$(j),BN),
	normal_form(1,One),
	add_expr(Iindex,One,Lower1),
	normal_form(Var,Upper),
	prod_expr($(i),Lower1,Upper,AN,P1),
	multiply_expr(Ival,P1,S1),
	normal_form(($(j))+1,Lower2),
	prod_expr($(i),Lower2,Upper,AN,P2),
	multiply_expr(BN,P2,P3),
	sum_expr($(j),Lower1,Upper,P3,S2),
	add_expr(S1,S2,Sol).

%
%  Test if a difference equation is linear second-order with constant 
%  coefficient.
%
second_order_con_diff_equ(Equ,Var,Ivalue,R1,R2,A1,A2,Bn) :-
	second_order_diff_equ(Equ,Var,Ivalue,An1,An2,B),
	An1 = [factor([],A1)],
	An2 = [factor([],A2)],
	real_quadratic_roots(A1,A2,R1,R2),
	const_coeff_solvable(B,Var),
	Bn = expr([],B).
	
%
%  Test if a difference equation is linear second-order.
%
second_order_diff_equ(Equ,Var,Ivalue,A1,A2,Bn) :-
	Equ = expr([term([Dvar1],A1),term([Dvar2],A2)],Bn),!,
	userfunc(Dvar1),
	functor(Dvar1,F,1),
	arg(1,Dvar1,Arg1),
	Arg1 = expr([],[factor([Var],1),factor([],I1)]),
	First is -1,
	I1 =:= First,
	userfunc(Dvar2),
	functor(Dvar2,F,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	Second is -2,
	I2 =:= Second,
	Ivalue = [val(Iindex1,_),val(Iindex2,_)],
	general_form(Iindex1,Index1),
	integer(Index1),
	general_form(Iindex2,Index2),
	integer(Index2),
	(Index1 > Index2 ->
		D is Index1-Index2;
		D is Index2-Index1),
	D =:= 1.
second_order_diff_equ(Equ,Var,Ivalue,A1,A2,Bn) :-
	Equ = expr([term([Dvar2],A2)],Bn),
	userfunc(Dvar2),
	functor(Dvar2,_,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	Second is -2,
	I2 =:= Second,
	A1 = [factor([],0)],
	Ivalue = [val(Iindex1,_),val(Iindex2,_)],
	general_form(Iindex1,Index1),
	integer(Index1),
	general_form(Iindex2,Index2),
	integer(Index2),
	(Index1 > Index2 ->
		D is Index1-Index2;
		D is Index2-Index1),
	D =:= 1.

%
%  Solve a linear second-order constant coefficient difference equation.
%
solve_socde(Var,Ivalue,R1,R2,A1,A2,Bn,GSol) :-
	Bn = expr([],BN),
	second_order_par_sol(BN,Var,R1,R2,A1,A2,PSol),
	second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol).

%
%
second_order_par_sol([],_,_,_,_,_,Sol) :-
	normal_form(0,Sol).
second_order_par_sol([F|Fs],Var,R1,R2,A1,A2,Sol) :-
	second_order_par_sol1(F,Var,R1,R2,A1,A2,Sol1),
	second_order_par_sol(Fs,Var,R1,R2,A1,A2,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
second_order_par_sol1(factor([],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
		normal_form(C/(1-A1-A2),Sol);
		(U =:= 1 ->
			normal_form((C/(A1+2*A2))*Var,Sol);
			normal_form((C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([Var],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
/*
		(normal_form(C/(1-A1-A2),D1),
		 normal_form(-(A1+2*A2)/(1-A1-A2),D2),
		 multiply_expr(D1,D2,D0),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E1,D0,Sol));
*/
		normal_form((C/(1-A1-A2))*Var+
			    (-C*(A1+2*A2)/exp(1-A1-A2,2)),Sol);
		(U =:= 1 ->
			normal_form((C/(2*(A1+2*A2)))*exp(Var,2)+
			     ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*Var,Sol);
			normal_form((C/6)*exp(Var,3)+(C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	unit_count(R1,R2,U),
	(U =:= 0 ->
		(normal_form(C/(1-A1-A2),D2),
		 normal_form(-2*(A1+2*A2)/(1-A1-A2),TD1),
		 multiply_expr(TD1,D2,D1),
		 normal_form((A1+2*A2)/(1-A1-A2),TD01),
		 multiply_expr(TD01,D1,TD03),
		 normal_form((A1+4*A2)/(1-A1-A2),TD02),
		 multiply_expr(TD02,D2,TD04),
		 subtract_expr(TD04,TD03,D0),
		 normal_form(exp(Var,2),Var2),
		 multiply_expr(D2,Var2,E2),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E2,E1,Sol1),
		 add_expr(Sol1,D0,Sol));
		(U =:= 1 ->
			normal_form((C/(3*(A1+2*A2)))*exp(Var,3)+
			   ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*exp(Var,2)+
			   ((C*(A1*A1+4*A1*A2+16*A2*A2))/(6*exp(A1+2*A2,3)))*
				Var,Sol);
			normal_form((C/12)*exp(Var,4)+(C/3)*exp(Var,3)+
			   (5*C/12)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	R1 =\= D,
	R2 =\= D,
	normal_form(((C*exp(D,2))/(exp(D,2)-A1*D-A2))*exp(D,Var),Sol).

%
%
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =\= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = exp(R2,Index2)*exp(R1,Index1)-exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*exp(R2,Var)+GPSol,GSol).
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =:= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = Index2*exp(R2,Index2)*exp(R1,Index1)-
	     Index1*exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*Index1*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*Var*exp(R2,Var)+GPSol,GSol).

%
%
divide_conquer_diff_equ(Equ,Var,Ivalue,A,C,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,_,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],I)]),
	I < 1,
	C is 1/I,
	An = [factor([],A)],
	divide_conquer_solvable(Bn,Var),
	Ivalue = [val(Iindex,_)],
	normal_form(1,Iindex).

%
%
solve_dcde(Var,[val(_,Ival)],A,C,Bn,Sol) :-
	divide_conquer_par_sol(Bn,Var,A,C,PSol),
	normal_form(exp(Var,log(C,A)),E),
	multiply_expr(Ival,E,GSol),
	add_expr(GSol,PSol,Sol).

%
%
divide_conquer_par_sol([],_,_,_,Sol) :-
	normal_form(0,Sol).
divide_conquer_par_sol([factor(I,D)|F],Var,A,C,Sol) :-
	divide_conquer_order(I,Var,O),
	simplification(exp(C,O),X),
	divide_conquer_par_sol1(O,X,Var,A,C,Sol1),
	normal_form(D,D1),
	multiply_expr(D1,Sol1,Sol2),
	divide_conquer_par_sol(F,Var,A,C,Sols),
	add_expr(Sol2,Sols,Sol).

%
%
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =:= E,
	normal_form(exp(Var,O)*log(C,Var),Sol).
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =\= E,
	N = exp(Var,log(C,A))-exp(Var,O),
	D = A-exp(C,O),
	Exp = exp(C,O)*(N/D),
	normal_form(Exp,Sol).

%
%
divide_conquer_order([],_,0).
divide_conquer_order([Var],Var,1).
divide_conquer_order([exp(E1,E2)],Var,I) :-
	normal_form(Var,E1),
	general_form(E2,E),
	I is integer(E).

%
%  Test if the factors of an equation is solvable using (1-order or 2-order)
%  constant coefficient solving methods. These methods handle several common
%  patterns efficiently.
%
const_coeff_solvable_factors([],_).
const_coeff_solvable_factors([F|Fs],Var) :-
	const_coeff_solvable_factor(F,Var),
	const_coeff_solvable_factors(Fs,Var).

const_coeff_solvable_factor(Factor,Var) :- constant_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- linear_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- quadratic_factor(Factor,Var).
const_coeff_solvable_factor(Factor,Var) :- constant_base_exp_factor(Factor,Var).

constant_factor(factor([],_),_).
linear_factor(factor([Var],_),Var).
quadratic_factor(factor([exp(E1,E2)],_),Var) :-
	normal_form(Var,E1),
	normal_form(2,E2).
constant_base_exp_factor(factor([exp(E1,E2)],_),Var) :-
	E1 = expr([],[factor([],_)]),
	normal_form(Var,E2).
%
%
exp_only_expr([],_,_,_).
exp_only_expr([factor(I,_)|F],Var,R1,R2) :-
	exp_only_expr1(I,Var,R1,R2),
	exp_only_expr(F,Var,R1,R2).

exp_only_expr1([exp(E1,E2)],Var,R1,R2) :-
	E1 = expr([],[factor([],C)]),
	C =\= R1,
	C =\= R2,
	normal_form(Var,E2).

%
%
real_quadratic_roots(A1,A2,R1,R2) :-
	D is A1*A1+4*A2,
	(D >= 0 ->
		normal_form(exp(D,0.5),ND),
		general_form(ND,D1),
		R1 is (A1+D1)/2,
		R2 is (A1-D1)/2).

%
%
unit_count(R1,R2,I) :-
	unit_count(R1,I1),
	unit_count(R2,I2),
	I is I1+I2.

unit_count(R,1) :-
	R =:= 1.
unit_count(R,0) :-
	R =\= 1.

%
%
divide_conquer_solvable([],_).
divide_conquer_solvable([factor(I,_)|F],Var) :-
	divide_conquer_solvable1(I,Var),
	divide_conquer_solvable(F,Var).

divide_conquer_solvable1([],_).
divide_conquer_solvable1([Var],Var).
divide_conquer_solvable1([exp(E1,E2)],Var) :-
	normal_form(Var,E1),
	general_form(E2,I),
	I1 is integer(I),
	I =:= I1.

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_msde(Var,Ivalue,An,Bn,Sol) :-
	normal_form(1,A),
	solve_fovde(Var,Ivalue,A,Bn,Sol1),
	substitute(An,Var,$(i),AN),
	normal_form(Var,Upper),
	sum_expr($(i),A,Upper,AN,Sol2),
	add_expr(Sol1,Sol2,Sol).

%
%  Test if a mutual difference equation is linear first-order with coeff 1.
%
mutual_size_diff_equ(Equ,Var,Pred,Ivalue,A,B) :-
	Equ = expr(Terms,Bn),
	primary_term(Terms,Pred,Var,MTerm),
	mutual_size_solvable(Bn,Var),
	normal_form(0,Zero),
	Ivalue = [val(Zero,_)],
	A = expr([MTerm],[]),
	B = expr([],Bn).

%
%
primary_term([T1,T2],Pred,Var,T2) :-
	primary_term(T1,Pred,Var),
	nonprimary_term(T2).
primary_term([T1,T2],Pred,Var,T1) :-
	primary_term(T2,Pred,Var),
	nonprimary_term(T1).

primary_term(term([P],[factor([],1)]),Pred,Var) :-
	userfunc(P),
	functor(P,F,1),
	Pred = F/_,
	arg(1,P,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	First is -1,
	I =:= First.
nonprimary_term(term([P],[factor([],1)])) :-
	userfunc(P),
	functor(P,_,N),
	N =\= 1.

%
%
mutual_size_solvable([],_).
mutual_size_solvable([factor(I,_)|F],Var) :-
	const_coeff_solvable(I,Var),
	mutual_size_solvable(F,Var).
mutual_size_solvable([factor(I,_)|F],Var) :-
	I = [arg(E1,E2)],
	general_form(E1,Var1),
	variable(Var1),
	normal_form(Var,E2),
	mutual_size_solvable(F,Var).

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_mstde(Var,Bn,Sol) :-
	solve_mstde1(Bn,Var,Sol).

solve_mstde1([],_,Zero) :-
	normal_form(0,Zero).
solve_mstde1([B|Bn],Var,Sol) :-
	solve_mstde2(B,Var,Sol1),
	solve_mstde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_mstde2(factor([],C),Var,Sol) :-
	normal_form(C*Var,Sol).
solve_mstde2(factor([Var],C),Var,Sol) :-
	normal_form((C/2)*Var*(Var+1),Sol).
solve_mstde2(factor([arity(E)],C),Var,Sol) :-
	normal_form(Var,E),
	normal_form(C*(Var-1),Sol).

%
%  Test if a mutual size difference equation is linear first-order with coeff 1.
%
mutual_struct_diff_equ(Equ,Var,F/_,Ivalue,Bn) :-
	Equ = expr(Term,Bn),
	NTerm = expr(Term,[]),
	general_form(NTerm,GTerm),
	GTerm = sum(Index,1,arity(Var),Expr),
	functor(Expr,F,1),
	arg(1,Expr,arg(Var,Index)),
	normal_form(1,One),
	Ivalue = [val(One,Ival)],
	general_form(Ival,GIval),
	mutual_struct_solvable(Bn,Var,GIval).

%
%
mutual_struct_solvable([],_,_).
mutual_struct_solvable([B|Bn],Var,GIval) :-
	mutual_struct_solvable1(B,Var,GIval),
	mutual_struct_solvable(Bn,Var,GIval).

mutual_struct_solvable1(factor([],Val),_,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([Var],Val),Var,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([arity(Expr)],_),Var,_) :-
	normal_form(Var,Expr).

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_mlsde(Var,[],Sol) :-
	normal_form(Var,Sol).
solve_mlsde(Var,Bn,Sol) :-
	Bn \== [],
	solve_mlsde1(Bn,Var,Sol).

solve_mlsde1([],_,Zero) :-
	normal_form(0,Zero).
solve_mlsde1([B|Bn],Var,Sol) :-
	solve_mlsde2(B,Var,Sol1),
	solve_mlsde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_mlsde2(factor([],C),Var,Sol) :-
	normal_form(C*Var,Sol).
solve_mlsde2(factor([Var],C),Var,Sol) :-
	normal_form((C/2)*Var*(Var+1),Sol).
solve_mlsde2(factor([head(E)],C),Var,Sol) :-
	normal_form(Var,E),
	normal_form(C*exp(Var-1,2)/2,Sol).
solve_mlsde2(factor([tail(E)],C),Var,Sol) :-
	normal_form(Var,E),
	normal_form(C*exp(Var-1,2)/2,Sol).

%
%  Test if a mutual size difference equation is linear first-order with coeff 1.
%
mutual_list_diff_equ(Equ,Var,F/_,Ivalue,Bn) :-
	Equ = expr(Term,Bn),
	mutual_list_term(Term,Var,F),
	normal_form(1,One),
	Ivalue = [val(One,Ival)],
	general_form(Ival,GIval),
	mutual_list_solvable(Bn,Var,GIval).

mutual_list_term([Term1,Term2],Var,Pred) :-
	mutual_list_head(Term2,Var,Pred),
	mutual_list_tail(Term1,Var,Pred).

mutual_list_head(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P),
	functor(P,Pred,1),
	arg(1,P,Arg),
	general_form(Arg,head(Var)).

mutual_list_tail(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P),
	functor(P,Pred,1),
	arg(1,P,Arg),
	general_form(Arg,tail(Var)).

%
%
mutual_list_solvable([],_,_).
mutual_list_solvable([B|Bn],Var,GIval) :-
	mutual_list_solvable1(B,Var,GIval),
	mutual_list_solvable(Bn,Var,GIval).

mutual_list_solvable1(factor([],Val),_,GIval) :-
	Val >= GIval.
mutual_list_solvable1(factor([Var],Val),Var,GIval) :-
	Val >= GIval.
mutual_list_solvable1(factor([head(Expr)],_),Var,_) :-
	normal_form(Var,Expr).
mutual_list_solvable1(factor([tail(Expr)],_),Var,_) :-
	normal_form(Var,Expr).
%
%  csl_poly.pl			Nai-Wei Lin			November 1990
%
%  This file contains the procedures for estimating the chromatic polynomial
%  of a graph.
%

/*********************************************************************
  Given a graph GRAPH, csl_poly(GRAPH,UPOLY,LPOLY,EXACT) returns the 
  upper bound UPOLY and lower bound LPOLY of the chromatic polynomial 
  beas on complete-smallest-last order. EXACT is 1 if the solution is
  exact solution, 0 otherwise.
 *********************************************************************/
csl_poly(Graph,Upoly,Lpoly,Exact) :-
	csl_order(Graph,Order,Exact),
	chro_poly(Graph,Order,Upoly,Lpoly).
%	output(Upoly,Lpoly).
/*********************************************************************
  Given a graph GRAPH, csl_oredr(GRAPH,CSLGRAPH) returns a graph 
  CSLGRAPH in complete-smallest-last order. The status STATUS is 1 
  if the order is complete; 0 otherwise.
 *********************************************************************/
csl_order(Graph,Cslgraph,St) :-
	csf_order(Graph,Csfgraph,1,St),
	gcp_reverse(Csfgraph,Cslgraph).

/*********************************************************************
  Given a graph GRAPH, csf_oredr(GRAPH,CSLGRAPH) returns a graph 
  CSFGRAPH in complete-smallest-first order. The status STATUS is 1 
  if the order is complete; 0 otherwise.
 *********************************************************************/
csf_order([],[],St,St).
csf_order(Graph,[Node|Nodes],St,FinalSt) :-
	iqsort(Graph,SortGraph),
	csf_node(SortGraph,Node,NewGraph,CurSt),
	NewSt is St*CurSt,
	csf_order(NewGraph,Nodes,NewSt,FinalSt).

/*********************************************************************
  iqsort(GRAPH,SORTGRAPH) applies quick sort to the graph GRAPH based 
  on the degrees of nodes and returns a graph SORTGRAPH in increasing
  order.
 *********************************************************************/
iqsort([],[]).
iqsort([N|Graph],SortGraph) :-
	split(N,Graph,Small,Large),
	iqsort(Small,Sgraph),
	iqsort(Large,Lgraph),
	append(Sgraph,[N|Lgraph],SortGraph).

split(_,[],[],[]).
split(node(N,D,Node),[node(N1,D1,Node1)|Rest],[node(N1,D1,Node1)|S],L) :-
	D1 =< D,
	split(node(N,D,Node),Rest,S,L).
split(node(N,D,Node),[node(N1,D1,Node1)|Rest],S,[node(N1,D1,Node1)|L]) :-
	D1 > D,
	split(node(N,D,Node),Rest,S,L).

/*
append([],L,L).
append([H|L1],L2,[H|L3]) :-
	append(L1,L2,L3).
*/

/*********************************************************************
  Given a graph GRAPH sorted in increasing order of degrees of nodes,
  csf_node(GRAPH,NODE,NEWGRAPH,STATUS) finds the node NODE in GRAPH 
  that has smallest degree and its corresponding interfacing subgraph 
  is complete, and returns a graph NEWGRAPH by deleting NODE from GRAPH.
  The status STATUS is 1 if the interfacing subgraph is complete; 0
  otherwise.
 *********************************************************************/
csf_node(Graph,Node,NewGraph,St) :-
	csf_node(Graph,Graph,Node,NewGraph,St).

csf_node([],[node(Name,D,Nodes)|Rest],node(Name,D,Nodes),NewGraph,0) :-
	gcp_delete(Name,Rest,NewGraph).
csf_node([node(Name,D,Nodes)|_],Graph,node(Name,D,Nodes),NewGraph,1) :-
	complete(Nodes,Graph),!,	
	gcp_delete(Name,Graph,NewGraph).
csf_node([_|Rest],Graph,Node,NewGraph,St) :-
	csf_node(Rest,Graph,Node,NewGraph,St).

/*********************************************************************
  complete(NODES,GRAPH) checks if the induced subgraph by NODES is 
  complete in graph GRAPH.
 *********************************************************************/
complete(Nodes,Graph) :-
	complete(Nodes,Nodes,Graph).

complete([],_,_).
complete([N|Rest],Nodes,Graph) :-
	join(N,Nodes,Graph),
	complete(Rest,Nodes,Graph).

/*********************************************************************
  join(NODE,NODES,GRAPH) checks if the node NODE joins all the nodes 
  of NODES-{NODE} in graph GRAPH.
 *********************************************************************/
join(Node,Nodes,[node(Node,_,Adj)|_]) :-
	!,subset(Node,Nodes,Adj).
join(Node,Nodes,[_|Rest]) :-
	join(Node,Nodes,Rest).

/*********************************************************************
  subset(N,SET1,SET2) checks if the set SET1-{N} is a subset of the
  set SET2.
 *********************************************************************/
subset(_,[],_).
subset(N,[N|SET1],SET2) :-
	subset(N,SET1,SET2).
subset(N,[M|SET1],SET2) :-
	N \== M,
	gcp_member(M,SET2),
	subset(N,SET1,SET2).

/*********************************************************************
  gcp_member(N,SET) checks the membership of N in a set SET.
 *********************************************************************/
gcp_member(N,[N|_]) :- !.
gcp_member(N,[M|Rest]) :-
	N \== M,
	gcp_member(N,Rest).

/*********************************************************************
  gcp_delete(NAME,GRAPH,NEWGRAPH) deletes a node named NAME from a graph 
  GRAPH and returns the resultant graph NEWGRAPH.
 *********************************************************************/
gcp_delete(_,[],[]).
gcp_delete(Name,[node(Name,_,_)|Graph],NewGraph) :-
	gcp_delete(Name,Graph,NewGraph).
gcp_delete(Name,[node(N,D,Nodes)|Graph],[node(N,NewDeg,NewNodes)|NewGraph]) :-
	Name \== N,
	remove(Name,Nodes,D,NewNodes,NewDeg),
	gcp_delete(Name,Graph,NewGraph).

/*********************************************************************
  remove(N,LIST,LENGTH,NEWLIST,NEWLENGTH) removes an element N from 
  a list LIST with length LENGTH and returns the resultant list 
  NEWLIST and its new length NEWLENGTH. 
 *********************************************************************/
remove(_,[],D,[],D).
remove(Name,[Name|Nodes],D,Nodes,NewDeg) :-
	NewDeg is D-1,!.
remove(Name,[N|Nodes],D,[N|NewNodes],NewDeg) :-
	Name \== N,
	remove(Name,Nodes,D,NewNodes,NewDeg).

/*********************************************************************
  gcp_reverse(LIST,NEWLIST) returns the reversed list NEWLIST of a list 
  LIST.
 *********************************************************************/
gcp_reverse(L1,L2) :- gcp_reverse(L1,[],L2).

gcp_reverse([],L,L).
gcp_reverse([H|L1],L2,L3) :-
	gcp_reverse(L1,[H|L2],L3).

/*********************************************************************
  chro_poly(GRAPH,ORDER,POLY) computes the chromatic polynomial POLY
  of a graph GRAPH according to the order ORDER.
 *********************************************************************/
chro_poly(_,[],[],[]).
chro_poly(Graph,[node(_,D,Nodes)|Orders],[Upoly|Upolys],[k-D|Lpolys]) :-
	subgraph(Graph,Nodes,Sgraph),
	ubound(Sgraph,D,Upoly),
	chro_poly(Graph,Orders,Upolys,Lpolys).

/*********************************************************************
  Given a graph GRAPH, subgraph(GRAPH,NODES,SUBGRAPH) returns the
  subgraph SUBGRAPH of GRAPH induced by the set of vertices NODES.
 *********************************************************************/
subgraph([],_,[]).
subgraph([node(N,_,Nodes)|Graph],Adj,[node(N,Size,NewNodes)|Sgraph]) :-
	gcp_member(N,Adj), !,
	gcp_intersect(Nodes,Adj,NewNodes,Size),
	subgraph(Graph,Adj,Sgraph).
subgraph([_|Graph],Adj,Sgraph) :-
	subgraph(Graph,Adj,Sgraph).

/*********************************************************************
  gcp_intersect(SET1,SET2,NEWSET,SIZE) returns the intersection 
  NEWSET of SET1 and SET2, and its size SIZE.
 *********************************************************************/
gcp_intersect(Nodes,Adj,NewNodes,Size) :-
	gcp_intersect(Nodes,Adj,NewNodes,0,Size).

gcp_intersect([],_,[],D,D).
gcp_intersect([Node|Nodes],Adj,[Node|NewNodes],Size,Fsize) :-
	gcp_member(Node,Adj), !,
	Size1 is Size+1,
	gcp_intersect(Nodes,Adj,NewNodes,Size1,Fsize).
gcp_intersect([_|Nodes],Adj,NewNodes,Size,Fsize) :-
	gcp_intersect(Nodes,Adj,NewNodes,Size,Fsize).

/*********************************************************************
  ubound(GRAPH,ORDER,TERM) returns one term TERM of the chromatic 
  polynomial corresponding to the interfacing subgraph GRAPH of
  oredr ORDER.
 *********************************************************************/
ubound(Graph,N,k-Bound) :-
	lbound(Graph,N,Bound).

/*********************************************************************
  lbound(GRAPH,ORDER,BOUND) returns the lower bound of the chromatic 
  number of a graph GRAPH of order ORDER based on Bondy's Theorem.
 *********************************************************************/
lbound(Graph,N,Bound) :-
	dqsort(Graph,SortGraph),
	lbound(SortGraph,N,0,0,Bound).

lbound(_,N,_,N,N).
lbound(Graph,N,Delta,K,Bound) :-
	Index is Delta+1,
	getdeg(Graph,Index,Deg),
	NewDelta is N-Deg+Delta,
	K1 is K+1,
	(NewDelta >= N ->
	   (Bound = K1);
	   (lbound(Graph,N,NewDelta,K1,Bound))).

/*********************************************************************
  getdeg(GRAPH,I,DEG) returns the Ist largest degree DEG of a graph 
  GRAPH.
 *********************************************************************/
getdeg([node(_,D,_)|_],1,D).
getdeg([_|Graph],Index,Deg) :-
	Index > 1,
	NewIndex is Index-1,
	getdeg(Graph,NewIndex,Deg).

/*********************************************************************
  dqsort(GRAPH,SORTGRAPH) applies quick sort to the graph GRAPH based 
  on the degrees of nodes and returns a graph SORTGRAPH in decreasing
  order.
 *********************************************************************/
dqsort([],[]).
dqsort([H|Graph],SortGraph) :-
	split(H,Graph,Small,Large),
	dqsort(Small,Sgraph),
	dqsort(Large,Lgraph),
	append(Lgraph,[H|Sgraph],SortGraph).

/*
output([],[]).
output([k-N|U],[k-M|L]) :-
	write(N),tab(1),
	write(M), nl,
	output(U,L).
*/
%
%  disequality.pl		Nai-Wei Lin			April, 1992
%
%  This file contains the procedures for testing the binary disequality.
%

%
%  Test if a predicate is a GCP. If it is, also return the domain size.
%
binary_disequality(Pred,ST,Disequality,DomainSize) :-
%	write(Disequality),nl,
	find_symbol_field(ST,Pred,domain,Domain),
	nonvar(Domain),
	domain_sizes(Domain,DomainSize),
	disj_bi_disequality(Disequality).

%
disj_bi_disequality([]).
disj_bi_disequality([Disequality]) :-
	conj_bi_disequality(Disequality).

%
conj_bi_disequality([]).
conj_bi_disequality([D|Disequality]) :-
	(list(D) ->
		disj_bi_disequality(D);
		atom_bi_disequality(D)),
	conj_bi_disequality(Disequality).

%
atom_bi_disequality(true).
atom_bi_disequality(D) :-
	D \== true,
	functor(D,Op,A),
	(disequality(Op/A) ->
		(arg(1,D,LHS),
		 var(LHS),
		 arg(2,D,RHS),
		 var(RHS),
		 LHS \== RHS)).

%
%  Test if the domains for distinct variables are the same. If it does,
%  also return the size of the domain.
%
domain_sizes([D|Domain],DomainSize) :-
	domain_type(D,DomainType),
	domain_size(DomainType,D,CDomainSize),
	domain_sizes(Domain,DomainType,CDomainSize,DomainSize).

domain_sizes([],_,DomainSize,DomainSize).
domain_sizes([D|Domain],DomainType,CDomainSize,DomainSize) :-
	domain_type(D,DomainType),
	domain_size(DomainType,D,CDomainSize),
	domain_sizes(Domain,DomainType,CDomainSize,DomainSize).

%
%  Check the type of a domain.
%
domain_type(_-_,interval).
domain_type(D,set) :- list(D).

%
%  Compute the size of a domain according to its type.
%
domain_size(interval,L-U,Size) :-
	Size is U-L+1.
domain_size(set,S,Size) :-
	length(S,Size).

%
%  Test if a literal is a disequality.
%
disequality((=\=)/2).
disequality((\==)/2).
	
%
%  gcp.pl			Nai-Wei Lin			April 1992
%
%  This file contains the procedures for estimating the number of solutions
%  of a GCP.
%

%
%  Estimate the number of solutions of a GCP.
%
gcp(_,_,[],0).
gcp(Vars,DomainSize,Disequality,Sol) :-
	Disequality \== [],
	rename_variables_gcp(Vars,1,NVars,Disequality,NDisequality),
	build_constraint_graph(NVars,NDisequality,Graph),
	csl_poly(Graph,Upoly,_,_),
	evaluate_poly(Upoly,DomainSize,Sol).

%
%  Assign symbolic names to variables to facilitate further manipulation.
%
rename_variables_gcp([],_,[],Disequality,Disequality).
rename_variables_gcp([V|Vars],N,[N|NVars],Disequality,NDisequality) :-
	substitute(Disequality,V,N,Disequality1),
	N1 is N+1,
	rename_variables_gcp(Vars,N1,NVars,Disequality1,NDisequality).

%
%  Build a constraint graph.
%
build_constraint_graph([],_,[]).
build_constraint_graph([V|Vs],Disequality,[node(V,Order,Edge)|Gs]):-
	build_constraint_edges(Disequality,V,Order,Edge),
	build_constraint_graph(Vs,Disequality,Gs).

%
%  Build the edges corresponding to the variable V in a constraint graph.
%
build_constraint_edges(Disequality,V,Order,Edge) :-
	build_constraint_edges(Disequality,V,0,Order,[],Edge).

build_constraint_edges([],_,Order,Order,Edge,Edge).
build_constraint_edges([E|Es],V,Order,NOrder,Edge,NEdge):-
	(list(E) ->
		build_constraint_edges(E,V,Order,Order1,Edge,Edge1);
		build_constraint_edge(E,V,Order,Order1,Edge,Edge1)),
	build_constraint_edges(Es,V,Order1,NOrder,Edge1,NEdge).
		
build_constraint_edge(E,V,Order,NOrder,Edge,NEdge) :-
	(constraint_edge(E,V,W) ->
		(member(Edge,W) ->
			(NOrder = Order,
		 	 NEdge = Edge);
			(NOrder is Order+1,
			 NEdge = [W|Edge]));
		(NOrder = Order,
		 NEdge = Edge)).

%
%  Test if a disequality involve the variable V. If it does, also return the
%  other involved variable.
%
constraint_edge(U =\= W,V,W) :-
	U == V.
constraint_edge(W =\= U,V,W) :-
	U == V.
constraint_edge(U =\= W,V,_) :-
	U \== V,
	W \== V,
	fail.

%
%  Evaluate the value of a polynomial at a point.
%
evaluate_poly([],_,1).
evaluate_poly([Term|Poly],Point,Value) :-
	Term = k-Const,
	Term_value is Point-Const,
	evaluate_poly(Poly,Point,Values),
	(Term_value > 0 ->
		Value is Term_value*Values;
		Value = 0).
%
%  clique.pl			Nai-Wei Lin			April, 1992
%
%  This file contains the procedures for estimating the number of n-cliques
%  of a consistency graph.
%

%
%  Estimate the number of n-cliques in a weighted consistency graph.
%
number_n_cliques([_],Domain,_,Sols) :-
	!,length(Domain,Sols).
number_n_cliques([_,_],_,Graph,Sols) :-
	!,number_2_clique(Graph,Sols).
number_n_cliques(Vars,Domain,Graph,Sols) :-
	number_n_cliques1(Vars,Domain,Graph,Sols).

number_n_cliques1([V|Vars],[domain(V,D)|Domain],[cgraph(V,CV)|Graph],Sols) :-
	consistency_graph_reduction(D,CV,Graph,[],NGraph),
	number_n_cliques(Vars,Domain,NGraph,Sols).

number_n_cliques2([V|Vars],[domain(V,D)|Domain],[cgraph(V,CV)|Graph],Sols) :-
	consistency_graph_reduction2(D,CV,Vars,Domain,Graph,0,Sols).

%
%  Estimate the number of 2-cliques in a weighted bipartite graph.
%
number_2_clique([cgraph(_,Var)|_],Sols) :-
	number_2_clique2(Var,Sols).

number_2_clique2([cvar(_,Edge)|_],Sols) :-
	number_2_clique3(Edge,0,Sols).

number_2_clique3([],Sols,Sols).
number_2_clique3([cedge(_,E)|Edge],Sol,Sols) :-
	number_2_clique4(E,0,Sol1),
	Sol2 is Sol+Sol1,
	number_2_clique3(Edge,Sol2,Sols).

number_2_clique4([],Sols,Sols).
number_2_clique4([weight(_,W)|E],Sol,Sols) :-
	Sol1 is W+Sol,
	number_2_clique4(E,Sol1,Sols).

%
%  Perform one step of consistency graph reduction.
%
consistency_graph_reduction([],_,_,Graph,Graph).
consistency_graph_reduction([D|Domain],CV,Graph,IGraph,OGraph) :-
	adjacent_nodes(CV,D,Nodes),
	adjacent_graph(Nodes,Graph,Graph1),
	graph_addition(IGraph,Graph1,Graph2),
	consistency_graph_reduction(Domain,CV,Graph,Graph2,OGraph).

consistency_graph_reduction2([],_,_,_,_,Sols,Sols).
consistency_graph_reduction2([D|Dom],CV,Vars,Domain,Graph,Sol,Sols) :-
	write(D),tab(1),
	adjacent_nodes(CV,D,Nodes),
	adjacent_graph(Nodes,Graph,G1),
	number_n_cliques(Vars,Domain,G1,Sol1),
	write(Sol1),nl,
	Sol2 is Sol+Sol1,
	consistency_graph_reduction2(Dom,CV,Vars,Domain,Graph,Sol2,Sols).

%
%  Compute the adjacent nodes.
%
adjacent_nodes([],_,[]).
adjacent_nodes([cvar(_,Edge)|CV],D,[Node|Nodes]) :-
	adjacent_nodes1(Edge,D,Node),
	adjacent_nodes(CV,D,Nodes).

adjacent_nodes1([],_,[]) :- !.
adjacent_nodes1([cedge(V,E)|_],D,E) :-
	V =:= D, !.
adjacent_nodes1([cedge(V,_)|Edge],D,AEdge) :-
	V =\= D,
	adjacent_nodes1(Edge,D,AEdge).

%
%  Compute the adjacent graph.
%
adjacent_graph([_],G,G).
adjacent_graph([N|Nodes],[cgraph(V,G)|Graph],[cgraph(V,AG)|AGraph]) :-
	adjacent_edges(Nodes,N,G,AG),
	adjacent_graph(Nodes,Graph,AGraph).

%
%  Compute the adjacent edges.
%
adjacent_edges([],_,_,[]).
adjacent_edges([M|Nodes],N,[cvar(V,Edge)|G],[cvar(V,AEdge)|AG]) :-
	adjacent_edges1(N,M,Edge,AEdge),
	adjacent_edges(Nodes,N,G,AG).

adjacent_edges1([],_,_,[]) :- !.
adjacent_edges1(N,_,[],[]) :- N \== [], !.
adjacent_edges1([weight(V,W)|N],M,[cedge(U,E)|Edge],AEdge) :-
	V =:= U,!,
	adjacent_edges2(M,E,W,AE),
	(AE == [] ->
		AEdge = AEdges;
		AEdge = [cedge(U,AE)|AEdges]),
	adjacent_edges1(N,M,Edge,AEdges).
adjacent_edges1([weight(V,_)|N],M,[cedge(U,E)|Edge],AEdge) :-
	V < U,!,
	adjacent_edges1(N,M,[cedge(U,E)|Edge],AEdge).
adjacent_edges1([weight(V,W)|N],M,[cedge(U,_)|Edge],AEdge) :-
	V > U,
	adjacent_edges1([weight(V,W)|N],M,Edge,AEdge).

adjacent_edges2([],_,_,[]) :- !.
adjacent_edges2(M,[],_,[]) :- M \== [],!.
adjacent_edges2([weight(M1,W1)|M],[weight(E1,W2)|E],W,[weight(E1,W3)|AE]) :-
	M1 =:= E1,!,
	min(W1,W2,MW), min(MW,W,W3),
	adjacent_edges2(M,E,W,AE).
adjacent_edges2([weight(M1,_)|M],[weight(E1,W2)|E],W,AE) :-
	M1 < E1,!,
	adjacent_edges2(M,[weight(E1,W2)|E],W,AE).
adjacent_edges2([weight(M1,W1)|M],[weight(E1,_)|E],W,AE) :-
	M1 > E1,
	adjacent_edges2([weight(M1,W1)|M],E,W,AE).

%
%  Perform graph addition.
%
graph_addition([],G,G) :- !.
graph_addition(G,[],G) :- G \== [], !.
graph_addition([cgraph(V,V1)|G1],[cgraph(V,V2)|G2],[cgraph(V,V3)|G3]) :-
	var_addition(V1,V2,V3),
	graph_addition(G1,G2,G3).

%
%  Perform variable addition.
%
var_addition([],_,[]).
var_addition([cvar(V,E1)|V1],[cvar(V,E2)|V2],[cvar(V,E3)|V3]) :-
	edges_addition(E1,E2,E3),
	var_addition(V1,V2,V3).

%
%  Perform edge addition.
%
edges_addition([],E,E) :- !.
edges_addition(E,[],E) :- E \== [],!.
edges_addition([cedge(V1,E1)|C1],[cedge(V2,E2)|C2],[cedge(V1,E3)|C3]) :-
	V1 =:= V2,!,
	edge_addition(E1,E2,E3),
	edges_addition(C1,C2,C3).
edges_addition([cedge(V1,E1)|C1],[cedge(V2,E2)|C2],[cedge(V1,E1)|C3]) :-
	V1 < V2,!,
	edges_addition(C1,[cedge(V2,E2)|C2],C3).
edges_addition([cedge(V1,E1)|C1],[cedge(V2,E2)|C2],[cedge(V2,E2)|C3]) :-
	V1 > V2,
	edges_addition([cedge(V1,E1)|C1],C2,C3).

edge_addition([],E,E) :- !.
edge_addition(E,[],E) :- E \== [], !.
edge_addition([weight(V1,W1)|E1],[weight(V2,W2)|E2],[weight(V1,W3)|E3]) :-
	V1 =:= V2,!,
	W3 is W1+W2,
	edge_addition(E1,E2,E3).
edge_addition([weight(V1,W1)|E1],[weight(V2,W2)|E2],[weight(V1,W1)|E3]) :-
	V1 < V2,!,
	edge_addition(E1,[weight(V2,W2)|E2],E3).
edge_addition([weight(V1,W1)|E1],[weight(V2,W2)|E2],[weight(V2,W2)|E3]) :-
	V1 > V2,
	edge_addition([weight(V1,W1)|E1],E2,E3).
%
%  consistency.pl		Nai-Wei Lin			March 1992
%
%  This file contains the procedures for building the consistency graph
%  corresponding to a CSP.
%

%
%  Build a consistency graph.
%
build_consistency_graph([],_,_,_,[]).
build_consistency_graph([V|Vs],Interval,Domain,Constraint,[cgraph(V,Edge)|Gs]):-
	%nl,write(V),nl,
	%ttyflush,
	build_consistency_edges(Vs,V,Interval,Domain,Constraint,Edge),
	%write(Edge),nl,
	build_consistency_graph(Vs,Interval,Domain,Constraint,Gs).

%
%  Build the edges corresponding to the variable H in a consistency graph.
%
build_consistency_edges([],_,_,_,_,[]).
build_consistency_edges([T|Vs],H,Interval,Domain,Constraint,[cvar(T,Edge)|Es]):-
	%write(T),nl,
	build_consistency_edge(T,H,Interval,Domain,Constraint,Edge),
	build_consistency_edges(Vs,H,Interval,Domain,Constraint,Es).

%
%  Build the edges corresponding to the variables H and T in a consistency 
%  graph.
%
build_consistency_edge(T,H,Interval,Domain,Constraint,Edge) :-
	%relevant_bi_constraints(Constraint,T,H,RConstraint),
	%write(RConstraint),nl,
	canonical_binary_constraints(Constraint,T,H,Interval,BConstraint),
	%write(BConstraint),nl,
	find_domain_entry(Domain,H,domain(_,HDomain)),
	%write(HDomain),nl,
	find_domain_entry(Domain,T,domain(_,TDomain)),
	%write(TDomain),nl,
	consistency_edges(HDomain,TDomain,BConstraint,H,Edge).

%
%  Collect the set of constraints involving both variables H and T.
%
relevant_bi_constraints(Constraint,T,H,RConstraint) :-
	or_bi_constraints(Constraint,T,H,RConstraint).

or_bi_constraints([],_,_,[]).
or_bi_constraints([C|Constraint],T,H,RConst) :-
	and_bi_constraints(C,T,H,RC),
	(RC == [] ->
		RConst = RConstraint;
		RConst = [RC|RConstraint]),
	or_bi_constraints(Constraint,T,H,RConstraint).

and_bi_constraints([],_,_,[]).
and_bi_constraints([C|Constraint],T,H,RConst) :-
	(list(C) ->
		or_bi_constraints(C,T,H,RC);
		atom_bi_constraints(C,T,H,RC)),
	(RC == [] ->
		RConst = RConstraint;
		RConst = [RC|RConstraint]),
	and_bi_constraints(Constraint,T,H,RConstraint).

atom_bi_constraints(C,T,H,RC) :-
	(both_involved(C,T,H) ->
		RC = C;
		RC = []).

%
%  Test if constraint C involves both variables H and T.
%
both_involved(C,T,H) :-
	both_involved(C,T,H,TF,HF),
	F is TF+HF,
	F =:= 0.

both_involved(C,_,_,1,1) :-
	number(C),!.
both_involved(C,T,_,0,1) :-
	C == T,!.
both_involved(C,_,H,1,0) :-
	C == H,!.
both_involved(C,T,H,1,1) :-
	variable(C),!,
	C \== T,
	C \== H.
both_involved(C,T,H,TF,HF) :-
	compound(C),
	functor(C,F,N),
	F \== ($),
	both_involved(N,C,T,H,TF,HF).

both_involved(0,_,_,_,1,1).
both_involved(N,C,T,H,TF,HF) :-
	N > 0,
	arg(N,C,Arg),
	both_involved(Arg,T,H,TF1,HF1),
	N1 is N-1,
	both_involved(N1,C,T,H,TFs,HFs),
	TF is TF1*TFs,
	HF is HF1*HFs.

%
%  Transform general constraints into canonical binary constraints.
%
canonical_binary_constraints(Constraint,T,H,Domain,BConstraint) :-
	or_binary_constraints(Constraint,T,H,Domain,BConstraint).

or_binary_constraints([],_,_,_,[]).
or_binary_constraints([C|Constraint],T,H,Domain,[BC|BConstraint]) :-
	and_binary_constraints(C,T,H,Domain,BC),
/*
	(BC == [] ->
		BConst = BConstraint;
		BConst = [BC|BConstraint]),
*/
	or_binary_constraints(Constraint,T,H,Domain,BConstraint).

and_binary_constraints([],_,_,_,[]).
and_binary_constraints([C|Constraint],T,H,Domain,[BC|BConstraint]) :-
	(list(C) ->
		or_binary_constraints(C,T,H,Domain,BC);
		atom_binary_constraint(C,T,H,Domain,BC)),
/*
	(BC == [] ->
		BConst = BConstraint;
		BConst = [BC|BConstraint]),
*/
	and_binary_constraints(Constraint,T,H,Domain,BConstraint).

%
%  Transform a general constraints into a canonical binary constraint.
%
atom_binary_constraint(C,T,H,Domain,BC) :-
	(both_involved(C,T,H) ->
		(functor(C,Op,2),
		 arg(1,C,LHS),
		 binary_constraint(LHS,T,H,Domain,T1,H1,dv(C1,C2)),
		 simplification(T1,Texpr),
		 substitute(Texpr,T,1,NTexpr),
		 simplification(NTexpr,A),
		 Con1 is -C2,
		 Con2 is -C1,
		 simplification(-H1,Hexpr),
		 BC = bc(Op,A,Hexpr,dv(Con1,Con2)));
		BC = []).

%
binary_constraint(C,_,_,_,0,0,dv(C,C)) :- number(C),!.
binary_constraint(T,T,_,_,T,0,dv(0,0)) :- !.
binary_constraint(H,_,H,_,0,H,dv(0,0)) :- !.
binary_constraint(V,T,H,Domain,0,0,dv(L,U)) :- variable(V),!,
	V \== T, V \== H,
	find_interval_entry(Domain,V,domain(V,L,U)).
binary_constraint(-E1,T,H,Domain,Texpr,Hexpr,dv(NC1,NC2)) :- !,
	binary_constraint(E1,T,H,Domain,T1,H1,dv(C1,C2)),
	minus(T1,Texpr), minus(H1,Hexpr),
	NC1 is -C2, NC2 is -C1.
binary_constraint(E1+E2,T,H,Domain,Texpr,Hexpr,dv(C1,C2)) :- !,
	binary_constraint(E1,T,H,Domain,T1,H1,dv(C11,C12)),
	binary_constraint(E2,T,H,Domain,T2,H2,dv(C21,C22)),
	addition(T1,T2,Texpr), addition(H1,H2,Hexpr),
	C1 is C11+C21, C2 is C12+C22.
binary_constraint(E1-E2,T,H,Domain,Texpr,Hexpr,dv(C1,C2)) :- !,
	binary_constraint(E1,T,H,Domain,T1,H1,dv(C11,C12)),
	binary_constraint(E2,T,H,Domain,T2,H2,dv(C21,C22)),
	subtraction(T1,T2,Texpr), subtraction(H1,H2,Hexpr),
	C1 is C11-C22, C2 is C12-C21.
binary_constraint(E1*E2,T,H,Domain,Texpr,Hexpr,dv(C1,C2)) :-
	binary_constraint(E1,T,H,Domain,T1,H1,dv(C11,C12)),
	binary_constraint(E2,T,H,Domain,T2,H2,dv(C21,C22)),
	constraint_term(T1,H1,C11,C12,T2,H2,C21,C22,Texpr,Hexpr,C1,C2).

%
constraint_term(0,0,C,C,T2,H2,C21,C22,Texpr,Hexpr,C1,C2) :- !,
	multiply(C,T2,Texpr), multiply(C,H2,Hexpr),
	C1 is C*C21, C2 is C*C22.
constraint_term(T1,H1,C11,C12,0,0,C,C,Texpr,Hexpr,C1,C2) :-
	multiply(C,T1,Texpr), multiply(C,H1,Hexpr),
	C1 is C*C11, C2 is C*C12.

%
%  Collect the edges corresponding to the variables H and T.
%
consistency_edges([],_,_,_,[]).
consistency_edges([V|HInterval],TInterval,BConstraint,H,[cedge(V,WEdge)|Es]) :-
	consistent_values(BConstraint,H,V,TInterval,Edge),
	weighted_edge(Edge,WEdge),
	consistency_edges(HInterval,TInterval,BConstraint,H,Es).

%
%  Generate the consistent interval corresponding to a set of constraints.
%
consistent_values(BCs,H,V,Interval,Edge) :-
	or_consistent_values(BCs,H,V,Interval,Edge).

or_consistent_values([],_,_,Edge,Edge).
or_consistent_values(BCs,H,V,Interval,Edge) :-
	BCs \== [],
	or_consistent_value(BCs,H,V,Interval,Edge).

/*
or_consistent_value(BCs,H,V,Interval,Edge) :-
	or_consistent_value_1(BCs,H,V,Interval,Edge1),
	number_difference(Interval,Edge1,Edge).
*/

or_consistent_value([],_,_,_,[]).
or_consistent_value([C|Cs],H,V,Interval,Edge) :-
	and_consistent_values(C,H,V,Interval,Edge1),
	%number_difference(Interval,Edge1,Edge2),
	or_consistent_value(Cs,H,V,Interval,Edge2),
	number_union(Edge1,Edge2,Edge).

and_consistent_values([],_,_,Edge,Edge).
and_consistent_values([C|Cs],H,V,Interval,Edge) :-
	(list(C) ->
		or_consistent_values(C,H,V,Interval,Edge1);
		atom_consistent_values(C,H,V,Interval,Edge1)),
	and_consistent_values(Cs,H,V,Edge1,Edge).

atom_consistent_values(bc(Op,A,Hexpr,dv(Con1,Con2)),H,V,DInterval,Edge) :-
	substitute(Hexpr,H,V,Nexpr),
	simplification(Nexpr,Val),
	ELB is Con1+Val,
	EUB is Con2+Val,
	gen_interval(ELB,EUB,EInterval1),
	divide_all(EInterval1,A,EInterval2),
	(A > 0 ->
		(EInterval = EInterval2,
		 Nop = Op);
		(reverse(EInterval2,[],EInterval),
		 reverse_op(Op,Nop))),
	consistency(Nop,DInterval,EInterval,Edge).

%
%  Extend consistency graph to weighted consistency graph.
%
weighted_edge([],[]).
weighted_edge([E|Edge],[weight(E,1)|WEdge]) :-
	weighted_edge(Edge,WEdge).

%
%  Generate the set of consistent edges between the interval LB-UB.
%
gen_interval(LB,UB,[]) :-
	LB > UB.
gen_interval(LB,UB,[LB|Es]) :-
	LB =< UB,
	LB1 is LB+1,
	gen_interval(LB1,UB,Es).

%
%  Generate the consistent interval corresponding to a constraint.
%
consistency((=:=),DEdge,EEdge,Edge) :-
	number_intersection(DEdge,EEdge,Edge).
consistency((=\=),DEdge,EEdge,Edge) :-
	(EEdge = [E] ->
		number_delete(DEdge,E,Edge);
		Edge = DEdge).
consistency((>=),DEdge,EEdge,Edge) :-
	smallest_element(EEdge,SE),
	geq_set(DEdge,SE,Edge).
consistency((>),DEdge,EEdge,Edge) :-
	smallest_element(EEdge,SE),
	g_set(DEdge,SE,Edge).
consistency((=<),DEdge,EEdge,Edge) :-
	largest_element(EEdge,LE),
	leq_set(DEdge,LE,Edge).
consistency((<),DEdge,EEdge,Edge) :-
	largest_element(EEdge,LE),
	l_set(DEdge,LE,Edge).

%
number_member([E|_],Ele) :- 
	E =:= Ele.
number_member([E|List],Ele) :- 
	E =\= Ele,
	number_member(List,Ele).

%
number_union(S,[],S).
number_union([],S,S) :-
	S \== [].
number_union([X1|S1],[X2|S2],[X1|S]) :-
	X1 =:= X2,
	number_union(S1,S2,S).
number_union([X1|S1],[X2|S2],[X1|S]) :-
	X1 < X2,
	number_union(S1,[X2|S2],S).
number_union([X1|S1],[X2|S2],[X2|S]) :-
	X1 > X2,
	number_union([X1|S1],S2,S).

%
number_intersection([],_,[]).
number_intersection([X|S1],S2,S) :-
	(number_member(S2,X) ->
		S = [X|SList];
		S = SList),
	number_intersection(S1,S2,SList).

number_difference([],_,[]).
number_difference([X|S1],S2,S) :-
	(number_member(S2,X) ->
		S = SList;
		S = [X|SList]),
	number_difference(S1,S2,SList).

%
number_delete([],_,[]).
number_delete([D|DEdge],E,Edge) :-
	(D =:= E ->
		Edge = Edges;
		Edge = [D|Edges]),
	number_delete(DEdge,E,Edges).

%
divide_all([],_,[]).
divide_all([E|EEdge],A,[D|DEdge]) :-
	D is E/A,
	divide_all(EEdge,A,DEdge).

%
reverse([],L,L).
reverse([X|L1],L2,L3) :-
	reverse(L1,[X|L2],L3).

%
reverse_op((=:=),(=:=)).
reverse_op((=\=),(=\=)).
reverse_op((>=),(=<)).
reverse_op((>),(<)).
reverse_op((=<),(>=)).
reverse_op((<),(>)).

%
complement_constraint(bc(Op1,X1,X2,X3),bc(Op2,X1,X2,X3)) :-
	complement_op(Op1,Op2).

%
complement_op((=:=),(=\=)).
complement_op((=\=),(=:=)).
complement_op((>=),(<)).
complement_op((>),(=<)).
complement_op((=<),(>)).
complement_op((<),(>=)).

%
smallest_element([E|_],E).

%
largest_element([E],E).
largest_element([_|List],L) :-
	largest_element(List,L).

%
geq_set([],_,[]).
geq_set([D|Ds],X,E) :-
	(D >= X ->
		E = [D|Es];
		E = Es),
	geq_set(Ds,X,Es).

g_set([],_,[]).
g_set([D|Ds],X,E) :-
	(D > X ->
		E = [D|Es];
		E = Es),
	g_set(Ds,X,Es).

leq_set([],_,[]).
leq_set([D|Ds],X,E) :-
	(D =< X ->
		E = [D|Es];
		E = Es),
	leq_set(Ds,X,Es).

l_set([],_,[]).
l_set([D|Ds],X,E) :-
	(D < X ->
		E = [D|Es];
		E = Es),
	l_set(Ds,X,Es).

%
%  constraint.pl		Nai-Wei Lin			April, 1992
%
%  This file contains the procedures for testing the linearity of arithmetic
%  constraints.
%

%
%  Test if a predicate consists of linear arithmetic constraints.
%
linear_arithmetic_constraints(Pred,ST,Constraints) :-
	find_symbol_field(ST,Pred,measure,Measure),
	integer_constraint(Measure),
	find_symbol_field(ST,Pred,domain,Domain),
	nonvar(Domain),
	interval_domain(Domain),
	linear_arithmetic_constraints(Constraints).

linear_arithmetic_constraints([]).
linear_arithmetic_constraints([C|Constraints]) :-
	(list(C) ->
		linear_arithmetic_constraints(C);
		linear_arithmetic_constraint(C)),
	linear_arithmetic_constraints(Constraints).

linear_arithmetic_constraint(true).
linear_arithmetic_constraint(C) :-
	C \== true,
	functor(C,Op,A),
	(arithmetic_op(Op/A) ->
		(arg(1,C,LHS),
		 constraint_expr(LHS),
		 arg(2,C,RHS),
		 constraint_expr(RHS))).

%
%  Check if a predicate involves only integer.
%
integer_constraint([]).
integer_constraint([int|Measure]) :-
	integer_constraint(Measure).

%
%  Check if all the domains are integer interval.
%
interval_domain([]).
interval_domain([_-_|Domain]) :-
	interval_domain(Domain).

%
%  Test if a literal is an arithmetic operator.
%
arithmetic_op((=:=)/2).
arithmetic_op((=\=)/2).
arithmetic_op((>=)/2).
arithmetic_op((>)/2).
arithmetic_op((=<)/2).
arithmetic_op((<)/2).
	
%
%  Test if an expression is linear.
%
constraint_expr(E) :-
	number(E),!.
constraint_expr(E) :-
	var(E),!.
constraint_expr(-E) :-
	!,constraint_term(E).
constraint_expr(E1+E2) :-
	!,
	constraint_expr(E1),
	constraint_expr(E2).
constraint_expr(E1-E2) :-
	!,
	constraint_expr(E1),
	constraint_expr(E2).
constraint_expr(E1*E2) :-
	(number(E1);number(E2)),!,
	constraint_factor(E1),
	constraint_factor(E2).

%
%  Test if a term is linear.
%
constraint_term(E) :-
	number(E),!.
constraint_term(E) :-
	var(E),!.
constraint_term(E1*E2) :-
	(nonvar(E1);nonvar(E2)),!,
	constraint_factor(E1),
	constraint_factor(E2).

%
%  Test if a factor is linear.
%
constraint_factor(E) :-
	number(E).
constraint_factor(E) :-
	var(E).

%
%  csp.pl			Nai-Wei Lin			March 1992
%
%  This file contains the procedures for estimating the number of solutions
%  of a CSP.
%

%
%  Estimate the number of solutions of a CSP.
%
csp(_,_,[],0).
csp(Vars,Domain,Constraint,Sols) :-
	%write(Constraint),nl,
	Constraint \== [],
	%write('rename_variables'),nl,
	rename_variables(Vars,1,NVars,Domain,NDomain,Constraint,NConstraint),
	%write(NConstraint),nl,
	%write('simplify_constraints'),nl,
	simplify_constraints(NConstraint,SConstraint),
	%write(SConstraint),nl,
	%write('node_consistency'),nl,
	node_consistency(NVars,NDomain,RDomain,SConstraint,RConstraint),
	%write(RConstraint),nl,
	%write('build_consistency_graph'),nl,
	build_consistency_graph(NVars,NDomain,RDomain,RConstraint,Graph),
	%write(Graph),nl,
	%write('number_of_cliques'),nl,
	number_n_cliques(NVars,RDomain,Graph,Sols).

%
%  Assign symbolic names to variables to facilitate further manipulation.
%
rename_variables([],_,[],_,[],Constraint,Constraint).
rename_variables([V|Vars],N,[$(N)|NVars],[L-U|Domain],[ND|NDomain],Constraint,
		NConstraint) :-
	ND = domain($(N),L,U),
	substitute(Constraint,V,$(N),Constraint1),
	N1 is N+1,
	rename_variables(Vars,N1,NVars,Domain,NDomain,Constraint1,NConstraint).

%
%  Simplify the constraints.
%
simplify_constraints([],[]).
simplify_constraints([C|Constraint],[SC|SConstraint]) :-
	(list(C) ->
		simplify_constraints(C,SC);
		simplify_constraint(C,SC)),
	simplify_constraints(Constraint,SConstraint).

simplify_constraint(true,[]).
simplify_constraint(C,SC) :-
	C \== true,
	functor(C,Op,2),
	arg(1,C,LHS), arg(2,C,RHS),
	simplification(LHS-RHS,SExpr),
	functor(SC,Op,2),
	arg(1,SC,SExpr), arg(2,SC,0).

%
%  Reduce the domain size by performing node consistency via unary constraints.
%
node_consistency([],_,[],Constraint,Constraint).
node_consistency([V|Vars],Domain,[domain(V,NInterval)|NDomain],Constraint,
		NConstraint) :-
	relevant_uni_constraints(Constraint,V,RConstraint,Constraint1),
	find_interval_entry(Domain,V,domain(_,L,U)),
	gen_interval(L,U,Interval),
	consistent_node(RConstraint,Interval,NInterval),
	node_consistency(Vars,Domain,NDomain,Constraint1,NConstraint).

%
%  Collect the set of unary constraints involing variable V and transform
%  them into canonical form.
%
relevant_uni_constraints(Constraint,V,RConstraint,NConstraint) :-
	or_uni_constraints(Constraint,V,RConstraint,NConstraint).

or_uni_constraints([],_,[],[]).
or_uni_constraints([C|Constraints],V,[RC|RCs],[NC|NCs]) :-
	and_uni_constraints(C,V,RC,NC),
/*
	(RC == [] ->
		RConst = RCs;
		RConst = [RC|RCs]),
	(NC == [] ->
		NConst = NCs;
		NConst = [NC|NCs]),
*/
	or_uni_constraints(Constraints,V,RCs,NCs).

and_uni_constraints([],_,[],[]).
and_uni_constraints([C|Constraints],V,[RC|RCs],[NC|NCs]) :-
	(list(C) ->
		or_uni_constraints(C,V,RC,NC);
		atom_uni_constraints(C,V,RC,NC)),
/*
	(RC == [] ->
		RConst = RCs;
		RConst = [RC|RCs]),
	(NC == [] ->
		NConst = NCs;
		NConst = [NC|NCs]),
*/
	and_uni_constraints(Constraints,V,RCs,NCs).

atom_uni_constraints(C,V,RCs,NCs) :-
	functor(C,Op,_), arg(1,C,LHS),
	(unary_constraint(LHS,V,Term,Const) ->
		(RCs = uc(Op,Term,Const),
		 NCs = []);
		(RCs = [],
		 NCs = C)).

%
%  Transform unary constraint into canonical form.
%
unary_constraint(V,V,1,0).
unary_constraint(C,_,0,C) :- number(C).
unary_constraint(-C,V,NTerm,NConst) :-
	unary_constraint(C,V,Term,Const),
	minus(Term,NTerm),
	minus(Const,NConst).
unary_constraint(C1+C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	addition(Term1,Term2,Term),
	addition(Const1,Const2,Const).
unary_constraint(C1-C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	subtraction(Term1,Term2,Term),
	subtraction(Const1,Const2,Const).
unary_constraint(C1*C2,V,Term,Const) :-
	unary_constraint(C1,V,Term1,Const1),
	unary_constraint(C2,V,Term2,Const2),
	multiply(Term1,Const2,M1),
	multiply(Term2,Const1,M2),
	addition(M1,M2,Term),
	multiply(Const1,Const2,Const).
unary_constraint(C,V) :-
	variable(C), C \== V, fail.

%
%  Reduce the domain size according to the set of unary constraints.
%
consistent_node(Constraint,Interval,NInterval) :-
	or_consistent_nodes(Constraint,Interval,NInterval).

or_consistent_nodes([],Interval,Interval).
or_consistent_nodes(Constraint,Interval,NInterval) :-
	Constraint \== [],
	or_consistent_node(Constraint,Interval,NInterval).

or_consistent_node([],_,[]).
or_consistent_node([C|Constraint],Interval,NInterval) :-
	and_consistent_node(C,Interval,Interval1),
	or_consistent_node(Constraint,Interval,Interval2),
	number_union(Interval1,Interval2,NInterval).

and_consistent_node([],Interval,Interval).
and_consistent_node([C|Constraint],Interval,NInterval) :-
	(list(C) ->
		or_consistent_nodes(C,Interval,Interval1);
		atom_consistent_node(C,Interval,Interval1)),
	and_consistent_node(Constraint,Interval1,NInterval).

atom_consistent_node(uc(Op,Term,Const),Interval,NInterval) :-
	Threshold is -(Const/Term),
	(Term > 0 ->
		Nop = Op;
		reverse_op(Op,Nop)),
	consistency(Nop,Interval,[Threshold],NInterval).

%
%  Find the defined interval for varibale Var.
%
find_interval_entry(Dom,_,_) :- 
	var(Dom).
find_interval_entry([],_,_).
find_interval_entry(Dom,Var,Entry) :- 
	nonvar(Dom),
	Dom = [Entry|_],
	Entry = domain(V,_,_),
	V == Var.
find_interval_entry(Dom,Var,Entry) :- 
	nonvar(Dom),
	Dom = [domain(V,_,_)|D],
	V \== Var,
	find_interval_entry(D,Var,Entry).

%
%  Find the defined domain for varibale Var.
%
find_domain_entry(Dom,_,_) :- 
	var(Dom).
find_domain_entry([],_,_).
find_domain_entry(Dom,Var,Entry) :- 
	nonvar(Dom),
	Dom = [Entry|_],
	Entry = domain(V,_),
	V == Var.
find_domain_entry(Dom,Var,Entry) :- 
	nonvar(Dom),
	Dom = [domain(V,_)|D],
	V \== Var,
	find_domain_entry(D,Var,Entry).

%
%  unfold.pl			Nai-Wei Lin			April 1992
%
%  This file contains the procedures for unfolding a user-defined test 
%  predicate into buildin test predicates.
%

%
%  Unfolding a test predicate into buildin predicates by using top-down
%  and left-right order.
%
unfoldable(Pred,BT,ST,HVars,UClauses) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	test_predicate(Mode),
	formal_predicate(Pred,ST,HVars),
	find_symbol_field(ST,Pred,clause,Clauses),
	disj_unfoldable(Clauses,HVars,BT,ST,UClauses).

%
disj_unfoldable(Clauses,_,_,_,[]) :-
	var(Clauses).
disj_unfoldable(Clauses,HVars,BT,ST,UClauses) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	formal_clause(C,CVars,RC),
	conj_unfoldable(RC,BT,ST,UC),
	(UC == [] ->
		UClauses = UCs;
		(rename_term(CVars,HVars,UC,NC),
		 UClauses = [NC|UCs])),
	disj_unfoldable(Cs,HVars,BT,ST,UCs).

%
conj_unfoldable((_:-Body),BT,ST,UC) :-
	unfold_body(Body,BT,ST,UC).

%
unfold_literal(Pred,BT,ST,Call,UClauses) :-
	find_symbol_entry(BT,Pred,Entry),
	(nonvar(Entry) ->
		(unfold_admissible_builtin(Pred),
		 UClauses = Call);
		(find_symbol_field(ST,Pred,(mode),Mode),
		 test_predicate(Mode),
		 find_symbol_field(ST,Pred,clause,Clauses),
		 unfold_clauses(Clauses,Call,BT,ST,UClauses))).

%
unfold_clauses(Clauses,_,_,_,[]) :-
	var(Clauses).
unfold_clauses(Clauses,Call,BT,ST,UClauses) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	new_term(C,RC),
	term_variables(Call,CVar),
	new_variables(CVar,NVar),
	rename_term(CVar,NVar,Call,NCall),
	unfold_clause(RC,NCall,CVar,NVar,BT,ST,UC),
	unfold_clauses(Cs,Call,BT,ST,UCs),
	(UC == [] ->
		UClauses = UCs;
		UClauses = [UC|UCs]).

%
unfold_clause(Clause,Call,CVar,NVar,BT,ST,UClause) :-
	clause_type(Clause,Type),
	unfold_clause(Type,Clause,Call,CVar,NVar,BT,ST,UClause).

unfold_clause(2,(Head:-Body),Call,CVar,NVar,BT,ST,UClause) :-
	((Head:-Body) = (Call:-B) ->
		(rename_term(NVar,CVar,B,NB),
		 unfold_body(NB,BT,ST,UClause));
		UClause = []).
unfold_clause(3,Fact,Call,_,_,_,_,UClause) :-
	(Fact = Call ->
		UClause = [true];
		UClause = []).

%
unfold_body(Lit,BT,ST,UClauses) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	unfold_literal(F/A,BT,ST,Lit,UClause),
	(UClause == [] -> UClauses = [];	/* literal unification fails */
		((F/A == fail/0) -> UClauses = [];
			((F/A == (is)/2; F/A == !/0; F/A == true/0) ->
				UClauses = [true];
				UClauses = [UClause]))).
unfold_body((Lit,Body),BT,ST,UClauses) :-
	functor(Lit,F,A),
	unfold_literal(F/A,BT,ST,Lit,UC),
	(UC == [] -> UClauses = [];	/* literal unification fails */
		(F/A == fail/0 -> UClauses = [];/* literal unification fails */
			(F/A == (is)/2 ->
				(arg(1,UC,LHS),	/* materialize is/2 literal */
		 		 arg(2,UC,RHS),
		 		 evaluate_general_expr(RHS,SRHS),
		 		 substitute(Body,LHS,SRHS,SBody),
		 		 unfold_body(SBody,BT,ST,UClause),
		 		 UClauses = UClause);
				((F/A == (!)/0; F/A == true/0) ->
				   (unfold_body(Body,BT,ST,UClause),
		 	 	    UClauses = UClause);
				   (unfold_body(Body,BT,ST,UClause),
				    (UClause == [] ->
					UClauses = []; /* rest fails */
		 	 	 	UClauses = [UC|UClause])))))).

%
%  Check if a predicate is a test.
%
test_predicate(Mode) :-
	Mode \== [],
	test_predicate1(Mode).

test_predicate1([]).
test_predicate1([+|Mode]) :-
	test_predicate1(Mode).

%
%  Create a new instance of the first clause and collect the variables
%  in the head as the formal parameters of the predicate.
%
formal_predicate(Pred,ST,HVars) :-
	find_symbol_field(ST,Pred,clause,Clauses),
	nonvar(Clauses), Clauses = [C|_],
	clause_type(C,2),
	new_term(C,RC),
	arg(1,RC,Head),
	constraint_head_vars(Head,HVars).

%
formal_clause(C,RVars,RC) :-
	clause_type(C,2),
	new_term(C,RC),
	arg(1,RC,Head),
	constraint_head_vars(Head,RVars).

%
new_term(C,RC) :-
	term_variables(C,Vars),
	new_variables(Vars,NewVars),
	rename_term(Vars,NewVars,C,RC).

%
/*RB
term_variables(Term,Var) :-
	term_variables(Term,[],Var).

term_variables(Term,Var,[Term|Var]) :-
	var(Term).
term_variables(Term,Var,NVar) :-
	nonvar(Term),
	functor(Term,_,A),
	term_variables(A,Term,Var,NVar).
	
term_variables(0,_,Var,Var).
term_variables(A,Term,Var,NVar) :-
	A > 0,
	arg(A,Term,Arg),
	term_variables(Arg,Var,Var1),
	A1 is A-1,
	term_variables(A1,Term,Var1,NVar).
*/

%
new_variables([],[]).
new_variables([_|Var],[_|NVar]) :-
	new_variables(Var,NVar).

%
rename_term([],_,Term,Term).
rename_term([V|Var],[NV|NVar],Term,NTerm) :-
	substitute(Term,V,NV,Term1),
	rename_term(Var,NVar,Term1,NTerm).

%
%  Collect the set of variables in the head of a constraint.
%
constraint_head_vars(Head,HVars) :-
	functor(Head,_,A),
	constraint_head_vars(1,A,Head,HVars).
	
constraint_head_vars(N,A,_,[]) :-
	N > A.
constraint_head_vars(N,A,Head,[Arg|HVars]) :-
	N =< A,
	arg(N,Head,Arg),
	var(Arg),
	N1 is N+1,
	constraint_head_vars(N1,A,Head,HVars).

%
%  The set of builtin predicates allowed during unfolding.
%
unfold_admissible_builtin((==)/2).
unfold_admissible_builtin((\==)/2).
unfold_admissible_builtin((=:=)/2).
unfold_admissible_builtin((=\=)/2).
unfold_admissible_builtin((>=)/2).
unfold_admissible_builtin((>)/2).
unfold_admissible_builtin((=<)/2).
unfold_admissible_builtin((<)/2).
unfold_admissible_builtin((is)/2).
unfold_admissible_builtin((!)/0).
unfold_admissible_builtin((true)/0).
unfold_admissible_builtin((fail)/0).

%
evaluate_general_expr(RHS,RHS) :-
	var(RHS),!.
evaluate_general_expr(RHS,RHS) :-
	number(RHS),!.
evaluate_general_expr(-E1,RHS) :-
	!,
	evaluate_general_expr(E1,RHS1),
	(number(RHS1) ->
		RHS is -RHS1;
		RHS = -RHS1).
evaluate_general_expr(E1+E2,RHS) :-
	!,
	evaluate_general_expr(E1,RHS1),
	evaluate_general_expr(E2,RHS2),
	((number(RHS1),number(RHS2)) ->
		RHS is RHS1+RHS2;
		RHS = RHS1+RHS2).
evaluate_general_expr(E1-E2,RHS) :-
	!,
	evaluate_general_expr(E1,RHS1),
	evaluate_general_expr(E2,RHS2),
	((number(RHS1),number(RHS2)) ->
		RHS is RHS1-RHS2;
		RHS = RHS1-RHS2).
evaluate_general_expr(E1*E2,RHS) :-
	!,
	evaluate_general_expr(E1,RHS1),
	evaluate_general_expr(E2,RHS2),
	((number(RHS1),number(RHS2)) ->
		RHS is RHS1*RHS2;
		RHS = RHS1*RHS2).
evaluate_general_expr(RHS,SRHS) :-
	functor(RHS,F,N),
	functor(SRHS,F,N),
	evaluate_general_expr(N,RHS,SRHS).
	
evaluate_general_expr(0,_,_).
evaluate_general_expr(N,RHS,SRHS) :-
	N > 0,
	arg(N,RHS,Arg),
	evaluate_general_expr(Arg,SArg),
	arg(N,SRHS,SArg),
	N1 is N-1,
	evaluate_general_expr(N1,RHS,SRHS).
%
%  adg.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for manipulating the argument
%  dependency graph.
%
%  The structure of the argument dependency graph:
%	adg(Pos,pred,succ,mode)
%

%
%  Insert an entry for argument position Pos in the argument dependency graph.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Pos is inserted and returned.
%
insert_adg_entry(Adg,Pos,Entry) :- 
	var(Adg),
	Entry = adg(Pos,_,_,_),
	Adg = [Entry|_].
insert_adg_entry(Adg,Pos,Entry) :- 
	nonvar(Adg),
	Adg = [Entry|_],
	Entry = adg(Pos,_,_,_).
insert_adg_entry(Adg,Pos,Entry) :- 
	nonvar(Adg),
	Adg = [adg(P,_,_,_)|A],
	P \== Pos,
	insert_adg_entry(A,Pos,Entry).

%
%  Insert an edge for argument position Pos into the argument dependency
%  graph.
%
insert_adg_field(Adg,Pos,pred,Edge) :-
	insert_adg_entry(Adg,Pos,adg(Pos,EdgeList,_,_)),
	insert_adg_edge(EdgeList,Edge).
insert_adg_field(Adg,Pos,succ,Edge) :-
	insert_adg_entry(Adg,Pos,adg(Pos,_,EdgeList,_)),
	insert_adg_edge(EdgeList,Edge).
insert_adg_field(Adg,Pos,(mode),Mode) :-
	insert_adg_entry(Adg,Pos,adg(Pos,_,_,Mode)).

insert_adg_edge(EdgeList,Edge) :-
	var(EdgeList),
	EdgeList = [Edge|_].
insert_adg_edge(EdgeList,Edge) :-
	nonvar(EdgeList),
	EdgeList = [Edge|_].
insert_adg_edge(EdgeList,Edge) :-
	nonvar(EdgeList),
	EdgeList = [E|EList],
	E \== Edge,
	insert_adg_edge(EList,Edge).

%
%  Find the entry for argument position Pos in the argument dependency graph.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_adg_entry(Adg,_,_) :- 
	var(Adg).
find_adg_entry(Adg,Pos,Entry) :- 
	nonvar(Adg),
	Adg = [Entry|_],
	Entry = adg(Pos,_,_,_).
find_adg_entry(Adg,Pos,Entry) :- 
	nonvar(Adg),
	Adg = [adg(P,_,_,_)|A],
	P \== Pos,
	find_adg_entry(A,Pos,Entry).

%
%  Find a field for argument position Pos in the argument dependency graph.
%
find_adg_field(Adg,Pos,pred,EdgeList) :-
	find_adg_entry(Adg,Pos,adg(Pos,EdgeList,_,_)).
find_adg_field(Adg,Pos,succ,EdgeList) :-
	find_adg_entry(Adg,Pos,adg(Pos,_,EdgeList,_)).
find_adg_field(Adg,Pos,(mode),Mode) :-
	find_adg_entry(Adg,Pos,adg(Pos,_,_,Mode)).

%
%  Print out the argument dependency graph.
%
print_adg(Adg) :-
	tell(adg),
	p_adg(Adg),
	told.

p_adg([]).
p_adg([E|Adg]) :-
	write(E),
	nl,
	p_adg(Adg).
%
%  build_adg.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for building argument dependency graphs.
%
%  We assume the Prolog execution order and ground bindings
%  in current implementation.
%

%
%  Build the argument dependency graph of a clause.
%
argument_dependency_graph(2,Clause,BT,ST,Adg,Gvars,Error) :-
	Clause = (Head :- Body),
	functor(Head,F,N),
	find_symbol_field(ST,F/N,(mode),Mode),
	adg_head_input(1,N,Head,Mode,Adg,Gvars),
	adg_body(1,Body,BT,ST,Adg,Gvars,Clause,Error1),
	(fail_body(Body) ->
		Error2 = 1;
		adg_head_output(1,N,Head,Mode,Adg,Gvars,Clause,Error2)),
	Error is Error1*Error2,
	close_gvars_list(Gvars).
argument_dependency_graph(3,Fact,_,ST,Adg,Gvars,Error) :-
	functor(Fact,F,N),
	find_symbol_field(ST,F/N,(mode),Mode),
	adg_head_input(1,N,Fact,Mode,Adg,Gvars),
	adg_head_output(1,N,Fact,Mode,Adg,Gvars,Fact,Error),
	close_gvars_list(Gvars).

%
%  Insert the input positions of the head into the argument dependency
%  graph, and insert the variables in input positions into the ground
%  variables list.
%
adg_head_input(M,N,_,_,_,_) :-
	M > N.
adg_head_input(M,N,Head,[(+)|Mode],Adg,Gvars) :-
	M =< N,
	new_pos(0,M,Pos),
	insert_adg_entry(Adg,Pos,_),
	insert_adg_field(Adg,Pos,(mode),+),
	arg(M,Head,Arg),
	insert_ground_vars(Gvars,Arg,Pos),
	M1 is M+1,
	adg_head_input(M1,N,Head,Mode,Adg,Gvars).
adg_head_input(M,N,Head,[(-)|Mode],Adg,Gvars) :-
	M =< N,
	M1 is M+1,
	adg_head_input(M1,N,Head,Mode,Adg,Gvars).

%
%  Insert the variables in an argument into the ground variables list.
%
insert_ground_vars(Gvars,Arg,Pos) :-
	var(Arg),
	insert_gvars_field(Gvars,Arg,def,Pos).
insert_ground_vars(_,Arg,_) :-
	atomic(Arg).
insert_ground_vars(Gvars,Arg,Pos) :-
	compound(Arg),
	functor(Arg,_,N),
	insert_ground_vars(1,N,Gvars,Arg,Pos).

insert_ground_vars(M,N,_,_,_) :-
	M > N.
insert_ground_vars(M,N,Gvars,Arg,Pos)  :-
	M =< N,
	arg(M,Arg,Arg1),
	insert_ground_vars(Gvars,Arg1,Pos),
	M1 is M+1,
	insert_ground_vars(M1,N,Gvars,Arg,Pos).

%
%  Insert the output positions of the head and the edges to their 
%  predecessors into the argument dependency graph. 
%
adg_head_output(M,N,_,_,_,_,_,1) :-
	M > N.
adg_head_output(M,N,Head,[(+)|Mode],Adg,Gvars,Clause,Error) :-
	M =< N,
	M1 is M+1,
	adg_head_output(M1,N,Head,Mode,Adg,Gvars,Clause,Error).
adg_head_output(M,N,Head,[(-)|Mode],Adg,Gvars,Clause,Error) :-
	M =< N,
	new_pos(0,M,Pos),
	insert_adg_entry(Adg,Pos,_),
	insert_adg_field(Adg,Pos,(mode),-),
	arg(M,Head,Arg),
	find_adg_predecessor(Adg,Gvars,Arg,Pos,Clause,Error1),
	M1 is M+1,
	adg_head_output(M1,N,Head,Mode,Adg,Gvars,Clause,Error2),
	Error is Error1*Error2.

%
%  Find the predecessors of an argument.
%
find_adg_predecessor(Adg,Gvars,Arg,Pos,Clause,Error) :-
	var(Arg),
	find_gvars_field(Gvars,Arg,def,PosList),
	(var(PosList) ->
		(error_message(bound1,Arg,Clause),
		 Error = 0);
		(insert_gvars_field(Gvars,Arg,use,Pos),
		 insert_adg_predecessor(Adg,Pos,PosList),
		 Error = 1)).
find_adg_predecessor(_,_,Arg,_,_,1) :-
	atomic(Arg).
find_adg_predecessor(Adg,Gvars,Arg,Pos,Clause,Error) :-
	compound(Arg),
	functor(Arg,_,N),
	find_adg_predecessor(1,N,Adg,Gvars,Arg,Pos,Clause,Error).

find_adg_predecessor(M,N,_,_,_,_,_,1) :-
	M > N.
find_adg_predecessor(M,N,Adg,Gvars,Arg,Pos,Clause,Error)  :-
	M =< N,
	arg(M,Arg,Arg1),
	find_adg_predecessor(Adg,Gvars,Arg1,Pos,Clause,Error1),
	M1 is M+1,
	find_adg_predecessor(M1,N,Adg,Gvars,Arg,Pos,Clause,Error2),
	Error is Error1*Error2.

%
%  Insert the edges to the predecessors of an argument into 
%  the argument dependency graph. 
%
insert_adg_predecessor(_,_,PosList) :-
	var(PosList).
insert_adg_predecessor(Adg,Pos,PosList) :-
	nonvar(PosList),
	PosList = [P|PList],
	insert_adg_field(Adg,Pos,pred,P),
	insert_adg_field(Adg,P,succ,Pos),
	insert_adg_predecessor(Adg,Pos,PList).

%
%  Insert information about the body into the argument dependency
%  graph and the ground variables list.
%
adg_body(LitNum,(Lit,Body),BT,ST,Adg,Gvars,Clause,Error) :-
	adg_literal(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error1),
	(Error1 =:= 1 ->
		(LitNum1 is LitNum+1,
		 adg_body(LitNum1,Body,BT,ST,Adg,Gvars,Clause,Error2),
		 Error = Error2);
		Error = Error1).
adg_body(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error) :-
	nonsequence(Lit),
	adg_literal(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error).

%
%  Insert information about a literal into the argument dependency
%  graph and the ground variables list.
%
adg_literal(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error) :-
	functor(Lit,F,A),
	(second_order_predicate(F/A) ->
		adg_literal_1(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error);
		adg_literal_2(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error)).

adg_literal_1(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error) :-
	functor(Lit,findall,3),
	second_order_predicate_pred_arg(Lit,L),
	(legal_pred_arg(L) ->
		(arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num),
		 adg_literal(Num,L,BT,ST,Adg,Gvars,Clause,Error),
		 new_pos(LitNum,3,Pos),		% add 3rd position into Adg
		 insert_adg_entry(Adg,Pos,_),	% and ignore 1st and 2nd pos.
		 insert_adg_field(Adg,Pos,(mode),-),
		 arg(3,Lit,Arg),
		 insert_ground_vars(Gvars,Arg,Pos));
		(Error = 0,
		 error_message(second_order1,L,Clause))).

adg_literal_2(LitNum,Lit,BT,ST,Adg,Gvars,Clause,Error) :-
	functor(Lit,F,N),
	literal_property(BT,ST,F/N,(mode),Mode),
	adg_literal_input(1,N,Lit,LitNum,Mode,NewMode,Adg,Gvars,Clause,Error),
	find_internal_predecessor(NewMode,1,LitNum,Pos),
	adg_literal_output(1,N,Lit,LitNum,NewMode,Pos,Adg,Gvars).

%
%  Insert the input positions of a literal and the edges to their 
%  predecessors into the argument dependency.
%
adg_literal_input(M,N,_,_,_,[],_,_,_,1) :-
	M > N.
adg_literal_input(M,N,Lit,LitNum,[(+)|Mode],[(+)|NewMode],Adg,Gvars,Clause,
		  Error) :-
	M =< N,
	new_pos(LitNum,M,Pos),
	insert_adg_entry(Adg,Pos,_),
	insert_adg_field(Adg,Pos,(mode),+),
	arg(M,Lit,Arg),
	find_adg_predecessor(Adg,Gvars,Arg,Pos,Clause,Error1),
	M1 is M+1,
	adg_literal_input(M1,N,Lit,LitNum,Mode,NewMode,Adg,Gvars,Clause,Error2),	Error is Error1*Error2.
adg_literal_input(M,N,Lit,LitNum,[(-)|Mode],[(-)|NewMode],Adg,Gvars,Clause,
		  Error) :-
	M =< N,
	M1 is M+1,
	adg_literal_input(M1,N,Lit,LitNum,Mode,NewMode,Adg,Gvars,Clause,Error).
adg_literal_input(M,N,Lit,LitNum,[(?)|Mode],[IO|NewMode],Adg,Gvars,Clause,
		  Error) :-
	M =< N,
	arg(M,Lit,Arg),
	(ground_argument(Arg,Gvars) ->
		(new_pos(LitNum,M,Pos),
		 insert_adg_entry(Adg,Pos,_),
		 insert_adg_field(Adg,Pos,(mode),+),
		 find_adg_predecessor(Adg,Gvars,Arg,Pos,Clause,Error1),
		 IO = (+));
		(Error1 = 1,
		 IO = (-))),
	M1 is M+1,
	adg_literal_input(M1,N,Lit,LitNum,Mode,NewMode,Adg,Gvars,Clause,Error2),	Error is Error1*Error2.

%
%  Insert the output positions of a literal into the argument dependency 
%  graph, and insert the variables in output positions into the ground
%  variables list.
%
adg_literal_output(M,N,_,_,_,_,_,_) :-
	M > N.
adg_literal_output(M,N,Lit,LitNum,[(+)|MList],PredPos,Adg,Gvars) :-
	M =< N,
	M1 is M+1,
	adg_literal_output(M1,N,Lit,LitNum,MList,PredPos,Adg,Gvars).
adg_literal_output(M,N,Lit,LitNum,[(-)|MList],PredPos,Adg,Gvars) :-
	M =< N,
	new_pos(LitNum,M,Pos),
	insert_adg_entry(Adg,Pos,_),
	insert_adg_field(Adg,Pos,(mode),-),
	arg(M,Lit,Arg),
	insert_ground_vars(Gvars,Arg,Pos),
	insert_adg_predecessor(Adg,Pos,PredPos),
	M1 is M+1,
	adg_literal_output(M1,N,Lit,LitNum,MList,PredPos,Adg,Gvars).

%
%  Test if the term at an argument position has been ground.
%
ground_argument(Term,Gvars) :-
	var(Term),
	find_gvars_field(Gvars,Term,def,PosList),
	nonvar(PosList).
ground_argument(Term,_) :-
	atomic(Term).
ground_argument(Term,Gvars) :-
	compound(Term),
	functor(Term,_,N),
	ground_argument(N,Term,Gvars).
	
ground_argument(0,_,_).
ground_argument(N,Term,Gvars) :-
	N > 0,
	arg(N,Term,Arg),
	ground_argument(Arg,Gvars),
	N1 is N-1,
	ground_argument(N1,Term,Gvars).

%
%  Find the input positions of a literal.
%
find_internal_predecessor([],_,_,_).
find_internal_predecessor([(+)|Mode],N,LitNum,[Pos|PList]) :-
	new_pos(LitNum,N,Pos),
	N1 is N+1,
	find_internal_predecessor(Mode,N1,LitNum,PList).
find_internal_predecessor([(-)|Mode],N,LitNum,Pos) :-
	N1 is N+1,
	find_internal_predecessor(Mode,N1,LitNum,Pos).

%
%  build_ldg.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for building literal dependency graphs.
%

%
%  Build the literal dependency graph of a clause.
%
literal_dependency_graph(Adg,Ldg) :-
	gen_clause_pos(Adg,PosSet),
	literal_dependency_graph(PosSet,Adg,Ldg).

literal_dependency_graph([],_,_).
literal_dependency_graph([Pos|PList],Adg,Ldg) :-
	insert_ldg_node(Adg,Ldg,Pos),
	find_adg_field(Adg,Pos,pred,Pred),
	insert_ldg_arcs(Adg,Ldg,Pos,Pred),
	literal_dependency_graph(PList,Adg,Ldg).

%
%  Insert a node Lit into the literal dependency graph.
%
insert_ldg_node(Adg,Ldg,Pos) :-
	pos_litnum(Pos,0),
	find_adg_field(Adg,Pos,(mode),Mode),
	new_lit(Mode,Lit),
	insert_ldg_entry(Ldg,Lit,_).
insert_ldg_node(_,Ldg,Pos) :-
	pos_litnum(Pos,I),
	I > 0,
	new_lit(I,Lit),
	insert_ldg_entry(Ldg,Lit,_).
	
%
%  Insert the set of arcs to the predecessors of a node into
%  the literal dependency graph.
%
insert_ldg_arcs(_,_,_,Pred) :-
	var(Pred).
insert_ldg_arcs(Adg,Ldg,Pos,Pred) :-
	nonvar(Pred),
	Pred = [P|PList],
	insert_ldg_arc(Adg,Ldg,Pos,P),
	insert_ldg_arcs(Adg,Ldg,Pos,PList).

%
%  Insert an arc into the literal dependency graph.
%
insert_ldg_arc(Adg,Ldg,Pos1,Pos2) :-
	pos_litnum(Pos1,0),
	pos_litnum(Pos2,0),
	find_adg_field(Adg,Pos1,(mode),Mode1),
	new_lit(Mode1,Lit1),
	find_adg_field(Adg,Pos2,(mode),Mode2),
	new_lit(Mode2,Lit2),
	insert_ldg_field(Ldg,Lit1,pred,Lit2),
	insert_ldg_field(Ldg,Lit2,succ,Lit1).
insert_ldg_arc(Adg,Ldg,Pos1,Pos2) :-
	pos_litnum(Pos1,0),
	pos_litnum(Pos2,J),
	J > 0,
	find_adg_field(Adg,Pos1,(mode),Mode1),
	new_lit(Mode1,Lit1),
	new_lit(J,Lit2),
	insert_ldg_field(Ldg,Lit1,pred,Lit2),
	insert_ldg_field(Ldg,Lit2,succ,Lit1).
insert_ldg_arc(Adg,Ldg,Pos1,Pos2) :-
	pos_litnum(Pos1,I),
	I > 0,
	pos_litnum(Pos2,0),
	new_lit(I,Lit1),
	find_adg_field(Adg,Pos2,(mode),Mode2),
	new_lit(Mode2,Lit2),
	insert_ldg_field(Ldg,Lit1,pred,Lit2),
	insert_ldg_field(Ldg,Lit2,succ,Lit1).
insert_ldg_arc(_,Ldg,Pos1,Pos2) :-
	pos_litnum(Pos1,I),
	I > 0,
	pos_litnum(Pos2,J),
	J > 0,
	I =\= J,
	new_lit(I,Lit1),
	new_lit(J,Lit2),
	insert_ldg_field(Ldg,Lit1,pred,Lit2),
	insert_ldg_field(Ldg,Lit2,succ,Lit1).
insert_ldg_arc(_,_,Pos1,Pos2) :-
	pos_litnum(Pos1,I),
	I > 0,
	pos_litnum(Pos2,J),
	J > 0,
	I =:= J.
%
%  dependency.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the data dependency
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the data dependency analysis for a strongly connected component.
%
dependency_analysis([],_,_,[],[],[],1).
dependency_analysis([Pred|CompList],BT,ST,[Adg|AList],[Ldg|LList],
		    [Gvars|GList],Error) :-
	dependency_predicate(Pred,BT,ST,Adg,Ldg,Gvars,Error1),
	dependency_analysis(CompList,BT,ST,AList,LList,GList,Error2),
	Error is Error1*Error2.

%
%  Perform the data dependency analysis for a predicate.
%
dependency_predicate(Pred,BT,ST,Adg,Ldg,Gvars,Error) :-
	find_symbol_field(ST,Pred,clause,Clauses),
	find_symbol_field(ST,Pred,(mode),Mode),
	mutual_exclusive_classes(Clauses,Mode,Classes),
	nl,
	write('* Mutually exclusive classes of clauses for predicate '),
	write(Pred),
	write(' :'),
	nl,nl,
	write(Classes),nl,
	insert_symbol_field(ST,Pred,mutex,Classes),
	dependency_clauses(Clauses,BT,ST,Adg,Ldg,Gvars,Error).

%
%  Perform the data dependency analysis for the set of clauses in a predicate.
%
dependency_clauses(Clauses,_,_,[],[],[],1) :-
	var(Clauses).
dependency_clauses(Clauses,BT,ST,[Adg|AList],[Ldg|LList],[Gvars|GList],Error) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	dependency_clause(Clause,BT,ST,Adg,Ldg,Gvars,Error1),
	dependency_clauses(CList,BT,ST,AList,LList,GList,Error2),
	Error is Error1*Error2.

%
%  Perform the data dependency analysis for a clause.
%
dependency_clause(Clause,BT,ST,Adg,Ldg,Gvars,Error) :-
	clause_type(Clause,Type),
	argument_dependency_graph(Type,Clause,BT,ST,Adg,Gvars,Error),
	literal_dependency_graph(Adg,Ldg).

%
%  gvars.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for manipulating the ground
%  variables list.
%
%  The structure of the ground variables list:
%	gv(Var,pos)
%

%
%  Insert an entry for variable Var in the ground variables list.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Var is inserted and returned.
%
insert_gvars_entry(Gvars,Var,Entry) :- 
	var(Gvars),
	Entry = gv(Var,_,_,_,_,_,_),
	Gvars = [Entry|_].
insert_gvars_entry(Gvars,Var,Entry) :- 
	nonvar(Gvars),
	Gvars = [E|_],
	E = gv(VAR,_,_,_,_,_,_),
	Var == VAR,
	Entry = E.
insert_gvars_entry(Gvars,Var,Entry) :- 
	nonvar(Gvars),
	Gvars = [E|G],
	E = gv(VAR,_,_,_,_,_,_),
	Var \== VAR,
	insert_gvars_entry(G,Var,Entry).

%
%  Insert a bound position for variable Var into the ground variables list.
%
insert_gvars_field(Gvars,Var,def,Pos) :-
	insert_gvars_entry(Gvars,Var,gv(Var,PosList,_,_,_,_,_)),
	insert_gvars_pos(PosList,Pos).
insert_gvars_field(Gvars,Var,use,Pos) :-
	insert_gvars_entry(Gvars,Var,gv(Var,_,PosList,_,_,_,_)),
	insert_gvars_pos(PosList,Pos).
insert_gvars_field(Gvars,Var,relation,Sol) :-
	insert_gvars_entry(Gvars,Var,gv(Var,_,_,Sol,_,_,_)).
insert_gvars_field(Gvars,Var,det,Sol) :-
	insert_gvars_entry(Gvars,Var,gv(Var,_,_,_,Sol,_,_)).
insert_gvars_field(Gvars,Var,redge,Edge) :-
	insert_gvars_entry(Gvars,Var,gv(Var,_,_,_,_,Edge,_)).
insert_gvars_field(Gvars,Var,sedge,Edge) :-
	insert_gvars_entry(Gvars,Var,gv(Var,_,_,_,_,_,Edge)).

insert_gvars_pos(PosList,Pos) :-
	var(PosList),
	PosList = [Pos|_].
insert_gvars_pos(PosList,Pos) :-
	nonvar(PosList),
	PosList = [Pos|_].
insert_gvars_pos(PosList,Pos) :-
	nonvar(PosList),
	PosList = [P|PList],
	P \== Pos,
	insert_gvars_pos(PList,Pos).

%
%  Find the entry for variable Var in the ground variables list.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_gvars_entry(Gvars,_,_) :- 
	var(Gvars).
find_gvars_entry(Gvars,Var,Entry) :- 
	nonvar(Gvars),
	Gvars = [E|_],
	E = gv(VAR,_,_,_,_,_,_),
	Var == VAR,
	Entry = E.
find_gvars_entry(Gvars,Var,Entry) :- 
	nonvar(Gvars),
	Gvars = [E|G],
	E = gv(VAR,_,_,_,_,_,_),
	Var \== VAR,
	find_gvars_entry(G,Var,Entry).

%
%  Find a field for variable Var into the ground variables list.
%
find_gvars_field(Gvars,Var,def,PosList) :-
	find_gvars_entry(Gvars,Var,gv(Var,PosList,_,_,_,_,_)).
find_gvars_field(Gvars,Var,use,PosList) :-
	find_gvars_entry(Gvars,Var,gv(Var,_,PosList,_,_,_,_)).
find_gvars_field(Gvars,Var,relation,Sol) :-
	find_gvars_entry(Gvars,Var,gv(Var,_,_,Sol,_,_,_)).
find_gvars_field(Gvars,Var,det,Sol) :-
	find_gvars_entry(Gvars,Var,gv(Var,_,_,_,Sol,_,_)).
find_gvars_field(Gvars,Var,redge,Edge) :-
	find_gvars_entry(Gvars,Var,gv(Var,_,_,_,_,Edge,_)).
find_gvars_field(Gvars,Var,sedge,Edge) :-
	find_gvars_entry(Gvars,Var,gv(Var,_,_,_,_,_,Edge)).

%
%  Close the postion lists of the ground variable list.
%
close_gvars_list(Gvars) :-
	var(Gvars).
close_gvars_list(Gvars) :-
	nonvar(Gvars),
	Gvars = [gv(_,Def,Use,_,_,_,_)|G],
	close_list(Def),
	close_list(Use),
	close_gvars_list(G).

%
%  Print out the ground variables list.
%
print_gvars_list(Gvars) :-
	tell(gvars_list),
	p_gvars_list(Gvars),
	told.

p_gvars_list([]).
p_gvars_list([E|Gvars]) :-
	write(E),
	nl,
	p_gvars_list(Gvars).
%
%  ldg.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for manipulating the literal
%  dependency graph.
%
%  The structure of the literal dependency graph:
%	ldg(Lit,edge)
%

%
%  Insert an entry for literal Lit in the literal dependency graph.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Lit is inserted and returned.
%
insert_ldg_entry(Ldg,Lit,Entry) :- 
	var(Ldg),
	Entry = ldg(Lit,_,_,_,_,_,_),
	Ldg = [Entry|_].
insert_ldg_entry(Ldg,Lit,Entry) :- 
	nonvar(Ldg),
	Ldg = [Entry|_],
	Entry = ldg(Lit,_,_,_,_,_,_).
insert_ldg_entry(Ldg,Lit,Entry) :- 
	nonvar(Ldg),
	Ldg = [ldg(Lit1,_,_,_,_,_,_)|L],
	Lit \== Lit1,
	insert_ldg_entry(L,Lit,Entry).

%
%  Insert an edge for literal Lit into the literal dependency
%  graph.
%
insert_ldg_field(Ldg,Lit,pred,Edge) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,Pred,_,_,_,_,_)),
	insert_ldg_edge(Pred,Edge).
insert_ldg_field(Ldg,Lit,succ,Edge) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,_,Succ,_,_,_,_)),
	insert_ldg_edge(Succ,Edge).
insert_ldg_field(Ldg,Lit,relation,Sol) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,_,_,Sol,_,_,_)).
insert_ldg_field(Ldg,Lit,det,Sol) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,Sol,_,_)).
insert_ldg_field(Ldg,Lit,redge,Maxv) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,_,Maxv,_)).
insert_ldg_field(Ldg,Lit,sedge,Maxv) :-
	insert_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,_,_,Maxv)).

insert_ldg_edge(EdgeList,Edge) :-
	var(EdgeList),
	EdgeList = [Edge|_].
insert_ldg_edge(EdgeList,Edge) :-
	nonvar(EdgeList),
	EdgeList = [Edge|_].
insert_ldg_edge(EdgeList,Edge) :-
	nonvar(EdgeList),
	EdgeList = [E|EList],
	E \== Edge,
	insert_ldg_edge(EList,Edge).

%
%  Find the entry for literal Lit in the literal dependency graph.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_ldg_entry(Ldg,_,_) :- 
	var(Ldg).
find_ldg_entry(Ldg,Lit,Entry) :- 
	nonvar(Ldg),
	Ldg = [Entry|_],
	Entry = ldg(Lit,_,_,_,_,_,_).
find_ldg_entry(Ldg,Lit,Entry) :- 
	nonvar(Ldg),
	Ldg = [ldg(Lit1,_,_,_,_,_,_)|L],
	Lit \== Lit1,
	find_ldg_entry(L,Lit,Entry).

%
%  Find a field for literal Lit in the literal dependency graph.
%
find_ldg_field(Ldg,Lit,pred,Pred) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,Pred,_,_,_,_,_)).
find_ldg_field(Ldg,Lit,succ,Succ) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,_,Succ,_,_,_,_)).
find_ldg_field(Ldg,Lit,relation,Sol) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,_,_,Sol,_,_,_)).
find_ldg_field(Ldg,Lit,det,Sol) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,Sol,_,_)).
find_ldg_field(Ldg,Lit,redge,Maxv) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,_,Maxv,_)).
find_ldg_field(Ldg,Lit,sedge,Maxv) :-
	find_ldg_entry(Ldg,Lit,ldg(Lit,_,_,_,_,_,Maxv)).

%
%  Create a new literal.
%
new_lit(LitNum,'$'(LitNum)).

%
%  Print out the literal dependency graph.
%
print_ldg(Ldg) :-
	tell(ldg),
	p_ldg(Ldg),
	told.

p_ldg([]).
p_ldg([E|Ldg]) :-
	write(E),
	nl,
	p_ldg(Ldg).
%
%  position.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for handling the positions.
%

%
:- op(200, fx, [$]).

%
%  Construct a new position structure.
%
new_pos(LitNum,ArgNum,'$'(LitNum,ArgNum)).

%
%  Get the literal number of a position.
%
pos_litnum('$'(LitNum,_),LitNum).

%
%  Get the argument number of a position.
%
pos_argnum('$'(_,ArgNum),ArgNum).

%
%  Generate the set of argument positions in a clause.
%
gen_clause_pos(Adg,[]) :-
	var(Adg).
gen_clause_pos(Adg,[Pos|PList]) :-
	nonvar(Adg),
	Adg = [adg(Pos,_,_,_)|AList],
	gen_clause_pos(AList,PList).

%
%  Generate the set of argument positions in a literal.
%
gen_literal_pos(_/N,LitNum,Pos) :-
	gen_lit_pos(0,N,LitNum,Pos).

gen_lit_pos(N,N,_,[]).
gen_lit_pos(M,N,LitNum,[Pos|PList]) :-
	M < N,
	M1 is M+1,
	new_pos(LitNum,M1,Pos),
	gen_lit_pos(M1,N,LitNum,PList).

%
%  Generate the set of input (or output) argument positions in a literal.
%
gen_literal_iopos(Adg,Lit,LitNum,Mode,Pos) :-
	gen_literal_pos(Lit,LitNum,Pos1),
	gen_lit_iopos(Pos1,Adg,Mode,Pos).

gen_lit_iopos([],_,_,[]).
gen_lit_iopos([P|PList1],Adg,Mode,Pos) :-
	find_adg_field(Adg,P,(mode),Mode1),
	(Mode1 == Mode ->
		Pos = [P|PList];
		PList = Pos),
	gen_lit_iopos(PList1,Adg,Mode,PList).
	
%
%  determinacy.pl		Nai-Wei Lin			May 1992
%
%  This file contains the procedures for performing the determinacy analysis
%  for the predicates in the program in topologically sorted order.
%

%
%  Perform the determinacy analysis for a strongly connected component.
%
determinacy_analysis(Comp,BT,ST,Adg) :-
	initial_determinacy(Comp,Det),
	determinacy_analysis1(Comp,BT,ST,Comp,Adg,Det).

determinacy_analysis1(Comp,BT,ST,Comp,Adg,Det) :-
	determinacy_analysis2(Comp,BT,ST,Comp,Det,Adg,NDet),
	(Det == NDet ->
		insert_determinacy(Det,ST);
		determinacy_analysis1(Comp,BT,ST,Comp,Adg,NDet)).

determinacy_analysis2([],_,_,_,_,_,[]).
determinacy_analysis2([Pred|Comps],BT,ST,Comp,CompDet,[Adg|AList],
		[comp(Pred,Det)|Dets]) :-
	determinacy_predicate(Pred,BT,ST,Comp,CompDet,Adg,Det),
	determinacy_analysis2(Comps,BT,ST,Comp,CompDet,AList,Dets).

%
%  Perform the determinacy analysis for a predicate.
%
determinacy_predicate(Pred,BT,ST,Comp,CompDet,Adg,Det) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	(pairwise_mutual_exclusion(Mutex) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 determinacy_clauses(Clauses,BT,ST,Comp,CompDet,Adg,Det));
		Det = 0).

%
%  Perform the determinacy analysis for the set of clauses in a predicate.
%
determinacy_clauses(Clauses,_,_,_,_,_,1) :-
	var(Clauses).
determinacy_clauses(Clauses,BT,ST,Comp,CompDet,[Adg|AList],Det) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	determinacy_clause(Clause,BT,ST,Comp,CompDet,Adg,Det1),
	(Det1 == 1 ->
		determinacy_clauses(CList,BT,ST,Comp,CompDet,AList,Det);
		Det = 0).

%
%  Perform the determinacy analysis for a clause.
%
determinacy_clause(Clause,BT,ST,Comp,CompDet,Adg,Det) :-
	clause_type(Clause,Type),
	determinacy_clause(Type,Clause,BT,ST,Comp,CompDet,Adg,Det).

determinacy_clause(2,Clause,BT,ST,Comp,CompDet,Adg,Det) :-
	Clause = (_ :- Body),
	no_of_cuts(Body,Cuts),
	determinacy_body(Body,1,BT,ST,Comp,Cuts,CompDet,Adg,Clause,Det).
determinacy_clause(3,_,_,_,_,_,_,1).

%
%  Perform the determinacy analysis for the body of a clause.
%
determinacy_body((Lit,Body),Num,BT,ST,Comp,Cuts,CompDet,Adg,Clause,Det) :-
	Num1 is Num+1,
	(Lit == (!) ->
		(Cuts1 is Cuts-1,
		 determinacy_body(Body,Num1,BT,ST,Comp,Cuts1,CompDet,Adg,Clause,Det));
		(Cuts > 0 ->
		 	determinacy_body(Body,Num1,BT,ST,Comp,Cuts,CompDet,Adg,Clause,Det);
			(determinacy_literal(Lit,Num,BT,ST,Comp,CompDet,Adg,Clause,Det1),
			 (Det1 == 1 ->
				determinacy_body(Body,Num1,BT,ST,Comp,Cuts,
					CompDet,Adg,Clause,Det);
				Det = 0)))).
determinacy_body(Lit,Num,BT,ST,Comp,_,CompDet,Adg,Clause,Det) :-
	nonsequence(Lit),
	(Lit == (!) ->
		Det = 1;
		determinacy_literal(Lit,Num,BT,ST,Comp,CompDet,Adg,Clause,Det)).

%
%  Perform the determinacy analysis for a literal.
%
determinacy_literal(Lit,LitNum,BT,ST,Comp,CompDet,Adg,Clause,Det) :-
	functor(Lit,F,A),
	(second_order_predicate(F/A) ->
		%% handle setof predicate
		(second_order_predicate_pred_arg(Lit,NLit),
		 functor(NLit,NF,NA),
		 arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num),
		 gen_literal_iopos(Adg,(NF/NA),Num,(-),Pos),
		 pos_var(Pos,NLit,Vars),
		 arg(1,Lit,Arg1),
		 term_var(Arg1,Var1),
		 (opened_set_equivalent(Var1,Vars) ->
			Det = 1;
			Det = 0));
		%% handle other predicates
		(member(Comp,(F/A)) ->
		   find_recursive_comp(CompDet,(F/A),Det);
		   (literal_property(BT,ST,(F/A),det,[Det1]),
		    ((Det1 == 0; Det1 == 1) ->
			Det = 1;
			Det = 0)))).
		
%
%  Initialize the determinacy to `true' for the predicates in the component.
%
initial_determinacy([],[]).
initial_determinacy([Pred|Comp],[comp(Pred,1)|Det]) :-
	initial_determinacy(Comp,Det).

%
%  Define the det field for the determinate predicates in the component.
%
insert_determinacy([],_).
insert_determinacy([comp(Pred,Det)|Dets],ST) :-
	(Det == 1 ->
		(find_symbol_field(ST,Pred,relation,Rel),
		 (Rel == 0 ->
			insert_symbol_field(ST,Pred,det,[0]);
			insert_symbol_field(ST,Pred,det,[1])));
		true),
	insert_determinacy(Dets,ST).

%
%  Test if the clauses of a predicate is pairwise mutually exclusive.
%
pairwise_mutual_exclusion([]).
pairwise_mutual_exclusion([[_]|Mutex]) :-
	pairwise_mutual_exclusion(Mutex).
%
%  mutual_exclusion.pl		Nai-Wei Lin			April 1992
%
%  This file contains the procedures for classifying the mutually exclusive
%  classes of clauses.
%

%
%  Find the mutually exclusive classes of clauses.
%
mutual_exclusive_classes(Clauses,Mode,Classes) :-
	initial_classes(Clauses,IClasses),
	mutual_exclusive_classes(Clauses,Mode,1,IClasses,Classes).

%
%  Set up the initial empty classes.
%
initial_classes(Clauses,[]) :-
	var(Clauses).
initial_classes(Clauses,[[]|Classes]) :-
	nonvar(Clauses),
	Clauses = [_|Cs],
	initial_classes(Cs,Classes).

%
%  Find the mutually exclusive classes of clauses based on Prolog ordering.
%
mutual_exclusive_classes(Clauses,_,_,_,[]) :-
	var(Clauses).
mutual_exclusive_classes(Clauses,Mode,I,IClasses,Classes) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	IClasses = [IC|ICs],
	mutual_exclusive_clauses(Cs,C,ICs,IC,Mode,I,NIClass,Mutex,In),
	((Mutex == 1; In == 1) ->
		(append(IC,[I],NIC),
		 Classes = [NIC|Classes1]);
		Classes = Classes1),
	I1 is I+1,
	mutual_exclusive_classes(Cs,Mode,I1,NIClass,Classes1).

%
%  Check the mutual exclusion of a clause and its sucessors.
%
mutual_exclusive_clauses(Clauses,_,_,_,_,_,[],1,1) :-
	var(Clauses).
mutual_exclusive_clauses(Clauses,Clause,IClasses,IClass,Mode,I,
		[NIC|NIClasses],Mutex,In) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	IClasses = [IC|ICs],
	(mutual_exclusive_clause(Clause,C,Mode) ->
		(Mutex1 = 1,		/* Clause & C are mutually exclusive */
		 In1 = 1,
		 NIC = IC);
		(Mutex1 = 0,
		 (set_inclusion(IClass,IC) ->
			In1 = 0;	/* Class IClass contained in IC */
			In1 = 1),
		 append(IC,[I],NIC))),	/* Put Clause in the same class as C */
	mutual_exclusive_clauses(Cs,Clause,ICs,IClass,Mode,I,NIClasses,
		Mutex2,In2),
	Mutex is Mutex1*Mutex2,
	In is In1*In2.

%
%  Check if clauses C1 & C2 are mutually exclusive. It is true if C1 contains
%  a cut (!) or the input arguments of C1 and C2 are not unifiable.
%
mutual_exclusive_clause(C1,C2,Mode) :-
	mutual_exclusive_clause1(C1,C2,Mode).

mutual_exclusive_clause1(C1,C2,Mode) :-
	((fail_clause(C1);fail_clause(C2)) ->
		true;			/* At least one of the clauses fails */
		mutual_exclusive_clause2(C1,C2,Mode)).
	
mutual_exclusive_clause2(C1,C2,Mode) :-
	(cut_clause(C1) ->
		true;			/* C1 contains a cut */
		mutual_exclusive_clause3(C1,C2,Mode)).

mutual_exclusive_clause3(C1,C2,Mode) :-
	copy_term(C1,T1),
	clause_head(T1,H1),
	input_term(Mode,H1,1,I1),
	copy_term(C2,T2),
	clause_head(T2,H2),
	input_term(Mode,H2,1,I2),
	(non_unifiable(I1,I2) ->
		true;			/* heads are not unifiable */
		(mutual_exclusive_clause4(I1,I2,T1,T2) ->
			true;
			(mutual_exclusive_clause5(I1,I2,T1,T2)))).
		
mutual_exclusive_clause4([A1|I1],[A2|I2],C1,C2) :-
	number(A1),
	var(A2),!,
	(mutual_exclusive_clause_number(C2,A2,(A2=:=A1)) ->
		true;
		mutual_exclusive_clause4(I1,I2,C1,C2)).
mutual_exclusive_clause4([A1|I1],[A2|I2],C1,C2) :-
	number(A2),
	var(A1),!,
	(mutual_exclusive_clause_number(C1,A1,(A1=:=A2)) ->
		true;
		mutual_exclusive_clause4(I1,I2,C1,C2)).
mutual_exclusive_clause4([A1|I1],[A2|I2],C1,C2) :-
	atom(A1),
	var(A2),!,
	(mutual_exclusive_clause_number(C2,A2,(A2=:=A1)) ->
		true;
		mutual_exclusive_clause4(I1,I2,C1,C2)).
mutual_exclusive_clause4([A1|I1],[A2|I2],C1,C2) :-
	atom(A2),
	var(A1),!,
	(mutual_exclusive_clause_number(C1,A1,(A1=:=A2)) ->
		true;
		mutual_exclusive_clause4(I1,I2,C1,C2)).
mutual_exclusive_clause4([_|I1],[_|I2],C1,C2) :-
	mutual_exclusive_clause4(I1,I2,C1,C2).

mutual_exclusive_clause5(I1,I2,C1,C2) :-
	variable_set(I1,[],V1),
	length(V1,L1),
	variable_set(I2,[],V2),
	length(V2,L2),
	L1 == L2,
	list_substitute(V2,V1,I2,I3),
	I1 == I3,!,
	list_substitute(V2,V1,C2,C3),
	mutual_exclusive_clauses5(V1,C1,C3).

mutual_exclusive_clauses5([V|_],C1,C2) :-
	get_comparison_literal(C1,V,L1),
	mutual_exclusive_clause_number(C2,V,L1),
	!.
mutual_exclusive_clauses5([_|V1],C1,C2) :-
	mutual_exclusive_clauses5(V1,C1,C2).

variable_set([],V,V).
variable_set([I|Is],V,NV) :-
	variable_set1(I,V,V1),
	variable_set(Is,V1,NV).

variable_set1(I,V,V1) :-
	var(I),!,
	(member(V,I) ->
		V1 = V;
		V1 = [I|V]).
variable_set1(I,V,V) :-
	atomic(I),!.
variable_set1(I,V,NV) :-
	functor(I,_,A),
	variable_set1(A,I,V,NV).

variable_set1(0,_,V,V).
variable_set1(A,I,V,NV) :-
	A > 0,
	arg(A,I,Arg),
	variable_set1(Arg,V,V1),
	A1 is A-1,
	variable_set1(A1,I,V1,NV).

list_substitute([],[],T,T).
list_substitute([V1|V1s],[V2|V2s],T1,T2) :-
	substitute(T1,V1,V2,T12),
	list_substitute(V1s,V2s,T12,T2).
	
get_comparison_literal(C,V,L) :-
	clause_type(C,2),
	C = (_:-Body),
	get_comparison_literal1(Body,V,L).

get_comparison_literal1((Lit,_),V,L) :-
	functor(Lit,F,A),
	comparison_op(F/A),
	convert_op(Lit,V,L).
get_comparison_literal1((_,Body),V,L) :-
	get_comparison_literal1(Body,V,L).
get_comparison_literal1(Lit,V,L) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	comparison_op(F/A),
	convert_op(Lit,V,L).

%
mutual_exclusive_clause_number(Clause,Var,Expr) :-
	clause_type(Clause,2),
	Clause = (_ :- Body),
	mutual_exclusive_body_number(Body,Var,Expr).

mutual_exclusive_body_number((Lit,Body),Var,Expr) :-
	!,
	functor(Lit,F,A),
	mutual_allowable_op((F/A)),
	(mutual_exclusive_literal_number(Lit,Var,Expr) ->
		true;
		mutual_exclusive_body_number(Body,Var,Expr)).
mutual_exclusive_body_number(Lit,Var,Expr) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	mutual_allowable_op((F/A)),
	mutual_exclusive_literal_number(Lit,Var,Expr).

mutual_exclusive_literal_number(Lit,Var,Expr) :-
	functor(Lit,F1,A1),
	functor(Expr,F2,A2),
	((comparison_op((F1/A1)),convert_op(Lit,Var,NLit)) ->
		(functor(NLit,F3,A3),
		 comparable_op((F3/A3),(F2/A2),Sign),
		 (Sign > 0 ->
			mutual_exclusive_number(NLit,Expr);
			mutual_exclusive_number(Expr,NLit)))).
		
convert_op((E1 == E2),Var,(E1 =:= E2)) :- E1 == Var,!.
convert_op((E1 == E2),Var,(E2 =:= E1)) :- E2 == Var,!.
convert_op((E1 \== E2),Var,(E1 =\= E2)) :- E1 == Var,!.
convert_op((E1 \== E2),Var,(E2 =\= E1)) :- E2 == Var,!.
convert_op(Lit,Var,Lit) :-
	arg(1,Lit,Arg),
	Arg == Var,!.
convert_op(Lit,Var,NLit) :-
	arg(2,Lit,Arg),
	Arg == Var,
	functor(Lit,F,A),
	functor(NLit,F,A),
	arg(1,NLit,Arg),
	arg(1,Lit,Arg1),
	arg(2,NLit,Arg1).

mutual_allowable_op(Op) :-
	comparison_op(Op).
mutual_allowable_op((is)/2).
mutual_allowable_op((functor)/3).
mutual_allowable_op((arg)/3).
mutual_allowable_op((atomic)/1).
mutual_allowable_op((atom)/1).
mutual_allowable_op((number)/1).
mutual_allowable_op((integer)/1).
mutual_allowable_op((float)/1).
mutual_allowable_op((true)/0).
mutual_allowable_op((!)/0).

comparison_op(Op) :-
	arithmetic_op(Op).
comparison_op((==)/2).
comparison_op((\==)/2).

comparable_op((>)/2,(=:=)/2,1).
comparable_op((=:=)/2,(>)/2,-1).
comparable_op((>=)/2,(=:=)/2,1).
comparable_op((=:=)/2,(>=)/2,-1).
comparable_op((<)/2,(=:=)/2,1).
comparable_op((=:=)/2,(<)/2,-1).
comparable_op((=<)/2,(=:=)/2,1).
comparable_op((=:=)/2,(=<)/2,-1).
comparable_op((=\=)/2,(=:=)/2,1).
comparable_op((=:=)/2,(=\=)/2,-1).
comparable_op((=:=)/2,(=:=)/2,1).
comparable_op((=:=)/2,(=:=)/2,-1).
comparable_op((>)/2,(<)/2,1).
comparable_op((<)/2,(>)/2,-1).
comparable_op((>=)/2,(<)/2,1).
comparable_op((<)/2,(>=)/2,-1).
comparable_op((>)/2,(=<)/2,1).
comparable_op((=<)/2,(>)/2,-1).
comparable_op((>=)/2,(=<)/2,1).
comparable_op((=<)/2,(>=)/2,-1).

mutual_exclusive_number((E1>E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number((E1>=E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 > E4.
mutual_exclusive_number((E1<E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =< E4.
mutual_exclusive_number((E1=<E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 < E4.
mutual_exclusive_number((E1=\=E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =:= E4.
mutual_exclusive_number((E1=:=E2),(E3=:=E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 =\= E4.
mutual_exclusive_number((E1>E2),(E3<E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number((E1>=E2),(E3<E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number((E1>E2),(E3=<E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 >= E4.
mutual_exclusive_number((E1>=E2),(E3=<E4)) :-
	E1 == E3,
	number(E2),
	number(E4),
	!,
	E2 > E4.
mutual_exclusive_number((E1=\=E2),(E3=:=E4)) :-
	E1 == E3,
	atom(E2),
	atom(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1=:=E2),(E3=:=E4)) :-
	E1 == E3,
	atom(E2),
	atom(E4),
	!,
	E2 \== E4.
mutual_exclusive_number((E1>E2),(E3=:=E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1<E2),(E3=:=E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1=\=E2),(E3=:=E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1>E2),(E3<E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1>=E2),(E3<E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.
mutual_exclusive_number((E1>E2),(E3=<E4)) :-
	E1 == E3,
	var(E2),
	var(E4),
	!,
	E2 == E4.

%
%  Test if a clause contains a cut.
%
cut_clause(C) :-
	clause_type(C,2),
	C = (_:-Body),
	cut_body(Body).

cut_body((Lit,Body)) :-
	(Lit == (!) ->
		true;
		cut_body(Body)).
cut_body((!)).
	
%  Check if two terms are not unifiable.
%
non_unifiable(C,C) :- !,fail.
non_unifiable(_,_).

%
%  Get the head of a clause.
%
clause_head(C,H) :-
	clause_type(C,Type),
	(Type == 2 ->
		C = (H:-_);
		C = H).

%
%  Get the input arguments of an atom based on its mode.
% 
input_term([],_,_,[]).
input_term([+|Mode],Term,I,[Arg|Is]) :-
	arg(I,Term,Arg),
	I1 is I+1,
	input_term(Mode,Term,I1,Is).
input_term([-|Mode],Term,I,Is) :-
	I1 is I+1,
	input_term(Mode,Term,I1,Is).

%
%  Test if a set is included in another set.
%
set_inclusion([],_).
set_inclusion([E|S1],S2) :-
	(member(S2,E) ->
		set_inclusion(S1,S2)).
%
%  diff_equ.pl			Nai-Wei Lin			February, 1992
%
%  This file contains the procedures for solving linear difference equations.
%

solve_typed_diff_equ(size,DE,BE,Var,ST,Pred,Pos,Sol) :-
	(product_diff_equ(DE,Pred,NDE) ->
		(diff_equ_type(NDE,Var,Pred,A1n,A2n,Bn,Dtype),
		 log_base_equs(BE,NBE),
		 solve_one_index_size_diff_equ(Dtype,NBE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol1),
		 exp_solution(Sol1,Sol));
		(diff_equ_type(DE,Var,Pred,A1n,A2n,Bn,Dtype),
		 solve_one_index_size_diff_equ(Dtype,BE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol))).
solve_typed_diff_equ(comp,DE,BE,Var,ST,Pred,Pos,Sol) :-
	(product_diff_equ(DE,Pred,NDE) ->
		(diff_equ_type(NDE,Var,Pred,A1n,A2n,Bn,Dtype),
		 log_base_equs(BE,NBE),
		 solve_one_index_comp_diff_equ(Dtype,NBE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol1),
		 exp_solution(Sol1,Sol));
		(diff_equ_type(DE,Var,Pred,A1n,A2n,Bn,Dtype),
		 solve_one_index_comp_diff_equ(Dtype,BE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol))).
%
%  Determine the type of a difference equation.
%
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,first_order) :-
	first_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,second_order) :-
	second_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,higher_order) :-
	higher_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,divide_conquer) :-
	divide_conquer_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,mutual_size) :-
	mutual_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,implicit_size) :-
	implicit_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,explicit_size) :-
	explicit_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,list_size) :-
	list_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(_,_,_,_,_,_,no_match).

%
%  Solve a difference equation.
%
solve_diff_equ(first_order,Var,An,_,Bn,Ivalue,Sol) :-
	solve_fode(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(second_order,Var,A1n,A2n,Bn,Ivalue,Sol) :-
	solve_sode(Var,Ivalue,A1n,A2n,Bn,Sol).
solve_diff_equ(higher_order,Var,A1n,_,Bn,Ivalue,Sol) :-
	solve_hode(Var,Ivalue,A1n,Bn,Sol).
solve_diff_equ(divide-conquer,Var,A1n,A2n,Bn,Ivalue,Sol) :-
	solve_dcde(Var,Ivalue,A1n,A2n,Bn,Sol).
solve_diff_equ(mutual_size,Var,An,_,Bn,Ivalue,Sol) :-
	solve_msde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(implicit_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_isde(Var,Ivalue,Bn,Sol).
solve_diff_equ(explicit_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_esde(Var,Ivalue,Bn,Sol).
solve_diff_equ(list_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_lsde(Var,Ivalue,Bn,Sol).
solve_diff_equ(no_match,_,_,_,inf).

%
%  Test if an equation is solvable using constant coefficient solving method.
%
const_coeff_solvable([],_).
const_coeff_solvable([factor(I,_)|F],Var) :-
	const_coeff_solvable1(I,Var),
	const_coeff_solvable(F,Var).

const_coeff_solvable1([],_).
const_coeff_solvable1([Var],Var).
const_coeff_solvable1([exp(E1,E2)],Var) :-
	normal_form(Var,E1),
	normal_form(2,E2).
const_coeff_solvable1([exp(E1,E2)],Var) :-
	E1 = expr([],[factor([],_)]),
	normal_form(Var,E2).

%
%
exp_only_expr([],_,_,_).
exp_only_expr([factor(I,_)|F],Var,R1,R2) :-
	exp_only_expr1(I,Var,R1,R2),
	exp_only_expr(F,Var,R1,R2).

exp_only_expr1([exp(E1,E2)],Var,R1,R2) :-
	E1 = expr([],[factor([],C)]),
	C =\= R1,
	C =\= R2,
	normal_form(Var,E2).

%
%
real_quadratic_roots(A1,A2,R1,R2) :-
	D is A1*A1+4*A2,
	(D >= 0 ->
		normal_form(exp(D,0.5),ND),
		general_form(ND,D1),
		R1 is (A1+D1)/2,
		R2 is (A1-D1)/2).

%
%
unit_count(R1,R2,I) :-
	unit_count(R1,I1),
	unit_count(R2,I2),
	I is I1+I2.

unit_count(R,1) :-
	R =:= 1.
unit_count(R,0) :-
	R =\= 1.
%
%  divide_conquer.pl
%
%    Handle difference equations from divide-and-conquer paradigm.
%

%
%  Test if a difference equation is linear divide-and-conquer.
%
divide_conquer_diff_equ(Equ,Var,F/_,A,C,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,F,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],I)]),
	I < 1,
	C is 1/I,
	An = [factor([],A)],
	divide_conquer_solvable(Bn,Var).

%
%  Solve a linear divide-and-conquer difference equation.
%
solve_dcde(Var,[val(Iindex,Ival)],A,C,Bn,Sol) :-
	normal_form(1,Iindex),!,
	divide_conquer_par_sol(Bn,Var,A,C,PSol),
	normal_form(exp(Var,log(C,A)),E),
	multiply_expr(Ival,E,GSol),
	add_expr(GSol,PSol,Sol).
solve_dcde(_,_,_,_,_,inf).

%
%
divide_conquer_par_sol([],_,_,_,Sol) :-
	normal_form(0,Sol).
divide_conquer_par_sol([factor(I,D)|F],Var,A,C,Sol) :-
	divide_conquer_order(I,Var,O),
	simplification(exp(C,O),X),
	divide_conquer_par_sol1(O,X,Var,A,C,Sol1),
	normal_form(D,D1),
	multiply_expr(D1,Sol1,Sol2),
	divide_conquer_par_sol(F,Var,A,C,Sols),
	add_expr(Sol2,Sols,Sol).

%
%
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =:= E,
	normal_form(exp(Var,O)*log(C,Var),Sol).
divide_conquer_par_sol1(O,E,Var,A,C,Sol) :-
	A =\= E,
	N = exp(Var,log(C,A))-exp(Var,O),
	D = A-exp(C,O),
	Exp = exp(C,O)*(N/D),
	normal_form(Exp,Sol).

%
%
divide_conquer_order([],_,0).
divide_conquer_order([Var],Var,1).
divide_conquer_order([exp(E1,E2)],Var,I) :-
	normal_form(Var,E1),
	general_form(E2,E),
	I is integer(E).

%
%
divide_conquer_solvable([],_).
divide_conquer_solvable([factor(I,_)|F],Var) :-
	divide_conquer_solvable1(I,Var),
	divide_conquer_solvable(F,Var).

divide_conquer_solvable1([],_).
divide_conquer_solvable1([Var],Var).
divide_conquer_solvable1([exp(E1,E2)],Var) :-
	normal_form(Var,E1),
	general_form(E2,I),
	I1 is integer(I),
	I =:= I1.
%
%  Test if a difference equation based on measure term-size on a structure
%  is linear ``first-order'' with coefficient 1. That is,
%  f(n) = f(arg(n,m)) + f(arg(n,m-1)) + ... + g(n), where
%  g(n) is either d, d*n, or arg(n,i).
%
explicit_size_diff_equ(Equ,Var,F/_,_,_,Bn) :-
	Equ = expr(Term,Bn),
	explicit_size_terms(Term,Var,F).

explicit_size_terms([],_,_).
explicit_size_terms([Term|Terms],Var,Pred) :-
	explicit_size_term(Term,Var,Pred),
	explicit_size_terms(Terms,Var,Pred).

explicit_size_term(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,arg(Var,I)),
	integer(I).

%
%  Solve a linear first-order difference equation with coefficient 1 based
%  on measure term-size on a structure.
%
solve_esde(Var,Ivalue,Bn,Sol) :- 
	%nl,write(Bn),nl,
	explicit_size_solvable(Bn,Var),!,
	%write('esde2'),nl,
	%write(Bn),nl,
	Ivalue = [val(_,Ival)],
	general_form(Ival,GIval),
	%write(GIval),nl,
	solve_esde1(Var,Bn,GIval,Sol).
solve_esde(_,_,_,inf).

solve_esde1(Var,[],GIval,Sol) :- !,normal_form(GIval*Var,Sol).
solve_esde1(Var,Bn,GIval,Sol) :- Bn \== [], solve_esde2(Bn,Var,GIval,Sol).

solve_esde2([],_,_,Zero) :- normal_form(0,Zero).
solve_esde2([B|Bn],Var,GIval,Sol) :-
	%write(B),nl,
	solve_esde3(B,Var,GIval,Sol1),
	%general_form(Sol1,GSol),nl,
	%write(GSol),nl,nl,
	solve_esde2(Bn,Var,GIval,Sols),
	add_expr(Sol1,Sols,Sol).

solve_esde3(factor([],C),Var,GIval,Sol) :- 	% d
	!, normal_form((max(C,GIval)*Var),Sol).
solve_esde3(factor([Var],C),Var,GIval,Sol) :- 	% d*x
	!, normal_form(((max(C,GIval)/2)*Var*(Var+1)),Sol).
solve_esde3(factor([arg(_,_)],C),Var,GIval,Sol) :- % an approx. same as d*x
	!, normal_form(((max(C,GIval)/2)*Var*(Var+1)),Sol).
solve_esde3(factor([exp(E1,E2)],C),Var,GIval,Sol) :- 	% d*2^x
	normal_form(Var,E2), general_form(E1,I), integer(I),!,
	(number(GIval) ->
		normal_form(max(C,GIval)*(exp(I,Var+1)-1)/(I-1),Sol);
		normal_form(C*(exp(I,Var+1)-1)/(I-1)+(GIval*Var),Sol)).
solve_esde3(factor([exp(E1,E2)],C),Var,GIval,Sol) :- 	% d*i^arg(x,n)
	general_form(E2,arg(Var,N)), integer(N),
	general_form(E1,I), integer(I),
	(number(GIval) ->
		normal_form(max(C,GIval)*(exp(I,Var+1)-1)/(I-1),Sol);
		normal_form(C*(exp(I,Var+1)-1)/(I-1)+(GIval*Var),Sol)).

%
explicit_size_solvable([],_).
explicit_size_solvable([B|Bn],Var) :-
	explicit_size_solvable1(B,Var),
	explicit_size_solvable(Bn,Var).

explicit_size_solvable1(factor([],_),_).	% d
explicit_size_solvable1(factor([Var],_),Var).	%d*x
explicit_size_solvable1(factor([arg(Expr1,Expr2)],_),Var) :- 	% arg(x,i)
	normal_form(Var,Expr1), general_form(Expr2,I), integer(I).
explicit_size_solvable1(factor([exp(Expr1,Expr2)],_),Var) :-	%d*i^x
	normal_form(Var,Expr2), general_form(Expr1,I), integer(I),!.
explicit_size_solvable1(factor([exp(Expr1,Expr2)],_),Var) :-	%d*i^arg(x,n)
	general_form(Expr2,arg(Var,N)), integer(N),
	general_form(Expr1,I), integer(I).

%
%  first_order.pl 
%
%    Handle first-order linear difference equation.
%

%
%  Test if a difference equation is linear first-order.
%
first_order_diff_equ(Equ,Var,F/_,An,_,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,F,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	I =:= -1.

%
%  Test if a difference equation is linear first-order with constant 
%  coefficient.
%
first_order_con_diff_equ(Var,Ivalue,A,B,An,Bn) :-
	A = [factor([],_)],	% A constant
	An = expr([],A),
	const_coeff_solvable(B,Var),
	Bn = expr([],B),
	Ivalue = [val(Iindex,_)],
	general_form(Iindex,Index),
	integer(Index).
	
%
%  Test if a difference equation is linear first-order with variable 
%  coefficient.
%
first_order_var_diff_equ(_,Ivalue,A,B,An,Bn) :-
	An = expr([],A),
	Bn = expr([],B),
	Ivalue = [val(Iindex,_)],
	general_form(Iindex,Index),
	integer(Index).

%
%  Solve a linear first-order difference equation.
%
solve_fode(Var,Ivalue,A,B,Sol) :-
	first_order_con_diff_equ(Var,Ivalue,A,B,An,Bn),!,
	solve_focde(Var,Ivalue,An,Bn,Sol).
solve_fode(Var,Ivalue,A,B,Sol) :-
	first_order_var_diff_equ(Var,Ivalue,A,B,An,Bn),!,
	solve_fovde(Var,Ivalue,An,Bn,Sol).
solve_fode(_,_,_,_,inf).

%
%  Solve a linear first-order constant coefficient difference equation.
%
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	normal_form(1,A),!,
	solve_fovde(Var,Ivalue,A,Bn,Sol).
solve_focde(Var,Ivalue,A,Bn,Sol) :-
	Bn = expr([],BN),
	first_order_par_sol(BN,Var,A,Sol1),
	first_order_gen_sol(Var,Ivalue,A,Sol1,Sol).

%
%
first_order_par_sol([],_,_,Sol) :-
	normal_form(0,Sol).
first_order_par_sol([F|Fs],Var,A,Sol) :-
	first_order_par_sol1(F,Var,A,Sol1),
	first_order_par_sol(Fs,Var,A,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
first_order_par_sol1(factor([],C),_,An,Sol) :-
	general_form(An,A),
	normal_form(C/(1-A),Sol).
first_order_par_sol1(factor([Var],C),Var,An,Sol) :-
	general_form(An,A),
	normal_form((C/(1-A))*Var-(A*C)/exp(1-A,2),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	general_form(An,A),
	normal_form((C/(1-A))*exp(Var,2)-((2*A*C)/exp(1-A,2))*Var+
		(exp(A,2)*C+A*C)/exp(1-A,3),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =\= D,
	normal_form((C*D/(D-A))*exp(D,Var),Sol).
first_order_par_sol1(factor([exp(E1,E2)],C),Var,An,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	general_form(An,A),
	A =:= D,
	normal_form(C*Var*exp(D,Var),Sol).

%
%
first_order_gen_sol(Var,Ivalue,An,PSol,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	general_form(PSol,PSol1),
	general_form(Iindex,Iindex1),
	substitute(PSol1,Var,Iindex1,PSol2),
	normal_form(PSol2,PSol3),
	subtract_expr(Ival,PSol3,N),
	exp_expr(An,Iindex,D),
	divide_expr(N,D,C),
	normal_form(Var,Var1),
	exp_expr(An,Var1,G1),
	multiply_expr(C,G1,G2),
	add_expr(G2,PSol,Sol).

%
%  Solve a linear first-order variable coefficient difference equation.
%
solve_fovde(Var,Ivalue,An,Bn,Sol) :-
	Ivalue = [val(Iindex,Ival)],
	substitute(An,Var,$(i),AN),
	substitute(Bn,Var,$(j),BN),
	normal_form(1,One),
	add_expr(Iindex,One,Lower1),
	normal_form(Var,Upper),
	prod_expr($(i),Lower1,Upper,AN,P1),
	multiply_expr(Ival,P1,S1),
	normal_form(($(j))+1,Lower2),
	prod_expr($(i),Lower2,Upper,AN,P2),
	multiply_expr(BN,P2,P3),
	sum_expr($(j),Lower1,Upper,P3,S2),
	add_expr(S1,S2,Sol).

%
%  higher_order.pl 
%
%    Handle higher-order linear difference equation.
%

%
%  Test if a difference equation is linear higher-order.
%
higher_order_diff_equ(Equ,Var,F/_,A,_,Bn) :-
	Equ = expr([term([Dvar],An)],Bn),
	userfunc(Dvar),
	functor(Dvar,F,1),
	arg(1,Dvar,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	integer(I),
	I < 0,
	A is -I,
	An = [factor([],1)].

%
%  Solve a linear higher-order difference equation.
%
solve_hode(Var,Ivalue,A,B,Sol) :-
	Ivalue = [val(Iindex,_)],
	general_form(Iindex,0),!,
	solve_hovde(Var,Ivalue,A,B,Sol).
solve_hode(_,_,_,_,inf).

%
%  Solve a linear first-order variable coefficient difference equation.
%
solve_hovde(Var,Ivalue,An,Bn,Sol) :-
	Ivalue = [val(_,Ival)],
	write(Ival),nl,
	B = expr([],Bn),
	general_form(B,GB),
	substitute(GB,Var,(An*($(i))),BN),
	write(BN),nl,
	normal_form(BN,NN),
	normal_form(1,One),
	normal_form((Var/An),Upper),
	sum_expr($(i),One,Upper,NN,S),
	write(S),nl,
	add_expr(Ival,S,Sol).

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_isde(Var,Ivalue,Bn,Sol) :-
	normal_form(0,Zero),
	normal_form(1,One),
	Ivalue = [val(Iindex,Ival)],
	(Iindex == Zero; Iindex == One),
	general_form(Ival,GIval),
	mutual_struct_solvable(Bn,Var,GIval),!,
	solve_isde1(Bn,Var,Sol).
solve_isde(_,_,_,inf).

solve_isde1([],_,Zero) :-
	normal_form(0,Zero).
solve_isde1([B|Bn],Var,Sol) :-
	solve_isde2(B,Var,Sol1),
	solve_isde1(Bn,Var,Sols),
	add_expr(Sol1,Sols,Sol).

solve_isde2(factor([],C),Var,Sol) :-
	normal_form(C*Var,Sol).
solve_isde2(factor([Var],C),Var,Sol) :-
	normal_form((C/2)*Var*(Var+1),Sol).
solve_isde2(factor([arity(E)],C),Var,Sol) :-
	normal_form(Var,E),
	normal_form(C*(Var-1),Sol).

%
%  Test if a mutual size difference equation is linear first-order with coeff 1.
%
implicit_size_diff_equ(Equ,Var,F/_,_,_,Bn) :-
	Equ = expr(Term,Bn),
	NTerm = expr(Term,[]),
	general_form(NTerm,GTerm),
	GTerm = sum(Index,1,arity(Var),Expr),
	functor(Expr,F,1),
	arg(1,Expr,arg(Var,Index)).

%
%
mutual_struct_solvable([],_,_).
mutual_struct_solvable([B|Bn],Var,GIval) :-
	mutual_struct_solvable1(B,Var,GIval),
	mutual_struct_solvable(Bn,Var,GIval).

mutual_struct_solvable1(factor([],Val),_,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([Var],Val),Var,GIval) :-
	Val >= GIval.
mutual_struct_solvable1(factor([arity(Expr)],_),Var,_) :-
	normal_form(Var,Expr).
%
%  Test if a difference equation based on measure term-size on a list
%  is linear ``first-order'' with coefficient 1. That is,
%  f(n) = f(head(n)) + f(tail(n)) + g(n), where
%  g(n) is either d, d*n, head(n), or tail(n).
%
list_size_diff_equ(Equ,Var,F/_,_,_,Bn) :-
	Equ = expr(Term,Bn),
	mutual_list_term(Term,Var,F).

mutual_list_term([Term1,Term2],Var,Pred) :-
	mutual_list_head(Term2,Var,Pred),
	mutual_list_tail(Term1,Var,Pred).

mutual_list_head(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,head(Var)).

mutual_list_tail(term([P],[factor([],1)]),Var,Pred) :-
	userfunc(P), functor(P,Pred,1), arg(1,P,Arg),
	general_form(Arg,tail(Var)).

%
%  Solve a linear first-order difference equation with coefficient 1 based
%  on measure term-size on a list.
%
solve_lsde(Var,Ivalue,Bn,Sol) :- 
	mutual_list_solvable(Bn,Var),
	normal_form(1,One), Ivalue = [val(One,Ival)],
	general_form(Ival,GIval),!,
	solve_lsde1(Var,Bn,GIval,Sol).
solve_lsde(_,_,_,inf).

solve_lsde1(Var,[],GIval,Sol) :- normal_form(max(1,GIval)*Var,Sol).
solve_lsde1(Var,Bn,GIval,Sol) :- Bn \== [], solve_lsde2(Bn,Var,GIval,Sol).

solve_lsde2([],_,_,Zero) :- normal_form(0,Zero).
solve_lsde2([B|Bn],Var,GIval,Sol) :-
	solve_lsde3(B,Var,GIval,Sol1),
	solve_lsde2(Bn,Var,GIval,Sols),
	add_expr(Sol1,Sols,Sol).

solve_lsde3(factor([],C),Var,GIval,Sol) :- normal_form(max(C,GIval)*Var,Sol).
solve_lsde3(factor([Var],C),Var,GIval,Sol) :- 
	normal_form((max(C,GIval)/2)*Var*(Var+1),Sol).
solve_lsde3(factor([head(E)],C),Var,GIval,Sol) :-
	normal_form(Var,E), normal_form(max(C,GIval)*exp(Var-1,2)/2,Sol).
solve_lsde3(factor([tail(E)],C),Var,GIval,Sol) :-
	normal_form(Var,E), normal_form(max(C,GIval)*exp(Var-1,2)/2,Sol).

%
%
mutual_list_solvable([],_).
mutual_list_solvable([B|Bn],Var) :-
	mutual_list_solvable1(B,Var),
	mutual_list_solvable(Bn,Var).

mutual_list_solvable1(factor([],_),_).	% d
mutual_list_solvable1(factor([Var],_),Var).	%d*n
mutual_list_solvable1(factor([head(Expr)],_),Var) :- normal_form(Var,Expr).
mutual_list_solvable1(factor([tail(Expr)],_),Var) :- normal_form(Var,Expr).

%
%  Solve a mutual linear first-order difference equation with coeff 1.
%
solve_msde(Var,Ivalue,An,Bn,Sol) :-
	normal_form(0,Zero),
	Ivalue = [val(Zero,_)],!,
	normal_form(1,A),
	solve_fovde(Var,Ivalue,A,Bn,Sol1),
	substitute(An,Var,$(i),AN),
	normal_form(Var,Upper),
	sum_expr($(i),A,Upper,AN,Sol2),
	add_expr(Sol1,Sol2,Sol).
solve_msde(_,_,_,_,inf).

%
%  Test if a difference equation, obtained from an auxiliary predicate
%  based on measure term-size and builtins functor/3 and arg/3, is 
%  linear first-order with coefficient 1. That is,
%  f(n) = f(n-1) + g(arg(x,n)).
%
mutual_size_diff_equ(Equ,Var,Pred,A,_,B) :-
	Equ = expr(Terms,Bn),
	primary_term(Terms,Pred,Var,MTerm),
	mutual_size_solvable(Bn,Var),
	A = expr([MTerm],[]),
	B = expr([],Bn).

%
%
primary_term([T1,T2],Pred,Var,T2) :-
	primary_term(T1,Pred,Var),
	nonprimary_term(T2).
primary_term([T1,T2],Pred,Var,T1) :-
	primary_term(T2,Pred,Var),
	nonprimary_term(T1).

primary_term(term([P],[factor([],1)]),Pred,Var) :-
	userfunc(P),
	functor(P,F,1),
	Pred = F/_,
	arg(1,P,Arg),
	Arg = expr([],[factor([Var],1),factor([],I)]),
	I =:= -1.
nonprimary_term(term([P],[factor([],1)])) :-
	userfunc(P),
	functor(P,_,N),
	N =\= 1.

%
%
mutual_size_solvable([],_).
mutual_size_solvable([factor(I,_)|F],Var) :-
	const_coeff_solvable(I,Var),
	mutual_size_solvable(F,Var).
mutual_size_solvable([factor(I,_)|F],Var) :-
	I = [arg(E1,E2)],
	general_form(E1,Var1),
	variable(Var1),
	normal_form(Var,E2),
	mutual_size_solvable(F,Var).
%
%  product.pl 
%
%    Handle nonlinear difference equations in the form of a product.
%

%
%  Test if a difference equation is linear first-order.
%
product_diff_equ(Equ,Pred,NEqu) :-
	Equ = expr([term(Term,Factor)],[]),
	Factor = [factor([],_)],
	product_term(Term,Pred,NTerm),
	NEqu = expr(NTerm,Factor).

product_term(Term,Pred,NTerm) :-
	length(Term,Len), Len > 1,
	product_term(Term,Pred,0,NTerm).

product_term([],_,Count,[]) :- Count > 0.	% direct difference equation
product_term([Dvar|Term],F/N,Count,[term([Dvar],[factor([],1)])|NT]) :-
	userfunc(Dvar),
	functor(Dvar,F,1),!,		% 1-index reducible only
	Count1 is Count+1,
	product_term(Term,F/N,Count1,NT).
product_term([Dvar|Term],F/N,Count,[term([Dvar],[factor([],1)])|NT]) :-
	userfunc(Dvar),
	functor(Dvar,F1,_), F \== F1,!,
	product_term(Term,F/N,Count,NT).

%
%
log_base_equs([],[]).
log_base_equs([equ(N,I,E)|BE],[equ(N,I,NE)|NBE]) :-
	normal_form(2,Two),
	log_expr(Two,E,NE),
	log_base_equs(BE,NBE).

%
%
exp_solution(Sol,NSol) :-
	normal_form(2,Two),
	exp_expr(Two,Sol,NSol).
%
%  second_order.pl
%
%    Handle linear second-order difference equations

%
%  Test if a difference equation is linear second-order.
%
second_order_diff_equ(Equ,Var,F/_,A1,A2,Bn) :-	% f(n) = f(n-1)+f(n-2)
	Equ = expr([term([Dvar1],A1),term([Dvar2],A2)],Bn),!,
	userfunc(Dvar1),
	functor(Dvar1,F,1),
	arg(1,Dvar1,Arg1),
	Arg1 = expr([],[factor([Var],1),factor([],I1)]),
	I1 =:= -1,
	userfunc(Dvar2),
	functor(Dvar2,F,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	I2 =:= -2.
second_order_diff_equ(Equ,Var,F/_,A1,A2,Bn) :-	% f(n) = f(n-2)
	Equ = expr([term([Dvar2],A2)],Bn),
	userfunc(Dvar2),
	functor(Dvar2,F,1),
	arg(1,Dvar2,Arg2),
	Arg2 = expr([],[factor([Var],1),factor([],I2)]),
	I2 =:= -2,
	A1 = [factor([],0)].

%
%  Test if a difference equation is linear second-order with constant 
%  coefficient.
%
second_order_con_diff_equ(Var,Ivalue,A1,A2,B,R1,R2,A1n,A2n,Bn) :-
	A1 = [factor([],A1n)],
	A2 = [factor([],A2n)],
	real_quadratic_roots(A1n,A2n,R1,R2),
	const_coeff_solvable(B,Var),
	Bn = expr([],B),
	Ivalue = [val(Iindex1,_),val(Iindex2,_)],
	general_form(Iindex1,Index1),
	integer(Index1),
	general_form(Iindex2,Index2),
	integer(Index2),
	(Index1 > Index2 ->
		D is Index1-Index2;
		D is Index2-Index1),
	D =:= 1.

%
%  Solve a linear second-order difference equation.
%
solve_sode(Var,Ivalue,A1,A2,B,Sol) :-
	second_order_con_diff_equ(Var,Ivalue,A1,A2,B,R1,R2,A1n,A2n,Bn),!,
	solve_socde(Var,Ivalue,R1,R2,A1n,A2n,Bn,Sol).
solve_sode(_,_,_,_,_,inf).

%
%  Solve a linear second-order constant coefficient difference equation.
%
solve_socde(Var,Ivalue,R1,R2,A1,A2,Bn,GSol) :-
	Bn = expr([],BN),
	second_order_par_sol(BN,Var,R1,R2,A1,A2,PSol),
	second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol).

%
%
second_order_par_sol([],_,_,_,_,_,Sol) :-
	normal_form(0,Sol).
second_order_par_sol([F|Fs],Var,R1,R2,A1,A2,Sol) :-
	second_order_par_sol1(F,Var,R1,R2,A1,A2,Sol1),
	second_order_par_sol(Fs,Var,R1,R2,A1,A2,Sols),
	add_expr(Sol1,Sols,Sol).

%
%
second_order_par_sol1(factor([],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
		normal_form(C/(1-A1-A2),Sol);
		(U =:= 1 ->
			normal_form((C/(A1+2*A2))*Var,Sol);
			normal_form((C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([Var],C),Var,R1,R2,A1,A2,Sol) :-
	unit_count(R1,R2,U),
	(U =:= 0 ->
/*
		(normal_form(C/(1-A1-A2),D1),
		 normal_form(-(A1+2*A2)/(1-A1-A2),D2),
		 multiply_expr(D1,D2,D0),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E1,D0,Sol));
*/
		normal_form((C/(1-A1-A2))*Var+
			    (-C*(A1+2*A2)/exp(1-A1-A2,2)),Sol);
		(U =:= 1 ->
			normal_form((C/(2*(A1+2*A2)))*exp(Var,2)+
			     ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*Var,Sol);
			normal_form((C/6)*exp(Var,3)+(C/2)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	normal_form(Var,E1),
	normal_form(2,E2),
	unit_count(R1,R2,U),
	(U =:= 0 ->
		(normal_form(C/(1-A1-A2),D2),
		 normal_form(-2*(A1+2*A2)/(1-A1-A2),TD1),
		 multiply_expr(TD1,D2,D1),
		 normal_form((A1+2*A2)/(1-A1-A2),TD01),
		 multiply_expr(TD01,D1,TD03),
		 normal_form((A1+4*A2)/(1-A1-A2),TD02),
		 multiply_expr(TD02,D2,TD04),
		 subtract_expr(TD04,TD03,D0),
		 normal_form(exp(Var,2),Var2),
		 multiply_expr(D2,Var2,E2),
		 normal_form(Var,Var1),
		 multiply_expr(D1,Var1,E1),
		 add_expr(E2,E1,Sol1),
		 add_expr(Sol1,D0,Sol));
		(U =:= 1 ->
			normal_form((C/(3*(A1+2*A2)))*exp(Var,3)+
			   ((C*(A1+4*A2))/(2*exp(A1+2*A2,2)))*exp(Var,2)+
			   ((C*(A1*A1+4*A1*A2+16*A2*A2))/(6*exp(A1+2*A2,3)))*
				Var,Sol);
			normal_form((C/12)*exp(Var,4)+(C/3)*exp(Var,3)+
			   (5*C/12)*exp(Var,2),Sol))).
second_order_par_sol1(factor([exp(E1,E2)],C),Var,R1,R2,A1,A2,Sol) :-
	E1 = expr([],[factor([],D)]),
	normal_form(Var,E2),
	R1 =\= D,
	R2 =\= D,
	normal_form(((C*exp(D,2))/(exp(D,2)-A1*D-A2))*exp(D,Var),Sol).

%
%
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =\= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = exp(R2,Index2)*exp(R1,Index1)-exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*exp(R2,Var)+GPSol,GSol).
second_order_gen_sol(Var,Ivalue,R1,R2,PSol,GSol) :-
	R1 =:= R2,
	Ivalue = [val(Iindex1,Ival1),val(Iindex2,Ival2)],
	general_form(PSol,GPSol),
	general_form(Iindex1,Index1),
	general_form(Ival1,Val1),
	substitute(GPSol,Var,Index1,GPSol1),
	general_form(Iindex2,Index2),
	general_form(Ival2,Val2),
	substitute(GPSol,Var,Index2,GPSol2),
	N2 = exp(R1,Index1)*(Val2-GPSol2)-exp(R1,Index2)*(Val1-GPSol1),
	D2 = Index2*exp(R2,Index2)*exp(R1,Index1)-
	     Index1*exp(R2,Index1)*exp(R1,Index2),
	C2 = N2/D2,
	C1 = (Val1-GPSol1-(C2*Index1*exp(R2,Index1)))/exp(R1,Index1),
	normal_form(C1*exp(R1,Var)+C2*Var*exp(R2,Var)+GPSol,GSol).
%
%  callgraph.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for handling the call graph.
%  
%  The structure of the call graph:
%	cg(Pred/Arity,edge,number,component).
%

%
%  Insert an entry Entry for predicate Pred in call graph CG.
%  If it is already in, return the entry as Entry; 
%  otherwise an entry is inserted and returned.
%
insert_call_entry(CG,Pred,Entry) :-
	var(CG),
	Entry = cg(Pred,_,_,_),
	CG = [Entry|_].
insert_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [Entry|_],
	Entry = cg(Pred,_,_,_).
insert_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [E|G],
	E \== cg(Pred,_,_,_),
	insert_call_entry(G,Pred,Entry).

%
%  Insert a literal into the edge field of the call graph.
%
insert_call_field(CG,Pred,edge,Lit) :-
	insert_call_entry(CG,Pred,cg(Pred,EdgeList,_,_)),
	insert_call_edge(EdgeList,Lit).

insert_call_edge(EdgeList,Lit) :-
	var(EdgeList),
	EdgeList = [Lit|_].
insert_call_edge(EdgeList,Lit) :-
	nonvar(EdgeList),
	EdgeList = [Lit|_].
insert_call_edge(EdgeList,Lit) :-
	nonvar(EdgeList),
	EdgeList = [E|Edge],
	E \== Lit,
	insert_call_edge(Edge,Lit).

%
%  Insert a DFS number into the number field of the call graph.
%
insert_call_field(CG,Pred,number,DFSNumber) :-
	insert_call_entry(CG,Pred,cg(Pred,_,DFSNumber,_)).

%
%  Insert a component number into the component field of the call graph.
%
insert_call_field(CG,Pred,component,Component) :-
	insert_call_entry(CG,Pred,cg(Pred,_,_,Component)).

%
%  Find the entry for predicate Pred in call graph.
%  If it is in, return the entry as Entry; 
%  otherwise Entry is returned as a variable.
%
find_call_entry(CG,_,_) :-
	var(CG).
find_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [Entry|_],
	Entry = cg(Pred,_,_,_).
find_call_entry(CG,Pred,Entry) :-
	nonvar(CG),
	CG = [E|G],
	E \== cg(Pred,_,_,_),
	find_call_entry(G,Pred,Entry).

%
%  Find a field for the predicate Pred in call graph.
%
find_call_field(CG,Pred,edge,EdgeList) :-
	find_call_entry(CG,Pred,cg(Pred,EdgeList,_,_)).
find_call_field(CG,Pred,number,DFSNumber) :-
	find_call_entry(CG,Pred,cg(Pred,_,DFSNumber,_)).
find_call_field(CG,Pred,component,Component) :-
	find_call_entry(CG,Pred,cg(Pred,_,_,Component)).

%
%  Find an entry in call graph which has not instantiated its number field.
%  If there is such an entry, return the name of the predicate as Pred;
%  otherwise Pred is returned as a variable.
%
find_call_vertex(CG,_) :-
	var(CG).
find_call_vertex(CG,Pred) :-
	nonvar(CG),
	CG = [cg(Pred,_,DFSNumber,_)|_],
	var(DFSNumber).
find_call_vertex(CG,Pred) :-
	nonvar(CG),
	CG = [cg(_,_,DFSNumber,_)|G],
	nonvar(DFSNumber),
	find_call_vertex(G,Pred).

%
%  Print out the call graph.
%
print_call_graph(CG) :-
	tell(call_graph),
	p_call_graph(CG),
	told.

p_call_graph(CG) :-
	var(CG).
p_call_graph(CG) :-
	nonvar(CG),
	CG = [E|G],
	write(E),
	nl,
	p_call_graph(G).
%
%  initsystem.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for initializing the system.
%

%
%  Initialize the system by initializing the buildin table and
%  read in the source program, and building the program table,
%  symbol table, call graph and the strongly connected components
%  of the call graph.
%
init_system(Files,BT,ST,SCCG,Error) :-
	init_buildin_table(BT),
	read_program(Files,BT,PT,ST,Error1),
	call_graph(PT,BT,ST,CG,Error2),
	Error is Error1*Error2,
	(Error =:= 1 ->
		strongly_connected_component(CG,SCCG);
		true).


%
%  Read in the source program and store the clauses in the program table,
%  symbol table and call graph.
%
read_program(Files,BT,PT,ST,Error) :-
	read_program(Files,BT,PT,[],ST,Error).

read_program([],_,PT,PT,_,1).
read_program([File|Fs],BT,PT,NPT,ST,Error) :-
	see(File),
	r_program(BT,PT,PT1,ST,Error1),
	seen,
	read_program(Fs,BT,PT1,NPT,ST,Error2),
	Error is Error1*Error2.

r_program(BT,PT,NPT,ST,Error) :-
	read(Clause),
	(Clause \== end_of_file -> 
		(PT=[Clause|PT1], 
		 clause_type(Clause,Type),
		 insert_symbol_table(Type,ST,Clause,Error1),
		 r_program(BT,PT1,NPT,ST,Error2),
		 Error is Error1*Error2);
		(NPT = PT,
		 Error = 1)).
%
%  Classify the type of a clause.
%    Declaration -- 1	
%    Rule        -- 2
%    Fact        -- 3
%
clause_type(Clause,Type) :-
	functor(Clause,F,N),
	clause_type(F,N,Type).

clause_type((:-),1,1).
clause_type((:-),2,2).
clause_type(F,_,3) :-
	F \== (:-).

% 
%  The following procedures insert the clauses into the symbol table.
% 

%
%  Insert a clause into the symbol table.
%
insert_symbol_table(1,ST,Clause,Error) :-
	insert_symbol_dec(ST,Clause,Error).
insert_symbol_table(2,ST,Clause,1) :-
	insert_symbol_rule(ST,Clause).
insert_symbol_table(3,ST,Clause,1) :-
	insert_symbol_fact(ST,Clause).

%
%  Insert a rule into the symbol table.
%
insert_symbol_rule(ST,Rule) :-
	arg(1,Rule,Head),
	functor(Head,F,N),
	insert_symbol_field(ST,F/N,clause,Rule).

%
%  Insert a fact into the symbol table.
%
insert_symbol_fact(ST,Fact) :-
	functor(Fact,F,N),
	insert_symbol_field(ST,F/N,clause,Fact).

%  
%  The following procedures insert the clauses into the call graph.
% 

%
%  Build the call graph of the program.
%
call_graph([],_,_,_,1).
call_graph([Clause|PT],BT,ST,CG,Error) :-
	clause_type(Clause,Type),
	insert_call_graph(Type,BT,ST,CG,Clause,Error1),
	call_graph(PT,BT,ST,CG,Error2),
	Error is Error1*Error2.

%
%
%  Insert a clause into the call graph.
%
insert_call_graph(1,_,_,_,_,1).
insert_call_graph(2,BT,ST,CG,Clause,Error) :-
	arg(1,Clause,Head),
	arg(2,Clause,Body),
	functor(Head,F,N),
	insert_call_entry(CG,F/N,_),
	insert_call_body(BT,ST,CG,F/N,Body,Clause,Error).
insert_call_graph(3,_,_,CG,Clause,1) :-
	functor(Clause,F,N),
	insert_call_entry(CG,F/N,_).

%
%  Insert the literals in the body of a rule into the call graph.
%
insert_call_body(BT,ST,CG,Pred,(Lit,Body),Clause,Error) :-
	insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error1),
	insert_call_body(BT,ST,CG,Pred,Body,Clause,Error2),
	Error is Error1*Error2.
insert_call_body(BT,ST,CG,Pred,Lit,Clause,Error) :-
	nonsequence(Lit),
	insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error).

%
%  Insert a non-builtin non-recursive literal into the call graph.
%
insert_call_lit(_,_,_,Pred,Lit,_,1) :-
	functor(Lit,F,A), F/A == Pred, !.
insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error) :-
	functor(Lit,F,A), F/A \== Pred,
	(second_order_predicate(F/A) ->
		insert_call_lit_1(BT,ST,CG,Pred,Lit,Clause,Error);
		insert_call_lit_2(BT,ST,CG,Pred,Lit,Clause,Error)).

insert_call_lit_1(BT,ST,CG,Pred,Lit,Clause,Error) :-
	second_order_predicate_pred_arg(Lit,L),
	insert_call_body(BT,ST,CG,Pred,L,Clause,Error).

insert_call_lit_2(BT,ST,CG,Pred,Lit,Clause,Error) :-
	functor(Lit,F,A),
	find_symbol_entry(BT,F/A,BTEntry),
	(var(BTEntry) ->
		(find_symbol_entry(ST,F/A,STEntry),
		 (var(STEntry) ->
			(error_message(lit1,F/A,Clause),
			 Error = 0);
			(insert_call_field(CG,Pred,edge,F/A),
			 Error = 1)));
		Error = 1).
%
%  scc.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for finding the strongly connected
%  components of the call graph.
%  These procedures are based on the algorithm in Udi Manber's book
%  "Introduction to Algorithms - A Creative Approach", page 231.
%

%
%  Find the strongly connected components of the call graph.
%
strongly_connected_component(CG,SCCG) :-
	strongly_connected_component(CG,SCCG,[],-1,_,0,_,[],_).

strongly_connected_component(CG,SCCG,NewSCCG,DFS_N,NewDFS_N,
			     Component,New_Component,Stack,NewStack) :-
	find_call_vertex(CG,V),
	(var(V) ->
		SCCG = NewSCCG;
		(scc(V,CG,SCCG,SCCG1,DFS_N,DFS_N1,Component,Component1,
		    Stack,Stack1,_),
		 strongly_connected_component(CG,SCCG1,NewSCCG,DFS_N1,NewDFS_N,
			     Component1,New_Component,Stack1,NewStack))).

%
%  Investigate a vertex.
%
scc(V,CG,SCCG,NewSCCG,DFS_N,NewDFS_N,Component,NewComponent,Stack,NewStack,
    VHigh) :-
	insert_call_field(CG,V,number,DFS_N),
	DFS_N1 is DFS_N-1,
	push(Stack,V,Stack1),
	find_call_field(CG,V,edge,EdgeList),
	scc_edges(EdgeList,V,DFS_N,VHigh,CG,SCCG,SCCG1,DFS_N1,NewDFS_N,
		  Component,Component1,Stack1,Stack2),
	generate_new_component(VHigh,DFS_N,V,CG,SCCG1,NewSCCG,Component1,
			       NewComponent,Stack2,NewStack).

%
%  Investigate all the edges out of a vertex.
%
scc_edges(EdgeList,_,VHigh,VHigh,_,SCCG,SCCG,DFS_N,DFS_N,
	  Component,Component,Stack,Stack) :-
	var(EdgeList).
scc_edges(EdgeList,V,VHigh,NewVHigh,CG,SCCG,NewSCCG,DFS_N,NewDFS_N,
	  Component,NewComponent,Stack,NewStack) :-
	nonvar(EdgeList),
	EdgeList = [W|Edge],
	find_call_field(CG,W,number,WDFSNumber),
	find_call_field(CG,W,component,WComponent),
	scc_edge(W,WDFSNumber,WComponent,VHigh,VHigh1,CG,SCCG,SCCG1,
		 DFS_N,DFS_N1,Component,Component1,Stack,Stack1),
	scc_edges(Edge,V,VHigh1,NewVHigh,CG,SCCG1,NewSCCG,DFS_N1,NewDFS_N,
	  	  Component1,NewComponent,Stack1,NewStack).

%
%  Investigate a single edge out of a vertex.
%
scc_edge(W,WDFSNumber,_,VHigh,NewVHigh,CG,SCCG,NewSCCG,DFS_N,NewDFS_N,
	 Component,NewComponent,Stack,NewStack) :-
	var(WDFSNumber),
	scc(W,CG,SCCG,NewSCCG,DFS_N,NewDFS_N,Component,NewComponent,
	    Stack,NewStack,WHigh),
	max(VHigh,WHigh,NewVHigh).
scc_edge(_,WDFSNumber,WComponent,VHigh,NewVHigh,_,SCCG,SCCG,DFS_N,DFS_N,
	 Component,Component,Stack,Stack) :-
	nonvar(WDFSNumber),
	var(WComponent),
	max(VHigh,WDFSNumber,NewVHigh).
scc_edge(_,WDFSNumber,WComponent,VHigh,VHigh,_,SCCG,SCCG,DFS_N,DFS_N,
	 Component,Component,Stack,Stack) :-
	nonvar(WDFSNumber),
	nonvar(WComponent).

%
%  Generate a new component.
%
generate_new_component(VHigh,VDFSNumber,_,_,SCCG,SCCG,Component,Component,
		       Stack,Stack) :-
	VHigh =\= VDFSNumber.
generate_new_component(VHigh,VHigh,V,CG,[G|SCCG],SCCG,Component,
		       NewComponent,Stack,NewStack) :-
	NewComponent is Component+1,
	gen_new_component(V,CG,G,NewComponent,Stack,NewStack).

%
%  Collect the elements in the component by popping them
%  out of the stack.
%
gen_new_component(V,CG,G,Component,Stack,NewStack) :-
	pop(Stack,Element,Stack1),
	insert_call_field(CG,Element,component,Component),
	G = [Element|G1],
	(Element == V ->
		(NewStack = Stack1,
		 G1 = []);
		gen_new_component(V,CG,G1,Component,Stack1,NewStack)).

%
%  Print out the strongly connected components of the call graph.
%
print_scc_list(SCCG) :-
	tell(scc_list),
	p_scc_list(SCCG),
	told.

p_scc_list([]).
p_scc_list([Component|SCCG]) :-
	write(Component),
	nl,
	p_scc_list(SCCG).
%
%  symtable.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for handling the symbol table.
%
%  The structure of the symbol table:
%	st(Pred/Arity,clause,mode,measure,mutex,det,size,solution,time)
%

%
%  Insert an entry for predicate Pred in the symbole table.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Pred is inserted and returned.
%
insert_symbol_entry(ST,Pred,Entry) :- 
	var(ST),
	Entry = st(Pred,_,_,_,_,_,_,_,_,_),
	ST = [Entry|_].
insert_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_,_,_,_,_).
insert_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E \== st(Pred,_,_,_,_,_,_,_,_,_),
	insert_symbol_entry(S,Pred,Entry).

%
%  Insert a clause for predicate Pred into the symbol table.
%
insert_symbol_field(ST,Pred,clause,Clause) :-
	insert_symbol_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_,_,_,_)),
	insert_symbol_clause(ClauseList,Clause).

insert_symbol_clause(ClauseList,Clause) :-
	var(ClauseList),
	ClauseList = [Clause|_].
insert_symbol_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|_],
	C == Clause.
insert_symbol_clause(ClauseList,Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|CList],
	C \== Clause,
	insert_symbol_clause(CList,Clause).

%
%  Insert a declaration for predicate Pred into the symbol table.
%
insert_symbol_field(ST,Pred,(mode),Mode) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,Mode,_,_,_,_,_,_,_)).
insert_symbol_field(ST,Pred,measure,Measure) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,Measure,_,_,_,_,_,_)).
insert_symbol_field(ST,Pred,mutex,Mutex) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,Mutex,_,_,_,_,_)).
insert_symbol_field(ST,Pred,det,Det) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,Det,_,_,_,_)).
insert_symbol_field(ST,Pred,size,Size) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,Size,_,_,_)).
insert_symbol_field(ST,Pred,relation,Solution) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,Solution,_,_)).
insert_symbol_field(ST,Pred,time,Time) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,Time,_)).
insert_symbol_field(ST,Pred,domain,Domain) :-
	insert_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Find the entry for predicate Pred in the symbole table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_symbol_entry(ST,_,_) :- 
	var(ST).
find_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred,_,_,_,_,_,_,_,_,_),!.
find_symbol_entry(ST,Pred,Entry) :- 
	nonvar(ST),
	ST = [E|S],
	E = st(Pred1,_,_,_,_,_,_,_,_,_),
	Pred \== Pred1,
	find_symbol_entry(S,Pred,Entry).

%
%  Find a field for predicate Pred in symbol table.
%
find_symbol_field(ST,Pred,clause,ClauseList) :-
	find_symbol_entry(ST,Pred,st(Pred,ClauseList,_,_,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,(mode),Mode) :-
	find_symbol_entry(ST,Pred,st(Pred,_,Mode,_,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,measure,Measure) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,Measure,_,_,_,_,_,_)).
find_symbol_field(ST,Pred,mutex,Mutex) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,Mutex,_,_,_,_,_)).
find_symbol_field(ST,Pred,det,Det) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,Det,_,_,_,_)).
find_symbol_field(ST,Pred,size,Size) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,Size,_,_,_)).
find_symbol_field(ST,Pred,relation,Solution) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,Solution,_,_)).
find_symbol_field(ST,Pred,time,Time) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,Time,_)).
find_symbol_field(ST,Pred,domain,Domain) :-
	find_symbol_entry(ST,Pred,st(Pred,_,_,_,_,_,_,_,_,Domain)).

%
%  Get a property of a literal from symbol table.
%
literal_property(BT,ST,Pred,PropName,Property) :-
	find_symbol_field(BT,Pred,PropName,Prop1),
	(var(Prop1) ->
		find_symbol_field(ST,Pred,PropName,Property);
		Property = Prop1).

%
%  Print out the symbol table.
%
print_symbol_table(ST) :-
	tell(symbol_table),
	p_symbol_table(ST),
	told.

p_symbol_table(ST) :-
	var(ST).
p_symbol_table(ST) :-
	nonvar(ST),
	ST = [E|S],
	write(E),
	nl,
	p_symbol_table(S).

%
%  dec.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for handling the declarations.
%

%
%  Insert a declaration into the symbol table.
%
insert_symbol_dec(ST,Clause,Error) :-
	arg(1,Clause,Dec),
	functor(Dec,F,A),
	arg(1,Dec,Pred),
	(A > 1 ->
		arg(2,Dec,DecList);
		DecList = []),
	legal_declaration(F/A,Pred,DecList,ST,Clause,Error).

%
%  Test if a declaration is legal.
%
legal_declaration((mode)/2,F/N,Mode,ST,Clause,Error) :-
	(legal_mode_dec(Mode,N,Clause) ->
		(insert_symbol_field(ST,F/N,(mode),Mode),
		 Error = 1);
		Error = 0).
legal_declaration(measure/2,F/N,Measure,ST,Clause,Error) :-
	(legal_measure_dec(Measure,N,Clause) ->
		(insert_symbol_field(ST,F/N,measure,Measure),
		 Error = 1);
		Error = 0).
/*
legal_declaration(mutex/2,F/N,Mutex,ST,Clause,Error) :-
	(legal_mutex_dec(Mutex,N,Clause) ->
		(insert_symbol_field(ST,F/N,mutex,Mutex),
		 Error = 1);
		Error = 0).

legal_declaration(size/2,F/N,Size,ST,Clause,Error) :-
	(legal_size_dec(Size,N,Clause) ->
		(in_comp_dec(Size,NSize,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,size,NSize);
			error_message(dec8,Size,Clause)));
		Error = 0).
*/
legal_declaration(det/1,F/N,_,ST,_,Error) :-
	Error = 1,
	insert_symbol_field(ST,F/N,det,[1]).

/*
	(legal_det_dec(Det,N,Clause) ->
		(in_comp_dec(Det,NDet,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,det,[NDet]);
			error_message(dec8,Det,Clause)));
		Error = 0).
*/

/*
legal_declaration(time/2,F/N,Time,ST,Clause,Error) :-
	(legal_time_dec(Time,N,Clause) ->
		(in_comp_dec(Time,NTime,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,time,[NTime]);
			error_message(dec8,Time,Clause)));
		Error = 0).
*/
legal_declaration((domain)/2,F/N,Domain,ST,Clause,Error) :-
	(legal_domain_dec(Domain,N,Clause) ->
		(insert_symbol_field(ST,F/N,domain,Domain),
		 Error = 1);
		Error = 0).
legal_declaration(Dec,_,_,_,Clause,0) :-
	Dec \== (mode)/2,
	Dec \== measure/2,
%	Dec \== mutex/2,
%	Dec \== size/2,
	Dec \== det/1,
%	Dec \== time/2,
	Dec \== domain/2,
	error_message(dec2,Dec,Clause).

%
%  Detect the mode declaration error.
%
legal_mode_dec(Mode,N,Clause) :-
	list(Mode),
	legal_mode_symbol(Mode,N,Clause).
legal_mode_dec(Mode,_,Clause) :-
	nonlist(Mode),
	error_message(mode1,_,Clause),
	fail.

legal_mode_symbol([],0,_).
legal_mode_symbol([],N,Clause) :-
	N =\= 0,
	error_message(mode2,_,Clause),
	fail.
legal_mode_symbol([_|_],0,Clause) :-
	error_message(mode2,_,Clause),
	fail.
legal_mode_symbol([(+)|Mode],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_mode_symbol(Mode,N1,Clause).
legal_mode_symbol([(-)|Mode],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_mode_symbol(Mode,N1,Clause).
legal_mode_symbol([M|_],_,Clause) :-
	M \== (+),
	M \== (-),
	error_message(mode3,M,Clause),
	fail.

%
%  Detect the measure declaration error.
%
legal_measure_dec(Measure,N,Clause) :-
	list(Measure),
	legal_measure_symbol(Measure,N,Clause).
legal_measure_dec(Measure,_,Clause) :-
	nonlist(Measure),
	error_message(measure1,_,Clause),
	fail.

legal_measure_symbol([],0,_).
legal_measure_symbol([],N,Clause) :-
	N =\= 0,
	error_message(measure2,_,Clause),
	fail.
legal_measure_symbol([_|_],0,Clause) :-
	error_message(measure2,_,Clause),
	fail.
legal_measure_symbol([void|Measure],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_measure_symbol(Measure,N1,Clause).
legal_measure_symbol([int|Measure],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_measure_symbol(Measure,N1,Clause).
legal_measure_symbol([length|Measure],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_measure_symbol(Measure,N1,Clause).
legal_measure_symbol([size|Measure],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_measure_symbol(Measure,N1,Clause).
legal_measure_symbol([depth([_|_])|Measure],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_measure_symbol(Measure,N1,Clause).
legal_measure_symbol([M|_],_,Clause) :-
	M \== void,
	M \== int,
	M \== length,
	M \== size,
	(M = depth([_|_]) ->
		fail;
		error_message(measure3,M,Clause)),
	fail.

%
%  Detect the mutex declaration error.
%
legal_mutex_dec(Mutex,_,Clause) :-
	list(Mutex),
	legal_mutex_dec1(Mutex,_,Clause).
legal_mutex_dec(Mutex,_,Clause) :-
	nonlist(Mutex),
	error_message(mutex1,_,Clause),
	fail.

legal_mutex_dec1([],_,_).
legal_mutex_dec1([M|Mutex],_,Clause) :-
	(list(M) ->
		(legal_mutex_dec2(M,_,Clause),
		 legal_mutex_dec1(Mutex,_,Clause));
		(error_message(mutex1,_,Clause),
		 fail)).

legal_mutex_dec2([],_,_).
legal_mutex_dec2([M|Ms],_,Clause) :-
	(integer(M) ->
		legal_mutex_dec2(Ms,_,Clause);
		(error_message(mutex2,_,Clause),
		 fail)).
%
legal_domain_dec(Domain,N,Clause) :-
	list(Domain),
	legal_domain_symbol(Domain,N,Clause).
legal_size_dec(Domain,_,Clause) :-
	nonlist(Domain),
	error_message(domain1,_,Clause),
	fail.

legal_domain_symbol([],0,_).
legal_domain_symbol([],N,Clause) :-
	N =\= 0,
	error_message(domain2,_,Clause),
	fail.
legal_domain_symbol([_|_],0,Clause) :-
	error_message(domain2,_,Clause),
	fail.
legal_domain_symbol([D|Domain],N,Clause) :-
	N > 0,
	(legal_domain_symbol1(D,N,Clause) ->
		(N1 is N-1,
		 legal_domain_symbol(Domain,N1,Clause));
		(error_message(domain3,_,Clause),
		 fail)).

legal_domain_symbol1((L-U),_,_) :-
	integer(L),
	integer(U),
	L =< U.
legal_domain_symbol1(D,_,_) :-
	list(D).

%
%  Detect the size declaration error.
%
legal_size_dec(Size,N,Clause) :-
	list(Size),
	legal_size_symbol(Size,N,Clause).
legal_size_dec(Size,_,Clause) :-
	nonlist(Size),
	error_message(size1,_,Clause),
	fail.

legal_size_symbol([],0,_).
legal_size_symbol([],N,Clause) :-
	N =\= 0,
	error_message(size2,_,Clause),
	fail.
legal_size_symbol([_|_],0,Clause) :-
	error_message(size2,_,Clause),
	fail.
legal_size_symbol([_|Size],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_size_symbol(Size,N1,Clause).

%
%  Detect the det declaration error.
%
legal_det_dec(Det,_,Clause) :-
	Det \== 1,
	error_message(det1,_,Clause),
	fail.
legal_det_dec(1,_,_).

%
%  Detect the time declaration error.
%
legal_time_dec(Time,_,Clause) :-
	list(Time),
	error_message(time1,_,Clause),
	fail.
legal_time_dec(Time,_,_) :-
	nonlist(Time).

%
%  Format the input complexity declaration.
%
input_comp_dec([],[],1).
input_comp_dec([Exp|EList],[NExp|NList],Error) :-
	in_comp_dec(Exp,NExp,Error1),
	input_comp_dec(EList,NList,Error2),
	Error is Error1*Error2.

in_comp_dec(Exp,Exp,0) :-
	var(Exp).
in_comp_dec(Exp,Exp,1) :-
	atomic(Exp).
in_comp_dec(Exp,NExp,Error) :-
	compound(Exp),
	functor(Exp,F,N),
	(F/N == ($)/1 ->
		(arg(1,Exp,Arg),
		 (integer(Arg) ->
			NExp =.. ['$',0,Arg];
			NExp = Exp),
		 Error = 1);
		(functor(NExp,F,N),
		 in_comp_dec(N,Exp,NExp,Error))).

in_comp_dec(0,_,_,1).
in_comp_dec(N,Exp,NExp,Error) :-
	N > 0,
	arg(N,Exp,Arg),
	in_comp_dec(Arg,NArg,Error1),
	arg(N,NExp,NArg),
	N1 is N-1,
	in_comp_dec(N1,Exp,NExp,Error2),
	Error is Error1*Error2.

%
%  Check if the mode declarations for the predicates of a SCC are declared.
%
mode_declared([],_,1).
mode_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	(var(Mode) -> 
		(error_message(dec3,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	mode_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the measure declarations for the predicates of a SCC are declared.
%
measure_declared([],_,1).
measure_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,measure,Measure),
	(var(Measure) -> 
		(error_message(dec4,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	measure_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the size declarations for the predicates of a SCC are declared.
%
size_declared([],_,1).
size_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,size,Size),
	(var(Size) -> 
		(error_message(dec5,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	size_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the det declarations for the predicates of a SCC are declared.
%
det_declared([],_,1).
det_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,det,Det),
	(var(Det) -> 
		(error_message(dec6,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	det_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the time declarations for the predicates of a SCC are declared.
%
time_declared([],_,1).
time_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,time,Time),
	(var(Time) -> 
		(error_message(dec7,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	time_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the mutex declarations for the predicates of a SCC are declared.
%
mutex_declared([],_,1).
mutex_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	(var(Mutex) -> 
		(find_symbol_field(ST,Pred,clause,Clauses),
		 default_mutex(Clauses,1,DMutex),
		 insert_symbol_field(ST,Pred,mutex,DMutex),
		 Error1 = 1);
		Error1 = 1),
	mutex_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
default_mutex(Clauses,_,[]) :-
	var(Clauses).
default_mutex(Clauses,N,[[N]|DMutex]) :-
	nonvar(Clauses),
	Clauses = [_|Cs],
	N1 is N+1,
	default_mutex(Cs,N1,DMutex).

%
%  Check if the program is well-declared.
%
analysis_check([],_,1).
analysis_check([Comp|CompList],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	analysis_check(CompList,ST,Error3),
	Error is Error1*Error2*Error3.

%
%  Check if the program is well-declared for size analysis.
%
size_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	Error is Error1*Error2.
size_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	size_analysis_check(CompList,ST,Error4),
	Error is Error1*Error2*Error3*Error4.

%
%  Check if the program is well-declared for solution analysis.
%
solution_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	mutex_declared(Comp,ST,Error4),
	Error is Error1*Error2*Error3*Error4.
solution_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	mutex_declared(Comp,ST,Error5),
	solution_analysis_check(CompList,ST,Error6),
	Error is Error1*Error2*Error3*Error4*Error5*Error6.

%
%  Check if the program is well-declared for time analysis.
%
time_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	mutex_declared(Comp,ST,Error5),
	Error is Error1*Error2*Error3*Error4*Error5.
time_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	time_declared(Comp,ST,Error5),
	mutex_declared(Comp,ST,Error6),
	time_analysis_check(CompList,ST,Error7),
	Error is Error1*Error2*Error3*Error4*Error5*Error6*Error7.
%
%  builtin.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for initializing builtin table.
%
%  The structure of the builtin table:
%	st(Pred/Arity,clause,mode,measure,mutex,det,size,solution,time,domain)
%

%
%  Initialize the buildin table.
%
init_buildin_table(BT) :-
	insert_symbol_entry(BT,(is)/2,st((is)/2,_,[-,+],[int,int],_,[1],[],
			    inf,[0],_)),
%	insert_symbol_entry(BT,(=)/2,st((=)/2,_,[(?),(?)],[(?),(?)],_,[1],[],
%			    inf,[0],_)),
	insert_symbol_entry(BT,functor/3,st(functor/3,_,[+,-,-],
			    [size,size,int],_,[1],[],inf,[0],_)),
	insert_symbol_entry(BT,arg/3,st(arg/3,_,[+,+,-],
			    [int,size,size],_,[1],[],inf,[0],_)),
	insert_symbol_entry(BT,functor1/3,st(functor1/3,_,[-,+,+],
			    [size,size,int],_,[1],[],inf,[0],_)),
	insert_symbol_entry(BT,arg/4,st(arg/4,_,[+,+,+,-],
			    [int,size,size,size],_,[1],[],inf,[0],_)),
%	insert_symbol_entry(BT,(=..)/2,st((=..)/2,_,[+,-],
%			    [size,length],_,[1],[],inf,[0],_)),
	insert_symbol_entry(BT,(==)/2,st((==)/2,_,[+,+],[(?),(?)],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,(\==)/2,st((\==)/2,_,[+,+],[(?),(?)],_,[1],_,
			    inf,[0],_)),
	insert_symbol_entry(BT,(=:=)/2,st((=:=)/2,_,[+,+],[int,int],_,[1],_,
			    inf,[0],_)),
	insert_symbol_entry(BT,(=\=)/2,st((=\=)/2,_,[+,+],[int,int],_,[1],_,
			    inf,[0],_)),
	insert_symbol_entry(BT,(<)/2,st((<)/2,_,[+,+],[int,int],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,(>)/2,st((>)/2,_,[+,+],[int,int],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,(=<)/2,st((=<)/2,_,[+,+],[int,int],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,(>=)/2,st((>=)/2,_,[+,+],[int,int],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,atomic/1,st(atomic/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,atom/1,st(atom/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,number/1,st(number/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,integer/1,st(integer/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,float/1,st(float/1,_,[+],[void],_,[1],_,inf,[0],_)),
%	insert_symbol_entry(BT,var/1,st(var/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,nonvar/1,st(nonvar/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,write/1,st(write/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,tab/1,st(tab/1,_,[+],[void],_,[1],_,inf,[0],_)),
	insert_symbol_entry(BT,nl/0,st(nl/0,_,[],[],_,[1],_,1,[0],_)),
	insert_symbol_entry(BT,fail/0,st(fail/0,_,[],[],_,[0],_,0,[0],_)),
	insert_symbol_entry(BT,true/0,st(true/0,_,[],[],_,[1],_,1,[0],_)),
	insert_symbol_entry(BT,(!)/0,st((!)/0,_,[],[],_,[1],_,1,[0],_)),
	insert_symbol_entry(BT,findall/3,st(findall/3,_,[-,+,-],[void,void,length],_,[1],_,inf,[0],_)).

second_order_predicate(findall/3).

second_order_predicate_pred_arg(findall(_,P,_),P).

second_order_predicate_pred_num(Body,LitNum,Num) :-
	number_of_literals(Body,1,Num1),
	Num is Num1+LitNum.

legal_pred_arg(Pred) :-
	functor(Pred,F,N),
	F/N \== (',')/2,			% single literal
	\+ second_order_predicate(F/N).	% non-second-order predicate
%
%  Print out the buildin table.
%
print_buildin_table(BT) :-
	tell(buildin_table),
	p_buildin_table(BT),
	told.

p_buildin_table(BT) :-
	var(BT).
p_buildin_table(BT) :-
	nonvar(BT),
	BT = [E|B],
	write(E),
	nl,
	p_buildin_table(B).

%
%  clause.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for finding information about a clause
%  or a literal in a clause.
%

%
%  Get the term and its measure at an argument position of a clause.
%
clause_term_measure(BT,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,LitNum),
	%write(LitNum),nl,
	(LitNum > 0 ->
		(arg(2,Clause,Body),
		 number_of_literals(Body,1,Num),
		 (LitNum > Num ->
			NewNum is LitNum-Num;	% assume depth-one single pred
			NewNum = LitNum));
		NewNum = LitNum),
	ith_clause_literal(NewNum,Clause,Lit),
	%write(Lit),nl,
	((LitNum > 0, LitNum > Num) ->
		second_order_predicate_pred_arg(Lit,NewLit);
		NewLit = Lit),
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,NewLit,Term),
	functor(NewLit,F,A),
	literal_property(BT,ST,F/A,measure,MeasureList),
	ith_list_element(ArgNum,MeasureList,Measure).
	
/*
clause_term_measure(_,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,0),
	pos_argnum(Pos,N),
	head_term_measure(ST,Clause,N,Term,Measure).
clause_term_measure(BT,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,M),
	M > 0,
	pos_argnum(Pos,N),
	body_term_measure(BT,ST,Clause,M,N,Term,Measure).
*/
%
%  Get the term and its measure at an argument position of the head.
%
head_term_measure(ST,Clause,N,Term,Measure) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Clause = Head),
	arg(N,Head,Term),
	functor(Head,F,A),
	find_symbol_field(ST,F/A,measure,MeasureList),
	ith_list_element(N,MeasureList,Measure).

%
%  Get the term and its measure at an argument position of a body literal.
%
body_term_measure(BT,ST,Clause,M,N,Term,Measure) :-
	arg(2,Clause,Body),
	literal_term_measure(BT,ST,Body,1,M,N,Term,Measure).

%
%  Get the term and its measure at an argument position of a literal.
%
literal_term_measure(BT,ST,(Lit,_),M,M,N,Term,Measure) :-
	arg(N,Lit,Term),
	functor(Lit,F,A),
	find_symbol_field(BT,F/A,measure,MeasureList),
	(var(MeasureList) ->
		find_symbol_field(ST,F/A,measure,MeasureList);
		true),
	ith_list_element(N,MeasureList,Measure).
literal_term_measure(BT,ST,Body,M,M,N,Term,Measure) :-
	nonsequence(Body),
	arg(N,Body,Term),
	functor(Body,F,A),
	find_symbol_field(BT,F/A,measure,MeasureList),
	(var(MeasureList) ->
		find_symbol_field(ST,F/A,measure,MeasureList);
		true),
	ith_list_element(N,MeasureList,Measure).
literal_term_measure(BT,ST,(_,Body),I,M,N,Term,Measure) :-
	I < M,
	I1 is I+1,
	literal_term_measure(BT,ST,Body,I1,M,N,Term,Measure).
literal_term_measure(_,_,Body,I,M,_,_,_) :-
	I > M,
	nonsequence(Body),
	%%% Fred
	error_message('illegal argument position',_,_).

%
%  Get the ith literal in the clause.
%
ith_clause_literal(I,Clause,Lit) :-
	clause_type(Clause,Type),
	ith_clause_literal(Type,I,Clause,Lit).

ith_clause_literal(2,0,Clause,Lit) :-
	arg(1,Clause,Lit).
ith_clause_literal(2,I,Clause,Lit) :-
	I > 0,
	arg(2,Clause,Body),
	ith_body_literal(I,Body,Lit).
ith_clause_literal(3,0,Clause,Clause).

%
%  Get the ith literal in the body.
%
ith_body_literal(1,(Lit,_),Lit).
ith_body_literal(1,Lit,Lit) :-
	nonsequence(Lit).
ith_body_literal(LitNum,(_,Body),Lit) :-
	LitNum > 1,
	LitNum1 is LitNum-1,
	ith_body_literal(LitNum1,Body,Lit).

%
number_of_literals(Lit,Num,Num) :-
	nonsequence(Lit).
number_of_literals((_,Body),N1,Num) :-
	N2 is N1+1,
	number_of_literals(Body,N2,Num).

%
%  ground_size.pl		Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for determining the size of a ground
%  term.
%

%
%  Determine the size of a ground term.
%
ground_term_size(void,_,0).
ground_term_size(int,Term,Size) :-
	ground_int(Term,Size).
ground_term_size(length,Term,Size) :-
	ground_length(Term,Size).
ground_term_size(size,Term,Size) :-
	ground_size(Term,Size).
ground_term_size(depth(ChildList),Term,Size) :-
	ground_depth(ChildList,Term,Size).

%
%  Determine the size of a ground term under measure int.
%
ground_int(Int,bot) :-
	noninteger(Int).
ground_int(Int,Int) :-
	integer(Int).

%
%  Determine the size of a ground term under measure length.
%
ground_length(Term,bot) :-
	nonlist(Term).
ground_length([],0).
ground_length([_|T],Size) :-
	ground_term_size(length,T,Size1),
	add(Size1,1,Size).

%
%  Determine the size of a ground term under measure size.
%
ground_size(Term,bot) :-
	var(Term).
ground_size(Term,1) :-
	atomic(Term).
ground_size(Term,Size) :-
	compound(Term),
	functor(Term,_,N),
	ground_size(N,Term,Size1),
	add(Size1,1,Size).

ground_size(0,_,0).
ground_size(N,Term,Size) :-
	N > 0,
	arg(N,Term,Arg),
	ground_size(Arg,Size1),
	N1 is N-1,
	ground_size(N1,Term,Size2),
	add(Size1,Size2,Size).

%
%  Determine the size of a ground term under measure depth.
%
ground_depth(_,Term,bot) :-
	var(Term).
ground_depth(_,Term,0) :-
	atomic(Term).
ground_depth(ChildList,Term,Size) :-
	compound(Term),
	functor(Term,_,N),
	ground_depth(ChildList,N,Term,Size1),
	add(Size1,1,Size).

ground_depth(_,0,_,bot).
ground_depth(ChildList,N,Term,Size) :-
	N > 0,
	(member(ChildList,N) ->
		(arg(N,Term,Arg),
		 ground_depth(ChildList,Arg,Size1));
		Size1 = bot),
	N1 is N-1,
	ground_depth(ChildList,N1,Term,Size2),
	maximum(Size1,Size2,Size).

%
%  implicit_size.pl		Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for finding the implicit size of
%  a variable at the input position of the head.
%

%
%  Compute the implicit size of a variable.
%
implicit_var_size(void,_,_,_,_,0).
implicit_var_size(_,Term,_,_,_,bot) :-
	nonvar(Term).
implicit_var_size(Measure,Term,Pos,Adg,Clause,Size) :-
	var(Term),
	find_adg_field(Adg,Pos,succ,Succ),
	implicit_var_size(Succ,Measure,Clause,Term,Size).

%
%  Compute the implicit sizes of the successors of a variable.
%
implicit_var_size([],_,_,_,bot).
implicit_var_size([Pos|PList],Measure,Clause,Term,Size) :-
	pos_litnum(Pos,0),
	implicit_var_size(PList,Measure,Clause,Term,Size).
implicit_var_size([Pos|PList],Measure,Clause,Term,Size) :-
	pos_litnum(Pos,LitNum),
	%write(LitNum),nl,
	LitNum > 0,
	arg(2,Clause,Body),
	number_of_literals(Body,1,Num),
	%write(Num),nl,
	(LitNum > Num ->
		NLitNum is LitNum-Num;
		NLitNum = LitNum),
	ith_clause_literal(NLitNum,Clause,Lit),
	(LitNum > Num ->
		second_order_predicate_pred_arg(Lit,NLit);
		NLit = Lit),
	functor(NLit,NF,NA),
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,NLit,Arg),
	%write(Arg),nl,
	(Arg == Term ->
		implicit_var_size(Measure,NF/NA,Pos,NLit,Size1);
		Size1 = bot),
	implicit_var_size(PList,Measure,Clause,Term,Size2),
	maximum(Size1,Size2,Size).

%
%  Compute the implicit sizes of a position under a particular measure.
%
implicit_var_size(int,Name,Pos,Lit,Size) :-
	implicit_int(Name,Pos,Lit,Size).
implicit_var_size(length,Name,Pos,Lit,Size) :-
	implicit_length(Name,Pos,Lit,Size).
implicit_var_size(size,Name,Pos,Lit,Size) :-
	implicit_size(Name,Pos,Lit,Size).
implicit_var_size(depth(Child),Name,Pos,Lit,Size) :-
	implicit_depth(Child,Name,Pos,Lit,Size).

%
%  Compute the implicit sizes of a position under measure int.
%
implicit_int((=:=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=:=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(int,Arg,Size).
implicit_int(Name,_,_,bot) :-
	Name \== (=:=)/2,
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure length.
%
implicit_length((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(length,Arg,Size).
implicit_length(Name,_,_,bot) :-
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure size.
%
implicit_size(atom/1,_,_,1).
implicit_size(atomic/1,_,_,1).
implicit_size(number/1,_,_,1).
implicit_size(integer/1,_,_,1).
implicit_size(float/1,_,_,1).
implicit_size((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size((=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	ground_term_size(size,Arg,Size).
implicit_size(Name,_,_,bot) :-
	Name \== atom/1,
	Name \== atomic/1,
	Name \== number/1,
	Name \== integer/1,
	Name \== float/1,
	Name \== (==)/2,
	Name \== (=)/2.

%
%  Compute the implicit sizes of a position under measure depth.
%
implicit_depth(_,atom/1,_,_,0).
implicit_depth(_,atomic/1,_,_,0).
implicit_depth(_,number/1,_,_,0).
implicit_depth(_,integer/1,_,_,0).
implicit_depth(_,float/1,_,_,0).
implicit_depth(Child,(==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	%% Fred
	%ground_term_depth(depth(Child),Arg,Size).
	ground_depth(depth(Child),Arg,Size).
implicit_depth(Child,(==)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	%ground_term_depth(depth(Child),Arg,Size).
	ground_depth(depth(Child),Arg,Size).
implicit_depth(Child,(=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,1),
	arg(2,Lit,Arg),
	%ground_term_depth(depth(Child),Arg,Size).
	ground_depth(depth(Child),Arg,Size).	
implicit_depth(Child,(=)/2,Pos,Lit,Size) :-
	pos_argnum(Pos,2),
	arg(1,Lit,Arg),
	%ground_term_depth(depth(Child),Arg,Size).
	ground_depth(depth(Child),Arg,Size).
implicit_depth(_,Name,_,_,bot) :-
	Name \== atom/1,
	Name \== atomic/1,
	Name \== number/1,
	Name \== integer/1,
	Name \== float/1,
	Name \== (==)/2,
	Name \== (=)/2.

%
%
insert_size_function([],_,_).
insert_size_function([Pred|Component],[Size|SList],ST) :-
	insert_symbol_field(ST,Pred,size,Size),
	insert_size_function(Component,SList,ST).
%
%  normalize.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing normalization.
%

%
%  Initialize the normalization queue.
%
init_normalize_queue(Pos,QHead,QTail) :-
	init_queue(QHead,ITail),
	set_put_queue(ITail,Pos,QTail).

%
%  Insert the predecessors of a position into the normalization queue.
%
insert_normalize_queue(Pos,Adg,QTail,NTail) :-
	find_adg_field(Adg,Pos,pred,EdgeList),
	set_put_queue(QTail,EdgeList,NTail).

%
%  Normalize an expression by the sizes in the normalization queue.
%
normalize(bot,_,_,_,_,_,_,_,_,_,_,_,bot).
normalize(Exp,QHead,QTail,_,_,_,_,_,_,_,_,_,Exp) :-
	Exp \== bot,
	empty_queue(QHead,QTail).
normalize(Exp,QHead,QTail,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,RSize,NExp) :-
	Exp \== bot,
	nonempty_queue(QHead,QTail),
	get_queue(QHead,Pos,NHead),
	(subterm(Pos,Exp) ->
		normalize_pos(Exp,Pos,NHead,QTail,BT,ST,Comp,Clause,Adg,
			      Gvars,PosSet,ISize,RSize,NExp);
		normalize(Exp,NHead,QTail,BT,ST,Comp,Clause,Adg,
				  Gvars,PosSet,ISize,RSize,NExp)).

%
%  Normalize an expression by the size of a position.
%
normalize_pos(Exp,Pos,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,RSize,NExp) :-
	find_adg_field(Adg,Pos,(mode),+),
	pos_litnum(Pos,0),
	pos_argnum(Pos,ArgNum),
	ith_list_element(ArgNum,ISize,Size),
	normalize_size(Exp,Pos,Size,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,
		       PosSet,ISize,RSize,NExp).
normalize_pos(Exp,Pos,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,RSize,NExp) :-
	find_adg_field(Adg,Pos,(mode),+),
	pos_litnum(Pos,LitNum),
	LitNum > 0,
	clause_term_measure(BT,ST,Clause,Pos,Term,Measure),
	general_term_size(Measure,Clause,BT,ST,Gvars,PosSet,Term,Size),
	normalize_size(Exp,Pos,Size,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,
		       PosSet,ISize,RSize,NExp).
normalize_pos(Exp,Pos,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,RSize,NExp) :-
	find_adg_field(Adg,Pos,(mode),-),
	pos_litnum(Pos,LitNum),
	pos_argnum(Pos,ArgNum),
	arg(2,Clause,Body),
	ith_body_literal(LitNum,Body,Lit),
	functor(Lit,F,N),
	(second_order_predicate(F/N) ->
		(second_order_predicate_pred_arg(Lit,Lit1),
		 functor(Lit1,F1,N1),
		 second_order_predicate_pred_num(Body,LitNum,Num),
		 literal_output_comp(F1/N1,Num,1,BT,ST,[],Adg,det,[],Size));
		literal_output_comp(F/N,LitNum,ArgNum,BT,ST,Comp,Adg,size,
			RSize,Size)),
	normalize_size(Exp,Pos,Size,QHd,QTl,BT,ST,Comp,Clause,Adg,Gvars,
		       PosSet,ISize,RSize,NExp).

%
%  Normalize an expression by a size.
%
normalize_size(_,_,bot,_,_,_,_,_,_,_,_,_,_,_,bot).
normalize_size(Exp,Pos,Size,QHead,QTail,BT,ST,Comp,Clause,Adg,Gvars,PosSet,
	       ISize,RSize,NExp) :-
	Size \== bot,
	substitute(Exp,Pos,Size,Exp1),
	insert_normalize_queue(Pos,Adg,QTail,NTail),
	normalize(Exp1,QHead,NTail,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,
		  RSize,NExp).

%
%  Get the output complexity function of a literal in the clause.
%
literal_output_comp(LitName,LitNum,ArgNum,BT,ST,Component,Adg,Type,RComp,Comp) :-
	find_symbol_field(BT,LitName,Type,BuildinComp),
	lit_output_comp(BuildinComp,ST,Component,LitName,LitNum,ArgNum,
			Adg,Type,RComp,Comp).

lit_output_comp(BuildinComp,ST,Component,LitName,LitNum,ArgNum,Adg,Type,RComp,Comp) :-
	var(BuildinComp),
	(member(Component,LitName) ->
		rec_literal_comp(Adg,LitName,LitNum,ArgNum,RComp,Comp);
		(find_symbol_field(ST,LitName,Type,LitComp),
		 (var(LitComp) ->
			(error_message(dec1,LitName,''),
			 Comp = bot);
			nonrec_literal_comp(LitComp,LitName,LitNum,ArgNum,
				Comp)))).
lit_output_comp(BuildinComp,_,_,LitName,LitNum,ArgNum,Adg,Type,_,Comp) :-
	nonvar(BuildinComp),
	buildin_output_comp(Type,LitName,LitNum,ArgNum,Adg,Comp).

%
%  Generate the symbolic output complexity function for a recursive literal.
%
rec_literal_comp(Adg,LitName,LitNum,ArgNum,RComp,Comp) :-
	find_recursive_comp(RComp,LitName,Rcomp),
	LitName = F/A,
	(var(Rcomp) ->
		(gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
		 Comp =.. [F,A,ArgNum|Pos]);
		(ith_list_element(ArgNum,Rcomp,Comp1),
		 substitute_literal_formal(A,Comp1,LitNum,Comp))).

%
%  Find the mutual recursive complexity when available.
%
find_recursive_comp([],_,_).
find_recursive_comp([comp(LitName,Rcomp)|_],LitName,Rcomp).
find_recursive_comp([comp(Pred,_)|RComp],LitName,Rcomp) :-
	Pred \== LitName,
	find_recursive_comp(RComp,LitName,Rcomp).

%
%  Substitute the actuals for the formals in the output complexity functions 
%  of a nonrecursive literal.
%
nonrec_literal_comp([C|_],LitName,LitNum,1,Comp) :-
	LitName = _/A,
	substitute_literal_formal(A,C,LitNum,Comp).
nonrec_literal_comp([_|CompList],LitName,LitNum,ArgNum,Comp) :-
	ArgNum > 1,
	ArgNum1 is ArgNum-1,
	nonrec_literal_comp(CompList,LitName,LitNum,ArgNum1,Comp).

%
%  Substitute an actual for a formal in the output complexity function 
%  of a nonrecursive literal.
%
substitute_literal_formal(0,Comp,_,Comp).
substitute_literal_formal(A,C,LitNum,Comp) :-
	A > 0,
	new_pos(0,A,Pos1),
	new_pos(LitNum,A,Pos2),
	substitute(C,Pos1,Pos2,C1),
	A1 is A-1,
	substitute_literal_formal(A1,C1,LitNum,Comp).

%
%  Substitute all occurrences of a subterm by another subterm.
%
substitute(Term,Var,NTerm,NTerm) :-
	Term == Var.
substitute(Term,Var,_,Term) :-
	Term \== Var,
	var(Term).
substitute(Term,Var,_,Term) :-
	Term \== Var,
	atomic(Term).
substitute(Term,Var,NVar,NTerm) :-
	Term \== Var,
	compound(Term),
	functor(Term,F,N),
	functor(NTerm,F,N),
	substitute(N,Term,Var,NVar,NTerm).

substitute(0,_,_,_,_).
substitute(N,Term,Var,NVar,NTerm) :-
	N > 0,
	arg(N,Term,Arg),
	substitute(Arg,Var,NVar,NArg),
	arg(N,NTerm,NArg),
	N1 is N-1,
	substitute(N1,Term,Var,NVar,NTerm).
	
%
%  Substitute the actuals for the formals in the output complexity function 
%  for an output position of a buildin predicate.
%
buildin_output_comp(size,LitName,LitNum,ArgNum,Adg,Comp) :-
	buildin_output_size(LitName,LitNum,ArgNum,Adg,Comp).
buildin_output_comp(det,_,_,_,_,1).
buildin_output_comp(time,_,_,_,_,0).

%
%  Substitute the actuals for the formals in the output size function 
%  for an output position of a buildin predicate.
%
buildin_output_size((is)/2,LitNum,1,_,Pos) :-
	new_pos(LitNum,2,Pos).
buildin_output_size((=)/2,LitNum,1,Adg,Size) :-
	new_pos(LitNum,1,Pos1),
	new_pos(LitNum,2,Pos2),
	find_adg_field(Adg,Pos1,pred,Pos),
	(var(Pos) ->
		Size = bot;
		Size = Pos2).
buildin_output_size((=)/2,LitNum,2,Adg,Size) :-
	new_pos(LitNum,1,Pos1),
	new_pos(LitNum,2,Pos2),
	find_adg_field(Adg,Pos2,pred,Pos),
	(var(Pos) ->
		Size = bot;
		Size = Pos1).
buildin_output_size(functor/3,_,2,_,1).
buildin_output_size(functor/3,LitNum,3,_,arity(Pos)) :-
	new_pos(LitNum,1,Pos).
buildin_output_size(arg/3,LitNum,3,_,arg(Pos2,Pos1)) :-
	new_pos(LitNum,1,Pos1),
	new_pos(LitNum,2,Pos2).
buildin_output_size(functor1/3,_,1,_,1).
buildin_output_size(arg/4,LitNum,4,_,Pos2+Pos3) :-
	new_pos(LitNum,2,Pos2),
	new_pos(LitNum,3,Pos3).
%buildin_output_size((=..)/2,LitNum,2,_,arity(Pos)+1) :-
%	new_pos(LitNum,1,Pos).
%
%  size.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the argument size
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the argument size analysis for a strongly connected component.
%
size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size) :-
	size_analysis(Comp,BT,ST,Comp,Adg,Gvars,[],Size).

size_analysis([],_,_,_,_,_,_,[]).
size_analysis([Pred|CompList],BT,ST,Comp,[Adg|AList],[Gvars|GList],RSize,
	      [Size|SList]) :-
	find_symbol_field(ST,Pred,clause,Clauses),
	size_clauses(Clauses,BT,ST,Comp,Adg,Gvars,RSize,Size),
	%write(Size),nl,
	solve_size_equs(Pred,ST,Comp,Size,Sol1),
	%write(Sol1),nl,
	size_analysis(CompList,BT,ST,Comp,AList,GList,[comp(Pred,Sol1)|RSize],
		SList),
	remove_recursive_comps(Sol1,ST,size,Sol),
	%write(Sol),nl,
	insert_symbol_field(ST,Pred,size,Sol).

%
%  Perform the argument size analysis for the set of clauses in a predicate.
%
size_clauses(Clauses,_,_,_,_,_,_,[]) :-
	var(Clauses).
size_clauses(Clauses,BT,ST,Comp,[Adg|AList],[Gvars|GList],RSize,[Size|SList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	size_clause(Clause,BT,ST,Comp,Adg,Gvars,RSize,TSize),
	list_simplification(TSize,Size),
	size_clauses(CList,BT,ST,Comp,AList,GList,RSize,SList).

%
%  Perform the argument size analysis for a clause.
%
size_clause(Clause,BT,ST,Comp,Adg,Gvars,RSize,Size) :-
	clause_type(Clause,Type),
	size_function(Type,Clause,BT,ST,Comp,Adg,Gvars,RSize,Size).

%
%  Compute the size functions of a clause as difference equations.
%
size_function(2,Clause,BT,ST,Comp,Adg,Gvars,RSize,Size) :-
	Clause = (Head:-_),
	size_func(Head,Clause,BT,ST,Comp,Adg,Gvars,RSize,Size).
size_function(3,Fact,BT,ST,Comp,Adg,Gvars,RSize,Size) :-
	size_func(Fact,Fact,BT,ST,Comp,Adg,Gvars,RSize,Size).

size_func(Head,Clause,BT,ST,Comp,Adg,Gvars,RSize,Size) :-
	functor(Head,F,N),
	find_symbol_field(ST,F/N,(mode),Mode),
	find_symbol_field(ST,F/N,measure,Measure),
	input_argument_size(1,Head,Mode,Measure,Clause,BT,ST,Adg,Gvars,Size),
	(fail_clause(Clause) ->
		fail_output_size(Mode,Size);
		output_argument_size(1,Head,Mode,Measure,Clause,BT,ST,Comp,
			Adg,Gvars,Size,RSize,Size)).

%
%  Compute the size function for an input position in a clause as 
%  a difference equation.
%
input_argument_size(_,_,[],_,_,_,_,_,_,[]).
input_argument_size(N,Head,[(+)|ModeList],[Measure|MeasureList],Clause,
		     BT,ST,Adg,Gvars,[Size2|Size]) :-
	arg(N,Head,Term),
	new_pos(0,N,Pos),
	(var(Term) ->
		implicit_input_size(Measure,Term,Pos,Adg,Clause,Size1);
		explicit_input_size(Measure,Term,Size1)),
	(Size1 == bot ->
		Size2 = Pos;
		Size2 = Size1),
	%write(Size2),nl,
	N1 is N+1,
	input_argument_size(N1,Head,ModeList,MeasureList,Clause,
			     BT,ST,Adg,Gvars,Size).
input_argument_size(N,Head,[(-)|ModeList],[_|MeasureList],Clause,
		     BT,ST,Adg,Gvars,[_|Size]) :-
	N1 is N+1,
	input_argument_size(N1,Head,ModeList,MeasureList,Clause,
			     BT,ST,Adg,Gvars,Size).

%
%  Compute the size function for an output position in a clause as 
%  a difference equation.
%
output_argument_size(_,_,[],_,_,_,_,_,_,_,_,_,[]).
output_argument_size(N,Head,[(+)|ModeList],[_|MeasureList],Clause,
		     BT,ST,Comp,Adg,Gvars,ISize,RSize,[_|Size]) :-
	N1 is N+1,
	output_argument_size(N1,Head,ModeList,MeasureList,Clause,
			     BT,ST,Comp,Adg,Gvars,ISize,RSize,Size).
output_argument_size(N,Head,[(-)|ModeList],[Measure|MeasureList],Clause,
		     BT,ST,Comp,Adg,Gvars,ISize,RSize,[Size3|Size]) :-
	arg(N,Head,Term),
	new_pos(0,N,Pos),
	(var(Term) ->
		implicit_output_size(Measure,Term,Adg,Clause,BT,ST,
				     Gvars,Size1);
		Size1 = bot),
	%write(Size1),nl,
	(Size1 == bot ->
		(explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,
				   Size2),
		 %write(Size2),nl,
		 normalize_size_function(Size2,Pos,BT,ST,Comp,Clause,Adg,
					 Gvars,ISize,RSize,Size3));
		Size3 = Size1),
	%write(Size3),nl,
	N1 is N+1,
	output_argument_size(N1,Head,ModeList,MeasureList,Clause,
			     BT,ST,Comp,Adg,Gvars,ISize,RSize,Size).

%
%  Compute the implicit size of a head input.
%
implicit_input_size(Measure,Term,Pos,Adg,Clause,Size) :-
	implicit_var_size(Measure,Term,Pos,Adg,Clause,Size).

%
%  Compute the explicit size of a head input.
%
explicit_input_size(Measure,Term,Size) :-
	ground_term_size(Measure,Term,Size).

%
%  Compute the implicit size of a head output.
%
implicit_output_size(Measure,Term,Adg,Clause,BT,ST,Gvars,Size) :-
	find_gvars_field(Gvars,Term,def,PosList),
	implicit_output_sizes(PosList,BT,ST,Measure,Term,Adg,Clause,Size).

implicit_output_sizes([],_,_,_,_,_,_,bot).
implicit_output_sizes([Pos|PosList],BT,ST,Measure,Term,Adg,Clause,Size) :-
	clause_term_measure(BT,ST,Clause,Pos,HTerm,_),
	(HTerm == Term ->
		implicit_var_size(Measure,Term,Pos,Adg,Clause,Size1);
		Size1 = bot),
	implicit_output_sizes(PosList,BT,ST,Measure,Term,Adg,Clause,Size2),
	maximum(Size1,Size2,Size).

%
%  Compute the explicit size of a head output.
%
explicit_output_size(Measure,Clause,BT,ST,Adg,Gvars,Term,Size) :-
	gen_clause_pos(Adg,PosSet),
	general_term_size(Measure,Clause,BT,ST,Gvars,PosSet,Term,Size).

%
%  Normalize the size function corresponding to an output position.
%
normalize_size_function(Size,Pos,BT,ST,Comp,Clause,Adg,Gvars,ISize,RSize,NSize) :-
	gen_clause_pos(Adg,PosSet),
	find_adg_field(Adg,Pos,pred,PredPos),
	init_normalize_queue(PredPos,QHead,QTail),
	normalize(Size,QHead,QTail,BT,ST,Comp,Clause,Adg,Gvars,PosSet,ISize,
		  RSize,NSize).
%
fail_output_size([],[]).
fail_output_size([+|Mode],[_|Size]) :-
	fail_output_size(Mode,Size).
fail_output_size([-|Mode],[bot|Size]) :-
	fail_output_size(Mode,Size).

%
%
remove_recursive_comps([],_,_,[]).
remove_recursive_comps([S1|Sol1],ST,Type,[S|Sol]) :-
	normal_form(S1,N1),
	remove_recursive_comp(N1,ST,Type,N),
	general_form(N,S),
	remove_recursive_comps(Sol1,ST,Type,Sol).

%
remove_recursive_comp(bot,_,_,bot).
remove_recursive_comp(inf,_,_,inf).
remove_recursive_comp(expr(T,F),ST,Type,Sol) :-
	remove_recursive_comp1(T,ST,Type,Sols),
	Sol1 = expr([],F),
	add_expr(Sol1,Sols,Sol).

%
remove_recursive_comp1([],_,_,Zero) :-
	normal_form(0,Zero).
remove_recursive_comp1([term(T,F)|Ts],ST,Type,Sol) :-
	remove_recursive_comp2(T,ST,Type,Sol1),
	C = expr([],F),
	multiply_expr(C,Sol1,Sol2),
	remove_recursive_comp1(Ts,ST,Type,Sols),
	add_expr(Sol2,Sols,Sol).

%
remove_recursive_comp2([],_,_,One) :-
	normal_form(1,One).
remove_recursive_comp2([P|Ps],ST,Type,Sol) :-
	remove_recursive_comp3(P,ST,Type,Sol1),
	remove_recursive_comp2(Ps,ST,Type,Sols),
	multiply_expr(Sol1,Sols,Sol).

%
remove_recursive_comp3(P,ST,Type,Sol) :-
	userfunc(P),
	functor(P,F,_),
	arg(1,P,Arity),
	general_form(Arity,A),
	find_symbol_field(ST,F/A,Type,Size),
	arg(2,P,OPos),
	general_form(OPos,O),
	ith_list_element(O,Size,Osize),
	substitute_literal_formal(A,Osize,1,NOsize),
	find_symbol_field(ST,F/A,(mode),Mode),
	substitute_actual(Mode,1,3,P,ST,NOsize,Type,Sol1),
	normal_form(Sol1,Sol).
remove_recursive_comp3(exp(E1,E2),ST,Type,Sol) :-
	remove_recursive_comp(E1,ST,Type,Sol1),
	remove_recursive_comp(E2,ST,Type,Sol2),
	exp_expr(Sol1,Sol2,Sol).
remove_recursive_comp3(log(E1,E2),ST,Type,Sol) :-
	remove_recursive_comp(E1,ST,Type,Sol1),
	remove_recursive_comp(E2,ST,Type,Sol2),
	log_expr(Sol1,Sol2,Sol).
remove_recursive_comp3(fact(E1),ST,Type,Sol) :-
	remove_recursive_comp(E1,ST,Type,Sol1),
	factorial_expr(Sol1,Sol).
remove_recursive_comp3(sum(E1,E2,E3,E4),ST,Type,Sol) :-
	remove_recursive_comp(E4,ST,Type,Expr),
	general_form(E1,Var),
	sum_expr(Var,E2,E3,Expr,Sol).
remove_recursive_comp3(prod(E1,E2,E3,E4),ST,Type,Sol) :-
	remove_recursive_comp(E4,ST,Type,Expr),
	general_form(E1,Var),
	prod_expr(Var,E2,E3,Expr,Sol).
remove_recursive_comp3(arg(E1,E2),ST,Type,Sol) :-
	remove_recursive_comp(E1,ST,Type,Sol1),
	remove_recursive_comp(E2,ST,Type,Sol2),
	Sol = expr([],[factor([arg(Sol1,Sol2)],1)]).
remove_recursive_comp3(arity(E1),ST,Type,Sol) :-
	remove_recursive_comp(E1,ST,Type,Sol1),
	Sol = expr([],[factor([arity(Sol1)],1)]).
	

%
%
substitute_actual([],_,_,_,_,Sol,_,Sol).
substitute_actual([+|Mode],M,N,P,ST,Osize,Type,Sol) :-
	arg(N,P,Arg),
	remove_recursive_comp(Arg,ST,Type,RArg),
	general_form(RArg,Rarg),
	new_pos(1,M,Pos),
	substitute(Osize,Pos,Rarg,NOsize),
	M1 is M+1,
	N1 is N+1,
	substitute_actual(Mode,M1,N1,P,ST,NOsize,Type,Sol).
substitute_actual([-|Mode],M,N,P,ST,Osize,Type,Sol) :-
	M1 is M+1,
	N1 is N+1,
	substitute_actual(Mode,M1,N1,P,ST,Osize,Type,Sol).
%
%  term_diff.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the diff functions.
%

%
%  Compute the size difference between a term and its predecessors.
%
general_term_diff(_,_,_,_,[],_,bot).
general_term_diff(BT,ST,Measure,Clause,[Pos|PList],Term,Size) :-
	clause_term_measure(BT,ST,Clause,Pos,PosTerm,PosMeasure),
	term_diff(PosMeasure,Measure,Pos,PosTerm,Term,Size1),
	general_term_diff(BT,ST,Measure,Clause,PList,Term,Size2),
	minimum(Size1,Size2,Size).

%
%  Compute the size difference between two terms.
%
term_diff(M1,M2,_,_,_,bot) :-
	M1 \== (?),
	M2 \== (?),
	M1 \== M2.
term_diff((?),(?),_,_,_,bot).
term_diff((?),M2,Pos,Term1,Term2,Size) :-
	M2 \== (?),
	term_diff(M2,Pos,Term1,Term2,Size).
term_diff(M1,(?),Pos,Term1,Term2,Size) :-
	M1 \== (?),
	term_diff(M1,Pos,Term1,Term2,Size).
term_diff(M1,M2,Pos,Term1,Term2,Size) :-
	M1 == M2,
	M1 \== (?),
	term_diff(M1,Pos,Term1,Term2,Size).

term_diff(int,Pos,Term1,Term2,Size) :-
	term_diff_int(Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(length,Pos,Term1,Term2,Size) :-
	term_diff_length(Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(depth(ChildList),Pos,Term1,Term2,Size) :-
	term_diff_depth(ChildList,Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(size,Pos,Term1,Term2,Size) :-
	term_diff_size(Pos,Term1,Term2,Size).

%
%  Compute the size difference between two terms under the measure int.
%
term_diff_int(Term1,Term2,0) :-
	Term1 == Term2.
term_diff_int(Term1,Term2,bot) :-
	Term1 \== Term2.

%
%  Compute the size difference between two terms under the measure length.
%
term_diff_length(Term1,Term2,0) :-
	Term1 == Term2.
term_diff_length(Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1).
term_diff_length(Term1,Term2,Size) :-
	Term1 \== Term2,
	Term1 = [_|TList],
	term_diff_length(TList,Term2,Size1),
	sub(Size1,1,Size).

%
%  Compute the size difference between two terms under the measure depth.
%
term_diff_depth(_,Term1,Term2,0) :-
	Term1 == Term2.
term_diff_depth(_,Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1).
term_diff_depth(ChildList,Term1,Term2,Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1,_,N),
	term_diff_depth(N,ChildList,Term1,Term2,Size1),
	sub(Size1,1,Size).

term_diff_depth(0,_,_,_,bot).
term_diff_depth(N,ChildList,Term1,Term2,Size) :-
	N > 0,
	N1 is N-1,
	(member(ChildList,N) ->
		(arg(N,Term1,Arg),
		 term_diff_depth(ChildList,Arg,Term2,SizeN),
		 term_diff_depth(N1,ChildList,Term1,Term2,SizeN1),
		 minimum(SizeN,SizeN1,Size));
		 term_diff_depth(N1,ChildList,Term1,Term2,Size)).

%
%  Compute the size difference between two terms under the measure size.
%
term_diff_size(Pos,Term1,Term2,Pos) :-
	Term1 == Term2.
term_diff_size(Pos,[Head|_],Term,head(Pos)) :-
	Head == Term.
term_diff_size(Pos,[_|Tail],Term,tail(Pos)) :-
	Tail == Term.
term_diff_size(_,Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1).
term_diff_size(Pos,Term1,Term2,Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1,_,N),
	term_diff_size(N,Pos,Term1,Term2,Size).

term_diff_size(0,_,_,_,bot).
term_diff_size(N,Pos,Term1,Term2,Size) :-
	N > 0,
	N1 is N-1,
	arg(N,Term1,Arg),
	(Arg == Term2 ->
		Size = arg(Pos,N);
		term_diff_size(N1,Pos,Term1,Term2,Size)).
%
%  term_size.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for determining the size of a general
%  term.
%

%
%  Determine the size of a general term.
%
general_term_size(void,_,_,_,_,_,_,0).
general_term_size(Measure,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	Measure \== void,
	ground_term_size(Measure,Term,Size1),
	(Size1 == bot ->
		nonground_term_size(Measure,Clause,BT,ST,Gvars,PosSet,
				    Term,Size);
		Size = Size1).

%
%  Determine the size of a general nonground term.
%
nonground_term_size(Measure,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	common_predecessor(Term,Gvars,PosSet,ComPred),
	general_term_diff(BT,ST,Measure,Clause,ComPred,Term,Size1),
	(Size1 == bot, compound(Term) ->
		general_subterm_size(Measure,Clause,BT,ST,Gvars,PosSet,
				     Term,Size);
		Size = Size1).

%
%  Determine the common predecessors of the variables in a term.
%
common_predecessor(Term,_,ComPred,ComPred) :-
	atomic(Term).
common_predecessor(Term,Gvars,ComPred,NewComPred) :-
	var(Term),
	find_gvars_field(Gvars,Term,def,Pos),
	intersection(ComPred,Pos,NewComPred).
common_predecessor(Term,Gvars,ComPred,NewComPred) :-
	compound(Term),
	functor(Term,_,N),
	common_predecessor(N,Term,Gvars,ComPred,NewComPred).
	
common_predecessor(0,_,_,ComPred,ComPred).
common_predecessor(N,Term,Gvars,ComPred,NewComPred) :-
	N > 0,
	arg(N,Term,Arg),
	common_predecessor(Arg,Gvars,ComPred,ComPred1),
	N1 is N-1,
	common_predecessor(N1,Term,Gvars,ComPred1,NewComPred).

%
%  Determine the sizes of the subterms of a general term.
%
general_subterm_size(int,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	functor(Term,Op,N),
	subterm_int(N,Op,Term,BT,ST,Clause,Gvars,PosSet,Size).
general_subterm_size(length,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	subterm_length(Clause,BT,ST,Gvars,PosSet,Term,Size1),
	addition(Size1,1,Size).
general_subterm_size(size,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	functor(Term,_,N),
	subterm_size(N,Clause,BT,ST,Gvars,PosSet,Term,Size1),
	addition(Size1,1,Size).
general_subterm_size(depth(ChildList),Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	functor(Term,_,N),
	subterm_depth(N,Clause,BT,ST,Gvars,PosSet,ChildList,Term,Size1),
	addition(Size1,1,Size).

%
%  Determine the sizes of the subterms of a general term under measure int.
%
subterm_int(1,Op,Term,BT,ST,Clause,Gvars,PosSet,Size) :-
	arg(1,Term,Term1),
	general_term_size(int,Clause,BT,ST,Gvars,PosSet,Term1,Size1),
	Size =.. [Op,Size1].
subterm_int(2,Op,Term,BT,ST,Clause,Gvars,PosSet,Size) :-
	arg(1,Term,Term1),
	general_term_size(int,Clause,BT,ST,Gvars,PosSet,Term1,Size1),
	arg(2,Term,Term2),
	general_term_size(int,Clause,BT,ST,Gvars,PosSet,Term2,Size2),
	Size =.. [Op,Size1,Size2].

%
%  Determine the sizes of the subterms of a general term under measure length.
%
subterm_length(_,_,_,_,_,Term,bot) :-
	nonlist(Term).
subterm_length(Clause,BT,ST,Gvars,PosSet,[_|T],Size) :-
	general_term_size(length,Clause,BT,ST,Gvars,PosSet,T,Size).

%
%  Determine the sizes of the subterms of a general term under measure size.
%
subterm_size(0,_,_,_,_,_,_,0).
subterm_size(N,Clause,BT,ST,Gvars,PosSet,Term,Size) :-
	N > 0,
	arg(N,Term,TermN),
	general_term_size(size,Clause,BT,ST,Gvars,PosSet,TermN,SizeN),
	N1 is N-1,
	subterm_size(N1,Clause,BT,ST,Gvars,PosSet,Term,SizeN1),
	addition(SizeN,SizeN1,Size).

%
%  Determine the sizes of the subterms of a general term under measure depth.
%
subterm_depth(0,_,_,_,_,_,_,_,0).
subterm_depth(N,Clause,BT,ST,Gvars,PosSet,ChildList,Term,Size) :-
	N > 0,
	(member(ChildList,N) ->
		(arg(N,Term,TermN),
		 general_term_size(depth(ChildList),Clause,BT,ST,Gvars,PosSet,
				   TermN,SizeN));
		SizeN = 0),
	N1 is N-1,
	subterm_depth(N1,Clause,BT,ST,Gvars,PosSet,ChildList,Term,SizeN1),
	maximum(SizeN,SizeN1,Size).

%
%  size_diff_equ.pl		Nai-Wei Lin			February, 1992
%
%  This file contains the programs for solving the size difference equations.
%

%
%  Solve size equations.
%
solve_size_equs(Pred,ST,Component,Size,Sol) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	input_sizes(Size,Mode,ISize),
	(consistent_input_sizes(Pred,ST,Component,ISize,Isize) ->
		(solve_size_complexity_equs(Pred,ST,Mode,Size,ISize,Osize),
		 combine_size(Mode,Isize,Osize,Sol));
		dummy_size(Mode,1,Sol)).

%
%  Solve size complexity equations.
%
solve_size_complexity_equs(Pred,ST,Mode,Size,ISize,Sol) :-
	construct_size_comp_equs(Mode,1,Size,ISize,Equs,OIndex),
	solve_size_comp_equs(Equs,ST,Pred,OIndex,Sol).

%
%  Construct the complexity equations for each output position and
%  a list of indices for output positions.
%
construct_size_comp_equs([],_,_,_,[],[]).
construct_size_comp_equs([+|Mode],I,Size,ISize,Equs,OIndex) :-
	I1 is I+1,
	construct_size_comp_equs(Mode,I1,Size,ISize,Equs,OIndex).
construct_size_comp_equs([-|Mode],I,Size,ISize,[E|Equs],[I|OIndex]) :-
	output_sizes(Size,I,OSize),
	construct_comp_equs(ISize,OSize,1,E),
	I1 is I+1,
	construct_size_comp_equs(Mode,I1,Size,ISize,Equs,OIndex).

%
%  Solve the set of size complexity equations.
%
solve_size_comp_equs([],_,_,[],[]).
solve_size_comp_equs([E|Equs],ST,Pred,[I|OIndex],[S|Sols]) :-
	classify_equs(E,DE,BE),
	solve_size_comp_equs1(DE,BE,ST,Pred,I,NS),
	general_form(NS,S),
	solve_size_comp_equs(Equs,ST,Pred,OIndex,Sols).

solve_size_comp_equs1(DE,BE,ST,Pred,I,Sol) :-
	abnormal_equs(BE,S),
	%write(S),nl,
	(general_form(S,0) ->
		solve_size_comp_equs2(DE,BE,ST,Pred,I,Sol);	% no abnormals
		Sol = S).

%
%  Test if any abnormal size equation exists.
%
abnormal_equs([],Zero) :- normal_form(0,Zero).
abnormal_equs([equ(_,_,O)|BE],Sol) :-
	abnormal_equ(O,S1),
	abnormal_equs(BE,S2),
	max_expr(S1,S2,Sol).

abnormal_equ(bot,bot) :- !.
abnormal_equ(inf,inf) :- !.
abnormal_equ(_,Zero) :- normal_form(0,Zero).

%
%  Solve the set of size complexity equations corresponding one output position.
%
solve_size_comp_equs2(DE,[],_,Pred,_,Sol) :-	% no base cases
	DE \== [],!,
	(indirect_recursion(DE,Pred) ->
		solve_comp_non_diff_equs(DE,Sol);
		Sol = bot).
solve_size_comp_equs2([],BE,_,_,_,Sol) :-	% no difference equations
	BE \== [],!,
	solve_comp_non_diff_equs(BE,Sol).
solve_size_comp_equs2(DE,BE,ST,Pred,I,Sol) :-	% both diff equs and base cases
	DE \== [],
	BE \== [],
	(indirect_recursion(DE,Pred) ->
		(solve_comp_non_diff_equs(DE,S1),
		 solve_comp_non_diff_equs(BE,S2),
		 max_expr(S1,S2,Sol));
		solve_size_comp_diff_equs(DE,BE,ST,Pred,I,Sol)).

%
%
solve_size_comp_diff_equs(Equ,BEqus,ST,Pred,I,Sol) :-
	exprs_one_index_reducible(Equ,Pred,I,IS,ROC,Pos),
	boundary_condition_one_index_reducible(BEqus,IS,Pos,RBC),
	solve_one_index_size_diff_equs(ROC,RBC,ST,Pred,Pos,Sol),!.
solve_size_comp_diff_equs(Equ,BEqus,_,Pred,I,Sol) :-
	exprs_two_indices_reducible(Equ,Pred,I,IS,Pos),
	adjust_pos(Pos,NPos),
	reduce_two_indices_exprs(Equ,Pred,I,IS,NPos,NEqu),
	NPos = [Pos1,Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	boundary_condition_two_indices_reducible(BEqus,IS,[POS1,POS2],BCs),
	solve_two_indices_diff_equs(NEqu,BCs,Pred,Sol),!.
solve_size_comp_diff_equs(_,_,_,_,_,inf).

solve_one_index_size_diff_equs([],_,_,_,_,Zero) :- normal_form(0,Zero).
solve_one_index_size_diff_equs([equ(_,Var,OC)|OCs],BEqus,ST,Pred,Pos,Sol) :-
	%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
	%general_form(OC,R),
	%write(R),nl,
	%write(Var),nl,
	(indirect_recursion_expr(OC,Pred) ->
		S = OC;
		solve_typed_diff_equ(size,OC,BEqus,Var,ST,Pred,Pos,S)),
	%general_form(S,GS),
	%write(GS),nl,
	%solve_one_index_size_diff_equ(Type,BEqus,Var,An1,An2,Bn,ST,Pred,Pos,S),
	solve_one_index_size_diff_equs(OCs,BEqus,ST,Pred,Pos,Sols),
	max_expr(Sols,S,Sol).

%
solve_one_index_size_diff_equ(second_order,BE,Var,An1,An2,Bn,_,_,_,Sol) :-
	!, 
	boundary_conds(BE,Ival),
	%write(Ival),nl,
	solve_diff_equ(second_order,Var,An1,An2,Bn,Ival,Sol).
solve_one_index_size_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
	solve_one_index_size_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol).

%
solve_one_index_size_diff_equ1([],_,_,_,_,_,_,_,_,Zero) :-
	normal_form(0,Zero).
solve_one_index_size_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
	E = equ(_,Index,Val),
	(integer(Index) ->
		(boundary_conds([E],Ival),
		 %write(Ival),nl,
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,S));
		(size_boundary_cond(E,ST,Pred,Pos,Ival),
		 %write(Ival),nl,
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
		 max_expr(TS,Val,S))),
	%general_form(S,GS),
	%write(GS),nl,
	solve_one_index_size_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sols),
	max_expr(Sols,S,Sol).

%
solve_two_indices_diff_equs([],_,_,Zero) :- normal_form(0,Zero).
solve_two_indices_diff_equs([equ(_,Vars,OC)|Equ],BCs,Pred,Sol) :-
	solve_two_indices_diff_equ(OC,BCs,Vars,Pred,Sol1),
	solve_two_indices_diff_equs(Equ,BCs,Pred,Sol2),
	max_expr(Sol1,Sol2,Sol).

solve_two_indices_diff_equ(Equ,BCs,Vars,Pred,Sol) :-
	two_indices_diff_equ_type(Equ,Vars,Pred,An,Bn,Type),
	solve_two_indices_diff_equ(Type,BCs,Vars,An,Bn,Sol),!.
solve_two_indices_diff_equ(_,_,_,_,inf).

solve_two_indices_diff_equ(first_index,BCs,Vars,An,Bn,Sol) :-
	two_indices_boundary_cond_type(BCs,Vars,_,Ivalue,_,BType),
	Vars = [Var1,_],
	((BType == exclusive; BType == both) ->
		solve_fode(Var1,Ivalue,An,Bn,Sol);
		Sol = inf).
solve_two_indices_diff_equ(second_index,BCs,Vars,An,Bn,Sol) :-
	two_indices_boundary_cond_type(BCs,Vars,_,_,Ivalue,BType),
	Vars = [_,Var2],
	((BType == exclusive; BType == both) ->
		solve_fode(Var2,Ivalue,An,Bn,Sol);
		Sol = inf).
solve_two_indices_diff_equ(both_indices,BCs,Vars,An,Bn,Sol) :-
	two_indices_boundary_cond_type(BCs,Vars,Ivalue,Ivalue1,Ivalue2,BType),
	Vars = [Var1,Var2],
	(BType == inclusive ->
		solve_fode(Var1,Ivalue,An,Bn,Sol);
	 	((BType == exclusive; BType == both) ->
			(solve_fode(Var1,Ivalue1,An,Bn,Sol1),
			 solve_fode(Var2,Ivalue2,An,Bn,Sol2),
			 max_expr(Sol1,Sol2,Sol));
			Sol = inf)).

two_indices_diff_equ_type(Equ,[Var1,Var2],F/_,An,Bn,Type) :-
	Equ = expr([term([DVar],An)],Bn),
	userfunc(DVar),
	functor(DVar,F,2),
	arg(1,DVar,Arg1),
	arg(2,DVar,Arg2),
	two_indices_equ_type(Arg1,Arg2,Var1,Var2,Type).

two_indices_equ_type(Arg1,Arg2,Var1,Var2,first_index) :-
	normal_form(Var1-1,Arg1),
	normal_form(Var2,Arg2).
two_indices_equ_type(Arg1,Arg2,Var1,Var2,second_index) :-
	normal_form(Var1,Arg1),
	normal_form(Var2-1,Arg2).
two_indices_equ_type(Arg1,Arg2,Var1,Var2,both_indices) :-
	normal_form(Var1-1,Arg1),
	normal_form(Var2-1,Arg2).

two_indices_boundary_cond_type([equ(_,[0,0],Expr)],_,[val(Zero,Expr)],_,_,
	inclusive) :- normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[0,Var2],Expr1),equ(_,[Var1,0],Expr2)],
	[Var1,Var2],_,[val(Zero,Expr1)],[val(Zero,Expr2)],exclusive) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[Var1,0],Expr2),equ(_,[0,Var2],Expr1)],
	[Var1,Var2],_,[val(Zero,Expr1)],[val(Zero,Expr2)],exclusive) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[0,0],Expr),equ(_,[0,Var2],Expr1),
	equ(_,[Var1,0],Expr2)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[0,0],Expr),equ(_,[Var1,0],Expr2),
	equ(_,[0,Var2],Expr1)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[0,Var2],Expr1),equ(_,[0,0],Expr),
	equ(_,[Var1,0],Expr2)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[0,Var2],Expr1),equ(_,[Var1,0],Expr2),
	equ(_,[0,0],Expr)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[Var1,0],Expr2),equ(_,[0,0],Expr),
	equ(_,[0,Var2],Expr1)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type([equ(_,[Var1,0],Expr2),equ(_,[0,Var2],Expr1),
	equ(_,[0,0],Expr)],[Var1,Var2],[val(Zero,Expr)],[val(Zero,Expr1)],
	[val(Zero,Expr2)],both) :-
	normal_form(0,Zero).
two_indices_boundary_cond_type(_,_,_,_,_,no_match).

%
%  Reduce a difference equation to an one-index difference equation.
%
exprs_one_index_reducible([],_,_,_,[],_).
exprs_one_index_reducible([equ(Num,IS,OC)|Equ],Pred,I,IS,
		[equ(Num,Var,ROC)|OCs],Pos) :-
	expr_one_index_reducible(OC,Pred,I,IS,ROC,Pos),
	ith_list_element(Pos,IS,Var),
	exprs_one_index_reducible(Equ,Pred,I,IS,OCs,Pos).

expr_one_index_reducible(expr(T,F),Pred,I,ISize,expr(NT,F),Pos) :-
	term_one_index_reducible(T,Pred,I,ISize,0,NT,Pos1),
	Pos is Pos1-2.

term_one_index_reducible([],_,_,_,0,[],_) :- fail.
term_one_index_reducible([],_,_,_,Flag,[],_) :-
	Flag > 0.
term_one_index_reducible([term(P,F)|T],Pred,I,ISize,Flag,[term(NP,F)|NT],Pos) :-
	primary_one_index_reducible(P,Pred,I,ISize,Flag1,NP,Pos),
	Flag2 is Flag1+Flag,
	term_one_index_reducible(T,Pred,I,ISize,Flag2,NT,Pos).

primary_one_index_reducible([],_,_,_,0,[],_).
primary_one_index_reducible([P|Ps],F/A,I,ISize,Flag,[R|Rs],Pos) :- % direct recursion
	userfunc(P), functor(P,F,N), arg(1,P,Arity),
	normal_form(A,Arity),
	arg(2,P,Index),
	normal_form(I,Index),
	one_arg_diff(3,N,P,ISize,0,RArg,Pos),
	functor(R,F,1), arg(1,R,RArg),
	primary_one_index_reducible(Ps,F/A,I,ISize,Flag1,Rs,Pos),
	Flag is Flag1+1.
primary_one_index_reducible([P|Ps],F/A,I,ISize,Flag,[P|Rs],Pos) :-	% indirect recursion
	userfunc(P), functor(P,F1,_),
	F1 \== F,
	primary_one_index_reducible(Ps,F/A,I,ISize,Flag,Rs,Pos).
primary_one_index_reducible([P|Ps],F/A,I,ISize,Flag,[P|Rs],Pos) :-	% indirect recursion
	userfunc(P), functor(P,F,_), arg(1,P,Arity),
	general_form(Arity,GArity),
	GArity \== A,
	primary_one_index_reducible(Ps,F/A,I,ISize,Flag,Rs,Pos).
primary_one_index_reducible([P|Ps],F/A,I,ISize,Flag,[R|Rs],Pos) :- % direct recursion
	P = sum(Index,Lower,Upper,Expr),
	Expr = expr([term([P1],FS)],CS),
	userfunc(P1), functor(P1,F,N), arg(1,P1,Arity),
	normal_form(A,Arity),
	arg(2,P1,OPos),
	normal_form(I,OPos),
	one_arg_diff(3,N,P1,ISize,0,RArg,Pos),
	functor(R1,F,1), arg(1,R1,RArg),
	RExpr = expr([term([R1],FS)],CS),
	R = sum(Index,Lower,Upper,RExpr),
	primary_one_index_reducible(Ps,F/A,I,ISize,Flag1,Rs,Pos),
	Flag is Flag1+1.

%
%  Test if the number of different arguments is 1.
%
one_arg_diff(M,N,_,[],1,_,_) :- M > N.
one_arg_diff(M,N,F,[S|ISize],Count,RArg,I) :- M =< N,
	arg(M,F,Arg), normal_form(S,Arg),!,
	M1 is M+1,
	one_arg_diff(M1,N,F,ISize,Count,RArg,I).
one_arg_diff(M,N,F,[S|ISize],0,Arg,M) :- M =< N,
	arg(M,F,Arg), normal_form(S,S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	one_arg_diff(M1,N,F,ISize,1,Arg,M).

%
%  Reduce a difference equation to an two-indices difference equation.
%
exprs_two_indices_reducible(Equ,Pred,I,IS,Pos) :-
	exprs_two_indices_reducible1(Equ,Pred,I,IS,Pos),
	length(Pos,Len), Len =:= 2.

exprs_two_indices_reducible1([],_,_,_,[]).
exprs_two_indices_reducible1([equ(_,IS,OC)|Equ],Pred,I,IS,Pos) :-
	expr_two_indices_reducible(OC,Pred,I,IS,Pos1),
	exprs_two_indices_reducible1(Equ,Pred,I,IS,Pos2),
	union(Pos1,Pos2,Pos).

expr_two_indices_reducible(expr(T,_),Pred,I,ISize,Pos) :-
	term_two_indices_reducible(T,Pred,I,ISize,0,Pos),
	length(Pos,Len), Len =< 2.

term_two_indices_reducible([],_,_,_,0,_) :- fail.
term_two_indices_reducible([],_,_,_,Flag,[]) :- Flag > 0.
term_two_indices_reducible([term(P,_)|T],Pred,I,ISize,Flag,Pos) :-
	primary_two_indices_reducible(P,Pred,I,ISize,Flag1,Pos1),
	Flag2 is Flag1+Flag,
	term_two_indices_reducible(T,Pred,I,ISize,Flag2,Pos2),
	union(Pos1,Pos2,Pos).

primary_two_indices_reducible([P],F/A,I,ISize,1,Pos) :- % direct recursion
	userfunc(P), functor(P,F,N), arg(1,P,Arity),
	normal_form(A,Arity), arg(2,P,Index),
	normal_form(I,Index),
	two_arg_diff(3,N,P,ISize,0,Count,[],Pos), Count > 0. 
primary_two_indices_reducible([P],F/_,_,_,0,[]) :-	% indirect recursion
	userfunc(P), functor(P,F1,_), F1 \== F.
primary_two_indices_reducible([P],F/A,_,_,0,[]) :-	% indirect recursion
	userfunc(P), functor(P,F,_), arg(1,P,Arity),
	general_form(Arity,GArity), GArity \== A.
primary_two_indices_reducible([P],F/A,I,ISize,1,Pos) :- % direct recursion
	P = sum(_,_,_,Expr),
	Expr = expr([term([P1],_)],_),
	userfunc(P1), functor(P1,F,N), arg(1,P1,Arity),
	normal_form(A,Arity), arg(2,P1,OPos),
	normal_form(I,OPos),
	two_arg_diff(3,N,P1,ISize,0,Count,[],Pos), Count > 0.

%
%  Test if the number of different arguments is at most 2.
%
two_arg_diff(M,N,_,[],Count,Count,Pos,Pos) :- M > N, Count =< 2.
two_arg_diff(M,N,F,[S|ISize],Count,FCount,Pos,FPos) :- M =< N,
	arg(M,F,Arg), normal_form(S,Arg),!,
	M1 is M+1,
	two_arg_diff(M1,N,F,ISize,Count,FCount,Pos,FPos).
two_arg_diff(M,N,F,[S|ISize],0,Count,Pos,FPos) :- M =< N,
	arg(M,F,Arg), normal_form(S,S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	two_arg_diff(M1,N,F,ISize,1,Count,[M|Pos],FPos).
two_arg_diff(M,N,F,[S|ISize],1,Count,Pos,FPos) :- M =< N,
	arg(M,F,Arg), normal_form(S,S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	two_arg_diff(M1,N,F,ISize,2,Count,[M|Pos],FPos).

adjust_pos([Pos1,Pos2],[Pos1,Pos2]) :- Pos1 < Pos2, !.
adjust_pos([Pos1,Pos2],[Pos2,Pos1]).

reduce_two_indices_exprs(Equ,Pred,I,IS,Pos,NEqu) :-
	Pos = [Pos1,Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	ith_list_element(POS1,IS,Var1),
	ith_list_element(POS2,IS,Var2),
	reduce_two_indices_exprs(Equ,Pred,I,Var1,Var2,Pos,NEqu).

reduce_two_indices_exprs([],_,_,_,_,_,[]).
reduce_two_indices_exprs([equ(Num,_,OC)|Equ],Pred,I,Var1,Var2,Pos,
		[equ(Num,[Var1,Var2],ROC)|OCs]) :-
	reduce_two_indices_expr(OC,Pred,I,Pos,ROC),
	reduce_two_indices_exprs(Equ,Pred,I,Var1,Var2,Pos,OCs).

reduce_two_indices_expr(expr(T,F),Pred,I,Pos,expr(ROC,F)) :-
	reduce_two_indices_term(T,Pred,I,Pos,ROC).

reduce_two_indices_term([],_,_,_,[]).
reduce_two_indices_term([term(P,F)|T],Pred,I,Pos,[term(NP,F)|NT]) :-
	reduce_two_indices_primary(P,Pred,I,Pos,NP),
	reduce_two_indices_term(T,Pred,I,Pos,NT).

reduce_two_indices_primary([P],F/A,I,Pos,[R]) :- % direct recursion
	userfunc(P), functor(P,F,_), arg(1,P,Arity),
	normal_form(A,Arity),
	arg(2,P,Index),
	normal_form(I,Index),
	Pos = [Pos1,Pos2], functor(R,F,2), 
	arg(Pos1,P,Arg1), arg(1,R,Arg1),
	arg(Pos2,P,Arg2), arg(2,R,Arg2).
reduce_two_indices_primary([P],F/_,_,_,[P]) :-	% indirect recursion
	userfunc(P), functor(P,F1,_),
	F1 \== F.
reduce_two_indices_primary([P],F/A,_,_,[P]) :-	% indirect recursion
	userfunc(P), functor(P,F,_), arg(1,P,Arity),
	general_form(Arity,GArity),
	GArity \== A.
reduce_two_indices_primary([P],F/A,I,Pos,[R]) :- % direct recursion
	P = sum(Index,Lower,Upper,Expr),
	Expr = expr([term([P1],FS)],CS),
	userfunc(P1), functor(P1,F,_), arg(1,P1,Arity),
	normal_form(A,Arity),
	arg(2,P1,OPos),
	normal_form(I,OPos),
	Pos = [Pos1,Pos2], functor(R,F,2), 
	arg(Pos1,P1,Arg1), arg(1,R1,Arg1),
	arg(Pos2,P1,Arg2), arg(2,R1,Arg2),
	RExpr = expr([term([R1],FS)],CS),
	R = sum(Index,Lower,Upper,RExpr).

%
%  Test if boundary conditions are one index reducible. If it is,
%  the reduced conditions are returned.
%
boundary_condition_one_index_reducible([],_,_,[]).
boundary_condition_one_index_reducible([equ(N,S,C)|Equ],IS,Pos,
		[equ(N,I,C)|RBC]) :-
	bc_one_index_reducible(S,IS,Pos,1),
	ith_list_element(Pos,S,I),
	boundary_condition_one_index_reducible(Equ,IS,Pos,RBC).

bc_one_index_reducible([],[],_,_).
bc_one_index_reducible([S|S1],[S|S2],Pos,I) :-
	Pos \== I, I1 is I+1,
	bc_one_index_reducible(S1,S2,Pos,I1).
bc_one_index_reducible([_|S1],[_|S2],Pos,Pos) :-
	I1 is Pos+1,
	bc_one_index_reducible(S1,S2,Pos,I1).

boundary_condition_two_indices_reducible([],_,_,[]).
boundary_condition_two_indices_reducible([equ(N,S,C)|Equ],IS,Pos,
		[equ(N,[I1,I2],C)|RBC]) :-
	bc_two_indices_reducible(S,IS,Pos,1),
	Pos = [Pos1,Pos2],
	ith_list_element(Pos1,S,I1),
	ith_list_element(Pos2,S,I2),
	boundary_condition_two_indices_reducible(Equ,IS,Pos,RBC).

bc_two_indices_reducible([],[],_,_).
bc_two_indices_reducible([H1|S1],[H2|S2],Pos,I) :-
	(member(Pos,I) ->
		true;
		H1 == H2),
	I1 is I+1,
	bc_two_indices_reducible(S1,S2,Pos,I1).

boundary_conds([],[]).
boundary_conds([equ(_,Index,Ival)|Equ],[val(NIndex,Ival)|Ivalue]) :-
	normal_form(Index,NIndex),
	boundary_conds(Equ,Ivalue).

%
%  Compute the boundary condition for a size difference equation.
%  Main task is to estimate the minimum unifiable input term size.
%
size_boundary_cond(equ(Num,Var,Val),ST,Pred,Pos,[val(Iind,Ival)]) :-
	min_unifiable_term_size(ST,Pred,Num,Pos,MinSize),
	(integer(MinSize) ->
		(normal_form(MinSize,Iind),
		 general_form(Val,Nval),
		 substitute(Nval,Var,MinSize,Sval),
		 normal_form(Sval,Ival));
		(Iind = bot,
		 Ival = bot)).

%
%  Find out the minimum unifiable size of a term in clause CNum at position Pos.
%
min_unifiable_term_size(ST,Pred,CNum,Pos,MinSize) :-
	find_symbol_field(ST,Pred,measure,Measure),
	find_symbol_field(ST,Pred,(mode),Mode),
	real_pos(Mode,Pos,1,RealPos),
	ith_list_element(RealPos,Measure,M),
	find_symbol_field(ST,Pred,clause,Clauses),
	ith_list_element(CNum,Clauses,Clause),
	min_unifiable_term_size1(M,Clause,RealPos,MinSize).

%
real_pos([+|_],1,Pos,Pos).
real_pos([+|Mode],PrincipalPos,I,Pos) :-
	PrincipalPos > 1,
	PrincipalPos1 is PrincipalPos-1,
	I1 is I+1,
	real_pos(Mode,PrincipalPos1,I1,Pos).
real_pos([-|Mode],PrincipalPos,I,Pos) :-
	I1 is I+1,
	real_pos(Mode,PrincipalPos,I1,Pos).

%
min_unifiable_term_size1(int,Clause,PrincipalPos,Size) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Head = Clause),
	arg(PrincipalPos,Head,Term),
	(var(Term) ->
		(Type =:= 2 ->
			(arg(2,Clause,Body),
			 min_unifiable_int(Body,Term,Size));
			Size = 0);	% may unify with any integer
		Size = bot).

min_unifiable_int(Lit,Var,Size) :-
	nonsequence(Lit),
	functor(Lit,F,N),
	(comparison_op(F/N) ->
		min_lit_int(Lit,Var,Size);
		Size = 0).
min_unifiable_int((Lit,Body),Var,Size) :-
	functor(Lit,F,N),
	(comparison_op(F/N) ->
		min_lit_int(Lit,Var,S);
		S = 0),
	min_unifiable_int(Body,Var,Sizes),
	maximum(S,Sizes,Size).

min_lit_int(Lit,Var,Size) :-
	arg(1,Lit,Arg1),
	Arg1 == Var,!,
	arg(2,Lit,Arg2),
	(integer(Arg2) ->
		(functor(Lit,F,_),
		 min_lit_int1(F,Arg2,Size));
		Size = 0).
min_lit_int(Lit,Var,Size) :-
	arg(2,Lit,Arg2),
	Arg2 == Var,!,
	arg(1,Lit,Arg1),
	(integer(Arg1) ->
		(functor(Lit,F,_),
		 min_lit_int2(F,Arg1,Size));
		Size = 0).
min_lit_int(_,_,0).

min_lit_int1((=:=),N,N).
min_lit_int1((==),N,N).
min_lit_int1((>=),N,N).
min_lit_int1((>),N,M) :- M is N+1.
min_lit_int1((=<),_,0).
min_lit_int1((<),_,0).
min_lit_int1((=\=),_,0).
min_lit_int1((\==),_,0).

min_lit_int2((=:=),N,N).
min_lit_int2((==),N,N).
min_lit_int2((>=),_,0).
min_lit_int2((>),_,0).
min_lit_int2((=<),N,N).
min_lit_int2((<),N,M) :- M is N+1.
min_lit_int2((=\=),_,0).
min_lit_int2((\==),_,0).

min_unifiable_term_size1(length,Clause,Pos,Size) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Head = Clause),
	arg(Pos,Head,Term),
	min_length(Term,Size).

min_length(Term,0) :- var(Term).
min_length(Term,Size) :- nonvar(Term), Term = [_|L],
	min_length(L,S), Size is S+1.

min_unifiable_term_size1(depth(R),Clause,PrinciplePos,Size) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Head = Clause),
	arg(PrinciplePos,Head,Term),
	min_depth(Term,R,Size).

min_depth(Term,_,0) :- var(Term).
min_depth(Term,R,Size) :- compound(Term), 
	functor(Term,_,N), 
	min_depth1(N,Term,R,S), Size is S+1.

min_depth1(0,_,_,0).
min_depth1(N,Term,R,Size) :- N > 0,
	(member(R,N) ->
		(arg(N,Term,Arg),
		 min_depth(Arg,R,S));
		S = 0),
	N1 is N-1,
	min_depth1(N1,Term,R,Ss),
	max(S,Ss,Size).

min_unifiable_term_size1(size,Clause,PrinciplePos,Size) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Head = Clause),
	arg(PrinciplePos,Head,Term),
	min_size(Term,Size).

min_size(Term,1) :- var(Term).
min_size(Term,Size) :- compound(Term), 
	functor(Term,_,N), 
	min_size1(N,Term,S), Size is S+1.

min_size1(0,_,0).
min_size1(N,Term,Size) :- N > 0,
	arg(N,Term,Arg),
	min_size(Arg,S),
	N1 is N-1,
	min_size1(N1,Term,Ss),
	add(S,Ss,Size).

%
%  Solve the complexity non-difference equations.
%
solve_comp_non_diff_equs(Equ,Sol) :-
	comp_list(Equ,Comp),
	maximum_list(Comp,Sol).

comp_list([],[]).
comp_list([equ(_,_,C)|Equ],[C|Comp]) :-
	comp_list(Equ,Comp).

%
%  Compute the maximum of a list of normal form expressions.
%
maximum_list([],Zero) :- normal_form(0,Zero).
maximum_list([S|Size],Sol) :-
	maximum_list(Size,Sols),
	%general_form(Sols,GSol),
	%write(GSol),nl,
	max_expr(Sols,S,Sol).

%
%  Fetch the input sizes for the clauses of a predicate.
%
input_sizes([],_,[]).
input_sizes([S|Size],Mode,[IS|ISize]) :-
	input_size(Mode,S,IS),
	input_sizes(Size,Mode,ISize).

%
%  Fetch the input sizes for a clause.
%
input_size([],_,[]).
input_size([(+)|Mode],[S|Size],[S|ISize]) :-
	input_size(Mode,Size,ISize).
input_size([(-)|Mode],[_|Size],ISize) :-
	input_size(Mode,Size,ISize).

%
%  Fetch the nth output sizes for the clauses of a predicate.
%
output_sizes([],_,[]).
output_sizes([S|Size],N,[O|OSize]) :-
	ith_list_element(N,S,O),
	output_sizes(Size,N,OSize).

%
%  Check if the input sizes of the clauses of a predicate is consistant.
%  recursive and nonrecursive predicates are considered differently.
%
consistent_input_sizes(Pred,ST,Component,ISize,Size) :-
	(recursive_predicate(Pred,ST,Component) ->
		(find_symbol_field(ST,Pred,clause,Clause),
		 recursive_consistent_input(Clause,Component,ISize,0,Size));
		nonrecursive_consistent_input(ISize,0,Size)).
	
%
%  If the predicate is recursive, the input sizes of recursive clauses must
%  be the same.
%
recursive_consistent_input(Clauses,_,_,_,_) :- var(Clauses).
recursive_consistent_input(Clauses,Component,[IS|ISize],0,Size) :-
	nonvar(Clauses),
	Clauses = [C|Clause],
	(recursive_clause(C,Component) ->
		(Size = IS,
		 recursive_consistent_input(Clause,Component,ISize,1,Size));
		recursive_consistent_input(Clause,Component,ISize,0,Size)).
recursive_consistent_input(Clauses,Component,[IS|ISize],1,Size) :-
	nonvar(Clauses),
	Clauses = [C|Clause],
	(recursive_clause(C,Component) ->
		(Size == IS,
		 recursive_consistent_input(Clause,Component,ISize,1,Size));
		recursive_consistent_input(Clause,Component,ISize,1,Size)).

%
%  If the predicate is nonrecursive, the input sizes for all the clauses
%  must be the same.
%
nonrecursive_consistent_input([],_,_).
nonrecursive_consistent_input([IS|ISize],0,Size) :-
	Size = IS,
	nonrecursive_consistent_input(ISize,1,Size).
nonrecursive_consistent_input([IS|ISize],1,Size) :-
	Size == IS,
	nonrecursive_consistent_input(ISize,1,Size).

%
%  Construct the complexity equations.
%
construct_comp_equs([],_,_,[]).
construct_comp_equs([I|ISize],[C|Comp],ClauseNum,[equ(ClauseNum,I,NC)|Equs]) :-
	normal_form(C,NC),
	ClauseNum1 is ClauseNum+1,
	construct_comp_equs(ISize,Comp,ClauseNum1,Equs).

classify_equs([],[],[]).
classify_equs([E|Equs],DEqu,BEqu) :-
	E = equ(_,_,O),
	%general_form(O,GO),
	%write(GO),nl,
	(diff_equ(O) ->
		(DEqu = [E|DEqus],
		 BEqu = BEqus);
		(DEqu = DEqus,
		 BEqu = [E|BEqus])),
	classify_equs(Equs,DEqus,BEqus).
%
%  Combine the input sizes and output sizes.
%
combine_size([],_,_,[]).
combine_size([+|Mode],[I|Isize],Osize,[S|Size]) :-
	simplification(I,S),
	combine_size(Mode,Isize,Osize,Size).
combine_size([-|Mode],Isize,[O|Osize],[S|Size]) :-
	simplification(O,S),
	combine_size(Mode,Isize,Osize,Size).

%
%  Construct a list of dummy sizes.
%
dummy_size([],_,[]).
dummy_size([+|Mode],I,[($(0,I))|Size]) :-
	I1 is I+1,
	dummy_size(Mode,I1,Size).
dummy_size([-|Mode],I,[inf|Size]) :-
	I1 is I+1,
	dummy_size(Mode,I1,Size).

%
%  Test if an equation is a difference equation or not.
%
diff_equ(expr(T,_)) :-
	T \== [].

non_diff_equ(expr([],_)).
%non_diff_equ(bot).
non_diff_equ(inf).

%
indirect_recursion([],_).
indirect_recursion([equ(_,_,E)|DE],Pred) :-
	%general_form(E,GE),
	%write(GE),nl,
	indirect_recursion_expr(E,Pred),
	indirect_recursion(DE,Pred).

indirect_recursion_expr(expr(T,_),Pred) :-
	indirect_recursion_term(T,Pred).

indirect_recursion_term([],_).
indirect_recursion_term([term(P,_)|Ts],Pred) :-
	indirect_recursion_primary(P,Pred),
	indirect_recursion_term(Ts,Pred).

indirect_recursion_primary([],_).
indirect_recursion_primary([DVar|P],F/N) :-
	userfunc(DVar),
	functor(DVar,F1,_), F1 \== F, !,
	indirect_recursion_primary(P,F/N).
indirect_recursion_primary([DVar|P],F/N) :-
	userfunc(DVar),
	functor(DVar,F,M), M > 2,
	arg(1,DVar,Arity), general_form(Arity,A), A \== N,
	indirect_recursion_primary(P,F/N).
%
%  binding.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for computing the binding pattern.
%  

%
%  Compute the number of head input bindings for relation size analysis.
%
relation_head_input(Head,BT,ST,Clause,Adg,Gvars,Ldg,Sol) :-
	new_lit(+,Lit),
	functor(Head,F,A),
	gen_literal_iopos(Adg,F/A,0,(+),Pos),
	pos_var(Pos,Head,Vars),
	head_input_tuple_size(Vars,BT,ST,Clause,Gvars,Sol),
	insert_ldg_field(Ldg,Lit,relation,Sol),
	insert_vars_binding(Vars,BT,ST,Clause,Gvars,relation,Sol).
	%insert_ldg_field(Ldg,Lit,redge,Mvars).

%
%  Compute the number of head bindings for relation size analysis.
%
relation_head(Head,Clause,Gvars,Ldg,TestLits,Binding) :-
	functor(Head,F,A),
	gen_literal_pos(F/A,0,Pos),
	pos_var(Pos,Head,Vars),
	var_def_list(Vars,Gvars,Lvars),
	vars_binding(Lvars,Gvars,Ldg,redge,Clause,Binding1),
	filter_effect(TestLits,Vars,Binding1,Binding).

%
%  Compute the number of head input bindings for solution size analysis.
%
solution_head_input(Head,BT,ST,Clause,Adg,Gvars,Ldg) :-
	new_lit(+,Lit),
	insert_ldg_field(Ldg,Lit,det,1),
	functor(Head,F,A),
	gen_literal_iopos(Adg,F/A,0,(+),Pos),
	pos_var(Pos,Head,Vars),
	insert_vars_binding(Vars,BT,ST,Clause,Gvars,det,1).
	%insert_ldg_field(Ldg,Lit,sedge,Mvars).

%
%  Compute the number of head output bindings for solution size analysis.
%
solution_head_output(Head,Clause,Adg,Gvars,Ldg,TestLits,Binding) :-
	functor(Head,F,A),
	gen_literal_iopos(Adg,F/A,0,(-),Pos),
	pos_var(Pos,Head,Vars),
	var_def_list(Vars,Gvars,Lvars),
	vars_binding(Lvars,Gvars,Ldg,sedge,Clause,Binding1),
	filter_effect(TestLits,Vars,Binding1,Binding).

%
%  Compute the number of body input and output bindings.
%
body_binding(LitNum,Lit,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,Cuts,TestLits,RSol) :-
	nonsequence(Lit),
	literal_binding(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,
		Cuts,TestLits,RSol).
body_binding(LitNum,(Lit,Body),BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,
		Cuts,TestLits,RSol) :-
	literal_binding(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,
		Cuts,TestLits,RSol),
	(Lit == (!) ->
		Cuts1 is Cuts-1;
		Cuts1 = Cuts),
	LitNum1 is LitNum+1,
	body_binding(LitNum1,Body,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,
		Cuts1,TestLits,RSol).
 
%
%  Compute the number of literal input and output bindings.
%
literal_binding(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,Cuts,
		TestLits,RSol) :-
	functor(Lit,F,A),
	(second_order_predicate(F/A) ->
		literal_binding_1(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,
			Gvars,Ldg,Type,Cuts,TestLits,RSol);
		literal_binding_2(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,
			Gvars,Ldg,Type,Cuts,TestLits,RSol)).

literal_binding_1(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,Cuts,
		TestLits,RSol) :-
	second_order_predicate_pred_arg(Lit,Lit1),
	arg(2,Clause,Body),
	second_order_predicate_pred_num(Body,LitNum,Num),
	literal_input_binding(Lit1,Num,BT,ST,Clause,Adg,Gvars,Ldg,Type,
			      TestLits,Binding,_),
	(Type == relation ->
		literal_output_binding_rel_1(Lit1,Num,Lit,LitNum,BT,ST,Comp,
			Clause,Size,Adg,Gvars,Ldg,RSol,Binding);
		literal_output_binding_det_1(Lit1,Num,Lit,LitNum,BT,ST,Comp,
			Clause,Size,Adg,Gvars,Ldg,RSol,Cuts,Binding)).

literal_binding_2(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,Type,Cuts,
		TestLits,RSol) :-
	literal_input_binding(Lit,LitNum,BT,ST,Clause,Adg,Gvars,Ldg,Type,
			      TestLits,Binding,Subsume),
	(Type == relation ->
		literal_output_binding_rel(Lit,LitNum,BT,ST,Comp,Clause,Size,
			Adg,Gvars,Ldg,RSol,Binding,Subsume);
		literal_output_binding_det(Lit,LitNum,BT,ST,Comp,Clause,Size,
			Adg,Gvars,Ldg,RSol,Cuts,Binding,Subsume)).

%  Compute the number of literal input bindings.
%
literal_input_binding(Lit,LitNum,BT,ST,Clause,Adg,Gvars,Ldg,Type,TestLits,Binding,
		SubsumeLit) :-
	functor(Lit,F,A),
	LitName = (F/A),
	gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
	pos_var(Pos,Lit,Vars),
	var_def_list(Vars,Gvars,Lvars),
	(Type == relation ->
		vars_binding(Lvars,Gvars,Ldg,redge,Clause,Binding2);
		vars_binding(Lvars,Gvars,Ldg,sedge,Clause,Binding2)),
	filter_effect(TestLits,Vars,Binding2,Binding1),
	literal_property(BT,ST,LitName,relation,Rel),
	minimum(Binding1,Rel,Binding),
	(opt_cond1(Lvars,Clause,Vars,Subsume) ->
		(opt_cond2(Subsume,Ldg,Binding1,Type) ->
			(opt_cond3(Binding1,Binding) ->
				SubsumeLit = Subsume;
				true);
			true);
		true).

%
%  Compute the number of literal output bindings.
%
literal_output_binding_rel(Lit,LitNum,BT,ST,_,Clause,Size,Adg,Gvars,Ldg,
		_,InBinding,SubsumeLit) :-
	functor(Lit,F,A),
	LitName = (F/A),
	literal_output_comp(LitName,LitNum,1,BT,ST,[],Adg,det,[],Sol),
	%write(Sol),nl,
	normalize_solution_function(Sol,LitName,LitNum,BT,ST,[],
			Clause,Adg,Gvars,Size,[],Sol1),
	%write(Sol1),nl,
	multiply(InBinding,Sol1,Sol2),
	%write(Sol2),nl,
	literal_property(BT,ST,LitName,relation,Rel),
	%write(Rel),nl,
	minimum(Sol2,Rel,Binding),
	%write(Binding),nl,
	gen_literal_iopos(Adg,LitName,LitNum,(-),Pos),
	pos_var(Pos,Lit,Vars),
	new_lit(LitNum,NLit),
	%find_ldg_field(Ldg,NLit,succ,Succ),
	%min_lit_binding(Succ,Clause,BT,ST,Adg,Vars,Binding,MinBinding),
	insert_ldg_field(Ldg,NLit,relation,Binding),
	insert_vars_binding(Vars,BT,ST,Clause,Gvars,relation,Binding),
	%insert_binding_coverage(Mvars,Gvars,Adg,Ldg,LitName,LitNum,Clause,
	%	relation),
	(nonvar(SubsumeLit) ->
		(opt_cond4(Sol2,Binding) ->
			(find_ldg_field(Ldg,SubsumeLit,redge,SLits),
			 insert_ldg_field(Ldg,NLit,redge,[SubsumeLit|SLits]));
			true);
		true).

min_lit_binding(Succ,_,_,_,_,_,MinBinding,MinBinding) :-
	var(Succ).
min_lit_binding(Succ,Clause,BT,ST,Adg,Vars,Binding,MinBinding) :-
	nonvar(Succ),
	Succ = [Lit|Succs],
	new_lit(LitNum,Lit),
	(integer(LitNum) ->
		(ith_clause_literal(LitNum,Clause,Lit1),
		 functor(Lit1,F1,A1),
		 gen_literal_iopos(Adg,F1/A1,LitNum,(+),Pos),
		 pos_var(Pos,Lit1,Var1),
		 (opened_set_inclusion(Vars,Var1) ->
		 	(literal_property(BT,ST,F1/A1,relation,Rel),
			 minimum(Binding,Rel,Binding1));
			Binding1 = Binding));
		Binding1 = Binding),
	simplification(Binding1,NBinding),
	min_lit_binding(Succs,Clause,BT,ST,Adg,Vars,NBinding,MinBinding).

literal_output_binding_rel_1(Lit,LitNum,OLit,OLitNum,BT,ST,_,Clause,Size,Adg,
		Gvars,Ldg,_,InBinding) :-
	arg(3,OLit,Arg3),
	term_var(Arg3,Var3),
	insert_ldg_field(Ldg,NLit,relation,InBinding),
	insert_vars_binding(Var3,BT,ST,Clause,Gvars,relation,InBinding).
/*
	functor(Lit,F,A),
	LitName = (F/A),
	literal_output_comp(LitName,LitNum,1,BT,ST,[],Adg,det,[],Sol),
	%write(Sol),nl,
	normalize_solution_function(Sol,LitName,LitNum,BT,ST,[],
			Clause,Adg,Gvars,Size,[],Sol1),
	literal_property(BT,ST,LitName,det,[Sol1]),
	%write(Sol1),nl,
	multiply(InBinding,Sol1,Sol2),
	%write(Sol2),nl,
	literal_property(BT,ST,LitName,relation,Rel),
	minimum(Sol2,Rel,Binding),
	%write(Binding),nl,
	%
	gen_literal_iopos(Adg,LitName,LitNum,(-),Pos),
	pos_var(Pos,Lit,Vars),
	arg(1,OLit,Arg1),
	term_var(Arg1,Var1),
	arg(3,OLit,Arg3),
	term_var(Arg3,Var3),
	new_lit(OLitNum,NLit),
	(opened_set_equivalent(Var1,Vars) ->
		(insert_ldg_field(Ldg,NLit,relation,InBinding),
		 insert_vars_binding(Var3,BT,ST,Clause,Gvars,relation,InBinding));
		(insert_ldg_field(Ldg,NLit,relation,Binding),
		 insert_vars_binding(Var3,BT,ST,Clause,Gvars,relation,Binding))).
*/

%
literal_output_binding_det_1(Lit,LitNum,OLit,OLitNum,BT,ST,Comp,Clause,Size,
		Adg,Gvars,Ldg,RSol,Cuts,InBinding) :-
	arg(3,OLit,Arg3),
	term_var(Arg3,Var3),
	insert_ldg_field(Ldg,NLit,det,InBinding),
	insert_vars_binding(Var3,BT,ST,Clause,Gvars,det,InBinding).
/*
	functor(Lit,F,A),
	LitName = (F/A),
	(Cuts > 0 ->
		Binding = 1;
		(literal_output_comp(LitName,LitNum,1,BT,ST,Comp,Adg,det,RSol,
			Sol),
		 %write(Sol),nl,
		 normalize_solution_function(Sol,LitName,LitNum,BT,ST,Comp,
			Clause,Adg,Gvars,Size,RSol,Sol1),
		 %write(Sol1),nl,
		 multiply(InBinding,Sol1,Sol2),
		 literal_property(BT,ST,LitName,relation,Rel),
		 minimum(Sol2,Rel,Binding))),
	%
	gen_literal_iopos(Adg,LitName,LitNum,(-),Pos),
	pos_var(Pos,Lit,Vars),
	arg(1,OLit,Arg1),
	term_var(Arg1,Var1),
	arg(3,OLit,Arg3),
	term_var(Arg3,Var3),
	new_lit(OLitNum,NLit),
	(opened_set_equivalent(Var1,Vars) ->
		(insert_ldg_field(Ldg,NLit,det,1),
		 insert_vars_binding(Var3,BT,ST,Clause,Gvars,det,1));
		(insert_ldg_field(Ldg,NLit,det,Binding),
		 insert_vars_binding(Var3,BT,ST,Clause,Gvars,det,Binding))).
*/

%
literal_output_binding_det(Lit,LitNum,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,
		RSol,Cuts,InBinding,SubsumeLit) :-
	functor(Lit,F,A),
	LitName = (F/A),
	(Cuts > 0 ->
		Binding = 1;
		(literal_output_comp(LitName,LitNum,1,BT,ST,Comp,Adg,det,RSol,
			Sol),
		 %write(Sol),nl,
		 normalize_solution_function(Sol,LitName,LitNum,BT,ST,Comp,
			Clause,Adg,Gvars,Size,RSol,Sol1),
		 %write(Sol1),nl,
		 multiply(InBinding,Sol1,Sol2),
		 %write(Sol2),nl,
		 literal_property(BT,ST,LitName,relation,Rel),
		 %write(Rel),nl,
		 minimum(Sol2,Rel,Binding))),
	%write(Binding),nl,
	gen_literal_iopos(Adg,LitName,LitNum,(-),Pos),
	pos_var(Pos,Lit,Vars),
	new_lit(LitNum,NLit),
	%find_ldg_field(Ldg,NLit,succ,Succ),
	%min_lit_binding(Succ,Clause,BT,ST,Adg,Vars,Binding,MinBinding),
	insert_ldg_field(Ldg,NLit,det,Binding),
	insert_vars_binding(Vars,BT,ST,Clause,Gvars,det,Binding),
	%insert_binding_coverage(Mvars,Gvars,Adg,Ldg,LitName,LitNum,Clause,det),
	(nonvar(SubsumeLit) ->
		(opt_cond4(Sol2,Binding) ->
			(find_ldg_field(Ldg,SubsumeLit,sedge,SLits),
			 insert_ldg_field(Ldg,NLit,sedge,[SubsumeLit|SLits]));
			true);
		true).

%
%  Normalize a solution function of a literal.
%
normalize_solution_function(LitSol,LitName,LitNum,BT,ST,Comp,Clause,Adg,
			    Gvars,Size,RSol,Sol) :-
	gen_clause_pos(Adg,PosSet),
	(recursive_clause(Clause,Comp) ->
		(ith_clause_literal(0,Clause,Lit),
		 functor(Lit,F,N),
		 find_symbol_field(ST,F/N,size,ISz));
		ISz = Size),
	gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
	init_normalize_queue(Pos,QHd,QTl),
	normalize(LitSol,QHd,QTl,BT,ST,[],Clause,Adg,Gvars,PosSet,ISz,RSol,Sol).

%
%  Estimate the number of input tuples for the head.
%
head_input_tuple_size(Vars,_,_,_,_,1) :-
	var(Vars).
head_input_tuple_size(Vars,BT,ST,Clause,Gvars,Size) :-
	nonvar(Vars),
	use_list(Vars,Gvars,UseList),
%	structure_isort(UseList,1,SUseList),
	lit_relation_size(Clause,BT,ST,LitList),
	head_active_lit(UseList,LitList,Clause,[],ActiveList),
	head_size(ActiveList,2,Size).

%
%  Collect the set of uses for the input variables in the head.
%
use_list(Vars,_,[]) :-
	var(Vars).
use_list(Vars,Gvars,[use(Len,UsePos)|UseList]) :-
	nonvar(Vars),
	Vars = [V|VList],
	find_gvars_field(Gvars,V,use,UsePos),
	length(UsePos,Len),
	use_list(VList,Gvars,UseList).

%
%  Perform a quick sort in increasing order on a list of structure by using
%  the ith argument as the key.
%
structure_isort([],_,[]).
structure_isort([U|UseList],I,SUseList) :-
	structure_split(UseList,U,I,Small,Large),
	structure_isort(Small,I,SList),
	structure_isort(Large,I,LList),
	append(SList,[U|LList],SUseList).

structure_split([],_,_,[],[]).
structure_split([V|UList],U,I,Small,Large) :-
	arg(I,V,VKey),
	arg(I,U,UKey),
	(VKey > UKey ->
		(Large = [V|NLarge],
		 Small = NSmall);
		(Large = NLarge,
		 Small = [V|NSmall])),
	structure_split(UList,U,I,NSmall,NLarge).

%
%  Collect the relation size for the literals in a clause.
%  Only non-infinite relation literals are collected.
%
lit_relation_size(Clause,BT,ST,LitList) :-
	clause_type(Clause,Type),
	lit_relation_size(Type,Clause,BT,ST,LitList).

lit_relation_size(2,(_:-Body),BT,ST,LitList) :-
	body_relation_size(Body,1,BT,ST,LitList).
lit_relation_size(3,_,_,_,[]).

body_relation_size(Lit,LitNum,BT,ST,LitList) :-
	nonsequence(Lit),
	functor(Lit,F,N),
	(second_order_predicate(F/N) ->
		second_order_predicate_pred_arg(Lit,NLit);
		NLit = Lit),
	functor(NLit,NF,NN),
	literal_property(BT,ST,NF/NN,relation,Rel),
	(Rel == inf ->
		LitList = [];
		LitList = [lit(LitNum,Rel)]).
body_relation_size((Lit,Body),LitNum,BT,ST,LitList) :-
	functor(Lit,F,N),
	(second_order_predicate(F/N) ->
		second_order_predicate_pred_arg(Lit,NLit);
		NLit = Lit),
	functor(NLit,NF,NN),
	literal_property(BT,ST,NF/NN,relation,Rel),
	(Rel == inf ->
		LitList = LitLists;
		LitList = [lit(LitNum,Rel)|LitLists]),
	LitNum1 is LitNum+1,
	body_relation_size(Body,LitNum1,BT,ST,LitLists).

%
head_active_lit(_,[],_,_,[]).	%  fact 
head_active_lit([],LitList,_,ActiveList,ActiveList) :-
	LitList \== [].
head_active_lit([use(_,[])|_],LitList,_,_,[]) :- LitList \== []. % dangling var
head_active_lit([use(_,U)|UseList],LitList,Clause,AList,ActiveList) :-
	LitList \== [],
	U \== [],
	least_lit(U,LitList,Clause,_,inf,Lit),
	(var(Lit) ->
		ActiveList = [];  % all uses are in the head or infinite lit
		(arg(1,Lit,LitNum),
		 (structure_member(AList,1,LitNum) ->
			AList1 = AList;		% vars appear in same lit 
			AList1 = [Lit|AList]),
		 head_active_lit(UseList,LitList,Clause,AList1,ActiveList))).


/*
head_active_lit([use(_,U)|UseList],LitList,AList,ActiveList) :-
	LitList \== [],
	U \== [],
	(structure_list_member(U,1,AList,1) ->
		head_active_lit(UseList,LitList,AList,ActiveList);
		(least_lit(U,LitList,_,inf,Lit),
		 (var(Lit) ->
			ActiveList = [];  % all uses are in the head 
		 	(reduce_use_list(UseList,Lit,UseLists),
		 	 ALists = [Lit|AList],
			 head_active_lit(UseLists,LitList,ALists,ActiveList))))).
*/

%
structure_list_member([S|SList],I,Structure,J) :-
	arg(I,S,Arg),
	(structure_member(Structure,J,Arg) ->
		true;
		structure_list_member(SList,I,Structure,J)).

structure_member([S|_],I,M) :-
	arg(I,S,Arg),
	Arg == M.
structure_member([S|SList],I,M) :-
	arg(I,S,Arg),
	Arg \== M,
	structure_member(SList,I,M).

%
least_lit([],_,_,Lit,_,Lit).
least_lit([U|UList],LitList,Clause,Lit,Rel,MLit) :-
	pos_litnum(U,LitNum),
	(LitNum > 0 ->
		(arg(2,Clause,Body),
		 number_of_literals(Body,1,Num),
		 NLitNum is LitNum-Num);
		NLitNum = LitNum),
	(structure_find(LitList,1,NLitNum,NLit) ->
		(arg(2,NLit,NRel),
		 minimum(Rel,NRel,MRel),
		 (MRel == NRel ->
			least_lit(UList,LitList,Clause,NLit,NRel,MLit);
			least_lit(UList,LitList,Clause,Lit,Rel,MLit)));
		(/* LitNum == 0 or an infinite relation lit*/
		 minimum(Rel,inf,NRel),
		 least_lit(UList,LitList,Clause,Lit,NRel,MLit))).

%
structure_find([L|_],I,LitNum,L) :-
	arg(I,L,Arg),
	Arg == LitNum, !.
structure_find([L|LitList],I,LitNum,Lit) :-
	arg(I,L,Arg),
	Arg \== LitNum,
	structure_find(LitList,I,LitNum,Lit).

%
reduce_use_list(UseList,lit(LitNum,_),UseLists) :-
	reduce_use_list1(UseList,LitNum,UseLists).

reduce_use_list1([],_,[]).
reduce_use_list1([U|UseList],Lit,NUseList) :-
	reduce_use_list1(UseList,Lit,UseLists),
	arg(2,U,Us),
	(structure_member(Us,1,Lit) ->
		NUseList = UseLists;
		NUseList = [U|UseLists]).

%
%  Compute the number of input tuples for the head by multiplying the
%  relation sizes of the active literals.
%
head_size([],_,inf).
head_size(S,I,Value) :-
	S \== [],
	structure_multiply(S,I,Value).

structure_multiply([],_,1).
structure_multiply([S|Structure],I,Value) :-
	arg(I,S,Val),
	structure_multiply(Structure,I,Values),
	multiply(Val,Values,Value).

%
%  Compute the number of bindings of a set of variables.
%
find_vars_binding(Vars,_,_,TBinding,TBinding) :-
	var(Vars).
find_vars_binding(Vars,Gvars,Type,TBinding,FBinding) :-
	nonvar(Vars),
	Vars = [Var|VList],
	find_gvars_field(Gvars,Var,Type,VBinding),
	multiply(TBinding,VBinding,TBinding1),
	find_vars_binding(VList,Gvars,Type,TBinding1,FBinding).

%
%  Compute the number of bindings for a set of variables.
%
insert_vars_binding(Vars,_,_,_,_,_,_) :-
	var(Vars).
insert_vars_binding(Vars,BT,ST,Clause,Gvars,Type,Default) :-
	nonvar(Vars),
	Vars = [Var|VList],
	insert_var_binding(Var,BT,ST,Clause,Gvars,Type,Default),
	insert_vars_binding(VList,BT,ST,Clause,Gvars,Type,Default).
/*
	(Default == Binding ->
		Mvars = [Var|MList];
		Mvars = MList),
*/

%
%  Insert the number of bindings of a variables into ground variable list.
%
insert_var_binding(Var,BT,ST,Clause,Gvars,Type,Default) :-
	find_gvars_field(Gvars,Var,use,Pos),
	min_use_binding(Pos,BT,ST,Clause,Binding1),
	minimum(Binding1,Default,Binding),
	simplification(Binding,SBinding),
	insert_gvars_field(Gvars,Var,Type,SBinding).

%
%  Compute the minimum number of bindings of a variable among its use 
%  positions.
%
min_use_binding([],_,_,_,inf).
min_use_binding([Pos|PList],BT,ST,Clause,Binding) :-
	pos_litnum(Pos,0),
	min_use_binding(PList,BT,ST,Clause,Binding).
min_use_binding([Pos|PList],BT,ST,Clause,Binding) :-
	pos_litnum(Pos,I),
	I > 0,
	arg(2,Clause,Body),
	number_of_literals(Body,1,Num),
	(I > Num ->
		NI is I-Num;
		NI = I),
	ith_clause_literal(NI,Clause,NLit),
	(I > Num ->
		second_order_predicate_pred_arg(NLit,Lit);
		Lit = NLit),
	functor(Lit,F,N),
	literal_property(BT,ST,F/N,relation,Rel),
	min_use_binding(PList,BT,ST,Clause,Binding1),
	minimum(Rel,Binding1,Binding).

%
%  Find the set of variables occurring at a set of positions.
%
pos_var([],_,_).
pos_var([Pos|PList],Lit,Vars) :-
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,Lit,Arg),
	term_var(Arg,Vars),
	pos_var(PList,Lit,Vars).

%
%  Find the set of variables occurring in a term.
%
term_var(Term,_) :-
	atomic(Term).
term_var(Term,Vars) :-
	var(Term),
	opened_set_insertion(Vars,Term).
term_var(Term,Vars) :-
	compound(Term),
	functor(Term,_,N),
	term_var(N,Term,Vars).

term_var(0,_,_).
term_var(N,Term,Vars) :-
	N > 0,
	arg(N,Term,Arg),
	term_var(Arg,Vars),
	N1 is N-1,
	term_var(N1,Term,Vars).

%
%  Insert a set of variables into a list.
%
insert_vars_list([],_).
insert_vars_list([Var|VList],List) :-
	insert_var_list(List,Var),
	insert_vars_list(VList,List).

%
%  Insert a variable into a list.
%
insert_var_list(List,Var) :-
	var(List),
	List = [Var|_].
insert_var_list([V|_],Var) :-
	V == Var.
insert_var_list([V|List],Var) :-
	V \== Var,
	insert_var_list(List,Var).

%
%  Establish the relations of binding coverage.
%
insert_binding_coverage(Vars,Gvars,Adg,Ldg,LitName,LitNum,Clause,Type) :-
	gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
	pos_var(Pos,Clause,IVars),
	extend_saturated_var(IVars,Gvars,Ldg,Type,CVars),
	insert_covered_vars(Vars,Gvars,CVars,Type).

%
%  Extand the binding coverage by considering saturated variables sets.
%
extend_saturated_var(Vars,_,_,_,_) :-
	var(Vars).
extend_saturated_var(Vars,Gvars,Ldg,Type,Cvars) :-
	nonvar(Vars),
	Vars = [Var|VList],
	var_def_lit(Gvars,Var,LitNum),
	(LitNum == 0 ->
		new_lit(+,Lit);
		new_lit(LitNum,Lit)),
	(Type == relation ->
		find_ldg_field(Ldg,Lit,redge,Svars);
		find_ldg_field(Ldg,Lit,sedge,Svars)),
	opened_set_union(Svars,Cvars),
	extend_saturated_var(VList,Gvars,Ldg,Type,Cvars).

%
%  Find the defining literal number of a variable.
%
var_def_lit(Gvars,Var,LitNum) :-
	find_gvars_field(Gvars,Var,def,Def),
	Def = [Pos|_],
	pos_litnum(Pos,LitNum).

%
%  Insert the binding coverages on the set of output variables.
%
insert_covered_vars([],_,_,_).
insert_covered_vars([Var|VList],Gvars,CVars,Type) :-
	insert_coverage_edge(Gvars,Var,Type,CVars),
	insert_covered_vars(VList,Gvars,CVars,Type).

%
%  Insert the binding coverages on an output variables.
%
insert_coverage_edge(Gvars,Var,relation,CVars) :-
	insert_gvars_field(Gvars,Var,redge,CVars).
insert_coverage_edge(Gvars,Var,det,CVars) :-
	insert_gvars_field(Gvars,Var,sedge,CVars).

%
%  Establish variables defining literals list by dividing the set of variables
%  based on the defining literals and sorting them in decreasing order.
%
var_def_list(Vars,Gvars,SVarList) :-
	var_d_list(Vars,Gvars,VarList),
	msort(VarList,SVarList).

var_d_list(Vars,_,VarList) :-
	var(Vars),
	close_list(VarList).
var_d_list(Vars,Gvars,VarList) :-
	nonvar(Vars),
	Vars = [Var|VList],
	var_def_lit(Gvars,Var,LitNum),
	insert_var_division(VarList,Var,LitNum),
	var_d_list(VList,Gvars,VarList).

%
%  Sort a list of element in decreasing order.
%
msort([],[]).
msort([X],[X]).
msort(LL,R) :- LL = [_,_|_],
	divide(LL,L1,L2),
	msort(L1,R1), msort(L2,R2),
	merge(R1,R2,R).

%
%  Divide a list into two lists.
%
divide([],[],[]).
divide([X],[X],[]).
divide([X1,X2|L],[X1|L1],[X2|L2]) :- divide(L,L1,L2).

%
%  Merge two lists into a list in decreasing order.
%
merge([],[],[]).
merge(L,[],L) :- L = [_|_].
merge([],L,L) :- L = [_|_].
merge([X1|L1],[X2|L2],[X1|L]) :-
	X1 = cv(LitNum1,_),
	X2 = cv(LitNum2,_),
	LitNum1 >= LitNum2,
	merge(L1,[X2|L2],L).
merge([X1|L1],[X2|L2],[X2|L]) :-
	X1 = cv(LitNum1,_),
	X2 = cv(LitNum2,_),
	LitNum1 < LitNum2,
	merge([X1|L1],L2,L).

%
%  Insert a variable into the variables defining literals list.
%
insert_var_division(Vars,Var,LitNum) :-
	var(Vars),
	Vars = [cv(LitNum,[Var|_])|_].
insert_var_division(Vars,Var,LitNum) :-
	nonvar(Vars),
	Vars = [cv(LitNum1,List)|VList],
	(LitNum == LitNum1 ->
		opened_set_insertion(List,Var);
		insert_var_division(VList,Var,LitNum)).

%
%  Compute the number of bindings of a set of variables which is in the form
%  of variables defining literals list.
%
vars_binding([],_,_,_,_,1).
vars_binding([HdLit|TlLit],Gvars,Ldg,Type,Clause,Binding) :-
	HdLit = cv(_,HdVars),
	var(HdVars),
	vars_binding(TlLit,Gvars,Ldg,Type,Clause,Binding).
vars_binding([HdLit|TlLit],Gvars,Ldg,Type,Clause,Binding) :-
	HdLit = cv(LitNum,HdVars),
	nonvar(HdVars),
	(LitNum =:= 0 ->
		new_lit(+,Lit);
		new_lit(LitNum,Lit)),
	optimize_var_lits(LitNum,HdVars,TlLit,Ldg,Type,Clause,NTlLit),
	%remove_covered_lits(HdVars,TlLit,Gvars,Type,NTlLit),
	(Type == redge ->
		(find_vars_binding(HdVars,Gvars,relation,1,Binding1),
		 find_ldg_field(Ldg,Lit,relation,Binding2));
		(find_vars_binding(HdVars,Gvars,det,1,Binding1),
		 find_ldg_field(Ldg,Lit,det,Binding2))),
	%write(Binding1),nl,
	%write(Binding2),nl,
	minimum(Binding1,Binding2,Binding3),
	%write(Binding3),nl,
	vars_binding(NTlLit,Gvars,Ldg,Type,Clause,Binding4),
	multiply(Binding3,Binding4,Binding).

%
%  Remove the set of covered variables from the tail of the variables
%  defining literals list.
%
remove_covered_lits(HdVars,TlLit,_,_,TlLit) :-
	var(HdVars).
remove_covered_lits(HdVars,[],_,_,[]) :-
	nonvar(HdVars).
remove_covered_lits(HdVars,[cv(LNum,Lit)|LList],Gvars,Type,
		    [cv(LNum,NLit)|NList]) :-
	nonvar(HdVars),
	remove_covered_lit(HdVars,Lit,Gvars,Type,NLit),
	remove_covered_lits(HdVars,LList,Gvars,Type,NList).

%
%  Remove the set of covered variables from a literal in the variables
%  defining literals list.
%
remove_covered_lit(_,Lit,_,_,Lit) :-
	var(Lit).
remove_covered_lit(HdVars,Lit,Gvars,Type,NLit) :-
	nonvar(Lit),
	Lit = [Var|VList],
	(covering_vars(Gvars,HdVars,Var,Type) ->
		NLit = NVList;
		NLit = [Var|NVList]),
	remove_covered_lit(HdVars,VList,Gvars,Type,NVList).

%
%  Test if a set of variables is covering a variable in binding coverage.
%
covering_vars(_,HdVars,_,_) :-
	var(HdVars),
	fail.
covering_vars(Gvars,HdVars,Var,Type) :-
	nonvar(HdVars),
	HdVars = [Hvar|HList],
	(covering_var(Gvars,Hvar,Var,Type) ->
		true;
		covering_vars(Gvars,HList,Var,Type)).

%
%  Test if a variable is covering another variable in binding coverage.
%
covering_var(Gvars,Hvar,Var,Type) :-
	find_gvars_field(Gvars,Hvar,Type,Edges),
	init_queue(Qhd,Qtl),
	set_put_queue(Qtl,Edges,Ntl),
	reachable_var(Gvars,Qhd,Ntl,Var,Type).

%
%  Test if a variable is reachable from a queue of variables in binding 
%  coverage.
%
reachable_var(_,Qhd,Qtl,_,_) :-
	empty_queue(Qhd,Qtl),
	fail.
reachable_var(Gvars,Qhd,Qtl,Var,Type) :-
	nonempty_queue(Qhd,Qtl),
	get_queue(Qhd,V,Nhd),
	(V == Var ->
		true;
		(find_gvars_field(Gvars,V,Type,Edges),
		 set_put_queue(Qtl,Edges,Ntl),
		 reachable_var(Gvars,Nhd,Ntl,Var,Type))).

%
opt_cond1([cv(LitNum,_)|Lvars],Clause,IVars,Subsume) :-
	ith_clause_literal(LitNum,Clause,Lit),
	term_var(Lit,LitVars),
	(opened_set_inclusion(IVars,LitVars) ->
		new_lit(LitNum,Subsume);
		opt_cond1(Lvars,Clause,IVars,Subsume)).

opt_cond2(Lit,Ldg,Instance,Type) :-
	find_ldg_field(Ldg,Lit,Type,OBinding),
	Instance == OBinding.

opt_cond3(Instance,IBinding) :- Instance == IBinding.

opt_cond4(Instance,OBinding) :- Instance == OBinding.

optimize_var_lits(_,HdVars,_,_,_,_,_) :-
	var(HdVars).
optimize_var_lits(HdLit,HdVars,TlLit,Ldg,Type,Clause,NTlLit) :-
	nonvar(HdVars),
	new_lit(HdLit,Lit),
	find_ldg_field(Ldg,Lit,Type,SubsumeLits),
	remove_subsumed_lits(TlLit,SubsumeLits,HdVars,TTlLit),
	find_ldg_field(Ldg,Lit,pred,PredLits),
	ith_clause_literal(HdLit,Clause,Literal),
	term_var(Literal,LitVars),
	merge_same_lit_vars(TTlLit,PredLits,LitVars,HdVars,NTlLit).

remove_subsumed_lits([],_,_,[]).
remove_subsumed_lits([cv(LitNum,LitVars)|TlLit],SubsumeLits,HdVars,NTlLit) :-
	new_lit(LitNum,Lit),
	(opened_set_member(SubsumeLits,Lit) ->
		(merge_subsumed_vars(LitVars,HdVars),
		 NTlLit = NTlLits);
		NTlLit = [cv(LitNum,LitVars)|NTlLits]),
	remove_subsumed_lits(TlLit,SubsumeLits,HdVars,NTlLits).

merge_subsumed_vars(LitVars,_) :-
	var(LitVars).
merge_subsumed_vars(LitVars,HdVars) :-
	nonvar(LitVars),
	LitVars = [V|Vars],
	opened_set_insertion(HdVars,V),
	merge_subsumed_vars(Vars,HdVars).

merge_same_lit_vars([],_,_,_,[]).
merge_same_lit_vars([cv(LitNum,LitVars)|TlLit],PredLits,CVars,HdVars,NTlLit) :-
	new_lit(LitNum,Lit),
	(opened_set_member(PredLits,Lit) ->
		(same_lit_vars(LitVars,CVars,HdVars,NLitVars),
		 (var(NLitVars) ->
		 	NTlLit = NTlLits;
			NTlLit = [cv(LitNum,NLitVars)|NTlLits]));
		NTlLit = [cv(LitNum,LitVars)|NTlLits]),
	merge_same_lit_vars(TlLit,PredLits,CVars,HdVars,NTlLits).

same_lit_vars(LitVars,_,_,_) :-
	var(LitVars).
same_lit_vars(LitVars,CVars,HdVars,NLitVars) :-
	nonvar(LitVars),
	LitVars = [V|Vars],
	(opened_set_member(CVars,V) ->
		opened_set_insertion(HdVars,V);
		opened_set_insertion(NLitVars,V)),
	same_lit_vars(Vars,CVars,HdVars,NLitVars).

%
filter_effect([],_,Binding,Binding).
filter_effect([test(Lit,Rel)|TestLits],Vars,Binding1,Binding) :-
	term_var(Lit,Var1),		% Lit is test literal
	(opened_set_inclusion(Vars,Var1) ->
		minimum(Rel,Binding1,Binding2);
		Binding2 = Binding1),
	filter_effect(TestLits,Vars,Binding2,Binding).
%
%  relation.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the relation size
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the relation size analysis for a strongly connected component.
%
relation_analysis([],_,_,_,_,_,_,_,[]).
relation_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
		  [Gvars|GList],[Ldg|LList],[[Sol]|OList]) :-
	relation_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol1),
	simplification(Sol1,Sol),
	insert_symbol_field(ST,Pred,relation,Sol),
	relation_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,OList).

%
%  Perform the relation size analysis for a predicate.
%
relation_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	find_symbol_field(ST,Pred,relation,Sol1),
	(var(Sol1) ->
		(recursive_predicate(Pred,ST,Comp) ->
			Sol = inf;
		 	relation_nonrec_pred(Pred,BT,ST,Comp,Size,Adg,Gvars,
				Ldg,Sol));
		Sol = Sol1).

%
%  Perform the relation size analysis for a predicate.
%
relation_nonrec_pred(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	find_symbol_field(ST,Pred,clause,Clauses),
	find_symbol_field(ST,Pred,domain,Domain),
	((nonvar(Domain),unfoldable(Pred,BT,ST,Vars,UClauses)) ->
		(binary_disequality(Pred,ST,UClauses,DomainSize) ->
			gcp(Vars,DomainSize,UClauses,Sol);
			(linear_arithmetic_constraints(Pred,ST,UClauses) ->
				csp(Vars,Domain,UClauses,Sol);
				relation_clauses(Clauses,BT,ST,Comp,Size,Adg,
					Gvars,Ldg,Sol)));
		relation_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol)).

%
%  Perform the relation size analysis for the set of clauses in a predicate.
%
relation_clauses(Clauses,_,_,_,_,_,_,_,0) :-
	var(Clauses).
relation_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
		 [Ldg|LList],Sol) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	relation_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol1),
	relation_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,Sol2),
	addition(Sol1,Sol2,Sol).

%
%  Perform the relation size analysis for a clause.
%
relation_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	clause_type(Clause,Type),
	relation_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol).

relation_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	Clause = (Head :- Body),
	(fail_body(Body) ->
		Sol = 0;
		(relation_head_input(Head,BT,ST,Clause,Adg,Gvars,Ldg,ISol),
		 (ISol == inf ->
			Sol = inf;
		 	(no_of_cuts(Body,Cuts),
			 collect_test_literals(Body,BT,ST,Lits),
		 	 body_binding(1,Body,BT,ST,Comp,Clause,Size,Adg,Gvars,
				Ldg,relation,Cuts,Lits,_),
		 	 relation_head(Head,Clause,Gvars,Ldg,Lits,Sol))))).
relation_clause(3,Fact,BT,ST,_,_,Adg,Gvars,Ldg,Sol) :-
	relation_head_input(Fact,BT,ST,Fact,Adg,Gvars,Ldg,ISol),
	(ISol == inf ->
		Sol = inf;
		relation_head(Fact,Fact,Gvars,Ldg,[],Sol)).

%
%  Test if a predicate is recursive.
%
recursive_predicate(Pred,ST,Component) :-
	find_symbol_field(ST,Pred,clause,Clauses),
	recursive_clauses(Clauses,Component).

recursive_clauses(Clauses,_) :-
	var(Clauses),
	fail.
recursive_clauses(Clauses,Component) :-
	nonvar(Clauses),
	Clauses = [C|CList],
	(recursive_clause(C,Component) ->
		true;
		recursive_clauses(CList,Component)).
	
recursive_clause(Clause,Component) :-
	clause_type(Clause,Type),
	recursive_clause(Type,Clause,Component).

recursive_clause(2,(_:-Body),Component) :-
	recursive_body(Body,Component).

recursive_body(Lit,Component) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	member(Component,F/A).
recursive_body((Lit,Body),Component) :-
	functor(Lit,F,A),
	(member(Component,F/A) ->
		true;
		recursive_body(Body,Component)).

%
fail_clause(Clause) :-
	clause_type(Clause,2),
	Clause = (_:-Body),
	fail_body(Body).

fail_body((Lit,Body)) :-
	(Lit == (fail) ->
		true;
		fail_body(Body)).
fail_body((fail)).

%
%  solution.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the solution size
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the solution size analysis for a strongly connected component.
%
solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Sol).

solution_analysis([],_,_,_,_,_,_,_,_,[]).
solution_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
		  [Gvars|GList],[Ldg|LList],RSol,[Sol|OList]) :-
	find_symbol_field(ST,Pred,det,Sol1),
	(var(Sol1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 solution_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,
			RSol,Sol2),
		 %write(Sol2),nl,
		 solve_complexity_equ(Pred,ST,Comp,Sol2,Size,TSol),
		 %write(TSol),nl,
		 Sol3 = [TSol]);
		Sol3 = Sol1),
	solution_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,
		[comp(Pred,Sol3)|RSol],OList),
	remove_recursive_comps(Sol3,ST,det,Sol),
	insert_symbol_field(ST,Pred,det,Sol).

/*
%
%  Perform the solution size analysis for a predicate.
%
solution_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	find_symbol_field(ST,Pred,det,Sol1),
	(var(Sol1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 solution_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,_,Sol2),
		 find_symbol_field(ST,Pred,mutex,Mutex),
		 solve_comp_equs(Pred,ST,Comp,Sol2,Size,Mutex,Sol),
		 insert_symbol_field(ST,Pred,det,[Sol]));
		Sol = Sol1).
*/

%
%  Perform the solution size analysis for the set of clauses in a predicate.
%
solution_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
solution_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
		 [Ldg|LList],RSol,[Sol|OList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	solution_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol),
	solution_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RSol,OList).

%
%  Perform the solution size analysis for a clause.
%
solution_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol) :-
	clause_type(Clause,Type),
	solution_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol).

solution_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RSol,Sol) :-
	Clause = (Head :- Body),
	(fail_body(Body) ->
		Sol = 0;
		(solution_head_input(Head,BT,ST,Clause,Adg,Gvars,Ldg),
		 no_of_cuts(Body,Cuts),
		 collect_test_literals(Body,BT,ST,Lits),
		 body_binding(1,Body,BT,ST,Comp,Clause,Size,Adg,Gvars,Ldg,
			det,Cuts,Lits,RSol),
		 solution_head_output(Head,Clause,Adg,Gvars,Ldg,Lits,Sol))).
solution_clause(3,Fact,BT,ST,_,_,Adg,Gvars,Ldg,_,Sol) :-
	solution_head_input(Fact,BT,ST,Fact,Adg,Gvars,Ldg),
	solution_head_output(Fact,Fact,Adg,Gvars,Ldg,[],Sol).

%
no_of_cuts((Lit,Body),No) :-
	no_of_cuts(Body,Nos),
	(Lit == (!) ->
		No is Nos+1;
		No = Nos).
no_of_cuts(Lit,No) :-
	nonsequence(Lit),
	(Lit == (!) ->
		No = 1;
		No = 0).

%
collect_test_literals(Lit,BT,ST,Lits) :-
	nonsequence(Lit),
	functor(Lit,F,A),
	literal_property(BT,ST,F/A,(mode),Mode),
	(test_predicate(Mode) ->
		(literal_property(BT,ST,F/A,relation,Rel),
		 (Rel \== inf ->
			Lits = [test(Lit,Rel)];
			Lits = []));
		Lits = []).
collect_test_literals((Lit,Body),BT,ST,Lits) :-
	functor(Lit,F,A),
	literal_property(BT,ST,F/A,(mode),Mode),
	(test_predicate(Mode) ->
		(literal_property(BT,ST,F/A,relation,Rel),
		 (Rel \== inf ->
			Lits = [test(Lit,Rel)|RestLits];
			Lits = RestLits));
		Lits = RestLits),
	collect_test_literals(Body,BT,ST,RestLits).
%
%  comp_diff_equ.pl		Nai-Wei Lin			February, 1992
%
%  This file contains the programs for solving the complexity difference 
%  equations.
%

%
%  Solve the complexity equation for a predicate.
%  Size is the size functions for the clauses of the predicate;
%  while SIZE is the computed size functions for the predicate.
%
solve_complexity_equ(Pred,ST,_,Comp,Size,Sol) :-
	find_symbol_field(ST,Pred,size,SIZE),
	find_symbol_field(ST,Pred,(mode),Mode),
	(inconsistent_sizes(Mode,SIZE) ->
		Sol1 = inf;	% size functions for the pred undefined 
		(construct_clause_complexity_equs(Pred,ST,Size,Comp,ClauseEqu),
		 (mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) ->
			solve_comp_equs(ST,Pred,ClassEqu,Sol1);
			Sol1 = inf))),	% sizes in cluster inconsistent 
	simplification(Sol1,Sol).

%
%  Test if the sizes are inconsistent.
%
inconsistent_sizes(Mode,SIZE) :-
	inconsistent_sizes(Mode,SIZE,1).

inconsistent_sizes([],[],0).
inconsistent_sizes([+|Mode],[bot|Size],_) :- 
	inconsistent_sizes(Mode,Size,0).
inconsistent_sizes([+|_],[S|_],_) :- S \== bot, !, fail.
inconsistent_sizes([-|Mode],[_|Size],I) :- inconsistent_sizes(Mode,Size,I).

%
%  Construct the complexity equation for each clause from the input size 
%  and complexity function for the clause.
%
construct_clause_complexity_equs(Pred,ST,Size,Comp,Equ) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	input_sizes(Size,Mode,ISize),
	construct_comp_equs(ISize,Comp,1,Equ).

%
%  Compose the complexity equation for each mutually exclusive cluster
%  from the complexity equations of its clauses.
%
mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	mutual_exclusive_comp(Mutex,ClauseEqu,ClassEqu).

mutual_exclusive_comp([],_,[]).
mutual_exclusive_comp([MutexClass|MList],ClauseComp,[Comp|CList]) :-
	mutual_exclusive_comp1(MutexClass,ClauseComp,Comp),
	mutual_exclusive_comp(MList,ClauseComp,CList).

% Extra checks on the consistent input sizes are also performed.
% The form of the input size for all clauses are the same.
mutual_exclusive_comp1([],_,equ(_,_,Zero)) :-
	normal_form(0,Zero).
mutual_exclusive_comp1([ClauseNum|CList],ClauseComp,equ(ClauseNum,ISize,Comp)):-
	ith_list_element(ClauseNum,ClauseComp,CompEqu1),
	CompEqu1 = equ(ClauseNum,ISize,Comp1),
	mutual_exclusive_comp1(CList,ClauseComp,CompEqu2),
	CompEqu2 = equ(_,ISize,Comp2),
	add_expr(Comp1,Comp2,Comp).

%
%  Solve the complexity equations corresponding to the mutually exclusive
%  clusters of a predicate.
%
solve_comp_equs(ST,Pred,Equ,Sol) :-
	classify_equs(Equ,DEqu,BEqu),
	%write(DEqu),nl,
	%write(BEqu),nl,
	solve_comp_equs1(ST,Pred,DEqu,BEqu,NSol),
	general_form(NSol,Sol).

solve_comp_equs1(ST,Pred,DE,BE,Sol) :-
	abnormal_equs(BE,S),
	%write(S),nl,
	(general_form(S,0) ->
		solve_comp_equs2(DE,BE,ST,Pred,Sol);
		Sol = inf).

solve_comp_equs2([],BE,_,_,Sol) :-
	solve_comp_non_diff_equs(BE,Sol).
solve_comp_equs2(DE,BE,ST,Pred,Sol) :-
	DE \== [],
	(indirect_recursion(DE,Pred) ->
		(solve_comp_non_diff_equs(DE,S1),
		 solve_comp_non_diff_equs(BE,S2),
		 max_expr(S1,S2,Sol));
		solve_comp_diff_equs(DE,BE,ST,Pred,Sol)).

%
solve_comp_diff_equs(DE,BE,ST,Pred,Sol) :-
	%write(DE),nl,
	exprs_one_index_reducible(DE,Pred,1,IS,RDE,Pos),
	%write(RDE),nl,
	(BE == [] ->
		implicit_boundary_condition(ST,Pred,Pos,RBE); % implicit failure
		boundary_condition_one_index_reducible(BE,IS,Pos,RBE)),
	%write(RBE),nl,
	solve_one_index_comp_diff_equs(RDE,RBE,ST,Pred,Pos,Sol),!.
solve_comp_diff_equs(DE,BE,_,Pred,Sol) :-
	exprs_two_indices_reducible(DE,Pred,1,IS,Pos),
	adjust_pos(Pos,NPos),
	reduce_two_indices_exprs(DE,Pred,1,IS,NPos,NDE),
	NPos = [Pos1,Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	boundary_condition_two_indices_reducible(BE,IS,[POS1,POS2],NBE),
	solve_two_indices_diff_equs(NDE,NBE,Pred,Sol),!.
solve_comp_diff_equs(_,_,_,_,inf).

%
%
solve_one_index_comp_diff_equs([],_,_,_,_,Zero) :- normal_form(0,Zero).
solve_one_index_comp_diff_equs([equ(_,Var,OC)|DE],BE,ST,Pred,Pos,Sol) :-
	%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
	%general_form(OC,R),
	%write(R),nl,
	%write(Var),nl,
	(indirect_recursion_expr(OC,Pred) ->
		Sol1 = OC;
		solve_typed_diff_equ(comp,OC,BE,Var,ST,Pred,Pos,Sol1)),
	%solve_one_index_comp_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol1),
	%general_form(Sol1,GSol),
	%write(GSol),nl,
	solve_one_index_comp_diff_equs(DE,BE,ST,Pred,Pos,Sol2),
	max_expr(Sol1,Sol2,Sol).

solve_one_index_comp_diff_equ(second_order,BE,Var,An1,An2,Bn,_,_,_,Sol) :-
	!,
	boundary_conds(BE,Ival),
	solve_diff_equ(second_order,Var,An1,An2,Bn,Ival,Sol).
solve_one_index_comp_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol).

solve_one_index_comp_diff_equ1([],_,_,_,_,_,_,_,_,Zero) :- normal_form(0,Zero).
solve_one_index_comp_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
	E = equ(_,Index,Val),
	(integer(Index) ->
		(boundary_conds([E],Ival),
		 %write(Ival),nl,
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,Sol1));
		(comp_boundary_cond(E,ST,Pred,Pos,Ival),
		 %write(Ival),nl,
		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
		 %write(TS),nl,
		 max_expr(TS,Val,Sol1))),
	%general_form(Sol1,GSol),
	%write(GSol),nl,
	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol2),
	max_expr(Sol1,Sol2,Sol).


%
%  Compute the boundary condition for a complexity difference equation.
%
comp_boundary_cond(equ(Num,Var,Val),ST,Pred,Pos,[val(Iind,Ival)]) :-
	min_unifiable_term_sizes(ST,Pred,Num,Pos,MinSize),
	(integer(MinSize) ->
		(normal_form(MinSize,Iind),
		 general_form(Val,Nval),
		 substitute(Nval,Var,MinSize,Sval),
		 normal_form(Sval,Ival));
		(Iind = bot,
		 Ival = bot)).

min_unifiable_term_sizes(ST,Pred,Num,Pos,MinSize) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	mutex_cluster(Mutex,Num,Cluster),
	(min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,_,MS) ->
		MinSize = MS;
		MinSize = bot).

mutex_cluster([],_,[]).
mutex_cluster([M|Mutex],Num,Cluster) :-
	(member(M,Num) ->
		Cluster = M;
		mutex_cluster(Mutex,Num,Cluster)).

min_unifiable_term_sizes1([],_,_,_,MinSize,MinSize) :- integer(MinSize).
min_unifiable_term_sizes1([Num|Cluster],ST,Pred,Pos,MinSize,FMinSize) :-
	min_unifiable_term_size(ST,Pred,Num,Pos,MinSize1),
	(var(MinSize) ->
		(integer(MinSize1),
		 min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,MinSize1,
			FMinSize));
		(integer(MinSize1),
		 MinSize =:= MinSize1,
		 min_unifiable_term_sizes1(Cluster,ST,Pred,Pos,MinSize1,
			FMinSize))).

implicit_boundary_condition(ST,Pred,Pos,[equ(_,Size,Zero)]) :-
	find_symbol_field(ST,Pred,measure,Measure),
	find_symbol_field(ST,Pred,(mode),Mode),
	real_pos(Mode,Pos,1,RealPos),
	ith_list_element(RealPos,Measure,M),
	find_symbol_field(ST,Pred,clause,Clauses),
	min_unifiable_implicit_term_size(Clauses,M,RealPos,MinSize),
	%write(MinSize),nl,
	Size is MinSize-1,
	normal_form(0,Zero).

min_unifiable_implicit_term_size(Clauses,_,_,inf) :-
	var(Clauses).
min_unifiable_implicit_term_size(Clauses,M,Pos,MinSize) :-
	nonvar(Clauses),
	Clauses = [C|Cs],
	min_unifiable_term_size1(M,C,Pos,MS1),
	min_unifiable_implicit_term_size(Cs,M,Pos,MS2),
	minimum(MS1,MS2,MinSize).
%
%  time.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the time
%  analysis for the predicates in the program in topologically sorted order.
%

%
%  Perform the time analysis for a strongly connected component.
%
time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
	time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,[],Time).

time_analysis([],_,_,_,_,_,_,_,_,[]).
time_analysis([Pred|CompList],BT,ST,Comp,[Size|SList],[Adg|AList],
	      [Gvars|GList],[Ldg|LList],RTime,[Time|TList]) :-
	find_symbol_field(ST,Pred,time,Time1),
	(var(Time1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
			Time2),
		 %write(Time2),nl,
		 solve_complexity_equ(Pred,ST,Comp,Time2,Size,TTime),
		 Time3 = [TTime]);
		Time3 = Time1),
	%write(Time3),nl,
	time_analysis(CompList,BT,ST,Comp,SList,AList,GList,LList,
		[comp(Pred,Time3)|RTime],TList),
	remove_recursive_comps(Time3,ST,time,Time),
	insert_symbol_field(ST,Pred,time,Time).

/*
%
%  Perform the time analysis for a predicate.
%
time_predicate(Pred,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
	find_symbol_field(ST,Pred,time,Time1),
	(var(Time1) ->
		(find_symbol_field(ST,Pred,clause,Clauses),
		 time_clauses(Clauses,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time2),
		 find_symbol_field(ST,Pred,mutex,Mutex),
		 solve_comp_equs(Pred,ST,Comp,Time2,Size,Mutex,Time),
		 insert_symbol_field(ST,Pred,time,[Time]));
		Time = Time1).
*/

%
%  Perform the time analysis for the set of clauses in a predicate.
%
time_clauses(Clauses,_,_,_,_,_,_,_,_,[]) :-
	var(Clauses).
time_clauses(Clauses,BT,ST,Comp,[Size|SList],[Adg|AList],[Gvars|GList],
	     [Ldg|LList],RTime,[Time|TList]) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time1),
	simplification(Time1,Time),
	time_clauses(CList,BT,ST,Comp,SList,AList,GList,LList,RTime,TList).

%
%  Perform the time analysis for a clause.
%
time_clause(Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	clause_type(Clause,Type),
	time_clause(Type,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time).

time_clause(2,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,Time) :-
	Clause = (_:-Body),
	time_body(Body,1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,1,RTime,Time1),
	addition(Time1,1,Time).
time_clause(3,_,_,_,_,_,_,_,_,_,1).

%
%  Perform the time analysis for the body of a clause.
%
time_body((Lit,Body),LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,
		Time) :-
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	(Lit == (!) ->
		Time2 = Time1;
		multiply(Times,Time1,Time2)),
	(Lit == (!) ->
		Times1 = 1; 
		(frequency_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,
			Gvars,_,_,Sol2),
		 multiply(Times,Sol2,Times1))),
	LitNum1 is LitNum+1,
	time_body(Body,LitNum1,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times1,
		  RTime,Time3),
	addition(Time2,Time3,Time).
time_body(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,Times,RTime,Time) :-
	nonsequence(Lit),
	time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,Ldg,RTime,
		Time1),
	multiply(Times,Time1,Time).

%
%  Perform the time analysis for a literal.
%
time_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,_,RTime,Time) :-
	functor(Lit,F,A),
	(second_order_predicate(F/A) ->
		(second_order_predicate_pred_arg(Lit,Lit1),
		 functor(Lit1,F1,A1),
		 arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num1));
		(F1 = F, A1 = A, Num1 = LitNum)),
	literal_output_comp(F1/A1,Num1,1,BT,ST,Comp,Adg,time,RTime,LitTime),
	normalize_time_function(LitTime,F1/A1,Num1,BT,ST,Comp,Clause,
		Adg,Gvars,Size,RTime,Time).

frequency_literal(Lit,LitNum,Clause,BT,ST,Comp,Size,Adg,Gvars,_,_,Sol) :-
	functor(Lit,F,A),
	literal_output_comp(F/A,LitNum,1,BT,ST,[],Adg,det,[],Sol1),
	normalize_solution_function(Sol1,F/A,LitNum,BT,ST,Comp,Clause,
			Adg,Gvars,Size,[],Sol).
/*
	(second_order_predicate(F/A) ->
		(second_order_predicate_pred_arg(Lit,Lit1),
		 functor(Lit1,F1,A1),
		 arg(2,Clause,Body),
		 second_order_predicate_pred_num(Body,LitNum,Num1));
		(F1 = F, A1 = A, Num1 = LitNum)),
	literal_output_comp(F1/A1,Num1,1,BT,ST,[],Adg,det,[],Sol1),
	normalize_solution_function(Sol1,F1/A1,Num1,BT,ST,Comp,Clause,
			Adg,Gvars,Size,[],Sol2),
	(second_order_predicate(F/A) ->
		(gen_literal_iopos(Adg,F1/A1,Num1,(-),Pos),
		 pos_var(Pos,Lit1,Vars),
		 arg(1,Lit,Arg1),
		 term_var(Arg1,Var1),
		 (opened_set_equivalent(Var1,Vars) ->
			Sol = 1;
			Sol = Sol2));
		Sol = Sol2).
*/


%
%  Normalize the time function of a literal.
%
normalize_time_function(LitTime,LitName,LitNum,BT,ST,Comp,Clause,Adg,Gvars,
			Size,RTime,Time) :-
	gen_clause_pos(Adg,PosSet),
	(recursive_clause(Clause,Comp) ->
		(ith_clause_literal(0,Clause,Lit),
		 functor(Lit,F,N),
		 find_symbol_field(ST,F/N,size,ISz));
		ISz = Size),
	gen_literal_iopos(Adg,LitName,LitNum,(+),Pos),
	init_normalize_queue(Pos,QHd,QTl),
	normalize(LitTime,QHd,QTl,BT,ST,[],Clause,Adg,Gvars,PosSet,ISz,RTime,Time).

%
%  top.pl			Nai-Wel Lin			December, 1991
%
%  This file contains the top level procedures for the system.
%

%
%  Top-level predicate of CASLOG in interactive mode.
%
caslog(Files) :- 
	nl,
	write('Caslog 1.0, April 1992.'),nl,
	statistics(runtime,[_,_]),
%	read_prog(Files),
	top(Files),
	statistics(runtime,[_,T]),
	nl, write('{Exexution Time: '),
	write(T), write(' msec}'), nl,
	nl,
	write('{End of Caslog execution.}'),nl.

%%%%
read_prog([]).
read_prog([File|Fs]) :-
	see(File),
	r_prog,
	seen,
	read_prog(Fs).

r_prog :-
	read(Clause),
	(Clause \== end_of_file -> 
		 r_prog;
		 true).
%%%%

top(Files) :-
	init_system(Files,BT,ST,SCCG,Error1),
	analysis_check(SCCG,ST,Error2),
	Error is Error1*Error2,
	(Error =:= 1 ->
		analysis(SCCG,BT,ST);
		true).

analysis([],_,_).
analysis([Comp|SCCG],BT,ST) :-
	%write('start dependency analysis'),nl,
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	%write(Adg),nl,
	%write(Ldg),nl,
	%write(Gvars),nl,
	(Error =:= 1 ->
		(%write('start size analysis'),nl,
		 size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		 print_size(Comp,ST),
		 ttyflush,
		 %write('start relation analysis'),nl,
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 ttyflush,
		 %write('start solution analysis'),nl,
		 determinacy_analysis(Comp,BT,ST,Adg),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 ttyflush,
		 %write('start time analysis'),nl,
		 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_time(Comp,ST),
		 ttyflush,
		 analysis(SCCG,BT,ST));
		true).

%
%  Top-level predicate of CASLOG in batch mode.
%
caslog(Complexity,File) :-
	(legal_complexity(Complexity) ->
		(init_system(File,BT,ST,SCCG,Error1), 
		 analysis_check(SCCG,ST,Error2),
%		 direct_recursive_check(SCCG,Error3),
		 Error3 = 1,
		 Error is Error1*Error2*Error3,
		 (Error =:= 1 ->
			analysis(Complexity,SCCG,BT,ST);
			true));
		true).

%
%  Check if the complexity name is legal.
%
legal_complexity(size).
legal_complexity(solution).
legal_complexity(time).
legal_complexity(all).
legal_complexity(Complexity) :-
	Complexity \== size,
	Complexity \== solution,
	Complexity \== time,
	Complexity \== all,
	error_message(comp1,Complexity,''),
	fail.

%
%  Analyze the program.
%
analysis(size,SCCG,BT,ST) :-
	size_analysis(SCCG,BT,ST).
/*
	size_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		size_analysis(SCCG,BT,ST);
		true).
*/
analysis(solution,SCCG,BT,ST) :-
	solution_analysis(SCCG,BT,ST).
/*
	solution_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		solution_analysis(SCCG,BT,ST);
		true).
*/
analysis(time,SCCG,BT,ST) :-
	time_analysis(SCCG,BT,ST).
/*
	time_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		time_analysis(SCCG,BT,ST);
		true).
*/
analysis(all,SCCG,BT,ST) :-
	all_analysis(SCCG,BT,ST).


%
%  Perform the argument size analysis.
%
size_analysis([],_,_).
size_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,_,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,_),
		 print_size(Comp,ST),
		 size_analysis(SCCG,BT,ST));
		true).

%
%  Perform the relation size and solution size analysis.
%
solution_analysis([],_,_).
solution_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		%print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 solution_analysis(SCCG,BT,ST));
		true).

%
%  Perform the time analysis.
%
time_analysis([],_,_).
time_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		%print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		%print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		%print_solution(Comp,ST),
		 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_time(Comp,ST),
		 time_analysis(SCCG,BT,ST));
		true).

%
%  Perform all the complexity analysis.
%
all_analysis([],_,_).
all_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		 print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_time(Comp,ST),
		 all_analysis(SCCG,BT,ST));
		true).

%
%
direct_recursive_check([],1).
direct_recursive_check([Comp|SCCG],Error) :-
	Comp = [_],
	direct_recursive_check(SCCG,Error).
direct_recursive_check([Comp|SCCG],0) :-
	Comp = [_|_],
	write('* caslog error: cannot handle indirect recursion'),
	nl,
	direct_recursive_check(SCCG,_).
%
%  utility.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the utility procedures.
%

%
%  The following procedure perform arithmetic operations.
%

%
%  Performing the symbolic addition with bottom.
%
addition(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1+A2.
addition(bot,bot,bot).
addition(A1,bot,bot) :-
	A1 \== bot.
addition(bot,A2,bot) :-
	A2 \== bot.
addition(inf,inf,inf).
addition(A1,inf,inf) :-
	A1 \== inf,
	A1 \== bot.
addition(inf,A2,inf) :-
	A2 \== inf,
	A2 \== bot.
addition(A1,A2,A1) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
addition(A1,A2,A2) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
addition(A1,A2,A1+A2) :-
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1),
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
addition(A1,A2,A1+A2) :-
	number(A1),
	A1 =\= 0,
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
addition(A1,A2,A1+A2) :-
	number(A2),
	A2 =\= 0,
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1).

%
%  Performing the symbolic subtraction with bottom.
%
subtraction(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1-A2.
subtraction(bot,bot,bot).
subtraction(A1,bot,bot) :-
	A1 \== bot.
subtraction(bot,A2,bot) :-
	A2 \== bot.
subtraction(A1,A2,A1) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== bot.
subtraction(A1,A2,-A2) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	nonnumber(A1),
	nonnumber(A2),
	A1 \== bot,
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	number(A1),
	A1 =\= 0,
	nonnumber(A2),
	A2 \== bot.
subtraction(A1,A2,A1-A2) :-
	nonnumber(A1),
	A1 \== bot,
	number(A2),
	A2 =\= 0.

%
%  Performing the symbolic multiplication with bottom.
%
multiply(A1,A2,A) :-
	number(A1),
	number(A2),
	A is A1*A2.
multiply(bot,bot,bot).
multiply(A1,bot,bot) :-
	A1 \== bot.
multiply(bot,A2,bot) :-
	A2 \== bot.
multiply(inf,inf,inf).
multiply(A1,inf,inf) :-
	A1 \== inf,
	A1 \== bot.
multiply(inf,A2,inf) :-
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A2) :-
	number(A2),
	A2 =:= 0,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
multiply(A1,A2,A1) :-
	number(A1),
	A1 =:= 0,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A1) :-
	number(A2),
	A2 =:= 1,
	nonnumber(A1),
	A1 \== inf,
	A1 \== bot.
multiply(A1,A2,A2) :-
	number(A1),
	A1 =:= 1,
	nonnumber(A2),
	A2 \== inf,
	A2 \== bot.
multiply(A1,A2,A1*A2) :-
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1),
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
multiply(A1,A2,A1*A2) :-
	number(A1),
	A1 =\= 0,
	A1 =\= 1,
	A2 \== bot,
	A2 \== inf,
	nonnumber(A2).
multiply(A1,A2,A1*A2) :-
	number(A2),
	A2 =\= 0,
	A2 =\= 1,
	A1 \== bot,
	A1 \== inf,
	nonnumber(A1).

%
minus(bot,bot).
minus(inf,bot).
minus(X,Y) :-
	number(X),!,
	(X =:= 0 ->
		Y = 0;
		Y is -X).
minus(X,-X) :-
	/* not number(X), */
	X \== bot,
	X \== inf.

%
%  Performing the arithmetical addition with bottom.
%
add(bot,bot,bot).
add(A1,bot,bot) :-
	A1 \== bot.
add(bot,A2,bot) :-
	A2 \== bot.
add(A1,A2,A) :-
	A1 \== bot,
	A2 \== bot,
	A is A1+A2.

%
%  Performing the arithmetical subtraction with bottom.
%
sub(bot,bot,bot).
sub(A1,bot,bot) :-
	A1 \== bot.
sub(bot,A2,bot) :-
	A2 \== bot.
sub(A1,A2,A) :-
	A1 \== bot,
	A2 \== bot,
	A is A1-A2.

%
%  Performing the arithmetic maximum function.
%
max(N,M,N) :-
	N >= M.
max(N,M,M) :-
	N < M.

%
%  Performing the arithmetic minimum function.
%
min(N,M,N) :-
	N < M.
min(N,M,M) :-
	N >= M.

%
%  Performing the maximum function with top, bottom, infinite and negative
%  infinite.
%
maximum(N,M,S) :-
	number(N),
	number(M),
	max(N,M,S).
maximum(_,top,top).
maximum(top,M,top) :-
	M \== top.
maximum(N,bot,N) :-
	N \== top.
maximum(bot,M,M) :-
	M \== top,
	M \== bot.
maximum(N,inf,inf) :-
	N \== top,
	N \== bot.
maximum(inf,M,inf) :-
	M \== top,
	M \== bot,
	M \== inf.
maximum(N,neginf,N) :-
	N \== top,
	N \== bot,
	N \== inf.
maximum(neginf,M,M) :-
	M \== top,
	M \== bot,
	M \== inf,
	M \== neginf.
maximum(N,N,N) :-
	N \== top,
	N \== bot,
	N \== inf,
	N \== neginf,
	nonnumber(N).
maximum(N,M,S) :-
	N \== M,
	N \== top,
	N \== bot,
	N \== inf,
	N \== neginf,
	M \== top,
	M \== bot,
	M \== inf,
	M \== neginf,
	(nonnumber(N);
	 nonnumber(M)),
	S = (max(N,M)).

%
%  Performing the minimum function with top, bottom, infinite and negative
%  infinite.
%
minimum(N,M,S) :-
	integer(N),
	integer(M),
	min(N,M,S).
minimum(_,top,top).
minimum(top,M,top) :-
	M \== top.
minimum(N,bot,N) :-
	N \== top.
minimum(bot,M,M) :-
	M \== top,
	M \== bot.
minimum(N,inf,N) :-
	N \== top,
	N \== bot.
minimum(inf,M,M) :-
	M \== top,
	M \== bot,
	M \== inf.
minimum(N,neginf,neginf) :-
	N \== top,
	N \== bot,
	N \== inf.
minimum(neginf,M,neginf) :-
	M \== top,
	M \== bot,
	M \== inf,
	M \== neginf.
minimum(N,N,N) :-
	N \== top,
	N \== bot,
	N \== inf,
	N \== neginf,
	noninteger(N).
minimum(N,M,S) :-
	N \== M,
	N \== top,
	N \== bot,
	N \== inf,
	N \== neginf,
	M \== top,
	M \== bot,
	M \== inf,
	M \== neginf,
	(noninteger(N);
	 noninteger(M)),
	S = (min(N,M)).

%
%  The following precedures perform list operations.
%

%
%  Close a partial list.
%
close_list(List) :-
	var(List),
	List = [].
close_list(List) :-
	nonvar(List),
	List = [_|VList],
	close_list(VList).

%
%  Test the membership of an element in a list.
%
member([],_) :- fail.
member([E|_],Ele) :-
	E == Ele.
member([E|List],Ele) :-
	E \== Ele,
	member(List,Ele).

%
%  Get the ith element in the list.
%
ith_list_element(_,List,_) :-
	var(List),
	fail.
ith_list_element(_,List,_) :-
	nonvar(List),
	List == [],
	fail.
ith_list_element(I,List,Element) :-
	I > 1,
	nonvar(List),
	List = [_|L],
	I1 is I-1,
	ith_list_element(I1,L,Element).
ith_list_element(1,List,Element) :-
	nonvar(List),
	List = [Element|_].

%
%  Select the set of elements in a list according to a given number list
%  that specifies the order of the desired elements in the list.
%
select_list_elements([],_,[]).
select_list_elements([N|NumList],List,[E|EleList]) :-
	ith_list_element(N,List,E),
	select_list_elements(NumList,List,EleList).

%
%  Append two lists into a list.
%
append([],L,L).
append([H|L1],L2,[H|L3]) :-
	append(L1,L2,L3).

%
%  The following precedures perform queue operations.
%

%
%  Initialize a queue.
%
init_queue(QHead,QHead).


%
%  Test if a queue is empty.
%
empty_queue(QHead,QTail) :-
	QHead == QTail.

%
%  Test if a queue is nonempty.
%
nonempty_queue(QHead,QTail) :-
	QHead \== QTail.

%
%  Get an element from a queue.
%
get_queue([Pos|QHead],Pos,QHead).

%
%  Put an element into a queue.
%
put_queue([Pos|QTail],Pos,QTail).

%
%  Put a set of elements into a queue.
%
set_put_queue(QTail,List,QTail) :-
	var(List).
set_put_queue(QTail,List,QTail) :-
	nonvar(List),
	List = [].
set_put_queue(QTail,List,NTail) :-
	nonvar(List),
	List = [Ele|NList],
	put_queue(QTail,Ele,QTail1),
	set_put_queue(QTail1,NList,NTail).

%
%  Print out the elements in a queue.
%
print_queue(QHead,QTail) :-
	empty_queue(QHead,QTail).
print_queue(QHead,QTail) :-
	nonempty_queue(QHead,QTail),
	get_queue(QHead,Pos,NHead),
	write(Pos),nl,
	print_queue(NHead,QTail).

%
%  The following precedures perform stack operations.
%

%
%  Test if a stack is empty.
%
empty_stack([]).

%
%  Test if a stack is nonempty.
%
nonempty_stack(Stack) :-
	Stack \== [].

%
%  Push an element into a stack.
%
push(Stack,Element,[Element|Stack]).

%
%  Pop an element out of a stack.
%
pop([],_,[]).
pop([Element|Stack],Element,Stack).

%
%  The following precedures perform set operations.
%

%
%  Test if two opened sets Set1 and Set2 are equivalent.
%
opened_set_equivalent(Set1,Set2) :-
	opened_set_inclusion(Set1,Set2),
	opened_set_inclusion(Set2,Set1).

%
%  Test if opened set Set1 is included in opened set Set2.
%
opened_set_inclusion(Set1,_) :-
	var(Set1).
opened_set_inclusion(Set1,Set2) :-
	nonvar(Set2),
	nonvar(Set1),
	Set1 = [E|S1],
	opened_set_member(Set2,E),
	opened_set_inclusion(S1,Set2).
		
%
%  Test if E is an element of an opened set Set.
%
opened_set_member(Set,E) :-
	nonvar(Set),
	Set = [E1|S1],
	(E == E1 ->
		true;
		opened_set_member(S1,E)).

%
%  Insert an element into an opened set represented by a non-ordered partial 
%  list.
%
opened_set_insertion(Set,Ele) :-
	var(Set),!,
	Set = [Ele|_].
opened_set_insertion(Set,Ele) :-
	nonvar(Set),
	Set = [Ele1|Set1],
	(Ele == Ele1 ->
		true;
		opened_set_insertion(Set1,Ele)).

%
%  Perform the union of a closed set and an opened set.
%
opened_set_union([],_).
opened_set_union([Ele|Set1],Set2) :-
	opened_set_insertion(Set2,Ele),
	opened_set_union(Set1,Set2).

%
%  Get the intersection of two sets.
%
intersection([],_,[]).
intersection([X|S1],S2,S) :-
	(member(S2,X) ->
		S = [X|SList];
		S = SList),
	intersection(S1,S2,SList).

union([],S,S).
union([X|S1],S2,S) :-
	(member(S2,X) ->
		S = SList;
		S = [X|SList]),
	union(S1,S2,SList).

%
%  The following are predicate procedures.
%

%
%  Test if a term is compound.
%
/*RB
compound(Term) :-
	nonvar(Term),
	functor(Term,_,N),
	N > 0.
*/
%
%  Test if a term is noncompound.
%
noncompound(Term) :-
	var(Term).
noncompound(Term) :-
	atomic(Term).

%
%  Test if a term is noninteger.
%
noninteger(Term) :-
	var(Term).
noninteger(Term) :-
	compound(Term).
noninteger(Term) :-
	atom(Term).
noninteger(Term) :-
	float(Term).

%
%  Test if a term is nonnumber.
%
nonnumber(Term) :-
	var(Term).
nonnumber(Term) :-
	compound(Term).
nonnumber(Term) :-
	atom(Term).

%
%  Test if a term is a list.
%
list([]).
list([_|_]).

%
%  Test if a term is nonlist.
%
nonlist(Term) :-
	var(Term).
nonlist(Term) :-
	atomic(Term),
	Term \== [].
nonlist(Term) :-
	compound(Term),
	functor(Term,F,N),
	F/N \== '.'/2.

%
%  Test if a term is nonsequence.
%
nonsequence(Term) :-
	var(Term).
nonsequence(Term) :-
	functor(Term,F,N),
	F/N \== (',')/2.

%
%  Test if a term is ground.
%
/*
ground(Term) :-
	atomic(Term).
ground(Term) :-
	compound(Term),
	functor(Term,_,N),
	ground(N,Term).

ground(0,_).
ground(N,Term) :-
	N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground(N1,Term).
*/

%
%  Test if a term is a subterm of another term.
%
subterm(Term,Term).
subterm(Sub,Term) :-
	Sub \== Term,
	compound(Term),
	functor(Term,_,N),
	subterm(N,Sub,Term).

subterm(N,Sub,Term) :-
	N > 1,
	N1 is N-1,
	subterm(N1,Sub,Term).
subterm(N,Sub,Term) :-
	arg(N,Term,Arg),
	subterm(Sub,Arg).

%
%  Print out the argument size functions.
%
print_size([],_).
print_size([Pred|CList],ST) :-
	nl,
	write('* Size functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,size,Size),
	output_comp_dec(Size,NSize),
	write(NSize), nl,
	print_size(CList,ST).

%
%  Print out the relation size functions.
%
print_relation([],_).
print_relation([Pred|CList],ST) :-
	nl,
	write('* Relation functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,relation,Relation),
	output_comp_dec(Relation,NRelation),
	write([NRelation]), nl,
	print_relation(CList,ST).

%
%  Print out the solution size functions.
%
print_solution([],_).
print_solution([Pred|CList],ST) :-
	nl,
	write('* Solution functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,det,Solution),
	output_comp_dec(Solution,NSolution),
	write(NSolution), nl,
	print_solution(CList,ST).

%
%  Print out the time functions.
%
print_time([],_).
print_time([Pred|CList],ST) :-
	nl,
	write('* Time functions for predicate '),
	write(Pred), 
	write(' :'),
	nl, nl, 
	find_symbol_field(ST,Pred,time,Time),
	output_comp_dec(Time,NTime),
	write(NTime), nl,
	print_time(CList,ST).

%
%  Format the output complexity functions.
%
output_comp_dec(Exp,Exp) :-
	atomic(Exp).
output_comp_dec(Exp,NExp) :-
	compound(Exp),
	functor(Exp,F,N),
	(F/N == ($)/2 ->
		(arg(1,Exp,Arg1),
		 arg(2,Exp,Arg2),
		 ((Arg1 =:= 0, integer(Arg2)) ->
			NExp =.. ['$',Arg2];
			NExp = Exp));
		(functor(NExp,F,N),
		 output_comp_dec(N,Exp,NExp))).

output_comp_dec(0,_,_).
output_comp_dec(N,Exp,NExp) :-
	N > 0,
	arg(N,Exp,Arg),
	output_comp_dec(Arg,NArg),
	arg(N,NExp,NArg),
	N1 is N-1,
	output_comp_dec(N1,Exp,NExp).
%
%  error.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for reporting error messages.
%

%
%  Print out an error message.
%
error_message(Code,Source,Clause) :-
	write('* Caslog error: '),
	error(Code,Source),
	nl,
	write('  '),
	write(Clause),
	nl.

%
%  Unknown complexity name error.
%
error(comp1,Source) :-
	write('unknown complexity name: '),
	write(Source).


%
%  Unknown predicate error.
%
error(lit1,Source) :-
	write('unknown predicate: '),
	write(Source).

%
%  Unbound input variable error.
%
error(bound1,Source) :-
	write('unbound input variable: '),
	write(Source).

%
%  Declaration error.
%
error(dec1,Source) :-
	write('complexity declaration is missed for predicate: '),
	write(Source).
error(dec2,Source) :-
	write('illegal declaration: '),
	write(Source).
error(dec3,Source) :-
	write('mode is not declared for predicate: '),
	write(Source).
error(dec4,Source) :-
	write('measure is not declared for predicate: '),
	write(Source).
error(dec5,Source) :-
	write('size is not declared for predicate: '),
	write(Source).
error(dec6,Source) :-
	write('det is not declared for predicate: '),
	write(Source).
error(dec7,Source) :-
	write('time is not declared for predicate: '),
	write(Source).
error(dec8,_) :-
	write('illegal expressions in declaration').
error(dec9,Source) :-
	write('mutex is not declared for predicate: '),
	write(Source).
error(mode1,_) :-
	write('use list for mode declaration').
error(mode2,_) :-
	write('arity inconsistency in mode declaration').
error(mode3,Source) :-
	write('illegal mode symbol: '),
	write(Source).
error(measure1,_) :-
	write('use list for measure declaration').
error(measure2,_) :-
	write('arity inconsistency in measure declaration').
error(measure3,Source) :-
	write('illegal measure name: '),
	write(Source).
error(mutex1,_) :-
	write('use list for mutex declaration').
error(mutex2,_) :-
	write('use list of integers for mutex declaration').
error(domain1,_) :-
	write('use list for domain declaration').
error(domain2,_) :-
	write('arity inconsistency in domain declaration').
error(domain3,_) :-
	write('use integer interval or list for domain declaration').
error(size1,_) :-
	write('use list for size declaration').
error(size2,_) :-
	write('arity inconsistency in size declaration').
error(det1,_) :-
	write('do not use list for det declaration').
error(time1,_) :-
	write('do not use list for time declaration').
error(second_order1,Source) :-
	write('illegal predicate argument '),
	write(Source),
	write(' in findall predicate').
