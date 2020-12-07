:- use_module(library(lists)).
:- use_module(library(clpb)).

:- ensure_loaded(main).
:- use_module(utils).

check([],[]).
check([PTC1|TC1s],[PTC2|TC2s]) :-
    \+ ( \+ (
        PTC1=predicate_term_condition(Atom,TC1),
        PTC2=predicate_term_condition(Atom,TC2),
        taut(TC1 =:= TC2,1))),
    check(TC1s,TC2s).
    

ptc(P,L) :-
	nl,nl,write_list(P),
	program_termination_conditions(P,L),
    write_list(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-
ptc([p(0)],L),
check(L,[predicate_term_condition(p(_A),1)]).

:-
ptc([p],L), 
check(L, [predicate_term_condition(p,1)]).

:-
ptc([(loop :-loop)],L),
check(L,[predicate_term_condition(loop,0)]).

:-
ptc([
 l(0,s(X)),
(l(s(X),s(Y)):-l(X,Y)),
(p(I,N):-l(I,N),p(s(I),N))],L), 
check(L, [predicate_term_condition(l(_A,_B),_B+_A),
	  predicate_term_condition(p(_C,_D),_D)]).
			      
:-
ptc([
(p(s(X),s(Y)):-q(X,Z),p(Z,Y)),
 p(0,0),
 q(0,0),
( q(s(X),s(Y)):-q(X,Y))],L), 
check(L, [predicate_term_condition(p(_A,_B),_A),
	  predicate_term_condition(q(_C,_D),_D+_C)]).

:-
ptc([
(test :- once(_X=1))],L), 
check(L, [predicate_term_condition(test,1)]).

:-  %% not is unknown, assumed to fail
ptc([
(test :- not(loop)),
(loop :- loop)],L), 
check(L, [predicate_term_condition(loop,0),
	  predicate_term_condition(test,1),
	  predicate_term_condition(not(_A),1)]).

:- %% use \+
ptc([
(test :- \+ loop),
(loop :- loop)],L), 
check(L, [predicate_term_condition(loop,0),
	  predicate_term_condition(test,0)]).


%%% TerHV %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%ex1
:-
ptc([
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(append(_A,_B,_C),_C+_A)]).
			       
%%ex2
:-
ptc([
    even(0),
(   even(s(s(X))):-even(X)),

    lte(0,Y),
(   lte(s(X),s(Y)):-lte(X,Y)),

(   goal :- lte(X,s(s(s(s(0))))),even(X))],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(even(_A),_A),
	  predicate_term_condition(lte(_B,_C),_C+_B)]).

%% ex3
:-
ptc([
    member(X,[X|_]),
(   member(X,[_|Ys]):-member(X,Ys))],L), 
check(L, [predicate_term_condition(member(_A,_B),_B)]).

%% ex4 
:-
ptc([
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y),

(   perm(Xs,[X|Ys]):-app(X1s,[X|X2s],Xs),app(X1s,X2s,Zs),perm(Zs,Ys))],L), 
check(L, [predicate_term_condition(perm(_A,_B),1),
	  predicate_term_condition(app(_C,_D,_E),1),
	  predicate_term_condition(append(_F,_G,_H),_H+_F)]).

:-
ptc([
(   app([A|X],Y,[A|Z]) :- app(X,Y,Z)),
    app([],Y,Y),

    perm([],[]),
(   perm(Xs,[X|Ys]):-app(X1s,[X|X2s],Xs),app(X1s,X2s,Zs),perm(Zs,Ys))],L), 
check(L, [predicate_term_condition(perm(_A,_B),_A),
	  predicate_term_condition(app(_C,_D,_E),_E+_C)]).

%% ex5
:-
ptc([
    even(0),
(   even(s(X)):-odd(X)),
(   odd(s(X)):-even(X))],L), 
check(L, [predicate_term_condition(even(_A),_A),
	  predicate_term_condition(odd(_B),_B)]).

%% ex6
:-
ptc([
    add(0,0,0),
(   add(s(X),Y,s(N)):-add(X,Y,N)),
(   add(X,s(Y),s(N)):-add(X,Y,N)),

    fib(0,0),
    fib(s(0),s(0)),
(   fib(s(s(X)),N):-fib(s(X),N1),fib(X,N2),add(N1,N2,N))],L), 
check(L, [predicate_term_condition(fib(_A,_B),_A),
	  predicate_term_condition(add(_C,_D,_E),_E+_C*_D)]).

:-
ptc([
    add(0,Y,Y),
(   add(s(X),Y,s(N)):-add(X,Y,N)),

    fib(0,0),
    fib(s(0),s(0)),
(   fib(s(s(X)),N):-fib(s(X),N1),fib(X,N2),add(N1,N2,N))],L), 
check(L, [predicate_term_condition(fib(_A,_B),_A),
	  predicate_term_condition(add(_C,_D,_E),_E+_C)]).

%% ex7
:-
ptc([
(   dis(or(B1,B2)) :- con(B1),dis(B2)),
(   dis(B) :- con(B)),

(   con(and(B1,B2)) :- dis(B1),con(B2)),
(   con(B) :- bool(B)),

(   bool(0):-true),
    bool(1)],L), 
check(L, [predicate_term_condition(bool(_A),1),
	  predicate_term_condition(con(_B),_B),
	  predicate_term_condition(dis(_C),_C)]).

%% ex8
:-
ptc([
    holds(a,[]),
    holds(l,[lo|_]),
(   holds(d,[shoot|X]):-holds(l,X)),

(   ab(a,shoot,X):-holds(l,X)),
(   holds(X,[Y|Z]):- \+ (ab(X,Y,Z)),holds(X,Z))],L), 
check(L, [predicate_term_condition(holds(_A,_B),_B),
	  predicate_term_condition(ab(_C,_D,_E),_E)]).

%% ex9
:-
ptc([
(   e(X+Y):-f(X),e(Y)),
(   e(X):-f(X)),

(   f(X*Y):-g(X),f(Y)),
(   f(X):-g(X)),

(   g(-(X)):-e(X)),
(   g(X):-integer(X))],L), 
check(L, [predicate_term_condition(e(_A),_A),
	  predicate_term_condition(f(_B),_B),
	  predicate_term_condition(g(_C),_C)]).

%% ex10
:-
ptc([
(   d(U+V,X,DU+DV):-!,d(U,X,DU),d(V,X,DV)),
(   d(U-V,X,DU-DV):-!,d(U,X,DU),d(V,X,DV)),
(   d(U*V,X,DU*V+DV*U):-!,d(U,X,DU),d(V,X,DV)),
(   d(U/V,X,(DU*V-U*DV)/(V^2)):-!,d(U,X,DU),d(V,X,DV)),
(   d(U^N,X,DU*N*(U^N1)):-!,integer(N),N1 is N-1,d(U,X,DU)),
(   d(-U,X,-DU):-!,d(U,X,DU)),
(   d(exp(U),X,exp(U)*DU):-!,d(U,X,DU)),
(   d(log(U),X,DU/U):-!,d(U,X,DU)),
(   d(X,X,1):-!),
(   d(_C,X,0):-true)],L), 
check(L, [predicate_term_condition(d(_A,_B,_C),_C+_A)]).


%% ex11
:-
ptc([
    as(X,X,0),
(   as([a|X],Y,s(N)):-as(X,Y,N))],L), 
check(L, [predicate_term_condition(as(_A,_B,_C),_C+_A)]).

%% ex12
:-
ptc([
(   som(U,V):-pdt(U,W),rds(W,V)),

(   pdt(U,V):-pri(U,W),rdp(W,V)),

(   pri([N|U],U):-integer(N)),
(   pri(['('|U],V):-exp(U,[')'|V])),

    rds(U,U),
(   rds([Op|U],V):-pdt(U,W),rds(W,V)),

    rdp(U,U),
(   rdp([Op|U],V):-pri(U,W),rdp(W,V)),

(   exp(U,V):-som(U,V))],L), 
check(L, [predicate_term_condition(exp(_A,_B),_A),
	  predicate_term_condition(pdt(_C,_D),_C),
	  predicate_term_condition(pri(_E,_F),_E),
	  predicate_term_condition(rdp(_G,_H),_G),
	  predicate_term_condition(rds(_I,_J),_I),
	  predicate_term_condition(som(_K,_L),_K)]).

:-
ptc([
(   pri([A|As],As):-integer(A))],L), 
check(L, [predicate_term_condition(pri(_A,_B),1)]).

:-
ptc([
    exa(X,X),
(   exa([o|X0],X):-exb(X0,X1),exb(X1,X)),
(   exb(X0,X):-exa(X0,X))],L), 
check(L, [predicate_term_condition(exa(_A,_B),_A),
	  predicate_term_condition(exb(_C,_D),_C)]).

:-
ptc([
    exa(X,X),
(   exa([o|X0],X):-exa(X0,X1),exa(X1,X))],L), 
check(L, [predicate_term_condition(exa(_A,_B),_A)]).

:-
ptc([
    exa(X,X),
(   exa(X0,X):-exa(X0,X1),exa(X1,X))],L), 
check(L, [predicate_term_condition(exa(_A,_B),0)]).

%%%%%%%%%%% exotic examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-
ptc([],_L),nl.

:-
ptc([
    a,
(   a :- a)],L), 
check(L, [predicate_term_condition(a,0)]).

:-
ptc([
    a(X),
(   a(X):-a([a|X]))],L), 
check(L, [predicate_term_condition(a(_A),0)]).

:-
ptc([
    a(X),
(   a(X):-b([a|X])),

(   b(X):-a(X)),

    c([]),
(   c([X|Xs]):-c(Xs))],L), 
check(L, [predicate_term_condition(a(_A),0),
	  predicate_term_condition(b(_B),0),
	  predicate_term_condition(c(_C),_C)]).

:-
ptc([
(   a([],X,X):-true)],L), 
check(L, [predicate_term_condition(a(_A,_B,_C),1)]).

:-
ptc([
(   sum3([],Ys,Ys)),
(   sum3([a|Xs],[],[a|Xs])),
(   sum3([a|Y1],[a|Y2],[a|Y3]) :-  sum3(Y1,Y2,Y3)),

(   sum41(As,Bs,Cs,Ds):-sum3(As,Bs,Es),sum3(Es,Cs,Ds)),

(   sum42(As,Bs,Cs,Ds):-sum3(Es,Cs,Ds),sum3(As,Bs,Es))],L), 
check(L, [predicate_term_condition(sum3(_A,_B,_C),_C+_B+_A),
	  predicate_term_condition(sum41(_D,_E,_F,_G),_E*_G+_E*_F+_D*_G+_D*_F+_D*_E),
	  predicate_term_condition(sum42(_H,_I,_J,_K),_K+_I*_J+_H*_J)]).

:-
ptc([
(   reverse(X,Y):- reverse(X,[],Y)),
    reverse([],X,X),
(   reverse([X|Y],Z,U) :- reverse(Y,[X|Z],U))],L), 
check(L, [predicate_term_condition(reverse(_A,_B),_A),
	  predicate_term_condition(reverse(_C,_D,_E),_C)]).

:-
ptc([
    permute([],[]),
(   permute([X|Y],[U|V]) :- delete(U,[X|Y],W),permute(W,V)),

    delete(X,[X|Y],Y),
(   delete(U,[X|Y],[X|Z]) :- delete(U,Y,Z))],L), 
check(L, [predicate_term_condition(permute(_A,_B),_A),
	  predicate_term_condition(delete(_C,_D,_E),_E+_D)]).

:-
ptc([
    duplicate([],[]),
(   duplicate([X|Y],[X,X|Z]) :- duplicate(Y,Z))],L), 
check(L, [predicate_term_condition(duplicate(_A,_B),_B+_A)]).

:-
ptc([
    sum([],[],[]),
(   sum([X1|Y1],[X2|Y2],[X3|Y3]) :- X3 is X1+X2, sum(Y1,Y2,Y3))],L), 
check(L, [predicate_term_condition(sum(_A,_B,_C),_C+_B+_A)]).

:-
ptc([
    merge([],X,X),
    merge(X,[],X),
(   merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y,merge(Xs,[Y|Ys],Zs)),
(   merge([X|Xs],[Y|Ys],[Y|Zs]) :- Y<X,merge([X|Xs],Ys,Zs))],L), 
check(L, [predicate_term_condition(merge(_A,_B,_C),_C+_A*_B)]).

:-
ptc([
    p([]),
(   p([_|Xs]):-p(Xs))],L), 
check(L, [predicate_term_condition(p(_A),_A)]).

:-
ptc([
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y),

(   sublist2(X,Y) :- append(X2,X3,Y),append(X1,X,X2)),

(   sublist3(X,Y) :- append(X,X1,X2), append(X3,X2,Y))],L), 
check(L, [predicate_term_condition(sublist2(_A,_B),_B),
	  predicate_term_condition(sublist3(_C,_D),_C*_D),
	  predicate_term_condition(append(_E,_F,_G),_G+_E)]).

:-
ptc([
    a(0),
(   a(s(X)):-b(X,Y),a(Y)),

    b(X,X)],L), 
check(L, [predicate_term_condition(a(_A),_A),
	  predicate_term_condition(b(_B,_C),1)]).

:-
ptc([
    mergesort([],[]),
    mergesort([X],[X]),
(   mergesort([X,Y|Xs],Ys) :- 
	split([X,Y|Xs],X1s,X2s),
	mergesort(X1s,Y1s), mergesort(X2s,Y2s), merge(Y1s,Y2s,Ys)),

    split([],[],[]),
(   split([X|Xs],[X|Ys],Zs) :- split(Xs,Zs,Ys)),

    merge([],Xs,Xs),
    merge(Xs,[],Xs),
(   merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y, merge(Xs,[Y|Ys],Zs)),
(   merge([X|Xs],[Y|Ys],[Y|Zs]) :- X>Y, merge([X|Xs],Ys,Zs))],
				  L), 
check(L, [predicate_term_condition(mergesort(_A,_B),0),
	  predicate_term_condition(merge(_C,_D,_E),_E+_C*_D),
	  predicate_term_condition(split(_F,_G,_H),_F+_G*_H)]).

:-
ptc([
    mergesort([],[]),
    mergesort([x],[x]),
(   mergesort([x,x|Xs],Ys) :- 
	split([x,x|Xs],X1s,X2s),
	mergesort(X1s,Y1s), mergesort(X2s,Y2s), merge(Y1s,Y2s,Ys)),

    split([],[],[]),
(   split([x|Xs],[x|Ys],Zs) :- split(Xs,Zs,Ys)),

    merge([],Xs,Xs),
    merge(Xs,[],Xs),
(   merge([x|Xs],[x|Ys],[x|Zs]) :- X=<Y, merge(Xs,[x|Ys],Zs)),
(   merge([x|Xs],[x|Ys],[x|Zs]) :- X>Y, merge([x|Xs],Ys,Zs))],L), 
check(L, [predicate_term_condition(mergesort(_A,_B),_A),
	  predicate_term_condition(merge(_C,_D,_E),_E+_C*_D),
	  predicate_term_condition(split(_F,_G,_H),_F+_G*_H)]).

:- current_cti_flag(nb_ite_clpn,NbIte),
   set_cti_flag(nb_ite_clpn,4),
ptc([
    mergesort([],[]),
    mergesort([x],[x]),
(   mergesort([x,x|Xs],Ys) :- 
	split([x,x|Xs],X1s,X2s),
	mergesort(X1s,Y1s), mergesort(X2s,Y2s), merge(Y1s,Y2s,Ys)),

    split([],[],[]),
(   split([x|Xs],[x|Ys],Zs) :- split(Xs,Zs,Ys)),

    merge([],Xs,Xs),
    merge(Xs,[],Xs),
(   merge([x|Xs],[x|Ys],[x|Zs]) :- X=<Y, merge(Xs,[x|Ys],Zs)),
(   merge([x|Xs],[x|Ys],[x|Zs]) :- X>Y, merge([x|Xs],Ys,Zs))],L),
set_cti_flag(nb_ite_clpn,NbIte),
check(L, [predicate_term_condition(mergesort(_A,_B),_A),
	  predicate_term_condition(merge(_C,_D,_E),_E+_C*_D),
	  predicate_term_condition(split(_F,_G,_H),_F+_G*_H)]).

:-
ptc([
(goal :-				   
    X = [[a]],
    Y = [[b]],
    append(X,Y,Z),
    Z = [C,D],
    append(C,D,_E)),	   
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(append(_A,_B,_C),_C+_A)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% From Ulrich

:-
ptc([
p(0,0),
(p(s(X),Y):-q(X,Y)),
(q(X,s(Y)):-p(X,Y))],L), 
check(L, [predicate_term_condition(p(_A,_B),_B+_A),
	  predicate_term_condition(q(_C,_D),_D+_C)]).

%% point 23 ok
:-
ptc([
    n(0,n),
(   n(s(N),n):-n(N,n)),
(   nn(N,M,n):-n(N,n),n(M,n))],L), 
check(L, [predicate_term_condition(n(_A,_B),_A),
	  predicate_term_condition(nn(_C,_D,_E),_C*_D)]).

:-
ptc([
    n(0),
(   n(s(N)):-n(N)),
(   nn(N,M):-n(N),n(M))],L), 
check(L, [predicate_term_condition(n(_A),_A),
	  predicate_term_condition(nn(_C,_D),_C*_D)]).

%% point 22
:-
ptc([
(   merge(X,[],X)),
(   merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y,merge(Xs,[Y|Ys],Zs)),
(   merge([X|Xs],[Y|Ys],[Y|Zs]) :- Y<X,merge([X|Xs],Ys,Zs))],L), 
check(L, [predicate_term_condition(merge(_A,_B,_C),_C+_A*_B)]).

%% point 20 ok
:-
ptc([		   
(   t2(As,Bs,Cs) :-
	app(As,Bs,Cs),
	inf),
(   inf :- inf),
    app([],Bs,Bs),
(   app([E|Es],Bs,[E|Cs]) :-app(Es,Bs,Cs))],L), 
check(L, [predicate_term_condition(inf,0),
	  predicate_term_condition(app(_A,_B,_C),_C+_A),
	  predicate_term_condition(t2(_D,_E,_F),0)]).

% ok
:-
ptc([		   
(   t2(As,Bs,Cs) :-
	inf,
        app(As,Bs,Cs)),
(   inf :- inf),
    app([],Bs,Bs),
(   app([E|Es],Bs,[E|Cs]) :-app(Es,Bs,Cs))],L), 
check(L, [predicate_term_condition(inf,0),
	  predicate_term_condition(app(_A,_B,_C),_C+_A),
	  predicate_term_condition(t2(_D,_E,_F),0)]).

% ok
:-
ptc([		   
(   t2(As,Bs,Cs) :-
	app(As,Bs,Cs)),
(   inf :- inf),
    app([],Bs,Bs),
(   app([E|Es],Bs,[E|Cs]) :-app(Es,Bs,Cs))],L), 
check(L, [predicate_term_condition(inf,0),
	  predicate_term_condition(app(_A,_B,_C),_C+_A),
	  predicate_term_condition(t2(_D,_E,_F),_F+_D)]).

%% point 19
:-
ptc([(p(A):- cti:{b(A)},p(A))],L), 
check(L, [predicate_term_condition(p(_A),0)]).

:-
ptc([(p(A):- cti:{b(A)},p2(A)),(p2(s(A)) :- p2(A))],L), 
check(L, [predicate_term_condition(p(_A),1),
	  predicate_term_condition(p2(_B),_B)]).

:-
ptc([
(   q(A) :- cti:{b(A)}, inf),
(   inf :- inf),
(   p(A) :- cti:{b(A)}, p(A))],L), 
check(L, [predicate_term_condition(inf,0),
	  predicate_term_condition(p(_A),0),
	  predicate_term_condition(q(_B),0)]).

%% point 16
:-
ptc([
    mergesort([X],[X]),
(   mergesort([X,Y|Xs],Ys) :- split2([X,Y|Xs],X1s,X2s),
                          mergesort(X1s,Y1s),
                          mergesort(X2s,Y2s),
                          merge(Y1s,Y2s,Ys)),

(   split(Xs,Ys,Zs) :- split0(Xs,Ys,Zs)),
(   split(Xs,Ys,Zs) :- split1(Xs,Ys,Zs)),
(   split(Xs,Ys,Zs) :- split2(Xs,Ys,Zs)),

   split0([],[],[]),
   split1([X],[X],[]),
(  split2([X,Y|Xs],[X|Ys],[Y|Zs]) :- split(Xs,Ys,Zs)),

    merge([],Xs,Xs),
    merge(Xs,[],Xs),
(   merge([X|Xs],[Y|Ys],[X|Zs]) :- X=<Y, merge(Xs,[Y|Ys],Zs)),
(   merge([X|Xs],[Y|Ys],[X|Zs]) :- X>Y, merge([X|Xs],Ys,Zs))
				  ],L), 
check(L, [predicate_term_condition(mergesort(_A,_B),_A),
	  predicate_term_condition(merge(_C,_D,_E),_E+_C*_D),
	  predicate_term_condition(split(_F,_G,_H),_H+_G+_F),
	  predicate_term_condition(split0(_I,_J,_K),1),
	  predicate_term_condition(split1(_L,_M,_N),1),
	  predicate_term_condition(split2(_O,_P,_Q),_Q+_P+_O)]).




% point 11
:-
ptc([
(   goal :- cti:{n(A) = n(A)+1}, loop(A)),
(   loop(A) :- loop(A))],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(loop(_A),0)]).

% point 11 bis
:-
ptc([
(   goal :- cti:{n(A) = n(B)+1},cti:{n(B)=n(A)+1},loop(A)),
(   loop(A) :- loop(A))],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(loop(_A),0)]).

% point 9
:-
ptc([
(   tokens(A, B) :- whitespace(A, C),tokens(C, B)),
(   whitespace([_|A], A))],L), 
check(L, [predicate_term_condition(tokens(_A,_B),_A),
	  predicate_term_condition(whitespace(_C,_D),1)]).

:-
ptc([
(   t(S0) :- w(S0,S1), t(S1)),   
(   w(s(C,S),S) :- C = 1)],L), 
check(L, [predicate_term_condition(t(_A),_A),
	  predicate_term_condition(w(_B,_C),1)]).

:-
ptc([
(   t2(As, Bs, Cs) :-
        cti:{n(Cs)>= n(As)+1},
        cti:{n(As)>= n(Cs)+1},
        app(As,Bs,Cs)),

    app([], As, As),
(   app([E|Es], Fs, [E|Gs]) :-
        app(Es, Fs, Gs))],L), 
check(L, [predicate_term_condition(app(_A,_B,_C),_C+_A),
	  predicate_term_condition(t2(_D,_E,_F),1)]).

:-
ptc([
(   t2(As, Bs, Cs) :-
        Cs = [_,_,_|As],
        cti:{n(As)>= n(Cs)+1},
        app(As,Bs,Cs)),

    app([], As, As),
(   app([E|Es], Fs, [E|Gs]) :-
        app(Es, Fs, Gs))],L), 
check(L, [predicate_term_condition(app(_A,_B,_C),_C+_A),
	  predicate_term_condition(t2(_D,_E,_F),1)]).

:-
ptc([
(   t2(As,_Bs,Cs):-
        Cs=[_,_|As],
        cti:{n(As)>=n(Cs)},
        a(As)),
(   a([_|As]):-a(As)),
    a([])],L), 
check(L, [predicate_term_condition(a(_A),_A),
	  predicate_term_condition(t2(_B,_C,_D),1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exemples d'analyse avec des programmes sans modele 

:-
ptc([
(   a(X):-a([a|X]))],
	L), 
check(L, [predicate_term_condition(a(_A),0)]).

:-
ptc([
(   a([a|Xs]):-a(Xs))],
	L), 
check(L, [predicate_term_condition(a(_A),_A)]).

:-
ptc([
(   a(X):-b(X)),
(   a([a|Xs]):-a(Xs)),

    c(1)],L), 
check(L, [predicate_term_condition(a(_A),_A),
	  predicate_term_condition(b(_B),1),
	  predicate_term_condition(c(_C),1)]).

:-
ptc([
(   a(X):-b(X)),
(   a([a|Xs]):-fail,a([a|Xs])),
(   a([b|Xs]):-a(Xs),fail)],
	L), 
check(L, [predicate_term_condition(a(_A),_A),
	  predicate_term_condition(b(_B),1)]).

:-
ptc([
(   a([a|Xs]):-fail,a([a|Xs])),
(   a([b|Xs]):-a(Xs),fail)],
	L), 
check(L, [predicate_term_condition(a(_A),_A)]).

:-
ptc([a],L), 
check(L, [predicate_term_condition(a,1)]).

:-
ptc([
    a,
(   a :-a)],L), 
check(L, [predicate_term_condition(a,0)]).

:-
ptc([
(   a :- not(b)),
    b],L), 
check(L, [predicate_term_condition(a,1),
	  predicate_term_condition(b,1),
	  predicate_term_condition(not(_A),1)]).

:-
ptc([
(    a :- \+ b),
     a,
     b],L), 
check(L, [predicate_term_condition(a,1),
	  predicate_term_condition(b,1)]).

:-
ptc([
(   a :- \+ b,a),
    a,
    b],L), 
check(L, [predicate_term_condition(a,0),
	  predicate_term_condition(b,1)]).

:-
ptc([
(   a :- b,a),
    b],L), 
check(L, [predicate_term_condition(a,0),
	  predicate_term_condition(b,1)]).
*/
%%%%%%%%%%%%%%  cti:{...}  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-
ptc([
(   goal :- cti:{b(_X)})],L), 
check(L, [predicate_term_condition(goal,1)]).

:-
ptc([
(   goal :- cti:{b(X)},append(X,Y,Z)),
				   
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(append(_A,_B,_C),_C+_A)]).

:-
ptc([
(   goal :- cti:{b(X);b(Z)},append(X,Y,Z)),

(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(append(_A,_B,_C),_C+_A)]).

:-
ptc([
(   goal :- cti:{b(X),b(Z)},append(X,Y,Z)),

(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(append(_A,_B,_C),_C+_A)]).

:-
ptc([
(   goal :- cti:{12 >= n(_Z)})],L), 
check(L, [predicate_term_condition(goal,1)]).

:-
ptc([
(   goal :- cti:{12 >= n(Z),n(Z) >= n(X)},append(X,Y,A)),
(   append([A|X],Y,[A|Z]) :- append(X,Y,Z)),
    append([],Y,Y)],L), 
check(L, [predicate_term_condition(goal,1),
	  predicate_term_condition(append(_A,_B,_C),_C+_A)]).

:-
ptc([
(   goal(X,Y) :- cti:{n(X) >= n(Y)})],L), 
check(L, [predicate_term_condition(goal(_A,_B),1)]).

:-
ptc([
(   goal(X,Y) :- cti:{n(X) >= n(Y)},true,cti:{n(Y) >= n(X)})],L), 
check(L, [predicate_term_condition(goal(_A,_B),1)]).

:-
ptc([
(   goal(X,Y) :- cti:{n(X) >= n(Y)},true,cti:{n(Y) >= n(X)+1}),
(   a :- goal(X,Y),a)],L), 
check(L, [predicate_term_condition(a,1),
	  predicate_term_condition(goal(_A,_B),1)]).

:-
ptc([
    split([], [], []),
(   split([A|B], [A|C], D) :-split(B, D, C)),

    merge([], A, A),
    merge(A, [], A),
(   merge([A|B], [C|D], [A|E]) :-A=<C,merge(B, [C|D], E)),
(   merge([A|B], [C|D], [C|E]) :-A>C,merge([A|B], D, E)),

    mergesort([], []),
    mergesort([A], [A]),
(   mergesort([A,B|C], D) :-
        split([A,B|C], E, F),
        cti:{n(E) >= 1,n(F) >= 1},  %%%%% 
        mergesort(E, G),
        mergesort(F, H),
        merge(G, H, D))],L), 
check(L, [predicate_term_condition(mergesort(_A,_B),_A),
	  predicate_term_condition(merge(_C,_D,_E),_E+_C*_D),
	  predicate_term_condition(split(_F,_G,_H),_F+_G*_H)]).

/*
:- file_termconds('Filex/testdcg1.pl',L),
write(L),nl,
check(L, [predicate_term_condition(bin(_A,_B),_A)]).

:- file_termconds('Filex/testdcg2.pl',L),
write(L),nl,
check(L, [predicate_term_condition(expr(_A,_B,_C),_B),
	  predicate_term_condition(factor(_D,_E,_F),1),
	  predicate_term_condition(term(_G,_H,_I),_H)]).
*/
    
 
:-
ptc([
    member(X,[X|_]),
   (member(X,[_|L]) :-member(X,L)),
   (test(L,M) :- setof(X,member(X,L),M))],L), 
check(L, [predicate_term_condition(member(_A,_B),_B),
	  predicate_term_condition(test(_C,_D),_C)]).

:-
ptc([
    member(X,[X|_]),
   (member(X,[_|L]) :-member(X,L)),
   (test(L1,L2,L3) :- setof(X,(member(X,L1),member(X,L2)),L3))],L), 
check(L, [predicate_term_condition(member(_A,_B),_B),
	  predicate_term_condition(test(_C,_D,_E),_C*_D)]).

:-  % trop faible !
ptc([
    member(X,[X|_]),
   (member(X,[_|L]) :-member(X,L)),
   (test(L,Y) :- setof(X,member(X,L),M),member(Y,M))],L), 
check(L, [predicate_term_condition(member(_A,_B),_B),
	  predicate_term_condition(test(_C,_D),0)]).

:-
ptc([
    member(X,[X|_]),
   (member(X,[_|L]) :-member(X,L)),
   (test(L,M) :- findall(X,member(X,L),M))],L), 
check(L, [predicate_term_condition(member(_A,_B),_B),
	  predicate_term_condition(test(_C,_D),_C)]).

:- halt.
