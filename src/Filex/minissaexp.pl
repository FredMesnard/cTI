%:- include_predef('/Users/fred/cTI/src/predef_for_compatibility.pl').

minissa(A, B, C, D) :-
        compiler(A, _, _, _, _, _, E, F),
        (   F='translation ok' ->
            interpreter(E, B, C, D)
        ;   D=F
        ).

compiler(A, B, C, D, E, F, G, H) :-
        run(source_reader(A,B),
	    H='reader error',
	    run(lexical_analysis(B,C),
		H='lexical error',
		run(syntax_analysis(C,D),
		    H='syntax error',
		    run(declaration_analysis(D),
			H='declaration error',
			run(assignment_analysis(D),
			    H='assignment error',
			    run(is_int_program(D),
				H='int syntax error1',
				run(ssa_generation(D,E),
				    H='ssa gen error',
				    run(is_ssa_program(E),
					H='ssa syntax error1',
					run(ssa_optimization(E,F),
					    H='ssa opt error',
					    run(is_ssa_program(F),
						H='ssa syntax error2',
						run(ssa_deletion(F,G),
						    H='ssa del error',
						    run(is_int_program(G),
							H='int syntax error2',
							run(fail,H='translation ok',true))))))))))))).
interpreter(A, B, C, D) :-
        run(is_int_program(A),
	    D='input program error',
	    run(number(B),
		D='input data error',
		run(execution(A,B,C),
		    D='execution error',
		    run(fail,D='execution ok',true)))).

run(A, _, B) :-
        call(A),
        call(B).
run(A, B, _) :-
        \+A,
        call(B).

source_reader(A, B) :-
        open(A, read, C),
        read_chars(C, B),
        close(C), !.


read_chars(A, []) :-
        at_end_of_stream(A).
read_chars(A, [B|C]) :-
        \+ at_end_of_stream(A),
        get0(A, B),
        read_chars(A, C).

map(_, [], []).
map(A, [B|C], [D|E]) :-
        A=..F,
        append(F, [B,D], G),
        H=..G,
        call(H),
        map(A, C, E).

fixpoint(A, B, C) :-
        D=..[A,B,E],
        call(D),
        fixpoint1(A, B, E, C).

fixpoint1(_, A, A, A).
fixpoint1(A, B, C, D) :-
        \+B==C,
        fixpoint(A, C, D).

        
program(A, B) :-
        A=[80|C],
        C=[82|D],
        D=[79|E],
        E=[67|F],
        F=[69|G],
        G=[68|H],
        H=[85|I],
        I=[82|J],
        J=[69|K],
        ident(K, L),
        pars(L, M),
        decl(M, N),
        N=[66|O],
        O=[69|P],
        P=[71|Q],
        Q=[73|R],
        R=[78|S],
        stats(S, T),
        T=[69|U],
        U=[78|V],
        V=[68|B].

pars(A, B) :-
        A=[40|C],
        C=[73|D],
        D=[78|E],
        ident(E, F),
        F=[44|G],
        G=[79|H],
        H=[85|I],
        I=[84|J],
        ident(J, K),
        K=[41|B].

decl(A, B) :-
        A=[86|C],
        C=[65|D],
        D=[82|E],
        idlist(E, B).
decl(A, B) :-
        B=A.

idlist(A, B) :-
        ident(A, C),
        C=[44|D],
        idlist(D, B).
idlist(A, B) :-
        ident(A, B).

stats(A, B) :-
        stat(A, C),
        C=[59|D],
        stats(D, B).
stats(A, B) :-
        stat(A, B).

stat(A, B) :-
        ident(A, C),
        C=[58|D],
        D=[61|E],
        expr(E, B).
stat(A, B) :-
        A=[73|C],
        C=[70|D],
        cond(D, E),
        E=[84|F],
        F=[72|G],
        G=[69|H],
        H=[78|I],
        stats(I, J),
        J=[69|K],
        K=[76|L],
        L=[83|M],
        M=[69|N],
        stats(N, O),
        O=[69|P],
        P=[78|Q],
        Q=[68|B].
stat(A, B) :-
        A=[87|C],
        C=[72|D],
        D=[73|E],
        E=[76|F],
        F=[69|G],
        cond(G, H),
        H=[68|I],
        I=[79|J],
        stats(J, K),
        K=[69|L],
        L=[78|M],
        M=[68|B].
stat(A, B) :-
        B=A.

cond(A, B) :-
        expr(A, C),
        (   C=[61|D]
        ;   C=[35|D]
        ;   C=[62|D]
        ),
        expr(D, B).

expr(A, B) :-
        term(A, C),
        exprr(C, B).

exprr(A, B) :-
        (   A=[43|C]
        ;   A=[45|C]
        ),
        term(C, D),
        exprr(D, B).
exprr(A, B) :-
        B=A.

term(A, B) :-
        factor(A, C),
        termr(C, B).

termr(A, B) :-
        A=[42|C],
        factor(C, D),
        termr(D, B).

termr(A, B) :-
        B=A.

factor(A, B) :-
        A=[40|C],
        expr(C, D),
        D=[41|B].
factor(A, B) :-
        ident(A, B).
factor(A, B) :-
        number(A, B).

is_int_program(p(A,B,C)) :-
        is_pars(A),
        is_decl(B),
        is_stat(C).
is_pars([A,B]) :-
        is_id(A),
        is_id(B).

is_decl([]).
is_decl([A|B]) :-
        is_id(A),
        is_decl(B).

is_stat(nil).
is_stat(ass(A,B)) :-
        is_id(A),
        is_expr(B).
is_stat((A;B)) :-
        is_stat(A),
        is_stat(B).
is_stat(ifs(A,B,C)) :-
        is_expr(A),
        is_stat(B),
        is_stat(C).
is_stat(whs(A,B)) :-
        is_expr(A),
        is_stat(B).

is_expr(bin(A,B,C)) :-
        is_op(A),
        is_expr(B),
        is_expr(C).
is_expr(var(A)) :-
        is_id(A).
is_expr(con(A)) :-
        is_con(A).

is_op(A) :-
        member(A, [=,#,>,+,-,*]).

is_con(A) :-
        number(A).

is_id(A) :-
        atom(A).

lexical_analysis(A, B) :-
        progtext(B, A, []).

progtext(A, B, C) :-
        blank(B, D),
        progtext(A, D, C).
progtext([A|B], C, D) :-
        sym(A, C, E), !,
        progtext(B, E, D).
progtext([], A, B) :-
        B=A.

sym('PROC', A, B) :-
        A=[80|C],
        C=[82|D],
        D=[79|E],
        E=[67|F],
        F=[69|G],
        G=[68|H],
        H=[85|I],
        I=[82|J],
        J=[69|B].
sym('BEGIN', A, B) :-
        A=[66|C],
        C=[69|D],
        D=[71|E],
        E=[73|F],
        F=[78|B].
sym('END', A, B) :-
        A=[69|C],
        C=[78|D],
        D=[68|B].
sym('VAR', A, B) :-
        A=[86|C],
        C=[65|D],
        D=[82|B].
sym('IF', A, B) :-
        A=[73|C],
        C=[70|B].
sym('THEN', A, B) :-
        A=[84|C],
        C=[72|D],
        D=[69|E],
        E=[78|B].
sym('ELSE', A, B) :-
        A=[69|C],
        C=[76|D],
        D=[83|E],
        E=[69|B].
sym('WHILE', A, B) :-
        A=[87|C],
        C=[72|D],
        D=[73|E],
        E=[76|F],
        F=[69|B].
sym('DO', A, B) :-
        A=[68|C],
        C=[79|B].
sym('IN', A, B) :-
        A=[73|C],
        C=[78|B].
sym('OUT', A, B) :-
        A=[79|C],
        C=[85|D],
        D=[84|B].
sym(:=, A, B) :-
        A=[58|C],
        C=[61|B].
sym(;, A, B) :-
        A=[59|B].
sym(',', A, B) :-
        A=[44|B].
sym('(', A, B) :-
        A=[40|B].
sym(')', A, B) :-
        A=[41|B].
sym(rop(=), A, B) :-
        A=[61|B].
sym(rop(#), A, B) :-
        A=[35|B].
sym(rop(>), A, B) :-
        A=[62|B].
sym(aop(+), A, B) :-
        A=[43|B].
sym(aop(-), A, B) :-
        A=[45|B].
sym(mop(*), A, B) :-
        A=[42|B].
sym(id(A), B, C) :-
        ident(D, B, C),
        name(A, D).
sym(num(A), B, C) :-
        number(D, B, C),
        name(A, D).

ident([A|B], C, D) :-
        letter(A, C, E),
        identr(B, E, D).
identr([A|B], C, D) :-
        letter(A, C, E),
        identr(B, E, D).
identr([A|B], C, D) :-
        digit(A, C, E),
        identr(B, E, D).
identr([], A, B) :-
        B=A.

number([A|B], C, D) :-
        digit(A, C, E),
        number(B, E, D).
number([A], B, C) :-
        digit(A, B, C).

letter(A, B, C) :-
        B=[A|C],
        (   65=<A,
            A=<90
        ;   97=<A,
            A=<122
        ).

digit(A, B, C) :-
        B=[A|C],
        48=<A,
        A=<57.

blank(A, B) :-
        A=[C|B],
        C=<32.

syntax_analysis(A, p(B,C,D)) :-
        prog(p(B,E,D), A, []),
        list_to_ord_set(B, F),
        list_to_ord_set(E, G),
        ord_union(F, G, C).

prog(p(A,B,C), D, E) :-
        D=['PROC'|F],
        F=[_|G],
        pars(A, G, H),
        decl(B, H, I),
        I=['BEGIN'|J],
        stats(C, J, K),
        K=['END'|E].

pars([A,B], C, D) :-
        C=['('|E],
        E=['IN'|F],
        F=[id(A)|G],
        G=[','|H],
        H=['OUT'|I],
        I=[id(B)|J],
        J=[')'|D].

decl(A, B, C) :-
        B=['VAR'|D],
        idlist(A, D, C).
decl([], A, B) :-
        B=A.

idlist([A|B], C, D) :-
        C=[id(A)|E],
        E=[','|F],
        idlist(B, F, D).
idlist([A], B, C) :-
        B=[id(A)|C].

stats((A;B), C, D) :-
        stat(A, C, E),
        E=[;|F],
        stats(B, F, D).
stats(A, B, C) :-
        stat(A, B, C).

stat(ass(A,B), C, D) :-
        C=[id(A)|E],
        E=[:=|F],
        expr(B, F, D).
stat(ifs(A,B,C), D, E) :-
        D=['IF'|F],
        cond(A, F, G),
        G=['THEN'|H],
        stats(B, H, I),
        I=['ELSE'|J],
        stats(C, J, K),
        K=['END'|E].
stat(whs(A,B), C, D) :-
        C=['WHILE'|E],
        cond(A, E, F),
        F=['DO'|G],
        stats(B, G, H),
        H=['END'|D].
stat(nil, A, B) :-
        B=A.

cond(bin(A,B,C), D, E) :-
        expr(B, D, F),
        F=[rop(A)|G],
        expr(C, G, E).

expr(A, B, C) :-
        term(D, B, E),
        exprr(D, A, E, C).

exprr(A, B, C, D) :-
        C=[aop(E)|F],
        term(G, F, H),
        exprr(bin(E,A,G), B, H, D).
exprr(A, A, B, C) :-
        C=B.

term(A, B, C) :-
        factor(D, B, E),
        termr(D, A, E, C).

termr(A, B, C, D) :-
        C=[mop(E)|F],
        factor(G, F, H),
        termr(bin(E,A,G), B, H, D).
termr(A, A, B, C) :-
        C=B.

factor(A, B, C) :-
        B=['('|D],
        expr(A, D, E),
        E=[')'|C].
factor(var(A), B, C) :-
        B=[id(A)|C].
factor(con(A), B, C) :-
        B=[num(A)|C].

declaration_analysis(p(_,A,B)) :-
        deca(B, A).

deca(nil, _).
deca(ass(A,B), C) :-
        checkdec(A, C),
        deca(B, C).
deca((A;B), C) :-
        deca(A, C),
        deca(B, C).
deca(ifs(A,B,C), D) :-
        deca(A, D),
        deca(B, D),
        deca(C, D).
deca(whs(A,B), C) :-
        deca(A, C),
        deca(B, C).
deca(bin(_,A,B), C) :-
        deca(A, C),
        deca(B, C).
deca(var(A), B) :-
        checkdec(A, B).
deca(con(_), _).

checkdec(A, B) :-
        member(A, B).

assignment_analysis(p([A,B],_,C)) :-
        asan(C, [A], D),
        checkass(B, D).

asan(nil, A, A).
asan(ass(A,B), C, D) :-
        asan(B, C),
        ord_add_element(C, A, D).
asan((A;B), C, D) :-
        asan(A, C, E),
        asan(B, E, D).
asan(ifs(A,B,C), D, E) :-
        asan(A, D),
        asan(B, D, F),
        asan(C, D, G),
        ord_intersection(F, G, H),
        ord_union(D, H, E).
asan(whs(A,B), C, C) :-
        asan(A, C),
        asan(B, C, _).
asan(bin(_,A,B), C) :-
        asan(A, C),
        asan(B, C).
asan(var(A), B) :-
        checkass(A, B).
asan(con(_), _).

checkass(A, B) :-
        member(A, B).

execution(p([A,B],C,D), E, F) :-
        make_B(C, G),
        next_B(A, E, G, H),
        exec(D, H, I),
        get_B(B, I, F).

exec(nil, A, A).
exec(ass(A,B), C, D) :-
        exec(B, C, E),
        next_B(A, E, C, D).
exec((A;B), C, D) :-
        exec(A, C, E),
        exec(B, E, D).
exec(ifs(A,B,_), C, D) :-
        exec(A, C, 1),
        exec(B, C, D).
exec(ifs(A,_,B), C, D) :-
        exec(A, C, 0),
        exec(B, C, D).
exec(whs(A,B), C, D) :-
        exec(A, C, 1),
        exec(B, C, E), !,
        exec(whs(A,B), E, D).
exec(whs(A,_), B, B) :-
        exec(A, B, 0).
exec(bin(A,B,C), D, E) :-
        exec(B, D, F),
        exec(C, D, G),
        exec(A, F, G, E).
exec(var(A), B, C) :-
        get_B(A, B, C).
exec(con(A), _, A).
exec(=, A, B, 1) :-
        A=B.
exec(=, A, B, 0) :-
        A=\=B.
exec(#, A, B, 1) :-
        A=\=B.
exec(#, A, B, 0) :-
        A=B.
exec(>, A, B, 1) :-
        A>B.
exec(>, A, B, 0) :-
        A=<B.
exec(+, A, B, C) :-
        C is A+B.
exec(-, A, B, C) :-
        C is A-B.
exec(*, A, B, C) :-
        C is A*B.

make_elemB(A, b(A,undef)).

make_B(A, B) :-
        map(make_elemB, A, B).

next_elemB(A, B, b(A,_), b(A,B)).
next_elemB(A, _, b(B,C), b(B,C)) :-
        \+A=B.

next_B(A, B, C, D) :-
        map(next_elemB(A,B), C, D).

get_B(A, B, C) :-
        member(b(A,C), B),
        \+C=undef.

is_ssa_program(p(A,B,C)) :-
        is_ssa_pars(A),
        is_count(B),
        is_ssa_stat(C).

is_ssa_pars([A,B]) :-
        is_ssa_id(A),
        is_ssa_id(B).

is_count([]).
is_count([c(A,B)|C]) :-
        is_id(A),
        number(B),
        is_count(C).

is_ssa_stat(nil).
is_ssa_stat(ass(A,B)) :-
        is_ssa_id(A),
        is_ssa_expr(B).
is_ssa_stat((A;B)) :-
        is_ssa_stat(A),
        is_ssa_stat(B).
is_ssa_stat(ifs(A,B,C,D)) :-
        is_ssa_expr(A),
        is_ssa_stat(B),
        is_ssa_stat(C),
        is_philist(D).
is_ssa_stat(whs(A,B,C)) :-
        is_philist(A),
        is_ssa_expr(B),
        is_ssa_stat(C).

is_philist([]).
is_philist([phi(A,B,C)|D]) :-
        is_ssa_id(A),
        is_phi_arg(B),
        is_phi_arg(C),
        is_philist(D).

is_phi_arg(var(A)) :-
        is_ssa_id(A).
is_phi_arg(con(A)) :-
        is_con(A).

is_ssa_expr(bin(A,B,C)) :-
        is_ssa_expr(B),
        is_ssa_expr(C),
        is_op(A).
is_ssa_expr(var(A)) :-
        is_ssa_id(A).
is_ssa_expr(con(A)) :-
        is_con(A).

is_ssa_id(A/B) :-
        is_id(A),
        number(B).
ssa_generation(A, B) :-
        ssa_generation1(A, B),
        ssa_generation2(B).
ssa_generation1(p([A,B],C,D), p([A/E,B/F],G,H)) :-
        make_C(C, I),
        make_A(I, J),
        next_C(A, I, K, E),
        next_A(A, E, J, L),
        ssa1(D, L, M, K, G, H),
        get_A(B, M, F).
ssa1(nil, A, A, B, B, nil).
ssa1(ass(A,B), C, D, E, F, ass(A/G,H)) :-
        ssa1(B, H),
        next_C(A, E, F, G),
        next_A(A, G, C, D).
ssa1((A;B), C, D, E, F, (G;H)) :-
        ssa1(A, C, I, E, J, G),
        ssa1(B, I, D, J, F, H).
ssa1(ifs(A,B,C), D, E, F, G, ifs(H,I,J,K)) :-
        ssa1(A, H),
        ssa1(B, D, L, F, M, I),
        ssa1(C, D, N, M, O, J),
        phigen1(L, N, D, E, O, G, K).
ssa1(whs(A,B), C, D, E, F, whs(G,H,I)) :-
        ssa1(A, H),
        ssa1(B, C, J, E, K, I),
        phigen1(C, J, C, D, K, F, G).
ssa1(bin(A,B,C), bin(A,D,E)) :-
        ssa1(B, D),
        ssa1(C, E).
ssa1(var(A), var(A/_)).
ssa1(con(A), con(A)).
phigen1([], [], A, A, B, B, []).
phigen1([A|B], [A|C], D, E, F, G, H) :-
        phigen1(B, C, D, E, F, G, H).
phigen1([a(A,B)|C], [a(A,D)|E], F, G, H, I, J) :-
        \+B=D,
        next_C(A, H, K, L),
        next_A(A, L, F, M),
        J=[phi(A/L,var(A/_),var(A/_))|N],
        phigen1(C, E, M, G, K, I, N).
make_elemC(A, c(A,0)).
make_C(A, B) :-
        map(make_elemC, A, B).
next_elemC(c(A,B), c(A,_), c(A,B)).
next_elemC(c(A,_), c(B,C), c(B,C)) :-
        \+A=B.
next_C(A, B, C, D) :-
        get_C(A, B, E),
        D is E+1,
        map(next_elemC(c(A,D)), B, C).
get_C(A, B, C) :-
        member(c(A,C), B).
make_elemA(c(A,_), a(A,0)).
make_A(A, B) :-
        map(make_elemA, A, B).
next_elemA(a(A,B), a(A,_), a(A,B)).
next_elemA(a(A,_), a(B,C), a(B,C)) :-
        \+A=B.
next_A(A, B, C, D) :-
        map(next_elemA(a(A,B)), C, D).
get_A(A, B, C) :-
        member(a(A,C), B).
ssa_generation2(p([A/B,_],C,D)) :-
        make_A(C, E),
        next_A(A, B, E, F),
        ssa2(D, F, _).
ssa2(nil, A, A).
ssa2(ass(A/B,C), D, E) :-
        ssa2(C, D),
        next_A(A, B, D, E).
ssa2((A;B), C, D) :-
        ssa2(A, C, E),
        ssa2(B, E, D).
ssa2(ifs(A,B,C,D), E, F) :-
        ssa2(A, E),
        ssa2(B, E, G),
        ssa2(C, E, H),
        assnphi(D, E, F),
        genphi2(D, G, H).
ssa2(whs(A,B,C), D, E) :-
        assnphi(A, D, E),
        ssa2(B, E),
        ssa2(C, E, F),
        genphi2(A, D, F).
ssa2(bin(_,A,B), C) :-
        ssa2(A, C),
        ssa2(B, C).
ssa2(var(A/B), C) :-
        get_A(A, C, B).
ssa2(con(_), _).
assnphi([], A, A).
assnphi([phi(A/B,_,_)|C], D, E) :-
        next_A(A, B, D, F),
        assnphi(C, F, E).
genphi2([], _, _).
genphi2([phi(A/_,var(A/B),var(A/C))|D], E, F) :-
        get_A(A, E, B),
        get_A(A, F, C),
        genphi2(D, E, F).
ssa_deletion(p([A/_,B/_],_,C), p([A,B],D,E)) :-
        ssad(C, F),
        list_to_ord_set([A,B], G),
        new_dec_list(F, G, D),
        nil_elimination(F, E).
ssad(nil, nil).
ssad(ass(A/_,B), ass(A,C)) :-
        ssad(B, C).
ssad((A;B), (C;D)) :-
        ssad(A, C),
        ssad(B, D).
ssad(ifs(A,B,C,D), ifs(E,(F;G),(H;I))) :-
        ssad(A, E),
        ssad(B, F),
        ssad(C, H),
        delphi1(D, G),
        delphi2(D, I).
ssad(whs(A,B,C), (D;whs(E,(F;G)))) :-
        ssad(B, E),
        ssad(C, F),
        delphi1(A, D),
        delphi2(A, G).
ssad(bin(A,B,C), bin(A,D,E)) :-
        ssad(B, D),
        ssad(C, E).
ssad(var(A/_), var(A)).
ssad(con(A), con(A)).
delphi1([], nil).
delphi1([phi(_/_,var(_),_)|A], B) :-
        delphi1(A, B).
delphi1([phi(A/_,con(B),_)|C], (ass(A,con(B));D)) :-
        delphi1(C, D).
delphi2([], nil).
delphi2([phi(_/_,_,var(_))|A], B) :-
        delphi2(A, B).
delphi2([phi(A/_,_,con(B))|C], (ass(A,con(B));D)) :-
        delphi2(C, D).
new_dec_list(A, B, C) :-
        newd(A, B, C).
newd(nil, A, A).
newd(ass(A,_), B, C) :-
        ord_add_element(B, A, C).
newd((A;B), C, D) :-
        newd(A, C, E),
        newd(B, E, D).
newd(ifs(_,A,B), C, D) :-
        newd(A, C, E),
        newd(B, E, D).
newd(whs(_,A), B, C) :-
        newd(A, B, C).
nil_elimination(A, B) :-
        fixpoint(nile, A, B).
nile(nil, nil).
nile(ass(A,B), ass(A,B)).
nile((A;B), (C;D)) :-
        \+A=nil,
        \+B=nil,
        nile(A, C),
        nile(B, D).
nile((A;nil), B) :-
        \+A=nil,
        nile(A, B).
nile((nil;A), B) :-
        \+A=nil,
        nile(A, B).
nile((nil;nil), nil).
nile(ifs(A,B,C), ifs(A,D,E)) :-
        nile(B, D),
        nile(C, E).
nile(whs(A,B), whs(A,C)) :-
        nile(B, C).
ssa_optimization(A, A).

%%%%%%%%%%%%%%%%%%

append([], List, List).
append([Head|Tail], List, [Head|Rest]) :- 
	append(Tail, List, Rest).

ord_union(Sets, Union) :-
	length(Sets, NumberOfSets),
	ord_union_all(NumberOfSets, Sets, Union, []).

ord_union_all(1, [Set|Sets], Set, Sets) :- !.
ord_union_all(2, [Set,Set2|Sets], Union, Sets) :- !,
	ord_union(Set, Set2, Union).
ord_union_all(N, Sets0, Union, Sets) :-
	A is N>>1,
	Z is N-A,
	ord_union_all(A, Sets0, X, Sets1),
	ord_union_all(Z, Sets1, Y, Sets),
	ord_union(X, Y, Union).



ord_intersection([], _, []).
ord_intersection([Head1|Tail1], Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

ord_intersection3(<, _, Set1, Head2, Tail2, Intersection) :-
	ord_intersection3(Set1, Head2, Tail2, Intersection).
ord_intersection3(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection3(>, Head1, Tail1, _, Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

ord_intersection3([], _, _, []).
ord_intersection3([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection3(Order, Head1, Tail1, Head2, Tail2, Intersection).


ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).

ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).


list_to_ord_set(List, Set) :-
	sort(List, Set).


member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).

%ident(I1,I2):-cti:{n(I2)>n(I1)}.
ident(I1,[I|I2]).
number(I1,[I|I2]).
%number(I1,I2):-cti:{n(I2)>=1+n(I1)}.


ord_union(Set1, Set2, Union) :-
	merge(Set1, Set2, Union).


merge([], Set, Set).
merge([O|Os], Ns, Set) :- merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).

