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
                (     C=[61|D];
                      C=[35|D];
                      C=[62|D]
                ),
                expr(D, B).
 expr(A, B) :-
                term(A, C),
                exprr(C, B).
 exprr(A, B) :-
                (     A=[43|C];
                      A=[45|C]
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
 ident(A, B) :-
                A=[97|B].
 number(A, B) :-
                A=[50|C],
                C=[55|B].


