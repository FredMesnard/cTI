
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