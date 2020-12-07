expr_bl(Xs,Xs, L,L).
expr_bl([op|Xs0],Xs, l(L0),L) :-
        expr_bl2(Xs0,Xs1, L0,L1),
        expr_bl2(Xs1,Xs, L1,L).

expr_bl2(Xs0,Xs, L0,L) :-
        expr_bl(Xs0,Xs, L0,L).

