/*
color_stack([]).
color_stack([V-Neibs|Stk]) :-
	sort(Neibs, Set),
	%Set=Neibs,
	color_stack(Set, 1, V, Stk).

color_stack([I|Is], I, V, Stk) :- !, color_stack(Is, J, V, Stk).
color_stack(_, V, V, Stk) :- color_stack(Stk).
%color_stack(_, V, V, []).
%color_stack(_, V, V, [W-Neibs|Stk]) :-
%	sort(Neibs, Set),
%	%cti:{n(Set)>=n(Neibs)},
%	color_stack(Neibs, 1, W, Stk).


%test([]).
%test([x|Xs]):-sort(Xs,Ys),test(Ys).
*/

visit([], A, A, B, B, C, C, D, D, E, F) :-
        F=E.
visit([A|B], C, D, E, F, G, H, I, J, K, L) :-
        get_assoc(A, E, node(M,N,O), P, node(M,Q,O)),
        (   N>0 ->
            N=Q,
            N=R,
            P=S,
            G=T,
            I=U,
            V=K
        ;   Q is G+1,
            visit(M, Q, R, P, W, Q, T, [A|I], X, K, Y),
            (   Q>R ->
                W=S,
                X=U,
                V=Y
            ;   pop(A, O, W, S, X, U, [], Y, V)
            )
        ),
        ctimin(C,R,Z),
        visit(B, Z, D, S, F, T, H, U, J, V, L).

ctimin(C,R,C):-cti:{C=<R}.
ctimin(C,R,R):-cti:{C>R}.

%pop(V, Eq, A0, A, [V1|Stk0], Stk, SCC0) -->
%	{get_assoc(V1, A0, node(Ns,_,Eq), A1, node(Ns,16'100000,Eq))},
%	(   {V==V1} -> [SCC], {A1=A, Stk0=Stk, sort([V1|SCC0], SCC)}
%	;   pop(V, Eq, A1, A, Stk0, Stk, [V1|SCC0])
%	).

pop(A, B, C, D, [E|F], G, H, I, J) :-
        get_assoc(E, C, node(K,_,B), L, node(K,1048576,B)),
        (   A==E ->
            pseudopredC(I, M, J),
            L=D,
            F=G,
            sort([E|H], M)
        ;   pop(A, B, L, D, F, G, [E|H], I, J)
        ).


get_assoc(Key, t(K,V,_,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, Val, V, L, R).

get_assoc(<, Key, Val, _, Tree, _) :- get_assoc(Key, Tree, Val).
get_assoc(=, _, Val, Val, _, _).
get_assoc(>, Key, Val, _, _, Tree) :- get_assoc(Key, Tree, Val).


get_assoc(Key, t(K0,V0,B,L0,R0), Val0, t(K,V,B,L,R), Val) :-
	compare(Rel, Key, K0),
	get_assoc(Rel, Key, K0, V0, L0, R0, Val0, K, V, L, R, Val).

get_assoc(<, Key, K, V, Tree0, R, Val0, K, V, Tree, R, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).
get_assoc(=, _, K, Val0, L, R, Val0, K, Val, L, R, Val).
get_assoc(>, Key, K, V, L, Tree0, Val0, K, V, L, Tree, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).

pseudopredC([X|S],X,S).
