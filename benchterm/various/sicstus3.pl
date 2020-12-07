% sicstus3 Association list primitives

get_assoc(Key, t(K,V,L,R), Val) :-
        compare(Rel, Key, K),
        get_assoc(Rel, Key, V, L, R, Val).

get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
        get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
        get_assoc(Key, Tree, Val).

put_assoc(Key, t, Val, Tree) :- !, Tree = t(Key,Val,t,t).
put_assoc(Key, t(K,V,L,R), Val, New) :-
        compare(Rel, Key, K),
        put_assoc(Rel, Key, K, V, L, R, Val, New).

put_assoc(=, Key, _, _, L, R, Val, t(Key,Val,L,R)).
put_assoc(<, Key, K, V, L, R, Val, t(K,V,Tree,R)) :-
        put_assoc(Key, L, Val, Tree).
put_assoc(>, Key, K, V, L, R, Val, t(K,V,L,Tree)) :-
        put_assoc(Key, R, Val, Tree).

