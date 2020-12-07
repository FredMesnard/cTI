assoc_to_list(t, A, B) :-
        B=A.
assoc_to_list(t(A,B,_,C,D), E, F) :-
        assoc_to_list(C, E, G),
        'C'(G, A-B, H),
        assoc_to_list(D, H, F).

pseudopredC([X|S],X,S).

gen_assoc(Key, t(K,V,_,L,R), Val) :-
	(   gen_assoc(Key, L, Val)
	;   Key = K, Val = V
	;   gen_assoc(Key, R, Val)
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


put_assoc(Key, Assoc0, Val, Assoc1) :-
	put_assoc(Assoc0, Key, Val, Assoc1, _).


put_assoc(t,            Key, Val, t(Key,Val,0,t,t), 1).
put_assoc(t(K,V,B,L,R), Key, Val, Result, Delta) :-
	compare(O, Key, K),
	put_assoc(O, Key, Val, Result, Delta, K, V, B, L, R).


put_assoc(<, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(L, Key, Val, Lassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B-D1,
	assoc(B1, K, V, Lassoc, R, Assoc).
put_assoc(=, Key, Val, t(Key,Val,B,L,R), 0, _, _, B, L, R).
put_assoc(>, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(R, Key, Val, Rassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B+D1,
	assoc(B1, K, V, L, Rassoc, Assoc).


assoc(-2, K, V, L, R, Assoc) :-
	L = t(K1,V1,B1,L1,R1),
	assoc_left(B1, K1, V1, L1, R1, K, V, R, Assoc).
assoc(-1, K, V, L, R, t(K,V,-1,L,R)).
assoc( 0, K, V, L, R, t(K,V, 0,L,R)).
assoc( 1, K, V, L, R, t(K,V, 1,L,R)).
assoc( 2, K, V, L, R, Assoc) :-
	R = t(K1,V1,B1,L1,R1),
	assoc_right(B1, K1, V1, L1, R1, K, V, L, Assoc).

assoc_left(-1, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 0,L1,t(K,V, 0,R1,R))).
assoc_left( 0, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 1,L1,t(K,V,-1,R1,R))).
assoc_left( 1, K1, V1, L1, R1, K, V, R,		% double LR rotation
	    t(K2,V2, 0,t(K1,V1,BK1,L1,L2),t(K,V,BK,R2,R))) :-
        R1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK1, BK).

assoc_right( 1, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1, 0,t(K,V, 0,L,L1),R1)).
assoc_right( 0, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1,-1,t(K,V, 1,L,L1),R1)).
assoc_right(-1, K1, V1, L1, R1, K, V, L,	% double RL rotation
	     t(K2,V2, 0,t(K,V,BK,L,L2),t(K1,V1,BK1,R2,R1))) :-
        L1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK, BK1).

assoc(-1,  0, 1).
assoc( 0,  0, 0).
assoc( 1, -1, 0).

%  --------
%  -- STATS
%  ------------------------------
%  TEMPS (ms)
% total : 25940
%  max___time : 1760000
%  modele_nat : 2030
%  ens_levmap : 3980
%  levmap_max : 15710
%  plcN__plcB : 100
%  model_bool : 1620
%  cond_termg : 2260
%  -------------------------------
%  MEMOIRE (octets)
%  nombre de gc  : 7
%  espace libere par gc          : 8303316
%  -------------------------------
%  QUALITE
%  modes :
%    total  : 6392
%    couvrt : 3228
%    global : 50%
%    moyen  : 65%
%  taux couverture : 100%
%  -------------------------------        