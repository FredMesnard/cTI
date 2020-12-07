%:- import(prolog_types).

%:- pred permute(list(T),list(T)).

permute([],[]).
permute([X|Y],[U|V]) :- delete(U,[X|Y],W),permute(W,V).


%:- pred delete(T,list(T),list(T)).

delete(X,[X|Y],Y).
delete(U,[X|Y],[X|Z]) :- delete(U,Y,Z).

