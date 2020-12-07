%:- import(prolog_types).


%:- pred reverse(list(T),list(T)).
reverse(X,Y):- reverse(X,[],Y).


%:- pred reverse(list(T),list(T),list(T)).
reverse([],X,X).
reverse([X|Y],Z,U) :- reverse(Y,[X|Z],U).
