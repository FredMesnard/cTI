mergesort([],[]).
mergesort([e],[e]).
mergesort([e,f|U],V) :- s([e,f|U],W,Y),
                        mergesort(W,X), mergesort(Y,Z),
                        merge(X,Z,V).
merge(X,[],X).
merge([],X,X).
merge([a|X],[b|Y],[a|Z])           :- a=<b,merge(X,[b|Y],Z).
merge([a|X],[b|Y],[b|Z]) :- a>b,merge([a|X],Y,Z).
s([],[],[]).
s([e|U],[e|V],W) :- s(U,W,V).

