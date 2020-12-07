mergesort([],[]).
mergesort([E],[E]).
mergesort([E,F|U],V) :- s([E,F|U],W,Y),
                        mergesort(W,X), mergesort(Y,Z),
                        merge(X,Z,V).
merge(X,[],X).
merge([],X,X).
merge([A|X],[B|Y],[A|Z])           :- A=<B,merge(X,[B|Y],Z).
merge([A|X],[B|Y],[B|Z]) :- A>B,merge([A|X],Y,Z).
s([],[],[]).
s([E|U],[E|V],W) :- s(U,W,V).

