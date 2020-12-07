mergesort([],[]).
mergesort([E],[E]).
mergesort([X,Y|Z],U) :- s(Z,V,W),
                        mergesort([X|V],X1), mergesort([Y|W],Y1),
                        merge(X1,Y1,U).
merge([],X,X).
merge([A|X],[B|Y],[A|Z])           :- A=<B,merge(X,[B|Y],Z).
merge([A|X],[B|Y],[B|Z]) :- A>B,merge([A|X],Y,Z).
s([],[],[]).
s([E|U],[E|V],W) :- s(U,W,V).

