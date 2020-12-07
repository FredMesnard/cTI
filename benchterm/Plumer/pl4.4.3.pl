merge(X,[],X).
merge([],X,X).
merge([A|X],[B|Y],[A|Z]) :- A=<B, merge(X,[B|Y],Z).
merge([A|X],[B|Y],[B|Z]) :- A>B, merge([A|X],Y,Z).
