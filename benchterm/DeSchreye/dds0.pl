
append([A|X],Y,[A|Z]) :- append(X,Y,Z).
append([],Y,Y).

append3(X,Y,Z,T):-append(X,Y,U),append(U,Z,T).