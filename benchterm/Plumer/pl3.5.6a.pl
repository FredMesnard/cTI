p([A]) :- l([A]).
q([A]).
r(1).
l([]).
l([H|T]) :- r(H),l(T).


/*
predicate_term_condition(l(_21558),_21558),
predicate_term_condition(p(_21640),_21640),
predicate_term_condition(q(_21743),1),
predicate_term_condition(r(_21821),1)])
*/
/*
cti(l,1,[[1]]).
cti(p,1,[[]]).
cti(q,1,[[]]).
cti(r,1,[[]]).
*/