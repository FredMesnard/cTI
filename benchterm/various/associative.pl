rewrite(op(op(A,B),C),op(A,op(B,C))) :- !.
rewrite(op(A,op(B,C)),op(A,L)) :-
	rewrite(op(B,C),L).

normal_form(F,N) :- rewrite(F,F1), !, normal_form(F1,N).
normal_form(F,F).

