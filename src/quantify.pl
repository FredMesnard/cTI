:- module(quantify,[ quantify/1 ]).

:- use_module(library(clpb)).

% quantify(+ListTermConds)
quantify([]):-!.
quantify(L2):-
	%trace,
	compte0s(L2,Per,_N,_Z),
	term_modes(L2,0,NTermModes),
	write('%  -------------------------------'),nl,
	%write('%  '),write(N),write(' predicates, of which '),%nl,
	%write('%  '),write(Z),write(' are possibly non-terminating'),nl,
	write('%  terminating predicates: '),format("~2d%",[Per]),nl,
	write('%  terminating modes: '),write(NTermModes),nl,
	write('%  -------------------------------'),nl.

compte0s(L,Q,N,Z):-
	length(L,N),
	c0s(L,0,Z),
	percent(N-Z,N,Q).

c0s([],N,N).
c0s([predicate_term_condition(_,CT)|L],N1,N2):-
	(CT==0 -> N3 is N1+1; N3 is N1),
	c0s(L,N3,N2).
	
percent(A,B,Q):- Q is (A*10000)//B.

term_modes([],N,N).
term_modes([predicate_term_condition(Atom,CT)|L],N0,N) :-
	Atom=..[_|Vars],
	findall(Vars,clpb:(sat(CT),labeling(Vars)),TermModes),
	length(TermModes,Ntm),
	N1 is N0+Ntm,
	term_modes(L,N1,N).


