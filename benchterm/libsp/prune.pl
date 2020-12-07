
prune(Graph0, [], K, Graph) :- cti:{K=0}, !, Graph = Graph0.
/*
prune(Graph0, Vs0, K, Graph) :- cti:{K>0},
	prune(Graph0, Vs0, K, Graph1, Vs1), % model N de prune 5?
	(   Vs1=[] -> Graph = Graph1
	;   prune(Graph1, Vs1, K, Graph)
	).
*/
prune(Graph0, Vs0, K, Graph) :- cti:{K>0},
	prune(Graph0, Vs0, K, Graph1, Vs1), % model N de prune 5?
	Vs1==[],
	Graph = Graph1.
prune(Graph0, Vs0, K, Graph) :- cti:{K>0},
	prune(Graph0, Vs0, K, Graph1, Vs1), % model N de prune 5?
	prune(Graph1, Vs1, K, Graph).

prune(A,B,C,D,E):-cti:{n(A)>=n(E)}.


merge(S1,S2,S3):-cti:{n(S1)+n(S2)>=n(S3),n(S3)>=n(S1),n(S3)>=n(S2)},merge2(S1,S2,S3).
merge2([], Set, Set).
merge2([O|Os], Ns, Set) :- merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).

