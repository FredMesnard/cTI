%   ord_union(+Sets, ?Union) 
%   is true when Union is the union of all the sets in Sets. 

ord_union([], Union) :- !, Union = [].
ord_union(Sets, Union) :-
	length(Sets, NumberOfSets),
	ord_union_all(NumberOfSets, Sets, Union, []).

ord_union_all(U, [Set|Sets], Set, Sets) :- true.%cti:{U=1},!.
ord_union_all(D, [Set,Set2|Sets], Union, Sets) :- true,%cti:{D=2},!,
	ord_union(Set, Set2, Union).
ord_union_all(N, Sets0, Union, Sets) :-
	cti:{N>2,2*A=<N,N=<2*A+1,Z=N-A},
	%A is N>>1,
	%Z is N-A,
	ord_union_all(A, Sets0, X, Sets1),
	ord_union_all(Z, Sets1, Y, Sets),
	ord_union(X, Y, Union).

ord_union(S1,S2,U):-merge(S1,S2,U).

merge([], Set, Set).
merge([O|Os], Ns, Set) :- merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).

