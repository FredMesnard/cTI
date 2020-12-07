
% merge(S1,S2,S3) => cti:{n(S1)+n(S2)>=n(S3),n(S3)>=n(S1),n(S3)>=n(S2)}
merge([], Set, Set).
merge(S1, Ns, Set) :- S1=[O|Os],cti:{n(S1)+n(Ns)>=n(Set),n(Set)>=n(Ns),n(Set)>=n(S1)},merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).


%   subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

% subtract(S1,S2,D) =>  cti:{n(S1)>=n(D)}.
subtract([], _, []).
subtract([Head1|Tail1], Set2, Difference) :-
	cti:{n(Tail1)+n(Head1)>n(Difference)},
	subtract3(Set2, Head1, Tail1, Difference).

subtract3(<, Head, Set1, Head2, Tail2, [Head|Difference]) :-
	(   Set1 = [], Difference = []
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    subtract3(Order, Head1, Tail1, Head2, Tail2, Difference)
	).
subtract3(=, _, Tail1, _, Tail2, Difference) :-
	subtract(Tail1, Tail2, Difference).
subtract3(>, Head1, Tail1, _, Set2, Difference) :-
	subtract3(Set2, Head1, Tail1, Difference).

subtract3([], Head1, Tail1, [Head1|Tail1]).
subtract3([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	subtract3(Order, Head1, Tail1, Head2, Tail2, Difference).
