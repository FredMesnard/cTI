

append([], List, List).
append([Head|Tail], List, [Head|Rest]) :- 
	append(Tail, List, Rest).


%   delete(+List, +Element, ?Residue)
%   is true when all *identical* occurences of Element in List are removed 
%   and the result is Residue.  

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).


%   is_list(+List)
%   is true when List is a proper list.

is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|Tail]) :- is_list(Tail).


%   last(?List, ?Element)
%   is true when Element is the last element in List.

last([Head|Tail], Element) :- last(Tail, Head, Element).

last([], Element, Element).
last([Head|Tail], _, Element) :- last(Tail, Head, Element).


%   member(?Element, +List)
%   is true when Element is a member of List.  It may be used to test 
%   for membership in a list, but it can also be used to enumerate all 
%   the elements in List.

member(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).


%   memberchk(+Element, +List)
%   is true when Element is a member of List, but memberchk/2 only succeeds
%   once and can therefore not be used to enumerate the elements in List.

memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :-
	memberchk(Element, Rest).


%   nextto(?X, ?Y, +List)
%   is true when X and Y appears side-by-side in List.

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Rest]) :-
	nextto(X, Y, Rest).


%   no_doubles(+List) 
%   is true when the List contains no duplicate elements.

no_doubles([]).
no_doubles([Head|Tail]) :-
	non_member_(Tail, Head),
	no_doubles(Tail).


%   non_member(+Element, +List)
%   non_member is true when Element does not exist in List.

non_member(Element, List) :-
	non_member_(List, Element).


non_member_([], _).
non_member_([Head|Tail], Element) :-
	dif(Head, Element),
	non_member_(Tail, Element).

%   nth(?N, +List, ?Element)
%   nth/3 is true when Element is the Nth element of List, counting the first
%   element as 1.


nth(N, List, Element) :-
	%integer(N), !,
	cti:{N >= 1, N1 = N-1},
	nth0i(N1, List, Element).
nth(N, List, Element) :-
	var(N),
	cti:{Un = 1},
	nth0v(List, Element, Un, N).


%   nth(?N, +List, ?Element, ?Rest)
%   nth is true when Element is the N:th element in List and Rest is all the
%   elements in the List except Element.

nth(N, List, Element, Rest) :-
	%integer(N), !,
	cti:{N >= 1, N1 = N-1},
	nth0i(N1, List, Element, Rest).
nth(N, List, Element, Rest) :-
	var(N),
	cti:{U=1},
	nth0v(List, Element, U, N, Rest).


%   nth0(?N, +List, ?Element)
%   nth0/3 is true when Element is the Nth element of List, counting the first
%   element as 0.


nth0(N, List, Element) :-
	%integer(N), !,
	cti:{N >= 0},
	nth0i(N, List, Element).
nth0(N, List, Element) :-
	%var(N),
	cti:{Z=0},
	nth0v(List, Element, Z, N).

nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	cti:{M < Index, N = M + 1},
	nth0v(Tail, Element, N, Index).

nth0i(Z, List, Head) :- !,cti:{Z=0},
	List = [Head|_].
nth0i(N, [_|Tail], Element) :-
	cti:{M = N - 1,N > 0},
	nth0i(M, Tail, Element).


%   nth0(?N, +List, ?Element, ?Rest)
%   nth0/4 unifies Element with the nth element in List, counting the
%   first element as 0 and Rest with rest of the elements.

nth0(N, List, Element, Rest) :-
	%integer(N), !,
	cti:{N >= 0},
	nth0i(N, List, Element, Rest).
nth0(N, List, Element, Rest) :-
	%var(N),
	cti:{Z = 0},
	nth0v(List, Element, Z, N, Rest).

nth0v([Element|Tail], Element, Index, Index, Tail).
nth0v([Head|Tail], Element, M, Index, [Head|Rest]) :-
	cti:{M < Index, N = M + 1},
	nth0v(Tail, Element, N, Index, Rest).

nth0i(Z, List, Head, Tail) :- !,cti:{Z=0},
	List = [Head|Tail].
nth0i(N, [Head|Tail], Element, [Head|Rest]) :-
	cti:{N > 0, M = N - 1},
	nth0i(M, Tail, Element, Rest).


%   permutation(?List, ?Perm)
%   is true when Perm is a permutation of List.

permutation([], []).
permutation(List, [First|Perm]) :- 
	select(First, List, Rest),
	permutation(Rest, Perm).


%   prefix(?Prefix, +List)
%   is true when Prefix is a prefix of List.

prefix([], _).
prefix([X|PreTail], [X|Tail]) :-
	prefix(PreTail, Tail).


%   remove_duplicates(+List, ?Pruned)
%   is true when Pruned is like List but with all *identical* duplicate 
%   elements removed.

remove_duplicates([], []).
remove_duplicates([Head|Tail1], [Head|Tail2]) :- 
	delete(Tail1, Head, Residue),
        remove_duplicates(Residue, Tail2).


%   reverse(?List, ?Reversed)
%   is true when Reversed is has the same element as List but in a reversed 
%   order. List must be a proper list.

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], SoFar, Reversed) :-
	reverse(Tail, [Head|SoFar], Reversed).


%   same_length(?List1, ?List2)
%   is true when List1 and List2 have the same number of elements.

same_length([], []).
same_length([_|L1], [_|L2]) :-
	same_length(L1, L2).


%   same_length(?List1, ?List2, ?Length)
%   is true when List1 and List2 have the same number of elements and
%   the length is Length.

same_length(L1, L2, Length) :-
	nonvar(Length), !,
	length(L1, Length),
	length(L2, Length).
same_length(L1, L2, Length) :-
	cti:{Zero = 0},
	same_length(L1, L2, Zero, Length).

same_length([], [], N, M):-cti:{N=M}.
same_length([_|L1], [_|L2], N0, N) :-
	cti:{N1 = N0+1, N0 < N},
	same_length(L1, L2, N1, N).



%   select(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.

select(Element, [Element|Tail], Tail).
select(Element, [Head|Tail1], [Head|Tail2]) :- 
	select(Element, Tail1, Tail2).


%   sublist(?Sub, +List)
%   is true when all members of Sub are members of List

sublist(List, List).
sublist(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

sublist_(Sub, _, Sub).
sublist_([Head|Tail], _, Sub) :- sublist_(Tail, Head, Sub).
sublist_([Head|Tail], X, [X|Sub]) :- sublist_(Tail, Head, Sub).


%   substitute(?X, ?Xlist, ?Y, ?Ylist)
%   is true when Xlist and Ylist are identical lists except for the 
%   corresponding elements X and Y.


substitute(_, [], _, []) :- !.
substitute(OldElem, [OldHead|OldRest], NewElem, [NewElem|NewRest]) :-
	OldElem==OldHead, !,
	substitute(OldElem, OldRest, NewElem, NewRest).
substitute(OldElem, [NotElem|OldRest], NewElem, [NotElem|NewRest]) :-
	substitute(OldElem, OldRest, NewElem, NewRest).


%   suffix(?Suffix, +List)
%   is true Suffix is an ending part of List.

suffix(Suffix, Suffix).
suffix(X, [_|Tail]) :-
	suffix(X, Tail).

%   max_list(+ListOfNumbers, ?Max)
%   is true when Max is the greatest of the numbers in the ListOfNumbers.


max_list([Head|Tail], Max) :- 
	max_list(Tail, Head, Max).

max_list([], Max, Max).
max_list([Head|Tail], Element, Max) :-
	Head =< Element, !,
	max_list(Tail, Element, Max).
max_list([Head|Tail], _, Max) :-
	max_list(Tail, Head, Max).


%   min_list(+ListOfNumbers, ?Min)
%   is true when Min is the smallest of the numbers in ListOfNumbers.

min_list([Head|Tail], Min) :- 
	min_list(Tail, Head, Min).

min_list([], Min, Min).
min_list([Head|Tail], Element, Min) :-
	Head >= Element, !,
	min_list(Tail, Element, Min).
min_list([Head|Tail], _, Min) :-
	min_list(Tail, Head, Min).


%   sum_list(+ListOfNumbers, ?Sum)
%   is true when the sum of ListOfNumbers is Sum. It can be used to check
%   a sum or to calculate the sum.

sum_list(List, Sum) :-
	sum_list(List, 0, Sum).

sum_list([], Sum, Sum).
sum_list([Head|Tail], Sum0, Sum) :-
	Sum1 is Head+Sum0,
	sum_list(Tail, Sum1, Sum).

