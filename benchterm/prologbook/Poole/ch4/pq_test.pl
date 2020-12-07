%This code is used to compare the runtime of pq.pl with the
% standard heaps module.

% timepq(+N,-T) returns the time T to heapsort a random list of N elements
% using the pq implementation
timepq(N,T) :-
     random_list(N,L),
     statistics(runtime,_),
     myheapsort(L,_),
     statistics(runtime,[_,T]).

% timeheap(+N,-T) returns the time T to heapsort a random list of N elements
% using the heaps implementation
timeheap(N,T) :-
     random_list(N,L),
     statistics(runtime,_),
     theirheapsort(L,_),
     statistics(runtime,[_,T]).



% the following code does an equivalent heapsort using heaps.

%   nlist_to_heap(+List, -Heap)
%   takes a list of Keys and forms them into a heap.  


nlist_to_heap(List, Heap) :-
	nlist_to_heap(List, 0, t, Heap).

nlist_to_heap([], N, Tree, t(N,[],Tree)) :- !.
nlist_to_heap([Key|Rest], M, OldTree, Heap) :-
	N is M+1,
	heaps:add_to_heap(N, Key, Key, OldTree, MidTree),
	nlist_to_heap(Rest, N, MidTree, Heap).



% myheapsort(+L,-S) sorts list L of numbers into list S if Nun-Num pairs
myheapsort(L,S) :-
   pq:nlist_to_PQ(L,PQ),
   pq:pq_to_list(PQ,S).

theirheapsort(L,S) :-
   nlist_to_heap(L,H),
   heaps:heap_to_list(H,S).
