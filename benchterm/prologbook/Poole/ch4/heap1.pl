\subsection{Priority Queues}

\index{priorityqueues}
In this section, we implement priority queues as a variant of a heap,
using Prolog-style trees.  The empty queue is represented by $nil$; 
otherwise the queue is represented recursively as	
\[pq(OBJ,LPQ,RPQ)\]
where $OBJ$ is the data element, and $LPQ$ \& $RPQ$ are the left and
right priority queues respectively.  Note that $OBJ$ is the data
element with higher priority than any element in either of 
$LPQ$ or $RPQ$.  

In our priority queues, we maintain the following invariant: {\em The
right subtree $RPQ$ has either the same number of data elements or
exactly one more than the left subtree $LPQ$.}  This invariant assures
that both the insert and delete operations have $O(\log n)$ time
complexity, where $n$ is the number of elements in the queue.

There are two operations on priority queues: insertion and deletion.

The insertion operation behaves as follows: $insertPQ(X,Q1,Q2)$ is
true if inserting $X$ into queue $Q1$ results in queue $Q2$.  The
invariant is maintained by inserting data elements into the left 
subtree of $Q1$, creating a new subtree.  The new subtree is made 
the right subtree of $Q2$, and the right subtree of $Q1$ becomes 
the new left subtree of $Q2$.  Note that if the new element has 
higher piority than the element at the root of the queue, the new 
element is made the root, and the old root element is inserted into 
the queue.

\index{insertPQ}
\begin{verbatim} */
insertPQ(NV, nil, pq(NV,nil,nil)).
insertPQ(NV, pq(V,LPQ,RPQ), pq(V,RPQ,NLPQ)) :-
   better(V,NV), !,
   insertPQ(NV,LPQ,NLPQ).
insertPQ(NV, pq(V,LPQ,RPQ), pq(NV,RPQ,NLPQ)) :-
   insertPQ(V,LPQ,NLPQ).
/* \end{verbatim}

The deletion operation behaves as follows:  $removePQ(Val,Q1,Q2)$ is
true if removing $Val$ from priority queue $Q1$ results in the
priority queue $Q2$.  Recall that the data element $Val$ always has 
higher priority than any element in its subtrees, and since it is
being deleted, it must be replaced by the element of next highest
priority.  The predicate $remany(Val,Q1,Q2)$ removes a node from the
right subtree (since we always remove from the right to maintain the
invariant) and puts it as the top element in the priority queue.  This
value is pushed down through the queue with $pushPQ(Q1,Q2)$, which
pushes the element down until the heap property is attained.
Finally, we swap subtrees, so that the left subtree is always at most
one element lighter than the right.
\index{removePQ}
\begin{verbatim} */

removePQ(Val, pq(Val,nil,PQ),PQ) :- !.
removePQ(Val, pq(Val,LPQ,RPQ),NPQ) 
  :- remanyPQ(RMV,RPQ,NRPQ),
     pushPQ(pq(RMV,NRPQ,LPQ),NPQ).

/*
\end{verbatim}

The predicate $pushPQ(Q1,Q2)$ is true if $Q2$ is a heap given a
"pseudo--heap" $Q1$.  If $LV$, the element in the left subtree, is of higher
priority than both $RV$ and $Val$, we exchange $LV$ and $Val$, pushing
$Val$ down the left subtree.  Otherwise, if  $Val$ is better than
$RV$, then we exchange these two, pushing $Val$ down the right
subtree.  Note that we are not adding or deleting elements, so we
don't do any subtree swapping.  Finally, if $Val$ is of higher
priority than either $RV$ or $LV$, we are finished.
\index{pushPQ}
\begin{verbatim} */
pushPQ(pq(Val,pq(LV,PQLL,PQLR),pq(RV,PQRL,PQRR)), 
       pq(LV,LPQ,pq(RV,PQRL,PQRR)) ) :-
  better(LV,RV),
  better(LV,Val), !,
  pushPQ(pq(Val,PQLL,PQLR),LPQ).

pushPQ(pq(Val,LPQ, pq(RV,PQRL,PQRR)), pq(RV,LPQ,RPQ))
  :- better(RV,Val), !,
     pushPQ(pq(Val,PQRL,PQRR),RPQ).

pushPQ(PQ,PQ).
/* \end{verbatim}

We remove an element
from a subtree using $remanyPQ(V,LQ,RQ)$.  This predicate
finds the right--most element which either has only a right child, 
or is itself the right leaf of a node with two children.  Swapping 
occurs in the latter case. 
\index{remanyPQ}
\begin{verbatim} */
remanyPQ(Val, pq(Val,nil,PQ),PQ) :- !.
remanyPQ(V, pq(Val,LPQ,RPQ), pq(Val,NLPQ,LPQ)) :-
   remanyPQ(V,RPQ,NLPQ).
/* \end{verbatim}

$better$ is used to order the priority queue. The first is used for the
probabilistic Horn abduction implementation. The second is used for the
heapsort.

\index{better}
\begin{verbatim} */
better(process(_,X,_),process(_,Y,_)) :- !,
   X > Y.
better(X,Y) :- X < Y.
/* \end{verbatim}

$heapsort(L,S)$ is used here to test priority queues, but what the
heck, I might as well leave it in.
\index{heapsort}
\begin{verbatim} */
heapsort(L,S) :-
   insertall(L,nil,PQ),
   remall(PQ,S).
insertall([],P,P).
insertall([H|T],P1,P3) :-
   insertPQ(H,P1,P2),
   insertall(T,P2,P3).
remall(nil,[]).
remall(L1,[H|T]) :-
   removePQ(H,L1,L2),
   remall(L2,T).
/* \end{verbatim}

$emptyPQ(Q)$ is true if queue $Q$ is empty
\index{emptyPQ}
\begin{verbatim} */
emptyPQ(nil).
/* \end{verbatim}

$sizePQ(Q,S)$ is true if queue $Q$ contains $S$ elements.
\index{emptyPQ}
\begin{verbatim} */
sizePQ(nil,0).
sizePQ(pq(_,L,R),S) :-
   sizePQ(L,S1),
   sizePQ(R,S2),
   S is S1 + S2 + 1 .
/* \end{verbatim}

