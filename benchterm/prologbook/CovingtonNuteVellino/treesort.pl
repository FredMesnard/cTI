/* From the book PROLOG PROGRAMMING IN DEPTH
   by Michael A. Covington, Donald Nute, and Andre Vellino.
   Copyright 1988 Scott, Foresman & Co.
   Non-commercial distribution of this file is permitted. */

/* TREESORT.PL */
/* Sorting a list by converting it into
   a binary tree, then back into a list */

treesort(List,NewList) :-
                  list_to_tree(List,Tree),
                  tree_to_list(Tree,NewList).

/*
 * insert(Element,Tree,NewTree)
 *   inserts Element into Tree giving NewTree.
 */

insert(NewItem,empty,tree(NewItem,empty,empty)) :- !.

insert(NewItem,tree(Element,Left,Right),tree(Element,NewLeft,Right)) :-
     NewItem < Element,
     !,
     insert(NewItem,Left,NewLeft).

insert(NewItem,tree(Element,Left,Right),tree(Element,Left,NewRight)) :-
     insert(NewItem,Right,NewRight).

/*
 * insert_list(List,Tree,NewTree)
 *   inserts all elements of List into Tree giving NewTree.
 */

insert_list([Head|Tail],Tree,NewTree) :-
     !,
     insert(Head,Tree,MiddleTree),
     insert_list(Tail,MiddleTree,NewTree).

insert_list([],Tree,Tree).

/*
 * list_to_tree(List,Tree)
 *   inserts all elements of List into an initially empty tree.
 */

list_to_tree(List,Tree) :- insert_list(List,empty,Tree).

/*
 * tree_to_list(Tree,List)
 *   places all elements of Tree into List in sorted order.
 */

tree_to_list(Tree,List) :-
     tree_to_list_aux(Tree,[],List).

tree_to_list_aux(empty,List,List).

tree_to_list_aux(tree(Item,Left,Right),OldList,NewList) :-
     tree_to_list_aux(Right,OldList,List1),
     tree_to_list_aux(Left,[Item|List1],NewList).

