/* From the book PROLOG PROGRAMMING IN DEPTH
   by Michael A. Covington, Donald Nute, and Andre Vellino.
   Copyright 1988 Scott, Foresman & Co.
   Non-commercial distribution of this file is permitted. */

/* FAMILY.PL */
/* Part of a family tree expressed in Prolog */

/* With "father," "mother," and "parent,"
   the first argument is the parent and
   the second is the son or daughter */

father(michael,cathy).
father(charles_gordon,michael).
father(charles_gordon,julie).
father(charles,charles_gordon).
father(jim,melody).
father(jim,crystal).
father(elmo,jim).
father(greg,stephanie).

mother(melody,cathy).
mother(hazel,michael).
mother(hazel,julie).
mother(eleanor,melody).
mother(eleanor,crystal).
mother(crystal,stephanie).

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).


