/* From the book PROLOG PROGRAMMING IN DEPTH
   by Michael A. Covington, Donald Nute, and Andre Vellino.
   Copyright 1988 Scott, Foresman & Co.
   Non-commercial distribution of this file is permitted. */

/* WRITELN.PL */

/* If the argument is a list, display the elements      */
/* one by one and start a new line after each.          */
/* Otherwise display the argument and start a new line. */

writeln([]) :- !.

writeln([Head|Tail]) :- !, write(Head),nl,writeln(Tail).

writeln(Arg) :- write(Arg),nl.

