/* From the book PROLOG PROGRAMMING IN DEPTH
   by Michael A. Covington, Donald Nute, and Andre Vellino.
   Copyright 1988 Scott, Foresman & Co.
   Non-commercial distribution of this file is permitted. */
/* Modified for Quintus Prolog by Andreas Siebert */

/* MAZE.PL */

solve_maze :- path([start],Solution), write(Solution).

path([OldSpot|RestOfPath],Solution) :-
     connected_to(NewSpot,OldSpot),
     \+ member(NewSpot,RestOfPath),
     check_path([NewSpot,OldSpot|RestOfPath],Solution).

check_path([finish|Rest],[finish|Rest]).
check_path(Path,Solution):- path(Path,Solution).

connected_to(A,B) :- connect(A,B).
connected_to(A,B) :- connect(B,A).

member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

/*************************************************
 * Connectivity table for the maze in Figure 8.2 *
 *************************************************/

connect(start,2).        connect(1,7).
connect(2,8).            connect(3,4).
connect(3,9).            connect(4,10).
connect(5,11).           connect(5,6).
connect(7,13).           connect(8,9).
connect(10,16).          connect(11,17).
connect(12,18).          connect(13,14).
connect(14,15).          connect(14,20).
connect(15,21).          connect(16,22).
connect(17,23).          connect(18,24).
connect(19,25).          connect(20,26).
connect(21,22).          connect(23,29).
connect(24,30).          connect(25,31).
connect(26,27).          connect(27,28).
connect(28,29).          connect(28,34).
connect(30,36).          connect(31,32).
connect(32,33).          connect(33,34).
connect(34,35).          connect(35,36).
connect(32,finish).

