
	father(terach,abraham).			male(terach).
	father(terach,nachor).			male(abraham).
	father(terach,haran).			male(nachor).
	father(abraham,isaac).			male(haran).
	father(haran,lot).			male(isaac).
	father(haran,milcah).			male(lot).
	father(haran,yiscah).

						female(sarah).
	mother(sarah,isaac).			female(milcah).
						female(yiscah).

%	Program 1.1: A biblical family database

	father(abraham,isaac).			male(isaac).
	father(haran,lot).			male(lot).
	father(haran,milcah).			female(milcah).
	father(haran,yiscah).			female(yiscah).

	son(X,Y) :- father(Y,X), male(X).
	daughter(X,Y) :- father(Y,X), female(X).

%	Program 1.2: Biblical family relationships

/*
   plus(X,Y,Z) :- The sum of the numbers X and Y is Z.
*/
   plus(X,Y,Z) :- nonvar(X), nonvar(Y), Z is X + Y.
   plus(X,Y,Z) :- nonvar(X), nonvar(Z), Y is Z - X.
   plus(X,Y,Z) :- nonvar(Y), nonvar(Z), X is Z - Y.

%   Program 10.1   Multiple uses for plus
/*
   length(Xs,N) :- The list Xs has length N.
*/
   length(Xs,N) :- nonvar(Xs), length1(Xs,N).
   length(Xs,N) :- var(Xs), nonvar(N), length2(Xs,N).

/*
   length1(Xs,N) :- N is the length of the list Xs.
*/
     length1([X|Xs],N) :- length1(Xs,N1), N is N1+1.
     length1([],0).

/*
   length2(Xs,N) :- Xs is a list of length N.
*/
     length2([X|Xs],N) :- N > 0, N1 is N-1, length2(Xs,N1).
     length2([],0).

%  Program 10.2    A multipurpose length program

/*
   grandparent(X,Z) :- X is the grandparent of Z.
*/
   grandparent(X,Z) :- nonvar(X), parent(X,Y), parent(Y,Z).
   grandparent(X,Z) :- nonvar(Z), parent(Y,Z), parent(X,Y).

%  Program 10.3  A more efficient version of grandparent
/* 
     ground(Term) :- Term is a ground term.
*/
     ground(Term) :- 
        nonvar(Term),
	constant(Term).
     ground(Term) :- 
        nonvar(Term),
	compound(Term),
	functor(Term,F,N),
	ground(N,Term).
	
     ground(N,Term) :-
        N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground(N1,Term).
     ground(0,Term).

%  Program 10.4   Testing if a term is ground
/*
   unify(Term1,Term2) :- 
	Term1 and Term2 are unified, ignoring the occurs check.
*/
   unify(X,Y) :-
	var(X), var(Y), X=Y.
   unify(X,Y) :-
	var(X), nonvar(Y), X=Y.
   unify(X,Y) :-
	var(Y), nonvar(X), Y=X.
   unify(X,Y) :-
	nonvar(X), nonvar(Y), constant(X), constant(Y), X=Y.
   unify(X,Y) :-
	nonvar(X), nonvar(Y), compound(X), compound(Y), term_unify(X,Y).

   term_unify(X,Y) :-
	functor(X,F,N), functor(Y,F,N), unify_args(N,X,Y).

   unify_args(N,X,Y) :-
	N > 0, unify_arg(N,X,Y), N1 is N-1, unify_args(N1,X,Y).
   unify_args(0,X,Y).

   unify_arg(N,X,Y) :-
	arg(N,X,ArgX), arg(N,Y,ArgY), unify(ArgX,ArgY).


%  Program 10.5    Unification algorithm
/*
   unify(Term1,Term2) :- Term1 and Term2 are unified with the occurs check.
*/
   unify(X,Y) :-
	var(X), var(Y), X=Y.
   unify(X,Y) :-
	var(X), nonvar(Y), not_occurs_in(X,Y), X=Y.
   unify(X,Y) :-
	var(Y), nonvar(X), not_occurs_in(Y,X), Y=X.
   unify(X,Y) :-
	nonvar(X), nonvar(Y), constant(X), constant(Y), X=Y.
   unify(X,Y) :-
	nonvar(X), nonvar(Y), compound(X), compound(Y), term_unify(X,Y).

   not_occurs_in(X,Y) :-
	var(Y), X \== Y.
   not_occurs_in(X,Y) :- 
	nonvar(Y), constant(Y).
   not_occurs_in(X,Y) :-
	nonvar(Y), compound(Y), functor(Y,F,N), not_occurs_in(N,X,Y).

   not_occurs_in(N,X,Y) :-
	N > 0, arg(N,Y,Arg), not_occurs_in(X,Arg), N1 is N-1, 
			not_occurs_in(N1,X,Y).
   not_occurs_in(0,X,Y).

   term_unify(X,Y) :-
	functor(X,F,N), functor(Y,F,N), unify_args(N,X,Y).

   unify_args(N,X,Y) :-
	N > 0, unify_arg(N,X,Y), N1 is N-1, unify_args(N1,X,Y).
   unify_args(0,X,Y).

   unify_arg(N,X,Y) :-
	arg(N,X,ArgX), arg(N,Y,ArgY), unify(ArgX,ArgY).

%  Program 10.6    Unification with the occurs check
/*
   occurs_in(Sub,Term) :- 
	Sub is a subterm of the (possibly non-ground term) Term.
*/
% a:  Using ==

   occurs_in(X,Term) :- subterm(Sub,Term), X == Sub.

% b:  Using freeze    $$$$  freeze is not in Standard Prolog

   occurs_in(X,Term) :- freeze(X,Xf), freeze(Y,Termf), subterm(Xf,Termf).

/*
   subterm(Sub,Term) :- Sub is a subterm of the ground term Term.
*/
     subterm(Term,Term).
     subterm(Sub,Term) :- 
        compound(Term), functor(Term,F,N), subterm(N,Sub,Term).

     subterm(N,Sub,Term) :- 
        arg(N,Term,Arg), subterm(Sub,Arg).
     subterm(N,Sub,Term) :- 
        N > 1, N1 is N-1, subterm(N1,Sub,Term).

%  Program 10.7    Occurs in
/*
   numbervars(Term,N1,N2) :- 
	The variables in Term are numbered from N1 to N2-1.
*/

	numbervars('$VAR'(N),N,N1) :- N1 is N+1.
	numbervars(Term,N1,N2) :-
	   nonvar(Term), functor(Term,Name,N), numbervars(0,N,Term,N1,N2).

	numbervars(N,N,Term,N1,N1).
	numbervars(I,N,Term,N1,N3) :-
		I < N,
		I1 is I+1,
		arg(I1,Term,Arg),
		numbervars(Arg,N1,N2),
		numbervars(I1,N,Term,N2,N3).

%  Program 10.8: Numbering the variables in a term
/*
    X : Y :- X or Y.
*   
*   Note: used ':' instead of ';' as ';' is a system predicate.
*/
	:- op(1100,xfy,[':']).

    X : Y :- X.
    X : Y :- Y.

%   Program 10.9    Logical disjunction
/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from merging
	the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,Y|Zs]) :-
	X =:= Y, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, merge([X|Xs],Ys,Zs).
     merge(Xs,[],Xs).
     merge([],Xs,Xs).

%  Program 11.1    Merging ordered lists
/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from merging
	the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, !, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,Y|Zs]) :-
	X =:= Y, !, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, !, merge([X|Xs],Ys,Zs).
     merge(Xs,[],Xs) :- !.
     merge([],Xs,Xs) :- !.

%  Program 11.2    Merging with cuts

/*
   minimum(X,Y,Min) :- Min is the minimum of the numbers X and Y.
*/
   minimum(X,Y,X) :- X =< Y, !.
   minimum(X,Y,Y) :- X > Y, !.

%  Program 11.3    Minimum with cuts
/*
     polynomial(Term,X) :- Term is a polynomial in X.
*/
	:- op(350, xfx,[^]).

     polynomial(X,X) :- !.
     polynomial(Term,X) :- 
        constant(Term), !.
     polynomial(Term1+Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1-Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1*Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1/Term2,X) :- 
        !, polynomial(Term1,X), constant(Term2).
     polynomial(Term ^ N,X) :- 		%   	$$$$ ^
        !, integer(N), N >= 0, polynomial(Term,X).	

%  Program 11.4   Recognizing polynomials
/*
    sort(Xs,Ys) :- 
	Ys is an ordered permutation of the list of integers Xs.
*/
    sort(Xs,Ys) :-
	append(As,[X,Y|Bs],Xs),
	X > Y,
	!,
	append(As,[Y,X|Bs],Xs1),
	sort(Xs1,Ys).
    sort(Xs,Xs) :-
	ordered(Xs), !.

	ordered([]).
	ordered([X]).
	ordered([X,Y|Ys]) :- X =< Y, ordered([Y|Ys]).

%  Program 11.5   Interchange sort
/*
   not X :- X is not provable.
*/
	:- op(900, fx, [not]).

     not X :- X, !, fail.
     not X.

%  Program 11.6   Negation as failure
/*
   variants(Term1,Term2) :- Term1 and Term2 are variants.
*/
   variants(Term1,Term2) :-
	verify((numbervars(Term1,0,N), 
		numbervars(Term2,0,N),
		Term1=Term2)).

	verify(Goal) :- call(G).

	numbervars('$VAR'(N),N,N1) :-
		N1 is N+1.
	numbervars(Term,N1,N2) :-
		nonvar(Term), functor(Term,Name,N),
		numbervars(0,N,Term,N1,N2).

	numbervars(N,N,Term,N1,N1).
	numbervars(I,N,Term,N1,N3) :-
		I < N,
		I1 is I+1,
		arg(I1,Term,Arg),
		numbervars(Arg,N1,N2),
		numbervars(I1,N,Term,N2,N3).

%  Program 11.7  Testing if terms are variants
/*
   X \= Y :- X and Y are not unifiable.
*/
	:- op(700, xfx, \=).

     X \= X :- !, fail.
     X \= Y.

%  Program 11.8   Implementing \=

/*
   delete(Xs,X,Ys) :- 
	Ys is the result of deleting all occurrences of X from the list Xs.
*/
     delete([X|Xs],X,Ys) :- !, delete(Xs,X,Ys).
     delete([X|Xs],Z,[X|Ys]) :- Y \== X, !, delete(Xs,Z,Ys).
     delete([],X,[]).

%   Program 11.9a   Deleting elements from a list
/*
   delete(Xs,X,Ys) :- 
	Ys is the result of deleting all occurrences of X from the list Xs.
*/
     delete([X|Xs],X,Ys) :- !, delete(Xs,X,Ys).
     delete([X|Xs],Z,[X|Ys]) :- !, delete(Xs,Z,Ys).
     delete([],X,[]).

%   Program 11.9b   Deleting elements from a list
/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from merging
	the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,Y|Zs]) :-
	X =:= Y, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, merge([X|Xs],Ys,Zs).
     merge(Xs,[],Xs).
     merge([],Xs,Xs).

%  Program 11.1    Merging ordered lists
/*
   if_then_else(P,Q,R) :- Either P and Q, or not P and R.
*/
     if_then_else(P,Q,R) :- P, !, Q.
     if_then_else(P,Q,R) :- R.

%   Program 11.10    If-then-else statement
/*
  pension(Person,Pension) :- Pension is the type of pension received by Person.
*/
     pension(X,invalid_pension) :- invalid(X).
     pension(X,old_age_pension) :- over_65(X), paid_up(X).
     pension(X,supplem_benefit) :- over_65(X).

     invalid(mc_tavish).

     over_65(mc_tavish).      over_65(mc_donald).    over_65(mc_duff).

     paid_up(mc_tavish).      paid_up(mc_donald).

%  Program 11.11a   Determining welfare payments
/*
  pension(Person,Pension) :- Pension is the type of pension received by Person.
*/
     pension(X,invalid_pension) :- invalid(X), !.
     pension(X,old_age_pension) :- over_65(X), paid_up(X), !.
     pension(X,supplem_benefit) :- over_65(X), !.
     pension(X,nothing).

     invalid(mc_tavish).

     over_65(mc_tavish).      over_65(mc_donald).    over_65(mc_duff).

     paid_up(mc_tavish).      paid_up(mc_donald).

%  Program 11.11b    Determining welfare payments
/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from merging
	the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, !, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,Y|Zs]) :-
	X =:= Y, !, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, !, merge([X|Xs],Ys,Zs).
     merge(Xs,[],Xs) :- !.
     merge([],Xs,Xs) :- !.

%  Program 11.2    Merging with cuts

/*
   minimum(X,Y,Min) :- Min is the minimum of the numbers X and Y.
*/
   minimum(X,Y,X) :- X =< Y, !.
   minimum(X,Y,Y) :- X > Y, !.

%  Program 11.3    Minimum with cuts
/*
     polynomial(Term,X) :- Term is a polynomial in X.
*/
	:- op(350, xfx,[^]).

     polynomial(X,X) :- !.
     polynomial(Term,X) :- 
        constant(Term), !.
     polynomial(Term1+Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1-Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1*Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1/Term2,X) :- 
        !, polynomial(Term1,X), constant(Term2).
     polynomial(Term ^ N,X) :- 		%   	$$$$ ^
        !, integer(N), N >= 0, polynomial(Term,X).	

%  Program 11.4   Recognizing polynomials
/*
    sort(Xs,Ys) :- 
	Ys is an ordered permutation of the list of integers Xs.
*/
    sort(Xs,Ys) :-
	append(As,[X,Y|Bs],Xs),
	X > Y,
	!,
	append(As,[Y,X|Bs],Xs1),
	sort(Xs1,Ys).
    sort(Xs,Xs) :-
	ordered(Xs), !.

	ordered([]).
	ordered([X]).
	ordered([X,Y|Ys]) :- X =< Y, ordered([Y|Ys]).

%  Program 11.5   Interchange sort
/*
   not X :- X is not provable.
*/
	:- op(900, fx, [not]).

     not X :- X, !, fail.
     not X.

%  Program 11.6   Negation as failure
/*
   variants(Term1,Term2) :- Term1 and Term2 are variants.
*/
   variants(Term1,Term2) :-
	verify((numbervars(Term1,0,N), 
		numbervars(Term2,0,N),
		Term1=Term2)).

	verify(Goal) :- not (not Goal).

	numbervars('$VAR'(N),N,N1) :-
		N1 is N+1.
	numbervars(Term,N1,N2) :-
		nonvar(Term), functor(Term,Name,N),
		numbervars(0,N,Term,N1,N2).

	numbervars(N,N,Term,N1,N1).
	numbervars(I,N,Term,N1,N3) :-
		I < N,
		I1 is I+1,
		arg(I1,Term,Arg),
		numbervars(Arg,N1,N2),
		numbervars(I1,N,Term,N2,N3).

%  Program 11.7  Testing if terms are variants
/*
   X \= Y :- X and Y are not unifiable.
*/
	:- op(700, xfx, \=).

     X \= X :- !, fail.
     X \= Y.

%  Program 11.8   Implementing \=

/*
   delete(Xs,X,Ys) :- 
	Ys is the result of deleting all occurrences of X from the list Xs.
*/
     delete([X|Xs],X,Ys) :- !, delete(Xs,X,Ys).
     delete([X|Xs],Z,[X|Ys]) :- Y \== X, !, delete(Xs,Z,Ys).
     delete([],X,[]).

%   Program 11.9a   Deleting elements from a list
/*
   delete(Xs,X,Ys) :- 
	Ys is the result of deleting all occurrences of X from the list Xs.
*/
     delete([X|Xs],X,Ys) :- !, delete(Xs,X,Ys).
     delete([X|Xs],Z,[X|Ys]) :- !, delete(Xs,Z,Ys).
     delete([],X,[]).

%   Program 11.9b   Deleting elements from a list
/*
     writeln(Xs) :- 
	The list of terms Xs is written on the output stream by side-effect.
*/
     writeln([X|Xs]) :- write(X), writeln(Xs).
     writeln([]) :- nl.

%  Program 12.1   Writing a list of terms
/*
   read_word_list(Words) :- 
	Words is a list of words read from the input stream via side effects.
*/
     read_word_list(Words) :- 
		get_char(FirstChar), read_words(FirstChar,Words).

     read_words(Char,[Word|Words]) :-
		word_char(Char), 
		read_word(Char,Word,NextChar), 
		read_words(NextChar,Words).
     read_words(Char,Words) :-
		fill_char(Char),
		get_char(NextChar),
		read_words(NextChar,Words).
     read_words(Char,[]) :-
		end_of_words_char(Char).

    read_word(Char,Word,NextChar) :- 
		word_chars(Char,Chars,NextChar),
		atom_list(Word,Chars).

     word_chars(Char,[Char|Chars],FinalChar) :- 
	  	word_char(Char), !,
		get_char(NextChar),
		word_chars(NextChar,Chars,FinalChar).
     word_chars(Char,[],Char) :- 
		not word_char(Char).

     word_char(C) :- 97 =< C, C =< 122.      % Lower-case letter
     word_char(C) :- 65 =< C, C =< 90.       % Upper-case letter
     word_char(95).                          % Hyphen
     word_char(C) :- 48 =< C, C =< 57.       % numeric digits as well

     fill_char(32).						 % Blank
     end_of_words_char(46).		   			 % Period

%  Program 12.2   Reading in a list of words
/*
	hanoi(N,A,B,C,Moves) :-
		Moves is the sequence of moves required to move N discs
		from peg A to peg B using peg C as an intermediary
		according to the rules of the Towers of Hanoi puzzle
*/

	:- op(100, xfx, [to]).

	hanoi(1,A,B,C,[A to B]).
	hanoi(N,A,B,C,Moves) :-
		N > 1,
		N1 is N -1,
		lemma(hanoi(N1,A,C,B,Ms1)),
		hanoi(N1,C,B,A,Ms2),
		append(Ms1,[A to B|Ms2],Moves).

	lemma(P):- P, asserta((P :- !)).

	/* Testing */

	test_hanoi(N,Pegs,Moves) :-
		hanoi(N,A,B,C,Moves), Pegs = [A,B,C].

%	Program 12.3: Towers of Hanoi using a memo-function
/*
    echo :- An interactive loop.
*/
    echo :- read(X), echo(X).

    echo(X) :- last_input(X), !.
    echo(X) :- write(X), nl, read(Y), !, echo(Y).

%  Program 12.4    Basic interactive loop
/*
    edit :- A line editor.  
	
Files are represented in the form file(Before,After), where After is a
list of lines after the current cursor position and Before is a list
of lines before the cursor in reverse order.

*   Note: Program has been augmented to accomodate boundary 
*         condition.
*/

     edit :- edit(file([],[])).

     edit(File) :-
		write_prompt, read(Command), edit(File,Command).

     edit(File,exit) :- !.
     edit(File,Command) :-
        apply(Command,File,File1), !, edit(File1).
     edit(File,Command) :-
		writeln([Command,' is not applicable']), !, edit(File).

     apply(up,file([X|Xs],Ys),file(Xs,[X|Ys])).
     apply(up,file([],Ys),file([],Ys)).
     apply(up(N),file(Xs,Ys),file(Xs1,Ys1)) :-
		N > 0, up(N,Xs,Ys,Xs1,Ys1).
     apply(down,file(Xs,[Y|Ys]),file([Y|Xs],Ys)).
     apply(down,file(Xs,[]),file(Xs,[])).
     apply(insert(Line),file(Xs,Ys),file(Xs,[Line|Ys])).
     apply(delete,file(Xs,[Y|Ys]),file(Xs,Ys)).
     apply(delete,file(Xs,[]),file(Xs,[])).
     apply(print,file([X|Xs],Ys),file([X|Xs],Ys)) :-
		write(X), nl.
     apply(print,file([],Ys),file([],Ys)) :-
		write('<<top>>'), nl.
     apply(print(*),file(Xs,Ys),file(Xs,Ys)) :- 
        reverse(Xs,Xs1), write_file(Xs1), write_file(Ys).
 
     up(N,[],Ys,[],Ys).
     up(0,Xs,Ys,Xs,Ys).
     up(N,[X|Xs],Ys,Xs1,Ys1) :- 
		N > 0, N1 is N-1, up(N1,Xs,[X|Ys],Xs1,Ys1).

     write_file([X|Xs]) :- 
		write(X), nl, write_file(Xs).
     write_file([]).

     write_prompt :- write('>>'), nl.

%  Program 12.5: A line editor

     shell :-  shell_prompt, read(Goal), shell(Goal).

     shell(exit) :- !.
     shell(Goal) :-
        ground(Goal), !, shell_solve_ground(Goal), shell. 
     shell(Goal) :- 
        shell_solve(Goal), shell.

     shell_solve(Goal) :-
	Goal, write(Goal), nl, fail.
     shell_solve(Goal) :- 
        write('No (more) solutions'), nl.
     
     shell_solve_ground(Goal) :- 
		Goal, !, write('Yes'), nl.
     shell_solve_ground(Goal) :- 
		write('No'), nl.
          
     shell_prompt :-  write('Next command? ').

%	Program 12.6    An interactive shell

     logg :- shell_flag(logg).

     shell_flag(Flag) :- 
		shell_prompt, shell_read(Goal,Flag), shell(Goal,Flag).

     shell(exit,Flag) :- 
	!, close_logging_file.
     shell(nolog,Flag) :-
	!, shell_flag(nolog).
     shell(logg,Flag) :-
	!, shell_flag(logg).
     shell(Goal,Flag) :-
	ground(Goal), !, shell_solve_ground(Goal,Flag), shell_flag(Flag).
     shell(Goal,Flag) :-
        shell_solve(Goal,Flag), shell_flag(Flag).
     
     shell_solve(Goal,Flag) :- 
        Goal, flag_write(Goal,Flag), nl.
     shell_solve(Goal,Flag) :- 
        flag_write('No (more) solutions',Flag), nl.
     
     shell_solve_ground(Goal,Flag) :- 
		Goal, !, flag_write('Yes',Flag), nl.
     shell_solve_ground(Goal,Flag) :- 
		flag_write('No',Flag), nl.
          
     shell_prompt :- write('Next command? ').

     shell_read(X,logg) :-
		read(X), 
		file_write(['Next command? ',X],'prolog.log').
     shell_read(X,nolog) :- read(X).

     flag_write(X,nolog) :- write(X).
     flag_write(X,logg) :- write(X), file_write(X,'prolog.log').

     file_write(X,File) :- write_term(File,Term,[]).
     close_logging_file :- close('prolog.log').

%	Program 12.7 Logging a session

	echo :- repeat_, read(X), echo(X), !.

	echo(X) :- last_input(X), !.
	echo(X) :- write(X), nl, fail.

	repeat_.
	repeat_ :- repeat_.

%	Program 12.8    Basic interactive repeat loop
/*
    consult_(File) :- 
	The clauses of the program in the file File are read and asserted.
*/

	consult_(File) :- open(File),read(DD), consult_loop(DD), close(DD).

	consult_loop(DD) :- repeat, read(Clause), process(Clause,DD), !.

	process(Clause,DD) :- at_end_of_stream(DD).
	process(Clause,DD) :- assertz(Clause), fail.

%  Program 12.9:    Consulting a file

/*
   union(Xs,Ys,Us) :- Us is the union of the elements in Xs and Ys. 
*/

union([X|Xs],Ys,Us) :- member(X,Ys), union(Xs,Ys,Us).
union([X|Xs],Ys,[X|Us]) :- nonmember(X,Ys), union(Xs,Ys,Us).
union([],Ys,Ys).

%  Program 13.1    Finding the union of two lists
/*
   intersection(Xs,Ys,Is) :- Is is the intersection of the elements in Xs and Ys. 
*/

intersection([X|Xs],Ys,[X|Is]) :- member(X,Ys), intersection(Xs,Ys,Is).
intersection([X|Xs],Ys,Is) :- nonmember(X,Ys), intersection(Xs,Ys,Is).
intersection([],Ys,[]).

%  Program 13.2    Finding the intersection of two lists
/*
   union_intersect(Xs,Ys,Us) :- 
	Us and is are the union and intersection, respectively, of the
		elements in Xs and Ys.  
*/

union_intersect([X|Xs],Ys,Us,[X|Is]) :- 
	member(X,Ys), union_intersect(Xs,Ys,Us,Is).
union_intersect([X|Xs],Ys,[X|Us],Is) :- 
	nonmember(X,Ys), union_intersect(Xs,Ys,Us,Is).
union_intersect([],Ys,Ys,[]).

%  Program 13.3    Finding the union and intersection of two lists
/*
	verb(Sentence,Verb) :-
		Verb is a verb in the list of words Sentence.
*/

	verb(Sentence,Word) :- member(Word,Sentence), verb(Word).
	noun(Sentence,Word) :- member(Word,Sentence), noun(Word).
	article(Sentence,Word) :- member(Word,Sentence), article(Word).

	/* Vocabulary */

	noun(man). 		noun(woman).
	article(a).		verb(loves).

%	Program 14.1: Finding parts of speech in a sentence
/*
   queens(N,Queens) :-
	Queens is a placement that solves the N queens problem,
	represented as a permutation of the list of numbers [1,2,...,N].
*/

     queens(N,Qs) :- range(1,N,Ns), permutation(Ns,Qs), safe(Qs).

/*
	safe(Qs) :- The placement Qs is safe.
*/     
     safe([Q|Qs]) :- safe(Qs), not attack(Q,Qs).
     safe([]).     
     
     attack(X,Xs) :- attack(X,1,Xs).

     attack(X,N,[Y|Ys]) :- X is Y+N ; X is Y-N.
     attack(X,N,[Y|Ys]) :- N1 is N+1, attack(X,N1,Ys).


	permutation(Xs,[Z|Zs]) :- select(Z,Xs,Ys), permutation(Ys,Zs).
	permutation([],[]).

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

     range(M,N,[M|Ns]) :- M < N, M1 is M+1, range(M1,N,Ns).
     range(N,N,[N]).

%	Program 14.2: Naive generate-and-test program solving N queens

/*   queens(N,Queens)  :-
	Queens is a placement that solves the N queens problem,
	represented as a permutation of the list of numbers [1,2,..,N].
*/

     queens(N,Qs) :- range(1,N,Ns), queens(Ns,[],Qs).

     queens(UnplacedQs,SafeQs,Qs) :-
         select(Q,UnplacedQs,UnplacedQs1), 
         not attack(Q,SafeQs), 
         queens(UnplacedQs1,[Q|SafeQs],Qs).
     queens([],Qs,Qs).

	range(I,N,[I|Ns]) :- I < N, I1 is I+1, range(I1,N,Ns).
	range(N,N,[N]).

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

     attack(X,Xs) :- attack(X,1,Xs).

     attack(X,N,[Y|Ys]) :- X is Y+N ; X is Y-N.
     attack(X,N,[Y|Ys]) :- N1 is N+1, attack(X,N1,Ys).

%	Program 14.3: Placing one queen at a time
/*
color_map(Map,Colors) :-
	Map is colored with Colors, so that no two neighbors have the same
	color. The map is represented as an adjacency-list of regions
	region(Name,Color,Neighbors), where Name is the name of the
	region, Color is its color, and Neighbors are the colors of the 
	neighbors. 
*/
	color_map([Region|Regions],Colors) :-
		color_region(Region,Colors),
		color_map(Regions,Colors).
	color_map([],Colors).
/*
   color_region(Region,Colors) :-
	Region and its neighbors are colored using Colors so that the
	region's color is different from the color of any of its neighbors.
*/
	color_region(region(Name,Color,Neighbors),Colors) :-
		select(Color,Colors,Colors1),
		members(Neighbors,Colors1).

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

     members([X|Xs],Ys) :- member(X,Ys), members(Xs,Ys).
     members([],Ys).

%	Program 14.4: Map coloring

	/* Test data */

	test_color(Name,Map) :-
		map(Name,Map),
		colors(Name,Colors),
		color_map(Map,Colors).

	map(test,[region(a,A,[B,C,D]),     region(b,B,[A,C,E]), 
		      region(c,C,[A,B,D,E,F]), region(d,D,[A,C,F]),
			  region(e,E,[B,C,F]),     region(f,F,[C,D,E])]).

	map(west_europe,
		 [	region(portugal,P,[E]),  region(spain,E,[F,P]),
			region(france,F,[E,I,S,B,WG,L]),  region(belgium,B,[F,H,L,WG]),
			region(holland,H,[B,WG]), region(west_germany,WG,[F,A,S,H,B,L]),
			region(luxembourg,L,[F,B,WG]), region(italy,I,[F,A,S]),
			region(switzerland,S,[F,I,A,WG]), region(austria,A,[I,S,WG])]).

	colors(X,[red,yellow,blue,white]).

%	Program 14.5: Test data for map coloring
/*
	solve_puzzle(Puzzle,Solution) :-
		Solution is a solution of Puzzle,
		where Puzzle is puzzle(Clues,Queries,Solution).
*/

	solve_puzzle(puzzle(Clues,Queries,Solution),Solution) :-
		solve(Clues),
		solve(Queries).

	solve([Clue|Clues]) :-
		Clue, solve(Clues).
	solve([]).

%	Program 14.6: A puzzle solver
	/* Test data */

	test_puzzle(Name,Solution) :-
		structure(Name,Structure),
		clues(Name,Structure,Clues),
		queries(Name,Structure,Queries,Solution),
		solve_puzzle(puzzle(Clues,Queries,Solution),Solution).

	structure(test,[friend(N1,C1,S1), friend(N2,C2,S2), friend(N3,C3,S3)]).

	clues(test,Friends,
		[(did_better(Man1Clue1, Man2Clue1, Friends),		% Clue 1
		  name_(Man1Clue1, michael), sport(Man1Clue1,basketball),
		  nationality(Man2Clue1,american)),
		 (did_better(Man1Clue2, Man2Clue2, Friends),		% Clue 2
		  name_(Man1Clue2, simon), nationality(Man1Clue2,israeli),
		  sport(Man2Clue2,tennis)),
		 (first(Friends,ManClue3),sport(ManClue3,cricket))
		]).

	queries(test, Friends,
	    [ member(Q1,Friends),
			name_(Q1,Name),
			nationality(Q1,australian),                     % Query 1
			member(Q2,Friends),
			name_(Q2,richard),
			sport(Q2,Sport)							        % Query 2
		],
		[['The Australian is', Name], ['Richard plays ', Sport]]
	).

	did_better(A,B,[A,B,C]).
	did_better(A,C,[A,B,C]).
	did_better(B,C,[A,B,C]).

	name_(friend(A,B,C),A).
	nationality(friend(A,B,C),B).
	sport(friend(A,B,C),C).

	first([X|Xs],X).

%	Program 14.7: A description of a puzzle
/*  path(X,Y,Path) :-
		Path is a path between two nodes X and Y in the
		DAG defined by the relation edge/2
*/
     path(X,X,[X]).
     path(X,Y,[X|P]) :- edge(X,N), path(N,Y,P).

%	Program 14.9: Finding a path by depth-first search
/*
   connected(X,Y) :-
	Node X is connected to node Y in the graph defined by edge/2.
*/	

     connected(X,Y) :- connected(X,Y,[X]).

     connected(X,X,Visited).
     connected(X,Y,Visited) :- 
		edge(X,N), not member(N,Visited), connected(N,Y,[N|Visited]).

%	Program 14.10: Connectivity in a graph
/*
    transform(State1,State2,Plan) :-
	Plan is a plan of actions to transform State1 into State2.
*/

transform(State1,State2,Plan) :- 
   transform(State1,State2,[State1],Plan).

transform(State,State,Visited,[]).
transform(State1,State2,Visited,[Action|Actions]) :-
   legal_action(Action,State1),
   update(Action,State1,State), 
   not member(State,Visited),
   transform(State,State2,[State|Visited],Actions).

legal_action(to_place(Block,Y,Place),State) :- 
   on(Block,Y,State), clear(Block,State), place(Place), clear(Place,State).
legal_action(to_block(Block1,Y,Block2),State) :- 
   on(Block1,Y,State), clear(Block1,State), block(Block2), 
	Block1 \== Block2, clear(Block2,State).

clear(X,State) :- not member(on(A,X),State).
on(X,Y,State) :- member(on(X,Y),State).

update(to_block(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).
update(to_place(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).

substitute(X,Y,[X|Xs],[Y|Xs]).
substitute(X,Y,[X1|Xs],[X1|Ys]) :- X \== X1, substitute(X,Y,Xs,Ys).

%	Program 14.11: A depth-first planner
  /*  Testing and data  */

test_plan(Name,Plan) :-
   initial_state(Name,I), final_state(Name,F), transform(I,F,Plan).

initial_state(test,[on(a,b),on(b,p),on(c,r)]).
final_state(test,[on(a,b),on(b,c),on(c,r)]).

block(a).	block(b).	block(c).
place(p).	place(q).	place(r).

%	Program 14.12: Testing the depth-first planner
/*
   analogy(Pair1,Pair2,Answers) :-
	An analogy holds between the pairs of figures Pair1 and Pair2.
	The second element of Pair2 is one of the possible Answers.
*/
:- op(100,xfx,[is_to]).

analogy(A is_to B,C is_to X,Answers) :-
    match(A,B,Match),
    match(C,X,Match),
    member(X,Answers).

match(inside(Figure1,Figure2),inside(Figure2,Figure1),invert).
match(above(Figure1,Figure2),above(Figure2,Figure1),invert).

%	Program 14.13: A program solving geometric analogies
   /*  Testing and data		*/

:- op(40,xfx,[is_to]).

test_analogy(Name,X) :-
    figures(Name,A,B,C),
    answers(Name,Answers),
    analogy(A is_to B,C is_to X,Answers).

figures(test1,inside(square,triangle),inside(triangle,square),
					inside(circle,square)).
answers(test1,[inside(circle,triangle),inside(square,circle),
					inside(triangle,square)]).

%	Program 14.14: Testing ANALOGY 
/*
    eliza :- Simulates a conversation via side effects.
*/

%  For testing this program prior to Standard Prolog being widley
%  available, the program below uses read rather than read_word_list.
%  You need to type in a list of waords such as [i, am,unhappy].

eliza :- read(Input), eliza(Input), !.

eliza([bye]) :-
   writeln(['Goodbye. I hope I have helped you']).
eliza(Input) :-
   pattern(Stimulus,Response),
   match(Stimulus,Table,Input),
   match(Response,Table,Output),
   reply(Output),
   read(Input1),
   !, eliza(Input1).

/*
	match(Patterm,Dictionary,Words) :-
		Pattern matches the list of words Words, and matchings
		are recorded in the Dictionary.
*/

match([N|Pattern],Table,Target) :-
   integer(N),
   lookup(N,Table,LeftTarget),
   append(LeftTarget,RightTarget,Target),
   match(Pattern,Table,RightTarget).
match([Word|Pattern],Table,[Word|Target]) :-
   atom(Word),
   match(Pattern,Table,Target).
match([],Table,[]).

/*
    pattern(Stimulus,Response) :-
	Response is an applicable response pattern to the pattern Stimulus.
*/

pattern([i,am,1],['How',long,have,you,been,1,?]).
pattern([1,you,2,me],['What',makes,you,think,'I',2,you,?]).
pattern([i,like,1],['Does',anyone,else,in,your,family,like,1,?]).
pattern([i,feel,1],['Do',you,often,feel,that,way,?]).
pattern([1,X,2],['Please',you,tell,me,more,about,X]) :-
	important(X).
pattern([1],['Please',go,on,'.']).

    important(father).			important(mother).
    important(sister).			important(brother).
    important(son).			important(daughter).

reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.

lookup(X,[(X,V)|XVs],V).
lookup(X,[(X1,V1)|XVs],V) :- X \== X1, lookup(X,XVs,V).


%	Program 14.15 ELIZA
/*
	mcsam(Story,Script) :-
		Script describes Story.
*/

mcsam(Story,Script) :-  
	find(Story,Script,Defaults),
        match(Script,Story),
        name_defaults(Defaults).

find(Story,Script,Defaults) :- 
	filler(Slot,Story),
        trigger(Slot,Name),
        script(Name,Script,Defaults).
/*
	match(Script,Story) :-
		Story is a subsequence of Script.
*/

match(Script,[]).
match([Line|Script],[Line|Story]) :- match(Script,Story).
match([Line|Script],Story) :- match(Script,Story).

/*
	filler(Slot,Story) :-
		Slot is a word in Story.
*/

filler(Slot,Story) :- 
	member([Action|Args],Story),
        member(Slot,Args),
        nonvar(Slot).

/*
	name_defaults(Defaults) :-
		Unifies default pairs in Defaults.
*/

name_defaults([]).
name_defaults([[N,N]|L]) :-  name_defaults(L).
name_defaults([[N1,N2]|L]) :-  N1 \== N2, name_defaults(L).

%	Program 14.16: McSAM
%	Testing and Data

test_mcsam(Name,UnderstoodStory) :-
	story(Name,Story), mcsam(Story,UnderstoodStory).

story(test,[[ptrans, john, john, X1, leones],
       [ingest, X2, hamburger, X3],
       [ptrans, Actor, Actor, X4, X5] ]).

script(restaurant,
       [ [ptrans, Actor, Actor, Earlier_place, Restaurant],
         [ptrans, Actor, Actor, Door, Seat],
         [mtrans, Actor, Waiter, Food],
         [ingest, Actor, Food, [mouth, Actor] ],
         [atrans, Actor, Money, Actor, Waiter],
         [ptrans, Actor, Actor, Restaurant, Gone] ],
       [ [Actor, customer], [Earlier_place, place1],
         [Restaurant, restaurant], [Door, door],
         [Seat, seat], [Food, meal], [Waiter, waiter],
         [Money, check], [Gone, place2] ] ).

trigger(leones, restaurant).  trigger(waiter, restaurant).

%	Program 14.17:   Testing McSAM
/*	
   append_dl(As,Bs,Cs) :-
	The difference-list Cs is the result of appending Bs to As,
		where As and Bs are compatible difference-lists.
*/
	:- op(40,xfx,\).

	append_dl(Xs\Ys,Ys\Zs,Xs\Zs).

%	Program 15.1:  Concatenating difference_lists
/*	
   flatten(Xs,Ys) :-
	Ys is a flattened list containing the elements in Xs.
*/
	:- op(40,xfx,\).

	flatten(Xs,Ys) :- flatten_dl(Xs,Ys\[]).
	
	flatten_dl([X|Xs],Ys\Zs) :-
	   flatten_dl(X,Ys\Ys1), flatten_dl(Xs,Ys1\Zs).
	flatten_dl(X,[X|Xs]\Xs) :-
	   constant(X), X \== [].
	flatten_dl([],Xs\Xs).

%	Program 15.2 : Flattening a list of lists using difference-lists
/*	
   reverse(Xs,Ys) :- Ys is the reversal of the list Xs.
*/
	:- op(40,xfx,\).

	reverse(Xs,Ys) :- reverse_dl(Xs,Ys\[]).
	
	reverse_dl([X|Xs],Ys\Zs) :-
		reverse_dl(Xs,Ys\[X|Zs]).
	reverse_dl([],Xs\Xs).


%	Program 15.3: Reverse with difference-lists.
/*	
    quicksort(List,Sortedlist) :-
	Sortedlist is an ordered permutation of list.

*/
	:- op(40,xfx,\).

	quicksort(Xs,Ys) :- quicksort_dl(Xs,Ys\[]).

	quicksort_dl([X|Xs],Ys\Zs)  :-
		partition(Xs,X,Littles,Bigs),
		quicksort_dl(Littles,Ys\[X|Ys1]),
		quicksort_dl(Bigs,Ys1\Zs).
	quicksort_dl([],Xs\Xs).

	partition([X|Xs],Y,[X|Ls],Bs) :-
	   X =< Y, !, partition(Xs,Y,Ls,Bs).
	partition([X|Xs],Y,Ls,[X|Bs]) :-
	   X > Y, !, partition(Xs,Y,Ls,Bs).
	partition([],Y,[],[]).

%	Program 15.4: Quicksort using difference-lists.
/*	
    dutch(Xs,RedsWhitesBlues) :-
	RedsWhitesBlues is a list of elements of Xs ordered
	by color: red, then white, then blue.
*/

	dutch(Xs,RedsWhitesBlues) :-
		distribute(Xs,Reds,Whites,Blues),
		append(Whites,Blues,WhitesBlues),
		append(Reds,WhitesBlues,RedsWhitesBlues).

/*	
    distribute(Xs,Reds,Whites,Blues) :-
	Reds, Whites, and Blues are the lists of red, white,
	and blue elements in Xs, respectively.
*/

	distribute([red(X)|Xs],[red(X)|Reds],Whites,Blues) :-
		distribute(Xs,Reds,Whites,Blues).
	distribute([white(X)|Xs],Reds,[white(X)|Whites],Blues) :-
		distribute(Xs,Reds,Whites,Blues).
	distribute([blue(X)|Xs],Reds,Whites,[blue(X)|Blues]) :-
		distribute(Xs,Reds,Whites,Blues).
	distribute([],[],[],[]).

%	Program 15.5: A solution to the Dutch flag problem

/*  
   dutch(Xs,RedsWhitesBlues) :-
	RedsWhitesBlues is a list of elements of Xs ordered 
		by color: red, then white, then blue.
*/
	:- op(40,xfx,\).

	dutch(Xs,RedsWhitesBlues) :-
		distribute_dls(Xs,RedsWhitesBlues\WhitesBlues,
			WhitesBlues\Blues,Blues\[]).

/*	
    distribute_dls(Xs,Reds,Whites,Blues) :-
	Reds,Whites,Blues are difference-lists of red, white,
		and blue elements in Xs, respectively.
*/

	distribute_dls([red(X)|Xs],[red(X)|Reds]\Reds1,Whites,Blues) :-
		distribute_dls(Xs,Reds\Reds1,Whites,Blues).
	distribute_dls([white(X)|Xs],Reds,[white(X)|Whites]\Whites1,Blues) :-
		distribute_dls(Xs,Reds,Whites\Whites1,Blues).
	distribute_dls([blue(X)|Xs],Reds,Whites,[blue(X)|Blues]\Blues1) :-
		distribute_dls(Xs,Reds,Whites,Blues\Blues1).
	distribute_dls([],Reds\Reds,Whites\Whites,Blues\Blues).

%	Program 15.6: Dutch Flag with difference-lists.
/*	
    normalize(Sum,NormalisedSum) :-
	NormalizedSum is the result of normalizing the sum expression Sum.

*/
	:- op(50,xfx,++).

	normalize(Exp,Norm) :- normalize_ds(Exp,Norm++0).

	normalize_ds(A+B,Norm++Space) :-
	   normalize_ds(A,Norm++NormB), normalize_ds(B,NormB++Space).
	normalize_ds(A,(A+Space)++Space) :-
	   constant(A).

%	Program 15.7 :	Normalizing plus expressions.
/*	
    lookup(Key,Dictionary,Value) :-
	Dictionary contains Value indexed under key.
	Dictionary is represented as an incomplete 
	list of pairs of the form (Key,Value).
*/

	lookup(Key,[(Key,Value)|Dict],Value).
	lookup(Key,[(Key1,Value1)|Dict],Value) :-
	   Key \== Key1, lookup(Key,Dict,Value).

%	Program 15.8: Dictionary lookup from a list of tuples
/*	
    lookup(Key,Dictionary,Value) :-
	Dictionary contains the value indexed under Key.
	Dictionary is represented as an ordered binary tree.

*/

	lookup(Key,dict(Key,X,Left,Right),Value) :-
		!, X = Value.
	lookup(Key,dict(Key1,X,Left,Right),Value) :-
		Key < Key1 , lookup(Key,Left,Value).
	lookup(Key,dict(Key1,X,Left,Right),Value) :-
		Key > Key1, lookup(Key,Right,Value).

%	Program 15.9: Dictionary lookup in a binary tree
/*
    freeze(A,B) :- Freeze term A into B.
*/

freeze(A,B) :- 
   copy_term(A,B), numbervars(B,0,N).

/*	
    melt_new(A,B) :- Melt the frozen term A into B.

*/
melt_new(A,B) :-
   melt(A,B,Dictionary), !.

melt('$VAR'(N),X,Dictionary) :-
	lookup(N,Dictionary,X).
melt(X,X,Dictionary) :-
	constant(X).
melt(X,Y,Dictionary) :-
	compound(X),
	functor(X,F,N),
	functor(Y,F,N),
	melt(N,X,Y,Dictionary).

melt(N,X,Y,Dictionary) :-
	N > 0, 
	arg(N,X,ArgX), 
	melt(ArgX,ArgY,Dictionary),
	arg(N,Y,ArgY), 
	N1 is N-1, 
	melt(N1,X,Y,Dictionary).
melt(0,X,Y,Dictionary).

	numbervars('$VAR'(N),N,N1) :-
	   N1 is N + 1.
	numbervars(Term,N1,N2) :-
	   nonvar(Term), functor(Term,Name,N), 	numbervars(0,N,Term,N1,N2).

	numbervars(N,N,Term,N1,N1).
	numbervars(I,N,Term,N1,N3) :-
	   I < N, I1 is I + 1, arg(I1,Term,Arg),
	   numbervars(Arg,N1,N2), numbervars(I1,N,Term,N2,N3).

	lookup(Key,dict(Key,X,Left,Right),Value) :-
		!, X = Value.
	lookup(Key,dict(Key,1,Left,Right),Value) :-
		Key < Key1 , !, lookup(Key,Left,Value).
	lookup(Key,dict(Key1,Left,Right),Value) :-
		Key > Key1, !, lookup(Key,Right,Value).

%   Program 15.10:   Melting a term
/*	
    queue(S) :-
	S is a sequence of enqueue and dequeue operations,
	represented as a list of terms enqueue(X) and dequeue(X).
*/
	:- op(40,xfx,\).

	queue(S) :- queue(S,Q\Q).

	queue([enqueue(X)|Xs],Q) :-
	   enqueue(X,Q,Q1), queue(Xs,Q1).
	queue([dequeue(X)|Xs],Q) :-
	   dequeue(X,Q,Q1), queue(Xs,Q1).
	queue([],Q).

	enqueue(X,Qh\[X|Qt],Qh\Qt).
	dequeue(X,[X|Qh]\Qt,Qh\Qt).

%	Program 15.11:	A queue process
/*	
    flatten(Xs,Ys) :-
	Ys is a flattened list containing the elements in Xs.
*/
	:- op(40,xfx,\).

	flatten(Xs,Ys) :- flatten_q(Xs,Qs\Qs,Ys).

	flatten_q([X|Xs],Ps\[Xs|Qs],Ys) :-
		flatten_q(X,Ps\Qs,Ys).
	flatten_q(X,[Q|Ps]\Qs,[X|Ys]) :-
		constant(X), X \= [], flatten_q(Q,Ps\Qs,Ys).
	flatten_q([],Q,Ys) :-
		non_empty(Q), dequeue(X,Q,Q1), flatten_q(X,Q1,Ys).
	flatten_q([],[]\[],[]).

non_empty([]\[]) :- !, fail.
non_empty(Q).

dequeue(X,[X|Qh]\Qt,Qh\Qt).

%	Program 15.12:	Flattening a list using a queue


	father(terach,abraham).		father(haran,lot).			
	father(terach,nachor).		father(haran,milcah).			
	father(terach,haran).		father(haran,yiscah).	
	father(abraham,isaac).			
	male(abraham).		male(haran).	female(yiscah).	
	male(isaac).		male(nachor).	female(milcah).
	male(lot).

%	Program 16.1:   Sample data
/*
	for_all(Goal,Condition) :-
		For all solutions of Goal, Condition is true

*/

	for_all(Goal,Condition) :-
		findall(Condition,Goal,Cases), check(Cases).

	check([Case|Cases]) :- Case, check(Cases).
	check([]).

%	Program 16.2 : Applying set predicates
/*	
   find_all_dl(X,Goal,Instances) :- Instances is the multiset
	of instances of X for which Goal is true. The multiplicity
	of an element is the number of different ways Goal can be
	proved with it as an instance of X.
*/
	:- op(40,xfx,\).

	find_all_dl(X,Goal,Xs) :-
		asserta('$instance'('$mark')),
		Goal,
		asserta('$instance'(X)),
		fail.
	find_all_dl(X,Goal,Xs\Ys) :-
		retract('$instance'(X)),
		reap(X,Xs\Ys), !.
	
	reap(X,Xs\Ys) :-	
		X \== '$mark',
		retract('$instance'(X1)), ! ,
		reap(X1,Xs\[X|Ys]).
	reap('$mark',Xs\Xs).

%	Program 16.3 : Implementing an all-solutions predicate using
%			difference-lists, assert and retract

/*	
    connected(X,Y) :-
	Node X is connected to node Y in the DAG defined
		by edge/2 facts.
*/
	:- op(40,xfx,\).

	connected(X,Y) :- enqueue(X,Q\Q,Q1), connected_bfs(Q1,Y).

	connected_bfs(Q,Y) :- empty(Q), !, fail.
	connected_bfs(Q,Y) :- dequeue(X,Q,Q1), X=Y.
	connected_bfs(Q,Y) :-
	   dequeue(X,Q,Q1), enqueue_edges(X,Q1,Q2), connected_bfs(Q2,Y).

	enqueue(X,Qh\[X|Qt],Qh\Qt).
	dequeue(X,[X|Qh]\Qt,Qh\Qt).
	empty([]\[]).

enqueue_edges(X,Xs\Ys,Xs\Zs) :- find_all_dl(N,edge(X,N),Ys\Zs), !.

%  findall_dl/3  :-  see Program 16.3

%	Data

     edge(a,b).   edge(a,c).   edge(a,d).  edge(a,e).  edge(d,j).  
     edge(c,f).   edge(c,g).   edge(f,h).  edge(e,k).  edge(f,i).  
     edge(x,y).   edge(y,z).   edge(z,x).  edge(y,u).  edge(z,v).  

%	Program 16.4:  Testing connectivity breadth-first in a DAG
/* 
    connected(X,Y) :-
	Node X is connected to node Y in the graph defined by edge/2 facts.
*/
	:- op(40,xfx,\).

	connected(X,Y) :- enqueue(X,Q\Q,Q1), connected_bfs(Q1,Y,[X]).

	connected_bfs(Q,Y,Visited) :- empty(Q), !, fail.
	connected_bfs(Q,Y,Visited) :- dequeue(X,Q,Q1), X=Y.
	connected_bfs(Q,Y,Visited) :- 
	   dequeue(X,Q,Q1), 
	   findall(N,edge(X,N),Edges),
	   filter(Edges,Visited,Visited1,Q1,Q2),
	   connected_bfs(Q2,Y,Visited1).

	filter([N|Ns],Visited,Visited1,Q,Q1) :-
	   member(N,Visited), !, filter(Ns,Visited,Visited1,Q,Q1).
	filter([N|Ns],Visited,Visited1,Q,Q2) :-
	   not member(N,Visited), !, 
	   enqueue(N,Q,Q1), 
	   filter(Ns,[N|Visited],Visited1,Q1,Q2).
	filter([],Visited,Visited,Q,Q).

	enqueue(X,Qh\[X|Qt],Qh\Qt).
	dequeue(X,[X|Qh]\Qt,Qh\Qt).
	empty([]\[]).

%	Data

     edge(a,b).   edge(a,c).   edge(a,d).  edge(a,e).  edge(d,j).  
     edge(c,f).   edge(c,g).   edge(f,h).  edge(e,k).  edge(f,i).  
     edge(x,y).   edge(y,z).   edge(z,x).  edge(y,u).  edge(z,v).  

%	Program 16.5: Testing connectivity breadth-first in a graph
/*	
    lee_route(Source,Destination,Obstacles,Path) :-
	Path is a minimal length path from Source to
	Destination which does not cross Obstacles.
*/

     lee_route(A,B,Obstacles,Path) :- 
	waves(B,[[A],[]],Obstacles,Waves),
	path(A,B,Waves,Path).

/*	
    waves(Destination,Wavessofar,Obstacles,Waves) :-
	Waves is a list of waves including Wavessofar
	(except,perhaps,its last wave) that leads to Destination
	without crossing Obstacles.
*/
     
     waves(B,[Wave|Waves],Obstacles,Waves) :- member(B,Wave), !.
     waves(B,[Wave,LastWave|LastWaves],Obstacles,Waves) :- 
         next_wave(Wave,LastWave,Obstacles,NextWave), 
	 waves(B,[NextWave,Wave,LastWave|LastWaves],Obstacles,Waves).

/*	
    next_waves(Wave,LastWave,Obstacles,NextWave) :-
	Nextwave is the set of admissible points from Wave,
	that is excluding points from Lastwave, Wave, 
	and points under Obstacles.
*/
     
     next_wave(Wave,LastWave,Obstacles,NextWave) :-  
         findall(X,admissible(X,Wave,LastWave,Obstacles),NextWave).

     admissible(X,Wave,LastWave,Obstacles) :-  
         adjacent(X,Wave,Obstacles), 
	 not member(X,LastWave), 
	 not member(X,Wave).

     adjacent(X,Wave,Obstacles) :- 
	 member(X1,Wave), 
	 neighbor(X1,X), 
	 not obstructed(X,Obstacles).
     
     neighbor(X1-Y,X2-Y) :- next_to(X1,X2).
     neighbor(X-Y1,X-Y2) :- next_to(Y1,Y2).
     
     next_to(X,X1) :- X1 is X+1.
     next_to(X,X1) :- X > 0, X1 is X-1.

     obstructed(Point,Obstacles) :-
	member(Obstacle,Obstacles), obstructs(Point,Obstacle).
   
     obstructs(X-Y,obstacle(X-Y1,X2-Y2)) :- Y1 =< Y, Y =< Y2.
     obstructs(X-Y,obstacle(X1-Y1,X-Y2)) :- Y1 =< Y, Y =< Y2.
     obstructs(X-Y,obstacle(X1-Y,X2-Y2)) :- X1 =< X, X =< X2.
     obstructs(X-Y,obstacle(X1-Y1,X2-Y)) :- X1 =< X, X =< X2.
     
/*
   path(Source,Destination,Waves,Path) :-
	Path is a path from Source to destination going through Waves.
*/

     path(A,A,Waves,[A]) :-  !.
     path(A,B,[Wave|Waves],[B|Path]) :- 
         member(B1,Wave), neighbor(B,B1), !, path(A,B1,Waves,Path).

%	Testing and Data

	test_lee(Name,Path) :-
	   data(Name,A,B,Obstacles), lee_route(A,B,Obstacles,Path).

data(test1,1-1,3-3,[]).
data(test2,1-1,5-5,[obstacle(2-3,4-5)]).
data(test,1-1,5-5,[obstacle(2-3,4-5),obstacle(6-6,8-8)]).

%	Program 16.6 	Lee routing 
/*
   kwic(Titles,KWTitles) :-
	KWTitles is a KWIC index of the list of titles Titles.
*/
     kwic(Titles,KWTitles) :-
        setof(Ys,Xs^(member(Xs,Titles),rotate_and_filter(Xs,Ys)),KWTitles).

/*
   rotate_and_filter(Xs,Ys) :-
	Ys is a rotation of the list Xs such that
	the first word of Ys is significant and |
	is inserted after the last word of Xs.
*/

     rotate_and_filter(Xs,Ys) :-
        append(As,[Key|Bs],Xs), 
        not insignificant(Key), 
	append([Key|Bs],['|'|As],Ys).

%	Vocabulary

     insignificant(a).        insignificant(the).
     insignificant(in).       insignificant(for).

%	Testing and data

test_kwic(Books,Kwic) :-
	titles(Books,Titles), kwic(Titles,Kwic).

titles(lp,[[logic,for,problem,solving],
	   [logic,programming],
	   [algorithmic,program,debugging],
	   [programming,in,prolog]]).

%  Program 16.7   Producing a keyword in context (KWIC) index
/*	
    has_property(Xs,P) :-
	Each element in the list Xs has property P.
*/

	has_property([X|Xs],P) :-
	   apply(P,X), has_property(Xs,P).
	has_property([],P).

	apply(male,X) :- male(X).

/*	
   map_list(Xs,P,Ys) :-
	Each element in the list Xs stands in relation
	P to its corresponding element in the list Ys.
*/

	map_list([X|Xs],P,[Y|Ys]) :-
	   apply(P,X,Y), map_list(Xs,P,Ys).
	map_list([],P,[]).

	apply(dict,X,Y) :- dict(X,Y).

%	Program 16.8:  Second-order predicates in Prolog
/*
	accept(Xs) :-
		The string represented by the list Xs is accepted by
		the NDFA defined by initial/1, delta/3, and final/1.
*/
	accept(Xs) :- initial(Q), accept(Xs,Q).

	accept([X|Xs],Q) :- delta(Q,X,Q1), accept(Xs,Q1).
	accept([],Q) :- final(Q).

%   Program 17.1: An interpreter for a nondeterministic finite automaton (NDFA)
	initial(q0).
	final(q0).

	delta(q0,a,q1).
	delta(q1,b,q0).

%	Program 17.2: An NDFA that accepts the language (ab)*
/*
	accept(Xs) :-
		The string represented by the list Xs is accepted by
		the NPDA defined by initial/1, delta/5, and final/1.
*/

	accept(Xs) :- initial(Q), accept(Xs,Q,[]).

	accept([X|Xs],Q,S) :- delta(Q,X,S,Q1,S1), accept(Xs,Q1,S1).
	accept([],Q,[]) :- final(Q).

%  Program 17.3: An interpreter for a nondeterministic pushdown automaton (NPDA)
	initial(q0).			final(q1).

	delta(q0,X,S,q0,[X|S]).
	delta(q0,X,S,q1,[X|S]).
	delta(q0,X,S,q1,S).
	delta(q1,X,[X|S],q1,S).

%	Program 17.4:An NPDA for palindromes over a finite alphabet
/*
  solve(Goal) :-
     Goal is true given the pure Prolog program defined by clause/2.
*/

     solve(true).
     solve((A,B)) :- solve(A), solve(B).
     solve(A) :- clause(A,B), solve(B).

%	Program 17.5	A meta-interpreter for pure Prolog
/*
  solve(Goal) :-
     Goal is true given the pure Prolog program defined by clause/2.
*/

  solve(Goal) :- solve(Goal,[]).

     solve([],[]).
     solve([],[G|Goals]) :- solve(G,Goals).
     solve([A|B],Goals) :- append(B,Goals,Goals1),solve(A,Goals1).
     solve(A,Goals) :- rule(A,B), solve(B,Goals).

%  Program 17.6  A meta-interpreter for pure Prolog in continuation style
/*  
  solve_trace(Goal) :-
	Goal is true given the pure Prolog program defined by 
	clause/2. The program traces the proof by side effects.
*/
		 
solve_trace(Goal) :-
	solve_trace(Goal,0).

     solve_trace(true,Depth) :- !.
     solve_trace((A,B),Depth) :-  !,
        solve_trace(A,Depth), solve_trace(B,Depth). 
     solve_trace(A,Depth) :-  
	builtin(A), !, A, display(A,Depth), nl.
     solve_trace(A,Depth) :-
        clause(A,B), 
	display(A,Depth), nl,
	Depth1 is Depth + 1, 
	solve_trace(B,Depth1).

     display(A,Depth) :-  Spacing is 3*Depth, put_spaces(Spacing), write(A). 

     	put_spaces(N) :- between(1,N,I), put_char(' '), fail.
	put_spaces(N).

     between(I,J,I) :- I =< J.
     between(I,J,K) :- I < J, I1 is I + 1, between(I1,J,K).

%	Program 17.7	A tracer for Prolog
/*  
   solve(Goal,Tree) :-		       
	Tree is a proof tree for Goal given the program 
	defined by clause/2.
*/
    solve(true,true) :- !.
    solve((A,B),(ProofA,ProofB)) :- !,
       solve(A,ProofA), solve(B,ProofB).
    solve(A,(A:-builtin)) :- builtin(A), !, A.
    solve(A,(A:-Proof)) :-
       clause(A,B), solve(B,Proof).

% Program 17.8	A meta-interpreter for building a proof tree
/*
  solve(Goal,Certainty) :-
     Certainty is our confidence that Goal is true. 
*/
     solve(true,1) :- !.
     solve((A,B),C) :- !,
	solve(A,C1), solve(B,C2), minimum(C1,C2,C).
     solve(A,1) :- builtin(A), !, A.
     solve(A,C) :-
	clause_cf(A,B,C1), solve(B,C2), C is C1 * C2.

   minimum(X,Y,X) :- X =< Y, !.
   minimum(X,Y,Y) :- X > Y, !.

%  Program 17.9   A meta-interpreter for reasoning with uncertainty
/*
  solve(Goal,Certainty,Threshold) :-
     Certainty is our confidence, greater than threshold, that Goal is true. 
*/
     solve(true,1,T) :- !.
     solve((A,B),C,T) :- !,
	solve(A,C1,T), solve(B,C2,T), minimum(C1,C2,C).
     solve(A,1,T) :- builtin(A), !, A.
     solve(A,C,T) :-
	clause_cf(A,B,C1), C1 > T, T1 is T/C1,
	solve(B,C2,T1), C is C1 * C2.

   minimum(X,Y,X) :- X =< Y, !.
   minimum(X,Y,Y) :- X > Y, !.

%  Program 17.10   Reasoning with uncertainty with threshold cutoff
/*
   solve(A,D,Overflow) :-
	A has a proof tree of depth less than D and Overflow equals 
	no_overflow,or A has a branch in the computation tree longer
	than D, and Overflow contains a list of its first D elements.
*/
	solve(true,D,no_overflow) :- !.
	solve(A,0,overflow([])).
	solve((A,B),D,Overflow) :- !,
		D > 0,
		solve(A,D,OverflowA),
		solve_conjunction(OverflowA,B,D,Overflow).
	solve(A,D,Overflow) :-
		D > 0,
		clause(A,B),
		D1 is D - 1,
		solve(B,D1,OverflowB),
		return_overflow(OverflowB,A,Overflow).
	solve(A,D,no_overflow) :-
		D > 0,
		system(A), A.

	solve_conjunction(overflow(S),B,D,overflow(S)).
	solve_conjunction(no_overflow,B,D,Overflow) :-
		solve(B,D,Overflow).

	return_overflow(no_overflow,A,no_overflow).
	return_overflow(overflow(S),A,overflow([A|S])).

% Program 17.11 :A meta-interpreter detecting a stack overflow
/*	
   isort(Xs,Ys) :-
	Ys is an ordered permutation of Xs. Nontermination program.
*/

	isort([X|Xs],Ys) :-
	   isort(Xs,Zs), insert(X,Zs,Ys).
	isort([],[]).

	insert(X,[Y|Ys],[X,Y|Ys]) :- X < Y.
	insert(X,[Y|Ys],[Y|Zs]) :- X >= Y, insert(Y,[X|Ys],Zs).
	insert(X,[],[X]).

% Program 17.12   A nonterminating insertion sort
/*	
    isort(Xs,Ys) :- Buggy insertion sort.
*/
	isort([X|Xs],Ys) :-
	   isort(Xs,Zs), insert(X,Zs,Ys).
	isort([],[]).

	insert(X,[Y|Ys],[X,Y|Ys]) :- X >= Y.
	insert(X,[Y|Ys],[Y|Zs]) :- X > Y, insert(X,Ys,Zs).
	insert(X,[],[X]).

%  Program 17.13   An incorrect and incomplete insertion sort
/* 
    false_solution(A,Clause) :-
	If A is a provable false instance, then Clause is 
	a false clause in the program. Bottom up algorithm.
*/
	false_solution(A,Clause) :-
	   solve(A,Proof),
	   false_clause(Proof,Clause).

    solve(true,true) :- !.
    solve((A,B),(ProofA,ProofB)) :- !,
       solve(A,ProofA), solve(B,ProofB).
    solve(A,(A:-builtin)) :- builtin(A), !, A.
    solve(A,(A:-Proof)) :-
       clause(A,B), solve(B,Proof).

	false_clause(true,ok).
	false_clause(builtin,ok).
	false_clause((A,B),Clause) :-
		false_clause(A,ClauseA),
		check_conjunction(ClauseA,B,Clause).
	false_clause((A :- B),Clause) :-
		false_clause(B,ClauseB),
		check_clause(ClauseB,A,B,Clause).

	check_conjunction(ok,B,Clause) :-
		false_clause(B,Clause).
	check_conjunction((A :- B1),B,(A :- B1)).

	check_clause(ok,A,B,Clause) :-
		query_goal(A,Answer),
		check_answer(Answer,A,B,Clause).
	check_clause((A1 :- B1),A,B,(A1 :- B1)).

	check_answer(true,A,B,ok).
	check_answer(false,A,B,(A :- B1)) :-
		extract_body(B,B1).

	extract_body(true,true).
	extract_body((A :- B),A).
	extract_body(((A :- B),Bs),(A,As)) :-
		extract_body(Bs,As).

	query_goal(A,true) :- builtin(A).
	query_goal(Goal,Answer) :- 
		not builtin(Goal),
		writeln(['Is the Goal',Goal,'true?']),
		read(Answer).

     writeln([T|Ts]) :- write(T), write(' '), writeln(Ts).
     writeln([]) :- nl.

%  Program 17.14   Bottom-up diagnosis of a false solution
/* 
   false_solution(A,Clause) :-
	If A is a provable false instance, then Clause 
	is a false clause in the program. Top down algorithm.
*/
	false_solution(A,Clause) :-
		solve(A,Proof),
		false_goal(Proof,Clause).

    solve(true,true) :- !.
    solve((A,B),(ProofA,ProofB)) :- !,
       solve(A,ProofA), solve(B,ProofB).
    solve(A,(A:-builtin)) :- builtin(A), !, A.
    solve(A,(A:-Proof)) :-
       clause(A,B), solve(B,Proof).

	false_goal((A :- B),Clause) :-
		false_conjunction(B,Clause), !.
	false_goal((A :- B),(A :- B1)) :-
		extract_body(B,B1).

	false_conjunction(((A :- B),Bs),Clause) :-
		query_goal(A,false), !,
		false_goal((A :- B),Clause).
	false_conjunction((A :- B),Clause) :-
		query_goal(A,false), !,
		false_goal((A :- B),Clause).
	false_conjunction((A,As),Clause) :-
		false_conjunction(As,Clause).

	extract_body(true,true).
	extract_body((A :- B),A).
	extract_body(((A :- B),Bs),(A,As)) :-
		extract_body(Bs,As).

	query_goal(A,true) :- builtin(A).
	query_goal(Goal,Answer) :- 
		not builtin(Goal),
		writeln(['Is the Goal',Goal,'true?']),
		read(Answer).

     writeln([T|Ts]) :- write(T), write(' '), writeln(Ts).
     writeln([]) :- nl.

%	Program 17.15: Top-down diagnosis of a false solution
/* 	
   missing_solution(A,Goal) :-
	If A is a non-provable true ground goal,then Goal is a
	true ground goal which is uncovered by the program.

*/

	missing_solution((A,B),Goal) :- !,
		( not(A),missing_solution(A,Goal);
		call(A), missing_solution(B,Goal)).
	missing_solution(A,Goal) :-
		clause(A,B),
		query_clause((A :- B)), !,
		missing_solution(B,Goal).
	missing_solution(A,A) :-
		not system(A).

	query_clause(Clause) :-
		writeln(['Enter a true ground instance of ']),
		read(Answer),
		!, check_answer(Answer,Clause).

	check_answer(no,Clause) :- !, fail.
	check_answer(Clause,Clause) :- !.
	check_answer(Answer,Clause) :-
		write('Illegal Answer'),
		!, query_clause(Clause).

     writeln([T|Ts]) :- write(T), write(' '), writeln(Ts).
     writeln([]) :- nl.

%  Program 17.16: Diagnosing missing solution
/* 

Rule base for a simple expert system for placing dishes in an oven.
The predicates used in the rules are
    place_in_oven(Dish,Rack)   :-
    	Dish should be placed in the oven at level Rack for baking.
    pastry(Dish) :-  Dish is a pastry.
    main_meal(Dish) :-  Dish is a main meal.
    slow_cooker(Dish) :-  Dish is a slow cooker.
    type(Dish,Type) :-  Dish is best described as Type.
    size(Dish,Size) :-  The size of Dish is Size.

The rules have the form rule(Head,Body,Name).
*/


:- op(40,xfy,&).
:- op(30,xf,is_true).


rule(place_in_oven(Dish,top),
    pastry(Dish) is_true & size(Dish,small) is_true,place1).
rule(place_in_oven(Dish,middle),
    pastry(Dish) is_true & size(Dish,big) is_true,place2).
rule(place_in_oven(Dish,middle),main_meal(Dish) is_true,place3).
rule(place_in_oven(Dish,bottom),slow_cooker(Dish) is_true,place4).

rule(pastry(Dish),type(Dish,cake) is_true,pastry1).
rule(pastry(Dish),type(Dish,bread) is_true,pastry2).

rule(main_meal(Dish),type(Dish,meat) is_true,main_meal).

rule(slow_cooker(Dish),type(Dish,milk_pudding) is_true,slow_cooker).

fact(type(dish1,bread)).
fact(size(dish1,big)).

%  Program 17.17: Oven placement rule-based system
/*
       monitor(Goal) :-
           Succeeds if a result of yes is returned from solving Goal
          at the solve level, or when the end of the computation is reached.
*/
       monitor(Goal) :-  solve(Goal,Result), filter(Result).
       monitor(Goal).

       filter(yes).
       % filter(no)   fail.

/*
       solve(Goal,Result) :-
           Given a set of rules of the form rule(A,B,Name), Goal has
           Result yes if it follows from the rules and no if it does not.
*/
       solve(A,yes) :-  fact(A).
       solve(A,Result) :-  rule(A,B,Name), solve_body(B,Result).
       solve(A,no).

%% fred
       solve_body(A&B,Result) :-
           solve(A,ResultA), solve_and(ResultA,B,Result).
       %solve_body(A is_true,Result)   solve(A,Result).

       solve_and(no,A,no).
       solve_and(yes,B,Result).   solve(B,Result).

%   Program 17.18: A skeleton two-level rule interpreter
/*
    solve(Goal,Result) :-
       Given a set of rules of the form rule(A,B,Name), Goal has
       Result yes if it follows from the rules and no if it does not.
       The user is prompted for missing information.
*/
    solve(A,yes) :-  fact(A).
    solve(A,Result) :-  rule(A,B,Name), solve_body(B,Result).
    solve(A,Result) :-  askable(A), solve_askable(A,Result).
    solve(A,no).

    solve_body(A&B,Result) :-
       solve_body(A,ResultA), solve_and(ResultA,B,Result).
    solve_body(A is_true,Result) :-  solve(A,Result).

    solve_and(no,A,no).
    solve_and(yes,B,Result) :-  solve(B,Result).

    solve_askable(A,Result) :-
       not known(A), ask(A,Response), respond(Response,A,Result).

%    The following predicates facilitate interaction with the user.

    ask(A,Response) :-  display_query(A), read(Response).

    respond(yes,A,yes) :-  assert(known_to_be_true(A)).
    respond(no,A,no) :-  assert(known_to_be_false(A)).

    known(A) :-  known_to_be_true(A).
    known(A) :-  known_to_be_false(A).

    display_query(A):-write(A), write('?  ').

%    Program 17.19: An interactive rule interpreter
/*
       monitor(Goal) :-
           Succeeds if a result of yes is returned from solving Goal
           at the solve level, or when the end of the computation is reached.
*/
       monitor(Goal) :-  solve(Goal,Result,[ ]), filter(Result).
       monitor(Goal).

       filter(yes).
       % filter(no) :-  fail.

/*
       solve(Goal,Result,Rules) :-
           Given a set of rules of the form rule(A,B,Name), Goal has
           Result yes if it follows from the rules and no if it does not.
           Rules is the current list of rules that have been used.
*/
       solve(A,yes,Rules) :-  fact(A).
       solve(A,Result,Rules) :-
           rule(A,B,Name), RulesB = [NamejRules],
           solve_body(B,Result,RulesB).
       solve(A,no,Rules).

       solve_body(A&B,Result,Rules) :-
           solve_body(A,ResultA,Rules),
           solve_and(ResultA,B,Result,Rules).
       solve_body(A is_true,Result,Rules) :-   solve(A,Result,Rules).

       solve_and(no,A,no,Rules).
       solve_and(yes,B,Result,Rules) :-  solve(B,Result,Rules).

%       Program 17.20: A two-level rule interpreter carrying rules
/*
    monitor(Goal,Proof) :-
       Succeeds if a result of yes is returned from solving Goal at the
       solve level, in which case Proof is a proof tree representing the
       successful computation, or when the end of the computation is reached,
       in which case Proof is a list of failure branches since the last success.
*/
    monitor(Goal,Proof) :-
       set_search_tree, solve(Goal,Result,Proof),
       filter(Result,Proof).
    monitor(Goal,Proof) :-
       collect_proof(P), reverse(P,[ ],P1),
       Proof = failed(Goal,P1).

    filter(yes,Proof) :-  reset_search_tree.
    filter(no,Proof) :-  store_proof(Proof), fail.

/*
    solve(Goal,Result,Proof)  :-
       Given a set of rules of the form rule(A,B,Name), Goal has
       Result yes if it follows from the rules and no if it does not.
       Proof  is a proof tree if the result is yes and a failure branch
       of the search tree if the result is no.
*/
    :- op(40,xfy,because).
    :- op(30,xfy,with).

    solve(A,yes,Tree) :-  fact(A), Tree = fact(A).
    solve(A,Result,Tree) :-
       rule(A,B,Name), solve_body(B,Result,Proof),
       Tree = A because B with Proof.
    solve(A,no,Tree) :-
       not fact(A), not rule(A,B,Name), Tree = no_match(A).

    solve_body(A&B,Result,Proof) :-
       solve_body(A,ResultA,ProofA),
       solve_and(ResultA,B,Result,ProofB),
       Proof = ProofA & ProofB.
    solve_body(A is_true,Result,Proof) :-  solve(A,Result,Proof).

    solve_and(no,A,no,unsearched).
    solve_and(yes,B,Result,Tree) :-  solve(B,Result,Tree).

%  The following predicates use side effects to record and remove 
%  branches of the search tree.

    collect_proof(Proof) :-  retract('search tree'(Proof)).

    store_proof(Proof) :- 
       retract('search tree'(Tree)),
       assert('search tree'([ProofjTree])).

    set_search_tree :-  assert('search tree'([ ])).

    reset_search_tree :- 
       retract('search tree'(Proof)),
       assert('search tree'([ ])).

	reverse([],[]).
	reverse([X|Xs],Zs) :- reverse(Xs,Ys), append(Ys,[X],Zs).

%    Program 17.21: A two-level rule interpreter with proof trees
/*
    explain(Goal)  :-
       Explains how the goal Goal was proved.
*/
    explain(Goal) :-  monitor(Goal,Proof), interpret(Proof).

%    monitor(Goal,Proof)   See Program 17.21.

    interpret(ProofA&ProofB) :-
       interpret(ProofA), interpret(ProofB).
    interpret(failed(A,Branches)) :-
       nl, writeln([A,' has failed with the following failure branches:']),
       interpret(Branches).
    interpret([Fail|Fails]) :-
       interpret(Fail), nl, write('NEW BRANCH'), nl,
       interpret(Fails).
    interpret([ ]).
    interpret(fact(A)) :-
       nl, writeln([A,' is a fact in the database.']).
    interpret(A because B with Proof) :-
       nl, writeln([A,' is proved using the rule']),
       display_rule(rule(A,B)), interpret(Proof).
    interpret(no_match(A)) :-
       nl, writeln([A,' has no matching fact or rule in the rule base.']).
    interpret(unsearched) :-
       nl, writeln(['The rest of the conjunct is unsearched.']).

    display_rule(rule(A,B)) :-
       write('IF '), write_conjunction(B), writeln(['THEN ',A ]).

    write_conjunction(A&B) :-
       write_conjunction(A), write(' AND '),
       write_conjunction(B).

    write_conjunction(A is_true) :-  write(A).

     writeln([X|Xs]) :- write(X), writeln(Xs).
     writeln([]) :- nl.

%    Program 17.22: Explaining a proof
/*
  monitor(Goal,Proof) :-
    Succeeds if a result of yes is returned from solving Goal at the
    solve level, in which case Proof  is a proof tree representing the
    successful computation, or when the end of the computation is reached,
    in which case Proof  is a list of failure branches since the last success.
*/

monitor(Goal,Proof) :-
    set_search_tree, solve(Goal,Result,[ ],Proof),
    filter(Result,Proof).
monitor(Goal,Proof) :-
    collect_proof(P), reverse(P,[ ],P1),
    Proof = failed(Goal,P1).

filter(yes,Proof) :-  reset_search_tree.
filter(no,Proof) :-  store_proof(Proof), fail.

/*
solve(Goal,Result,Rules,Proof) :-
    Given a set of rules of the form rule(A,B,Name), Goal has
    Result yes if it follows from the rules and no if it does not.
    Rules is the current list of rules that have been used.
    Proof  is a proof tree if the result is yes and a failure branch
    of the search tree if the result is no.
*/

:- op(40,xfy,because).
:- op(30,xfy,with).

solve(A,yes,Rules,Tree) :-  fact(A), Tree = fact(A).
solve(A,Result,Rules,Tree) :-
    rule(A,B,Name), RulesB = [NamejRules],
    solve_body(B,Result,RulesB,Proof),
    Tree = A because B with Proof.
solve(A,Result,Rules,Tree) :-
    askable(A), solve_askable(A,Result,Rules), Tree = user(A).
solve(A,no,Rules,Tree) :-
    not fact(A), not rule(A,B,Name), Tree = no_match(A).

solve_body(A&B,Result,Rules,Proof) :-
    solve_body(A,ResultA,Rules,ProofA),
    solve_and(ResultA,B,Result,Rules,ProofB),
    Proof = ProofA & ProofB.
solve_body(A is_true,Result,Rules,Proof) :-
    solve(A,Result,Rules,Proof).

solve_and(no,A,no,Rules,unsearched).
solve_and(yes,B,Result,Rules,Tree) :-  solve(B,Result,Rules,Tree).

%  The following predicates use side effects to record and remove 
%  branches of the search tree.

    collect_proof(Proof) :-  retract('search tree'(Proof)).

    store_proof(Proof) :- 
       retract('search tree'(Tree)),
       assert('search tree'([ProofjTree])).

    set_search_tree :-  assert('search tree'([ ])).

    reset_search_tree :- 
       retract('search tree'(Proof)),
       assert('search tree'([ ])).

	reverse([],[]).
	reverse([X|Xs],Zs) :- reverse(Xs,Ys), append(Ys,[X],Zs).

%  The following predicates facilitate interaction with the user.

ask(A,Response) :-  display_query(A), read(Response).

respond(yes,A,yes) :-  assert(known_to_be_true(A)).
respond(no,A,no) :-  assert(known_to_be_false(A)).
respond(why,A,[RulejRules]) :-
    display_rule(Rule), ask(A,Answer), respond(Answer,A,Rules).
respond(why,A,[ ]) :-
    writeln(['No more explanation possible']), ask(A,Answer),
    respond(Answer,A,[ ]).

known(A) :-  known_to_be_true(A).
known(A) :-  known_to_be_false(A).

display_query(A) :-  write(A), write('?  ').

    display_rule(rule(A,B)) :-
       write('IF '), write_conjunction(B), writeln(['THEN ',A ]).

    write_conjunction(A&B) :-
       write_conjunction(A), write(' AND '),
       write_conjunction(B).

    write_conjunction(A is_true) :-  write(A).


     writeln([X|Xs]) :- write(X), writeln(Xs).
     writeln([]) :- nl.

%  Program 17.23: An explanation shell


/*
	palindrome(Xs) :-
		The string represented by the list Xs is a palindrome.
*/

	palindrome(Xs) :- palindrome(Xs,push,[]).

	palindrome([X|Xs],push,S) :- palindrome(Xs,push,[X|S]).
	palindrome([X|Xs],push,S) :- palindrome(Xs,pop,[X|S]).
	palindrome([X|Xs],push,S) :- palindrome(Xs,pop,S).
	palindrome([X|Xs],pop,[X|S]) :- palindrome(Xs,pop,S).
	palindrome([],pop,[]).

%  Program 18.1 : A program accepting palindromes
/*
        preduce(Goal,Residue) :-
           Partially reduce Goal to leave the residue Residue.
*/
        preduce(true,true) :-  !.
        preduce((A,B),(PA,PB)) :-  !, preduce(A,PA), preduce(B,PB).
        preduce(A,B) :-  should_fold(A,B), !.
        preduce(A,Residue) :-
           should_unfold(A), !, clause(A,B), preduce(B,Residue).
        preduce(A,A).

%     Program 18.2: A meta-interpreter for determining a residue
/*
        process(Program, RedProgram) :-
           Partially reduce each of the clauses in Program to produce
           RedProgram.
*/
        process(Prog,NewProg) :-
           findall(PCl,(member(Cl,Prog),preduce(Cl,PCl)),NewProg).

        test(Name,Program) :-
           program(Name,Clauses), process(Clauses,Program).

/*
        preduce(Goal,Residue) :-
           Partially reduce Goal to leave the residue Residue.
*/
        preduce((A :-  B),(PA :-  PB))  :-
           !, preduce(B,PB), preduce(A,PA).
        preduce(true,true) :-  !.
        preduce((A,B),Res) :-
           !, preduce(A,PA), preduce(B,PB), combine(PA,PB,Res).
        preduce(A,B) :-  should_fold(A,B), !.
        preduce(A,Residue) :-
           should_unfold(A), !, clause(A,B), preduce(B,Residue).
        preduce(A,A).

        combine(true,B,B) :-  !.
        combine(A,true,A) :-  !.
        combine(A,B,(A,B)).

%    Program 18.3: A simple partial reduction system
    program(npda,[(accept(Xs1) :-  initial(Q1), accept(Xs1,Q1,[ ])),
        (accept([X2jXs2],Q2,S2) :-  delta(Q2,X2,S2,Q12,S12),
        accept(Xs2,Q12,S12)), (accept([ ],Q3,[ ]) :-  true)]).

    should_unfold(initial(Q)).
    should_unfold(final(Q)).
    should_unfold(delta(A,B,C,D,E)).

    should_fold(accept(Q,Xs,Q1),palindrome(Q,Xs,Q1)).
    should_fold(accept(Xs),palindrome(Xs)).

%    Program 18.4: Specializing an NPDA
/*
        Rule interpreter for counting reductions
*/
        solve(A,1) :-  fact(A).
        solve(A,N) :-  rule(A,B,Name), solve_body(B,NB), N is NB+1.

        solve_body(A&B,N) :-
           solve_body(A,NA), solve_body(B,NB), N is NA+NB.
        solve_body(A is_true,N) :-  solve(A,N).

%        Sample rule base

        rule(oven(Dish,top),pastry(Dish) is_true
           & size(Dish,small) is_true,place1).
        rule(oven(Dish,middle),pastry(Dish) is_true
           & size(Dish,big) is_true,place2).
        rule(oven(Dish,middle),main_meal(Dish) is_true,place3).
        rule(oven(Dish,bottom),slow_cooker(Dish) is_true,place4).
        rule(pastry(Dish),type(Dish,cake) is_true,pastry1).
        rule(pastry(Dish),type(Dish,bread) is_true,pastry2).
        rule(main_meal(Dish),type(Dish,meat) is_true,main_meal).
        rule(slow_cooker(Dish),type(Dish,milk_pudding)
           is_true,slow_cooker).

        should_fold(solve(oven(D,P),N),oven(D,P,N)).
        should_fold(solve(pastry(D),N),pastry(D,N)).
        should_fold(solve(main_meal(D),N),main_meal(D,N)).
        should_fold(solve(slow_cooker(D),N),slow_cooker(D,N)).
        should_fold(solve(type(D,P),N),type(D,P,N)).
        should_fold(solve(size(D,P),N),size(D,P,N)).

        should_unfold(solve_body(G,N)).
        should_unfold(rule(A,B,Name)).

        program(rule_interpreter,[(solve(A1,1) :-  fact(A1)),
           (solve(A2,N) :-  rule(A2,B,Name), solve_body(B,NB), N is NB+1)]).

%        Program 18.5: Specializing a rule interpreter
/*
  compose(Program1,Program2,Skeleton,FinalProgram) :-
     FinalProgram is the result of composing Program1 and
     Program2, which are both enhancements of Skeleton.
*/

compose([Cl1|Cls1],[Cl2|Cls2],[ClSkel|ClsSkel],[Cl|Cls]) :-
    compose_clause(Cl1,Cl2,ClSkel,Cl),
    compose(Cls1,Cls2,ClsSkel,Cls).
compose([ ],[ ],[ ],[ ]).

compose_clause((A1 :- B1),(A2 :- B2),(ASkel :- BSkel),(A :- B)) :-
    composition_specification(A1,A2,ASkel,A),
    compose_bodies(BSkel,B1,B2,B\true).

compose_bodies(SkelBody,Body1,Body2,B\BRest) :-
    first(SkelBody,G), !,
    align(G,Body1,G1,RestBody1,B\B1),
    align(G,Body2,G2,RestBody2,B1\(Goal,B2)),
    compose_goal(G1,G2,Goal),
    rest(SkelBody,Gs),
    compose_bodies(Gs,RestBody1,RestBody2,B2\BRest).
compose_bodies(true,Body1,Body2,B\BRest) :-
    rest_goals(Body1,B\B1), rest_goals(Body2,B1\BRest).

align(Goal,Body,G,RestBody,B\B) :-
    first(Body,G), correspond(G,Goal), !, rest(Body,RestBody).
align(Goal,(G,Body),CorrespondingG,RestBody,(G,B)\B1) :-
    align(Goal,Body,CorrespondingG,RestBody,B\B1).

first((G,Gs),G).                    
first(G,G) :- G \== (A,B), G \== true. 

rest((G,Gs),Gs).
rest(G,true) :-  G \== (A,B).

correspond(G,G).
correspond(G,B) :-  map(G,B).

compose_goal(G,G,G) :-  !.
compose_goal(A1,A2,A) :-
    !, composition_specification(A1,A2,ASkel,A).

rest_goals(true,B\B) :-  !.
rest_goals(Body,(G,B)\BRest) :-
    first(Body,G), !, rest(Body,Body1), rest_goals(Body1,B\BRest).

%  Program 18.6: Composing two enhancements of a skeleton
    test_compose(X,Prog) :-
        program1(X,Prog1), program2(X,Prog2),
        skeleton(X,Skeleton), compose(Prog1,Prog2,Skeleton,Prog).

    program1(test,[
        (union([X1jXs1],Ys1,Zs1) :-
           member(X1,Ys1), union(Xs1,Ys1,Zs1)),
        (union([X2|Xs2],Ys2,[X2|Zs2]) :-
           nonmember(X2,Ys2), union(Xs2,Ys2,Zs2)),
        (union([ ],Ys3,Ys3) :-  true)]).

    program2(test,[
        (common([X1|Xs1],Ys1,N1) :-
           member(X1,Ys1), common(Xs1,Ys1,M1), N1 is M1+1),
        (common([X2|Xs2],Ys2,N2) :-
           nonmember(X2,Ys2), common(Xs2,Ys2,N2)),
        (common([ ],Ys3,0) :-  true)]).

    skeleton(test,[
        (skel([X1|Xs1],Ys1) :-  member(X1,Ys1), skel(Xs1,Ys1)),
        (skel([X2|Xs2],Ys2) :-  nonmember(X2,Ys2), skel(Xs2,Ys2)),
        (skel([ ],Ys3) :-  true)]).

    composition_specification(union(Xs,Ys,Us), common(Xs,Ys,N),
        skel(Xs,Ys),uc(Xs,Ys,Us,N)).

    map(union(Xs,Ys,Zs), skel(Xs,Ys)).
    map(common(Xs,Ys,N), skel(Xs,Ys)).

%    Program 18.7: Testing program composition
        s(As\Xs) :-  a(As\Bs), b(Bs\Cs), c(Cs\Xs).

        a(Xs\Ys) :-  connect([a],Xs\Xs1), a(Xs1\Ys).
        a(Xs\Ys) :-  connect([ ],Xs\Ys).

        b(Xs\Ys) :-  connect([b],Xs\Xs1), b(Xs1\Ys).
        b(Xs\Ys) :-  connect([ ],Xs\Ys).

        c(Xs\Ys) :-  connect([c],Xs\Xs1), c(Xs1\Ys).
        c(Xs\Ys) :-  connect([ ],Xs\Ys).

        connect([ ],Xs\Xs).
        connect([W|Ws],[W|Xs]\Ys) :-  connect(Ws,Xs\Ys).

%     Program 18.8: A Prolog program parsing the language a*b*c
/*	
    translate(Grammar,Program) :-
	Program is the Prolog equivalent of the context-free
	grammar Grammar.
*/
        translate([Rule|Rules],[Clause|Clauses]) :-
           translate_rule(Rule,Clause),
           translate(Rules,Clauses).
        translate([ ],[ ]).

/*
        translate_rule(GrammarRule,PrologClause) :-
           PrologClause is the Prolog equivalent of the grammar
           rule GrammarRule.
*/
	translate_rule((Lhs --> Rhs),(Head :- Body)) :-
	   translate_head(Lhs,Head,Xs\Ys),
	   translate_body(Rhs,Body,Xs\Ys),!.

        translate_head(A,A1,Xs) :-
           translate_goal(A,A1,Xs).

	translate_body((A,B),(A1,B1),Xs\Ys) :-
	   !, translate_body(A,A1,Xs\Xs1), translate_body(B,B1,Xs1\Ys).
        translate_body(A,A1,Xs) :-
           translate_goal(A,A1,Xs).

        translate_goal(A,A1,DList) :-
           nonterminal(A), functor(A1,A,1), arg(1,A1,DList).
        translate_goal(Terms,connect(Terms,S),S) :-
           terminals(Terms).

	non_terminal(A) :- atom(A).

	terminals(Xs) :- list(Xs).

	list([]).
	list([X|Xs]) :- list(Xs).

%     Program 18.9: Translating grammar rules to Prolog clauses
        s(N) --> a(NA), b(NB), c(NC), {N is NA+NB+NC}.

        a(N) --> [a], a(N1), {N is N1+1}.
        a(0) --> [ ].

        b(N) --> [b], b(N1), {N is N1+1}.
        b(0) --> [ ].

        c(N) --> [c], c(N1), {N is N1+1}.
        c(0) --> [ ].

%     Program 19.1:  Enhancing the language a*b*c
/*
        The grammar for the declarative part of a Pascal program.
*/
        declarative_part -->
            const_declaration, type_declaration,
            var_declaration, procedure_declaration.

%        Constant declarations

        const_declaration --> [ ].
        const_declaration -->
            [const], const_definition, [;], const_definitions.

        const_definitions --> [ ].
        const_definitions -->
            const_definition, [;], const_definitions.

        const_definition --> identifier, [=], constant.

        identifier --> [X], {atom(X)}.

        constant --> [X], {constant(X)}.

%        Type declarations

        type_declaration --> [ ].
        type_declaration -->
            [type], type_definition, [;], type_definitions.

        type_definitions --> [ ].
        type_definitions --> type_definition, [;], type_definitions.

        type_definition --> identifier, [=], type.

        type --> ['INTEGER'].
        type --> ['REAL'].
        type --> ['BOOLEAN'].
        type --> ['CHAR'].

%        Variable declarations

        var_declaration --> [ ].
        var_declaration -->
            [var], var_definition, [;], var_definitions.

        var_definitions --> [ ].
        var_definitions --> var_definition, [;], var_definitions.
        var_definition --> identifiers, [:], type.

        identifiers --> identifier.
        identifiers --> identifier, [','], identifiers.

%        Procedure declarations

        procedure_declaration --> [ ].
        procedure_declaration --> procedure_heading, [';'], block.

        procedure_heading -->
            [procedure], identifier, formal_parameter_part.

        formal_parameter_part --> [ ].
        formal_parameter_part --> ['('], formal_parameter_section, [')'].

        formal_parameter_section --> formal_parameters.
        formal_parameter_section -->
            formal_parameters, [';'], formal_parameter_section.

        formal_parameters --> value_parameters.

        formal_parameters --> variable_parameters.

        value_parameters --> var_definition.

        variable_parameters --> [var], var_definition.

%     Program 19.3:  Parsing the declarative part of a Pascal block
/*
        parse(Start,Tokens) :-
            The sequence of tokens Tokens represented as a difference-list
            can be reached by applying the grammar rules defined by -->/2,
            starting from Start.
*/
        parse(A,Tokens) :-
		%% Fred
            nonterminal(A), A=B, parse(B,Tokens).
        parse((A,B),Tokens\Xs) :-
            parse(A,Tokens\Tokens1), parse(B,Tokens1\Xs).

        parse(A,Tokens) :-  terminals(A), connect(A,Tokens).
        parse({A},Xs\Xs) :-  A.

	terminals(Xs) :- list(Xs).

	list([]).
	list([X|Xs]) :- list(Xs).

        connect([ ],Xs\Xs).
        connect([W|Ws],[W|Xs]\Ys) :-  connect(Ws,Xs\Ys).

%     Program 19.4:  A definite clause grammar (DCG) interpreter
/*
        parse(Start,Tokens,N) :-
            The sequence of tokens Tokens, represented as a difference-list,
            can be reached by applying the grammar rules defined by -->/2,
            starting from Start, and N tokens are found.
*/
        parse(A,Tokens,N) :-
		%% fred
            nonterminal(A), A=B, parse(B,Tokens,N).
        parse((A,B),Tokens\Xs,N) :-
            parse(A,Tokens\Tokens1,NA), parse(B,Tokens1\Xs,NB),
	    N is NA+NB.

        parse(A,Tokens,N) :-  terminals(A), connect(A,Tokens), length(A,N).
        parse({A},Xs\Xs,0) :-  A.

	terminals(Xs) :- list(Xs).

	list([]).
	list([X|Xs]) :- list(Xs).

        connect([ ],Xs\Xs).
        connect([W|Ws],[W|Xs]\Ys) :-  connect(Ws,Xs\Ys).

     length([X|Xs],N) :- length(Xs,N1), N is N1+1.
     length([],0).

%      Program 19.5:  A DCG interpreter that counts words
%    Grammar Rules

        sentence --> noun_phrase, verb_phrase.

        noun_phrase --> determiner, noun_phrase2.
        noun_phrase --> noun_phrase2.

        noun_phrase2 --> adjective, noun_phrase2.
        noun_phrase2 --> noun.

        verb_phrase --> verb.
        verb_phrase --> verb, noun_phrase.

%       Vocabulary

        determiner --> [the].    adjective --> [decorated].
        determiner --> [a].

        noun --> [pieplate].    verb --> [contains].
        noun --> [surprise].

%     Program 19.6:  A DCG context-free grammar
        sentence(sentence(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

        noun_phrase(np(D,N)) --> determiner(D), noun_phrase2(N).
        noun_phrase(np(N)) --> noun_phrase2(N).

        noun_phrase2(np2(A,N)) --> adjective(A), noun_phrase2(N).
        noun_phrase2(np2(N)) --> noun(N).

        verb_phrase(vp(V)) --> verb(V).
        verb_phrase(vp(V,N)) --> verb(V), noun_phrase(N).

%       Vocabulary

        determiner(det(the)) --> [the].
        determiner(det(a)) --> [a].

        noun(noun(pieplate)) --> [pieplate].
        noun(noun(surprise)) --> [surprise].

        adjective(adj(decorated)) --> [decorated].

        verb(verb(contains)) --> [contains].

%      Program 19.7:  A DCG computing a parse tree
        sentence(sentence(NP,VP)) -->
            noun_phrase(NP,Num), verb_phrase(VP,Num).

        noun_phrase(np(D,N),Num) -->
            determiner(D,Num), noun_phrase2(N,Num).
        noun_phrase(np(N),Num) --> noun_phrase2(N,Num).

        noun_phrase2(np2(A,N),Num) -->
            adjective(A), noun_phrase2(N,Num).
        noun_phrase2(np2(N),Num) --> noun(N,Num).

        verb_phrase(vp(V),Num) --> verb(V,Num).
        verb_phrase(vp(V,N),Num) -->
            verb(V,Num), noun_phrase(N,Num1).

%       Vocabulary

        determiner(det(the),Num) --> [the].
        determiner(det(a),singular) --> [a].

        noun(noun(pieplate),singular) --> [pieplate].
        noun(noun(pieplates),plural) --> [pieplates].
        noun(noun(surprise),singular) --> [surprise].
        noun(noun(surprises),plural) --> [surprises].

        adjective(adj(decorated)) --> [decorated].

        verb(verb(contains),singular) --> [contains].
        verb(verb(contain),plural) --> [contain].

%     Program 19.8:  A DCG with subject/object number agreement
        number(0) --> [zero].
        number(N) --> xxx(N).

        xxx(N) -->
            digit(D), [hundred], rest_xxx(N1), {N is D100+N1}.
        xxx(N) --> xx(N).

        rest_xxx(0) --> [ ].
        rest_xxx(N) --> [and], xx(N).

        xx(N) --> digit(N).
        xx(N) --> teen(N).
        xx(N) --> tens(T), rest_xx(N1), {N is T+N1}.

        rest_xx(0) --> [ ].
        rest_xx(N) --> digit(N).

        digit(1) --> [one].         teen(10) --> [ten].
        digit(2) --> [two].         teen(11) --> [eleven].
        digit(3) --> [three].       teen(12) --> [twelve].
        digit(4) --> [four].        teen(13) --> [thirteen].
        digit(5) --> [five].        teen(14) --> [fourteen].
        digit(6) --> [six].         teen(15) --> [fifteen].
        digit(7) --> [seven].       teen(16) --> [sixteen].
        digit(8) --> [eight].       teen(17) --> [seventeen].
        digit(9) --> [nine].        teen(18) --> [eighteen].
                                    teen(19) --> [nineteen].
        tens(20) --> [twenty].
        tens(30) --> [thirty].
        tens(40) --> [forty].
        tens(50) --> [fifty].
        tens(60) --> [sixty].
        tens(70) --> [seventy].
        tens(80) --> [eighty].
        tens(90) --> [ninety].

%     Program 19.9:  A DCG for recognizing numbers

	uncle(Uncle,Person) :-
		brother(Uncle,Parent), parent(Parent,Person).

	sibling(Sib1,Sib2) :-
		parent(Parent,Sib1), parent(Parent,SIb2), Sib1 \= Sib2.

	cousin(Cousin1,Cousin2) :-
		parent(Parent1,Cousin1),
		parent(Parent2,Cousin2),
		sibling(Parent1,Parent2).

%	Program 2.1:  Defining family relationships
	resistor(power,n1).
	resistor(power,n2).

	transistor(n2,ground,n1).
	transistor(n3,n4,n2).
	transistor(n5,ground,n4).

/*
	inverter(Input,Output) :-
		Output is the inversion of Input.
*/

	inverter(Input,Output) :-
		transistor(Input,ground,Output),
		resistor(power,Output).

/*
	nand_gate(Input1,Input2,Output):-
	   Output is the logical nand of Input1 and Input2.
*/

	nand_gate(Input1,Input2,Output) :-
		transistor(Input1,X,Output),
		transistor(Input2,ground,X),
		resistor(power,Output).

/*
	and_gate(Input1,Input2,Output):-
	  Output is the logical and of Input1 and Input2.		     
*/

	and_gate(Input1,Input2,Output) :-
		nand_gate(Input1,Input2,X),
		inverter(X,Output).

%	Program 2.2: A circuit for a logical and-gate
/*
	resistor(R,Node1,Node2) :-
	   R is a resistor between Node1 and Node2.
*/
	resistor(r1,power,n1).
	resistor(r2,power,n2).

/*
	transistor(T,Gate,Source,Drain) :-
		T is a transistor whose gate is Gate,
		source is Source, and drain is Drain.
*/

	transistor(t1,n2,ground,n1).
	transistor(t2,n3,n4,n2).
	transistor(t3,n5,ground,n4).

/*
	inverter(I,Input,Output) :-
	  I is an inverter which inverts Input to Output.
*/
	inverter(inv(T,R),Input,Output) :-
		transistor(T,Input,ground,Output),
		resistor(R,power,Output).

/*
	nand_gate(Nand,Input1,Input2,Output):-
		Nand is a gate forming the logical nand, Output,
		of Input1 and Input2.
*/
	nand_gate(nand(T1,T2,R),INput1,Input2,Output) :-
		transistor(T1,Input1,X,Output),
		transistor(T2,Input2,ground,X),
		resistor(R,power,Output).

/*
	and_gate(And,Input1,Input2,Output):-
		And is a gate forming the logical and, Output, 
		of Input1 and Input2.
*/

	and_gate(and(N,I),INput1,Input2,Output) :-
		nand_gate(N,Input1,Input2,X),
		inverter(I,X,Output).

%	   Program 2.3: The circuit database with names
	lecturer(Lecturer,Course) :-
		course(Course,Time,Lecturer,Location).

	duration(Course,Length) :-
		course(Course,time(Day,Start,Finish),Lecturer,Location),
		plus(Start,Length,Finish).

	teaches(Lecturer,Day) :-
		course(Course,time(Day,Start,Finish),Lecturer,Location).

	occupied(Room,Day,Time) :-
		course(Course,time(Day,Start,Finish),Lecturer,Location),
		Start =< Time, Time =< Finish.

%	Program 2.4: Course rules
/*
	ancestor(Ancestor,Descendant)  :-
		Ancestor is an ancestor of Descendant.
*/
	ancestor(Ancestor,Descendant) :-
		parent(Ancestor,Descendant).
	ancestor(Ancestor,Descendant) :-
		parent(Ancestor,Person), ancestor(Person,Descendant).

%	Program 2.5: The ancestor relationship
	edge(a,b).	edge(a,c).	edge(b,d).
	edge(c,d).	edge(d,e).	edge(f,g).

%	Program 2.6: A directed graph
/*
	connected(Node1,Node2) :-
		Node1 is connected to Node2 in the graph
		defined by the edge/2 relation.
*/
	connected(Node,Node).
	connected(Node1,Node2) :- edge(Node1,Link), connected(Link,Node2).

%	Program 2.7: The transitive closure of the edge relationship
/*  
  solve_dfs(State,History,Moves) :-
      Moves is the sequence of moves to reach a desired final state 
	from the current State, where History contains the states 
	visited previously.
*/
     solve_dfs(State,History,[]) :- 
	final_state(State).
     solve_dfs(State,History,[Move|Moves]) :-
	move(State,Move),
	update(State,Move,State1),
	legal(State1),
	not member(State1,History),
	solve_dfs(State1,[State1|History],Moves).

/*  Testing the framework	*/

     test_dfs(Problem,Moves) :-
        initial_state(Problem,State), solve_dfs(State,[State],Moves).

%  Program 20.1  A depth-first state-transition framework for problem solving
/*
   States for the wolf, goat and cabbage problem are a structure
   wgc(Boat,Left,Right), where Boat is the bank on which the boat 
   currently is, Left is the list of occupants on the left bank of 
   the river, and Right is the list of occupants on the right bank.
*/
     initial_state(wgc,wgc(left,[wolf,goat,cabbage],[])).

     final_state(wgc(right,[],[wolf,goat,cabbage])).

     move(wgc(left,L,R),Cargo) :- member(Cargo,L).
     move(wgc(right,L,R),Cargo) :- member(Cargo,R).
     move(wgc(B,L,R),alone).

     update(wgc(B,L,R),Cargo,wgc(B1,L1,R1)) :-
         update_boat(B,B1), update_banks(Cargo,B,L,R,L1,R1).

     update_boat(left,right).        
     update_boat(right,left).
        
     update_banks(alone,B,L,R,L,R).
     update_banks(Cargo,left,L,R,L1,R1) :- 
	select(Cargo,L,L1), insert(Cargo,R,R1).
     update_banks(Cargo,right,L,R,L1,R1) :- 
	select(Cargo,R,R1), insert(Cargo,L,L1).

     insert(X,[Y|Ys],[X,Y|Ys]) :-
	precedes(X,Y).
     insert(X,[Y|Ys],[Y|Zs]) :-
	precedes(Y,X), insert(X,Ys,Zs).
     insert(X,[],[X]).

     precedes(wolf,X).
     precedes(X,cabbage).

     legal(wgc(left,L,R)) :- not illegal(R).
     legal(wgc(right,L,R)) :- not illegal(L).

	illegal(Bank) :- member(wolf,Bank), member(goat,Bank).
        illegal(Bank) :- member(goat,Bank), member(cabbage,Bank).

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :-
		select(X,Ys,Zs).

%    Program 20.2:  Solving the wolf, goat, and cabbage problem	/* Problem Solving :- Water Jugs Problem */

	initial_state(jugs,jugs(0,0)).

     final_state(jugs(4,V2)).            
     final_state(jugs(V1,4)).

     move(jugs(V1,V2),fill(1)).	        
     move(jugs(V1,V2),fill(2)).
     move(jugs(V1,V2),empty(1)).	
     move(jugs(V1,V2),empty(2)).
     move(jugs(V1,V2),transfer(2,1)).	
     move(jugs(V1,V2),transfer(1,2)).

     update(jugs(V1,V2),fill(1),jugs(C1,V2)) :- capacity(1,C1).
     update(jugs(V1,V2),fill(2),jugs(V1,C2)) :- capacity(2,C2).
     update(jugs(V1,V2),empty(1),jugs(0,V2)).
     update(jugs(V1,V2),empty(2),jugs(V1,0)).
     update(jugs(V1,V2),transfer(2,1),jugs(W1,W2)) :-
	capacity(1,C1),
	Liquid is V1 + V2,
	Excess is Liquid - C1,
	adjust(Liquid,Excess,W1,W2).
     update(jugs(V1,V2),transfer(1,2),jugs(W1,W2)) :-
	capacity(2,C2),
        Liquid is V1 + V2,
        Excess is Liquid - C2,
        adjust(Liquid,Excess,W2,W1).

     adjust(Liquid, Excess,Liquid,0) :- Excess =< 0.
     adjust(Liquid,Excess,V,Excess) :- Excess > 0, V is Liquid - Excess.

     legal(jugs(V1,V2)).

     capacity(1,8).		
     capacity(2,5).

%  Program 20.3    Solving the water jugs problem
/*  
   solve_hill_climb(State,History,Moves) :-
      Moves is the sequence of moves to reach a desired final state 
	from the current State, where History are the states 
	visited previously. 
*/

     solve_hill_climb(State,History,[]) :- 
	final_state(State).
     solve_hill_climb(State,History,[Move|Moves]) :-
	hill_climb(State,Move),
	update(State,Move,State1),
	legal(State1),
	not member(State1,History),
	solve_hill_climb(State1,[State1|History],Moves).

     hill_climb(State,Move) :-
        findall(M,move(State,M),Moves),
        evaluate_and_order(Moves,State,[],MVs),
	member((Move,Value),MVs).

/*   
   evaluate_and_order(Moves,State,SoFar,OrderedMVs) :-
	All the Moves from the current State are evaluated and 
	ordered as OrderedMVs. SoFar is an accumulator for 
	partial computations.
*/
     evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
	update(State,Move,State1),
	value(State1,Value),
	insert((Move,Value),MVs,MVs1),
	evaluate_and_order(Moves,State,MVs1,OrderedMVs).
     evaluate_and_order([],State,MVs,MVs).

     insert(MV,[],[MV]).
     insert((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :- 
	V >= V1.
     insert((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
	V < V1, insert((M,V),MVs,MVs1).

/*	Testing the Framework */

	test_hill_climb(Problem,Moves)  :-
		initial_state(Problem,State),
		solve_hill_climb(State,[State],Moves).

%   Program 20.4: Hill climbing framework for problem solving
% Data

initial_state(tree,a).	value(a,0).	final_state(j).

move(a,b).	value(b,1).	move(c,g).	value(g,6).
move(a,c).	value(c,5).	move(d,j).	value(j,9).
move(a,d).	value(d,7).	move(e,k).	value(k,1).
move(a,e).	value(e,2).	move(f,h).	value(h,3).
move(c,f).	value(f,4).	move(f,i).	value(i,2).

%   Program 20.5  Test data
/*
   solve_best(Frontier,History,Moves) :-
      Moves is the sequence of moves to reach a desired final state 
	from the initial state, where Frontier contains the current 
	states under consideration, and History contains the states 
	visited previously.
*/

     solve_best([state(State,Path,Value)|Frontier],History,Moves) :- 
	final_state(State), reverse(Path,Moves).
     solve_best([state(State,Path,Value)|Frontier],History,FinalPath) :-
	findall(M,move(State,M),Moves),
	updates(Moves,Path,State,States),
	legals(States,States1),
	news(States1,History,States2),
	evaluates(States2,Values),
	inserts(Values,Frontier,Frontier1),
	solve_best(Frontier1,[State|History],FinalPath).

/*
   updates(Moves,Path,State,States) :-
	States is the list of possible states accessible from the 
	current State, according to the list of possible Moves,
	where Path is a path from the initial node to State.
*/

     updates([Move|Moves],Path,State,[(State1,[Move|Path])|States]) :-
	update(State,Move,State1), updates(Moves,Path,State,States).
     updates([],Path,State,[]).

/*
   legals(States,States1) :-
	States1 is the subset of the list of States that are legal.
*/

     legals([(S,P)|States],[(S,P)|States1]) :-
	legal(S), legals(States,States1).
     legals([(S,P)|States],States1) :-
	not legal(S), legals(States,States1).
     legals([],[]).

/*
   news(States,History,States1) :-
	States1 is the list of states in States but not	in History.
*/
     news([(State,Path)|States],History,States1) :-
	member(State,History), news(States,History,States1).
     news([(State,Path)|States],History,[(State,Path)|States1]) :-
	not member(State,History), news(States,History,States1).
     news([],History,[]).

/*
   evaluates(States,Values) :- 
	Values is the list of tuples of States augmented by their value.
*/
     evaluates([(State,Path)|States],[state(State,Path,Value)|Values]) :-
	value(State,Value), evaluates(States,Values).
     evaluates([],[]).

/*
   inserts(States,Frontier,Frontier1) :-
	Frontier1 is the result of inserting States into the current Frontier.
*/
     inserts([Value|Values],Frontier,Frontier1) :-
	insert(Value,Frontier,Frontier0),
	inserts(Values,Frontier0,Frontier1).
     inserts([],Frontier,Frontier).

     insert(State,[],[State]).
     insert(State,[State1|States],[State,State1|States]) :- 
	lesseq_value(State,State1).
     insert(State,[State1|States],[State|States]) :- 
	equals(State,State1).
     insert(State,[State1|States],[State1|States1]) :-
	greater_value(State,State1), insert(State,States,States1).

     equals(state(S,P,V),state(S,P1,V)).

     lesseq_value(state(S1,P1,V1),state(S2,P2,V2)) :- S1 \== S2, V1 =< V2.

     greater_value(state(S1,P1,V1),state(S2,P2,V2)) :- V1 > V2.

%  Program 20.6     Best first framework for problem solving
/*
solve_best(Frontier,History,Moves) :-
      Moves is the sequence of moves to reach a desired final state 
	from the initial state. Frontier contains the current states 
	under consideration. History contains the states visited previously.
*/
     solve_best([state(State,Path,Value)|Frontier],History,Moves) :- 
	final_state(State), reverse(Path,[],Moves).
     solve_best([state(State,Path,Value)|Frontier],History,FinalPath) :-
	findall(M,move(State,M),Moves),
	update_frontier(Moves,State,Path,History,Frontier,Frontier1),
	solve_best(Frontier1,[State|History],FinalPath).

update_frontier([Move|Moves],State,Path,History,Frontier,Frontier1) :-
   	update(State,Move,State1),
	legal(State1),
	value(State1,Value),
	not member(State1,History),
	insert(state(State1,[Move|Path],Value),Frontier,Frontier0),
	update_frontier(Moves,State,Path,History,Frontier0,Frontier1).
update_frontier([],State,Path,History,Frontier,Frontier).


     insert(State,[],[State]).
     insert(State,[State1|States],[State,State1|States]) :- 
	lesseq_value(State,State1).
     insert(State,[State1|States],[State|States]) :- 
	equals(State,State1).
     insert(State,[State1|States],[State1|States1]) :-
	greater_value(State,State1), insert(State,States,States1).

     equals(state(S,P,V),state(S,P1,V)).

     lesseq_value(state(S1,P1,V1),state(S2,P2,V2)) :- S1 \== S2, V1 =< V2.

     greater_value(state(S1,P1,V1),state(S2,P2,V2)) :- V1 > V2.

	reverse([X|Xs],Acc,Ys) :- reverse(Xs,[X|Acc],Ys).
	reverse([],Ys,Ys).

%  Program 20.7     Concise best first framework for problem solving
/*
   play(Game) :- Play game with name Game.
*/
     play(Game) :- 
	initialize(Game,Position,Player), 
	display_game(Position,Player),
	play(Position,Player,Result).

     play(Position,Player,Result) :- 
        game_over(Position,Player,Result), !, announce(Result).
     play(Position,Player,Result) :-
        choose_move(Position,Player,Move),
        move(Move,Position,Position1),
        display_game(Position1,Player), 
        next_player(Player,Player1),
        !, play(Position1,Player1,Result).

%   Program 20.8 : Framework for Game Playing.
/*
    evaluate_and_choose(Moves,Position,Record,BestMove) :-
	Chooses the BestMove from the set of Moves from the current
	Position, Record records the current best move.
*/
     evaluate_and_choose([Move|Moves],Position,Record,BestMove) :-
	move(Move,Position,Position1),
	value(Position1,Value), 
	update(Move,Value,Record,Record1), 
	evaluate_and_choose(Moves,Position,Record1,BestMove).
     evaluate_and_choose([],Position,(Move,Value),Move).

	update(Move,Value,(Move1,Value1),(Move1,Value1)) :- 
	   Value =< Value1.
	update(Move,Value,(Move1,Value1),(Move,Value)) :- 
	  Value > Value1.

%  Program 20.9   Choosing the best move 
/*
   evaluate_and_choose(Moves,Position,Depth,Flag,Record,BestMove) :-
	Chooses the BestMove from the set of Moves from the current Position
	using the minimax algorithm searching Depth ply ahead.
	Flag indicates if we are currently minimizing or maximizing.
	Record records the current best move
*/

     evaluate_and_choose([Move|Moves],Position,D,MaxMin,Record,BestMove) :-
	move(Move,Position,Position1),
        minimax(D,Position1,MaxMin,MoveX,Value), 
	update(Move,Value,Record,Record1),
 	evaluate_and_choose(Moves,Position,D,MaxMin,Record1,BestMove).
     evaluate_and_choose([],Position,D,MaxMin,Record,Record).

     minimax(0,Position,MaxMin,Move,Value) :- 
	value(Position,V), 
	Value is V * MaxMin.
     minimax(D,Position,MaxMin,Move,Value) :-
	D > 0,
        findall(M,move(Position,M),Moves),
        D1 is D - 1,
        MinMax is -MaxMin,
        evaluate_and_choose(Moves,Position,D1,MinMax,(nil,-1000),(Move,Value)).

	update(Move,Value,(Move1,Value1),(Move1,Value1)) :- 
	   Value =< Value1.
	update(Move,Value,(Move1,Value1),(Move,Value)) :- 
	  Value > Value1.

%  Program 20.10   Choosing the best move with the minimax algorithm
/*
   evaluate_and_choose(Moves,Position,Depth,Alpha,Beta,Record,BestMove) :-
	Chooses the BestMove from the set of Moves from the current Position
	using the minimax algorithm with alpha-beta cutoff searching 
	Depth ply ahead. Alpha and Beta are the parameters of the algorithm.
	Record records the current best move
*/
    evaluate_and_choose([Move|Moves],Position,D,Alpha,Beta,Move1,BestMove) :- 
	move(Move,Position,Position1),
        alpha_beta(D,Position1,Alpha,Beta,MoveX,Value),
        Value1 is -Value,   
        cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Move1,BestMove).
     evaluate_and_choose([],Position,D,Alpha,Beta,Move,(Move,A)).

     alpha_beta(0,Position,Alpha,Beta,Move,Value) :- 
	value(Position,Value).
     alpha_beta(D,Position,Alpha,Beta,Move,Value) :- 
        findall(M,move(Position,M),Moves),
        Alpha1 is -Beta, 
	Beta1 is -Alpha, 
        D1 is D-1,
	evaluate_and_choose(Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,(Move,Value)) :- 
	Value >= Beta.
     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :- 
        Alpha < Value, Value < Beta, 
	evaluate_and_choose(Moves,Position,D,Value,Beta,Move,BestMove).
     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-
        Value =< Alpha, 
	evaluate_and_choose(Moves,Position,D,Alpha,Beta,Move1,BestMove).

%  Program 20.11   Choosing a move using minimax with alpha-beta pruning
     mastermind(Code) :-
        cleanup, guess(Code), check(Code), announce.
     
     guess(Code) :-
	Code = [X1,X2,X3], selects(Code,[1,2,3,4]).
     
  /*  Verify the proposed guess	   */

     check(Guess) :-
        not inconsistent(Guess), ask(Guess).
     
     inconsistent(Guess) :-		
        query(OldGuess,Bulls,Cows), 
	not bulls_and_cows_match(OldGuess,Guess,Bulls,Cows).
     
     bulls_and_cows_match(OldGuess,Guess,Bulls,Cows) :- 
        exact_matches(OldGuess,Guess,N1),
        Bulls =:= N1,
        common_members(OldGuess,Guess,N2),
        Cows =:= N2 - Bulls.
     
     exact_matches(X,Y,N) :- 
	size_of(A,same_place(A,X,Y),N).  

     common_members(X,Y,N) :- 
	size_of(A,(member(A,X),member(A,Y)),N).

     same_place(X,[X|Xs],[X|Ys]).
     same_place(A,[X|Xs],[Y|Ys]) :- same_place(A,Xs,Ys).

/*   Asking a guess	*/

     ask(Guess) :-
	repeat,
        writeln(['How many bulls and cows in ',Guess,'?']),
        read((Bulls,Cows)),
        sensible(Bulls,Cows), !,
        assert(query(Guess,Bulls,Cows)), 
	Bulls =:= 4.
     
     sensible(Bulls,Cows) :- 
	integer(Bulls), integer(Cows), Bulls + Cows =< 4.
     
     /*    Bookkeeping     */

     cleanup :- abolish(query,3).

     announce :-
	size_of(X,query(X,A,B),N),
        writeln(['Found the answer after ',N,' queries']).

     size_of(X,Goal,N) :-
	findall(X,Goal,Instances), length(Instances,N).

	selects([X|Xs],Ys) :- 
		select(X,Ys,Ys1),selects(Xs,Ys1).
	selects([],Ys).

	 select(X,[X|Xs],Xs).
	 select(X,[Y|Ys],[Y|Zs]) :-
		select(X,Ys,Zs).

	length(Xs,N) :- length(Xs,0,N).
	length([X|Xs],Acc,N) :-
		Acc1 is Acc + 1,
		length(Xs,Acc1,N).
	length([],N,N).

%   Program 21.1  Playing mastermind

/*   The play framework	   */

     play(Game) :- 
	initialize(Game,Position,Player), 
	display_game(Position,Player),
	play(Position,Player,Result).

     play(Position,Player,Result) :- 
        game_over(Position,Player,Result), !, announce(Result).
     play(Position,Player,Result) :-
        choose_move(Position,Player,Move),
        move(Move,Position,Position1),
        display_game(Position1,Player), 
        next_player(Player,Player1),
        !, 
        play(Position1,Player1,Result).

/*  Filling in the game-playing framework	*/

     initialize(nim,[1,3,5,7],opponent).

     display_game(Position,X) :- write(Position), nl.

     game_over([],Player,Player).

     announce(computer) :- write('You won! Congratulations.'), nl.
     announce(opponent) :- write('I won.'), nl.

/*  Choosing moves		*/

     choose_move(Position,opponent,Move) :- 
	writeln(['Please make move']), read(Move), legal(Move,Position).

legal((K,N),Position) :- nth_member(N,Position,M), N =< M.

nth_member(1,[X|Xs],X).
nth_member(N,[X|Xs],Y) :- N > 1, N1 is N-1, nth_member(N1,Xs,Y).

     choose_move(Ns,computer,Move) :- 
	evaluate(Position,Safety,Sum),
	decide_move(Safety,Position,Sum,Move).

evaluate(Position,Safety,Sum) :-
   nim_sum(Position,[],Sum), safety(Sum,Safety).

safety(Sum,safe) :- zero(Sum), !.
safety(Sum,unsafe) :- not zero(Sum), !.

decide_move(safe,Position,Sum,(1,1)).  % The computer's arbitrary move
decide_move(unsafe,Position,Sum,Move) :-
   safe_move(Position,Sum,Move).

/*  
    move(Move,Position,Position1) :-
	Position1 is the result of executing the move Move
	from the current Position.
*/
     move((K,M),[N|Ns],[N|Ns1]) :- 
	K > 1, K1 is K - 1, move((K1,M),Ns,Ns1).
     move((1,N),[N|Ns],Ns).
     move((1,M),[N|Ns],[N1|Ns]) :- 
	N > M, N1 is N - M.

     next_player(computer,opponent).    next_player(opponent,computer).

/*  
    nim_sum(Position,SoFar,Sum) :-
	Sum is the nim-sum of the current Position,
	and SoFar is an accumulated value.
*/
     nim_sum([N|Ns],Bs,Sum) :- 
         binary(N,Ds), nim_add(Ds,Bs,Bs1), nim_sum(Ns,Bs1,Sum).
     nim_sum([],Sum,Sum).

	nim_add(Bs,[],Bs).
	nim_add([],Bs,Bs).
        nim_add([B|Bs],[C|Cs],[D|Ds]) :- 
           D is (B+C) mod 2, nim_add(Bs,Cs,Ds).

	 binary(1,[1]).
	 binary(N,[D|Ds]) :-
	     N > 1, D is N mod 2, N1 is N/2, binary(N1,Ds).

 	decimal(Ds,N) :- decimal(Ds,0,1,N).
	decimal([],N,T,N).
	decimal([D|Ds],A,T,N) :- A1 is A+D*T, T1 is T*2, decimal(Ds,A1,T1,N).

          zero([]).
          zero([0|Zs]) :- zero(Zs).

/*    
    safe_move(Position,NimSum,Move) :-
	Move is a move from the current Position with 
	the value NimSum which leaves a safe position.
*/
     safe_move(Piles,NimSum,Move) :- 
	safe_move(Piles,NimSum,1,Move).

     safe_move([Pile|Piles],NimSum,K,(K,M)) :- 
        binary(Pile,Bs), can_zero(Bs,NimSum,Ds,0), decimal(Ds,M).
     safe_move([Pile|Piles],NimSum,K,Move) :- 
	K1 is K + 1, safe_move(Piles,NimSum,K1,Move).

     can_zero([],NimSum,[],0) :-
        zero(NimSum).
     can_zero([B|Bs],[0|NimSum],[C|Ds],C) :- 
	can_zero(Bs,NimSum,Ds,C).
     can_zero([B|Bs],[1|NimSum],[D|Ds],C) :- 
    	D is 1 - B*C, C1 is 1 - B, can_zero(Bs,NimSum,Ds,C1).

%  Program 21.2   A program for playing a winning game of Nim

 /* Play framework  */

     play(Game) :- 
	initialise(Game,Position,Player), 
	display_game(Position,Player),
	play(Position,Player,Result).

     play(Position,Player,Result) :- 
        game_over(Position,Player,Result), !, announce(Result).
     play(Position,Player,Result) :-
        choose_move(Position,Player,Move),
        move(Move,Position,Position1),
        display_game(Position1,Player), 
        next_player(Player,Player1),
        !, 
        play(Position1,Player1,Result).

 /* Choosing a move by minimax with alpha-beta cut-off  */

     choose_move(Position,computer,Move) :-
        lookahead(Depth), 
	alpha_beta(Depth,Position,-40,40,Move,Value),
	nl, write(Move), nl.
     choose_move(Position,opponent,Move) :- 
	nl, writeln(['please make move']), read(Move), legal(Move).

     alpha_beta(0,Position,Alpha,Beta,Move,Value) :- 
	value(Position,Value).
     alpha_beta(D,Position,Alpha,Beta,Move,Value) :- 
        findall(M,move(Position,M),Moves),
        Alpha1 is -Beta, 
	Beta1 is -Alpha, 
        D1 is D-1,
	evaluate_and_choose(Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,(Move,Value)) :- 
	Value >= Beta.
     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :- 
        Alpha < Value, Value < Beta, 
	evaluate_and_choose(Moves,Position,D,Value,Beta,Move,BestMove).
     cutoff(Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-
        Value =< Alpha, 
	evaluate_and_choose(Moves,Position,D,Alpha,Beta,Move1,BestMove).

     move(Board,[M|Ms]) :- 
        member(M,[1,2,3,4,5,6]), 
	stones_in_hole(M,Board,N),
        extend_move(N,M,Board,Ms).
     move(board([0,0,0,0,0,0],K,Ys,L),[]).

     stones_in_hole(M,board(Hs,K,Ys,L),Stones) :-
	nth_member(M,Hs,Stones), Stones > 0.

     extend_move(Stones,M,Board,[]) :-
	Stones =\= (7-M) mod 13, !.
     extend_move(Stones,M,Board,Ms) :- 
	Stones =:= (7-M) mod 13, !, 
        distribute_stones(Stones,M,Board,Board1),
	move(Board1,Ms).

/*  Executing a move  */

     move([N|Ns],Board,FinalBoard) :- 
       stones_in_hole(N,Board,Stones),
       distribute_stones(Stones,N,Board,Board1),
       move(Ns,Board1,FinalBoard).
     move([],Board1,Board2) :-
	swap(Board1,Board2).

/*  distribute_stones(Stones,Hole,Board,Board1) :-
	Board1 is the result of distributing the number of stones,
	Stones, from Hole from the current Board.
	It consists of two stages: distributing the stones in the player's
	holes, distribute_my_holes, and distributing the stones in 
	the opponent's holes, distribute_your_holes.
*/

distribute_stones(Stones,Hole,Board,FinalBoard) :-
   distribute_my_holes(Stones,Hole,Board,Board1,Stones1),
   distribute_your_holes(Stones1,Board1,FinalBoard).

distribute_my_holes(Stones,N,board(Hs,K,Ys,L),board(Hs1,K1,Ys,L),Stones1) :-
  Stones > 7-N, !, 
  pick_up_and_distribute(N,Stones,Hs,Hs1),
  K1 is K+1, Stones1 is Stones+N-7.
distribute_my_holes(Stones,N,board(Hs,K,Ys,L),Board,0) :-
  pick_up_and_distribute(N,Stones,Hs,Hs1),
  check_capture(N,Stones,Hs1,Hs2,Ys,Ys1,Pieces),
  update_kalah(Pieces,N,Stones,K,K1),
  check_if_finished(board(Hs2,K1,Ys1,L),Board).
				       
check_capture(N,Stones,Hs,Hs1,Ys,Ys1,Pieces) :-
  FinishingHole is N+Stones,
  nth_member(FinishingHole,Hs,1),
  OppositeHole is 7-FinishingHole,
  nth_member(OppositeHole,Ys,Y),
  Y > 0, !,
  n_substitute(OppositeHole,Ys,0,Ys1),
  n_substitute(FinishingHole,Hs,0,Hs1),
  Pieces is Y+1.
check_capture(N,Stones,Hs,Hs,Ys,Ys,0) :- !.

check_if_finished(board(Hs,K,Ys,L),board(Hs,K,Hs,L1)) :-
  zero(Hs), !, sumlist(Ys,YsSum), L1 is L+YsSum.
check_if_finished(board(Hs,K,Ys,L),board(Ys,K1,Ys,L)) :-
  zero(Ys), !, sumlist(Hs,HsSum), K1 is K+HsSum.
check_if_finished(Board,Board) :- !.
    
update_kalah(0,Stones,N,K,K) :- Stones < 7-N, !.
update_kalah(0,Stones,N,K,K1) :- Stones =:= 7-N, !, K1 is K+1.
update_kalah(Pieces,Stones,N,K,K1) :- Pieces > 0, !, K1 is K+Pieces.

distribute_your_holes(0,Board,Board) :- !.
distribute_your_holes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
  1 =< Stones, Stones =< 6, 
  non_zero(Hs), !, 
  distribute(Stones,Ys,Ys1).
distribute_your_holes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
  Stones > 6, !, 
  distribute(6,Ys,Ys1),
  Stones1 is Stones-6,
  distribute_stones(Stones1,0,board(Hs,K,Ys1,L),Board).
distribute_your_holes(Stones,board(Hs,K,Ys,L),board(Hs,K,Hs,L1)) :-
  zero(Hs), !, sumlist(Ys,YsSum), L1 is Stones+YsSum+L.

/*  Lower level stone distribution    */

pick_up_and_distribute(0,N,Hs,Hs1) :-
  !, distribute(N,Hs,Hs1).
pick_up_and_distribute(1,N,[H|Hs],[0|Hs1]) :-
  !, distribute(N,Hs,Hs1).
pick_up_and_distribute(K,N,[H|Hs],[H|Hs1]) :- 
  K > 1, !, K1 is K-1, pick_up_and_distribute(K1,N,Hs,Hs1).

     distribute(0,Hs,Hs) :- !.
     distribute(N,[H|Hs],[H1|Hs1]) :-
        N > 0, !, N1 is N-1, H1 is H+1, distribute(N1,Hs,Hs1).
     distribute(N,[],[]) :- !.

/*   Evaluation function	*/

     value(board(H,K,Y,L),Value) :- Value is K-L.

/*  Testing for the end of the game	*/

     game_over(board(0,N,0,N),Player,draw) :-
	pieces(K), N =:= 6*K, !.
     game_over(board(H,K,Y,L),Player,Player) :- 
	pieces(N), K > 6*N, !.
     game_over(board(H,K,Y,L),Player,Opponent) :-
	pieces(N), L > 6*N, next_player(Player,Opponent).

     announce(opponent) :- writeln(['You won! Congratulations.']).
     announce(computer) :- writeln(['I won.']).
     announce(draw) :- writeln(['The game is a draw.']).

/*  Miscellaneous game utilities	*/

	nth_member(N,[H|Hs],K) :-
	    N > 1, !, N1 is N - 1, nth_member(N1,Hs,K).
	nth_member(1,[H|Hs],H).

n_substitute(1,[X|Xs],Y,[Y|Xs]) :- !.
n_substitute(N,[X|Xs],Y,[X|Xs1]) :- 
  N > 1, !, N1 is N-1, n_substitute(N1,Xs,Y,Xs1).
			       
     next_player(computer,opponent).	
     next_player(opponent,computer).

     legal([N|Ns]) :-  0 < N, N < 7, legal(Ns).
     legal([]).

     swap(board(Hs,K,Ys,L),board(Ys,L,Hs,K)).

     display_game(Position,computer) :-
	show(Position).
     display_game(Position,opponent) :-
	swap(Position,Position1), show(Position1).

     show(board(H,K,Y,L)) :-
        reverse(H,HR), 	write_stones(HR), write_kalahs(K,L), write_stones(Y).

     write_stones(H) :- 
	nl, tab(5), display_holes(H).

     display_holes([H|Hs]) :-
	write_pile(H), display_holes(Hs).
     display_holes([]) :- nl.

	write_pile(N) :- N < 10, write(N), tab(4).
	write_pile(N) :- N >= 10, write(N), tab(3).

     write_kalahs(K,L) :- 
        write(K), tab(34), write(L), nl.

     zero([0,0,0,0,0,0]).

     non_zero(Hs) :- Hs \== [0,0,0,0,0,0].

/*  Initializing	*/

     lookahead(2).
     initialize(kalah,board([N,N,N,N,N,N],0,[N,N,N,N,N,N],0),opponent) :-
	pieces(N).

     pieces(6).

%  Program 21.3  A complete program for playing Kalah
/*   Credit Evaluation

     credit(Client,Answer) :-
	Answer is the reply to a request by Client for credit.
*/
      credit(Client,Answer) :-
           ok_profile(Client),
           collateral_rating(Client,CollateralRating),
           financial_rating(Client,FinancialRating),
           bank_yield(Client,Yield),
           evaluate(profile(CollateralRating,FinancialRating,Yield),Answer) , !.

/*  The collateral rating module

     collateral_rating(Client,Rating) :-
	Rating is a qualitative description assessing the collateral
	offered by Client to cover the request for credit.
*/
     collateral_rating(Client,Rating) :-
        collateral_profile(Client,FirstClass,SecondClass,Illiquid),
        collateral_evaluation(FirstClass,SecondClass,Illiquid,Rating).

     collateral_profile(Client,FirstClass,SecondClass,Illiquid) :-
	requested_credit(Client,Credit),
	collateral_percent(first_class,Client,Credit,FirstClass),
	collateral_percent(second_class,Client,Credit,SecondClass),
	collateral_percent(Illiquid,Client,Credit,Illiquid).

     collateral_percent(Type,Client,Total,Value) :-
	findall(X,(collateral(Collateral,Type),
			amount(Collateral,Client,X)),Xs),
   	sumlist(Xs,Sum),
	Value is Sum*100/Total.

     /*   Evaluation rules    */

     collateral_evaluation(FirstClass,SecondClass,Illiquid,excellent) :-
  	FirstClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,Illiquid,excellent) :-
  	FirstClass > 70, FirstClass + SecondClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,Illiquid,good) :-
  	FirstClass + SecondClass > 60,
  	FirstClass + SecondClass < 70,
  	FirstClass + SecondClass + Illiquid >= 100.

     /*  Bank data - classification of collateral   */

     collateral(local_currency_deposits,first_class).
     collateral(foreign_currency_deposits,first_class).
     collateral(negotiate_instruments,second_class).
     collateral(mortgage,illiquid).

%    Financial rating

/*  
    financial_rating(Client,Rating) :-
	Rating  is a qualitative description assessing the financial 
	record offered by Client to support the request for credit.
*/	
     financial_rating(Client,Rating) :-
        financial_factors(Factors),
        score(Factors,Client,0,Score),
        calibrate(Score,Rating).

     /*   Financial evalauation rules   */

     calibrate(Score,bad) :- 	   Score =< -500.
     calibrate(Score,medium) :-    -500 < Score, Score < 150.
     calibrate(Score,good) :- 	   150 =< Score, Score < 1000.
     calibrate(Score,excellent) :- Score >= 1000.

     /*  Bank data - weighting factors	*/

     financial_factors([(net_worth_per_assets,5),
         (last_year_sales_growth,1),
         (gross_profits_on_sales,5),
         (short_term_debt_per_annual_sales,2)  ]).

     score([(Factor,Weight)|Factors],Client,Acc,Score) :-
        value(Factor,Client,Value),
        Acc1 is Acc + Weight*Value,
        score(Factors,Client,Acc1,Score).
     score([],Client,Score,Score).

/*  Final evaluation    

     evaluate(Profile,Outcome) :-
	Outcome is the reply to the client's Profile.
*/
     evaluate(Profile,Answer) :- 
	rule(Conditions,Answer), verify(Conditions,Profile).

     verify([condition(Type,Test,Rating)|Conditions],Profile) :-
        scale(Type,Scale),
        select_value(Type,Profile,Fact),
        compare(Test,Scale,Fact,Rating),
        verify(Conditions,Profile).
     verify([],Profile).

     compare('=',Scale,Rating,Rating).
     compare('>',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2).
     compare('>=',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2) ; Rating1 = Rating2.
     compare('<',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1).
     compare('=<',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1) ; Rating1 = Rating2.

     precedes([R1|Rs],R1,R2).
     precedes([R|Rs],R1,R2) :- R \== R2, precedes(Rs,R1,R2).

	select_value(collateral,profile(C,F,Y),C).
	select_value(finances,profile(C,F,Y),F).
	select_value(yield,profile(C,F,Y),Y).

     /*  Utilities   */

	sumlist(Is,Sum) :-
		sumlist(Is,0,Sum).
	sumlist([I|Is],Temp,Sum) :-
		Temp1 is Temp + I,
		sumlist(Is,Temp1,Sum).
	sumlist([],Sum,Sum).


/*  Bank data and rules	*/

rule([condition(collateral,'>=',excellent),condition(finances,'>=',good),
			condition(yield,'>=',reasonable)],give_credit).  
rule([condition(collateral,'=',good),condition(finances,'=',good),
			condition(yield,'>=',reasonable)],consult_superior).
rule([condition(collateral,'=<',moderate),condition(finances,'=<',medium)],
							  refuse_credit).

scale(collateral,[excellent,good,moderate]).
scale(finances,[excellent,good,medium,bad]).
scale(yield,[excellent,reasonable,poor]).

%  Program 22.1:  A credit evaluation system
   /*   Client data  */

bank_yield(client1,excellent).
requested_credit(client1,5000).

amount(local_currency_deposits,client1,3000).
amount(foreign_currency_deposits,client1,2000).
amount(bank_guarantees,client1,300).
                     
amount(negotiate_instruments,client1,500).
amount(stocks,client1,900).

amount(mortgage,client1,1200).
amount(documents,client1,1400).

value(net_worth_per_assets,client1,40).
value(last_year_sales_growth,client1,20).
value(gross_profits_on_sales,client1,45).
value(short_term_debt_per_annual_sales,client1,9).

ok_profile(client1).


%  Program 22.2: Test data for the credit evaluation system
/*  
     solve_equation(Equation,Unknown,Solution) :-
	Solution is a solution to the equation Equation 
	in the unknown Unknown.
*/
	:- op(40,xfx,\).
	:- op(50,xfx,^).

     solve_equation(A*B=0,X,Solution) :- 
	!,
        factorize(A*B,X,Factors\[]),
	remove_duplicates(Factors,Factors1),
	solve_factors(Factors1,X,Solution).

     solve_equation(Equation,X,Solution) :-
        single_occurrence(X,Equation), 
	!,
        position(X,Equation,[Side|Position]),
        maneuver_sides(Side,Equation,Equation1),
        isolate(Position,Equation1,Solution).

     solve_equation(Lhs=Rhs,X,Solution) :-
        is_polynomial(Lhs,X),
	is_polynomial(Rhs,X),
	!,
	polynomial_normal_form(Lhs-Rhs,X,PolyForm),
        solve_polynomial_equation(PolyForm,X,Solution).

     solve_equation(Equation,X,Solution) :-
        offenders(Equation,X,Offenders),
	multiple(Offenders),
    	homogenize(Equation,X,Offenders,Equation1,X1),
        solve_equation(Equation1,X1,Solution1),
        solve_equation(Solution1,X,Solution).

/*  The factorization method

     factorize(Expression,Subterm,Factors) :-
	  Factors is a difference-list consisting of the factors of
	  the multiplicative term Expression that contains the Subterm.
*/
     factorize(A*B,X,Factors\Rest) :-
	!, factorize(A,X,Factors\Factors1), factorize(B,X,Factors1\Rest).
     factorize(C,X,[C|Factors]\Factors) :-
	subterm(X,C),  !.
     factorize(C,X,Factors\Factors).

/*   solve_factors(Factors,Unknown,Solution) :-
	Solution is a solution of the equation Factor=0 in
	the Unknown for some Factor in the list of Factors.
*/
     solve_factors([Factor|Factors],X,Solution) :-
	solve_equation(Factor=0,X,Solution).
     solve_factors([Factor|Factors],X,Solution) :-
	solve_factors(Factors,X,Solution).

/*  The isolation method  */

     maneuver_sides(1,Lhs = Rhs,Lhs = Rhs) :- !.
     maneuver_sides(2,Lhs = Rhs,Rhs = Lhs) :- !.

     isolate([N|Position],Equation,IsolatedEquation) :- 
	isolax(N,Equation,Equation1), 
	isolate(Position,Equation1,IsolatedEquation).
     isolate([],Equation,Equation).

     /* Axioms for Isolation	*/

isolax(1,-Lhs = Rhs,Lhs = -Rhs).			% Unary minus 

isolax(1,Term1+Term2 = Rhs,Term1 = Rhs-Term2).		% Addition
isolax(2,Term1+Term2 = Rhs,Term2 = Rhs-Term1). 		% Addition

isolax(1,Term1-Term2 = Rhs,Term1 = Rhs+Term2).		% Subtraction
isolax(2,Term1-Term2 = Rhs,Term2 = Term1-Rhs). 		% Subtraction

isolax(1,Term1*Term2 = Rhs,Term1 = Rhs/Term2) :- 	% Multiplication 
   Term2 \== 0.
isolax(2,Term1*Term2 = Rhs,Term2 = Rhs/Term1) :- 	% Multiplication 
   Term1 \== 0.

isolax(1,Term1/Term2 = Rhs,Term1 = Rhs*Term2) :- 	% Division
   Term2 \== 0.
isolax(2,Term1/Term2 = Rhs,Term2 = Term1/Rhs) :- 	% Division
   Rhs \== 0. 

isolax(1,Term1^Term2 = Rhs,Term1 = Rhs^(-Term2)).	% Exponentiation $$$ ^
isolax(2,Term1^Term2 = Rhs,Term2 = log(base(Term1),Rhs)). % Exponentiation

isolax(1,sin(U) = V,U = arcsin(V)).			% Sine
isolax(1,sin(U) = V,U = 180 - arcsin(V)).		% Sine
isolax(1,cos(U) = V,U = arccos(V)).			% Cosine
isolax(1,cos(U) = V,U = -arccos(V)).			% Cosine

/*  The polynomial method	*/

     polynomial(X,X) :- !.
     polynomial(Term,X) :- 
        constant(Term), !.
     polynomial(Term1+Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1-Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1*Term2,X) :- 
        !, polynomial(Term1,X), polynomial(Term2,X).
     polynomial(Term1/Term2,X) :- 
        !, polynomial(Term1,X), constant(Term2).
     polynomial(Term ^ N,X) :- 	
        !, integer(N), N >= 0, polynomial(Term,X).	

/* 
     polynomial_normal_form(Expression,Term,PolyNormalForm) :-
	   PolyNormalForm  is the polynomial normal form of the 
	   Expression, which is a polynomial in Term.
*/
     polynomial_normal_form(Polynomial,X,NormalForm) :-
	polynomial_form(Polynomial,X,PolyForm),
	remove_zero_terms(PolyForm,NormalForm), !.

     polynomial_form(X,X,[(1,1)]).
     polynomial_form(X^N,X,[(1,N)]).
     polynomial_form(Term1+Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
	add_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term1-Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
	subtract_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term1*Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
	multiply_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term^N,X,PolyForm) :- !,
	polynomial_form(Term,X,PolyForm1),
	binomial(PolyForm1,N,PolyForm).
     polynomial_form(Term,X,[(Term,0)]) :-
	free_of(X,Term), !.

   remove_zero_terms([(0,N)|Poly],Poly1) :-
	!, remove_zero_terms(Poly,Poly1).
   remove_zero_terms([(C,N)|Poly],[(C,N)|Poly1]) :-
	C \== 0, !, remove_zero_terms(Poly,Poly1).
   remove_zero_terms([],[]).

   /*  Polynomial manipulation routines		*/

/*  add_polynomials(Poly1,Poly2,Poly) :-
	Poly is the sum of Poly1 and Poly2, where
	Poly1, Poly2 and Poly are all in polynomial form.
*/
   add_polynomials([],Poly,Poly) :- !.
   add_polynomials(Poly,[],Poly) :- !.
   add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Ai,Ni)|Poly]) :-
        Ni > Nj, !, add_polynomials(Poly1,[(Aj,Nj)|Poly2],Poly).
   add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(A,Ni)|Poly]) :-
	Ni =:= Nj, !, A is Ai+Aj, add_polynomials(Poly1,Poly2,Poly).
   add_polynomials([(Ai,Ni)|Poly1],[(Aj,Nj)|Poly2],[(Aj,Nj)|Poly]) :-
	Ni < Nj, !, add_polynomials([(Ai,Ni)|Poly1],Poly2,Poly).

/*  subtract_polynomials(Poly1,Poly2,Poly) :-
	Poly is the difference of Poly1 and Poly2, where
	Poly1, Poly2 and Poly are all in polynomial form.
*/
   subtract_polynomials(Poly1,Poly2,Poly) :-
	multiply_single(Poly2,(-1,0),Poly3),
   	add_polynomials(Poly1,Poly3,Poly), !.

/*  multiply_single(Poly1,Monomial,Poly) :-
	Poly is the product of Poly1 and Monomial, where
	Poly1, and Poly are in polynomial form, and Monomial 
 	has the form (C,N) denoting the monomial C*X^N.
*/

   multiply_single([(C1,N1)|Poly1],(C,N),[(C2,N2)|Poly]) :-
	C2 is C1*C, N2 is N1+N, multiply_single(Poly1,(C,N),Poly).
   multiply_single([],Factor,[]).

/*  multiply_polynomials(Poly1,Poly2,Poly) :-
	Poly  is the product of Poly1 and Poly2, where
	Poly1, Poly2 and Poly are all in polynomial form.
*/
   multiply_polynomials([(C,N)|Poly1],Poly2,Poly) :-
	multiply_single(Poly2,(C,N),Poly3),
	multiply_polynomials(Poly1,Poly2,Poly4),
   	add_polynomials(Poly3,Poly4,Poly).
   multiply_polynomials([],P,[]).

   binomial(Poly,1,Poly).
	
/*   solve_polynomial_equation(Equation,Unknown,Solution) :-
	Solution  is a solution to the polynomial Equation  
	in the unknown Unknown.
*/

   solve_polynomial_equation(PolyEquation,X,X = -B/A) :-
   	linear(PolyEquation), !, 
	pad(PolyEquation,[(A,1),(B,0)]). 
   solve_polynomial_equation(PolyEquation,X,Solution) :-
	quadratic(PolyEquation), !,
	pad(PolyEquation,[(A,2),(B,1),(C,0)]),
	discriminant(A,B,C,Discriminant),
	root(X,A,B,C,Discriminant,Solution).

	discriminant(A,B,C,D) :- D is B*B - 4*A*C.

	root(X,A,B,C,0,X= -B/(2*A)).
	root(X,A,B,C,D,X= (-B+sqrt(D))/(2*A)) :- D > 0.
	root(X,A,B,C,D,X= (-B-sqrt(D))/(2*A)) :- D > 0.

   pad([(C,N)|Poly],[(C,N)|Poly1]) :-
	!, pad(Poly,Poly1).
   pad(Poly,[(0,N)|Poly1]) :-
	pad(Poly,Poly1).
   pad([],[]).

   linear([(Coeff,1)|Poly]).	

   quadratic([(Coeff,2)|Poly]).

/*  The homogenization method	

   homogenize(Equation,X,Equation1,X1) :-
	The Equation in X is transformed to the polynomial
	Equation1 in X1 where X1 contains X.
*/				
     homogenize(Equation,X,Equation1,X1) :-
	offenders(Equation,X,Offenders),
        reduced_term(X,Offenders,Type,X1),
        rewrite(Offenders,Type,X1,Substitutions),
        substitute(Equation,Substitutions,Equation1).

     /*  offenders(Equation,Unknown,Offenders) 
	Offenders is the set of offenders of the equation in the Unknown  */

     offenders(Equation,X,Offenders) :-
        parse(Equation,X,Offenders1\[]),
        remove_duplicates(Offenders1,Offenders),
	multiple(Offenders).

     reduced_term(X,Offenders,Type,X1) :-
	classify(Offenders,X,Type),
	candidate(Type,Offenders,X,X1).

    /*  Heuristics for exponential equations	*/
 
    classify(Offenders,X,exponential) :-
	exponential_offenders(Offenders,X).

     exponential_offenders([A^B|Offs],X) :-
	free_of(X,A), subterm(X,B), exponential_offenders(Offs,X).
     exponential_offenders([],X).

     candidate(exponential,Offenders,X,A^X) :-
	base(Offenders,A), polynomial_exponents(Offenders,X).

     base([A^B|Offs],A) :- base(Offs,A).
     base([],A).

     polynomial_exponents([A^B|Offs],X) :-
	polynomial(B,X), polynomial_exponents(Offs,X).
     polynomial_exponents([],X).

    /*   Parsing the equation and making substitutions	   */

   /*  parse(Expression,Term,Offenders)
	Expression is traversed to produce the set of Offenders in Term,
	that is the non-algebraic subterms of Expression containing Term  */

     parse(A+B,X,L1\L2) :-
	!, parse(A,X,L1\L3), parse(B,X,L3\L2).     
     parse(A*B,X,L1\L2) :-
	!, parse(A,X,L1\L3), parse(B,X,L3\L2).     
     parse(A-B,X,L1\L2) :-
	!, parse(A,X,L1\L3), parse(B,X,L3\L2).     
     parse(A=B,X,L1\L2) :-
	!, parse(A,X,L1\L3), parse(B,X,L3\L2).     
     parse(A^B,X,L) :-
	integer(B), !, parse(A,X,L).
     parse(A,X,L\L) :-
	free_of(X,A), !.
     parse(A,X,[A|L]\L) :-
	subterm(X,A), !.

/*     substitute(Equation,Substitutions,Equation1) :-
	Equation1 is the result of applying the list of 
	Substitutions to Equation.
   */
     substitute(A+B,Subs,NewA+NewB) :-
	!, substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A*B,Subs,NewA*NewB) :-
	!, substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A-B,Subs,NewA-NewB) :-
	!, substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A=B,Subs,NewA=NewB) :-
	!, substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A^B,Subs,NewA^B) :-
	integer(B), !, substitute(A,Subs,NewA).
     substitute(A,Subs,B) :-
	member(A=B,Subs), !.
     substitute(A,Subs,A).

     /*  Finding homogenization rewrite rules	*/
 
     rewrite([Off|Offs],Type,X1,[Off=Term|Rewrites]) :-
	homogenize_axiom(Type,Off,X1,Term),
	rewrite(Offs,Type,X1,Rewrites).
     rewrite([],Type,X,[]).

     /*  Homogenization axioms	*/

     homogenize_axiom(exponential,A^(N*X),A^X,(A^X)^N).
     homogenize_axiom(exponential,A^(-X),A^X,1/(A^X)).
     homogenize_axiom(exponential,A^(X+B),A^X,A^B*A^X).

/*	Utilities	*/

subterm(Term,Term).
subterm(Sub,Term) :-
	compound(Term), functor(Term,F,N), subterm(N,Sub,Term).

subterm(N,Sub,Term) :-
   arg(N,Term,Arg), subterm(Sub,Arg).
subterm(N,Sub,Term) :-
	N > 0,
	N1 is N - 1,
	subterm(N1,Sub,Term).

position(Term,Term,[]) :- !.
position(Sub,Term,Path) :-
        compound(Term), functor(Term,F,N), position(N,Sub,Term,Path), !.

position(N,Sub,Term,[N|Path]) :-
   arg(N,Term,Arg), position(Sub,Arg,Path).
position(N,Sub,Term,Path) :- 
   N > 1, N1 is N-1, position(N1,Sub,Term,Path).


     free_of(Subterm,Term) :-
        occurrence(Subterm,Term,N), !, N=0.

     single_occurrence(Subterm,Term) :-      
        occurrence(Subterm,Term,N), !, N=1.

  occurrence(Term,Term,1) :- !.
  occurrence(Sub,Term,N) :-
	compound(Term), !, functor(Term,F,M), occurrence(M,Sub,Term,0,N).
  occurrence(Sub,Term,0) :- Term \== Sub.

  occurrence(M,Sub,Term,N1,N2) :-
	M > 0, !, arg(M,Term,Arg), occurrence(Sub,Arg,N), N3 is N+N1,
		M1 is M-1, occurrence(M1,Sub,Term,N3,N2).
  occurrence(0,Sub,Term,N,N).

  multiple([X1,X2|Xs]).

remove_duplicates(Xs,Ys) :- no_doubles(Xs,Ys).

no_doubles([X|Xs],Ys) :-
	member(X,Xs), no_doubles(Xs,Ys).
no_doubles([X|Xs],[X|Ys]) :-
	nonmember(X,Xs), no_doubles(Xs,Ys).
no_doubles([],[]).

     nonmember(X,[Y|Ys]) :- X \== Y, nonmember(X,Ys).
     nonmember(X,[]).

	%compound(Term) :- functor(Term,F,N),N > 0,!.

%  Testing and data

test_press(X,Y) :- equation(X,E,U), solve_equation(E,U,Y).

equation(1,x^2-3*x+2=0,x).

equation(2,cos(x)*(1-2*sin(x))=0,x).

equation(3,2^(2*x) - 5*2^(x+1) + 16 = 0,x).

%  Program 23.1  A program for solving equations

%  Program 22.2     /* Testing and data	*/

/*
   compile(Tokens,ObjectCode) :-
	ObjectCode is the result of compilation of a list of tokens 
	representing a PL program.
*/
	:- op(40,xfx,\).
	:- op(800,fx,#).
	:- op(780,xf,^).

	compile(Tokens,ObjectCode) :-
	   parse(Tokens,Structure),
	   encode(Structure,Dictionary,Code),
	   assemble(Code,Dictionary,ObjectCode).

/*    The parser

   parse(Tokens,Structure) :-
     Structure represents the successfully parsed list of Tokens.
*/

parse(Source,Structure) :-
	pl_program(Source\[],Structure).

pl_program(S) --> [program], identifier(X), [';'], statement(S).    
                           
statement((S;Ss)) --> 
    [begin], statement(S), rest_statements(Ss).
statement(assign(X,V)) --> 
    identifier(X), [':='], expression(V).
statement(if(T,S1,S2)) -->
    [if], test(T), [then], statement(S1), [else], statement(S2).
statement(while(T,S)) --> 
    [while], test(T), [do], statement(S).
statement(read(X)) --> 
    [read], identifier(X).
statement(write(X)) --> 
    [write], expression(X).

rest_statements((S;Ss)) --> [';'], statement(S), rest_statements(Ss).  
rest_statements(void) --> [end].

expression(X) --> pl_constant(X).
expression(expr(Op,X,Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).

arithmetic_op('+') --> ['+'].
arithmetic_op('-') --> ['-'].
arithmetic_op('*') --> ['*'].
arithmetic_op('/') --> ['/'].

pl_constant(name(X)) --> identifier(X).
pl_constant(number(X)) --> pl_integer(X).

	identifier(X) --> [X], {atom(X)}.
	pl_integer(X) --> [X], {integer(X)}.

test(compare(Op,X,Y)) --> expression(X), comparison_op(Op), expression(Y).

comparison_op('=') --> ['='].
comparison_op('/=') --> ['/='].
comparison_op('>') --> ['>'].
comparison_op('<') --> ['<'].
comparison_op('>=') --> ['>='].
comparison_op('=<') --> ['=<'].

/*   The code generator

   encode(Structure,Dictionary,RelocatableCode) :-
	RelocatableCode is generated from the parsed Structure 
	building a Dictionary associating variables with addresses.
*/
   encode((X;Xs),D,(Y;Ys)) :-  
       encode(X,D,Y), encode(Xs,D,Ys).
   encode(void,D,no_op).
   encode(assign(Name,E),D,(Code; instr(store,Address))) :-
      lookup(Name,D,Address), encode_expression(E,D,Code).
   encode(if(Test,Then,Else),D,
       (TestCode; ThenCode; instr(jump,L2); label(L1); ElseCode; label(L2))) :-
      encode_test(Test,L1,D,TestCode),
      encode(Then,D,ThenCode),
      encode(Else,D,ElseCode).
   encode(while(Test,Do),D, 
	(label(L1); TestCode; DoCode; instr(jump,L1); label(L2))) :-
    	encode_test(Test,L2,D,TestCode), encode(Do,D,DoCode).
   encode(read(X),D,instr(read,Address)) :-
   	lookup(X,D,Address).
   encode(write(E),D,(Code; instr(write,0))) :-
    	encode_expression(E,D,Code).

/*   encode_expression(Expression,Dictionary,Code) :-
	Code corresponds to an arithmetic Expression.
*/
    encode_expression(number(C),D,instr(loadc,C)).
    encode_expression(name(X),D,instr(load,Address)) :- 
       lookup(X,D,Address).
    encode_expression(expr(Op,E1,E2),D,(Load;Instruction)) :-
       single_instruction(Op,E2,D,Instruction),
       encode_expression(E1,D,Load).
    encode_expression(expr(Op,E1,E2),D,Code) :-
       not single_instruction(Op,E2,D,Instruction),
       single_operation(Op,E1,D,E2Code,Code),
       encode_expression(E2,D,E2Code).

    single_instruction(Op,number(C),D,instr(OpCode,C)) :- 
        literal_operation(Op,OpCode).
    single_instruction(Op,name(X),D,instr(OpCode,A)) :- 
        memory_operation(Op,OpCode), lookup(X,D,A).

    single_operation(Op,E,D,Code,(Code;Instruction)) :-
	commutative(Op), single_instruction(Op,E,D,Instruction).
    single_operation(Op,E,D,Code,
   		(Code;instr(store,Address);Load;instr(OpCode,Address))) :-
	not commutative(Op), 
	lookup('$temp',D,Address),
	encode_expression(E,D,Load),
	op_code(E,Op,OpCode).

     op_code(number(C),Op,OpCode) :-  literal_operation(Op,OpCode).
     op_code(name(X),Op,OpCode) :-  memory_operation(Op,OpCode).

     literal_operation('+',addc).	memory_operation('+',add).
     literal_operation('-',subc).	memory_operation('-',sub).
     literal_operation('*',mulc).	memory_operation('*',mul).
     literal_operation('/',divc).	memory_operation('/',div).

     commutative('+').		commutative('*').

   encode_test(compare(Op,E1,E2),Label,D,(Code; instr(OpCode,Label))) :-
   	comparison_opcode(Op,OpCode),
   	encode_expression(expr('-',E1,E2),D,Code).
	
   comparison_opcode('=',jumpne).   	comparison_opcode('/=',jumpeq).
   comparison_opcode('>',jumple).	comparison_opcode('>=',jumplt).
   comparison_opcode('<',jumpge).	comparison_opcode('=<',jumpgt).

	lookup(Key,dict(Key,X,Left,Right),Value) :-
		!, X = Value.
	lookup(Key,dict(Key1,X,Left,Right),Value) :-
		Key < Key1 , lookup(Key,Left,Value).
	lookup(Key,dict(Key1,X,Left,Right),Value) :-
		Key > Key1, lookup(Key,Right,Value).

/*  The assembler

   assemble(Code,Dictionary,TidyCode) :-
	TidyCode is the result of assembling Code removing
	no_ops and labels, and filling in the Dictionary.
*/

   assemble(Code,Dictionary,TidyCode) :-
      tidy_and_count(Code,1,N,TidyCode\(instr(halt,0);block(L))),
      N1 is N + 1,
      allocate(Dictionary,N1,N2),
      L is N2 - N1, !.

   tidy_and_count((Code1;Code2),M,N,TCode1\TCode2) :-
      tidy_and_count(Code1,M,M1,TCode1\Rest), 
      tidy_and_count(Code2,M1,N,Rest\TCode2).
   tidy_and_count(instr(X,Y),N,N1,(instr(X,Y);Code)\Code) :- 
      N1 is N + 1.
   tidy_and_count(label(N),N,N,Code\Code).
   tidy_and_count(no_op,N,N,Code\Code).

     allocate(void,N,N).
     allocate(dic(Name,N1,Before,After),N0,N) :-
        allocate(Before,N0,N1),
        N2 is N1 + 1,
        allocate(After,N2,N).

%  Program 24.1:  A compiler from PL to machine language

test_compiler(X,Y) :-
    program(X,P), compile(P,Y).

program(test1,[program,test1,';',begin,write,x,'+',y,'-',z,'/',2,end]).

program(test2,[program,test2,';',
	begin,if,a,'>',b,then,max,':=',a,else,max,':=',b,end]).

program(factorial,
    [program,factorial,';'
    ,begin
         ,read,value,';'
         ,count,':=',1,';'
         ,result,':=',1,';'
         ,while,count,'<',value,do
              ,begin
                   ,count,':=',count,'+',1,';'
                   ,result,':=',result,'*',count
              ,end,';'
         ,write,result
    ,end]).

%   Program 24.2   Test data
/* 
	natural_number(X) :- X is a natural number.
*/

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.1:  Defining the natural numbers
/*
	X lesseq Y :-  X and Y are natural numbers,
	  		  such that X is less than or equal to Y.

We use lesseq to represent the operator rather than cause problems
with an error message from Prolog about redefining an operator!
*/

	:- op(40,xfx, lesseq).
	0 lesseq X :- natural_number(X).
	s(X) lesseq s(Y) :- X lesseq Y.

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.2: The less than or equal relation
/*
	plus(X,Y,Z) :-
		X, Y and Z are natural numbers
		such that Z is the sum of X and Y.
*/

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.3: Addition
/*
	times(X,Y,Z) :-
		X, Y and Z are natural numbers
		such that Z is the product of X and Y
*/

	times(0,X,0).
	times(s(X),Y,Z) :- times(X,Y,XY), plus(XY,Y,Z).

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.4: Multiplication as repeated addition
/*
	exp(N,X,Y) :-
			N, X and Y are natural numbers
			such that Y equals X raised to the power N.
*/

	exp(s(N),0,0).
	exp(0,s(X),s(0)).
	exp(s(N),X,Y) :- exp(N,X,Z), times(Z,X,Y).

	times(0,Y,0).
	times(s(X),Y,Z) :- times(X,Y,XY), plus(XY,Y,Z).

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.5: Exponentiation as repeated multiplication
/*
	factorial(N,F) :- F equals N factorial.
*/

	factorial(0,s(0)).
	factorial(s(N),F) :- factorial(N,F1), times(s(N),F1,F).

	times(0,X,0).
	times(s(X),Y,Z) :- times(X,Y,XY), plus(XY,Y,Z).

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.6: Computing factorials
/*
	minimum(N1,N2,Min) :- 
		The minimum of natural numbers N1 and N2 is Min.

We use lesseq to represent the operator rather than cause problems
with an error message from Prolog about redefining an operator!
*/
						
	:- op(40,xfx, lesseq).

	minimum(N1,N2,N1) :- N1 lesseq N2.
	minimum(N1,N2,N2) :- N2 lesseq N1.

	0 lesseq X :- natural_number(X).
	s(X) lesseq s(Y) :- X lesseq Y.

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.7: The minimum of two numbers

/*
	mod(X,Y,Z) :-
		Z is the remainder of the integer division of X by Y.
*/

	mod(X,Y,Z) :- Z < Y, times(Y,Q,QY), plus(QY,Z,X).

	times(0,X,0).
	times(s(X),Y,Z) :- times(X,Y,XY), plus(XY,Y,Z).

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.8a: A nonrecursive definition of modulus
/*
	mod(X,Y,Z) :-
		Z is the remainder of the integer division of X and Y.
*/

	mod(X,Y,X):- X < Y.
	mod(X,Y,Z) :- plus(X1,Y,X), mod(X1,Y,Z).

	plus(0,X,X) :- natural_number(X).
	plus(s(X),Y,s(Z)):- plus(X,Y,Z).

	natural_number(0).
	natural_number(s(X)) :- natural_number(X).

%	Program 3.8b: A recursive definition of modulus
/*
	ackermann(X,Y,A) :-
		A is the value of Ackermann's function for
		the natural numbers X and Y.
*/

	ackermann(0,N,s(N)).
	ackermann(s(M),0,Val) :- ackermann(M,s(0),Val).
	ackermann(s(M),s(N),Val) :-
		ackermann(s(M),N,Val1), ackermann(M,Val1,Val).

%	Program 3.9: Ackermann's function

/*
	gcd(X,Y,Z) :- Z is the greatest common divisor of the 
			      natural numbers X and Y.
*/

	gcd(X,Y,Gcd) :- mod(X,Y,Z), gcd(Y,Z,Gcd).
	gcd(X,0,X) :- X > 0.

%	Program 3.10: The Euclidean algorithm
/*
	list(Xs) :- Xs is a list.
*/

	list([]).
	list([X|Xs]) :- list(Xs).

%	Program 3.11: Defining a list
/*
	member(Element,List) :- Element is an element of the list List
*/

	member(X,[X|Xs]).
	member(X,[Y|Ys]) :- member(X,Ys).

%	Program 3.12: Membership of a list
/*
	prefix(Prefix,List) :- Prefix is a prefix of List.
*/

	prefix([],Ys).
	prefix([X|Xs],[X|Ys]) :- prefix(Xs,Ys).

/*
	suffix(Suffix,List) :- Suffix is a suffix of List.
*/

	suffix(Xs,Xs).
	suffix(Xs,[Y|Ys]) :- suffix(Xs,Ys).

%	Program 3.13: Prefixes and suffixes of a list
/*
	sublist(Sub,List) :- Sub is a sublist of List.
*/

	% a: Suffix of a prefix
	sublist(Xs,Ys) :- prefix(Ps,Ys), suffix(Xs,Ps).

	% b: Prefix of a suffix
	sublist(Xs,Ys) :- prefix(Xs,Ss), suffix(Ss,Ys).

	% c: Recursive definition of a sublist
	sublist(Xs,Ys) :- prefix(Xs,Ys).
	sublist(Xs,[Y|Ys]) :- sublist(Xs,Ys).

	% d: Prefix of a suffix, using append
	sublist(Xs,AsXsBs) :-
		append(As,XsBs,AsXsBs), append(Xs,Bs,XsBs).

	% e: Suffix of a prefix, using append
	sublist(Xs,AsXsBs) :-
		append(AsXs,Bs,AsXsBs), append(As,Xs,AsXs).

%	Program 3.14: Determining sublists of lists
/*
	append(Xs,Ys,XsYs) :-
		XsYs is the result of concatening the lists Xs and Ys.
*/

	append([],Ys,Ys).
	append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

%	Program 3.15: Appending two lists
/*
	reverse(List,Tsil):-
		Tsil is the result of reversing the list List.
*/
	% a: Naive reverse
	reverse([],[]).
	reverse([X|Xs],Zs) :- reverse(Xs,Ys), append(Ys,[X],Zs).

	% b: Reverse-accumulate

	reverse(Xs,Ys):- reverse(Xs,[],Ys).
	reverse([X|Xs],Acc,Ys) :- reverse(Xs,[X|Acc],Ys).
	reverse([],Ys,Ys).

%	Program 3.16: Reversing a list
/*
	length(Xs,N) :- The list Xs has N elements.
*/

	length([],0).
	length([X|Xs],s(N)) :- length(Xs,N).

%	Program 3.17: Determining the length of a list
/*
	delete(List,X,HasNoXs) :-
		The list HasNoXs is the result of removing all
		occurrences of X from the list List.
*/

	delete([X|Xs],X,Ys) :- delete(Xs,X,Ys).
	delete([X|Xs],Z,[X|Ys]) :- X \== Z, delete(Xs,Z,Ys).
	delete([],X,[]).

%	Program 3.18: Deleting all occurrences of an element from a list
/*		
	select(X,HasXs,OneLessXs) :- 
		The list OneLessXs is the result of removing one 
		occurrence of X from the list HasXs.
*/

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

%	Program 3.19: Selecting an element from a list
/*
	sort(Xs,Ys) :- 
		The list Ys is an ordered permutation of the list Xs.
*/

	sort([X|Xs],Ys) :- sort(Xs,Zs), insert(X,Zs,Ys).
	sort([],[]).

	insert(X,[],X).
	insert(X,[Y|Ys],[Y|Zs]) :- X > Y, insert(X,Ys,Zs).
	insert(X,[Y|Ys],[X,Y|Ys]) :- X =< Y.

%	Program 3.21: Insertion sort
/*
	sort(Xs,Ys) :- 
		The list Ys is an ordered permutation of the list Xs.
*/
	quicksort([X|Xs],Ys) :-
		partition(Xs,X,Littles,Bigs),
		quicksort(Littles,Ls),
		quicksort(Bigs,Bs),
		append(Ls,[X|Bs],Ys).
	quicksort([],[]).

	partition([X|Xs],Y,[X|Ls],Bs) :- X =< Y, partition(Xs,Y,Ls,Bs).
	partition([X|Xs],Y,Ls,[X|Bs]) :- X >  Y, partition(Xs,Y,Ls,Bs).
	partition([],Y,[],[]).

	append([],Ys,Ys).
	append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

%	Program 3.22: Quicksort
/*
	binary_tree(Tree) :- Tree is a binary tree.
*/
	binary_tree(void).
	binary_tree(tree(Element,Left,Right)) :-
		binary_tree(Left), binary_tree(Right).

%	Program 3.23: Defining binary trees
/*
	tree_member(Element,Tree):-
		Element is an element of the binary tree Tree
*/
	tree_member(X,tree(X,Left,Right)).
	tree_member(X,tree(Y,Left,Right)) :- tree_member(X,Left).
	tree_member(X,tree(Y,Left,Right)) :- tree_member(X,Right).

%	Program 3.24: Testing tree membership

/*
	isotree(Tree1,Tree2) :- 
		Tree1 and Tree2 are isomorphic binary trees
*/
	isotree(void,void).
	isotree(tree(X,Left1,Right1),tree(X,Left2,Right2)) :- 
		isotree(Left1,Left2), isotree(Right1,Right2).
	isotree(tree(X,Left1,Right1),tree(X,Left2,Right2)) :- 
		isotree(Left1,Right2), isotree(Right1,Left2).
	
%	Program 3.25: Determining when trees are isomorphic
/*		
	substitute(X,Y,TreeX,TreeY) :- 
		The binary tree TreeY is the result of replacing all 
		occurrences of X in the binary tree TreeX by Y.
*/

	substitute(X,Y,void,void).
	substitute(X,Y,tree(Leaf,Left,Right),tree(Leaf1,Left1,Right1)) :-
		replace(X,Y,Leaf,Leaf1),
		substitute(X,Y,Left,Left1), 
		substitute(X,Y,Right,Right1).

	replace(X,Y,X,Y).
	replace(X,Y,Z,Z) :- X \== Z.

%	Program 3.26: Substituting for a term in a tree
/*
	preorder(Tree,Pre) :- 
		Pre is a preorder traversal of the binary tree Tree.
*/
	preorder(tree(X,L,R),Xs) :-
		preorder(L,Ls), preorder(R,Rs), append([X|Ls],Rs,Xs).
	preorder(void,[]).

/*
	inorder(Tree,In) :- 
		In is an inorder traversal of the binary tree Tree.
*/
	inorder(tree(X,L,R),Xs) :-
		inorder(L,Ls), inorder(R,Rs), append(Ls,[X|Rs],Xs).
	inorder(void,[]).
/*
	postorder(Tree,Post) :- 
		Post is a postorder traversal of the binary tree Tree.
*/
	postorder(tree(X,L,R),Xs) :-
		postorder(L,Ls), 
		postorder(R,Rs),
		append(Rs,[X],Rs1), 
		append(Ls,Rs1,Xs).
	postorder(void,[]).

%	Program 3.27: Traversals of a binary tree
/*
	heapify(Tree,Heap) :-
	  The elements of the complete binary tree Tree have been adjusted
	  to form the binary tree Heap, which has the same shape as Tree 
	  and satisfies the heap property that the value of each parent node
	  is greater than or equal to the values of its children.
*/

heapify(void,void).
heapify(tree(X,L,R),Heap) :-
   heapify(L,HeapL), heapify(R,HeapR), adjust(X,HeapL,HeapR,Heap).

adjust(X,HeapL,HeapR,tree(X,HeapL,HeapR)) :-
    greater(X,HeapL), greater(X,HeapR).
adjust(X,tree(X1,L,R),HeapR,tree(X1,HeapL,HeapR)) :-
    X < X1, greater(X1,HeapR), adjust(X,L,R,HeapL).
adjust(X,HeapL,tree(X1,L,R),tree(X1,HeapL,HeapR)) :-
    X < X1, greater(X1,HeapL), adjust(X,L,R,HeapR).

    greater(X,void).
    greater(X,tree(X1,L,R)) :- X >= X1.
    
%   Program 3.28    Adjusting a binary tree to satisfy the heap property
/*
	polynomial(Expression,X) :- 
		Expression is a polynomial in X.
*/
	polynomial(X,X).
	polynomial(Term,X) :- constant(Term).
	polynomial(Term1+Term2,X) :-
		polynomial(Term1,X), polynomial(Term2,X).
	polynomial(Term1-Term2,X) :-
		polynomial(Term1,X), polynomial(Term2,X).
	polynomial(Term1*Term2,X) :-
		polynomial(Term1,X), polynomial(Term2,X).
	polynomial(Term1/Term2,X) :-
		polynomial(Term1,X), constant(Term2).
	polynomial(Term ^ N,X) :-
		natural_number(N), polynomial(Term,X).

%	Program 3.29: Recognizing polynomials
/*
	derivative(Expression,X,DifferentiatedExpression) :-
		DifferentiatedExpression is the derivative of
			Expression with respect to X.
*/

	derivative(X,X,s(0)).
	derivative(X ^ s(N),X,s(N) * X ^ N).
	derivative(sin(X),X,cos(X)).
	derivative(cos(X),X,-sin(X)).
	derivative(e ^ X,X,e ^ X).
	derivative(log(X),X,1/X).

	derivative(F+G,X,DF+DG) :-
		derivative(F,X,DF), derivative(G,X,DG).
	derivative(F-G,X,DF-DG) :-
		derivative(F,X,DF), derivative(G,X,DG).
	derivative(F*G,X,F*DG + DF*G) :-
		derivative(F,X,DF), derivative(G,X,DG).
	derivative(1/F,X,-DF/(F*F)) :-
		derivative(F,X,DF).
	derivative(F/G,X,(G*DF-F*DG)/(G*G)) :-
		derivative(F,X,DF), derivative(G,X,DG).

%	Program 3.30: Derivative rules
/*
	hanoi(N,A,B,C,Moves) :- 
		Moves is a sequence of moves for solving the Towers of
		Hanoi puzzle with N disks and three pegs, A, B and C.
*/

:- op(40,xfx,[to]).

	hanoi(s(0),A,B,C,[A to B]).
	hanoi(s(N),A,B,C,Moves) :-
		hanoi(N,A,C,B,Ms1),
		hanoi(N,C,B,A,Ms2),
		append(Ms1,[A to B|Ms2],Moves).

%	Program 3.31: Towers of Hanoi
/*
	satisfiable(Formula) :- 
		There is a true instance of the Boolean formula Formula.	     
*/
	:- op(950, xfx, [&]).
	:- op(950, xfx, ['l']).
	:- op(900,  fx, [~]).


	satisfiable(true).
	satisfiable(X & Y) :- satisfiable(X), satisfiable(Y).
	satisfiable(X '|' Y) :- satisfiable(X).
	satisfiable(X '|' Y) :- satisfiable(Y).
	satisfiable((~ X)) :-  invalid(Y).
/*
	invalid(Formula) :-	
		There is a false instance of the Boolean formula Formula.	  
*/
	invalid(false).
	invalid(X '|' Y) :- invalid(X), invalid(Y).
	invalid(X & Y) :- invalid(X).
	invalid(X & Y) :- invalid(Y).
	invalid((~ X)) :- satisfiable(X).

%	Program 3.32: Satisfiability of Boolean formulae
	parent(terach,abraham).		parent(abraham,isaac).
	parent(isaac,jacob).		parent(jacob,benjamin).

	ancestor(X,Y) :- parent(X,Y).
	ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

%  Program 5.1    Yet another family example
	parent(terach,abraham).		parent(abraham,isaac).
	parent(isaac,jacob).		parent(jacob,benjamin).

	ancestor(X,Y) :- parent(X,Y).
	ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

%  Program 7.1    Yet another family example
/*
   merge(Xs,Ys,Zs) :- 
	Zs is an ordered list of integers obtained from 
	merging the ordered lists of integers Xs and Ys.
*/
     merge([X|Xs],[Y|Ys],[X|Zs]) :-
	X < Y, merge(Xs,[Y|Ys],Zs).
     merge([X|Xs],[Y|Ys],[X,X|Zs]) :-
	X =:= Y, merge(Xs,Ys,Zs).
     merge([X|Xs],[Y|Ys],[Y|Zs]) :-
	X > Y, merge([X|Xs],Ys,Zs).
     merge([],[X|Xs],[X|Xs]).
     merge(Xs,[],Xs).

%  Program 7.2    Merging ordered lists
/*
   member_check(X,Xs) :- X is a member of the list Xs.
*/
     member_check(X,[X|Xs]).
     member_check(X,[Y|Ys]) :- X \== Y, member_check(X,Ys).

%  Program 7.3    Checking for list membership
/*
   select_first(X,Xs,Ys) :- 
	Ys is the list obtained by removing the 
	first occurrence of X from the list Xs.
*/
     select_first(X,[X|Xs],Xs).
     select_first(X,[Y|Ys],[Y|Zs]) :- X \== Y, select_first(X,Ys,Zs).

%  Program 7.4    Selecting the first occurrence of an element from a list
/*
   nonmember(X,Xs) :- X is not a member of the list Xs.
*/
     nonmember(X,[Y|Ys]) :- X \== Y, nonmember(X,Ys).
     nonmember(X,[]).

%  Program 7.5    Nonmembership of a list
/*
   members(Xs,Ys) :- Each element of the list Xs is an element of the list Ys.
*/
     members([X|Xs],Ys) :- member(X,Ys), members(Xs,Ys).
     members([],Ys).

%  Program 7.6    Testing for a subset
/*
   selects(Xs,Ys) :- The list Xs is a subset of the list Ys.
*/
     selects([X|Xs],Ys) :- select(X,Ys,Ys1), selects(Xs,Ys1).
     selects([],Ys).

	select(X,[X|Xs],Xs).
	select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

%  Program 7.7    Testing for a subset
/*
     translate(Words,Mots) :- 
	Mots is a list of French words that is the 
	translation of the list of English words Words.
*/
     translate([Word|Words],[Mot|Mots]) :- 
        dict(Word,Mot), translate(Words,Mots).
     translate([],[]).

     dict(the,le).                 dict(dog,chien).         
     dict(chases,chasse).          dict(cat,chat).

%  Program 7.8   Translating word for word
/*
	no_doubles(Xs,Ys) :-
		Ys is the list obtained by removing 
		duplicate elements from the list Xs.
*/

no_doubles([X|Xs],Ys) :-
	member(X,Xs), no_doubles(Xs,Ys).
no_doubles([X|Xs],[X|Ys]) :-
	nonmember(X,Xs), no_doubles(Xs,Ys).
no_doubles([],[]).

     nonmember(X,[Y|Ys]) :- X \== Y, nonmember(X,Ys).
     nonmember(X,[]).

%  Program 7.9   Removing duplicates from a list
/*
     nd_reverse(Xs,Ys) :- 
	Ys is the reversal of the list obtained by 
	removing duplicate elements from the list Xs.
*/
     nd_reverse(Xs,Ys) :- nd_reverse(Xs,[],Ys).

     nd_reverse([X|Xs],Revs,Ys) :-
	member(X,Revs), nd_reverse(Xs,Revs,Ys).
     nd_reverse([X|Xs],Revs,Ys) :-
	nonmember(X,Revs), nd_reverse(Xs,[X|Revs],Ys).
     nd_reverse([],Ys,Ys).

     nonmember(X,[Y|Ys]) :- X \== Y, nonmember(X,Ys).
     nonmember(X,[]).

%  Program 7.10   Reversing with no duplicates
/*
   greatest_common_divisor(X,Y,Z) :- 
	Z is the greatest common divisor of the integers X and Y.
*/
     greatest_common_divisor(I,0,I).
     greatest_common_divisor(I,J,Gcd) :-
          J > 0, R is I mod J, greatest_common_divisor(J,R,Gcd).

%  Program 8.1    Computing the greatest common divisor of two integers
/*
   factorial(N,F) :- F is the integer N factorial.
*/ 
     factorial(N,F) :-
        N > 0, N1 is N-1, factorial(N1,F1), F is N*F1.
     factorial(0,1).
%
%  Program 8.2   Computing the factorial of a number
/*
   factorial(N,F) :- F is the integer N factorial.
*/ 
     factorial(N,F) :- factorial(0,N,1,F).

     factorial(I,N,T,F) :-
        I < N, I1 is I+1, T1 is T*I1, factorial(I1,N,T1,F).
     factorial(N,N,F,F). 

%  Program 8.3   An iterative factorial
/*
   factorial(N,F) :- F is the integer N factorial.
*/ 
     factorial(N,F) :- factorial(N,1,F).

     factorial(N,T,F) :-
        N > 0, T1 is T*N, N1 is N-1, factorial(N1,T1,F).
     factorial(0,F,F). 

%  Program 8.4   Another iterative factorial
/*  
  between(I,J,K) :- K is an integer between the integers I and J inclusive.
*/
     between(I,J,I) :- I =< J.
     between(I,J,K) :- I < J, I1 is I + 1, between(I1,J,K).

%  Program 8.5   Generating a range of integers
/*
   sumlist(Is,Sum) :- Sum is the sum of the list of integers Is.
*/
     sumlist([I|Is],Sum) :- sumlist(Is,IsSum), Sum is I+IsSum.
     sumlist([],0).

%  Program 8.6a   Summing a list of integers
/*
   sumlist(Is,Sum) :- Sum is the sum of the list of integers Is.
*/
     sumlist(Is,Sum) :- sumlist(Is,0,Sum).

     sumlist([I|Is],Temp,Sum) :- 
	Temp1 is Temp+I, sumlist(Is,Temp1,Sum).
     sumlist([],Sum,Sum).

%  Program 8.6b   Iterative version of summing a list of integers
%					using an accumulator
/*
   inner_product(Xs,Ys,Value) :- 
	Value is the inner product of the vectors
	represented by the lists of integers Xs and Ys.
*/
     inner_product([X|Xs],[Y|Ys],IP) :-
	inner_product(Xs,Ys,IP1), IP is X*Y + IP1.
     inner_product([],[],0).

%  Program 8.7a   Computing inner products of vectors
/*
   inner_product(Xs,Ys,Value) :- 
	Value is the inner product of the vectors
	represented by the lists of integers Xs and Ys.
*/
     inner_product(Xs,Ys,IP) :- inner_product(Xs,Ys,0,IP).
 
     inner_product([X|Xs],[Y|Ys],Temp,IP) :-
         Temp1 is X*Y+Temp, inner_product(Xs,Ys,Temp1,IP).
     inner_product([],[],IP,IP).

%  Program 8.7b   Computing inner products of vectors iteratively
/*
   area(Chain,Area) :- 
	Area is the area of the polygon enclosed by the list of points
	Chain, where the coordinates of each point are represented by 
	a pair (X,Y) of integers.
*/
     area([Tuple],0).
     area([(X1,Y1),(X2,Y2)|XYs],Area) :-
        area([(X2,Y2)|XYs],Area1), 
        Area is (X1*Y2-Y1*X2)/2 + Area1.

%  Program 8.8  Computing the area of polygons

/*
   maxlist(Xs,N) :- N is the maximum of the list of integers Xs.
*/
     maxlist([X|Xs],M) :- maxlist(Xs,X,M).

     maxlist([X|Xs],Y,M) :- maximum(X,Y,Y1), maxlist(Xs,Y1,M).
     maxlist([],M,M).

	maximum(X,Y,Y) :- X =< Y.
	maximum(X,Y,X) :- X > Y.

%  Program 8.9    Finding the maximum of a list of integers
/*
   length(Xs,N) :- Xs is a list of length N.
*/
     length([X|Xs],N) :- N > 0, N1 is N-1, length(Xs,N1).
     length([],0).

%  Program 8.10   Checking the length of a list
/*
   length(Xs,N) :- N is the length of the list Xs.
*/
     length([X|Xs],N) :- length(Xs,N1), N is N1+1.
     length([],0).

%  Program 8.11  Finding the length of a list
/*
   range(M,N,Ns) :- Ns is the list of integers between M and N inclusive.
*/
     range(M,N,[M|Ns]) :- M < N, M1 is M+1, range(M1,N,Ns).
     range(N,N,[N]).

%  Program 8.12   Generating a list of integers in a given range

/*
   flatten(Xs,Ys) :- Ys is a list of the elements of Xs.
*/

    flatten([X|Xs],Ys) :- 
        flatten(X,Ys1), flatten(Xs,Ys2), append(Ys1,Ys2,Ys).
    flatten(X,[X]) :-
	constant(X), X \== [].
    flatten([],[]).


% Program 9.1a   Flattening a list with double recursion
/*
   flatten(Xs,Ys) :- Ys is a list of the elements of Xs.
*/
     flatten(Xs,Ys) :- flatten(Xs,[],Ys).

     flatten([X|Xs],S,Ys) :- 
		list(X), flatten(X,[Xs|S],Ys).
     flatten([X|Xs],S,[X|Ys]) :- 
		constant(X), X \== [], flatten(Xs,S,Ys).
     flatten([],[X|S],Ys) :- 
		flatten(X,S,Ys).
     flatten([],[],[]).

     list([X|Xs]).

% Program 9.1b   Flattening a list using a stack

/*
   subterm(Sub,Term) :- Sub is a subterm of the ground term Term.
*/
     subterm(Term,Term).
     subterm(Sub,Term) :- 
        compound(Term), functor(Term,F,N), subterm(N,Sub,Term).

     subterm(N,Sub,Term) :- 
        N > 1, N1 is N-1, subterm(N1,Sub,Term).
     subterm(N,Sub,Term) :- 
        arg(N,Term,Arg), subterm(Sub,Arg).

%  Program 9.2    Finding subterms of a term

/*   
  substitute(Old,New,OldTerm,NewTerm) :- NewTerm is the result of replacing
	all occurences of Old in OldTerm by New.	
*/
     substitute(Old,New,Old,New).
     substitute(Old,New,Term,Term) :- 
        constant(Term), Term \== Old.
     substitute(Old,New,Term,Term1) :-
        compound(Term), 
        functor(Term,F,N), 
        functor(Term1,F,N),
        substitute(N,Old,New,Term,Term1).

	substitute(N,Old,New,Term,Term1) :-
            N > 0,
	    arg(N,Term,Arg),
	    substitute(Old,New,Arg,Arg1),
	    arg(N,Term1,Arg1),
	    N1 is N-1,
	    substitute(N1,Old,New,Term,Term1).
        substitute(0,Old,New,Term,Term1).

%  Program 9.3  A program for substituting in a term
/*
   subterm(Sub,Term) :- Sub is a subterm of the ground term Term.
*/
     subterm(Term,Term).
     subterm(Sub,Term) :- 
        compound(Term), Term =.. [F|Args], subterm_list(Sub,Args).

     subterm_list(Sub,[Arg|Args]) :- 
        subterm(Sub,Arg).
     subterm_list(Sub,[Arg|Args]) :-
	subterm_list(Sub,Args).

%  Program 9.4   Subterm defined using univ
/*
   univ(Term, List) :- List is a list containing the functor of Term followed 
		by the arguments of Term.
*/
     univ(Term, [F|Args]) :-
		functor(Term,F,N), args(0,N,Term,Args).

     args(I,N,Term,Arg,Args) :-
		I < N, I1 is I+1, arg(I1,Term,Arg), args(I1,N,Term,Args).
     args(N,N,Term,[]).

%  Program 9.5a   Constructing a list corresponding to a term
/*
   univ(Term, List) :- 
	The functor of Term is the first element of the list List, 
	and its arguments are the rest of List's elements.
*/

     univ(Term, [F|Args]) :-
	length(Args,N), functor(Term,F,N), args(Args,Term,1).

     args([Arg|Args],Term,N) :-
		arg(N,Term,Arg), N1 is N+1, args(Args,Term,N1).
     args([],Term,N).
/*
   length(Xs,N) :- N is the length of the list Xs.
*/
     length([X|Xs],N) :- length(Xs,N1), N is N1+1.
     length([],0).

%  Program 9.5b   Constructing a term corresponding to a list
