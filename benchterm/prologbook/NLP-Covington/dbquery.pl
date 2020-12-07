% File DBQUERY.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 2

% Database querying system using keywords

% Requires code from READAT2.PL or equivalent.
% Be sure to use a version that correctly processes numbers.
:- reconsult('readat2.pl').


% The database, with elementary operations...

employee(1001,'Doe, John P.',[1947,01,30],'President',100000).
employee(1002,'Smith, Mary J.',[1960,09,05],'Programmer',30000).
employee(1003,'Zimmer, Fred',[1957,3,12],'Sales rep',45000).

full_name(ID,N) :- employee(ID,N,_,_,_).
birth_date(ID,B) :- employee(ID,_,B,_,_).
title(ID,T) :- employee(ID,_,_,T,_).
salary(ID,S) :- employee(ID,_,_,_,S).

display_record(ID) :-
   employee(ID,N,B,T,S),
   write([ID,N,B,T,S]),
   nl.

remove_record(ID) :-
   retract(employee(ID,_,_,_,_)).


% process_queries
%  Accepts database queries from the keyboard and
%  executes all solutions to each query.

process_queries :-
   repeat,
     write('Query: '),
     read_atomics(Words),
     % some systems would have a 'simplify' stage here,
     translate(Words,_,Query),
     write(Query), nl,            % for testing
     do_all_solutions(Query),
     Words == [quit],
   !.


% do_all_solutions(+Query)
%  Makes execution backtrack through all solutions to Query.

do_all_solutions(Query) :-
   call(Query),
   fail.

do_all_solutions(_).


% translate(Words,Variable,Query)
%  Translates Words (a list of atoms) into Query
%  (a compound Prolog query to access the database).
%  Variable serves to identify the records being retrieved.

translate([W|Words],X,(Queries,Q)) :-
   action(W,X^Q),
   !,
   translate(Words,X,Queries).

translate([W|Words],X,(Q,Queries)) :-
   test(W,X^Q),
   !,
   translate(Words,X,Queries).

translate([Arg1,W,Arg2|Words],X,(Q1,Q2,Q3,Queries)) :-
   relation(W,Y^Z^Q3),
   !,
   argument(Arg1,X^Y^Q1),
   argument(Arg2,X^Z^Q2),
   translate(Words,X,Queries).

translate([_|Words],X,Query) :-    % skip unrecognized word
   translate(Words,X,Query).

translate([],_,true).


% Vocabulary for the database query system

action(show,X^display_record(X)).
action(display,X^display_record(X)).
action(delete,X^remove_record(X)).

test(programmer,X^title(X,'Programmer')).
test(programmers,X^title(X,'Programmer')).
test(salesrep,X^title(X,'Sales rep')).
test(salesreps,X^title(X,'Sales rep')).

relation(over,Y^Z^(Y>Z)).
relation(under,Y^Z^(Y<Z)).

argument(salary,X^Y^salary(X,Y)).
argument(birthdate,X^Y^birth_date(X,Y)).
argument(N,_^Y^(Y=N)) :- number(N).   % numeric(N) in ESL Prolog-2.
