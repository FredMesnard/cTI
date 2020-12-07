% File REVERSE.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix A, section A.7.2

% reverse(+List1,?List2)
%  reverses order of elements in List1 (which must be instantiated).

reverse(List1,List2) :- reverse_aux(List1,[],List2).

reverse_aux([H|T],Stack,Result) :-
   reverse_aux(T,[H|Stack],Result).

reverse_aux([],Result,Result).

