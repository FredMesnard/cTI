% File REWRITE.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix A, section A.7.3

% rewrite_list(+List1,?List2)
%   copies List1 into List2 changing every element 'a' to 'b'

rewrite(a,b) :- !.     % a rewrites as b
rewrite(X,X).          % anything else rewrites as itself

rewrite_list([],[]).

rewrite_list([First|Rest],[NewFirst|NewRest]) :-
   rewrite(First,NewFirst),
   rewrite_list(Rest,NewRest).

