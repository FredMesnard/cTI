% File TAILREC.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix A, section A.7.5

f(X) :- X < 100.                         % Tail recursive example
f(X) :- X >= 100, Y is X-1, f(Y).

g(X) :- X >= 100, Y is X-1, g(Y), z.     % Not tail recursive

h(X) :- X >= 100, Y is X-1, h(Y).        % Not tail recursive
h(X) :- X < 100.

z.                                       % Called by h/1

j(X) :- X >= 100, !, Y is X-1, j(Y).     % Tail recursive
j(X) :- X < 100.                         % because of cut

