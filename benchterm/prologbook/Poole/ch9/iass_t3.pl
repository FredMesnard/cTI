% Computational Intelligence: A Logical Approach
% Prolog Code.  Example 9.5
% Copyright, 1997. All rights reserved.

% load iass.pl first

interesting(X) <- about_ai(X) & int_ai(X).
about_ai(X) <- about_fl(X).
about_ai(X) <- about_ml(X).
false <- about_fl(X) & unint_fl(X) & interesting(X).
false <- intro_question(X) & unint_iq(X) & interesting(X).

false <- about_fl(X) & int_ai(X).

about_ai(article_23) <- true.
about_fl(article_77) <- true.
about_ml(article_34) <- true.
about_ml(article_99) <- true.
intro_question(article_99) <- true.

assumable(int_ai(X)).
assumable(unint_fl(X)).
assumable(unint_iq(X)).

%  Example Query:
% | ?- explain(interesting(X),E).
