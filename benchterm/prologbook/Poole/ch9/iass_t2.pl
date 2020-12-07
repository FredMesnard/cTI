% Computational Intelligence: a logical approach. 
% Prolog Code. Example 9.4.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% load iass.pl first

interesting(X) <- about_ai(X) & int_ai(X).
about_ai(X) <- about_fl(X).
about_ai(X) <- about_ml(X).
false <- about_fl(X) & interesting(X).
false <- intro_question(X) & interesting(X).

about_ai(article_23) <- true.
about_fl(article_77) <- true.
about_ml(article_34) <- true.
about_ml(article_99) <- true.
intro_question(article_99) <- true.

assumable(int_ai(X)).

%  Example Query:
% | ?- explain(interesting(X),E).
