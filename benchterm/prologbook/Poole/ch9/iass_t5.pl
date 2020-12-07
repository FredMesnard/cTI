% Computational Intelligence: a logical approach. 
% Prolog Code. Example 9.9.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% load iass.pl first

selects(Ag,Art) <- 
   about(Art,Topic) & 
   interested_in(Ag,Topic).

about(article_94,ai) <- true.
about(article_94,information_highway) <- true.
about(article_34,ai) <- true.
about(article_34,skiing) <- true.

assumable(interested_in(_,_)).

%  Example Query:
% | ?- explain(selects(fred,article_94),E).
% | ?- explain(selects(fred,article_94)&selects(fred,article_34),E).
