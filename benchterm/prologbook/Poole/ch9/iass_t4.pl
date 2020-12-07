% Computational Intelligence: a logical approach. 
% Prolog Code. Example 9.6.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% load iass.pl first

about_ai(X) <- about_learning(X) & l_ai(X).
non_academic_recreation(X) <- about_skiing(X) & s_nar(X).
false <- about_ai(X) & non_academic_recreation(X).
interesting_to_mary(X) <- about_ai(X) & ai_im(X).
interesting_to_mary(X) <- non_academic_recreation(X) & nar_im(X).
interesting_to_fred(X) <- non_academic_recreation(X) & nar_if(X).

about_skiing(learning_to_ski) <- true.
about_learning(learning_to_ski) <- true.
about_skiing(ski_Whistler_page) <- true.
about_learning(induction_page) <- true.

assumable(l_ai(X)).
assumable(s_nar(X)).
assumable(ai_im(X)).
assumable(nar_im(X)).
assumable(nar_if(X)).

%  Example Query:
% | ?- explain(interesting_to_mary(X),E).
% | ?- explain(interesting_to_fred(X),E).
