% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A small example to test the inequality solver.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

passed_two_courses(S) <-
   C_1 \= C_2 &
   passed(S,C_1) &
   passed(S,C_2).
passed(sally,engl101) <- true.
passed(sally,phys101) <- true.
passed(david,engl101) <- true.

% ? neprove(passed_two_courses(S)).
