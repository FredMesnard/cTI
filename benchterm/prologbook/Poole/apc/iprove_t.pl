% Computational Intelligence: a logical approach. 
% Prolog Code.
% Example for Iterative-deepening Interpreter (Appendix C.2, page 503)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

odd(s(s(N))) <- odd(N).
odd(s(0)) <- true.
even(s(s(N))) <- even(N).
even(0) <- true.
num(N) <- even(N).
num(N) <- odd(N).
