% Computational Intelligence: a logical approach. 
% Prolog Code. Bottom-up definite clause interpreter (Appendix C.1)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% "<-" is the object-level "if"
:- op(1150, xfx, <- ).
% "&" is the object level conjunction.
:- op(950,xfy, &).

% fc(C,R) is true if you can forward chain from
%   atoms in C resulting in R.
fc(C,R) :-
   ( A <- B ),      % there is a rule in the KB
   derived(B,C),    % whose body is derived and
   notin(A,C),      % whose head isn't derived.
   !,               % commit to this selection.
   fc([A|C],R).     % forward chain with head derived.
fc(R,R).            % no selection is possible.

% derived(B,C) is true if body B can be directly 
% derived from atoms in C.
derived(true,_) :-
   !.   % the other rules aren't applicable for true body
derived((A&B),C) :-
   !,   % the other rule isn't applicable for conjunctive body
   derived(A,C),
   derived(B,C).
derived(A,C) :- 
   member(A,C).
