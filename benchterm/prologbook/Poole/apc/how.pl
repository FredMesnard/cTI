% Computational Intelligence: a logical approach. 
% Prolog Code.
% Meta-interpreter for traversing proof trees (Appendix C.2)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% <- is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).

% show(G) means prove goal G and show how it was proved
show(G) :-
   solve(G,T),
   traverse(T).

% solve(Goal,Tree) is true if Tree is a proof tree for Goal
solve(true,true).
solve((A&B),(AT&BT)) :-
   solve(A,AT),
   solve(B,BT).
solve(H,if(H,builtin)) :-
   builtin(H),
   call(H).
solve(H,if(H,BT)) :-
   ( H <- B ),
   solve(B,BT).

% builtin(G) is true if goal G is defined in Prolog 
% (as opposed to being defined by rules in the 
%  object-level knowledge base.)
builtin((_ is _)).
builtin((_ =< _)).
builtin((_ >= _)).
builtin((_ = _)).
builtin((_ < _)).
builtin((_ > _)).

% traverse(T) true if T is a tree being traversed
traverse(if(H,true)) :-
   !,                    % no other rules are applicable
   writeln([H,' is a fact']).
traverse(if(H,builtin)) :-
   !,                    % no other rules are applicable
   writeln([H,' is built-in.']).
traverse(if(H,B)) :-
   B \== true,
   B \== builtin,
   writeln([H,' :-']),
   print_body(B,1,Max),
   read(Comm),
   interpret_command(Comm,B,Max,if(H,B)).

% print_body(B,N1,N2) is true if B is a body to be
% printed and N1 is the count of atoms before B
% N2 is the count after
print_body(true,N,N) :-
   !.                    % no other rules are applicable
print_body((A&B),N1,N3) :-
   !,                    % no other rules are applicable
   print_body(A,N1,N2),
   print_body(B,N2,N3).
print_body(if(H,_),N,N1) :-
   writeln(['   ',N,': ',H]),
   N1 is N+1.

% interpret_command(Comm,B,Max,G) interprets the
% command Comm on body B where Max is the number of
% conjunctions in the body and G is the goal
interpret_command(N,B,Max,G) :-
   integer(N),
   N > 0,
   N =< Max,
   !,                    % no other rules are applicable
   nths(B,N,E),
   traverse(E),
   traverse(G).
interpret_command(N,_,Max,G) :-
   integer(N),
   !,                    % no other rules are applicable
   (N < 1 ; N > Max),
   writeln(['Number out of range: ',N]),
   traverse(G).
interpret_command(up,_,_,_) :-
   !.                    % no other rules are applicable
interpret_command(C,_,_,G) :-
   C \== retry,
   writeln(['Give either a number, up or retry.']),
   traverse(G).
% note that interpret_command(retry,_,_,_) fails.

% nths(S,N,E) is true if E is the N-th element of
% conjunction S. This assumes that `&' is left associated.
nths(A,1,A) :-
   \+ (A = (_&_)).
nths((A&_),1,A).
nths((_&B),N,E) :-
   N>1,
   N1 is N-1,
   nths(B,N1,E).
