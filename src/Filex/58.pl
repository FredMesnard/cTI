
% split(A,B,C)terminates_if b(A);b(B),b(C). % unresolved cases: [split(f,b,f)].

%split([],[],[]).
%split([X|Xs],[X|Ys],Zs) :- split(Xs,Zs,Ys).

% ok :    mes([s/3-[A,B,C,D]],[A=0,C=D,B>=0,D>=0,B+D>=1]), 100 et 011
%s([],[],[]).
%s([x|Xs],[x|Ys],Zs):-s(Xs,Zs,Ys).

% pb :    on devrait trouver 010 et 100
%s([],[],[]).
%s([x|Xs],[x|Ys],Zs):-cti:{n(Zs)>=n(Ys)},s(Xs,Zs,Ys).

s([],[]).
s([X|Ys],[X|Zs]):-
	%cti:{n(Zs) >= n(Ys),n(Ys)+1 >= n(Zs)},
	s(Ys,Zs).

%s([],[],[]).
%s([x|Xs],[x|Ys],Zs):-cti:{n(Ys)>=n(Zs),n(Zs)+1 >= n(Ys)},s(Xs,Zs,Ys).
