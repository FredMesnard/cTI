
p(A):-cti:{b(A)},p2(A).
p2(s(A)):-p2(A).

goal :- cti:{n(A) = n(B)+1}, cti:{n(B)=n(A)+1}, loop.
loop :- loop.

:- initialization(loop).

%%query: idem(o,i).   lkmkm
idem([],[]).
idem([X|Xs],[Y|Ys]) :- X=Y, idem(Xs,Ys). % 1+2 : ok
idem([X|Xs],[y|Ys]) :- idem(Xs,Ys). % 1+2 : ok
idem([X|Xs],[X|Ys]) :- idem(Xs,Ys). % 1 : bug !

/*
mapp([],Ys,Ys).
mapp([X|Xs],Ys,[X|Zs]) :- mapp(Xs,Ys,Zs).

app([], X, X).     
app([e|X], Y, [e|Z]) :- app(X, Y, Z).


a4(X,Y,Z,T) :-  mapp(X,Y,U),mapp(U,Z,T).

nrev([], []).                             
nrev([E|X], Y) :-
      nrev(X, Z),
      mapp(Z, [E], Y).
 

p :- q.
q :- p.

r :- r.
r.

s.


pair(0).
pair(s(X)) :- impair(X).

impair(s(X)) :- pair(X).

pi(X) :- pair(X), impair(X).
*/
