test :- repeat.

test0 :- fail, repeat.

test1 :- L=[1],L=[2,3],repeat.

test2(X,Y) :- leq(X,Y),sup(X,Y),repeat.

leq(0,_).
leq(s(X),s(Y)) :- leq(X,Y).

sup(s(_),0).
sup(s(X),s(Y)):-sup(X,Y).

test3(X) :- X=s(0),Y=0,test3(Y).

