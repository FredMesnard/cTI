

transform(State1,State2,Plan) :-
   transform(State1,State2,[State1],Plan).

transform(State,State,Visited,[]).
transform(State1,State2,Visited,[Action|Actions]) :-
   chooseaction(Action,State1,State2),
   update(Action,State1,State),
   notmember(State,Visited),
   transform(State,State2,[State|Visited],Actions).

chooseaction(Action,State1,State2):-
  suggest(Action,State2), 
  legalaction(Action,State1).
chooseaction(Action,State1,State2):-
  legalaction(Action,State1).

suggest(toplace(X,Y,Z),State) :-
   member(on(X,Z),State), 
   place(Z).
suggest(tobloc(X,Y,Z),State) :-
   member(on(X,Z),State),
   bloc(Z).

legalaction(toplace(bloc,Y,Place),State) :-
   on(bloc,Y,State),
   clear(bloc,State),
   place(Place),
   clear(Place,State).

legalaction(tobloc(bloc1,Y,bloc2),State) :-
   on(bloc1,Y,State),
   clear(bloc1,State),
   bloc(bloc2),
   bloc1 \== bloc2,
   clear(bloc2,State).

clear(X,State) :-
   bloc(A),
   notmember(on(A,X),State).
on(X,Y,State) :-
   member(on(X,Y),State).

update(tobloc(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).
update(toplace(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).

member(X,[X|Y]).
member(X,[F|T]) :-
   member(X,T).

notmember(X,[]).
notmember(X,[F|T]) :-
   X \== F,
   notmember(X,T).

substitute(X,Y,[],[]).
substitute(X,Y,[X|T],[Y|Ts]) :-
  substitute(X,Y,T,Ts).
substitute(X,Y,[F|T],[F|Ts]) :-
  X \== F,
  substitute(X,Y,T,Ts).

testplan(Name,Plan) :-
  initialstate(Name,I),
  finalstate(Name,F),
  transform(I,F,Plan).

initialstate(test,[on(a,b),on(b,p),on(c,r)]).
finalstate(test,[on(a,b),on(b,c),on(c,r)]).

bloc(a).
bloc(b).
bloc(c).

place(p).
place(q).
place(r).

