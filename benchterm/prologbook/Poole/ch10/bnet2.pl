p(Var,Obs,Dist) :-
   partition_obs(Var,Obs,Desc,NonDesc,Children).

% partition_obs(Var,Obs,Desc,NonDesc,Children),
partition_obs(_,[],[],[],[]).
partition_obs(Var,[Obsv=Val|RObs],[Obsv=Val|D],ND,Ch1) :-
    descendent(Var,Obsv,Child),
    !,
    partition_obs(Var,RObs,D,ND,Ch0),
    insert(Child,Ch0,Ch1).
partition_obs(Var,[Obsv=Val|RObs],D,[Obsv=Val|ND],Ch) :-
    partition_obs(Var,RObs,D,ND,Ch).

descendent(Var,Obsv,Obsv) :-
   parents(Obsv,PObs),
   member(Var,PObs).

descendent(Var,Obsv,Par) :-
   parents(Obsv,PObs),
   member(PO,PObs),
   descendent(Var,PO,Par).

% partition_obs(fire,[report=yes,tampering=no,smoke=no],D,N,C).

insert(A,L,L) :-
   member(A,L),!.
insert(A,L,[A|L]).
