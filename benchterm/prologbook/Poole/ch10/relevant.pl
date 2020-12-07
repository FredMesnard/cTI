% Computational Intelligence: a logical approach. 
% Prolog Code.
% This replaces the "relevant" code from bnet.pl.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% relevant(Var,Obs,RelVars) given query variable Var
% and observations Obs, RelVars is the list of
% relevant variables. Var is not observed.
relevant(Var,Obs,RelVars) :-
   make_children_db(ChnDB),
   add_rel_vars([Var],Obs,ChnDB,[],[],[],RelVars).

% add_rel_vars(TAA,Obs,ChnDB,TBCOD,COD,RV0,RV1).
%   adds relevant variables to RV0 producing RV1
%   maintains variables thich we need to add ancestors
%   and those which need to have descendents checked
%    TAA list of variables to add ancestors
%    Obs set of observations
%    TBCOD to be cheked for observed descendents
%    COD checked for observed descendebts
%    RV0 discovered relevant variables
%    RV1 final set of observed variables
add_rel_vars([],_,_,[],_,RV,RV) :- !.
add_rel_vars([],Obs,ChnDB,[V|TBCOD],COD,RV0,RV1) :-
   member(V=_,Obs),!,
   parents(V,VP),
   add_if_not_present(VP,[],RV0,TAA1),
   add_rel_vars(TAA1,Obs,ChnDB,TBCOD,[V|COD],[V|RV0],RV1).
add_rel_vars([],Obs,ChnDB,[V|TBCOD],COD,RV0,RV1) :-
   children(V,ChnDB,VC),
   add_if_not_present(VC,TBCOD,COD,TBCOD1),
   add_rel_vars([],Obs,ChnDB,TBCOD1,[V|COD],RV0,RV1).
add_rel_vars([V|Vs],Obs,ChnDB,TBCOD,COD,RV0,RV1) :-
   member(V=_,Obs),!,
   add_rel_vars(Vs,Obs,ChnDB,TBCOD,COD,RV0,RV1).
add_rel_vars([V|Vs],Obs,ChnDB,TBCOD,COD,RV0,RV1) :-
   parents(V,PV),
   add_if_not_present(PV,Vs,RV0,Vs1),
   children(V,ChnDB,CV),
   add_if_not_present(CV,TBCOD,COD,TBCOD1),
   add_rel_vars(Vs1,Obs,ChnDB,TBCOD1,COD,[V|RV0],RV1).

% add_if_not_present(L,L0,L1,L2) adds the elements
% of L that are not in L0 U L1 to L0 forming L2
add_if_not_present([],L0,_,L0).
add_if_not_present([H|T],L0,L1,L2) :-
    member(H,L0),!,
    add_if_not_present(T,L0,L1,L2).
add_if_not_present([H|T],L0,L1,L2) :-
    member(H,L1),!,
    add_if_not_present(T,L0,L1,L2).
add_if_not_present([H|T],L0,L1,L2) :-
    add_if_not_present(T,[H|L0],L1,L2).

% CHILDREN database
% we want to be able to find the children of a node
% reasonably quickly, but the graphs are only contain
% parents relationships. We could as the user to provide
% the children relationship initially (this would provide
% consistency problems), or we could axiomatize children in
% terms of parents, but this is very innefficent. A compromise
% is to build an inverted list of children initially, and use
% this to determine children. [We could think of asserting the 
% children relationships to the database, but then there would
% be consistency problems if the network were changed.]

make_children_db(ChnDB) :-
    variables(Vars),
   initial_chn_db(Vars, InitDB),
   update_chn_db(Vars, InitDB, ChnDB).

initial_chn_db([],[]).
initial_chn_db([V|R],[ch(V,[])|CDB]) :-
   initial_chn_db(R,CDB).

% update_chn_db(Vars,DB0,DB1) update DB0 to DB1 with parents
% of elements of Vars.
update_chn_db([],DB,DB).
update_chn_db([V|Vs],DB0,DB2) :-
   parents(V,PV),
  add_to_each_parent(PV,V,DB0,DB1),
  update_chn_db(Vs,DB1,DB2).

add_to_each_parent([],_,DB,DB).
add_to_each_parent([P|Ps],Ch,DB0,DB2) :-
   remove(ch(P,CP),DB0,DB1),
   add_to_each_parent(Ps,Ch,[ch(P,[Ch|CP])|DB1],DB2).

children(V,ChnDB,Ch) :-
   member(ch(V,Ch),ChnDB).
