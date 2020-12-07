% p(Var,Obs,Dist) is true if Dist represents the
% probability distribution of P(Var|Obs)

p(Var,Obs,Dist) :-
   member(Var=Val,Obs),!,
   values(Var,ValList),
   make_obs_dist(ValList,Val,Dist).

p(Var,Obs,Dist) :-
   partition_obs(Var,Obs,ObsND,Children),
   condition_nondesc(Var,ObsND,DistN),
   condition_desc(Var,ObsND,Children,DistN,Dist).

%%%%%%%%%%%%%%%%%%%%%%
% DIRECT OBSERVATION %
%%%%%%%%%%%%%%%%%%%%%%

% make_obs_dist(Vals,Val,Dist) created distribution
% Dist corresponding to the observation of value Val,
% where Vals is the list of all values.
make_obs_dist([],_,[]).
make_obs_dist([Val|R],Val,[1|D]) :-
   !,
   make_obs_dist(R,Val,D).
make_obs_dist([_|R],Val,[0|D]) :-
   make_obs_dist(R,Val,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARTITIONING THE OBSERVATIONS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% partition_obs(Var,Obs,ObsND,Children),
partition_obs(_,[],[],[]).
partition_obs(Var,[Obsv=Val|RObs],ND,Ch1) :-
    descendent(Var,Obsv,[obs(Obsv=Val)],Path),
    !,
    partition_obs(Var,RObs,ND,Ch0),
    insert_path(Path,Ch0,Ch1).
partition_obs(Var,[Obsv=Val|RObs],[Obsv=Val|ND],Ch) :-
    partition_obs(Var,RObs,ND,Ch).

descendent(Var,Obsv,Path,Path) :-
   parents(Obsv,PObs),
   member(Var,PObs).

descendent(Var,Obsv,Path0,Path1) :-
   parents(Obsv,PObs),
   member(PO,PObs),
   descendent(Var,PO,[PO|Path0],Path1).

insert_path([obs(Var=Val)],[],[obs(Var=Val)]) :- !.
insert_path([obs(Var=Val)],[child(Var,_)|Chn],[obs(Var=Val)|Chn]) :- !.
insert_path([obs(Var=Val)],[CH|Chn0],[CH|Chn1]) :- !,
   insert_path([obs(Var=Val)],Chn0,Chn1).
insert_path([Ch|P1],[],[child(Ch,Chn1)]) :-
   insert_path(P1,[],Chn1).
insert_path([Ch|_],[obs(Ch=Val)|Chn],[obs(Ch=Val)|Chn]) :- !.
insert_path([Ch|P1],[child(Ch,ChnCh0)|Chn],[child(Ch,ChnCh1)|Chn]) :-
   !,
   insert_path(P1,ChnCh0,ChnCh1).
insert_path([Ch|P1],[Ch1|Chn0],[Ch1|Chn1]) :-
   insert_path([Ch|P1],Chn0,Chn1).

%%%%%%%%%%%%%%%%%%%%
% CAUSAL REASONING %
%%%%%%%%%%%%%%%%%%%%

% condition_nondesc(Var,Obs,Dist) means Dist =
% P(Var|Obs) & we know there are no descendents in Obs.
condition_nondesc(Var,Obs,Dist) :-
   parents(Var,Pars),
   get_dists(Pars,[],RPars,Obs,[],PDists),
   values(Var,Values),
   make_zero_dist(Values,ZeroD),
   multiply_dists(RPars,PDists,Var,[],1,ZeroD,Dist).

% Get the distributions for all parents -
% this is only correct if the graph is singly connected.
% This also reverses lists so that we query the
% probability of variables in the correct order.
% get_dists(Pars,PAcc,RPars,Obs,DAcc,Dists)
% Par is the list of parents left to find distribution for
% PAcc is the list of parents whose distributions have been found
% PRev is the list parents in reverse order
% DAcc is the distributions for the elements of Pacc
% Dists is the list of distributions corresponding to PRev
get_dists([],RP,RP,_,PD,PD).
get_dists([V|R],PAcc,PRev,Obs,DistAcc,Dist) :-
   p(V,Obs,VD),
   get_dists(R,[V|PAcc],PRev,Obs,[VD|DistAcc],Dist).

% make_zero_dist(Values,ZeroD) makes a Zero
% distribution the same length as the list Values
make_zero_dist([],[]).
make_zero_dist([_|L],[0|R]) :-
   make_zero_dist(L,R).


% multiply_dists(Vars,Dists,Par,Context,Weight,Dist0,Dist1).
% updates the distribution for variable Par from Dist0 to Dist1
% Where Context is an assignment of values to children of Par,
% and Vars is the children not assigned a value in Context,
% Dists is the list of distributions of elements of Vars.
% Weight is the (possibly unnormalized) probabilty of the context.

multiply_dists([],[],Par,Context,Weight,Dist0,Dist1) :-
   pr(Par,Context,Dist),
   weight_dist(Weight,Dist,Dist0,Dist1).
multiply_dists([CVar|RVars],[Dist|RDists],Par,Con,Weight,Dist0,Dist1) :-
   values(CVar,CVals),
   expand_context(CVals,Dist,CVar,RVars,RDists,Par,Con,Weight,Dist0,Dist1).

% expand_context(CVals,Dist,CVar,RVars,RDists,Par,Context,Weight,Dist0,Dist1).
% CVals is a list of values for variable CVar that is a child of Par,
% and Dist is a list corresponding to the probabilities of the values in CVals.
% RVars is the remaining variables to be assigned a value, and
% RDists is the list of their distributions.
expand_context([],[],_,_,_,_,_,_,W,W).
expand_context([_|RVals],[0|RPs],CVar,RVars,RDists,Par,Con,W,Dist0,Dist1) :-
   !,     % P(CVar=Val1)=0 and so no need to consider this context
   expand_context(RVals,RPs,CVar,RVars,RDists,Par,Con,W,Dist0,Dist1).
expand_context([Val1|RVals],[P1|RPs],CVar,RVars,RDists,Par,Con,W,Dist0,Dist2) :-
   We1 is W * P1,
   multiply_dists(RVars,RDists,Par,[CVar=Val1|Con],We1,Dist0,Dist1),
   expand_context(RVals,RPs,CVar,RVars,RDists,Par,Con,W,Dist1,Dist2).

% weight_dist(Weight,Dist,AccDist,WDist)  WDist is
% list Dist mutiplied by scalar Weight added onto the
% accumulated distribution.
weight_dist(_,[],[],[]).
weight_dist(Weight,[N|R],[A|RA],[P|WR]) :-
   P is N*Weight+A,
   weight_dist(Weight,R,RA,WR).

%%%%%%%%%%%%%%%%%%%%%%%%
% EVIDENTIAL REASONING %
%%%%%%%%%%%%%%%%%%%%%%%%

% condition_desc(Xq,ObsND,Xcs,DistN,Dist)
condition_desc(_,_,[],DXq,DXq).
condition_desc(Xq,ObsND,[Ch|Chs],DXq0,DXq2) :-
   condition_child(Xq,ObsND,Ch,DXq0,DXq1),
   condition_desc(Xq,ObsND,Chs,DXq1,DXq2).

condition_child(Xq,ObsND,obs(Xc=Vc),DXq0,DXq1) :-
   parents(Xc,Xps),
   get_dists_except(Xq,Xps,ObsND,DXps0),
   values(Xq,Vqs),
   pr_obs_for_each_Vq(Vqs,Xq,Vqs,Xps,DXps0,Xc,Vc,DXq0,DXq1).

pr_obs_for_each_Vq([],_,_,_,_,_,_,DXq,DXq).
pr_obs_for_each_Vq([Vq|Vqs0],Xq,Vqs,Xps,DXps0,Xc,OVc,[PVq0|DXq0],[PVq1|DXq1]) :-
    make_Vq_dist(Vq,Xq,Vqs,Xps,DXps0,[],RXps,[],DXps),
    values(Xc,Vcs),
    make_zero_dist(Vcs,ZeroDc),
    multiply_dists(RXps,DXps,Xc,[],1,ZeroDc,DXc),
    prob_from_dist(DXc,Vcs,OVc,0,PSum,PObs),
    PVq1 is PVq0*PObs/PSum,
    pr_obs_for_each_Vq(Vqs0,Xq,Vqs,Xps,DXps0,Xc,OVc,DXq0,DXq1).


% prob_from_dist(DXc,Vcs,OVc,0,PSum,PObs)
prob_from_dist([],[],_,PSum,PSum,_).
prob_from_dist([PVc|DXc],[OVc|Vcs],OVc,PSum0,PSum1+PVc,PVc) :-
   !,
   prob_from_dist(DXc,Vcs,OVc,PSum0,PSum1,_).
prob_from_dist([PVc|DXc],[_|Vcs],OVc,PSum0,PSum1+PVc,PObs) :-
   !,
   prob_from_dist(DXc,Vcs,OVc,PSum0,PSum1,PObs).






% make_Vq_dist(Vq,Xq,Vqs,Xps,DXps,RXps0,RXps1,DXps0,DXps1),
make_Vq_dist(_,_,_,[],[],P,P,DXps,DXps).
make_Vq_dist(Vq,Xq,Vqs,[Xq|Xps],DXps,Xps0,Xps1,DXps0,DXps1):-
   !,
   make_obs_dist(Vqs,Vq,DXq),
   make_Vq_dist(Vq,Xq,Vqs,Xps,DXps,[Xq|Xps0],Xps1,[DXq|DXps0],DXps1).

make_Vq_dist(Vq,Xq,Vqs,[_|Xps],[DXp|DXps],RXps0,RXps1,DXps0,DXps1):-
   make_Vq_dist(Vq,Xq,Vqs,Xps,DXps,[Xq|RXps0],RXps1,[DXp|DXps0],DXps1).




% get_dists_except(Xq,Xps,Obs,DXps)
% get distribiutions for all XPs except for Xq given Obs
% XPs is the list of parents left to find distribution for
% DXps is the list of distributions corresponding to Pars
get_dists_except(_,[],_,[]).
get_dists_except(Xq,[Xq|Xps],Obs,DXps) :- 
   !,
   get_dists_except(Xq,Xps,Obs,DXps).
get_dists_except(Xq,[Xp|Xps],Obs,[DXp|DXps]) :-
   p(Xp,Obs,DXp),
   get_dists_except(Xq,Xps,Obs,DXps).


%%%%%%%%%%%%%%%%%%%%%%%%
% STANDARD DEFINITIONS %
%%%%%%%%%%%%%%%%%%%%%%%%
% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

% insert(E,L,R) inserts E into list L producing list R.
% only correct if E and L are ground and R is free.
insert(A,L,L) :-
   member(A,L),!.
insert(A,L,[A|L]).
