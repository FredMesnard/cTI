% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learner
%      binary attributes, myopic max info split, noise allowed.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% dtlearn(Goal, Examples, Attributes, DT)
%    Given Examples and Attributes induces a decision tree DT for Goal.
%    Stochastic (gives probabilities at leaves), myopic (only looks one
%    step ahead), maximum information split, for Boolean attributes without
%    solving overfitting.

dtlearn(Goal, Examples, Atts, if(Cond,YT,NT)) :-
   select_split(Goal, Examples, Atts, Cond),
   split(Examples, Cond, Yes, No),
   remove(Cond, Atts, Rem_Atts),
   dtlearn(Goal, Yes, Rem_Atts, YT),
   dtlearn(Goal, No, Rem_Atts, NT).

dtlearn(Goal, Examples, Atts, Val) :-
   \+ select_split(Goal, Examples, Atts, _),
   count(Goal,Examples,T,F),
   Val is T / (T+F).

% split(Examples,Att,T,F) is true if T is the examples in Examples
%   with attribute Att true and F is the examples with attribute Att false.
split([],_,[],[]).
split([Obj|Rest],Cond,[Obj|Yes],No) :-
   val(Obj,Cond,true),
   split(Rest,Cond,Yes,No).
split([Obj|Rest],Cond,Yes,[Obj|No]) :-
   val(Obj,Cond,false),
   split(Rest,Cond,Yes,No).

% count(Att,Examples,T,F) is true if there are T examples in Examples
%   with attribute Att true and F examples with attribute Att false.
count(_,[],0,0).
count(Att,[Ex|R],T1,F) :-
   val(Ex,Att,true),
   count(Att,R,T,F),
   T1 is T+1.
count(Att,[Ex|R],T,F1) :-
   val(Ex,Att,false),
   count(Att,R,T,F),
   F1 is F+1.

select_split(Goal, Examples, Atts, Cond) :-
% is true if Cond is the best attribute in Atts to determine Goal from
% Examples. It fails if there are no splits that give a gain in information.
    information(Goal,Examples,I1),
    select_max_info_gain(Goal, Examples, Atts, none , I1, Cond).

% select_max_info_gain(Goal, Examples, Atts, BestC, BestI,  Cond)
%  is true if Cond is the attribute that gives the maximum information gain
%  where the information gain is positive (i.e., the information after the
%  split is less than without splitting). BestC is the current best condition
%  (or is "none" if no previous attribute gave any information gain),
%  and BestI is the information after splitting on BestC.

select_max_info_gain(_, _, [], BestC, _, BestC) :- dif(BestC, none).
select_max_info_gain(Goal, Exs, [Att|R], BestC, BestI,Cond) :-
   split_info(Goal,Exs,Att,Info),
   ( Info >= BestI
     -> select_max_info_gain(Goal, Exs, R, BestC, BestI,Cond)
     ; select_max_info_gain(Goal, Exs, R, Att, Info ,Cond)
   ).

% split_info(Goal,Examples,Att,Info) is true if Info is the expected 
% information about Goal in Examples given a test for Att
split_info(Goal,Examples,Att,Info) :-
   split(Examples,Att,T,F),
   information(Goal,T,IT),
   information(Goal,F,IF),
   length(T,NT),
   length(F,NF),
   Info is (NT*IT + NF*IF)/(NT+NF).


% information(Goal,Examples,I) is true if I is the amount of information
%    in Examples with respect to Goal
information(Goal,Examples,I) :-
    count(Goal,Examples,True,False),
   ( (True=0 ; False=0) -> I=0
   ;
    Total is True + False,
    I is - True/Total * log(2,True/Total) - False/Total * log(2,False/Total)
   ).

% remove(E,L,R) removes one occurrence of element E from list L resulting in R
remove(A,[A|R],R).
remove(A,[B|C],[B|R]) :-
   remove(A,C,R).

