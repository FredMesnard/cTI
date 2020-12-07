% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learner
%    binary attributes, myopic max information split, no noise
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% dtlearn(Goal, Examples, Attributes, DT)
%    Given Examples and Attributes induces a decision tree DT for Goal.
%    Splits on the maximum information gain split
%    Works for noise-free features, with binary attributes.

dtlearn(Goal, Examples, _ , Val) :-
   all_examples_agree(Goal, Examples, Val).

dtlearn(Goal, Examples, Atts, if(Cond=YesVal,YT,NT)) :-
   \+ all_examples_agree(Goal, Examples, _),
   select_split(Goal, Examples, Atts, Cond, Rem_Atts),
   split(Examples, Cond, YesVal, Yes, No),
   dtlearn(Goal, Yes, Rem_Atts, YT),
   dtlearn(Goal, No, Rem_Atts, NT).

% all_examples_agree(Goal, Examples, Val) is true if all examples agree that
%      Val is the value of attribute Goal.
all_examples_agree(_,[],_).
all_examples_agree(Att,[Obj|Rest],Val) :-
   val(Obj,Att,Val),
   all_examples_agree(Att,Rest,Val).


% split(Examples,Att,YesVal,T,F) is true if T is
% the examples in Examples with attribute Att
% having value YesVal and F is the examples with
% attribute Att having the other value.
split([],_,_,[],[]).
split([Obj|Rest],Cond,YesVal,[Obj|Yes],No) :-
   val(Obj,Cond,YesVal),
   split(Rest,Cond,YesVal,	Yes,No).
split([Obj|Rest],Cond,YesVal,Yes,[Obj|No]) :-
   val(Obj,Cond,NoVal),
   \+ NoVal = YesVal,
   split(Rest,Cond,YesVal,Yes,No).

select_split(Goal, Examples, [A1|RAtts], Cond, Rem_Atts) :-
%   here is where we really want maximum information, but it
%   is too early in the book. This is defined using max information here. 
    split_info(Goal,Examples,A1,I1),
    select_max_info_gain(Goal, Examples, RAtts, A1, I1, Cond, [], Rem_Atts).

% select_max_info_gain(Goal, Examples, Atts, BestC, BestIG, 
%       Cond, Rej_Atts, Rem_Atts)
%  This selects the attribute with the minimum expected information after the split.
select_max_info_gain(_, _, [], BestC, _, BestC, Rem_Atts, Rem_Atts).
select_max_info_gain(Goal, Exs, [Att|R], BestC, BestIG,Cond, Rej_Atts, Rem_Atts) :-
   split_info(Goal,Exs,Att,Info),
   ( Info > BestIG
     -> select_max_info_gain(Goal, Exs, R, BestC, BestIG,Cond, [Att|Rej_Atts], Rem_Atts)
     ; select_max_info_gain(Goal, Exs, R, Att, Info ,Cond, [BestC|Rej_Atts], Rem_Atts)
   ).

% split_info(Goal,Examples,Att,Info) is true if Info is the expected 
% information about Goal in Examples given a test for Att
split_info(Goal,Examples,Att,Info) :-
   split(Examples,Att,_,T,F),
   information(Goal,T,IT),
   information(Goal,F,IF),
   length(T,NT),
   length(F,NF),
   Info is (NT*IT + NF*IF)/(NT+NF).


% information(Goal,Examples,I) is true if I is the amount of information
%    in Examples with respect to Goal
information(Goal,Examples,I) :-
    count(Goal,_,Examples,True,False),
   ( (True=0 ; False=0) -> I=0
   ;
    Total is True + False,
    I is - True/Total * log(2,True/Total) - False/Total * log(2,False/Total)
   ).

% count(Att,TrueVal,Examples,T,F) is true if there are T examples in Examples
%   with attribute Att true and F examples with attribute Att false.
count(_,_,[],0,0).
count(Att,TrueVal,[Ex|R],T1,F) :-
   val(Ex,Att,TrueVal),
   count(Att,TrueVal,R,T,F),
   T1 is T+1.
count(Att,TrueVal,[Ex|R],T,F1) :-
   val(Ex,Att,FalseVal),
   \+ TrueVal=FalseVal,
   count(Att,TrueVal,R,T,F),
   F1 is F+1.

