% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learner
%    binary attributes, GINI index, no noise.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% dtlearn(Goal, Examples, Attributes, DT)
%    Given Examples and Attributes induces a decision tree DT for Goal.
%    Uses GINI index for splits
%    Works for noise-free features, with boolean attributes.

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
    impurity(A1,Goal,Examples,I1),
    select_min_impurity(Goal, Examples, RAtts, A1, I1, Cond, [], Rem_Atts).

% select_min_impurity(Goal, Examples, Atts, BestC, BestIG, 
%       Cond, Rej_Atts, Rem_Atts)
select_min_impurity(_, _, [], BestC, _, BestC, Rem_Atts, Rem_Atts).
select_min_impurity(Goal, Exs, [Att|R], BestC, BestIG,Cond, Rej_Atts, Rem_Atts) :-
   impurity(Att,Goal,Exs,Imp),
   ( Imp > BestIG
     -> select_min_impurity(Goal, Exs, R, BestC, BestIG,Cond, [Att|Rej_Atts], Rem_Atts)
     ; select_min_impurity(Goal, Exs, R, Att, Imp ,Cond, [BestC|Rej_Atts], Rem_Atts)
   ).

impurity(Att,Goal,Examples,Impurity) :-
   split(Examples,Att,_,T,F),
   gini(Goal,T,GIT),
   gini(Goal,F,GIF),
   Impurity is GIT+GIF.

gini(Att,Examples,Gini_Index) :-
   count_examples_true_and_false(Examples,Att,_,0,NTrue,0,NFalse),
   Gini_Index is 1- (NTrue*NTrue + NFalse*NFalse)/((NTrue + NFalse)*(NTrue + NFalse)).

count_examples_true_and_false([],_,_,NT,NT,NF,NF).
count_examples_true_and_false([E|Es],Att,YesVal,NT0,NT2,NF0,NF2) :-
   val(E,Att,YesVal),
   NT1 is NT0+1,
   count_examples_true_and_false(Es,Att,YesVal,NT1,NT2,NF0,NF2).
count_examples_true_and_false([E|Es],Att,YesVal,NT0,NT2,NF0,NF2) :-
   val(E,Att,NoVal),
   \+ (YesVal = NoVal),
   NF1 is NF0+1,
   count_examples_true_and_false(Es,Att,YesVal,NT0,NT2,NF1,NF2).


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

