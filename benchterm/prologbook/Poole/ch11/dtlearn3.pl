% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learner
%    binary attributes, , full search for smallest tree, no noise.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% dtlearn(Goal, Examples, Attributes, DT)
%    Given Examples and Attributes induces a decision tree DT for Goal.
%    non-myopic - does full search for smallest tree, for binary attributes 
%    Does not handle noisy data

dtlearn(Goal, Examples, Attributes, DT) :-
   length(Attributes,NAtts),
   power(2,NAtts,MaxTree),
   int(N,MaxTree),
   dtlearn(Goal, Examples, Attributes, DT,N,0).

dtlearn(Goal, Examples, _ , Val,N,N) :-
   all_examples_agree(Goal, Examples, Val).


dtlearn(Goal, Examples, Atts, if(Cond=YesVal,YT,NT),N0,N3) :-
   N0>0,
   \+ all_examples_agree(Goal, Examples, _),
   select_split(Goal, Examples, Atts, Cond, Rem_Atts),
   split(Examples, Cond,YesVal, Yes, No),
   N1 is N0-1,
   dtlearn(Goal, Yes, Rem_Atts, YT,N1,N2),
   dtlearn(Goal, No, Rem_Atts, NT,N2,N3).

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


% select_split(Goal, Examples, Atts, Cond, Rem_Atts)
% is true if Cond is an attribute in Atts to split on
select_split(_, _, Atts, Cond, Rem_Atts) :-
   remove(Cond,Atts,Rem_Atts).
select_split(_, Examples, [], _, _) :-
   writeln(['The examples ',Examples,' cannot be split.']),
   fail.

% remove(E,L,R) removes one occurrence of element E from list L resulting in R
remove(A,[A|R],R).
remove(A,[B|C],[B|R]) :-
   remove(A,C,R).

% int(N,M) is true if N is an integer in the range 0 =< N < M
% This enumerates the integers in order.

int(0,M) :- M>0.
int(N,M) :-
   M>0,
   M1 is M-1,
   int(N1,M1),
   N is N1+1.

% power(X,N,P) is true if P=X^N
power(_,0,1).
power(X,N,PX) :-
   1 is N mod 2,
   N1 is N-1,
   power(X,N1,P),
   PX is P*X.
power(X,N,XN) :-
   0 is N mod 2,
   N>0,
   XX is X*X,
   N2 is N // 2,
   power(XX,N2,XN).

writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).

