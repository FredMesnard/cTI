% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Structured Semantic Network from Figure 5.6 and Example 5.13.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

prop(comp_2347,is_a,lemon_laptop_10000).
prop(comp_2347,owned_by,craig).
prop(X,weight,light) :-
   prop(X,is_a,lemon_laptop_10000).
prop(X,size,medium) :-
   prop(X,is_a,lemon_laptop_10000).
prop(X,is_a,lemon_computer) :-
   prop(X,is_a,lemon_laptop_10000).
prop(X,is_a,computer) :-
   prop(X,is_a,lemon_computer).
prop(X,logo,lemon_disc) :-
   prop(X,is_a,lemon_computer).
prop(X,color,brown) :-
   prop(X,is_a,lemon_computer).
prop(X,packing,cardboard_box) :-
   prop(X,is_a,computer).
prop(X,deliver_to,ming) :-
   prop(X,is_a,computer).
prop(ming,room,r117).
prop(craig,room,r107).
prop(r117,building,comp_sci).
prop(r107,building,comp_sci).

% To generate Example 5.9 find all answers to the following query:
% | ?- prop(A,B,C).
