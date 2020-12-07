% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A CSP solver using GSAT. 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% this assumes that random.pl and standard.pl are also loaded.
                                    
% DATA STRUCTURES

% Domains represented as list of 
% [dom(X,[c1,...,cn]),...]

% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r

% An assignment is represented as a list
% [val(X,V),...] where X is a variable
% and V is a value in the domain of X
% If variable X appears more than once, the first value is meant

% RELATIONS

% gsat(Doms,Relns,N,M,Ass) is true if Ass is an
% assignment of a value for each variable that satisfies
% Relns using GSAT. 
% N is the maxumum number of restarts.
% Each restart does M steps of hill climbing.
gsat(Doms,Relns,_,M,SAss) :-
   random_assign(Doms,Asst),
%   writeln(['Given Doms = ',Doms]),
   writeln(['   Random Assignment: ',Asst]),
   it_improve(Asst,Doms,Relns,M,SAss).
gsat(Doms,Relns,N,M,SAss) :-
   N>0,
   N1 is N-1,
   gsat(Doms,Relns,N1,M,SAss).

% random_assign(Doms,Asst) is true if Asst is a random assignment
% of a value to each variable, given domains Doms
random_assign([],[]).
random_assign([dom(X,D)|Ds],[val(X,E)|Vs]) :-
   random_elt(E,D),
   random_assign(Ds,Vs).

% it_improve(Asst,Doms,Relns,M,SAss)
% is true if, given
% Asst is a random Assignment,
% Doms is a set of domains
% Relns is a set of Relations
% M is a bound on the number of iterations,
% we can find an satisfying assignment SAss of values to variables that
% satisfies all of the relations.
it_improve(Asst,_,Relns,_,Asst) :-
   number_unsat_relns(Relns,Asst,NSat),
   writeln(['   Value = ',NSat]),
   NSat=0.                     % it randomly chose satisfying asst
it_improve(Asst,Doms,Relns,M,SAss) :-
   vars(Doms,Vars),
   improve_one(Vars,Doms,Relns,Asst,99999,Asst,BVal,BAss),
   it_improve_new(Asst,Doms,Relns,Vars,M,BVal,BAss,SAss).

% it_improve_new(Asst,Doms,Relns,Vars,M,BVal,BAss,SAss).
% is true if, given
% Asst is a random Assignment,
% Doms is a set of domains
% Relns is a set of Relations
% Vars is the list of all assignments
% M is a bound on the number of iterations,
% BVal is the value of the previous iteration
% BAss is the best assignment of the previous iteration
% we can find an satisfying assignment SAss of values to variables that
% satisfies all of the relations.
% Note that this is seperate from it_improve as we have to do
%  something different in the first iteration.
it_improve_new(_,_,_,_,_,BVal,val(Var,Val),_) :-
   writeln([' Assign ',Val,' to ',Var,'. New value = ',BVal]),
   fail.
it_improve_new(Asst,_,_,_,_,0,BAss,SAss) :-
   update_asst(Asst,BAss,SAss).
it_improve_new(Asst,Doms,Relns,Vars,M,_,val(Var,Val),SAss) :-
   M>0,
   rem_id(Var,Vars,RVars),
   update_asst(Asst,val(Var,Val),NAss),
   writeln(['    New Asst: ',NAss]),
   M1 is M-1,
   improve_one(RVars,Doms,Relns,NAss,99999,NAss,BVal2,BAss2),
   it_improve_new(NAss,Doms,Relns,Vars,M1,BVal2,BAss2,SAss).

% update_asst(Assts,Ass,SAss) given a list of assignments Assts,
% and a new assignment Ass returns the updated assignment list SAss.
update_asst([val(Var1,_)|Vals],val(Var,Val),[val(Var,Val)|Vals]) :-
   Var==Var1,!.
update_asst([val(Var1,Val1)|Vals],val(Var,Val),[val(Var1,Val1)|RVals]) :-
   update_asst(Vals,val(Var,Val),RVals).

% finds the best assignment to improve for this iteration
% improve_one(RemVars,             remaining variables to check
%         Doms,                    domains list
%         Relns,                   relations list
%         CurrentTotalAssignment,
%         CurrentBestValue,        
%         CurrentBestAssign,       current best val(Var,Val) to change
%         FinalBestValue,          final best value
%         FinalBestAssign)         val(Var,Val) for the best one to change
improve_one([],_,_,_,BV,BA,BV,BA).
improve_one(Vars,Doms,Relns,CTA,CBV0,CBA0,FBV,FBA) :-
   random_rem(Var,Vars,Vars2),
   domain(Var,Dom,Doms),
   lookup(Var,CTA,Val),
   remove(Val,Dom,RDom),
   check_other_vals_for_var(RDom,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1),
   improve_one(Vars2,Doms,Relns,CTA,CBV1,CBA1,FBV,FBA).

% check_other_vals_for_var(RDom,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1)
% checks the values RDom for variable Var
% Relns is the list of relations
% CTA is the current total assignment
% CBV0 is the previous best value
% CBA0 is the previous best assignment
% CBV1 is the final best value
% CBA1 is the final best assignment
check_other_vals_for_var([],_,_,_,CBV,CBA,CBV,CBA).
check_other_vals_for_var(Vals,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1) :-
   random_rem(Val,Vals,RVals),
   number_unsat_relns(Relns,[val(Var,Val)|CTA],Num),
   ( Num < CBV0
   -> check_other_vals_for_var(RVals,Var,Relns,CTA,Num,
                   val(Var,Val),CBV1,CBA1)
   ; check_other_vals_for_var(RVals,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1)
   ).
   
% number_unsat_relns(Relns,Asst,N) means N is the number of unsatisfied
% relations of Relns using assignment Asst
number_unsat_relns([],_,0).
number_unsat_relns([rel([X,Y],R)|Rels],Asst,N) :-
    number_unsat_relns(Rels,Asst,N0),
    (\+ (val_in_asst(X,Asst), val_in_asst(Y,Asst), R) 
    -> N is N0+1
     ; N=N0).

% domain(Var,Dom,Doms) is true if Dom is the domain of variable Var in Doms
domain(Var,Dom,[dom(Var1,Dom)|_]) :- Var==Var1,!.
domain(Var,Dom,[_|Doms]) :-
   domain(Var,Dom,Doms).

% val_in_asst(Var,Assignment) unifies Var with its value in Assignment
val_in_asst(Var,[val(Var1,Val1)|_]) :-
   Var==Var1,!,Var=Val1.
val_in_asst(Var,[_|Ass]) :-
   val_in_asst(Var,Ass).

% lookup(Var,Assignment,Val) unifies Var with its value in Assignment
lookup(Var,[val(Var1,Val1)|_],Val) :-
   Var==Var1,!,Val=Val1.
lookup(Var,[_|Ass],Val) :-
   lookup(Var,Ass,Val).

% rem_id(El,Lst,Rem) is true if Rem is the list remaining
% from removing the element of Lst that is identical to El.
rem_id(Var1,[Var|Vars],RVars) :-
   Var==Var1, !, RVars=Vars.
rem_id(Var1,[Var|Vars],[Var|RVars]) :-
    rem_id(Var1,Vars,RVars).

% vars(Doms,Vars) is true if Vars is the list of variables in Doms
vars([],[]).
vars([dom(X,_)|Ds],[X|Vs]) :-
   vars(Ds,Vs).

%%%% INTERFACE TO STANDARD CSP SETUP %%%%

% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
csp(Doms,Relns) :-
   gsat(Doms,Relns,20,20,Ans),      % 20 20 is arbitrary setting
   set_all(Ans).

% set_all(Asst) sets all of the variables in Asst to their values
set_all([]).
set_all([val(X,X)|R]) :-
   set_all(R).

