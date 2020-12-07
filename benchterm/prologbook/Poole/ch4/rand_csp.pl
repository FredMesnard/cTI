% Computational Intelligence: a logical approach. 
% Prolog Code. 
% CSP solver by picking random assignments
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% this assumes that standard.pl and random.pl are also loaded.

% random_csp(Doms,Relns,Maxiter,Asst) chooses at most Maxiter assignments
% at random, until one satisfies Relns.
random_csp(Doms,Relns,_,Asst) :-
   random_assign(Doms,Asst),
   writeln(['Trying Assignment: ',Asst]),
   number_unsat_relns(Relns,Asst,0).
random_csp(Doms,Relns,Maxiter,Asst) :-
   Maxiter>1,
   IterLeft is Maxiter-1,
   random_csp(Doms,Relns,IterLeft,Asst).


% random_assign(Doms,Asst) is true if Asst is a random assignment
% of a value to each variable, given domains Doms
random_assign([],[]).
random_assign([dom(X,D)|Ds],[val(X,E)|Vs]) :-
   random_elt(E,D),
   random_assign(Ds,Vs).

   % number_unsat_relns(Relns,Asst,N) means N is the number of unsatisfied
% relations of Relns using assignment Asst
number_unsat_relns([],_,0).
number_unsat_relns([rel([X,Y],R)|Rels],Asst,N) :-
    number_unsat_relns(Rels,Asst,N0),
    (\+ (val_in_asst(X,Asst), val_in_asst(Y,Asst), R) 
    -> N is N0+1
     ; N=N0).

% val_in_asst(Var,Assignment) unifies Var with its value in Assignment
val_in_asst(Var,[val(Var1,Val1)|_]) :-
   Var==Var1,!,Var=Val1.
val_in_asst(Var,[_|Ass]) :-
   val_in_asst(Var,Ass).


%%%% INTERFACE TO STANDARD CSP SETUP %%%%

% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
csp(Doms,Relns) :-
   random_csp(Doms,Relns,20,Ans),      % 20 is arbitrary setting
   set_all(Ans).

% set_all(Asst) sets all of the variables in Asst to their values
set_all([]).
set_all([val(X,X)|R]) :-
   set_all(R).
