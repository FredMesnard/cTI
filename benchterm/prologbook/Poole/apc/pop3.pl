% PARTIAL ORDER PLANNER using STRIPS REPRESENTATION
% This assumes the underlying Prolog contains delaying - in particular
%    the when construct

% Definitions:
% An agenda is a list of goals.
% A goal is of the form goal(P,NA) where P is a proposition 
%   that is a precondition of action instance index NA.
% A plan is of the form plan(As,Os,Ls) where 
%   As is a list of actions instances,
%      An action instance is of the form act(N,A) 
%         where N is an integer and A is an action
%        (this is needed in case there are multiple 
%        instances of the same action). 
%        N is the action instance index.
%   Os is a list of ordering constraints (A1<A2) 
%       where A1 & A2 are action instance indexes.
%   Ls is a list of causal links.
% A causal link is of the form cl(A0,P,A1) 
%   where A0 and A1 are action instance indexes,
%   and P is a proposition that is a precondition of 
%   action A1 --- means that A0 is making P true for A1.

% `<-' is the object-level `if'
:- op(1150, xfx, <- ).
% `&' is the object level conjunction.
:- op(950,xfy, &).
% `\=' is the object level not equal.
:- op(700,xfx, \=).

% pop(CPlan, Agenda, FPlan,DB) is true if
%  CPlan is the current plan
%  Agenda is the current agenda 
%  FPlan is the final plan (with all subgoals supported)
%  and there are DB or fewer actions in the plan 
%        -- DB is the depth-bound for the plan size.

pop(Plan,[],Plan,_).
pop(CPlan,Agenda,FPlan,DB) :-
   solve_a_goal(Agenda,CPlan,NPlan,NAgenda,DB,NDB),
   pop(NPlan,NAgenda,FPlan,NDB).


% solve_a_goal(Agenda,CPl,NPl,NAg,DB,DB1) 
%    chooses an action to solve a goal in Agenda, 
%    updating plan CPl to NPl and agenda to NAg,
%    and updating depth bound DB to depth bound DB1

% CASE 0: inequality goal
solve_a_goal([goal((X \= Y),_)|Ag],Pl,Pl,Ag,DB,DB)  :- 
   !,
   when(?=(X,Y), X\==Y).

% CASE 1: conjunction
solve_a_goal([goal((A & B),GG)|Ag],P,P,[goal(A,GG),goal(B,GG)|Ag],DB,DB)  
   :- !.

solve_a_goal([goal(true,_)|Ag],Pl,Pl,Ag,DB,DB) :- !.

solve_a_goal([goal(H,GG)|Ag],P,P,[goal(B,GG)|Ag],DB,DB)  :- 
   \+ primitive(H),!,
   (H <- B). 

% CASE 1: use existing action
solve_a_goal([goal(P,A1)|Ag],
           plan(As,Os,Ls),
           plan(As,NOs,[cl(N0,P,A1)|Ls]),
           Ag,DB,DB) :-
   member(act(N0,Act0),As),
   achieves(Act0,P),
   add_constraint(N0<A1,Os,Os1) ,
   incorporate_causal_link(cl(N0,P,A1),As,Os1,NOs).

% CASE 2: add new action. 
%   Note that DB acts as the index of the new action instance.
solve_a_goal([goal(P,A1)|Ag],
           plan(As,Os,Ls),
           plan([act(DB,Act0)|As],NOs,[cl(DB,P,A1)|Ls]),
           Ag1,
           DB,NDB) :-
   DB>0,
   achieves(Act0,P),
   writeln(['*** new action ',act(DB,Act0), 
            ' to achieve ',P,' for ',A1]),
   add_constraint(DB<A1,Os,Os1),
   add_constraint(start<DB,Os1,Os2),
   incorporate_action(act(DB,Act0),Ls,Os2,Os3),
   incorporate_causal_link(cl(DB,P,A1),As,Os3,NOs),
   NDB is DB-1,
   append(Ag,[goal(poss(Act0),DB)],Ag1).


% add_constraint(A0<A1,Os,Os1) adds ordering constraint  
%    A0<A1 to partial ordering Os producing ordering Os1.
% Fails if A0<A1 is inconsistent with Os.
% We represent partial orderings as their transitive closure.
add_constraint(C,L,L) :- 
    member(C,L),
    !.                 % green cut for efficiency only
add_constraint(A0<A1,L1,L2) :-
   A0 \== A1,
%   \+ member(A0<A1,L1),    
                % omitted because of the cut.
   \+ member(A1<A0,L1),
   add_constraint1(A0<A1,L1,L1,L2).

% add_constraint1(A0<A1,Os,AOs,Os1) adds constraint A0<A1 
%    partial ordering Os is the remaining orderings to be checked
%    AOs is the list of all orderings
%    Os1 is the final set of orderings
add_constraint1(A0<A1,[],AOs,NOs) :-
    insert(A0<A1,AOs,NOs).
add_constraint1(A0<A1,[A1<A2|R],AOs,NR) :-
   A0 \== A2,
   insert(A0<A2,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A0|R],AOs,NR) :-
   A1 \== A2,
   insert(A2<A1,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A3|R],AOs,NR) :-
   A0 \== A3,
   A1 \== A2,
   add_constraint1(A0<A1,R,AOs,NR).

% incorporate_causal_link(CL, As, Os, NOs)
%  incorporates causal link CL to links Ls 
%  producing new links NLs, and updating
%  partial ordering Os to NOs. 
incorporate_causal_link(_,[],Os,Os).
incorporate_causal_link(CL,[A|RAs],Os,NOs) :-
   protect(CL,A,Os,Os1),
   incorporate_causal_link(CL,RAs,Os1,NOs).

% incorporate_action(A,Ls,Os,NOs)
% incorporates action A into list Ls is causal lists
% updates partial ordering Os to NOs
% fails if the action cannot be reordered
incorporate_action(_,[],Os,Os).
incorporate_action(A,[CL|Ls],Os,NOs) :-
   protect(CL,A,Os,Os1),
   incorporate_action(A,Ls,Os1,NOs).

% protect(Cl,Action,Os0,Os1) protects 
%  causal link CL from Action if necessary
protect(cl(A0,_,_),act(A0,_),Os,Os) :- !.
protect(cl(A0,_,_),act(NA,_),Os,Os) :-
   member(NA<A0,Os),!.
protect(cl(_,_,A1),act(A1,_),Os,Os) :- !.
protect(cl(_,_,A1),act(NA,_),Os,Os) :-
   member(A1<NA,Os), !.
protect(cl(_,P,_),act(_,A),Os,Os) :-
%  NA \== A0, \+ member(NA<A0,Os), NA \== A1, \+ member(A1<NA,Os), 
     % deleted because of cuts
   when(ground((A,P)), \+ deletes(A,P)).
%    \+ deletes(A,P).
protect(cl(A0,P,A1),act(NA,A),Os,Os1) :-
%  NA \== A0, \+ member(NA<A0,Os), NA \== A1, \+ member(A1<NA,Os), 
     % deleted because of cuts
   deletes(A,P),
   enforce_order(A0,NA,A1,Os,Os1).

%enforce_order(A0,A,A1,Os,Os1) extends Os to 
% ensure that A<A0 or A>A1 in Os1
enforce_order(_,A,A1,Os,Os1) :-
   add_constraint(A1<A,Os,Os1),
   writeln(['   ... adding constraint ',A,' after ',A1]).
enforce_order(A0,A,_,Os,Os1) :-
   add_constraint(A<A0,Os,Os1),
   writeln(['   ... adding constraint ',A,' before ',A0]).

% =====================================

solve(Goals,Plan,DB) :-
   pop(plan([act(finish,end),act(start,init)],[start<finish],[]),
       [goal(Goals,finish)],Plan,DB).

% seq(Plan,Seq) extracts a legal sequence Seq from Plan
seq(plan([],_,_),[]).
seq(plan(As,Os,_),[A|P]) :-
   remove(act(N,A),As,As1),
   \+ (member(act(N1,_),As1), member(N1<N,Os)),
   seq(plan(As1,Os,_),P).

% ======================================================

% TRY THE FOLLOWING QUERIES with pop_t.pl:
% solve(carrying(rob,k1),P,3), seq(P,S).
% solve(sitting_at(k1,lab2),P,7), seq(P,S).
% solve((carrying(rob,parcel) & sitting_at(rob,lab2)),P,9), seq(P,S).
% solve((sitting_at(rob,lab2) & carrying(rob,parcel)),P,9), seq(P,S).
