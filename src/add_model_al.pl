:- module(add_model_al,[add_model_after_litt/5]).

:- use_module(utils).
:- use_module(db).
:- use_module(compat_swi).

add_model_after_litt(LPRs1,Db,IdModel,Sccs,TO) :-
	add_model0(LPRs1,Db,IdModel,Sccs,TO).

add_model0([],_Db,_IdModel,[],_TO).
add_model0([Ps|Pss],Db,IdModel,[Qs|Qss],TO) :-
	add_model1(Ps,Db,IdModel,Qs,TO),
	add_model0(Pss,Db,IdModel,Qss,TO).

add_model1([],_Db,_IM,[],_TO).
add_model1([P/N-Rs|LPRs],Db,IdM,[P/N-Qs|Qss],TO) :-
    add_model2(Rs,P,N,Db,IdM,Qs,TO),
	add_model1(LPRs,Db,IdM,Qss,TO).
	
add_model2([],_P,_N,_Db,_IM,[],_TO).
add_model2([Ref|Rs],P,N,Db,IdM,[Cl|Cls],TO) :-
	clause_head(Ref,H),
	clause_body(Ref,Bs),
	specialize(Bs,Db,IdM,Bs2),
	%(IdModel==modelb -> trace ; true),
	simplify(Bs2,IdM,Bs3,TO),
    %Bs3 = Bs2,
	build_clause(H,Bs3,Cl),
	add_model2(Rs,P,N,Db,IdM,Cls,TO).
	
specialize([],_Db,_IdM,[]).
specialize(['$predef'(P)|Bs],Db,IdM,['$predef'(P)|Bs2]) :- !, 
    specialize(Bs,Db,IdM,Bs2).
specialize(['$constraint'(Cs)|Bs],Db,IdM,['$constraint'(Cs)|Bs2]) :- !, 
    specialize(Bs,Db,IdM,Bs2).
%specialize([B],_Db,_IdM,[B]) :- !.
specialize([B|Bs],Db,IdM,[B,'$constraint'(Cs)|Bs2]) :-
	functor(B,P,N), B=..[P|Vars],
	get_db(P,N,IdM,Db,Vars-Cs-_Prec-_Ite),
	specialize(Bs,Db,IdM,Bs2).

simplify([],_IdM,[],_TO).
simplify(['$predef'(P)|Bs],IdM,['$predef'(P)|Cs],TO) :- !, 
    simplify(Bs,IdM,Cs,TO).
simplify(['$constraint'(A0s)|Cs],IdM,['$constraint'(A2s)|Ds],TO) :-!,
    consecutive_constraints(Cs,IdM,A0s,A1s,RemainingBody),
    simplify_constraint(IdM,A1s,A2s,TO),
	simplify(RemainingBody,IdM,Ds,TO).
simplify([B|Bs],IdM,[B|Cs],TO) :- simplify(Bs,IdM,Cs,TO).


    consecutive_constraints(_Cs,IdM,A0s,A1s,RemainingBody) :-
         (IdM=modeln -> \+ (num_op:satisfiable(A0s)) ; \+ (bool_op:satisfiable(A0s))),
         %unsatisfiable(IdM,A0s), 
         !, 
         (IdM=modeln -> num_op:false(A1s) ; bool_op:false(A1s)),
         RemainingBody = [].
    consecutive_constraints(Cs,IdM,A0s,A1s,RemainingBody) :-
        consecutive_constraints_sat(Cs,IdM,A0s,A1s,RemainingBody).
          
    consecutive_constraints_sat([],_IdM,As,As,[]).
    consecutive_constraints_sat(['$constraint'(Bs)|Body],IdM,As,Ds,RBody) :- !, 
        conjunction(IdM,Bs,As,Cs), 
        consecutive_constraints(Body,IdM,Cs,Ds,RBody).
    consecutive_constraints_sat(['$predef'(P)|Body],IdM,As,Ds,['$predef'(P)|RBody]) :- !, 
        consecutive_constraints(Body,IdM,As,Ds,RBody).
    consecutive_constraints_sat([B|Body],_IdM,As,As,[B|Body]).
        
        conjunction(modeln,Bs,As,Cs) :- num_op:conjunction(Bs,As,Cs).
        conjunction(modelb,Bs,As,Cs) :- bool_op:conjunction(Bs,As,Cs).


    simplify_constraint(modeln,A1s,A2s,TimeOut) :- num_op:simplify_constraint(A1s,A2s,TimeOut).
    simplify_constraint(modelb,A1s,A2s,TimeOut) :- bool_op:simplify_constraint(A1s,A2s,TimeOut).

