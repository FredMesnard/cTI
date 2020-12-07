/*   Credit Evaluation

     credit(Client,Answer) :-
	Answer is the reply to a request by Client for credit.
*/
      credit(Client,Answer) :-
           ok_profile(Client),
           collateral_rating(Client,CollateralRating),
           financial_rating(Client,FinancialRating),
           bank_yield(Client,Yield),
           evaluate(profile(CollateralRating,FinancialRating,Yield),Answer) , !.

/*  The collateral rating module

     collateral_rating(Client,Rating) :-
	Rating is a qualitative description assessing the collateral
	offered by Client to cover the request for credit.
*/
     collateral_rating(Client,Rating) :-
        collateral_profile(Client,FirstClass,SecondClass,Illiquid),
        collateral_evaluation(FirstClass,SecondClass,Illiquid,Rating).

     collateral_profile(Client,FirstClass,SecondClass,Illiquid) :-
	requested_credit(Client,Credit),
	collateral_percent(first_class,Client,Credit,FirstClass),
	collateral_percent(second_class,Client,Credit,SecondClass),
	collateral_percent(Illiquid,Client,Credit,Illiquid).

     collateral_percent(Type,Client,Total,Value) :-
%TRANSFORM
%	findall(X,(collateral(Collateral,Type),
%			amount(Collateral,Client,X)),Xs),
        collateral(Collateral,Type),
        amount( Collateral,Client,X),
%  	sumlist(Xs,Sum),
        sumlist([X],Sum),	
%END TRANSFORM
        Value is Sum*100/Total.

     /*   Evaluation rules    */

    collateral_evaluation(FirstClass,SecondClass,Illiquid,excellent) :-
  	FirstClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,Illiquid,excellent) :-
  	FirstClass > 70, FirstClass + SecondClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,Illiquid,good) :-
  	FirstClass + SecondClass > 60,
  	FirstClass + SecondClass < 70,
  	FirstClass + SecondClass + Illiquid >= 100.

     /*  Bank data - classification of collateral   */

     collateral(local_currency_deposits,first_class).
     collateral(foreign_currency_deposits,first_class).
     collateral(negotiate_instruments,second_class).
     collateral(mortgage,illiquid).

%    Financial rating

/*  
    financial_rating(Client,Rating) :-
	Rating  is a qualitative description assessing the financial 
	record offered by Client to support the request for credit.
*/	
     financial_rating(Client,Rating) :-
        financial_factors(Factors),
        score(Factors,Client,0,Score),
        calibrate(Score,Rating).

     /*   Financial evalauation rules   */

     calibrate(Score,bad) :- 	   Score =< -500.
     calibrate(Score,medium) :-    -500 < Score, Score < 150.
     calibrate(Score,good) :- 	   150 =< Score, Score < 1000.
     calibrate(Score,excellent) :- Score >= 1000.

     /*  Bank data - weighting factors	*/

     financial_factors([(net_worth_per_assets,5),
         (last_year_sales_growth,1),
         (gross_profits_on_sales,5),
         (short_term_debt_per_annual_sales,2)  ]).

     score([(Factor,Weight)|Factors],Client,Acc,Score) :-
        value(Factor,Client,Value),
        Acc1 is Acc + Weight*Value,
        score(Factors,Client,Acc1,Score).
     score([],Client,Score,Score).

/*  Final evaluation    

     evaluate(Profile,Outcome) :-
	Outcome is the reply to the client's Profile.
*/
     evaluate(Profile,Answer) :- 
	rule(Conditions,Answer), verify(Conditions,Profile).

     verify([condition(Type,Test,Rating)|Conditions],Profile) :-
        scale(Type,Scale),
        select_value(Type,Profile,Fact),
        compare(Test,Scale,Fact,Rating),
        verify(Conditions,Profile).
     verify([],Profile).

     compare('=',Scale,Rating,Rating).
     compare('>',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2).
%     compare('>=',Scale,Rating1,Rating2) :-
%        precedes(Scale,Rating1,Rating2) ; Rating1 = Rating2.
     compare('>=',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2).
      compare('>=',Scale,Rating1,Rating2) :-
         Rating1 = Rating2.
  
    compare('<',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1).
%     compare('=<',Scale,Rating1,Rating2) :-
%        precedes(Scale,Rating2,Rating1) ; Rating1 = Rating2.
     compare('=<',Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1).
     compare('=<',Scale,Rating1,Rating2) :-
       Rating1 = Rating2.

     precedes([R1|Rs],R1,R2).
     precedes([R|Rs],R1,R2) :- R \== R2, precedes(Rs,R1,R2).

	select_value(collateral,profile(C,F,Y),C).
	select_value(finances,profile(C,F,Y),F).
	select_value(yield,profile(C,F,Y),Y).

     /*  Utilities   */

	sumlist(Is,Sum) :-
		sumlist(Is,0,Sum).
	sumlist([I|Is],Temp,Sum) :-
		Temp1 is Temp + I,
		sumlist(Is,Temp1,Sum).
	sumlist([],Sum,Sum).


/*  Bank data and rules	*/

rule([condition(collateral,'>=',excellent),condition(finances,'>=',good),
			condition(yield,'>=',reasonable)],give_credit).  
rule([condition(collateral,'=',good),condition(finances,'=',good),
			condition(yield,'>=',reasonable)],consult_superior).
rule([condition(collateral,'=<',moderate),condition(finances,'=<',medium)],
							  refuse_credit).

scale(collateral,[excellent,good,moderate]).
scale(finances,[excellent,good,medium,bad]).
scale(yield,[excellent,reasonable,poor]).

%  Program 22.1:  A credit evaluation system
   /*   Client data  */

bank_yield(client1,excellent).
requested_credit(client1,5000).

amount(local_currency_deposits,client1,3000).
amount(foreign_currency_deposits,client1,2000).
amount(bank_guarantees,client1,300).
                     
amount(negotiate_instruments,client1,500).
amount(stocks,client1,900).

amount(mortgage,client1,1200).
amount(documents,client1,1400).

value(net_worth_per_assets,client1,40).
value(last_year_sales_growth,client1,20).
value(gross_profits_on_sales,client1,45).
value(short_term_debt_per_annual_sales,client1,9).

ok_profile(client1).


%  Program 22.2: Test data for the credit evaluation system

/*
    requested_credit/2-[$ref(3048477,0)]
    (rule)/2-[$ref(2946273,2),$ref(2946209,1),$ref(2946465,0)]
    scale/2-[$ref(3056741,2),$ref(2945541,1),$ref(2886229,0)]
    score/4-[$ref(2900961,1),$ref(3048029,0)]
    select_value/3-[$ref(3037941,2),$ref(3038101,1),$ref(3038609,0)]
    sumlist/2-[$ref(3035917,0)]
    sumlist/3-[$ref(3038269,1),$ref(3030233,0)]
    value/3-[$ref(3023129,3),$ref(3030449,2),$ref(3030405,1),$ref(3030361,0)]
    verify/2-[$ref(3048553,1),$ref(3030749,0)]


amount/3:
    amount(A,B,C):- 
        [A=0,B=0,C=1400]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=1200]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=900]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=500]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=300]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=2000]
        []
    amount(A,B,C):- 
        [A=0,B=0,C=3000]
        []

bank_yield/2:
    bank_yield(A,B):- 
        [A=0,B=0]
        []

calibrate/2:
    calibrate(A,B):- 
        [B=0,A>=0]
        []
    calibrate(A,B):- 
        [B=0,A>=0]
        []
    calibrate(A,B):- 
        [B=0,A>=0]
        []
    calibrate(A,B):- 
        [B=0,A>=0]
        []

collateral/2:
    collateral(A,B):- 
        [A=0,B=0]
        []
    collateral(A,B):- 
        [A=0,B=0]
        []
    collateral(A,B):- 
        [A=0,B=0]
        []
    collateral(A,B):- 
        [A=0,B=0]
        []

collateral_evaluation/4:
    collateral_evaluation(A,B,C,D):- 
        [D=0,A>=0,B>=0,C>=0]
        []
    collateral_evaluation(A,B,C,D):- 
        [D=0,A>=0,B>=0,C>=0]
        []
    collateral_evaluation(A,B,C,D):- 
        [D=0,A>=0,B>=0,C>=0]
        []

collateral_percent/4:
    collateral_percent(A,B,C,D):- 
        [A>=0,B>=0,C>=0,D>=0,E=-1+F,F>=1,G>=0,H>=0,-100*(1/C*G)+D=0]
        [collateral(H,A),amount(H,B,E),sumlist(F,G)]

collateral_profile/4:
    collateral_profile(A,B,C,D):- 
        [E=0,F=0,A>=0,B>=0,C>=0,D>=0,G>=0]
        [requested_credit(A,G),collateral_percent(E,A,G,B),collateral_percent(F,A,G,C),collateral_percent(D,A,G,D)]

collateral_rating/2:
    collateral_rating(A,B):- 
        [A>=0,B>=0,C>=0,D>=0,E>=0]
        [collateral_profile(A,C,D,E),collateral_evaluation(C,D,E,B)]

compare/4:
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        []
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        [precedes(B,D,C)]
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        [precedes(B,D,C)]
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        []
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        [precedes(B,C,D)]
    compare(A,B,C,D):- 
        [A=0,B>=0,C>=0,D>=0]
        [precedes(B,C,D)]
    compare(A,B,C,C):- 
        [A=0,B>=0,C>=0]
        []

credit/2:
    credit(A,B):- 
        [A>=0,B>=0,C>=0,D>=0,E=-1-D-C+F,D+C-F=<-1]
        [ok_profile(A),collateral_rating(A,E),financial_rating(A,D),bank_yield(A,C),evaluate(F,B)]

evaluate/2:
    evaluate(A,B):- 
        [A>=0,B>=0,C>=0]
        [rule(C,B),verify(C,A)]

financial_factors/1:
    financial_factors(A):- 
        [A=21]
        []

financial_rating/2:
    financial_rating(A,B):- 
        [C=0,A>=0,B>=0,D>=0,E>=0]
        [financial_factors(E),score(E,A,C,D),calibrate(D,B)]

ok_profile/1:
    ok_profile(A):- 
        [A=0]
        []

precedes/3:
    precedes(A,B,C):- 
        [D>=0,B>=0,C>=0,D-A=<-1]
        [precedes(D,B,C)]
    precedes(A,B,C):- 
        [B>=0,C>=0,B-A=<-1]
        []

requested_credit/2:
    requested_credit(A,B):- 
        [A=0,B=5000]
        []

(rule)/2:
    rule(A,B):- 
        [A=4,B=0]
        []
    rule(A,B):- 
        [A=6,B=0]
        []
    rule(A,B):- 
        [A=6,B=0]
        []

scale/2:
    scale(A,B):- 
        [A=0,B=3]
        []
    scale(A,B):- 
        [A=0,B=4]
        []
    scale(A,B):- 
        [A=0,B=3]
        []

score/4:
    score(A,B,C,C):- 
        [A=0,B>=0,C>=0]
        []
    score(A,B,C,D):- 
        [E>=0,F>=0,B>=0,D>=0,C>=0,G>=0,H>=0,I=-2-F-E+A,F+E-A=<-2,-(I*H)-C+G=0]
        [value(F,B,H),score(E,B,G,D)]

select_value/3:
    select_value(A,B,C):- 
        [A=0,C>=0,C-B=<-1]
        []
    select_value(A,B,C):- 
        [A=0,C>=0,C-B=<-1]
        []
    select_value(A,B,C):- 
        [A=0,C>=0,C-B=<-1]
        []

sumlist/2:
    sumlist(A,B):- 
        [C=0,A>=0,B>=0]
        [sumlist(A,C,B)]

sumlist/3:
    sumlist(A,B,B):- 
        [A=0,B>=0]
        []
    sumlist(A,B,C):- 
        [D=-1+B-E+A,B-E+A>=1,C>=0,B>=0,B-E=<0]
        [sumlist(D,E,C)]

value/3:
    value(A,B,C):- 
        [A=0,B=0,C=9]
        []
    value(A,B,C):- 
        [A=0,B=0,C=45]
        []
    value(A,B,C):- 
        [A=0,B=0,C=20]
        []
    value(A,B,C):- 
        [A=0,B=0,C=40]
        []

verify/2:
    verify(A,B):- 
        [A=0,B>=0]
        []
    verify(A,B):- 
        [C>=0,D>=0,E>=0,F=-2-E-D-C+A,E+D+C-A=<-2,B>=0,G>=0,H>=0]
        [scale(F,G),select_value(F,B,H),compare(E,G,H,D),verify(C,B)]



























Construction, reduction et tri du graphe d'appel ... en 33 ms
Graphe d'appel reduit et trie par ordre croissant (croit en descendant) :
    [bank_yield/2]
    [collateral_evaluation/4]
    [amount/3]
    [collateral/2]
    [sumlist/3]
    [sumlist/2]
    [collateral_percent/4]
    [requested_credit/2]
    [collateral_profile/4]
    [collateral_rating/2]
    [(rule)/2]
    [precedes/3]
    [compare/4]
    [scale/2]
    [select_value/3]
    [verify/2]
    [evaluate/2]
    [calibrate/2]
    [financial_factors/1]
    [value/3]
    [score/4]
    [financial_rating/2]
    [ok_profile/1]
    [credit/2]

Calcul des relations inter-arguments avec precision = 2 ... 
{The debugger will first leap -- showing spypoints (debug)}
Pour [bank_yield/2], precision = 2
    model(bank_yield,2,[A,B],[A=0,B=0])
Pour [collateral_evaluation/4], precision = 2
    model(collateral_evaluation,4,[A,B,C,D],[D=0,A>=0,B>=0,C>=0])
Pour [amount/3], precision = 2
    model(amount,3,[A,B,C],[A=0,B=0,C=<3000,C>=300])
Pour [collateral/2], precision = 2
    model(collateral,2,[A,B],[A=0,B=0])
Pour [sumlist/3], precision = 2
    model(sumlist,3,[A,B,C],[B>=0,B-C=<0,A+B-C>=0])
Pour [sumlist/2], precision = 2
    model(sumlist,2,[A,B],[B>=0,A-B>=0])
Pour [collateral_percent/4], precision = 2
{ERROR: multExp(rat(-100,1)*(rat(1,1)/_32*_35),_44,_45)}
*/
