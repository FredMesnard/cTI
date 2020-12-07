/*
:- type bool_exp --> and(bool_exp,bool_exp);
					 or(bool_exp,bool_exp);
					 0;
					 1.
*/

%:- pred dis(bool_exp).
dis(or(B1,B2)) :- con(B1),dis(B2).
dis(B) :- con(B).

%:- pred con(bool_exp).
con(and(B1,B2)) :- dis(B1),con(B2).
con(B) :- bool(B).

%:- pred bool(bool_exp).
bool(0).
bool(1).
