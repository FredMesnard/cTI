:- module(lm_nat_bool,[lm_nat_bool/2]).

:- use_module(library(lists),[append/3]).
:- use_module(library(clpq)).
:- use_module(utils).

% [  [s/2]-[[s/2-[0,1,0]],[s/2-[0,0,1]]],
%    [p/2,q/2]-[[p/2-[1,0,2],q/2-[0,0,2]],[p/2-[1,2,0],q/2-[0,2,0]]]  ]
%    [p/1,q/1]-false

lm_nat_bool([],[]).
lm_nat_bool([Scc-false|SccLms],Blms) :-
	!,lm_false(Scc,Blms0),
	append(Blms0,Blms1,Blms),
	lm_nat_bool(SccLms,Blms1).
lm_nat_bool([Scc-Lms|SccLms],Blms) :-
	process1(Scc,Lms,Blms0),
	append(Blms0,Blms1,Blms),
	lm_nat_bool(SccLms,Blms1).
	
lm_false([],[]).
lm_false([P/N|Ps],[P/N-(Vars-0)|Lmb]) :-
	length(Vars,N),
	lm_false(Ps,Lmb).

process1([],_,[]).
process1([P/N|Ps],Lms1,[P/N-(Vars-Cb)|Lmb]) :-
	length(Vars,N),
	process2(Lms1,Lms2,Vars,Cb),
	process1(Ps,Lms2,Lmb).

process2([],[],_,0).
process2([M|Lm],Lms2,Vars,Cb1+Cb2) :-
	M=[_-[_|Mp]|Ms],
	append([Ms],Lms3,Lms2),
	lm_bool(Mp,Vars,Cb1),
	process2(Lm,Lms3,Vars,Cb2).

lm_bool([],[],1).
lm_bool([Mui|Muis],[_|Xs],Cb) :- {Mui=0},!,lm_bool(Muis,Xs,Cb).
lm_bool([Mui|Muis],[X|Xs],X*Cb) :- {Mui > 0},!,lm_bool(Muis,Xs,Cb).
lm_bool([Mui|Muis],[_X|Xs],Cb) :- {Mui < 0},lm_bool(Muis,Xs,Cb).







