% Copyright (C) 2003 Fred Mesnard <fred@univ-reunion.fr>
%
% This file is part of cTI.
%
% cTI is free software; you can redistribute it and/or modify it
% under the terms of the GNU General Public License as published by the
% Free Software Foundation; either version 2 of the License, or (at your
% option) any later version.
%
% cTI is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
% USA.
%
% For the most up-to-date information see the cTI's web site:
% http://www.cs.unipr.it/cTI/

:- module(predef,[predef_nat_bool_tc/4,predef/1,predef_iso/1,predef_include/1]).

:- dynamic(predef_include/4).

predef_nat_bool_tc(Predef,Eqns,BoolTerm,TermCond) :-
	predef0(Predef,Cs,Bool,TermCond),!,
	BoolTerm=Bool,Eqns=Cs.
predef_nat_bool_tc(X,_,_,_) :-
	throw(cti_exception(predef,unknown_predef(X))).

predef(At) :-
	nonvar(At),
	functor(At,P,N),
	functor(AtP,P,N),
	predef0(AtP,_,_,_),!.
predef(At) :-
	var(At),
	predef0(At,_,_,_).

% predef which are included have priority
predef0(At,N,B,T) :- predef_include(At,N,B,T),!.
predef0(At,N,B,T) :- predef_iso(At,N,B,T).

predef_iso(A) :- predef_iso(A,_,_,_),!.
predef_include(A) :- predef_include(A,_,_,_),!.


% predef_iso(AtPredefProlog,ContraintesEquivNum,ContraintesEquivBool,CondTerm)
% nb : les args de At sont toujours des variables, sauf pour '$bool'/1 et '$num'/1


%---------------------------------------------------------------------------------
% Prolog: The Standard, Deransart et al., Springer,  p. 261
%----------------------------------------------------------

% all solutions
predef_iso(	setof(_,_G,_L),		     [],1,1). % ((G,false);true)
predef_iso(	findall(_,_,_),		     [],1,1).
predef_iso(	bagof(_,_,_),		     [],1,1).

% arithmetic comparison & evaluation
predef_iso( X is Y,   [X=0],X*Y,1). 
predef_iso( X >  Y,   [],X*Y,1).
predef_iso( X >= Y,   [],X*Y,1).
predef_iso( X <  Y,   [],X*Y,1).
predef_iso( X =< Y,   [],X*Y,1).
predef_iso( X =:=Y,   [],X*Y,1).
predef_iso( X =\=Y,   [],X*Y,1).

% atomic term processing
predef_iso( atom_chars(X,Y),             [X=0,Y>=0],X*Y,1).
predef_iso( atom_codes(X,Y),             [X=0,Y>=0],X*Y,1).
predef_iso( atom_concat(X,Y,Z),          [X=0,Y=0,Z=0],X*Y*Z,1).
predef_iso( atom_length(X,L),            [X=0,L>=0],X*L,1).
predef_iso( char_code(Char,Code),        [Char=0,Code=0],Char*Code,1).
predef_iso( number_chars(N,Chars),       [N=0,Chars>=1],N*Chars,1).
predef_iso( number_codes(N,Codes),       [N=0,Codes>=1],N*Codes,1).
predef_iso( sub_atom(A,I,J,K,B),         [A=0,B=0,I=0,J=0,K=0],A*B*I*J*K,1).

% byte input/output
predef_iso( get_byte(B),                 [B=0],B,1).
predef_iso( get_byte(SA,B),              [B=0],SA*B,1).
predef_iso( peek_byte(C),                [C=0],C,1).
predef_iso( peek_byte(SA,B),             [B=0],SA*B,1).
predef_iso( put_byte(B),                 [B=0],B,1).
predef_iso( put_byte(SA,B),              [B=0],SA*B,1).
      
% char input/output
predef_iso( get_char(C),                 [C=0],C,1). 
predef_iso( get_char(SA,C),              [C=0],SA*C,1).
predef_iso( get_code(C),                 [C=0],C,1).
predef_iso( get_code(SA,C),              [C=0],SA*C,1).
predef_iso( peek_char(C),                [C=0],C,1).
predef_iso( peek_char(SA,C),             [C=0],SA*C,1).
predef_iso( peek_code(C),                [C=0],C,1).
predef_iso( peek_code(SA,C),             [C=0],SA*C,1).
predef_iso( put_char(C),                 [C=0],C,1).
predef_iso( put_char(SA,C),              [C=0],SA*C,1).
predef_iso( put_code(C),                 [C=0],C,1).
predef_iso( put_code(SA,C),              [C=0],SA*C,1).
predef_iso( nl,                          [],1,1).
predef_iso( nl(SA),                      [],SA,1).

% clause retrieval & information      
predef_iso( clause(_,_),	             [],1,1).
predef_iso( current_predicate(PI),       [PI=1],PI,1).

% clause creation & destruction
predef_iso( abolish(PI),                 [],PI,1).
predef_iso( asserta(_),                  [],1,1).
predef_iso( assertz(_),                  [],1,1). 
predef_iso( retract(_),                  [],1,1). 

% flag updates
predef_iso( current_prolog_flag(_,_),    [],1,1).
predef_iso( set_prolog_flag(_,_),        [],1,1).

% logic & control
predef_iso( !,			                 [],1,1).
predef_iso( true,			             [],1,1).
predef_iso( fail,			             [0=1],0,1).
predef_iso( repeat,		                 [],1,0). 
predef_iso( call(_),		             [],1,1).
predef_iso( \+(_),  		             [],1,1).
predef_iso( halt,                        [0=1],0,1).
predef_iso( halt(_),                     [0=1],0,1).
predef_iso( once(_),                     [],1,0).
predef_iso( ','(_,_),                    [],1,0).
predef_iso( ';'(_,_),                    [],1,0).
predef_iso( '->'(_,_),                   [],1,0).
predef_iso( catch(_,_,_),                [],1,0).
predef_iso( throw(_),                    [0=1],0,0).

% stream selection & control
predef_iso( at_end_of_stream,            [],1,1).
predef_iso( at_end_of_stream(_),         [],1,1).
predef_iso( close(_),                    [],1,1).
predef_iso( current_input(_),            [],1,1).
predef_iso( current_input(_,_),          [],1,1).
predef_iso( flush_output,                [],1,1).
predef_iso( flush_output(_),             [],1,1).
predef_iso( open(_,_,_),                 [],1,1).
predef_iso( open(_,_,_,_),               [],1,1).
predef_iso( set_input(_),                [],1,1).
predef_iso( set_output(_),               [],1,1).
predef_iso( set_stream_position(_,_),    [],1,1).
predef_iso( stream_property(_,_),        [],1,1).

% term comparison
predef_iso( X == Y,        [X=Y],(X=:=Y),1).
predef_iso( _ \== _,       [],1,1).
predef_iso( _ @< _,        [],1,1).
predef_iso( _ @=< _,       [],1,1).
predef_iso( _ @> _,        [],1,1).
predef_iso( _ @>= _,       [],1,1).

% term creation & decomposition
predef_iso(	functor(_T,F,N),	     [F=0,N=0],F*N,1).
predef_iso(	arg(N,T,A),     	     [N=0,T >= A+1, A>=0],N*(T =< A),1).
%predef_iso(	X  =.. Y,		     [N+X>=Y,Y >=X+1,X>=0],(X =:= Y),1) :- current_prolog_flag(max_arity,N).
predef_iso(	X  =.. Y,		         [Y >=0,X>=0],(X =:= Y),1).
predef_iso(	copy_term(_X,_Y),	     [],1,1). 

% term unification
predef_iso(	X = Y,		                 [X=Y],(X=:=Y),1).
%predef_iso( '\='(_,_),                   [],1,1). % sinon bug sur =
predef_iso(unify_with_occurs_check(X,Y), [X=Y],(X=:=Y),1).

% term testing
predef_iso(	var(_),			     [],1,1).
predef_iso(	nonvar(_),		     [],1,1).
predef_iso(	compound(X),	     [X >= 1],1,1).
predef_iso(	atomic(X),		     [X = 0],X,1).
predef_iso(	atom(X),		     [X = 0],X,1).
predef_iso(	number(X),		     [X = 0],X,1).
predef_iso(	float(X),		     [X = 0],X,1).
predef_iso(	integer(X),		     [X = 0],X,1).

% term input/ouput
predef_iso( char_conversion(X,Y),               [X=0,Y=0],X*Y,1).
predef_iso( current_char_conversion(X,Y),       [X=0,Y=0],X*Y,1).
predef_iso( current_op(_,_,_),      	        [X=0,Y=0,Z=0],X*Y*Z,1).
predef_iso( op(_,_,_),                          [],1,1).
predef_iso( read(_),        		            [],1,1).
predef_iso( read(_,_),                          [],1,1).
predef_iso( read_term(_),                       [],1,1).
predef_iso( read_term(_,_),                     [],1,1).
predef_iso( write(_),       		            [],1,1).
predef_iso( write(_,_),                         [],1,1).
predef_iso( write_canonical(_),                 [],1,1).
predef_iso( write_canonical(_,_),               [],1,1).
predef_iso( write_term(_,_),                    [],1,1).
predef_iso( write_term(_,_,_),                  [],1,1).
predef_iso( writeq(_),      		            [],1,1).
predef_iso( writeq(_,_),                        [],1,1).

% Flags (Prolog: The Standard, p. 215--219)
predef_iso(current_prolog_flag(X,Y),  [X=0,Y=0],X*Y,1).
predef_iso(set_prolog_flag(X,Y),      [X=0,Y=0],X*Y,1).

% Directives (should not appear inside clauses, but this is not checked)
predef_iso(discontiguous(_PI),    _,_,_).
predef_iso(dynamic(_PI),          _,_,_).
predef_iso(multifile(_PI),        _,_,_).
predef_iso(ensure_loaded(_Ptext), _,_,_).
predef_iso(include(_Ptext),       _,_,_).
predef_iso(initialization(_G),    _,_,_).

%------------------------------------------------------------------------
% internal to cTI
predef_iso( '$bool'(X),                         [],X,1).  
predef_iso( '$num'(X),                           X,1,1).  
predef_iso( '$term_cond'(TC),                   [],1,TC).
predef_iso( '$constraint'(_),                   [],1,1).
%------------------------------------------------------------------------



