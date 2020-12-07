% added for processing DCG
predef( 'C'(X,Y,Z), [X=1+Y+Z,Y>=0,Z>=0],(X=:=Y*Z),1).
 
% compatibilite C&M, SP, ...

predef(	assert(_),		     [],1,1).
predef(	retractall(_),	             [],1,1).

predef(	recorda(_,_,_),		     [],1,1).
predef(	recordz(_,_,_),		     [],1,1).
predef(	recorded(_,_,_),	     [],1,1).
predef(	erase(_),		     [],1,1).

predef(	false,			     [0=1],0,1).
predef(	dif(_,_),		     [],1,1).

predef(	sort(X,Y),		     [Y>=0,X >= Y],(X =< Y),1). 
predef(	compare(X,_,_),		     [X=0],1,1).
predef(	numbervars(_,_,_),	     [],1,1).
predef(	keysort(_,_),		     [],1,1).

predef(	ground(X),		     [],X,1).

predef(	length(Xs,L),	             [],L,Xs+L).
predef(	statistics(X,Y),             [X=0,Y=2],X*Y,1).

predef(	format(_,_),	             [],1,1).
predef(	display(_),		     [],1,1).
predef(	print(_),		     [],1,1).

predef(	get0(_),		     [],1,1).         
predef(	get0(_,_),		     [],1,1).
predef(	tab(_),			     [],1,1).
predef(	ttynl,			     [],1,1).
predef(	ttyput(_),		     [],1,1).
predef(	name(X,Y),		     [],X*Y,1).

% pour pl2wam.pl
predef( abort,                       [],0,1).
predef( absolute_file_name(X,Y),     [],X*Y,1).
predef(	callable(_),                 [],1,1).
predef( character_count(A,B),        [],A*B,1).
predef( expand_term(_,_),            [],1,1).
predef( current_output(P),           [],P,1).
predef( format(_S,_F,_A),            [],1,1).
predef( line_count(_A,B),            [],B,1).
predef( line_position(_A,B),         [],B,1).
predef( predicate_property(PI,PP),   [PI=1,PP=0],PI*PP,1).
predef( portray_clause(_),           [],1,1).

% pour lptp.pl
predef( consult(L),                  [],L,1).
predef(	get(C),		             [],C,1).
predef( tmp_clause(A,B,C,D),         [A=1,B>=1,C>=1,D=1],A*B*C*D,1).
predef( tmp_predicate_name(A),       [A=1],A,1).

% pour slice-all.pl
predef(	'$top'(_),		     [],1,1).
predef(	fileerrors,		     [],1,1).
predef(	nofileerrors,		     [],1,1).
predef( prompt(O,N),                 [],O*N,1).
predef(	see(F),		             [],F,1).
predef(	seeing(F),	             [],F,1).
predef(	seen,	         	     [],1,1).
predef(	tell(F),	       	     [],F,1).
predef(	telling(F),	       	     [],F,1).
predef(	told,        	       	     [],1,1).
predef(	ttyflush,	       	     [],1,1).
predef(	unix(F),	       	     [],F,1).

% pour petsan.pl
predef(	nogc,    	       	     [],1,1).
predef(	put(C),	       	             [],C,1).

% pour caslog.pl
predef( term_variables(_,_),         [],1,1).
