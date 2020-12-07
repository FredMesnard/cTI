file_termconds(FileName,TermConds) :- 
	open(FileName,read,Stream),
	read(Stream,Clause),
	file_clauses(Clause,Stream,[],L,[FileName]),
	close(Stream),
	program_termination_conditions(L,TermConds).

file_clauses(end_of_file,_S,L,L,_F) :- !.
file_clauses((:-  op(Int,Opsp,Ops)),Stream,L1,L2,Fs) :- !,
    op(Int,Opsp,Ops),
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses((:-  discontiguous(_PIs)),Stream,L1,L2,Fs) :- !,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses((:-  dynamic(_PIs)),Stream,L1,L2,Fs) :- !,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses((:-  multifile(_PIs)),Stream,L1,L2,Fs) :- !,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses((:-  initialization(G)),Stream,L1,L2,Fs) :- !,
	read(Stream,Clause),Clause_exp=Clause,
	(current_cti_flag(print_info,yes) -> (write('% Added clause: '),write('$initialization :- '),write(G),writeln('.')) ; true),
	file_clauses(Clause_exp,Stream,[('$initialization' :- G)|L1],L2,Fs).
file_clauses((:-  include(File)),Stream,L1,L2,Fs) :- !,
        current_cti_flag(process_include_ensure_loaded,Flag), % on/off
	file_clauses_aux(Flag,File,Stream,L1,L2,Fs).
file_clauses((:-  ensure_loaded(File)),Stream,L1,L2,Fs) :- !,
        current_cti_flag(process_include_ensure_loaded,Flag), % on/off
	file_clauses_aux(Flag,File,Stream,L1,L2,Fs).
file_clauses((:- include_predef(File)),Stream,L1,L2,Fs) :- !,
        include_predef(File),
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,0,_).
file_clauses((:-  D),Stream,L1,L2,Fs) :- !,
        write('% Unknown skipped directive: '),write(D),nl,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses(Clause,Stream,L1,L2,Fs) :- 
	read(Stream,Clause2),Clause2_exp=Clause2,
	file_clauses(Clause2_exp,Stream,[Clause|L1],L2,Fs).

file_clauses_aux(off,File,Stream,L1,L2,Fs) :-
	write('% '),write(File),write(' will not be included'),nl,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses_aux(on,File,Stream,L1,L2,Fs) :-
	member(File,Fs),!,
	write('% '),write(File),write(' already included'),nl,
	read(Stream,Clause),Clause_exp=Clause,
	file_clauses(Clause_exp,Stream,L1,L2,Fs).
file_clauses_aux(on,File,Stream,L1,L,Fs) :-	
	    open(File,read,Stream2),
	    read(Stream2,Clause),
	    Clause_exp=Clause,
	    file_clauses(Clause_exp,Stream2,L1,L2,[File|Fs]),
	    close(Stream2),
	write('% '),write(File),write(' included'),nl,
	read(Stream,Clause2),Clause2=Clause_exp2,
	file_clauses(Clause_exp2,Stream,L2,L,[File|Fs]).


%% For non-strict ISO-Prolog
%% we expand DCG.
file_expand_termconds(FileName,TermConds) :-
	open(FileName,read,Stream),
	read(Stream,Clause),
	expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,[],L,[FileName],0,N),
	close(Stream),
	write('% '),write(N),write(' clauses'),nl,
	program_termination_conditions(L,TermConds),
	retractall(predef:predef_include(_At,_Mn,_Mb,_Tc)).

file_expand_clauses(end_of_file,_S,L,L,_F,N,N) :- !.
file_expand_clauses((:-  op(Int,Opsp,Ops)),Stream,L1,L2,Fs,N,M) :- !,
        op(Int,Opsp,Ops),
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  discontiguous(_PIs)),Stream,L1,L2,Fs,N,M) :- !,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  dynamic(_PIs)),Stream,L1,L2,Fs,N,M) :- !,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  multifile(_PIs)),Stream,L1,L2,Fs,N,M) :- !,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  initialization(G)),Stream,L1,L2,Fs,N,M) :- !,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	write('% Added clause: '),write('$initialization :- '),write(G),nl,
	file_expand_clauses(Clause_exp,Stream,[('$initialization' :- G)|L1],L2,Fs,N,M).
file_expand_clauses((:-  include(File)),Stream,L1,L2,Fs,N,M) :- !,
        current_cti_flag(process_include_ensure_loaded,Flag), % on/off
	file_expand_clauses_aux(Flag,File,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  ensure_loaded(File)),Stream,L1,L2,Fs,N,M) :- !,
        current_cti_flag(process_include_ensure_loaded,Flag), % on/off
	file_expand_clauses_aux(Flag,File,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:- include_predef(File)),Stream,L1,L2,Fs,N,M) :- !,
        include_predef(File),
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses((:-  D),Stream,L1,L2,Fs,N,M) :- !,
        write('% Unknown directive: '),write(D),nl,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses(Clause,Stream,L1,L2,Fs,N,M) :- 
	read(Stream,Clause2),expand_term(Clause2,Clause2_exp),
	N1 is N+1,
	file_expand_clauses(Clause2_exp,Stream,[Clause|L1],L2,Fs,N1,M).

file_expand_clauses_aux(off,File,Stream,L1,L2,Fs,N,M) :-
	write('% '),write(File),write(' will not be included'),nl,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses_aux(on,File,Stream,L1,L2,Fs,N,M) :-
	member(File,Fs),!,
	write('% '),write(File),write(' already included'),nl,
	read(Stream,Clause),expand_term(Clause,Clause_exp),
	file_expand_clauses(Clause_exp,Stream,L1,L2,Fs,N,M).
file_expand_clauses_aux(on,File,Stream,L1,L,Fs,N,M) :-	
	    open(File,read,Stream2),
	    read(Stream2,Clause),
	    expand_term(Clause,Clause_exp),
	    file_expand_clauses(Clause_exp,Stream2,L1,L2,[File|Fs],N,N1),
	    close(Stream2),
	write('% '),write(File),write(' included'),nl,
	read(Stream,Clause2),expand_term(Clause2,Clause_exp2),
	file_expand_clauses(Clause_exp2,Stream,L2,L,[File|Fs],N1,M).

%% Including predefined predicates:
include_predef(File) :-
	open(File,read,Stream),
	read(Stream,Clause),
	include_predef_aux(Clause,Stream),
	write('% Built-ins from '),write(File),write(' added'),nl,
	close(Stream).

include_predef_aux(end_of_file,_) :-!.
include_predef_aux(predef(At,Mn,Mb,Tc),Stream) :-
	predef_iso(At),!,functor(At,F,N),
	assertz(predef:predef_include(At,Mn,Mb,Tc)),
	write('% WARNING: '),write(F/N),write(' was an ISO built-in but is now redefined'),nl,	
	read(Stream,Clause),
	include_predef_aux(Clause,Stream).
include_predef_aux(predef(At,Mn,Mb,Tc),Stream) :-
	predef_include(At),!,functor(At,F,N),
	assertz(predef:predef_include(At,Mn,Mb,Tc)),
	write('% WARNING: '),write(F/N),write(' was a built-in but is now redefined'),nl,	
	read(Stream,Clause),
	include_predef_aux(Clause,Stream).
include_predef_aux(predef(At,Mn,Mb,Tc),Stream) :-
	!,assertz(predef:predef_include(At,Mn,Mb,Tc)),
	%functor(At,P,N),
	%write('% '),write(P/N),write(' is now built-in'),nl,	
	read(Stream,Clause),
	include_predef_aux(Clause,Stream).
include_predef_aux(X,Stream) :-
        write('% WARNING: '),write(X),write(' is not of the form predef(_,_,_,_) and is skipped'),nl,
	read(Stream,Clause),
	include_predef_aux(Clause,Stream).
	
