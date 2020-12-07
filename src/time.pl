:- module(cti_time,[stat_init/1,add_stat/3,stat_final/1]).

:- use_module(library(lists),[member/2]).

stat_init(E) :-
        E=[global_stack-G,local_stack-L,trail-T,
           %choice-C,
           runtime-R,garbage_collection-GC],
        garbage_collect,
        statistics(global_stack,G),
        statistics(local_stack,L),
        statistics(trail,T),
        %statistics(choice,C),
        statistics(runtime,[R,_]),
        garbage_collect,
        statistics(garbage_collection,GC).
 
add_stat(E1,Id,E2) :-
	statistics(runtime,[_,Temps]),
	E2=[Id-Temps|E1].
 
stat_final(EtatFinal) :-
        statistics(runtime,[Rf,_]),
        member1(runtime-R,EtatFinal),
	    write('%  ------------------------------'),nl,
        write('%  Timings (ms)'),nl,
        write('%  total: '),Temps is Rf-R,write(Temps),nl,
        stat_int(EtatFinal).
 
stat_int([global_stack-_|_]) :-!.
stat_int([Info-Temps|Etat]) :-
        stat_int(Etat),write('%  '),
        write(Info), % Info sur 10 caracteres
        write(': '),write(Temps),nl.

member1(X,L) :- once(member(X,L)).
