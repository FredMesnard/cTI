% Computational Intelligence: a logical approach. 
% Prolog Code. A grammar for outputting canned English from Figure 3.7.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

trans(scheduled(S,C,T,R),[is|T1],T9) :-
   trans(session(S),T1,[of|T3]),
   trans(course(C),T3,[scheduled,at|T5]),
   trans(time(T),T5,[in|T7]),
   trans(room(R),T7,[?|T9]).

trans(session(w92),[the,winter,1992,session|T],T).

trans(course(cs422),[the,intro,artificial,intelligence,course|T],T).

trans(time(pm(H,M)),[H,:,M,pm|T],T).
trans(time(am(H,M)),[H,:,M,am|T],T).
trans(time(clock(H,M)),[H,:,M,am|T],T) :- H < 12.
trans(time(clock(12,M)),[12,:,M,pm|T],T).
trans(time(clock(H,M)),[H1,:,M,pm|T],T) :- H > 12, H1 is H-12.
trans(time(noon),[noon|T],T).

trans(room(above(R)),[the,room,above|T1],T2) :-
   trans(room(R),T1,T2).
trans(room(csci333),[the,computer,science,department,office|T],T).

/*
| ?- trans(scheduled(w92,cs422,pm(1,30),above(csci333)),T,[]).

T = [is,the,winter,1992,session,of,the,intro,artificial,intelligence,course,scheduled,at,1,:,30,pm,in,the,room,above,the,computer,science,department,office,?] ? 


| ?- trans(scheduled(w92,cs422,clock(15,30),above(csci333)),T,[]).

T = [is,the,winter,1992,session,of,the,intro,artificial,intelligence,course,scheduled,at,3,:,30,pm,in,the,room,above,the,computer,science,department,office,?] ? 
*/
