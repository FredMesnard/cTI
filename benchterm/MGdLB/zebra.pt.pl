% zebra part

houses(Prop):- domain(Prop,[first,second,third,fourth,fifth]).

domain([],_).
domain([X|Rest],Domain):- 
	select(X,Domain,NewDomain), 
	domain(Rest,NewDomain).

select(X,[X|R],R).
select(X,[Y|R],[Y|Rest]):-
	select(X,R,Rest).





