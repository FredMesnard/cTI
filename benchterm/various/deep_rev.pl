%From orenf@cs.huji.ac.il Mon May 15 19:54:00 1995


% deep(X,Y) : Y is the reverse of  X


deep(X,X) :- 
	atomic(X), !.
deep(X,Y) :- 
        deep0(X,[],Y).


% deep0(X,A,Y) : auxilary predicate for deep.
%               it recursively reverses X into Y
%                keeping the temporary result in 
%                 an accomulator A

deep0([],A,A).

deep0([X|Xs],A,Y) :- 
	deep(X,Xr),
	deep0(Xs,[Xr|A],Y).








