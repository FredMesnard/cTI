least([[C,P]],[[C,P]]).
least([[C,P]|Pairs],[[C1,P1],[C,P]|L1]) :- 
                                least(Pairs,[[C1,P1]|L1]),P1<P.
least([[C,P]|Pairs],[[C,P],[C1,P1]|L1]) :- 
                                least(Pairs,[[C1,P1]|L1]),P=<P1.

twoleast(P,[[C1,P1],[C2,P2]|L]) :- 
                    least(P,[[C1,P1]|L1]),least(L1,[[C2,P2]|L]).

huffman([[C,P]],C).
huffman(Pairlist,T) :- twoleast(Pairlist,[[C1,P1],[C2,P2]|L]),
			P is P1+P2, huffman([[tree(C1,C2),P]|L],T).

code(tree(X,Y),C,[0|L]) :- code(X,C,L).
code(tree(X,Y),C,[1|L]) :- code(Y,C,L).
code(X,X,[]) :- atom(X).

goal :-  huffman([[a,0.3],[b,0.2],[c,0.1],[d,0.05],[e,0.05],
         [f,0.25],[g,0.03],[h,0.02]],Tree), code(Tree,Char,Code).

