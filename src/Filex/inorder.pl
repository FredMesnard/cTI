ino(T,I):-ino(T,I,[],[],[],[]).

ino(nil,X,X,[],[],[]).
ino(nil,X,X,[C|Cs],[H|Hs],[T|Ts]):- ino(C,H,T,Cs,Hs,Ts).
ino(tree(L,V,R),H,T,Cs,Hs,Ts):- ino(L,H,[V|T1],[R|Cs],[T1|Hs],[T|Ts]).

/*
{% clause rec 1
    D+E+F>=1,D>=A,E>=B,F>=C,B+C>=0,	%mu(H-B)>=1
    A>=0,B>=0,C>=0,                     %mu(B)>=0
 % clause rec 2
    A>=1+D+C+E+F,C+E=0,A>=C,A>=D,C>=F, %mu(H-B)>=1
    A,B,C,D,F>=0,C+E>=0                %mu(B)>=0
    %ie B>=0,C=E=F=0,A>=D+1,D>=0
}    
pas de sol !
*/

ino2(size(nil,[]),nil,X,X,[],[],[]).
ino2(size(nil,[C|Cs]),nil,X,X,[C|Cs],[H|Hs],[T|Ts]):- ino2(size(C,Cs),C,H,T,Cs,Hs,Ts).
ino2(size(tree(L,V,R),Cs),tree(L,V,R),H,T,Cs,Hs,Ts):- ino2(size(L,[R|Cs]),L,H,[V|T1],[R|Cs],[T1|Hs],[T|Ts]).

/*
{A=0,C-F=<0,B+C>=0,B>=0,D>=0,D-G=<0,B+F+G+H>=1,E>=0,E-H=<0}  ( => B+F >=0 )  pour la 1ere clause rec ino2
{A=0,E=0,G=0,H=0,D>=0,C-F>=1,B+F>=0,B+1/2*F>=0}                              pour la 2eme clause rec ino2

a la main, pour la 2eme clause de ino/2 :
project([B,C,D,E,F,G,H],[2*B+E+F+G+H>=0,B+C>=0,B+F>=0,D>=0,E>=0,E+G>=0,G>=0,H>=0,C>=1+E+F+G+H,C>=F,B+C>=E,E+G=0,H=0],[B,C,D,E,F,G,H],Cs),
numbervars([B,C,D,E,F,G,H],1,_),print(Cs),nl.
       [E=0,G=0,H=0,D>=0,C-F>=1,B+F>=0,B+1/2*F>=0]

la conjonction de 2 clauses est insatisfiable :       
| ?- {A=0,C-F=<0,B+C>=0,B>=0,D>=0,D-G=<0,B+F+G+H>=1,E>=0,E-H=<0,A=0,E=0,G=0,H=0,D>=0,C-F>=1,B+F>=0,B+1/2*F>=0}.

no

*/