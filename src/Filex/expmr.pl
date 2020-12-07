/*
p(0,0).
p(s(X),s(Y)):-q(X,Y).

q(X,Y):-p(X,Y).


Calcul des contraintes de l'ens des level mappings ... 
    mes([p/2-[A,B,C],q/2-[D,E,F]],[A-D=< -1,A>=0,E=B,B>=0,C=F,F>=0,A+B-D+F>=1])
en 1720 ms
Calcul des level mappings max (clpq) ... 
    mes_max([p/2,q/2],[[p/2-[0,0,2],q/2-[1,2,0]],[p/2-[0,0,2],q/2-[1,0,2]],[p/2-[0,2,0],q/2-[1,2,0]],[p/2-[0,2,0],q/2-[1,0,2]]])
en 170 ms
*/

/*
p(0,0).
p(s(X),s(Y)):-q(X,Y).

q(X,Y):-p(Y,X).

Calcul des contraintes de l'ens des level mappings ... 
    mes([p/2-[A,B,C],q/2-[D,E,F]],[A-D=< -1,A>=0,B=F,E=F,C=F,A-D+2*F>=1])
en 1690 ms
Calcul des level mappings max (clpq) ... 
    mes_max([p/2,q/2],[[p/2-[0,1,1],q/2-[1,1,1]]])
en 90 ms
*/

p(0,0).
p(s(X),Y):-q(X,Y).
q(X,s(Y)):-p(X,Y).

/*
Calcul des contraintes de l'ens des level mappings ... 
    mes([p/2-[A,B,C],q/2-[D,E,F]],[A-C-D=< -1,A>=0,D>=0,E=B,B>=0,A+B-D>=1,F=C,C>=0])
en 1730 ms
Calcul des level mappings max (clpq) ... 
    mes_max([p/2,q/2],[[p/2-[1,0,2],q/2-[1,2,0]],[p/2-[1,0,2],q/2-[0,0,2]],[p/2-[0,2,0],q/2-[1,2,0]],[p/2-[0,2,0],q/2-[0,0,2]]])
en 250 ms
*/


