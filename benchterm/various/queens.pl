queens(X,Y) :-
   perm(X,Y),
   safe(Y).

perm([],[]).
perm([X|Y],[V|Res]) :-
  delete(V,[X|Y],Rest),
  perm(Rest,Res).

delete(X,[X|Y],Y).
delete(X,[F|T],[F|R]) :-
  delete(X,T,R).

safe([]).
safe([X|Y]) :-
  noattack(X,Y,1),
  safe(Y).

noattack(X,[],N).
noattack(X,[F|T],N) :-
   X =\= F,
   X =\= F + N,
   F =\= X + N,
   N1 is N + 1,
   noattack(X,T,N1).



/*| ?- go.
Fichier : queens.

Type-checking ... tap: checking queens
read_module(queens)

check/infer: queens(A,B):-perm(A,B),safe(B)
Consistent with variable types []

check/infer: perm([],[])
Consistent with variable types []

check/infer: perm([A|B],[C|D]):-delete(C,[A|B],E),perm(E,D)
Consistent with variable types []

check/infer: delete(A,[A|B],B)
Consistent with variable types []

check/infer: delete(A,[B|C],[B|D]):-delete(A,C,D)
Consistent with variable types []

check/infer: safe([])
Consistent with variable types []

check/infer: safe([A|B]):-noattack(A,B,1),safe(B)
Consistent with variable types []

check/infer: noattack(A,[],B)
Consistent with variable types []

check/infer: noattack(A,[B|C],D):-A=\=B,A=\=B+D,B=\=A+D,E is D+1,noattack(A,C,E)
Consistent with variable types []
end
Stop
en 3234 ms

Lecture du fichier et assertion dans nat_pgm ... en 900 ms
    delete/3-[$ref(2981613,1),$ref(2981961,0)]
    noattack/3-[$ref(3124657,1),$ref(2981693,0)]
    perm/2-[$ref(2981525,1),$ref(3150857,0)]
    queens/2-[$ref(2982049,0)]
    safe/1-[$ref(2575277,1),$ref(3129489,0)]


delete/3 :
    delete(A,B,C):- 
        [D=A,A>=0,E>=0,B=C+E-F,F>=0,C-F>=1]
        [delete(D,E,F)]
    delete(A,B,C):- 
        [A=-1+B-C,B-C>=1,C>=0]
        []

noattack/3 :
    noattack(A,B,C):- 
        [D=A,A>=0,E>=0,C>=0,F>=0,B-E>=1]
        [noattack(D,E,F)]
    noattack(A,B,C):- 
        [B=0,A>=0,C>=0]
        []

perm/2 :
    perm(A,B):- 
        [C=D,A=E,F>=0,G>=0,B=1+G+F,D>=0,E>=1]
        [delete(G,E,D),perm(C,F)]
    perm(A,B):- 
        [A=0,B=0]
        []

queens/2 :
    queens(A,B):- 
        [C=A,D=B,E=B,A>=0,B>=0]
        [perm(C,D),safe(E)]

safe/1 :
    safe(A):- 
        [B=0,C=D,D>=0,E>=0,A=1+E+D]
        [noattack(E,D,B),safe(C)]
    safe(A):- 
        [A=0]
        []

Construction, reduction et tri du graphe d'appel ... en 16 ms
Graphe d'appel reduit et trie par ordre croissant (croit en descendant) :
    [delete/3]
    [perm/2]
    [noattack/3]
    [safe/1]
    [queens/2]

Calcul des relations inter-arguments
{The debugger will first leap -- showing spypoints (debug)}
Pour [delete/3], precision = 2
    model(delete,3,[A,B,C],[A=-1+B-C,B-C>=1,C>=0])
Pour [perm/2], precision = 2
    model(perm,2,[A,B],[A=B,B>=0])
Pour [noattack/3], precision = 2
    model(noattack,3,[A,B,C],[C>=0,A>=0,B>=0])
Pour [safe/1], precision = 2
    model(safe,1,[A],[A>=0])
Pour [queens/2], precision = 2
    model(queens,2,[A,B],[B=A,A>=0])
{The debugger is switched off}
en 4000 ms

Calcul des contraintes definissant l'ensemble des mesures ... 
    mes([delete/3-[A,B,C,D]],[C+D>=1,B>=0,C>=0,D>=0,A>=0])
    mes([perm/2-[A,B,C]],[B+C>=1,B>=0,C>=0,A>=0])
    mes([noattack/3-[A,B,C,D]],[D=0,C>=1,B>=0,A>=0])
    mes([safe/1-[A,B]],[B>=1,A>=0])
    mes([queens/2-[A,B,C]],[])
en 9184 ms

Calcul des mesures max ... 
    mes_max([delete/3],[[delete/3-[0,0,1,0]],[delete/3-[0,0,0,1]]])
    mes_max([perm/2],[[perm/2-[0,1,0]],[perm/2-[0,0,1]]])
    mes_max([noattack/3],[[noattack/3-[0,0,1,0]]])
    mes_max([safe/1],[[safe/1-[0,1]]])
    mes_max([queens/2],[[queens/2-[0,0,0]]])
en 917 ms

Traduction pgm nat vers bool ... 
en 17 ms

    delete/3-[$ref(2981613,1),$ref(2981961,0)]
    noattack/3-[$ref(3124657,1),$ref(2981693,0)]
    perm/2-[$ref(2981525,1),$ref(3150857,0)]
    queens/2-[$ref(2982049,0)]
    safe/1-[$ref(2575277,1),$ref(3129489,0)]

delete/3 :
    delete(A,B,C):- 
        (D=:=A)*((A=<1)*((E=<1)*((B+(0+F)=:=C+E+0)*((F=<1)*((C+0=<F)*1)))))
        [delete(D,E,F)]
    delete(A,B,C):- 
        (A+(0+C)=:=B+0)*((B+0=<C)*((C=<1)*1))
        []

noattack/3 :
    noattack(A,B,C):- 
        (D=:=A)*((A=<1)*((E=<1)*((C=<1)*((F=<1)*((B+0=<E)*1)))))
        [noattack(D,E,F)]
    noattack(A,B,C):- 
        (B=:=1)*((A=<1)*((C=<1)*1))
        []

perm/2 :
    perm(A,B):- 
        (C=:=D)*((A=:=E)*((F=<1)*((G=<1)*((B+(0+0)=:=G+F)*((D=<1)*((E=<1)*1))))))
        [delete(G,E,D),perm(C,F)]
    perm(A,B):- 
        (A=:=1)*((B=:=1)*1)
        []

queens/2 :
    queens(A,B):- 
        (C=:=A)*((D=:=B)*((E=:=B)*((A=<1)*((B=<1)*1))))
        [perm(C,D),safe(E)]

safe/1 :
    safe(A):- 
        (B=:=1)*((C=:=D)*((D=<1)*((E=<1)*((A+(0+0)=:=E+D)*1))))
        [noattack(E,D,B),safe(C)]
    safe(A):- 
        (A=:=1)*1
        []

Calcul du modele booleen ... 
{The debugger will first leap -- showing spypoints (debug)}
Pour [delete/3] dans bool :
    model(delete,3,[A,B,C],1)
    [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
Pour [perm/2] dans bool :
    model(perm,2,[A,B],B*1)
    [[0,1],[1,1]]
Pour [noattack/3] dans bool :
    model(noattack,3,[A,B,C],1)
    [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
Pour [safe/1] dans bool :
    model(safe,1,[A],A*1)
    [[1]]
Pour [queens/2] dans bool :
    model(queens,2,[A,B],B*1)
    [[0,1],[1,1]]
{The debugger is switched off}
en 2567 ms

Traduction mesures nat vers bool ... 
    mes([delete/3],[delete/3-[A,B,C]-D^(C=\=D*B#B)])
    mes([perm/2],[perm/2-[A,B]-C^(B=\=C*A#A)])
    mes([noattack/3],[noattack/3-[A,B,C]-B*1])
    mes([safe/1],[safe/1-[A]-A*1])
    mes([queens/2],[queens/2-[A,B]-1])
en 433 ms

Calcul des conditions de terminaison ... 
    bool_tc(delete,3,[A,B,C],B+C)
    bool_tc(perm,2,[A,B],A)
    bool_tc(noattack,3,[A,B,C],B)
{ERROR: module(term_cond,pb(safe/1))}
*/
