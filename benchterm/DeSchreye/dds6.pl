/*
:- type bool_exp --> and(bool_exp,bool_exp);
					 or(bool_exp,bool_exp);
					 0;
					 1.
*/
					 
% :- pred dis(bool_exp).
%:- pred dis(top).
dis(or(B1,B2)) :- con(B1),dis(B2).
dis(B) :- con(B).

% :- pred con(bool_exp).
%:- pred con(top).
con(and(B1,B2)) :- dis(B1),con(B2).
con(B) :- bool(B).


% :- pred bool(bool_exp).
%:- pred bool(top).
bool(0).
bool(1).


/*
| ?- go.
Fichier : dds6.

Type-checking ... tap: checking dds6
read_module(dds6)

check/infer: dis(or(A,B)):-con(A),dis(B)
Consistent with variable types []

check/infer: dis(A):-con(A)
Consistent with variable types []

check/infer: con(and(A,B)):-dis(A),con(B)
Consistent with variable types []

check/infer: con(A):-bool(A)
Consistent with variable types []

check/infer: bool(0)
Consistent with variable types []

check/infer: bool(1)
Consistent with variable types []
end
Stop
en 2067 ms

Lecture du fichier et assertion dans nat_pgm ... en 417 ms
    bool/1-[$ref(3558705,1),$ref(2957853,0)]
    con/1-[$ref(3558585,1),$ref(3553245,0)]
    dis/1-[$ref(3558621,1),$ref(3222217,0)]


bool/1 :
    bool(A):- 
        [A=0]
        []
    bool(A):- 
        [A=0]
        []

con/1 :
    con(A):- 
        [A>=0]
        [bool(A)]
    con(A):- 
        [B>=0,C=-1-B+A,B-A=<-1]
        [dis(C),con(B)]

dis/1 :
    dis(A):- 
        [A>=0]
        [con(A)]
    dis(A):- 
        [B>=0,C=-1-B+A,B-A=<-1]
        [con(C),dis(B)]

Construction, reduction et tri du graphe d'appel ... en 17 ms
Graphe d'appel reduit et trie par ordre croissant (croit en descendant) :
    [bool/1]
    [con/1,dis/1]

Calcul des relations inter-arguments
{The debugger will first leap -- showing spypoints (debug)}
Pour [bool/1], precision = 2
    model(bool,1,[A],[A=0])
Pour [con/1,dis/1], precision = 3
    model(con,1,[A],[A>=0])
    model(dis,1,[A],[A>=0])
{The debugger is switched off}
en 4233 ms

Calcul des contraintes definissant l'ensemble des mesures ... 
    mes([bool/1-[A,B]],[])
    mes([con/1-[A,B],dis/1-[C,D]],[B=D,A-C=<-1,A-C+D>=1,A>=0])
en 35186 ms

Calcul des mesures max ... 
    mes_max([bool/1],[[bool/1-[0,0]]])
    mes_max([con/1,dis/1],[[con/1-[0,2],dis/1-[1,2]]])
en 284 ms

Traduction nat vers bool ... 
en 0 ms

    bool/1-[$ref(3558705,1),$ref(2957853,0)]
    con/1-[$ref(3558585,1),$ref(3553245,0)]
    dis/1-[$ref(3558621,1),$ref(3222217,0)]

bool/1 :
    bool(A):- 
        (A=:=1)*1
        []
    bool(A):- 
        (A=:=1)*1
        []

con/1 :
    con(A):- 
        (A=<1)*1
        [bool(A)]
    con(A):- 
        (B=<1)*((C+(B+0)=:=0+A)*((B+0>=A)*1))
        [dis(C),con(B)]

dis/1 :
    dis(A):- 
        (A=<1)*1
        [con(A)]
    dis(A):- 
        (B=<1)*((C+(B+0)=:=0+A)*((B+0>=A)*1))
        [con(C),dis(B)]

Calcul du modele booleen ... 
{The debugger will first leap -- showing spypoints (debug)}
Pour [bool/1] dans bool :
    model(bool,1,[A],A*1)
    [[1]]
Pour [con/1,dis/1] dans bool :
    model(con,1,[A],A*1)
    [[1]]
    model(dis,1,[A],A*1)
    [[1]]
{The debugger is switched off}
en 850 ms


yes
| ?- 
*/
