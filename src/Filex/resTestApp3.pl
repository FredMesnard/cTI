%%%%%% Mes commentaires sont prefixes par '%%%%%%'


%%%%%% le fichier initiale (erreur de ta part dans la 1ere ligne !) 
% app3(Xs, Ys, Zs, Us) :- Us is the result of concatentating the lists Xs, Ys and Zs
%                 ----
app3(Xs, Ys, Zs, Us) :- app(Xs, Ys, Vs), app(Vs, Zs, Us).

%app3SigTerm(Xs, Ys, Zs, Us):- app(Vs,Zs,Us),app(Xs,Ys,Vs).

% augmented by the APPEND program
app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).


/*
| ?- file_termconds('/home/fred/CTI/lt-0.23/testapp3.pl',L).
-------------------------------------------------------------------
Determination des arguments sans influence pour la terminaison ... en 0 ms
%%%%%%
    app(0,1,0)
    app3(0,0,1,0)

Lecture du fichier et assertion dans proimp ... en 0 ms
Traduction de Prolog (primp) a Prolog pur (propur) ...en 0 ms
Traduction de Prolog pur (propur) a Prolog normalise (pronor) ...en 0 ms
Traduction de Prolog normalise (pronor) a nat (pronat) ...en 20 ms

-------------------------------------------------------------------
Lecture du fichier et assertion dans pronat ... en 0 ms
Predicats inconnus, supposes non-terminants : aucun
Construction, reduction et tri du graphe d'appel ... en 0 ms
Graphe d'appel reduit et trie (croit en descendant) :
    [app/3]
    [app3/4]

Calcul d'un modele PLC(N) ... 
[app/3] ... 
    bd:10ms i1:pseudo_widening i2:pseudo_widening 
    50ms
[app3/4] ... 
    bd:10ms i1:pseudo_widening i2:pseudo_widening 
    50ms
en 120 ms
    app/3-([A,B,C]-[A= -(B)+C,B>=0,B-C=<0]-(precision=0)-(nb_ite=2))
    app3/4-([A,B,C,D]-[A= -(B)-C+D,C>=0,B>=0,B+C-D=<0]-(precision=0)-(nb_ite=2))

Specialisation prog PLC(N) (pronat/pronatspec) ... en 0 ms

Calcul des contraintes de l'ens des level mappings ... 
    mes([app/3-[A,B,0,C]],[A=0,0=0,B>=0,C>=0,B+C>=1])
    mes([app3/4-[A,B,C,0,D]],[A=0,0=0])
en 70 ms
Calcul des level mappings max (clpq) ... 
    mes_max([app/3],[[app/3-[0,1,0,0]],[app/3-[0,0,0,1]]])
    mes_max([app3/4],[[app3/4-[0,0,0,0,0]]])
en 10 ms

Traduction de prog nat (pronat) vers bool (probool) ... en 10 ms

Traduction mes max (pronatres) vers bool (proboolres) ... 
    mes([app/3],[app/3-[A,B,C]-D^(C=\=D*A#A)])
    mes([app3/4],[app3/4-[A,B,C,D]-1])
en 0 ms
%%%%%% ie :
gamma_app(X,Y,Z) = X + Z
gamma_app3(X,Y,Z,U) = 1

Calcul du modele PLC(B) ... 
[app/3] ... 
    bd:10ms i1  i2  
    10ms
[app3/4] ... 
    bd:0ms i1  i2  
    10ms
en 30 ms
    app/3-([A,B,C]-(C=:=B*A)-(precision=1000000)-(nb_ite=2))
    app3/4-([A,B,C,D]-(D=:=C*B*A)-(precision=1000000)-(nb_ite=2))

Calcul des conditions de terminaison gauche ... 
    [app/3] : 0
    [app3/4] : 20
en 20 ms
    predicate_term_condition(app(A,B,C),C+A)
    predicate_term_condition(app3(A,B,C,D),A*D+A*B)

%%%%%% condition de sigma-terminaison:
sigmaTC_app(A,B,C)=A+C
sigmaTC_app3(A,B,C,D)=D+A*B


%%%%%% sur mon PowerBook, SP 3.7.1, temps et espace pour cTI_lt :

  --------
  -- STATS 
  ------------------------------
  TEMPS (ms)
  analyse totale : 270 incluant :
  modele_nat : 120
  ens_levmap : 70
  levmap_max : 10
  plcN__plcB : 10
  model_bool : 30
  cond_termg : 20
  -------------------------------
  MEMOIRE (octets)
  nombre de gc  : 0
                   global stack : 164232
                   local stack  : 1176
                   trail stack  : 5080
                   choice stack : 812

L = [predicate_term_condition(app(_A,_B,_C),_C+_A),predicate_term_condition(app3(_D,_E,_F,_G),_D*_G+_D*_E)] ? ;

no
*/