
%%%%%%%%%%%%%%%% ORDSETS

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).

ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).


%   ord_del_element(+Set1, +Element, ?Set2)
%   is true when Set2 is Set1 but with Element removed.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Head, Tail, Element, Set).

ord_del_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, Tail, _, Tail).
ord_del_element(>, Head, Tail, _, [Head|Tail]).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  

ord_disjoint(Set1, Set2) :-
	\+ ord_intersect(Set1, Set2).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).
ord_intersect(=, _, _, _, _).
ord_intersect(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).



%   ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersecton of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], _, []).
ord_intersection([Head1|Tail1], Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

ord_intersection3(<, _, Set1, Head2, Tail2, Intersection) :-
	ord_intersection3(Set1, Head2, Tail2, Intersection).
ord_intersection3(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection3(>, Head1, Tail1, _, Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

% could be a disjunction, but is used in three places
ord_intersection3([], _, _, []).
ord_intersection3([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection3(Order, Head1, Tail1, Head2, Tail2, Intersection).



ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).

ord_subset(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).


%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

ord_subtract(Set1, Set2, Union) :-
	subtract(Set1, Set2, Union).

ord_union(Set1, Set2, Union) :-
	merge(Set1, Set2, Union).


ord_list_to_assoc(List, Assoc) :-
	length(List, N),
	ord_list_to_assoc(N, List, [], Assoc).

ord_list_to_assoc(0, List, List, t) :- !.
ord_list_to_assoc(1, [Key-Val|List], List, t(Key,Val,0,t,t)) :- !.
ord_list_to_assoc(N, List0, List, t(Key,Val,Bal,L,R)) :-
	Bal is msb(N)-msb(N-1),
	A is (N-1) >> 1,
	Z is (N-1) - A,
	ord_list_to_assoc(A, List0, [Key-Val|List1], L),
	ord_list_to_assoc(Z, List1, List, R).



%   ord_union(+Set1, +Set2, ?Union, ?New)
%   is true when Union is the union of Set1 and Set2, and New is
%   Set2 \ Set1.  This is useful if you
%   are accumulating members of a set and you want to process new
%   elements as they are added to the set.

ord_union([], Set2, Set2, Set2).
ord_union([Head1|Tail1], Set2, Union, Difference) :-
	ord_union4(Set2, Head1, Tail1, Union, Difference).

ord_union4(<, Head, Set1, Head2, Tail2, [Head|Union], Difference) :-
	(   Set1 = [], Union = [Head2|Tail2], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference)
	).
ord_union4(=, Head, Tail1, _, Tail2, [Head|Union], Difference) :-
	ord_union(Tail1, Tail2, Union, Difference).
ord_union4(>, Head1, Tail1, Head2, Set2, [Head2|Union], [Head2|Difference]) :-
	ord_union4(Set2, Head1, Tail1, Union, Difference).

ord_union4([], Head1, Tail1, [Head1|Tail1], []).
ord_union4([Head2|Tail2], Head1, Tail1, Union, Difference) :-
	compare(Order, Head1, Head2),
	ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference).


%%%
merge([], Set, Set).
merge([O|Os], Ns, Set) :- merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).


%   subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

subtract([], _, []).
subtract([Head1|Tail1], Set2, Difference) :-
	subtract3(Set2, Head1, Tail1, Difference).

subtract3(<, Head, Set1, Head2, Tail2, [Head|Difference]) :-
	(   Set1 = [], Difference = []
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    subtract3(Order, Head1, Tail1, Head2, Tail2, Difference)
	).
subtract3(=, _, Tail1, _, Tail2, Difference) :-
	subtract(Tail1, Tail2, Difference).
subtract3(>, Head1, Tail1, _, Set2, Difference) :-
	subtract3(Set2, Head1, Tail1, Difference).

subtract3([], Head1, Tail1, [Head1|Tail1]).
subtract3([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	subtract3(Order, Head1, Tail1, Head2, Tail2, Difference).


/*
|: 'miniordsets.pl'.
Enter number of runs followed by a full stop
|: 1.
% Lecture du fichier et assertion dans proimp ... en 0 ms
% Traduction de Prolog (primp) a Prolog pur (propur) ... en 0 ms
% Traduction de Prolog pur (propur) a Prolog normalise (pronor) ... en 10 ms
% Traduction de Prolog normalise (pronor) a nat (pronat) ... en 180 ms


% Taille Prog  = 126 lignes
% Lecture du fichier et assertion dans pronat ... en 10 ms
% Predicats inconnus, supposes non-terminants : aucun
% Construction, reduction et tri du graphe d'appel ... en 0 ms
% Nb Pred      = 25
% Nb Sccs      = 14
% Nb Pred/Sccs = 1.7857142857142858
% Estimation temps max inference : 2240000 ms
% Calcul d'un modele PLC(N) ... en 1940 ms
modele_nat-1940,
% Specialisation prog PLC(N) (pronat/pronatspec) ... en 0 ms
% Calcul des contraintes de l'ens des level mappings ... en (sans TNA) 7580 ms
ens_levmap-7580,
% Calcul des level mappings max (clpq) ... en 1920 ms
levmap_max-1920,
% Traduction de prog nat (pronat) vers bool (probool) ... en 100 ms
plcN__plcB-100,
% Traduction mes max (pronatres) vers bool (proboolres) ... en 30 ms
% Calcul du modele PLC(B) ... en 1440 ms
model_bool-1440,
% Calcul des conditions de terminaison gauche ... en 280 ms
cond_termg-280,
%  --------
%  -- STATS
%  ------------------------------
%  TEMPS (ms)
% total : 13510
%  max___time : 2240000
%  modele_nat : 1940
%  ens_levmap : 7580
%  levmap_max : 1920
%  plcN__plcB : 100
%  model_bool : 1440
%  cond_termg : 280
tps__total-13510
%  -------------------------------
%  MEMOIRE (octets)
%  nombre de gc  : 4
%  espace libere par gc          : 4725560
%  -------------------------------
%  QUALITE
%  modes :
%    total  : 616
%    couvrt : 289
%    global : 46%
%    moyen  : 45%
%  taux couverture : 100%
%  -------------------------------
[predicate_term_condition(merge(_75644,_75642,_75640),_75640+_75644*_75642),predicate_term_condition(merge(_75726,_75724,_75722,_75720),_75720+_75726*_75722),predicate_term_condition(merge(_75814,_75812,_75810,_75808,_75806,_75804),_75804+_75810*_75806),predicate_term_condition(ord_add_element(_75919,_75917,_75915),_75915+_75919),predicate_term_condition(ord_add_element(_76021,_76019,_76017,_76015,_76013),_76013+_76017),predicate_term_condition(ord_del_element(_76228,_76226,_76224),_76224+_76228),predicate_term_condition(ord_del_element(_76358,_76356,_76354,_76352,_76350),_76350+_76354),predicate_term_condition(ord_disjoint(_76507,_76505),_76507*_76505),predicate_term_condition(ord_intersect(_76608,_76606),_76608*_76606),predicate_term_condition(ord_intersect(_76719,_76717,_76715,_76713,_76711),_76715*_76711),predicate_term_condition(ord_intersection(_76820,_76818,_76816),_76820*_76818),predicate_term_condition(ord_intersection3(_76966,_76964,_76962,_76960),_76966*_76962),predicate_term_condition(ord_intersection3(_77126,_77124,_77122,_77120,_77118,_77116),_77122*_77118),predicate_term_condition(ord_list_to_assoc(_77299,_77297),_77299*_77297),predicate_term_condition(ord_list_to_assoc(_77420,_77418,_77416,_77407),_77407),predicate_term_condition(ord_subset(_77602,_77593),_77593),predicate_term_condition(ord_subset(_77700,_77698,_77696,_77687),_77687),predicate_term_condition(ord_subtract(_77821,_77819,_77817),_77819*_77817+_77821*_77819),predicate_term_condition(ord_union(_77960,_77958,_77956),_77956+_77960*_77958),predicate_term_condition(ord_union(_78119,_78117,_78115,_78113),_78115+_78119*_78113+_78119*_78117),predicate_term_condition(ord_union4(_78334,_78332,_78330,_78328,_78326),_78328+_78330*_78326+_78334*_78330),predicate_term_condition(ord_union4(_78669,_78667,_78665,_78663,_78661,_78659,_78657),_78659+_78665*_78657+_78665*_78661),predicate_term_condition(subtract(_79068,_79066,_79064),_79066*_79064+_79068*_79066),predicate_term_condition(subtract3(_79198,_79196,_79194,_79192),_79198*_79192+_79198*_79194),predicate_term_condition(subtract3(_79350,_79348,_79346,_79344,_79342,_79340),_79342*_79340+_79346*_79342)]
{consulting /home/fred/BenchTerm/LibSP/miniordsets.cti...}
{consulted /home/fred/BenchTerm/LibSP/miniordsets.cti in module user, 20 msec 1800 bytes}
------
miniordsets.pl:0[modele_nat-1910,ens_levmap-7560,levmap_max-1900,plcN__plcB-100,model_bool-1430,cond_termg-280,tps__total-13430]
------
miniordsets.pl:m[modele_nat-1910,ens_levmap-7560,levmap_max-1900,plcN__plcB-100,model_bool-1430,cond_termg-280,tps__total-13430]
miniordsets.pl:%[modele_nat-14,ens_levmap-56,levmap_max-14,plcN__plcB-0,model_bool-10,cond_termg-2,tps__total-100]
*/