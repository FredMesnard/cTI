
map_colors([], _, []).
map_colors(Ns, Coloring, Ns1) :-
	Ns = [X|_],
	Coloring = [V-_|_],
	compare(C, X, V),
	map_colors(C, Ns, Coloring, Ns1).

map_colors(=, [_|Xs], [_-Y|Coloring], [Y|Ns1]) :-
	map_colors(Xs, Coloring, Ns1).
map_colors(>, Ns, [_|Coloring], Ns1) :-
	map_colors(Ns, Coloring, Ns1).

