player(toto).
tab(_).

%%%
play(A) :-
        initialize(A, B, C),
        display_game(B),
        play(B, C, _).
play(A, B, C) :-
        game_over(A, B, C), !,
        write(C),
        %abolish0(player).
	true.
play(A, B, C) :-
        choose(A, B, D),
        move(D, B, A, E),
        display_game(E),
        next_player(B, F), !,
        play(E, F, C).
choose(A, computer, B) :-
        write('Computer is thinking...'),
        nl,
        alpha_beta(computer, A, -1, 1, B, _).
choose(A, B, C) :-
        write(B),
        write(', enter move (followed by a period and return) --> '),
        read(D),
        legal(A, B, D, C).
alpha_beta(A, B, _, _, _, C) :-
        A=computer,
        game_win_o(B),
        C is 1.
alpha_beta(A, B, _, _, _, C) :-
        player(A),
        game_win_x(B),
        C is 1.
alpha_beta(_, A, _, _, _, B) :-
        game_tie(A),
        B is 0.
alpha_beta(A, B, _, _, _, C) :-
        A=computer,
        game_win_x(B),
        C is-1.
alpha_beta(A, B, _, _, _, C) :-
        player(A),
        game_win_o(B),
        C is-1.
alpha_beta(A, B, C, D, E, F) :-
        setof(G, move(B,G), H),
        I is-(D),
        J is-(C),
        evaluate(A, H, B, I, J, nil, (E,F)).
evaluate(A, [B|C], D, E, F, G, H) :-
        move(B, A, D, I),
        next_player(A, J),
        alpha_beta(J, I, E, F, _, K),
        L is -(K),
        cutoff(A, B, L, E, F, C, D, G, H), !.
evaluate(_, [], _, A, _, B, (B,A)).
cutoff(_, A, B, _, C, _, _, _, (A,B)) :-
        B>=C, !.
cutoff(A, B, C, D, E, F, G, _, H) :-
        D<C,
        C<E, !,
        evaluate(A, F, G, C, E, B, H).
cutoff(A, _, B, C, D, E, F, G, H) :-
        B=<C, !,
        evaluate(A, E, F, C, D, G, H).
initialize(tictactoe, ((' ',' ',' '),(' ',' ',' '),' ',' ',' '), A) :-
        write('Enter player`s name (followed by a period and return) --> '),
        %read(A),
	A=toto,
        write(A),
	nl,
	write(A),
        write(', you will go first and use the symbol "x".'),
        nl,
        %assert(player(A)).
	true.
display_game(((A,B,C),(D,E,F),G,H,I)) :-
        nl,
        write(' '),
        write(A),
        write(' '),
        write('|'),
        write(' '),
        write(B),
        write(' '),
        write('|'),
        write(' '),
        write(C),
        write(' '),
        tab(3),
        write('u-l = upper left,  u-c = upper center,  u-r = upper right'),
        nl,
        write(' --+---+-- '),
        nl,
        write(' '),
        write(D),
        write(' '),
        write('|'),
        write(' '),
        write(E),
        write(' '),
        write('|'),
        write(' '),
        write(F),
        write(' '),
        tab(3),
        write('c-l = center left, c-c = center center, c-r = center right'),
        nl,
        write(' --+---+-- '),
        nl,
        write(' '),
        write(G),
        write(' '),
        write('|'),
        write(' '),
        write(H),
        write(' '),
        write('|'),
        write(' '),
        write(I),
        write(' '),
        tab(3),
        write('l-l = lower left,  l-c = lower center,  l-r = lower right'),
        nl,
        nl.
next_player(A, computer) :-
        player(A).
next_player(computer, A) :-
        player(A).
legal(A, _, B, B) :-
        move(A, B).
legal(A, B, _, C) :-
        write('Illegal move!  '), !,
        choose(A, B, C).
game_over(A, _, (B,' you win!')) :-
        game_win_x(A),
        player(B).
game_over(A, _, 'Computer wins!') :-
        game_win_o(A).
game_over(A, _, 'It is a tie!') :-
        game_tie(A).
game_tie(((A,B,C),(D,E,F),G,H,I)) :-
        \+game_win_x(((A,B,C),(D,E,F),G,H,I)),
        \+game_win_o(((A,B,C),(D,E,F),G,H,I)),
        A\==' ',
        B\==' ',
        C\==' ',
        D\==' ',
        E\==' ',
        F\==' ',
        G\==' ',
        H\==' ',
        I\==' '.
game_win_x(((x,x,x),(_,_,_),_,_,_)).
game_win_x(((_,_,_),(x,x,x),_,_,_)).
game_win_x(((_,_,_),(_,_,_),x,x,x)).
game_win_x(((x,_,_),(x,_,_),x,_,_)).
game_win_x(((_,x,_),(_,x,_),_,x,_)).
game_win_x(((_,_,x),(_,_,x),_,_,x)).
game_win_x(((x,_,_),(_,x,_),_,_,x)).
game_win_x(((_,_,x),(_,x,_),x,_,_)).
game_win_o(((o,o,o),(_,_,_),_,_,_)).
game_win_o(((_,_,_),(o,o,o),_,_,_)).
game_win_o(((_,_,_),(_,_,_),o,o,o)).
game_win_o(((o,_,_),(o,_,_),o,_,_)).
game_win_o(((_,o,_),(_,o,_),_,o,_)).
game_win_o(((_,_,o),(_,_,o),_,_,o)).
game_win_o(((o,_,_),(_,o,_),_,_,o)).
game_win_o(((_,_,o),(_,o,_),o,_,_)).
move(((' ',_,_),(_,_,_),_,_,_), u-l).
move(((_,' ',_),(_,_,_),_,_,_), u-c).
move(((_,_,' '),(_,_,_),_,_,_), u-r).
move(((_,_,_),(' ',_,_),_,_,_), c-l).
move(((_,_,_),(_,' ',_),_,_,_), c-c).
move(((_,_,_),(_,_,' '),_,_,_), c-r).
move(((_,_,_),(_,_,_),' ',_,_), l-l).
move(((_,_,_),(_,_,_),_,' ',_), l-c).
move(((_,_,_),(_,_,_),_,_,' '), l-r).
move(u-l, computer, A, B) :-
        ((_,C,D),(E,F,G),H,I,J)=A,
        B=((o,C,D),(E,F,G),H,I,J).
move(u-c, computer, A, B) :-
        ((C,_,D),(E,F,G),H,I,J)=A,
        B=((C,o,D),(E,F,G),H,I,J).
move(u-r, computer, A, B) :-
        ((C,D,_),(E,F,G),H,I,J)=A,
        B=((C,D,o),(E,F,G),H,I,J).
move(c-l, computer, A, B) :-
        ((C,D,E),(_,F,G),H,I,J)=A,
        B=((C,D,E),(o,F,G),H,I,J).
move(c-c, computer, A, B) :-
        ((C,D,E),(F,_,G),H,I,J)=A,
        B=((C,D,E),(F,o,G),H,I,J).
move(c-r, computer, A, B) :-
        ((C,D,E),(F,G,_),H,I,J)=A,
        B=((C,D,E),(F,G,o),H,I,J).
move(l-l, computer, A, B) :-
        ((C,D,E),(F,G,H),_,I,J)=A,
        B=((C,D,E),(F,G,H),o,I,J).
move(l-c, computer, A, B) :-
        ((C,D,E),(F,G,H),I,_,J)=A,
        B=((C,D,E),(F,G,H),I,o,J).
move(l-r, computer, A, B) :-
        ((C,D,E),(F,G,H),I,J,_)=A,
        B=((C,D,E),(F,G,H),I,J,o).
move(u-l, _, A, B) :-
        ((_,C,D),(E,F,G),H,I,J)=A,
        B=((x,C,D),(E,F,G),H,I,J).
move(u-c, _, A, B) :-
        ((C,_,D),(E,F,G),H,I,J)=A,
        B=((C,x,D),(E,F,G),H,I,J).
move(u-r, _, A, B) :-
        ((C,D,_),(E,F,G),H,I,J)=A,
        B=((C,D,x),(E,F,G),H,I,J).
move(c-l, _, A, B) :-
        ((C,D,E),(_,F,G),H,I,J)=A,
        B=((C,D,E),(x,F,G),H,I,J).
move(c-c, _, A, B) :-
        ((C,D,E),(F,_,G),H,I,J)=A,
        B=((C,D,E),(F,x,G),H,I,J).
move(c-r, _, A, B) :-
        ((C,D,E),(F,G,_),H,I,J)=A,
        B=((C,D,E),(F,G,x),H,I,J).
move(l-l, _, A, B) :-
        ((C,D,E),(F,G,H),_,I,J)=A,
        B=((C,D,E),(F,G,H),x,I,J).
move(l-c, _, A, B) :-
        ((C,D,E),(F,G,H),I,_,J)=A,
        B=((C,D,E),(F,G,H),I,x,J).
move(l-r, _, A, B) :-
        ((C,D,E),(F,G,H),I,J,_)=A,
        B=((C,D,E),(F,G,H),I,J,x).

