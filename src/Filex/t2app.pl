t2(As, Bs, Cs) :-
        Cs = [_,_,_|As],
        cti:{n(As)>= n(Cs)+1},
        app(As,Bs,Cs).

app([], As, As).
app([E|Es], Fs, [E|Gs]) :-
        app(Es, Fs, Gs).