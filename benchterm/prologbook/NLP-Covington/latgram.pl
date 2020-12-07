% File LATGRAM.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 8, Section 8.2.3

% DCG parser for a small subset of Latin.

xs --> xnp(_,nom), xvp.

xvp --> xv.
xvp --> xnp(_,acc), xv.

xnp(Gender,Case) --> xd(Gender,Case), xn(Gender,Case).
xnp(Gender,Case) --> xn(Gender,Case), xd2(Gender,Case).

xd(_,_)   --> [].
xd(_,nom) --> [omnis].
xd(_,acc) --> [omnem].

xd2(masc,nom) --> [quidam].
xd2(masc,acc) --> [quendam].
xd2(fem,nom)  --> [quaedam].
xd2(fem,acc)  --> [quandam].

xn(masc,nom)  --> [canis].
xn(masc,acc)  --> [canem].
xn(fem,nom)   --> [felis].
xn(fem,acc)   --> [felem].

xv --> [ululavit];[vidit];[agitavit].

