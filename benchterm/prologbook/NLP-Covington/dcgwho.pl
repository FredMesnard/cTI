% File DCGWHO.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 3, Section 3.4.5

% Parser that handles 'who' questions

s(In,Out) --> [who,did], np([who|In],Out1), vp(Out1,Out).

s(In,Out) --> np(In,Out1), vp(Out1,Out).


np([who|Out],Out) --> [].

np(X,X) --> [max];[joe];[bill];[fido].
           % Proper names are complete NPs

vp(X,X) --> v.
vp(In,Out) --> v, np(In,Out).
vp(In,Out) --> v, s(In,Out).

v --> [saw];[said];[thought];[believed];[barked].
v --> [see];[say]; [think];  [believe]; [bark].


% To test:
%  ?- s([],[],[who,did,max,say,barked],[]).  % note extra args!
%  yes
