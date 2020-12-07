% File EARLEYS.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.6.8

% Earley parser with subsumption check

:- reconsult('subsumes.pl').  % not needed in Quintus Prolog

% Following line required in Quintus (LPA) DOS Prolog only,
% to prevent error messages if we try to look at the chart
% before asserting anything into it.
% '?ERROR?'(2,_) :- fail.


% Form of chart entries:
%  chart(Constituent,WhereConstituentStarts,Goals,WhereGoalsStart).
%
% Example (sentence, after parsing NP):
%  chart(s,[the,dog,barked],[vp],[barked]).


% clear_chart
%  Utility to clear the chart before starting a parse

  clear_chart :- abolish(chart/4).   % most Prologs
% clear_chart :- abolish(chart,4).   % ALS Prolog only


% parse(+C,+S1,-S)
%  Parse a constituent of type C from input string S1,
%  leaving remainder of input in S.

parse(C,S1,S) :-
   clear_chart,
   store(chart(start,S1,[C],S1)),
   process(S1),
   chart(C,S1,[],S).


% process(+Position)
%  Starting with input string Position, work through
%  the Earley parsing process.

process([]) :- !.

process(Position) :-
  predictor(Position),
  scanner(Position,NewPosition),
  completer(NewPosition),
  process(NewPosition).


% predictor(+Position)
%  Creates new goals using syntax rules.  See text.

predictor(Position) :-
  chart(C,PC,[Goal|Goals],Position), % For every chart entry of this kind
  predict(Goal,Position),            % do all possible predictions
  fail.

predictor(_).                        % then succeed with no further action.


predict(Goal,Position) :-
  rule(Goal,[H|T]),                  % For every rule expanding Goal
  store(chart(Goal,Position,
               [H|T],Position)),     % make a new chart entry
  predict(H,Position),               % and make predictions from it too
  fail.

predict(_,_).                        % then succeed with no further action.


% scanner(+Position,-NewPosition)
%  Accept a word and use it to satisfy goals.

scanner([W|Words],Words) :-
   chart(C,PC,[G|Goals],[W|Words]), % for each current goal at current position
   word(G,W),                       % if category of W matches it
   store(chart(C,PC,Goals,Words)),  % make a new chart entry
   fail.

scanner([_|Words],Words).           % then succeed with no further action.


% completer(+Position)
%  Process all consituents that are ready to be completed.  See text.

completer(Position) :-
   chart(C,PC,[],Position),   % For every chart entry with no goals
   complete(C,PC,Position),   % complete all possible higher constituents
   fail.

completer(_).                 % then succeed with no further action.


complete(C,PC,Position) :-
   chart(C0,PC0,[C|Goals],PC),  % For every constituent that can be completed
   store(chart(C0,PC0,
            Goals,Position)),   % make a new chart entry,
   Goals == [],                 % then fail here if Goals not empty,
   complete(C0,PC0,Position),   % or process new entry the same way
   fail.

complete(_,_,_).                % then succeed with no further action.



% store(+chart(A,B,C,D))
%  Make a new chart entry if one that subsumes it does not already exist.

store(chart(A,B,C,D)) :-
  \+ (chart(A1,B,C1,D), subsumes_chk(A1,A), subsumes_chk(C1,C)),
  asserta(chart(A,B,C,D)).


% Phrase-structure rules

rule(s,[np,vp]).
rule(np,[d,n]).
rule(vp,[v,np]).
rule(vp,[v,np,pp]).
rule(pp,[p,np]).
rule(d,[]).


% Lexicon

word(d,the).

word(p,near).

word(n,dog).       word(n,dogs).
word(n,cat).       word(n,cats).
word(n,elephant).  word(n,elephants).

word(v,chase).     word(v,chases).
word(v,see).       word(v,sees).
word(v,amuse).     word(v,amuses).


