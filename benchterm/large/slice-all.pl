%%:- entry go.

abort :-
  fail.

append(X) :-
  ground(X).

%% Dynamic

scheck(X) :- '$top'(X).
tracing(X) :- '$top'(X).
loaded(X) :- '$top'(X).
checked(X) :- '$top'(X).
theProgram(X) :- '$top'(X).
traceFile(X) :- '$top'(X).
traceStore(X) :- '$top'(X).
traceConfig(X) :- '$top'(X).
progname(X) :- '$top'(X).
language(X) :- '$top'(X).

topDynamicFunctional(X) :-
        expStar([], X, Y), display(Y), ttynl.
topDynamicFunctional(X) :-
        expTrace([], X, _).

topDynamicImperative(C) :-
        comStar([], C, [], halt, _).
topDynamicImperative(C) :-
        comTrace([], C, [], halt, _).

comStar(E, C, S, C1, S1) :- c1t(E, C, S, C1, S1), !.
comStar(_, halt, S, halt, S) :- !.
comStar(E, X, S, Y, Sf) :-
        c1t(E, X, S, X1, S1), !,
        comStar(E, X1, S1, Y, Sf).
comStar([], C, [], halt, _).

expStar(E, X, Y) :- e1t(E, X, Y), !.
expStar(_, X, X) :- isConst(X), !.
expStar(E, X, Y) :-
        e1t(E, X, X1), !,
        expStar(E, X1, Y).
expStar(E, X, S, X1, S1) :- e1t(E, X, S, X1, S1), !.
expStar(_, X, S, X, S) :- isConst(X), !.
expStar(E, X, S, Y, Sf) :-
        e1t(E, X, S, X1, S1), !,
        expStar(E, X1, S1, Y, Sf).

declStar(E, X, Y) :- d1t(E, X, Y), !.
declStar(_, X, X) :- isEnv(X), !.
declStar(E, X, Y) :-
        d1t(E, X, X1), !,
        declStar(E, X1, Y).
declStar(E, D, S, D1, S1) :- d1t(E, D, S, D1, S1), !.
declStar(_, X, S, X, S) :- isEnv(X), !.
declStar(E, X, S, Y, Sf) :-
        d1t(E, X, S, X1, S1), !,
        declStar(E, X1, S1, Y, Sf).

/*
**    SLice: modulo `sysdep'
**
**    Questo modulo contiene i predicati "system dependent" di SLice.
**    Tutti gli altri moduli sono scritti in Prolog "Clocksin & Mellish",
**    e dunque questo e` l'unico modulo da modificare per effettuare il
**    "porting" di SLice.
**
**    Predicati definiti in questo modulo:
**
**    buildArgsList/2, callSystem/1, cpuTime/1, existingFile/1,
**    invokeBrowser/1, invokeEditor/1, shellCommand/1, showDirectory/1.
*/

:- dynamic tracing/1.

/*
**    Definizioni per
**
**               Interprete: SICStus Prolog;
**        Sistema Operativo: Unix.
*/
/*
:- prolog_flag(unknown, _, fail).
*/

/*
**    cpuTime(X)
**        - X viene istanziato con il tempo macchina, in secondi,
**            utilizzato dal processo.
*/

cpuTime(X) :- statistics(runtime, [_, X]).

/*
**    callSystem(X)
**       - X ad una lista di codici ASCII.
**
**    Esegue (passandolo alla shell) il comando corrispondente
**    alla stringa rappresentata da X.
*/

callSystem(X) :- name(Y, X), unix(system(Y)).

/*
**    existingFile(X)
**       - X istanziata ad un atomo.
**
**    Ha successo se il file di nome X esiste.
*/

existingFile(X) :-
    nofileerrors,
    see(X),
    !,
    seen,
    fileerrors.
existingFile(_) :-
    fileerrors,
    !,
    fail.


/*
**    shellCommand(Args)
**       - Args e` una lista, possibilmente vuota, di atomi
**         che rappresentano (parte di) comandi per la shell.
**
**    Se Args e` la lista vuota viene invocata una shell interattiva,
**    altrimenti viene eseguito il comando shell rappresentato da
**    Args.
*/

shellCommand([]) :- !,
    callSystem("csh").

shellCommand(Args) :-
    buildArgsList(Args, L),
    callSystem(L).

buildArgsList([A], L) :- !, name(A, L).
buildArgsList([A|Args], L) :-
    name(A, L1), buildArgsList(Args, L2),
    append(L1, [32|L2], L).
buildArgsList([], []).

/*
**    showDirectory(Args)
**       - Args e` una lista, possibilmente vuota, di atomi
**         che rappresentano opzioni per il comando `directory'.
*/

showDirectory(Args) :-
    shellCommand([ls|Args]).

/*
**    invokeEditor(Args)
**       - Args e` una lista, possibilmente vuota, di atomi
**         che rappresentano opzioni l'editor.
*/

invokeEditor(Args) :-
    shellCommand(['emacs'|Args]).

/*
**    invokeBrowser(Args)
**       - Args e` una lista, possibilmente vuota, di atomi
**         che rappresentano opzioni per il browser.
*/

invokeBrowser(Args) :-
    shellCommand([less|Args]).

/*
**    SLice: modulo `commands'
**
**    Questo modulo contiene i predicati che implementano l'interazione
**    con l'utente, nonche` il predicato principale di SLice.
**
**    Predicati definiti in questo modulo:
**
**    ambiguousCommand/2, ambiguousKeyword/2, argNeeded/2,
**    doCommand/2, doTrace/2, do_load/1, expand/3, getKeyword/3,
**    go/0, illegalKeyword/2, inWord/1, init/0, introduce/0,
**    noLoaded/0, obeyCommands/0, prefix/2, readCommandLine/1,
**    readCommandLine_1/2, readWord/3, setTrace/1, unknownCommand/1,
**    wordRemainder/3.
*/

/*
**    go
**
**    Predicato principale di SLice, fa partire l'inteprete.
*/

go :-
    init, introduce, obeyCommands, halt.

/*
**    init
**
**    Inizializza le "variabili globali" delll'interprete e
**    asserisce le clausole che dipendono dallo stato del tracing.
*/

init :-
    setGlobal(language(imperative)),
    setGlobal(checked(no)),
    setGlobal(loaded('*no file loaded*')),
    setGlobal(progname('*no program loaded*')),
    setGlobal(traceConfig(yes)),
    setGlobal(traceStore(yes)),
    setGlobal(traceFile(user)),
    setTrace(off).

/*
**    introduce
**
**    Stampa un breve messaggio di avvio.
*/

introduce :-
  ttynl,
  display('Welcome aboard SLice, the Slow and Nice multi-language interpreter'),
  ttynl,
  display('devised and developed by Roberto Bagnara and Alessandro Riaudo.'),
  ttynl,
  display('Type `help'' for advice, `quit'' to exit.'),
  ttynl,
  display('Default settings follows.'),
  ttynl, ttynl,
  doCommand(info, []),
  ttynl.

/*
**    obeyCommands
**
**    Realizza il ciclo di interazione con l'utente:
**    legge un comando, ne controlla la legalita` e,
**    se corretto, ne causa l'esecuzione.
*/

obeyCommands :-
    repeat,
        readCommandLine([Command | Arguments]),
        expand(Command, [help,
                        quit,
                        info,
                        list,
                        load,
                        edit,
                        run,
                        compile,
                        check,
                        trace,
                        language,
                        show,
                        shell,
                        directory], C),
        (C=[] -> (unknownCommand(Command), fail) ;
            (C=[C1] -> doCommand(C1, Arguments) ;
                (ambiguousCommand(Command, C), fail))),
        C1 = quit,  !,
        display('Bye!'), ttynl.

unknownCommand(C) :-
  display('Unknown command `'), display(C),
  display(''': type `help'' for advice or `quit'' to exit.'), ttynl.

ambiguousCommand(C, L) :-
  display('Ambiguous command `'), display(C),
  display(''': choose one of '), display(L), ttynl.

/*
**    doCommand(Command, Args)
**        - Command e` un atomo che rappresenta il comando;
**        - Args e` una lista (possibilmente vuota) di argomenti.
**
**    Provoca l'esecuzione del comando (SLice) Command con
**    argomenti Args.
*/

/*
**    help
*/

doCommand(help, _) :- !, ttynl,
  display('SLice provides you the following commands:'),
  ttynl, ttynl,
  display('help      - the command you have just issued;'),
  ttynl,
  display('quit      - leave SLice, back to the system;'),
  ttynl,
  display('info      - displays the current interpreter status;'),
  ttynl,
  display('list  <f> - show file <fname> on the screen;'),
  ttynl,
  display('load  <f> - load program from file <fname>;'),
  ttynl,
  display('edit  <f> - edit file <f>;'),
  ttynl,
  display('run       - run the currently loaded program, if any;'),
  ttynl,
  display('check     - check static semantics of the currently'),
  ttynl,
  display('          - loaded program, if any;'),
  ttynl,
  display('trace ... - set trace status, possible formats are:'),
  ttynl,
  display('  trace on/off            - turn tracing on/off'),
  ttynl,
  display('  trace config/store/both - what to trace'),
  ttynl,
  display('  trace file <f>          - where to trace'),
  ttynl,
  display('lang <l>  - set the language type, possible values for <l>'),
  ttynl,
  display('            are: `imperative'' (default), `functional'';'),
  ttynl,
  display('show      - pretty print abstract syntax of program;'),
  ttynl,
  display('shell <c> - if <c> omitted open a nested shell session,'),
  ttynl,
  display('            shell command <c> is executed otherwise,'),
  ttynl,
  display('dir       - shows the current directory contents;'),
  ttynl.

/*
**    quit
*/

doCommand(quit, _).

/*
**    info
*/

doCommand(info, []) :- !,
    language(LType), checked(CStat),
    tracing(TStat), traceStore(TStor),
    traceConfig(TConf), traceFile(TFile),
    loaded(File), progname(PName),
    display('The language currently recognized is: '),
        display(LType), display(';'), ttynl,
    display('    program file currently loaded is: '),
        display(File), display(';'), ttynl,
    display('                     program name is: '),
        display(PName), display(';'), ttynl,
    display('                and has been checked: '),
        display(CStat), display(';'), ttynl,
    display('                      tracing is set: '),
        display(TStat), display(';'), ttynl,
    display('              tracing configurations: '),
        display(TConf), display(';'), ttynl,
    display('                       tracing store: '),
        display(TStor), display(';'), ttynl,
    display('              tracing output goes to: '),
        display(TFile), display('.'), ttynl.

doCommand(info, [_|_]) :- !,
    argNeeded(info, 0).

/*
**    list <filename>
*/

doCommand(list, Args) :- !,
    invokeBrowser(Args).

doCommand(list, []) :- !,
    argNeeded(list, 1).

/*
**    load <filename>
*/

doCommand(load, [File]) :- !,
    (existingFile(File) ->
        do_load(File)
    ;
        (display('Cannot find `'), display(File),
        display(''''), ttynl)).

doCommand(load, []) :- !,
    argNeeded(load, 1).

do_load(File) :- !,
  display('Loading program file `'),
  display(File), display(''''), ttynl,
  seeing(Oldfile),
  see(File),
  display('Tokenising... '), ttyflush,
  tokenizeProgram(L),
  seen,
  see(Oldfile),
  display('done.'), ttynl,
  display('Parsing... '), ttyflush,
  (((language(functional),
        expr(A, L, []), setGlobal(progname('*no name*')))
    ;
    (language(imperative),
        program(N, A, L, []), setGlobal(progname(N)))) ->
            (display('syntax ok!'), ttynl,
            setGlobal(theProgram(A)),
            setGlobal(loaded(File)),
            setGlobal(checked(no)))
  ;
            (display('*bleach*, syntax error!'), ttynl)).

/*
**    edit
*/

doCommand(edit, Args) :- !,
  invokeEditor(Args).

/*
**    run
*/

doCommand(run, []) :-
  noLoaded, !.

doCommand(run, []) :- !,
    (checked(wrong) ->
        (display('Static semantics check failed.'), ttynl, !, fail)
    ;
        (checked(no) ->
            doCommand(check, [])
        ;
            true)),
    (checked(yes) ->
        (theProgram(X),
        setGlobal(currentStep(-1)),
        setGlobal(currentLocNum(-1)),
        cpuTime(StartTime),
        ((language(functional), topDynamicFunctional(X)) ;
         (language(imperative), topDynamicImperative(X))), !,
        cpuTime(EndTime),
        CpuTime is EndTime-StartTime,
        display(CpuTime), print(' seconds to run.'), ttynl)
    ;
        true).

doCommand(run, [_|_]) :- !,
    argNeeded(run, 0).

/*
**    compile
*/

doCommand(compile, []) :-
  noLoaded, !.

doCommand(compile, []) :- !,
    (checked(wrong) ->
        (display('Static semantics check failed.'), ttynl, !, fail)
    ;
        (checked(no) ->
            doCommand(check, [])
        ;
            true)),
    (checked(yes) ->
        (theProgram(X),
        setGlobal(currentLabNum(-1)),
        cpuTime(StartTime),
        my_compile(X),
        cpuTime(EndTime),
        CpuTime is EndTime-StartTime,
        display(CpuTime), print(' seconds to compile.'), ttynl)
    ;
        true).

doCommand(compile, [_|_]) :- !,
    argNeeded(run, 0).

/*
**    check
*/

doCommand(check, []) :-
    noLoaded, !.

doCommand(check, []) :- !,
    theProgram(X),
    setGlobal(scheck(ok)),
    (language(functional) ->
        ((essem([], X, T), scheck(ok)) ->
             display('Static semantics ok, expression type is '),
             display(T), display(.), ttynl,
             setGlobal(checked(yes))
        ;
             display('Static semantics check failed.'), ttynl,
             setGlobal(checked(wrong))
        )

    ; /* language(imperative) */
        ((wfc([], X), scheck(ok)) ->
             display('Static semantics ok.'), ttynl,
             setGlobal(checked(yes))
        ;
             display('Static semantics check failed.'), ttynl,
             setGlobal(checked(wrong))
        )
    ).

doCommand(check, [_|_]) :- !,
    argNeeded(check, 0).

/*
**    trace
*/

doCommand(trace, [X|L]) :- !,
    getKeyword(X, [on, off, configurations, store, both, file, tty], S),
    doTrace(S, L), !.

doCommand(trace, _) :- !,
    argNeeded(trace, 1).

doTrace(on, []) :-
    setTrace(on).
doTrace(off, []) :-
    setTrace(off).
doTrace(configurations, []) :-
    setGlobal(traceConfig(yes)),
    setGlobal(traceStore(no)).
doTrace(store, []) :-
    setGlobal(traceConfig(no)),
    setGlobal(traceStore(yes)).
doTrace(both, []) :-
    setGlobal(traceConfig(yes)),
    setGlobal(traceStore(yes)).
doTrace(file, [F]) :-
    setGlobal(traceFile(F)).
doTrace(tty, []) :-
    setGlobal(traceFile(user)).
doTrace(_, _) :-
    argNeeded(trace, -1).

setTrace(S) :- tracing(S), !.
setTrace(S) :-
    retractall(expStar(_,_,_)),
    retractall(expStar(_,_,_,_,_)),
    retractall(declStar(_,_,_)),
    retractall(declStar(_,_,_,_,_)),
    retractall(comStar(_,_,_,_,_)),
    retractall(topDynamicFunctional(_)),
    retractall(topDynamicImperative(_)),
    traceDependentClause(S, X),
    assertz(X),
    fail.
setTrace(S) :- setGlobal(tracing(S)).

/*
**    language imperative/functional
*/

doCommand(language, [X]) :- !,
    getKeyword(X, [imperative, functional], S),
    setGlobal(language(S)).

doCommand(language, _) :- !,
    argNeeded(language, 1).

/*
**    show
*/

doCommand(show, []) :-
    noLoaded, !.

doCommand(show, []) :-
    theProgram(X),
    pp(X), !.

doCommand(show, [_|_]) :- !,
    argNeeded(show, 0).

/*
**    shell
*/

doCommand(shell, Args) :- !,
    shellCommand(Args), ttynl.

/*
**    directory
*/

doCommand(directory, Args) :- !,
    showDirectory(Args).

/*
**    noLoaded
**
**    Ha successo se in SLice non e` stato caricato alcun
**    programma. Stampa un messaggio d'errrore.
*/

noLoaded :-
    loaded('*no file loaded*'),
    display('No program has been loaded yet: use `load'''),
    ttynl.

/*
**    Predicati per la lettura e interpretazione dei comandi.
**    -------------------------------------------------------
*/

/*
**    readCommandLine(L)
**        - L e` una lista di atomi
**
**    Legge una linea (fino al carriage-return) di comando dal terminale,
**    istanziando L come la lista delle parole (stringhe di caratteri
**    non-blank) lette. Ad esempio: se l'utente digita
**
**        load test/mergesort
**
**        L = ['load', 'test/mergesort']
*/

readCommandLine(L) :-
    prompt(Old, 'SLice> '),
    get0(C), readCommandLine_1(C,L),
    prompt(_, Old).

readCommandLine_1(10, []) :- ! .
readCommandLine_1(C, X) :-
    readWord(C, Word, C1), readCommandLine_1(C1, Words),
    ((Word = skip) -> X = Words ; X = [Word|Words]).

readWord(C, W, C2) :- inWord(C), ! , 
    get0(C1), wordRemainder(C1, Cs, C2), 
    name(W, [C|Cs]).
readWord(_, skip, C1) :- get0(C1).

wordRemainder(C, [C|Cs], C2) :-
    inWord(C), !, 
    get0(C1), wordRemainder(C1, Cs, C2).
wordRemainder(C, [], C).

inWord(C) :- (C=32 ; C=10), !, fail.
inWord(_).

/*
**    prefix(X, Y)
**        - X, Y sono liste di codici ASCII.
**
**    Vero se la stringa Y e` un prefisso di X.
*/

prefix([X|TS], [X|TA]) :- prefix(TS, TA). 
prefix([_|_], []).
prefix([], []).

/*
**    expand(Short, KeyList, Keys)
**        - Short e` un atomo;
**        - KeyList, Keys sono liste di atomi.
**
**    Vero se Keys e` la lista degli atomi in KeyList
**    di cui Short e` un abbreviazione.
*/

expand(Short,  [Key|KeyList], [Key|OtherKeys]) :- 
    name(Short, ShortL), 
    name(Key, KeyL), 
    prefix(KeyL, ShortL), 
    expand(Short, KeyList, OtherKeys),  !.

expand(Short, [_|KeyList], OtherKeys) :-
    expand(Short, KeyList, OtherKeys),  !.
expand(_, [], []).

/*
**    getKeyword(Short, KeyList, Key)
**
**        - Short e Key sono atomi;
**        - KeyList e` una lista di atomi.
**
**    Vero se Key e` l'unico atomo in KeyList di cui Short e`
**    una abbreviazione. In caso di errore vengono stampati
**    i relativi messaggi.
*/

getKeyword(Key, List, ExpandedKey) :-
    expand(Key, List, C),
    (C=[] -> (illegalKeyword(Key, List), fail) ;
        (C=[ExpandedKey] -> true ;
            (ambiguousKeyword(Key, C), fail))).

illegalKeyword(Key, Valid) :-
  display('Illegal keyword `'), display(Key),
  display(''': must be one of '), display(Valid), ttynl.

ambiguousKeyword(Key, Choices) :-
  display('Ambiguous keyword `'), display(Key),
  display(''': choose one of '), display(Choices), ttynl.

/*
**    argNeeded(Command, N)
**        - Command e` un atomo, nome del comando;
**        - N e` un intero.
**
**    Visualizza un messaggio di errore riguardante il numero
**    di argomenti del comando Command.
*/

argNeeded(Command, -1) :- !,
  display('Wrong number of arguments for command: `'),
  display(Command), display(''''), ttynl.

argNeeded(Command, N) :-
  display('Illegal command: `'), display(Command), display(''' requires '),
  (N =:= 0 -> display('no') ; display(N)),
  (N =:= 1 -> display(' argument') ; display(' arguments')), ttynl.

/*
**    SLice: modulo `misc'
**
**    Questo modulo contiene alcuni predicati di utilita` utilizzati
**    qua e la` in  SLice.
**
**    Predicati definiti in questo modulo:
**
**    append/3, intersection/3, member/2, moreThanOnce/2,
**    retractall/1, setGlobal/1, subtraction/3, union/3.
*/

/*
**    setGlobal(X)
**        - X e` un termine del tipo f(Y).
**
**    La clausola unitaria f(Y) viene aggiunta al database, eventualmente
**    sostituendo una clausola f(Z) preesistente.
*/

setGlobal(X) :- functor(X, F, 1), Y =.. [F, _],
    (retract(Y) ; true), assert(X), !.

/*
**    member(E, L)
**    append(L1, L2, L3)
**
**    Le usuali operazioni su liste.
*/

member(Elem, [Elem|_]) :- ! .
member(Elem, [_|Tail]) :- member(Elem,Tail).

append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).
append([], L, L).

/*
**    intersection(X, Y, Z)
**    union(X, Y, Z)
**    subtraction(X, Y, Z)
**        - X, Y, Z sono liste che rappresentano insiemi.
**
**    Sono le attese operazioni insiemistiche, ad esempio:
**
**        union(A, B, C) <=> C rappresenta l'insieme unione degli insiemi
**                           rappresentati da A e B.
*/

union([X|R], Y, Z) :- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]) :- union(R, Y, Z).
union([], X, X).

intersection([Head|Tail],List,[Head|Remainder]) :-
    member(Head,List), ! ,
    intersection(Tail,List,Remainder).
intersection([_|Tail],List,Res) :- intersection(Tail,List,Res).
intersection([],_,[]).

subtraction([X|R], Y, Z) :- member(X, Y), !, subtraction(R, Y, Z).
subtraction([X|R], Y, [X|Z]) :- subtraction(R, Y, Z).
subtraction([], _, []).

/*
**    moreThenOnce(L, D)
**        - L e` una lista;
**        - D e` la lista degli elementi che compaiono in L
**          piu` di una volta.
*/

moreThanOnce([H|T], D) :-
    member(H, T), !,
    moreThanOnce(T, C), union([H], C, D).
moreThanOnce([_|T], D) :-
    moreThanOnce(T, D).
moreThanOnce([], []).

/*
**    retractall(X)
**
**    Rimuove tutte le clausole la cui testa unifica con X
**    dal database.
*/
/*RB
retractall(X) :- retract(X), fail.
retractall(X) :- retract(( X :- _)), fail.
retractall(_).
*/
/*
**    SLice: modulo `tokenize'
**
**    Questo modulo contiene il tokenizzatore di SLice. I predicati
**    qui definiti sono stati desunti (ma con notevoli modifiche),
**    dal testo di Clocksin & Mellish.
**
**    Predicati definiti in questo modulo:
**
**    doubleCharacter/2, inToken/2, moreString/4, readString/3,
**    readString/4, readToken/3, singleCharacterToken/1, skipComment/0,
**    skipComment1/0, startComment/2, tokenRemainder/3, tokenize/2,
**    tokenizeProgram/1. 
*/

/*
**    tokenizeProgram(L)
**
**    Questo e` il predicato principale del tokenizzatore.
**    L viene istanziato con la lista di token corrispondente alla
**    lettura dello stream di ingresso corrente fino a EOF.
*/

tokenizeProgram(L) :- get0(C), tokenize(C,L).

/* Ferma la lettura a fine file */
/* get0(-1) a fine file         */

tokenize(-1, []) :- ! .

tokenize(C, X) :-
    readToken(C,Word,C1),
    ((Word = skip) -> X = Words ; X = [Word|Words]),
    tokenize(C1,Words).

readToken(34, W, C) :- !,
    readString(L, 34, C), name(W, L).

readToken(C,W,C2) :- singleCharacterToken(C), ! ,
    get0(C1),
    (startComment(C,C1) ->
        (skipComment, !, W=skip, get0(C2))
    ;
        (doubleCharacter(C,C1) ->
            (name(W,[C,C1]), get0(C2))
        ;
            (name(W,[C]), C2 = C1))).

readToken(C,W,C2) :- inToken(C,NewC), ! ,
    get0(C1), tokenRemainder(C1,Cs,C2),
    name(W,[NewC|Cs]).

readToken(_,skip,C1) :- get0(C1).

tokenRemainder(C,[NewC|Cs],C2) :-
    inToken(C,NewC), !,
    get0(C1), tokenRemainder(C1,Cs,C2).
tokenRemainder(C,[],C).

readString(Chars, Quote, NextCh) :-
    get0(Ch),
    readString(Ch, Chars, Quote, NextCh).

readString(-1, _, Quote, -1) :-
    display('! end of file in '), ttyput(Quote),
    display(token), ttyput(Quote), ttynl,
    !, fail.
readString(Quote, Chars, Quote, NextCh) :- !,
    get0(Ch),                               /* Chiuso o doppio apice */
    moreString(Ch, Quote, Chars, NextCh).
readString(Char, [Char|Chars], Quote, NextCh) :-
    readString(Chars, Quote, NextCh).       /* Carattere normale */

moreString(Quote, Quote, [Quote|Chars], NextCh) :- !,
    readString(Chars, Quote, NextCh).       /* Doppio apice */
moreString(NextCh, _, [], NextCh).          /* Fine */

startComment(47, 42).

skipComment :-
    get0(C),
    (C = -1 ; (C=42, skipComment1) ; skipComment).

skipComment1 :-
    get0(C),
    (C = -1 ; C=47 ; (C=42, skipComment1) ; skipComment).

singleCharacterToken(40).  /* ascii('(',40)  */
singleCharacterToken(41).  /* ascii(')',41)  */
singleCharacterToken(42).  /* ascii('*',42)  */
singleCharacterToken(43).  /* ascii('+',43)  */
singleCharacterToken(44).  /* ascii(',',44)  */
singleCharacterToken(45).  /* ascii('-',45)  */
singleCharacterToken(46).  /* ascii('.',46)  */
singleCharacterToken(47).  /* ascii('/',47)  */
singleCharacterToken(58).  /* ascii(':',58)  */
singleCharacterToken(59).  /* ascii(';',59)  */
singleCharacterToken(60).  /* ascii('<',60)  */
singleCharacterToken(61).  /* ascii('=',61)  */
singleCharacterToken(62).  /* ascii('>',62)  */
singleCharacterToken(91).  /* ascii('[',91)  */
singleCharacterToken(93).  /* ascii(']',93)  */
singleCharacterToken(124). /* ascii('|',124) */

doubleCharacter(58, 61). /* := */
doubleCharacter(61, 61). /* == */
doubleCharacter(60, 61). /* <= */
doubleCharacter(62, 61). /* >= */
doubleCharacter(60, 62). /* <> */
doubleCharacter(46, 46). /* .. */
doubleCharacter(45, 62). /* -> */

inToken(C,C) :- C >= 97, C =< 122 .  /* the lower-case letters */
inToken(C,C) :- C >= 65, C =< 90 .   /* the upper-case letters */
inToken(C,C) :- C >= 48, C =< 57 .   /* the digits */
inToken(95,95). /* ascii('_',95) */

/*
**    SLice: modulo `parse'
**
**    Questo modulo contiene il parser di SLice. E` stato realizzato
**    utilizzando le DCG (Definite Clause Grammars), un formalismo
**    per la realizzazione di parser in Prolog.
**
**    Le DCG permettono di realizzare parser in maniera molto veloce
**    e leggibile, una DCG e` leggibile quasi quanto la produzione
**    di una qualunque grammatica libera.
**    Vedi "Clocksin & Mellish" e "Sterling & Shapiro".
**
**    Il parser accetta in ingresso una lista di token prodotta
**    dal modulo `tokenize' e produce l'albero astratto del programma
**    sotto forma di termine Prolog.
**
**    Il parsing viene effettuato in modo top-down CON backtracking,
**    per il solo motivo che viene consentita l'omissione del ramo
**    else nel comando condizionale.
**    Se si decidesse di eliminare tale possibilita` la grammatica
**    sarebbe LL(1) e quindi il parsing deterministico (predictive).
**
**    Predicati definiti in questo modulo:
**
**    afterBegin/3, afterSide/5, alphaNumeric/1, boolConstant/3,
**    compStat/3, decl/3, decl1/3, decl2/3, decl3/3, expr/3,
**    exprList/3, factor/3, factorAfterId/4, forDirection/5,
**    formList/3, formal/3, funcHead/5, id/3, init/3, lexId/1,
**    limit/3, limits/3, maybeElse/3, maybeInit/4, moreDecl/4,
**    moreDecl1/4, moreDecl2/4, moreExpr/4, moreLimits/4, moreNeEL/4,
**    moreNeFL/4, morePrint/3, moreSExpr/4, moreSList/4, moreTerm/4,
**    neExprList/3, neFormList/3, num/3, optionalStat/3, procHead/4,
**    program/4, reserved/1, simpleExpr/3, simpleType/3, stat/3,
**    statAfterId/4, statList/3, term/3, type/3, unsignedNum/3,
**    varAfterId/4. 
*/

/*
**    Tipi semplici.
**    --------------
*/

simpleType(int) --> [integer], !.
simpleType(bool) --> [boolean], !.

/*
**    Tipi in genere, array compresi.
**    -------------------------------
*/

type(T) --> simpleType(T), !.

type(array(Limits, T)) --> {language(imperative)},
    [array], !, ['['], limits(Limits), [']'], [of], simpleType(T).

limit(b(L, H)) --> num(L), ['..'], num(H).

limits(X) --> limit(Y), moreLimits([Y], X).

moreLimits(Y, X) --> [','], !, limit(Z), {append(Y, [Z], W)}, moreLimits(W, X).
moreLimits(X, X) --> [].

/*
**    Identificatori.
**    ---------------
*/

id(X) --> [X], {atom(X), lexId(X), !, \+ reserved(X)}.

lexId(X) :- name(X, [C|L]),            /* Il primo carattere puo` essere: */
               ((C >= 97, C =< 122) ;  /*   una lettera minuscola o       */
                (C >= 65, C =< 90)  ;  /*   una lettera maiuscola o       */
                 C = 95),              /*   il carattere underscore "_".  */
                   alphaNumeric(L).

alphaNumeric([C|T]) :- ((C >= 97, C =< 122) ;
                 (C >= 65, C =< 90)  ;
                 (C >= 48, C =< 57)  ;
                  C = 95),
                    alphaNumeric(T).
alphaNumeric([]).

/*
**    Variabile o riferimento di array (dopo id).
**    -------------------------------------------
*/

varAfterId(X, aref(X, El)) --> {language(imperative)},
    ['['], !, neExprList(El), [']'].

varAfterId(X, i(X)) --> [].

/*
**    Costanti numeriche.
**    -------------------
*/

unsignedNum(X) --> [X], {integer(X)}, !.

num(X) --> unsignedNum(X), !.
num(X) --> [+], !, unsignedNum(X).
num(X) --> [-], !, unsignedNum(Y), {X is -Y}.

/*
**    Costanti booleane.
**    ------------------
*/

boolConstant(tt) --> [true], !.
boolConstant(ff) --> [false], !.

/*
**    Espressioni.
**    ------------
*/

expr(X) --> simpleExpr(Y), moreExpr(Y, X).

/*
**    moreExpr(Y, X)
**        - Y e` l'albero costruito finora in questa sequenza
**            di termini (ereditato);
**        - X e` l'albero finale (sintetizzato).
**
**    La stessa tecnica (che tra l'altro consente di realizzare
**    l'associativita` a sinistra degli operatori) e` utilizzata
**    piu` volte nel seguito.
**    Vedi Aho, Sethi, Ullman:
**         "Compilers: principles, techniques and tools", chapter 5.
*/

moreExpr(Y, X) --> [==], !, simpleExpr(Z), moreExpr(eq(Y, Z), X).
moreExpr(Y, X) --> [<>], !, simpleExpr(Z), moreExpr(ne(Y, Z), X).
moreExpr(Y, X) --> [<],  !, simpleExpr(Z), moreExpr(lt(Y, Z), X).
moreExpr(Y, X) --> [<=], !, simpleExpr(Z), moreExpr(le(Y, Z), X).
moreExpr(Y, X) --> [>=], !, simpleExpr(Z), moreExpr(ge(Y, Z), X).
moreExpr(Y, X) --> [>],  !, simpleExpr(Z), moreExpr(gt(Y, Z), X).
moreExpr(X, X) --> [].

simpleExpr(X) --> term(Y), moreSExpr(Y, X).

moreSExpr(Y, X) --> [+],  !, term(Z), moreSExpr(plus(Y, Z), X).
moreSExpr(Y, X) --> [-],  !, term(Z), moreSExpr(minus(Y, Z), X).
moreSExpr(Y, X) --> [or], !, term(Z), moreSExpr(or(Y, Z), X).
moreSExpr(X, X) --> [].

term(X) --> factor(Y), moreTerm(Y, X).

moreTerm(Y, X) --> [*],   !, factor(Z), moreTerm(prod(Y, Z), X).
moreTerm(Y, X) --> [div], !, factor(Z), moreTerm(div(Y, Z), X).
moreTerm(Y, X) --> [mod], !, factor(Z), moreTerm(mod(Y, Z), X).
moreTerm(Y, X) --> [and], !, factor(Z), moreTerm(and(Y, Z), X).
moreTerm(X, X) --> [].

factor(X) --> unsignedNum(X), !.
factor(X) --> boolConstant(X), !.
factor(X) --> ['('], !, expr(X), [')'].

factor(X) --> id(Y), !, factorAfterId(Y, X).

/*
**    Chiamata di funzione.
**    ---------------------
*/

factorAfterId(Y, funcall(Y, ExprList)) -->
    ['('], !, exprList(ExprList), [')'].

/*
**    Identificatore o riferimento di array.
**    --------------------------------------
*/

factorAfterId(Y, X) --> varAfterId(Y, X), !.

/*
**    Operatori unari.
**    ----------------
*/

factor(not(X)) -->    [not], !, factor(X).
factor(X) -->         [+],   !, factor(X).
factor(uminus(X)) --> [-],   !, factor(X).

/*
**    Blocco delle espressioni.
**    -------------------------
*/

factor(let(D,E)) --> {language(functional)}, [let], !, decl(D), [in], expr(E).

/*
**    Espressione condizionale.
**    -------------------------
*/

factor(if(E,E1,E2)) --> [if], !, expr(E), [then], expr(E1), [else], expr(E2).

/*
**    Espressioni con side-effects.
**    -----------------------------
*/

factor(side(D, C, E)) --> {language(imperative)},
    [expr], !, afterSide(D, C, E).

afterSide(D, C, E)   --> decl(D), !, [;], optionalStat(C), [result], expr(E).
afterSide(nil, C, E) --> optionalStat(C), !, [result], expr(E).

/*
**    Input dall'utente.
**    ------------------
*/

factor(input(X)) --> [input], !, simpleType(X).

/*
**    Lista di espressioni (attuali).
**    -------------------------------
*/

exprList(X) --> neExprList(X).
exprList([]) --> [].

neExprList(X) --> expr(Y), moreNeEL([Y], X).

moreNeEL(Y, X) --> [','], !, expr(Z), {append(Y, [Z], W)}, moreNeEL(W, X).
moreNeEL(X, X) --> [].

/*
**    Dichiarazioni.
**    --------------
*/

decl(X) --> decl1(Y), moreDecl(Y, X).

/*
**    Composizione sequenziale.
**    -------------------------
*/

moreDecl(Y, X) --> [;], decl1(Z), moreDecl(seq(Y, Z), X).
moreDecl(X, X) --> [].

decl1(X) --> decl2(Y), moreDecl1(Y, X).

/*
**    Composizione parallela.
**    -----------------------
*/

moreDecl1(Y, X) --> ['|'], !, decl2(Z), moreDecl1(parallel(Y, Z), X).
moreDecl1(X, X) --> [].

decl2(X) --> decl3(Y), moreDecl2(Y, X).

/*
**    Composizione privata.
**    ---------------------
*/

moreDecl2(Y, X) --> [->], !, decl3(Z), moreDecl2(private(Y, Z), X).
moreDecl2(X, X) --> [].

/*
**    Dichiarazione semplice.
**    -----------------------
*/

decl3(def(X,T,E)) --> {language(functional)},
                          id(X), !, [:], simpleType(T), init(E).

/*
**    Dichiarazione di costante.
**    --------------------------
*/

decl3(con(X,T,E)) --> {language(imperative)},
                          [const], !, id(X), [:], simpleType(T),
                               init(E).

/*
**    Dichiarazione di variabile.
**    ---------------------------
*/

decl3(var(X,T,E)) --> {language(imperative)},
                          [var], !, id(X), [:], type(T),
                              maybeInit(T, E).

/*
**    Parentetizzazione delle dichiarazioni.
**    --------------------------------------
*/

decl3(X) --> ['('], !, decl(X), [')'].

/*
**    Dichiarazione ricorsiva.
**    ------------------------
*/

decl3(rec(X)) --> [rec], !, decl(X).

/*
**    Inizializzazione di variabile (obbligatoria ed opzionale).
**    ----------------------------------------------------------
*/

init(E) --> [=], expr(E).

maybeInit(_, E) --> init(E).
maybeInit(T, X) --> [],
    {((T=array(_, TypeArray), RealType=TypeArray) ; RealType=T),
        ((RealType=int, X=0) ; (RealType=bool, X=ff))}.

/*
**    Dichiarazione di funzione.
**    --------------------------
*/

decl3(fundef(F, Formals, T, E)) -->
    funcHead(F, Formals, T), !, [=], expr(E).

/*
**    Dichiarazione di procedura.
**    ---------------------------
*/

decl3(procdef(P, Formals, C)) --> {language(imperative)},
    procHead(P, Formals), !, compStat(C).

/*
**    Testa della dichiarazione di procedura.
**    ---------------------------------------
*/

procHead(P, Formals) --> [procedure],
    id(P), ['('], formList(Formals), [')'].

/*
**    Testa della dichiarazione di funzione.
**    --------------------------------------
*/

funcHead(F, Formals, T) --> [function],
    id(F), ['('], formList(Formals), [')'], [:], simpleType(T).

/*
**    Parametri formali.
**    ------------------
*/

formList(X) --> neFormList(X).
formList([]) --> [].

neFormList(X) --> formal(Y), moreNeFL([Y], X).

moreNeFL(Y, X) --> [','], !, formal(Z), {append(Y, [Z], W)}, moreNeFL(W, X).
moreNeFL(X, X) --> [].

formal(ival(X,T)) --> {language(imperative)},    /* call by value         */
    id(X), !, [:], simpleType(T).
formal(fval(X,T)) --> {language(functional)},    /* call by value         */ 
    id(X), !, [:], simpleType(T).
formal(name(X,T)) -->                            /* call by name          */
    [name], !, id(X), [:], simpleType(T).
formal(ref(X,T)) --> {language(imperative)},     /* call by reference     */
    [ref], !, id(X), [:], simpleType(T).
formal(const(X,T)) --> {language(imperative)},   /* call by constant      */
    [const], !, id(X), [:], simpleType(T).
formal(copy(X,T)) --> {language(imperative)},    /* call by value-result  */
    [copy], !, id(X), [:], simpleType(T).
formal(funcpar(F, Formals, T)) -->               /* parametro funzionale  */ 
    funcHead(F, Formals, T), !.
formal(procpar(P, Formals)) -->                  /* parametro procedurale */
    {language(imperative)}, 
    procHead(P, Formals), !.

/*
**    Comandi.
**    --------
*/

/*
**    Blocco di comandi o comando composto.
**    -------------------------------------
*/

compStat(X) --> [begin], afterBegin(X).

afterBegin(block(D, C)) --> decl(D), !, [;], optionalStat(C), [end].
afterBegin(C) --> optionalStat(C), !, [end].

/*
**    Sequenza di comandi opzionale.
**    ------------------------------
*/

optionalStat(X) --> statList(X).
optionalStat(nop) --> [].

/*
**    Sequenza di comandi.
**    --------------------
*/

statList(X) --> stat(Y), moreSList(Y, X).

moreSList(Y, X) --> [;], stat(Z), moreSList(seq(Y, Z), X).
moreSList(X, X) --> [].

/*
**    Comandi.
**    --------
*/

/*
**    Chiamata di procedura o assegnamento.
**    -------------------------------------
*/

stat(X) -->
    id(Y), !, statAfterId(Y, X).

/*
**    Chiamata di procedura.
**    ----------------------
*/

statAfterId(Y, proccall(Y, ExprList)) -->
    ['('], !, exprList(ExprList), [')'].

/*
**    Assegnamento.
**    -------------
*/

statAfterId(Y, ass(X, E)) -->
    varAfterId(Y, X), !, [:=], expr(E).

/*
**    Comando composto.
**    -----------------
*/

stat(X) -->
    compStat(X), !.

/*
**    Comando condizionale.
**    ---------------------
*/

stat(if(E, C1, C2)) -->
    [if], expr(E),
    [then], stat(C1),
    maybeElse(C2).

maybeElse(C) --> [else], !, stat(C).
maybeElse(nop) --> [].

/*
**    Comando while.
**    --------------
*/

stat(while(E,C)) -->
    [while], !, expr(E), [do], stat(C).

/*
**    Comando di uscita `print'.
**    --------------------------
*/

stat(print(Format, El)) -->
    [print], !, ['('], [Format], morePrint(El).
morePrint(El) -->
    [','], !, exprList(El), [')'].
morePrint([]) -->
    [')'].

/*
**    Comando for.
**    ------------
**
**    N.B.: Il comando `for' NON ha un corrispondente in sintassi astratta
**          ma viene espanso dal parser in un ciclo `while' come segue:
**
**                for X := E1 to (downto) E2 do C
**
**          viene tradotto in
**
**                X := E1;
**                begin
**                  const $limit : integer = E2;
**                  while X <= (>=) $limit do begin
**                    begin
**                      const X : integer = X;
**                      C
**                    end;
**                    X := X + (-) 1
**                  end
**                end
**
**          Si noti come C (il comando) sia eseguito in un ambiente
**          in cui X (l'indice del for) e` una costante, e quindi
**          non modificabile.
**          Il comando for del linguaggio imperativo di SLice e` dunque
**          sintatticamente e semanticamente identico a quello di
**          Pascal.
*/

stat(seq(ass(i(X),E1),
            block(con('$limit',int,E2),
              while(Comparison,
                seq(block(con(X,int,i(X)),C),ass(i(X),Update))))))
  --> [for], !, id(X), [:=], expr(E1), forDirection(X, Comparison, Update),
          expr(E2), [do], stat(C).

forDirection(X, le(i(X),i('$limit')), plus(i(X),1)) -->  [to], !.
forDirection(X, ge(i(X),i('$limit')), minus(i(X),1)) --> [downto], !.

/*
**    Programma (categoria iniziale per il linguaggio imperativo).
**    ------------------------------------------------------------
*/

program(Name, Command) --> [program], id(Name), compStat(Command).

/*
**    Parole riservate.
**    -----------------
*/

reserved(program) :- language(imperative).
reserved(const) :- language(imperative).
reserved(var) :- language(imperative).
reserved(array) :- language(imperative).
reserved(of) :- language(imperative).
reserved(begin) :- language(imperative).
reserved(end) :- language(imperative).
reserved(expr) :- language(imperative).
reserved(result) :- language(imperative).
reserved(ref) :- language(imperative).
reserved(copy) :- language(imperative).
reserved(while) :- language(imperative).
reserved(do) :- language(imperative).
reserved(for) :- language(imperative).
reserved(to) :- language(imperative).
reserved(downto) :- language(imperative).
reserved(procedure) :- language(imperative).
reserved(print) :- language(imperative).

reserved(let) :- language(functional).
reserved(in) :- language(functional).

reserved(integer).
reserved(boolean).
reserved(input).
reserved(name).
reserved(rec).
reserved(and).
reserved(or).
reserved(mod).
reserved(div).
reserved(if).
reserved(then).
reserved(else).
reserved(true).
reserved(false).
reserved(not).
reserved(function).

/*
**    SLice: modulo `statsem'
**
**    Questo modulo contiene i predicati che implementano tutti
**    i controlli e le funzioni di semantica statica di SLice.
**
**    Predicati definiti in questo modulo:
**
**    aeMatchForm/3, cFV/2, dDV/2, dFV/2, dssem/2, eFV/2, elFV/2,
**    elssem/3, envDomain/2, envRangeFV/2, essem/3, fEnv/2, formDV/2,
**    formMatchForm/2, limitsOk/1, nIndexOk/2, tIndexOk/1, wfc/2, wfd/2.
*/

/*
**                          IMPORTANTE
**                          ==========
**
**    I predicati wfd/3, essem/3, aeMatchForm/3 e wfc/2, definiti sotto,
**    hanno sempre successo per consentire un'analisi completa
**    dell'albero astratto e la generazioni di messaggi di errore
**    sensati e non ridondanti.
**
**    Quando un errore viene rilevato, i predicati di cui sopra,
**    invocano error/1 che provvede ad asserire la clausola
**    `scheck(failed)'. La presenza di questa clausola permette,
**    al termine dell'analisi di semantica statica di sapere
**    se il programma e` corretto (rispetto ai tipi) oppure no.
**    Al predicato error/1 e` inoltre demandato il compito di
**    fornire un messaggio d'errore per l'utente.
**
**    Solo grazie a questo `artificio' e` possibile controllare staticamente
**    tutto il programma (infatti se i predicati di cui sopra fallissero
**    l'analisi sarebbe bloccata, oppure inizierebbe il backtracking
**    che darebbe luogo a parecchi messaggi d'errore non significativi).
*/

/*
**    Clausole per la semantica statica delle dichiarazioni.
**    ======================================================
**
**    Definizione dei predicati:
**
**    dFV(D, V)
**       - D e` una dichiarazione;
**       - V e` un insieme di variabili (rappresentato a mezzo lista).
**
**    Vero se V e` l'insieme delle variabili libere della dichiarazione D.
**
**    dDV(D, V)
**       - D e` una dichiarazione;
**       - V e` un insieme di variabili (rappresentato a mezzo lista).
**
**    Vero  se V e` l'insieme delle variabili di definizione di D.
**
**    wfd(A, D)
**       - A e` un ambiente di tipi;
**       - D e` una dichiarazione.
**
**    Sempre vero.
**    Se la dichiarazione D NON e` ben formata nell'ambiente
**    di tipi A asserisce la clausola `scheck(failed)' e fornisce
**    il relativo messaggio d'errore (vedi commento sopra).
**
**    dssem(D, A)
**       - D e` una dichiarazione;
**       - A e` un ambiente di tipi.
**
**    Vero se A e` l'ambiente di tipi generato dalla dichiarazione D.
*/

/*
**    Nil.
**    ----
*/

dFV(nil, []).
dDV(nil, []).
wfd(_, nil).
dssem(nil, []).

/*
**   Dichiarazione semplice (solo linguaggio funzionale).
**   ----------------------------------------------------
*/

dFV(def(_, _, E), V) :-
    eFV(E, V).
dDV(def(X, _, _), [X]).
wfd(A, def(X, T, E)) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, varDecl, X, T, Te])).
dssem(def(X, T, _), [e(X, T)]).

/*
**    Dichiarazione di costante (solo linguaggio imperativo).
**    -------------------------------------------------------
*/

dFV(con(_, _, E), V) :-
    eFV(E, V).
dDV(con(X, _, _), [X]).
wfd(A, con(X, T, E)) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, constDecl, X, T, Te])).
dssem(con(X, T, _), [e(X, T)]).

/*
**    Dichiarazione di variabile ed array (solo linguaggio imperativo).
**    -----------------------------------------------------------------
*/

dFV(var(_, _, E), V) :-
    eFV(E, V).
dDV(var(X, _, _), [X]).
wfd(A, var(X, array(Limits, T), E)) :- /*!,*/
    (limitsOk(Limits) ->
        true
    ;
        error([malformed, arrayDecl, X])),
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, arrayDecl, X, T, Te])).

wfd(A, var(X, T, E)) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, varDecl, X, T, Te])).

dssem(var(X, array(Limits, T), _), [e(X, a(Limits, T))]) :- !.
dssem(var(X, T, _), [e(X, l(T))]).

/*
**    limitsOk(Limits)
**        - Limits e` una lista di limiti di array.
**
**    Vero se, per ogni limite <L, H> in Limits si ha L <= H.
*/

limitsOk([b(L, H)|T]) :- L =< H, limitsOk(T).
limitsOk([]).

/*
**    Dichiarazione di procedura (solo linguaggio imperativo).
**    --------------------------------------------------------
*/

dFV(procdef(_, Formals, C), V) :- 
    cFV(C, V1), formDV(Formals, V2), 
    subtraction(V1, V2, V).
dDV(procdef(P, _, _), [P]).
wfd(A, procdef(P, Formals, C)) :-
    formDV(Formals, V),
    moreThanOnce(V, Duplicates),
    (Duplicates=[] ->
        true
    ;
        error([malformed, procDecl, P, Duplicates])),
    fEnv(Formals, Fenv),
    compose(A, Fenv, G),
    wfc(G, C).

dssem(procdef(P, Formals, _), [e(P, ptype(Formals))]).

/*
**    Composizione sequenziale.
**    -------------------------
*/

dFV(seq(D1, D2), V) :- 
    dFV(D1, FVd1), dDV(D1, DVd1), dFV(D2, FVd2), 
    subtraction(FVd2, DVd1, RFVd2), 
    union(FVd1, RFVd2, V).
dDV(seq(D1, D2), V) :- 
    dDV(D1, DVd1), dDV(D2, DVd2), 
    union(DVd1, DVd2, V).
wfd(A, seq(D1, D2)) :- 
    wfd(A, D1),
    dssem(D1, B), 
    compose(A, B, G),
    wfd(G, D2).
dssem(seq(D1, D2), G) :- 
    dssem(D1, B1),
    dssem(D2, B2),
    compose(B1, B2, G).

/*
**    Composizione parallela.
**    -----------------------
*/

dFV(parallel(D1, D2), V) :-
    dFV(D1, FVd1), dFV(D2, FVd2),
    union(FVd1, FVd2, V).
dDV(parallel(D1, D2), V) :-
    dDV(D1, DVd1), dDV(D2, DVd2),
    union(DVd1, DVd2, V).
wfd(A, parallel(D1, D2)) :-
    wfd(A, D1),
    wfd(A, D2),
    dDV(D1, V1), dDV(D2, V2),
    intersection(V1, V2, V),
    (V=[] ->
        true
    ;
        error([malformed, parallel, V])).

dssem(parallel(D1, D2), G) :-
    dssem(D1, B1),
    dssem(D2, B2),
    compose(B1, B2, G).

/*
**    Composizione privata.
**    ---------------------
*/

dFV(private(D1, D2), V) :- 
    dFV(D1, FVd1), dDV(D1, DVd1), dFV(D2, FVd2),
    subtraction(FVd2, DVd1, RFVd2),
    union(FVd1, RFVd2, V).
dDV(private(_, D2), V) :- 
    dDV(D2, V).
wfd(A, private(D1, D2)) :-
    wfd(A, D1),
    dssem(D1, B),
    compose(A, B, G),
    wfd(G, D2).
dssem(private(_, D2), G) :- 
    dssem(D2, G).

/*
**    Dichiarazione di funzione.
**    --------------------------
*/

dFV(fundef(_, Formals, _, E), V) :- 
    eFV(E, V1), formDV(Formals, V2), 
    subtraction(V1, V2, V).
dDV(fundef(F, _, _, _), [F]).
wfd(A, fundef(F, Formals, T, E)) :-
    formDV(Formals, V),
    moreThanOnce(V, Duplicates),
    (Duplicates=[] ->
        true
    ;
        error([malformed, funDecl, F, Duplicates])),
    fEnv(Formals, Fenv),
    compose(A, Fenv, G),
    essem(G, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, funDecl, F, T, Te])).

dssem(fundef(F, Formals, T, _), [e(F, ftype(Formals, T))]).

/*
**    Dichiarazione ricorsiva.
**    ------------------------
*/

dFV(rec(D), V) :- 
    dFV(D, FVd), dDV(D, DVd),
    subtraction(FVd, DVd, V).
dDV(rec(D), V) :- 
    dDV(D, V).
wfd(A, rec(D)) :- 
    dssem(D, B),
    dFV(D, FVd), dDV(D, DVd),
    intersection(FVd, DVd, V0),
    projection(B, V0, B0), 
    compose(A, B0, G),
    wfd(G, D).

dssem(rec(D), A) :- 
    dssem(D, A).

/*
**    Passaggio dei parametri (solo in sintassi astratta estesa).
**    -----------------------------------------------------------
*/

dFV(parPass(_, Ae), V) :-
    elFV(Ae, V).
dDV(parPass(Form, _), V) :-
    formDV(Form, V).

/*
**    No wfd, dssem: il termine parPass(Form, Ae) non compare
**                   nelle configurazioni iniziali.
*/

/*
**    Ambiente di valori (in sintassi astratta estesa e` una dichiarazioni).
**    ----------------------------------------------------------------------
*/

dFV(E, V) :- 
    isEnv(E),
    envRangeFV(E, V).
dDV(E, V) :- 
    isEnv(E),
    envDomain(E, V).

/*
**    No wfd, dssem: gli ambienti di valori non compaiono
**                   nelle configurazioni iniziali.
*/

/*
**    envRangeFV(E, V)
**        - E e` un ambiente di valori;
**        - V e` un insieme di variabili.
**
**    Vero se V e` l'insieme delle variabili libere del codominio di E.
**    Necessario per le dichiarazioni ricorsive.
*/

envRangeFV([e(_, func(Formals, Body))|E], V) :-
    eFV(Body, FVbody), formDV(Formals, DVformals),
    subtraction(FVbody, DVformals, V1),
    envRangeFV(E, V2), union(V1, V2, V).

envRangeFV([e(_, proc(Formals, Body))|E], V) :-
    cFV(Body, FVbody), formDV(Formals, DVformals),
    subtraction(FVbody, DVformals, V1),
    envRangeFV(E, V2), union(V1, V2, V).

envRangeFV([_|E], V) :-
    envRangeFV(E, V).
envRangeFV([], []).

/*
**    envDomain(E, V)
**        - E e` un ambiente di valori;
**        - V e` un insieme di variabili.
**
**    Vero se V e` il dominio di (l'insieme delle variabili definite in) E.
*/

envDomain([e(X, _)|E], V) :-
    envDomain(E, V0), union([X], V0, V).
envDomain([], []).

/*
**    Parametri formali.
**    ------------------
*/

/*
**    formDV(FormList, V)
**        - FormList e` una lista di parametri formali;
**        - V e` un insieme di variabili (rappresentato a mezzo lista).
**
**    Vero se V e` l'insieme di variabili di definizione di FormList.
*/

formDV([   ival(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([   fval(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([   name(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([    ref(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([  const(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([   copy(X, _)    | Form], [X | V]) :- formDV(Form, V).
formDV([funcpar(N, _, _) | Form], [N | V]) :- formDV(Form, V).
formDV([procpar(N, _)    | Form], [N | V]) :- formDV(Form, V).
formDV([], []).

/*
**    fEnv(FormList, Env)
**        - FormList e` una lista di formali;
**        - Env e` un ambiente di tipi.
**
**    Vero se Env e` l'ambiente di tipi generato dalla
**    lista di formali FormList.
*/

fEnv([      ival(X, T) | Form], [e(X, l(T))        | A]) :- fEnv(Form, A).
fEnv([      fval(X, T) | Form], [e(X, T)           | A]) :- fEnv(Form, A).
fEnv([      name(X, T) | Form], [e(X, T)           | A]) :- fEnv(Form, A).
fEnv([       ref(X, T) | Form], [e(X, l(T))        | A]) :- fEnv(Form, A).
fEnv([     const(X, T) | Form], [e(X, T)           | A]) :- fEnv(Form, A).
fEnv([      copy(X, T) | Form], [e(X, l(T))        | A]) :- fEnv(Form, A).
fEnv([funcpar(N, F, T) | Form], [e(N, ftype(F, T)) | A]) :- fEnv(Form, A).
fEnv([   procpar(N, F) | Form], [e(N, ptype(F))    | A]) :- fEnv(Form, A).
fEnv([], []).

/*
**    Clausole per la semantica statica delle espressioni.
**    ====================================================
**
**    Definizione dei predicati:
**
**    eFV(E, V)
**        - E e` un'espressione;
**        - V e` un insieme di variabili (rappresentato a mezzo lista).
**
**    Vero se V e` l'insieme delle variabili libere dell'espressione E.
**
**    essem(A, E, T)
**      - A e` un ambiente di tipi;
**      - E e` un'espressione;
**      - T e` un tipo.
**
**    Sempre vero.
**    Se, nell'ambiente di tipi A, l'espressione E NON e` ben formata
**    o NON ha tipo T asserisce la clausola `scheck(failed)' e fornisce
**    il relativo messaggio d'errore (vedi commento sopra).
*/

/*
**    Costanti booleane.
**    ------------------
*/

eFV(tt, []).
essem(_, tt, bool).

eFV(ff, []).
essem(_, ff, bool).

/*
**    Identificatori.
**    ---------------
*/

eFV(i(X), [X]).
essem(A, i(X), T) :-
    (getEnv(A, X, W) ->
        ((W=l(T); W=T) ->
            true
        ; 
            error([typeMismatch, id, X, T, W]))
    ;
        error([undeclared, id, X])).

/*
**    Riferimento di array (solo linguaggio imperativo).
**    --------------------------------------------------
*/

eFV(aref(X, El), V) :- 
    elFV(El, FVel),
    union([X], FVel, V).
essem(A, aref(X, El), T) :-
    (getEnv(A, X, W) ->
        (W=a(Limits, T) ->
            true
        ;
            error([typeMismatch, array, X, T, W]))
    ;
        error([undeclared, array, X])),
    elssem(A, El, Tl),
    (tIndexOk(Tl) ->
        true
    ;
        error([typeMismatch, arrayIndexType, X])),
    (nIndexOk(Tl, Limits) ->
        true
    ;
        error([typeMismatch, arrayIndexNumber, X])).

/*
**    nIndexOk(TypeL, Limits)
**        - TypeL  e` una lista di tipi;
**        - Limits e` una lista di limiti di array.
**
**    Vero se TypeL e Limits hanno la stessa lunghezza. Questo predicato
**    e` usato per verificare che gli indici di un riferimento di array
**    siano in numero pari alla dimensione dell'array stesso.
*/

nIndexOk([_|Tl], [_|Limits]) :- 
    nIndexOk(Tl, Limits).
nIndexOk([], []).

/*
**    tIndexOk(TypeL)
**        - TypeL e` una lista di tipi.
**
**    Vero se tutti gli elementi in TypeL valgono int (rappresentazione
**    del tipo integer). Questo predicato e` usato per verificare che
**    le espressioni indice di un riferimento di array siano tutte intere.
*/

tIndexOk([int|Tl]) :- 
    tIndexOk(Tl).
tIndexOk([]).

/*
**    Negazione booleana.
**    -------------------
*/

eFV(not(X), V) :-
    eFV(X, V).
essem(A, not(X), bool) :-
    essem(A, X, T),
    (T=bool ->
        true
    ;
        error([typeMismatch, uop, not, bool, T])).

/*
**    Meno unario.
**    ------------
*/

eFV(uminus(X), V) :-
    eFV(X, V).
essem(A, uminus(X), int) :-
    essem(A, X, T),
    (T=int ->
        true
    ;
        error([typeMismatch, uop, uminus, int, T])).

/*
**    Blocco delle espressioni.
**    -------------------------
*/

eFV(let(D, E), V) :-
    eFV(E, FVe), dDV(D, DVd),
    subtraction(FVe, DVd, V).
essem(A, let(D, E), T) :-
    wfd(A, D),
    dssem(D, B),
    compose(A, B, G),
    essem(G, E, T).

/*
**    Blocco di chiusura delle espressioni.
**    -------------------------------------
*/

eFV(closureE(_, _), []) :- display('!!! eFV(closureE) !!!'), ttynl.
 /* Non ha variabili libere per definizione.*/

/* No essem: closureE non compare nelle configurazioni iniziali. */

/*
**    Espressione condizionale.
**    -------------------------
*/

eFV(if(E, E1, E2), V) :- 
    eFV(E, FVe), eFV(E1, FVe1), eFV(E2, FVe2),
    union(FVe1, FVe2, FVe1e2), 
    union(FVe, FVe1e2, V).
essem(A, if(E, E1, E2), T) :-
    essem(A, E, T0),
    (T0=bool ->
        true
    ;
        error([typeMismatch, if, T0 ])),
    essem(A, E1, T1), essem(A, E2, T2), T=T1,
    (T1=T2 ->
        true
    ;
        error([typeMismatch, if, T1, T2])).

/*
**    Chiamata di funzione.
**    ---------------------
*/

eFV(funcall(F, Ae), V) :-
    elFV(Ae, FVae),
    union([F], FVae, V).

essem(A, funcall(F, Ae), T) :-
    (getEnv(A, F, ftype(Formals, T)) ->
        (aeMatchForm(A, Ae, Formals) ->
            true
        ;
            error([typeMismatch, function, wrongNumberOfActuals, F]))
    ;
        error([undeclared, function, F])).

/*
**    Input.
**    ------
*/

eFV(input(_), []).
essem(_, input(T), T).

/* 
**    Espressioni con side-effects (solo linguaggio imperativo).
**    ----------------------------------------------------------
*/

eFV(side(D, C, E), V) :-
    dDV(D, DVd), dFV(D, FVd),
    cFV(C, FVc), eFV(E, FVe),
    union(FVc, FVe, V1),
    subtraction(V1, DVd, V2),
    union(V2, FVd, V).
essem(A, side(D, C, E), T) :-
    wfd(A, D),
    dssem(D, B),
    compose(A, B, G),
    wfc(G, C),
    essem(G, E, T).

/*
**    Costanti intere.
**    ----------------
*/

eFV(X, []) :- 
    integer(X).
essem(_, X, int) :-
    integer(X).

/*
**    Operatori binari.
**    -----------------
*/

eFV(X, V) :-
    X =.. [O, Arg1, Arg2],
    tbop(O, _, _, _, _),
    eFV(Arg1, V1), eFV(Arg2, V2),
    union(V1, V2, V).
essem(A, X, T) :-
    X =.. [O, Arg1, Arg2],
    tbop(O, T1, T2, T, _),
    essem(A, Arg1, T1a),
    essem(A, Arg2, T2a),
    ((T1=T1a, T2=T2a) ->
        true
    ;
        error([typeMismatch, bop, O, T1a, T2a])).

/*
**    Liste di espressioni.
**    ---------------------
*/

/*
**    elFV(E, V)
**      - E e` una lista di espressioni
**      - V e` l'insieme delle variabili libere
**          nella lista E.
*/

/*
**    elssem(A, El, Tl) 
**      - A e` un ambiente di tipi,
**      - El e` una lista di espressioni,
**      - Tl e` la corrispondente lista dei tipi.
*/

elFV([E | Ae], V) :- 
    eFV(E, FVe), elFV(Ae, FVae),
    union(FVe, FVae, V).
elFV([], []).
elssem(A, [E | Ae], [T | Aet]) :- 
    essem(A, E, T),
    elssem(A, Ae, Aet).
elssem(_, [], []).

/*
**    Clausole di semantica statica per i comandi.
**    ============================================
**
**    Definizione dei predicati:
**    
**    cFV(C, V)
**        - C e` un comando;
**        - V e` un insieme di variabili (rappresentato a mezzo lista).
**
**    Vero se V e` l'insieme delle variabili libere nel comando C.
**
**    wfc(A, C)
**        - A e` un ambiente di tipi;
**        - C e` un comando.
**
**    Sempre vero.
**    Se C NON e` ben formato nell'ambiente di tipi A
**    asserisce la clausola `scheck(failed)' e fornisce
**    il relativo messaggio d'errore (vedi commento sopra).
*/

/*
**    Nop.
**    ----
*/

cFV(nop, []).
wfc(_, nop).

/*
**    Assegnamento.
**    -------------
*/

cFV(ass(X, E), V) :-
    eFV(X, FVx), eFV(E, FVe),
    union(FVx, FVe, V).
wfc(A, ass(aref(X, El), E)) :-
    essem(A, aref(X, El), T),
    essem(A, E, T).
wfc(A, ass(i(X), E)) :-
    essem(A, E, T1),
   (getEnv(A, X, Y) ->
       (Y=l(T) ->
           (T=T1 ->
               true
           ;
               error([typeMismatch, ass, X, T, T1]))
       ; 
           error([nolvalue, ass, X]))
   ;
       error([undeclared, var, X])).

/*
**    Sequenza di comandi.
**    --------------------
*/

cFV(seq(C1, C2), V) :-
    cFV(C1, V1), cFV(C2, V2),
    union(V1, V2, V).
wfc(A, seq(C1, C2)) :-
    wfc(A, C1),
    wfc(A, C2).

/*
**    Comando condizionale if.
**    ------------------------
*/

cFV(if(E, C1, C2), V) :-
    eFV(E, FVe),
    cFV(C1, V1), cFV(C2, V2),
    union(V1, V2, V12),
    union(FVe, V12, V).
wfc(A, if(E, C1, C2)) :-
    essem(A, E, T),
    (T=bool ->
        true
    ;
        error([typeMismatch, cif, T])),
    wfc(A, C1), wfc(A, C2).

/*
**    Comando while.
**    --------------
*/

cFV(while(E, C), V) :-
    eFV(E, FVe), cFV(C, FVc),
    union(FVe, FVc, V).
wfc(A, while(E, C)) :-
    essem(A, E, T),
    (T=bool ->
        true
    ;
        error([typeMismatch, while, T])),
    wfc(A, C).

/*
**    Blocco di comandi.
**    ------------------
*/

cFV(block(D, C), V) :-
    dDV(D, DVd), dFV(D, FVd), cFV(C, FVc),
    subtraction(FVc, DVd, V0),
    union(FVd, V0, V).
wfc(A, block(D, C)) :-
    wfd(A, D),
    dssem(D, B),
    compose(A, B, G),
    wfc(G, C).

/*
**    Blocco di chiusura delle astrazioni procedurali.
**    ------------------------------------------------
*/

cFV(closureC(_, _), []) :- display('!!! cFV(clocureC) !!!'), ttynl.  /* Non ha variabili libere per definizione.*/

/* No wfc: closureC non compare nelle configurazioni iniziali. */

/*
**    Chiamata di procedura.
**    ----------------------
*/

cFV(proccall(P, Ae), V) :-
    elFV(Ae, FVae),
    union([P], FVae, V).
wfc(A, proccall(P, Ae)) :-
    (getEnv(A, P, ptype(Formals)) ->
        (aeMatchForm(A, Ae, Formals) ->
            true
        ;
            error([typeMismatch, procedure, wrongNumberOfActuals, P]))
    ;
        error([undeclared, procedure, P])).

/*
**    Comando di uscita `print'.
**    --------------------------
*/

cFV(print(_, El), V) :-
    elFV(El, V).
wfc(A, print(_, El)) :-
    elssem(A, El, _).

/*
**    aeMatchForm(A, Ae, Form)
**        - A    e` un ambiente di tipi;
**        - Ae   e` una lista di parametri attuali;
**        - Form e` una lista di parametri formali.
**
**    Sempre vero.
**    Se, nell'ambiente di tipi A, la lista di parametri attuali Ae
**    NON e` consistente con la lista di parametri formali Form
**    asserisce la clausola `scheck(failed)' e fornisce
**    il relativo messaggio d'errore (vedi commento sopra).
**
**    N.B. Consistenza di un parametro attuale rispetto ad un formale:
**
**         1) passaggio per valore, nome e costante:
**              a) i tipi devono coincidere;
**         2) passaggio per riferimento e value-result:
**              a) i tipi devono coincidere;
**              b) l'attuale deve avere valore sinistro (variabile, array);
**         3) parametri funzionali:
**              a) i parametri devono essere funzioni (ovviamente);
**              b) i formali dei parametri devono coincidere in numero,
**                 tipo e meccanismo di passaggio;
**         4) parametri procedurali:
**              a) i parametri devono essere procedure (ovviamente);
**              b) i formali dei parametri devono coincidere in numero,
**                 tipo e meccanismo di passaggio.
*/

/*
**    Call by value (linguaggio imperativo).
**    --------------------------------------
*/

aeMatchForm(A, [E | El], [ival(X, T) | Form]) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    aeMatchForm(A, El, Form).

/*
**    Call by value (linguaggio funzionale).
**    --------------------------------------
*/

aeMatchForm(A, [E | El], [fval(X, T) | Form]) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    aeMatchForm(A, El, Form).

/*
**    Call by name.
**    -------------
*/

aeMatchForm(A, [E | El], [name(X, T) | Form]) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    aeMatchForm(A, El, Form).

/*
**    Call by reference (linguaggio imperativo).
**    ------------------------------------------
*/

aeMatchForm(A, [E | El], [ref(X, T) | Form]) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    (((E=i(Y), getEnv(A, Y, l(_))) ; E=aref(_, _)) ->
        true
    ;
        error([nolvalue, callByReference, X])),
    aeMatchForm(A, El, Form).

/*
**    Call by constant (linguaggio imperativo).
**    ----------------------------------------- 
*/

aeMatchForm(A, [E | El], [const(X, T) | Form]) :- 
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    aeMatchForm(A, El, Form).

/*
**    Call by value-result (linguaggio imperativo).
**    ---------------------------------------------
*/

aeMatchForm(A, [E | El], [copy(X, T) | Form]) :-
    essem(A, E, Te),
    (T=Te ->
        true
    ;
        error([typeMismatch, formal, X, T, Te])),
    (((E=i(Y), getEnv(A, Y, l(_))) ; E=aref(_, _)) ->
        true
    ;
        error([nolvalue, callByValueResult, X])),
    aeMatchForm(A, El, Form).

/*
**    Parametri funzionali.
**    ---------------------
*/

aeMatchForm(A, [E | El], [funcpar(N, F, T) | Form]) :-
    (E=i(X) ->
        (getEnv(A, X, De) ->
            (De=ftype(FormE, Te) ->
                ((formMatchForm(F,FormE) ->
                    true
                ;
                    error([formalMismatch, function, N, F, X, FormE]))),
                (T=Te ->
                    true
                ;
                    error([typeMismatch, functionReturn, N, T, X, Te]))
            ;
                error([typeMismatch, notAFunction, id, N, X, De]))
        ;
            error([undeclared, function, X]))
    ;
        error([typeMismatch, notAFunction, expr, N])),
    aeMatchForm(A, El, Form).

/*
**    Parametri procedurali (linguaggio imperativo).
**    ----------------------------------------------
*/

aeMatchForm(A, [E | El], [procpar(N, F) | Form]) :-
    (E=i(X) ->
        (getEnv(A, X, De) ->
            (De=ptype(FormE) ->
                (formMatchForm(F,FormE) ->
                    true
                ;
                    error([formalMismatch, procedure, N, F, X, FormE]))
            ;
                error([typeMismatch, notAProcedure, id, N, X, De]))
        ;
            error([undeclared, procedure, X]))
    ;
        error([typeMismatch, notAProcedure, expr, N])),
    aeMatchForm(A, El, Form).

aeMatchForm(_, [], []).

/*
**    formMatchForm(F1, F2)
**        - F1, F2 sono liste di parametri formali.
**
**    Vero se le liste di formali F1 e F2 coincidono per quanto riguarda,
**    numero, ordine, tipo e meccanismo dei formali contenuti.
*/

formMatchForm([   ival(_, T)    | T1], [   ival(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([   fval(_, T)    | T1], [   fval(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([   name(_, T)    | T1], [   name(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([    ref(_, T)    | T1], [    ref(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([  const(_, T)    | T1], [  const(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([   copy(_, T)    | T1], [   copy(_, T)    | T2]) :-
    formMatchForm(T1, T2).
formMatchForm([funcpar(_, F1, T) | T1], [funcpar(_, F2, T) | T2]) :-
    formMatchForm(F1, F2), formMatchForm(T1, T2).
formMatchForm([procpar(_, F1)    | T1], [procpar(_, F2)    | T2]) :-
    formMatchForm(F1, F2), formMatchForm(T1, T2).
formMatchForm([], []).

/*
**    SLice: modulo `errors'
**
**    Questo modulo contiene i predicati che gestiscono gli errori
**    rilevati in fase di analisi statica dei programmi.
**    Il predicato principale e` error/1 che asserisce la clausola
**    `scheck(failed)' e notifica all'utente l'errore.
**    Ogni ulteriore commento a questo modulo e` superfluo,
**    esiste solo per mantenere il modulo `statsem' piu` pulito.
**
**    Predicati definiti in questo modulo:
**
**    error/1, n/0, notify/1, w/1, wform/1, wfrm/1, wid/1, wlimits/1,
**    wop/1, wstype/1, wtype/1. 
*/


/*
**    error(X)
**        - X e` una lista di atomi che identifica l'errore.
**
**    Asserisce la clausola `scheck(failed)' e notifica l'errore
**    all'utente.
*/

error(X) :-
  setGlobal(scheck(failed)),
  notify(X).

/*
**    Clausole per la generazione dei messaggi d'errore.
**    --------------------------------------------------
*/

/*
**    Abbreviazioni di display/1 e ttynl/0.
*/

w(X) :- display(X).
n :- ttynl.

/*
**    Visualizzazione operatori.
*/

wop(not) :- w('`not''').
wop(uminus) :- w('unary `-''').

wop(eq) :- w('`==''').
wop(ne) :- w('`<>''').
wop(lt) :- w('`<''').
wop(le) :- w('`<=''').
wop(ge) :- w('`>=''').
wop(gt) :- w('`>''').

wop(plus) :- w('`+''').
wop(minus) :- w('`-''').
wop(or) :- w('`or''').

wop(prod) :- w('`*''').
wop(div) :- w('`div''').
wop(mod) :- w('`mod''').
wop(and) :- w('`and''').

/*
**    Visualizzazione identificatori.
*/

wid(X) :- w('`'), w(X), w('''').

/*
**    Visualizzazione tipi.
*/

wstype(int) :- w('integer').
wstype(bool) :- w('boolean').

wtype(X) :- var(X), w('`?''').
wtype(l(T)) :- w('`(loc) '), wstype(T), w(''''). 
wtype(int) :- w('`integer''').
wtype(bool) :- w('`boolean''').
wtype(a(L, T)) :- w('`array ['), wlimits(L), w('] of '),
    wstype(T), w('''').
wtype(ftype(Ft, T)) :- w('`function('), wform(Ft), w(') -> '),
    wstype(T), w('''').
wtype(ptype(Ft)) :- w('`procedure('), wform(Ft), w(')''').

wlimits([b(L, H)]) :- w(L), w('..'), w(H).
wlimits([First|Rest]) :- wlimits([First]), w(','), wlimits(Rest).
wlimits([]).

/*
**    Visualizzazione parametri formali.
*/

wform([F]) :- wfrm(F).
wform([First|Rest]) :- wform([First]), w(','), wform(Rest).
wform([]).

wfrm(ival(_, T)) :- wstype(T).
wfrm(fval(_, T)) :- wstype(T).
wfrm(name(_, T)) :- w('(name) '), wstype(T).
wfrm(ref(_, T)) :- w('(ref) '), wstype(T).
wfrm(const(_, T)) :- w('(const) '), wstype(T).
wfrm(copy(_, T)) :- w('(copy) '), wstype(T).
wfrm(funcpar(_, F, T)) :-
    w('function('), wform(F), w('):'), wstype(T).
wfrm(procpar(_, F)) :-
    w('procedure('), wform(F), w(')').

/*
**    Notifica dei messaggi d'errore.
*/

notify([nolvalue, ass, Identifier]) :-
    w('No L-value: cannot assign to '), wid(Identifier), n.

notify([nolvalue, callByReference, X]) :-
    w('No L-value: formal '), wid(X),
    w(' must be passed by reference'), n.

notify([nolvalue, callByValueResult, X]) :-
    w('No L-value: formal '), wid(X),
    w(' must be passed by value-result'), n.

notify([undeclared, id, Identifier]) :-
    w('Undeclared identifier: '), wid(Identifier), n.

notify([undeclared, var, Identifier]) :-
    w('Undeclared variable: '), wid(Identifier), n.

notify([undeclared, array, Array]) :-
    w('Undeclared array: '), wid(Array), n.

notify([undeclared, function, Func]) :-
    w('Undeclared function: '), wid(Func), n.

notify([undeclared, procedure, Proc]) :-
    w('Undeclared procedure: '), wid(Proc), n.

notify([typeMismatch, arrayIndexType, Array]) :-
    w('Type mismatch: bad indexes for array '), wid(Array),
    w(', must all be `integer'''), n.

notify([typeMismatch, arrayIndexNumber, Array]) :-
    w('Type mismatch: wrong number of indexes for array '),
    wid(Array), n.

notify([typeMismatch, while, Type]) :-
    w('Type mismatch: `while E do C'', E must be `boolean'''), n,
    w('               '), wtype(Type), w(' is incorrect'), n.

notify([typeMismatch, cif, Type]) :-
    w('Type mismatch: `if E then C1 else C2'', E must be `boolean'''), n,
    w('               '), wtype(Type), w(' is incorrect'), n.

notify([typeMismatch, ass, Identifier, EType, RType]) :-
    w('Type mismatch: a value of type '), wtype(RType),
    w(' cannot be assigned to'), n,
    w('               variable '), wid(Identifier),
    w(' ('), wtype(EType), w(')'), n.

notify([typeMismatch, formal, Identifier, EType, RType]) :-
    w('Type mismatch: a value of type '), wtype(RType),
    w(' cannot be passed,'), n,
    w('               being formal '), wid(Identifier),
    w(' of type '), wtype(EType), n.

notify([typeMismatch, function, wrongNumberOfActuals, F]) :-
    w('Type mismatch: wrong number of actual parameters'), n,
    w('               for function '), wid(F), n.

notify([typeMismatch, procedure, wrongNumberOfActuals, P]) :-
    w('Type mismatch: wrong number of actual parameters'), n,
    w('               for procedure '), wid(P), n.

notify([typeMismatch, funDecl, F, FType, RType]) :-
    w('Type mismatch: function '), wid(F),
    w(' must return '), wtype(FType), n,
    w('               body expression type cannot be '),
    wtype(RType), n.

notify([typeMismatch, if, Type]) :-
    w('Type mismatch: `if E then E1 else E2'', E must be `boolean'''), n,
    w('               '), wtype(Type), w(' is incorrect'), n.

notify([typeMismatch, if, T1, T2]) :-
    w('Type mismatch: `if E then E1 else E2'', E1 and E2 not of the'),
    w(' same type,'), n,
    w('               cannot mix '), wtype(T1), w(' and '),
    wtype(T2), n.

notify([typeMismatch, id, Identifier, EType, RType]) :-
    w('Type mismatch: '), wid(Identifier),
    w(' is '), wtype(RType), n,
    w('               where needed to be '),
    wtype(EType), n.

notify([formalMismatch, function, FormN, FormF, ActN, ActF]) :-
    w('Functional parameter mismatch: formal parameters must be'), n,
    w('  equal in number, type and mechanism. Formal and actual are:'), n,
    w('    function '), wid(FormN), w(' takes '), wform(FormF), n,
    w('    function '), wid(ActN),  w(' takes '), wform(ActF), n.

notify([formalMismatch, procedure, FormN, FormF, ActN, ActF]) :-
    w('Procedural parameter mismatch: formal parameters must be'), n,
    w('  equal in number, type and mechanism. Formal and actual are:'), n,
    w('    procedure '), wid(FormN), w(' takes '), wform(FormF), n,
    w('    procedure '), wid(ActN),  w(' takes '), wform(ActF), n.

notify([typeMismatch, notAFunction, expr, N]) :-
    w('Type mismatch: formal '), wid(N),
    w(' is a function, cannot pass anything else'), n.

notify([typeMismatch, notAProcedure, expr, N]) :-
    w('Type mismatch: formal '), wid(N),
    w(' is a procedure, cannot pass anything else'), n.

notify([typeMismatch, notAFunction, id, N, X, Dx]) :-
    w('Type mismatch: formal '), wid(N),
    w(' is a function, cannot pass'), n,
    w('               '), wid(X), w(' ('), wtype(Dx), w(')'), n.

notify([typeMismatch, notAProcedure, id, N, X, Dx]) :-
    w('Type mismatch: formal '), wid(N),
    w(' is a procedure, cannot pass'), n,
    w('               '), wid(X), w(' ('), wtype(Dx), w(')'), n.

notify([typeMismatch, functionReturn, N, EType, R, RType]) :-
    w('Type mismatch: formal '), wid(N),
    w(' is function returning '), wtype(EType), n,
    w('               cannot pass '), wid(R), w(' (returns '),
    wstype(RType), w(')'), n.
 
notify([typeMismatch, array, Array, BType, RType]) :-
    w('Type mismatch: '), wid(Array),
    w(' is '), wtype(RType), w(' where needed to be'), n,
    w('               array [...] of '), wtype(BType), n.

notify([typeMismatch, uop, Uop, EType, RType]) :-
    w('Type mismatch: '), wop(Uop), w(' applied to '),
    wtype(RType), w(' instead of '), wtype(EType), n.

notify([typeMismatch, bop, Bop, TArg1, TArg2]) :-
    w('Type mismatch: '), wop(Bop), w(' applied to '),
    wtype(TArg1), w(' and '), wtype(TArg2), n.

notify([typeMismatch, varDecl, Identifier, EType, RType]) :-
    w('Type mismatch: variable '), wid(Identifier),
    w(' defined '), wtype(EType), w(' and initialized with '),
    wtype(RType), n.
 
notify([typeMismatch, constDecl, Identifier, EType, RType]) :-
    w('Type mismatch: constant '), wid(Identifier),
    w(' defined '), wtype(EType), w(' and initialized with '),
    wtype(RType), n.

notify([typeMismatch, arrayDecl, Identifier, EType, RType]) :-
    w('Type mismatch: array '), wid(Identifier),
    w(' defined [...] of '), wtype(EType), n,
    w('               and initialized with '),
    wtype(RType), n.

notify([malformed, arrayDecl, Identifier]) :-
    w('Malformed declaration: array '), wid(Identifier),
    w(', `L..H'' requires L <= H'), n.

notify([malformed, procDecl, Proc, Duplicates]) :-
    w('Malformed declaration: procedure '), wid(Proc),
    w(', duplicated formal'), n,
    w('                       parameters '), w(Duplicates), n.

notify([malformed, funDecl, Fun, Duplicates]) :-
    w('Malformed declaration: function '), wid(Fun),
    w(', duplicated formal'), n,
    w('                       parameters '), w(Duplicates), n.

notify([malformed, parallel, Duplicates]) :-
    w('Malformed declaration: composite declaration `D1 | D2'' '),
    w('requires D1 and D2'), n,
    w('                       to be disjoint, duplicate variables are'),
    n,
    w('                       '), w(Duplicates), n.

/*
**    SLice: modulo `common'
**
**    Questo modulo contiene predicati che sono necessari sia per
**    i controlli di semantica statica di SLice, che per l'interpretazione
**    dei linguaggi funzionale e imperativo.
**
**    Predicati definiti in questo modulo:
**
**    compose/3, getEnv/3, isConst/1, isEnv/1, isLoc/1, isTerminalAe/2,
**    mask/3, projection/3, tbop/5. 
*/

/*
**    isConst(X)
**
**    Vero se X e` tt, ff o intero.
*/

isConst(tt).
isConst(ff).
isConst(X) :- integer(X).

/*
**    isEnv(X)
**        - ha successo se X e` un'ambiente.
**
**    N.B. Gli ambienti sono rappresentati da liste di atomi della forma
**         e(S, D), dove S e` un identificatore e D e` il valore/tipo
**         denotabile associato. Qui e nel seguito la proposizione
**         "E e` un ambiente" va interpretata come
**         "E e` una lista che rappresenta un ambiente".
*/

isEnv([_|_]).
isEnv([]).

/*
**    getEnv(E, X, Y)
**        - E e` un ambiente
**        - X e` un identificatore
**        - Y e` il valore/tipo denotabile associato
**
**    Ha successo se nell'ambiente E l'identificatore X e` associato ad Y.
*/

getEnv([e(X, Z)|_], X, Y) :- !, Y=Z.
getEnv([_|T], X, Y) :-
    getEnv(T, X, Y).

/*
**    compose(A, B, C)
**        - A, B, C, sono ambienti
**
**    Realizza la composizione di ambienti:
**    compose(A, B, C) ha successo <=> A[B] = C. Essendo gli ambienti
**    realizzati come liste compose/3 non e` altro che il classico
**    append/3 con i primi due argomenti scambiati.
*/

compose(E, [], E).
compose(E1, [B|E2], [B|E3]) :-
    compose(E1, E2, E3).

/*
**    projection(E, V, A)
**       - E e` un ambiente;
**       - V e` un insieme di identificatori;
**       - A e` un ambiente.
**
**    Vero se A e` composto da tutti e soli gli elementi
**    di E che definiscono identificatori appartenenti a V.
*/
 
projection([e(X, Y) | E1], V, [e(X, Y) | E2]) :-
    member(X, V), ! ,
    projection(E1, V, E2).
projection([_|T], V, E) :-
    projection(T, V, E).
projection([], _, []).

/*
**    mask(E, V, A)
**       - E e` un ambiente;
**       - V e` un insieme di variabili;
**       - A e` un ambiente.
**
**    Vero se l'ambiente A e` composto da tutti e soli gli elementi
**    di E che definiscono identificatori NON appartenenti a V.
*/

mask([e(X, _) | E1], V, E2) :-
    member(X, V), ! ,
    mask(E1, V, E2).
mask([e(X, Y) | E1], V, [e(X, Y) | E2]) :-
    mask(E1, V, E2).
mask([], _, []).

/*
**    Definizione degli operatori binari.
**    -----------------------------------
**
**    tbop(N, T1, T2, T, O)
**       - N  e` il nome in sintassi astratta dell'operatore O;
**       - T1 e` il tipo del primo argomento;
**       - T2 e` il tipo del secondo argomento;
**       - T  e` il tipo del risultato dell`operazione;
**       - O  e` l'operatore semantico associato.
**
**    Vero se N e` un operatore binario con dominio di tipo T1xT2 e
**    codominio T cui corrisponde l'operazione semantica O.
*/

/* Operatori aritmetici. */

tbop(eq, int, int, bool, =:=).       /* =:= operatore builtin */
tbop(ne, int, int, bool, =\=).       /* =\= operatore builtin */
tbop(lt, int, int, bool, <).         /* <   operatore builtin */
tbop(le, int, int, bool, =<).        /* =<  operatore builtin */
tbop(ge, int, int, bool, >=).        /* >=  operatore builtin */
tbop(gt, int, int, bool, >).         /* >   operatore builtin */

tbop(plus, int, int, int, +).        /* +   operatore builtin */
tbop(minus, int, int, int, -).       /* -   operatore builtin */
tbop(or, bool, bool, bool, semOr).   /* semOr/3  definito in runtime */

tbop(prod, int, int, int, *).        /* *   operatore builtin */
tbop(div, int, int, int, //).        /* //  operatore builtin */
tbop(mod, int, int, int, mod).       /* mod operatore builtin */
tbop(and, bool, bool, bool, semAnd). /* semAnd/3 definito in runtime */

/*
**    isTerminalAe(Fl, El)
**        - Fl e` una lista di formali;
**        - El e` una lista di espressioni attuali.
**
**    Vero se El e` una configurazione terminale di espressione
**    attuale data la lista di formali Fl. Questo predicato
**    viene utilizzato per controllare la valutazione
**    delle espressioni attuali, sia del linguaggio imperativo,
**    che di quello funzionale.
*/

isTerminalAe([ival(_,_)|Form], [C|Ae]) :-
    isConst(C),
    isTerminalAe(Form, Ae).
isTerminalAe([fval(_,_)|Form], [C|Ae]) :-
    isConst(C),
    isTerminalAe(Form, Ae).
isTerminalAe([name(_,_)|Form], [E|Ae]) :-
    eFV(E,[]),
    isTerminalAe(Form, Ae).
isTerminalAe([ref(_,_)|Form], [C|Ae]) :- 
    (isLoc(C) ; C=a(_,_)),
    isTerminalAe(Form, Ae).
isTerminalAe([const(_,_)|Form], [C|Ae]) :-
    isConst(C),
    isTerminalAe(Form, Ae).
isTerminalAe([copy(_,_)|Form], [lr(_,_)|Ae]) :-
    isTerminalAe(Form, Ae).
isTerminalAe([funcpar(_,_,_)|Form], [func(_, _)|Ae]) :-
    isTerminalAe(Form, Ae).
isTerminalAe([procpar(_,_)|Form], [proc(_, _)|Ae]) :-
    isTerminalAe(Form, Ae).
isTerminalAe([], []).

isLoc(L) :-
    integer(L).

/*
**    SLice: modulo `fdinsem'
**
**    Questo modulo contiene i predicati che implementano
**    l'interprete del linguaggio funzionale di SLice.
**
**    Predicati definiti in questo modulo:
**
**    ae1t/4, d1t/3, e1t/3, expTrace/3, traceDependentClause/2. 
*/

/*
**    Clausole per la semantica dinamica per le dichiarazioni.
**    ========================================================
**
**    d1t(E, D, D1)
**        - E     e` un ambiente di valori;
**        - D, D1 sono dichiarazioni.
**
**    Vero se nell'ambiente E la configurazione di dichiarazione
**    D transisce in D1 in un certo numero di passi.
**
**    N.B. Con trace attivato la transizione D -> D1
**         e` in un passo, altrimenti in tanti passi quanti ne
**         occorrono per far si` che D1 sia un ambiente
**         (configurazione terminale per le dichiarazioni).
*/

/*
**    Nil.
**    ----
*/

d1t(_, nil, []).

/*
**    Definizione semplice.
**    ---------------------
*/

d1t(_, def(X,_,Y), [e(X, Y)]) :-
    isConst(Y).
d1t(E, def(X,T,Y), def(X,T,Y1)) :-
    expStar(E, Y, Y1).

/*
**    Composizione sequenziale.
**    -------------------------
*/

d1t(_, seq(X, Y), Z) :-
    isEnv(X), isEnv(Y),
    compose(X, Y, Z).
d1t(E, seq(X, Y), seq(X, Y1)) :-
    isEnv(X),
    compose(E, X, E1),
    declStar(E1, Y, Y1).
d1t(E, seq(X, Y), seq(X1, Y)) :-
    declStar(E, X, X1).

/* 
**    Composizione privata.
**    ---------------------
*/

d1t(_, private(X, Y), Y) :-
    isEnv(X), isEnv(Y).
d1t(E, private(X, Y), private(X, Y1)) :-
    isEnv(X),
    compose(E, X, E1),
    declStar(E1, Y, Y1).
d1t(E, private(X, Y), private(X1, Y)) :-
    declStar(E, X, X1).

/*
**    Composizione parallela.
**    -----------------------
*/

d1t(_, parallel(X, Y), Z) :-
    isEnv(X), isEnv(Y),
    compose(X, Y, Z).
d1t(E, parallel(X, Y), parallel(X, Y1)) :-
    isEnv(X),
    declStar(E, Y, Y1).
d1t(E, parallel(X, Y), parallel(X1, Y)) :-
    declStar(E, X, X1).

/*
**    Definizione di funzione.
**    ------------------------
*/

d1t(E, fundef(F, Formals, _, Body), [e(F,func(Formals, NewBody))]) :-
    eFV(Body, FVbody), formDV(Formals, DVformals),
    subtraction(FVbody, DVformals, V),
    projection(E, V, E1),
    (E1 = [] -> NewBody = Body ; NewBody = let(E1, Body)).

/*
**    Definizione ricorsiva.
**    ----------------------
*/

d1t(_, rec(E0), E1) :-
    isEnv(E0),
    makeRecEnv(E0, E0, E1).
d1t(E, rec(D), rec(D1)) :-
    dDV(D, DVd), dFV(D, FVd),
    intersection(DVd, FVd, V0),
    mask(E, V0, E0),
    declStar(E0, D, D1).

/*
**    Passaggio dei parametri.
**    ------------------------
*/

d1t(_, parPass(Formals, Ae), R) :-
    isTerminalAe(Formals, Ae), 
    makeEnv(Formals, Ae, R).
d1t(E, parPass(Formals, Ae), parPass(Formals, Ae1)) :- 
    ae1t(E, Formals, Ae, Ae1).

/*
**    Clausole di semantica dinamica per le espressioni.
**    ==================================================
**
**    e1t(A, E, E1)
**        - A     e` un ambiente di valori;
**        - E, E1 sono espressioni.
**
**    Vero se nell'ambiente A la configurazione di espressione
**    E transisce in E1 in un certo numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

/*
**    Negazione booleana.
**    -------------------
*/

e1t(_, not(tt), ff).
e1t(_, not(ff), tt).
e1t(E, not(X), not(X1)) :-
    expStar(E, X, X1).

/*
**    Meno unario.
**    ------------
*/

e1t(_, uminus(X), Y) :-
    integer(X),
    Y is -X.
e1t(E, uminus(X), uminus(X1)) :-
    expStar(E, X, X1).

/*
**    Identificatore.
**    ---------------
*/

e1t(E, i(X), Y) :-
    getEnv(E, X, Y).

/*
**    Operatori binari.
**    -----------------
**
**    Le clausole che seguono, apparentemente complicate, hanno lo
**    scopo, insieme a quelle in `statsem' e `common', di evitare
**    la proliferazione di clausole per il trattamento degli operatori
**    binari. In tal modo, inoltre, l'introduzione di nuovo operatori
**    e` estremamente agevole, richiedendo pochissime modifiche
**    (basta aggiungere clausole per il predicato tbop/5 (`common')
**    ed eventualmente la definizione dell'operatore semantico
**    in `runtime'.
*/

e1t(_, X, Y) :-
    X =.. [O, Arg1, Arg2],
    tbop(O, D, D, R, F),
    isConst(Arg1), isConst(Arg2),
    (D = int ->                   /* ==> op aritmetico o relazionale */
        (Exp =.. [F, Arg1, Arg2],
        (R = int ->               /* ==> op aritmetico */
            (Eval =.. [is, Y, Exp], call(Eval))
        ;                         /* ==> op relazionale */
            (call(Exp) -> Y = tt ; Y = ff)))
    ;                             /* ==> op booleano */
        (Eval =.. [F, Arg1, Arg2, Y], call(Eval))).

e1t(E, X, Y) :-
    X =.. [O, A, B],
    tbop(O, _, _, _, _),
    isConst(A),
    expStar(E, B, B1),
    Y =.. [O, A, B1].

e1t(E, X, Y) :-
    X =.. [O, A, B],
    tbop(O, _, _, _, _),
    expStar(E, A, A1),
    Y =.. [O, A1, B].

/*
**    Espressione condizionale.
**    -------------------------
*/

e1t(_, if(tt, Y, _), Y).
e1t(_, if(ff, _, Z), Z).
e1t(E, if(X, Y, Z), if(X1, Y, Z)) :-
    expStar(E, X, X1).

/*
**    Blocco delle espressioni.
**    -------------------------
*/

e1t(_, let(X, Y), Y) :-
    isEnv(X),
    isConst(Y).
e1t(E, let(X, Y), let(X, Y1)) :-
    isEnv(X),
    compose(E, X, E1),
    expStar(E1, Y, Y1).
e1t(E, let(X, Y), let(X1, Y)) :-
    declStar(E, X, X1).

/*
**    Chiamata di funzione.
**    ---------------------
*/

e1t(E, funcall(F, Ae), let(parPass(Formals,Ae), Body)) :-
    getEnv(E, F, func(Formals, Body)).

/*
**    Input.
**    ------
*/

e1t(_, input(X), C) :-
    doInput(X, C).

/*
**    Clausole di semantica dinamica delle espressioni attuali.
**    =========================================================
**
**    ae1t(E, Fl, El, El1)
**        - E       e` un ambiente di valori;
**        - Fl      e` una lista di parametri formali;
**        - El, El1 sono espressioni attuali.
**
**    Vero se nell'ambiente E con la lista di formali Fl
**    la configurazione di espressione attuale El transisce
**    in El1 in un certo numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

/*
**    Call by value.
**    --------------
*/

ae1t(E, [fval(_,_) | Form], [C | Ae], [C | Ae1]) :-
    isConst(C), 
    ae1t(E, Form, Ae, Ae1).
ae1t(E, [fval(_,_) | _], [C | Ae], [C1 | Ae]) :-
    expStar(E, C, C1).

/*
**    Call by name.
**    -------------
*/

ae1t(E ,[name(_,_) | Form], [Exp | Ae], [NewExp | Ae1]) :-
    eFV(Exp, V),
    (V=[] ->
        NewExp=Exp
    ;
        (projection(E, V, E0), NewExp=let(E0,Exp))),
    ae1t(E, Form, Ae, Ae1).

/*
**    Parametri funzionali.
**    ---------------------
*/

ae1t(E, [funcpar(_,_,_)|Form], [func(F, B)|Ae], [func(F, B)|Ae1]) :-
    ae1t(E, Form, Ae, Ae1).
ae1t(E, [funcpar(_,_,_)|Form], [i(FuncId)|Ae], [func(F, B)|Ae1]) :-
    getEnv(E, FuncId, func(F, B)),
    ae1t(E, Form, Ae, Ae1).

ae1t(_, [], [], []).

/*
**    expTrace(A, E, Ef)
**        - A     e` un ambiente di valori;
**        - E, Ef sono espressioni.
**
**    Vero se nell'ambiente A la configurazione di espressione
**    E transisce nella configurazione (terminale) Ef (costante).
**    Come side-effect provoca il tracciamento della valutazione
**    dell'espressione.
*/

expTrace(_, X, X) :-
    isConst(X), !,
    outputConfig(X),
    (\+ traceFile(user) ->
       (display(X), ttynl)     /* Visualizzazione del risultato */
    ;
       true).

expTrace(E, X, Y) :-
    outputConfig(X),
    e1t(E, X, X1), !, expTrace(E, X1, Y).

/*
**    Clausole di semantica dinamica dipendenti dallo stato del tracer.
**    =================================================================
*/

/*
**    expStar(A, E, E1)
**        - A     e` un ambiente di valori;
**        - E, E1 sono espressioni.
**
**    Vero se nell'ambiente A la configurazione di espressione
**    E transisce nella configurazione E1.
**    Se il tracer e` attivo expStar/3 e` equivalente a e1t/3,
**    altrimenti expStar/3 invoca ricorsivamente se stesso
**    finche` E1 non e` una costante.
*/

traceDependentClause(on, (
    expStar(E, X, Y) :- e1t(E, X, Y), !
)).

traceDependentClause(off, (
    expStar(_, X, X) :- isConst(X), !
)).

traceDependentClause(off, (
    expStar(E, X, Y) :-
        e1t(E, X, X1), !,
        expStar(E, X1, Y)
)).

/*
**    declStar(E, D, D1)
**        - E     e` un ambiente di valori;
**        - D, D1 sono dichiarazioni.
**
**    Vero se nell'ambiente E la configurazione di dichiarazione
**    D transisce nella configurazione D1.
**    Se il tracer e` attivo declStar/3 e` equivalente a d1t/3,
**    altrimenti declStar/3 invoca ricorsivamente se stesso
**    finche` D1 non e` un ambiente.
*/

traceDependentClause(on, (
    declStar(E, X, Y) :- d1t(E, X, Y), !
)).

traceDependentClause(off, (
    declStar(_, X, X) :- isEnv(X), !
)).

traceDependentClause(off, (
    declStar(E, X, Y) :-
        d1t(E, X, X1), !,
        declStar(E, X1, Y)
)).

/*
**    topDynamicFunctional(E)
**        - E e` un'espressione.
**
**    Vero se esiste una sequenza (finita) di transizioni che,
**    nell'ambiente vuoto, porta la configurazione di espressione
**    E in una configurazione terminale K (dove K e` una costante).
**
*/

traceDependentClause(off, (
    topDynamicFunctional(X) :-
        expStar([], X, Y), display(Y), ttynl
)).

traceDependentClause(on, (
    topDynamicFunctional(X) :-
        expTrace([], X, _)
)).

/*
**    SLice: modulo `idinsem'
**
**    Questo modulo contiene i predicati che implementano
**    l'interprete del linguaggio imperativo di SLice.
**
**    Predicati definiti in questo modulo:
**
**    ae1t/6, c1t/5, comTrace/5, d1t/5, e1t/5, el1t/5,
**    isConstList/1, traceDependentClause/2.
*/

/*
**    Clausole per la semantica dinamica delle dichiarazioni.
**    =======================================================
**
**    d1t(E, D, S, D1, S1)
**        - E     e` un ambiente di valori;
**        - D, D1 sono dichiarazioni;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente E la configurazione di dichiarazione
**    <D, S> transisce in <D1, S1> in un certo numero di passi.
**
**    N.B. Con trace attivato la transizione <D, S> -> <D1, S1>
**         e` in un passo, altrimenti in tanti passi quanti ne
**         occorrono per far si` che D1 sia un'ambiente
**         (configurazione terminale per le dichiarazioni).
*/

/*
**    Nil.
**    ----
*/

d1t(_, nil, S, [], S).

/*    
**    Dichiarazione di costante.
**    --------------------------
*/

d1t(_, con(X,_,Y), S, [e(X, Y)], S) :-
    isConst(Y).
d1t(E, con(X,T,Y), S, con(X,T,Y1), S1) :-
    expStar(E, Y, S, Y1, S1).

/*    
**    Dichiarazione di variabile (array compreso).
**    --------------------------------------------
*/

d1t(_, var(X, array(Limits, _), Y), S, [e(X, a(Limits, Base))], S1) :-
    isConst(Y),
    makeArray(Limits, Base, Y, S2),
    append(S, S2, S1).
d1t(E, var(X, array(Limits, T), Y), S, var(X, array(Limits, T), Y1), S1) :-
    expStar(E, Y, S, Y1, S1).

d1t(_, var(X,_,Y), S, [e(X, l(L))], [s(L, Y)|S]) :-
    isConst(Y),
    getNewLoc(L).
d1t(E, var(X,T,Y), S, var(X,T,Y1), S1) :-
    expStar(E, Y, S, Y1, S1).

/*
**    Composizione sequenziale.
**    -------------------------
*/

d1t(_, seq(X, Y), S, Z, S) :-
    isEnv(X), isEnv(Y),
    compose(X, Y, Z).
d1t(E, seq(X, Y), S, seq(X, Y1), S1) :-
    isEnv(X),
    compose(E, X, E1),
    declStar(E1, Y, S, Y1, S1).
d1t(E, seq(X, Y), S, seq(X1, Y), S1) :-
    declStar(E, X, S, X1, S1).

/*    
**    Composizione privata.
**    ---------------------
*/

d1t(_, private(X, Y), S, Y, S) :-
    isEnv(X),
    isEnv(Y).
d1t(E, private(X, Y), S, private(X, Y1), S1) :-
    isEnv(X),
    compose(E, X, E1),
    declStar(E1, Y, S, Y1, S1).
d1t(E, private(X, Y), S, private(X1, Y), S1) :-
    declStar(E, X, S, X1, S1).

/*    
**    Composizione parallela.
**    -----------------------
*/

d1t(_, parallel(X, Y), S, Z, S) :-
    isEnv(X), isEnv(Y),
    compose(X, Y, Z).
d1t(E, parallel(X, Y), S, parallel(X, Y1), S1) :-
    isEnv(X),
    declStar(E, Y, S, Y1, S1).
d1t(E, parallel(X, Y), S, parallel(X1, Y), S1) :-
    declStar(E, X, S, X1, S1).

/*
**    Dichiarazione di procedura.
**    ---------------------------
*/

d1t(E, procdef(P, Form, Body), S, [e(P, proc(Form, NewBody))], S) :-
    cFV(Body, FVbody), formDV(Form, DVform),
    subtraction(FVbody, DVform, V),
    projection(E, V, E1),
    (E1 = [] ->
        NewBody = Body
    ;
        NewBody = closureC(E1, Body)).

/*
**    Dichiarazione di funzione.
**    --------------------------
*/

d1t(E, fundef(F, Form, _, Body), S, [e(F, func(Form, NewBody))], S) :-
    eFV(Body, FVbody), formDV(Form, DVform),
    subtraction(FVbody, DVform, V),
    projection(E, V, E1),
    (E1 = [] ->
        NewBody = Body
    ;
        NewBody = closureE(E1, Body)).

/*
**    Passaggio dei parametri.
**    ------------------------
*/

d1t(_, parPass(Formals, Ae), S, Env, S1) :-
    isTerminalAe(Formals, Ae),
    makeEnvStore(Formals, Ae, Env, ParamStore),
    append(ParamStore, S, S1).

d1t(E, parPass(Formals, Ae), S, parPass(Formals, Ae1), S1) :-
    ae1t(E, Formals, Ae, S, Ae1, S1).

/*
**    Dichiarazione ricorsiva.
**    ------------------------
*/

d1t(_, rec(E0), S, E1, S) :-
    isEnv(E0),
    makeRecEnv(E0, E0, E1).
d1t(E, rec(D), S, rec(D1), S1) :-
    dDV(D, DVd), dFV(D, FVd),
    intersection(DVd, FVd, V0),
    mask(E, V0, E0),
    declStar(E0, D, S, D1, S1).

/*
**    Clausole per la semantica dinamica dei comandi.
**    ===============================================
**
**    c1t(E, C, S, C1, S1)
**        - E     e` un ambiente di valori;
**        - C, C1 sono comandi;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente E la configurazione di comando
**    <C, S> transisce in <C1, S1> in un certo numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

/*
**    Nop.
**    ----
*/

c1t(_, nop, S, halt, S).

/*
**    Assegnamento.
**    -------------
*/

c1t(E, ass(aref(X, El), Y), S, halt, S1) :-
    isConstList(El), isConst(Y),
    getEnv(E, X, a(Limits, Base)),
    arrayIndex(Limits, El, Offset),
    Loc is Base+Offset,
    changeStore(S, Loc, Y, S1).

c1t(E, ass(aref(X, El), Y), S, ass(aref(X, El), C), S1) :-
    isConstList(El),
    expStar(E, Y, S, C, S1).

c1t(E, ass(aref(X, El), Y), S, ass(aref(X, El1), Y), S1) :-
    el1t(E, El, S, El1, S1).

c1t(E, ass(i(X), Y), S, halt, S1) :-
    isConst(Y),
    getEnv(E, X, H),
    (H=l(L) ; H=r(L) ; H=c(_, L)),
    changeStore(S, L, Y, S1).

c1t(E, ass(i(X), Y), S, ass(i(X), C), S1) :-
    expStar(E, Y, S, C, S1).

/*
**    Sequenza di comandi.
**    --------------------
*/

c1t(_, seq(halt, C2), S, C2, S).
c1t(E, seq(C1, C2), S, seq(C11, C2), S1) :-
    comStar(E, C1, S, C11, S1).

/*
**    Comando condizionale if.
**    ------------------------
*/

c1t(_, if(tt, C1, _), S, C1, S).
c1t(_, if(ff, _, C2), S, C2, S).
c1t(E, if(Exp, C1, C2), S, if(B, C1, C2), S1) :-
    expStar(E, Exp, S, B, S1).

/*
**    Comando while.
**    --------------
*/

c1t(_, while(Exp, C), S, if(Exp, seq(C, while(Exp, C)), halt), S).

/*
**    Blocco di comandi.
**    ------------------
*/

c1t(_, block(D, halt), S, halt, S1) :-
    dispose(D, S, S1).
c1t(E, block(D, C), S, block(D, C1), S1) :-
    isEnv(D),
    compose(E, D, G),
    comStar(G, C, S, C1, S1).
c1t(E, block(D, C), S, block(D1, C), S1) :-
    declStar(E, D, S, D1, S1).

/*
**    Blocco per la chiusura delle astrazioni procedurali.
**    ----------------------------------------------------
**
**    E` del tutto simile al normale blocco con le seguenti differenze:
**
**        1) riceve sempre un ambiente;
**        2) non effettua il dispose delle variabili.
*/

c1t(_, closureC(_, halt), S, halt, S).
c1t(E, closureC(D, C), S, closureC(D, C1), S1) :-
    compose(E, D, G),
    comStar(G, C, S, C1, S1).

/*
**    Comando di output `print'.
**    --------------------------
*/

c1t(_, print(Format, El), S, halt, S) :-
    isConstList(El),
    doPrint(Format, El).

c1t(E, print(Format, El), S, print(Format, El1), S1) :-
    el1t(E, El, S, El1, S1).

/*
**    Chiamata di procedura.
**    ----------------------
*/

c1t(E, proccall(P, Ae), S, block(parPass(Formals, Ae), Body), S) :-
    getEnv(E, P, proc(Formals, Body)).

/*
**    Clausole di semantica dinamica delle espressioni.
**    =================================================
**
**    e1t(A, E, S, E1, S1)
**        - A     e` un ambiente di valori;
**        - E, E1 sono espressioni;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente A la configurazione di espressione
**    <E, S> transisce in <E1, S1> in un certo numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

/*
**    Negazione booleana.
**    -------------------
*/

e1t(_, not(tt), S, ff, S).
e1t(_, not(ff), S, tt, S).
e1t(E, not(X), S, not(X1), S1) :-
    expStar(E, X, S, X1, S1).

/*
**    Meno unario.
**    ------------
*/

e1t(_, uminus(X), S, Y, S) :-
    integer(X),
    Y is -X.
e1t(E, uminus(X), S, uminus(X1), S1) :-
    expStar(E, X, S, X1, S1).

/*
**    Riferimento di array.
**    ---------------------
*/

e1t(E, aref(X, El), S, C, S) :-
    isConstList(El),
    getEnv(E, X, a(Limits, Base)), arrayIndex(Limits, El, Offset),
    Loc is Base+Offset,
    getStore(S, Loc, C).
e1t(E, aref(X, El), S, aref(X, El1), S1) :-
    el1t(E, El, S, El1, S1).

/*
**    Identificatore.
**    ---------------
*/

e1t(E, i(X), S, C, S) :-
    getEnv(E, X, D),
    (((D=l(L) ; D=r(L) ; D=c(_, L)), getStore(S, L, C)) ; C=D).

/*
**    Operatori binari.
**    -----------------
**
**    Le clausole che seguono, apparentemente complicate, hanno lo
**    scopo, insieme a quelle in `statsem' e `common', di evitare
**    la proliferazione di clausole per il trattamento degli operatori
**    binari. In tal modo, inoltre, l'introduzione di nuovo operatori
**    e` estremamente agevole, richiedendo pochissime modifiche
**    (basta aggiungere clausole per il predicato tbop/5 (`common')
**    ed eventualmente la definizione dell'operatore semantico
**    in `runtime'.
*/

e1t(_, X, S, Y, S) :-
    X =.. [O, Arg1, Arg2],
    tbop(O, D, D, R, F),
    isConst(Arg1), isConst(Arg2),
    (D = int ->                   /* ==> op aritmetico o relazionale */
        (Exp =.. [F, Arg1, Arg2],
        (R = int ->               /* ==> op aritmetico */
            (Eval =.. [is, Y, Exp], call(Eval))
        ;                         /* ==> op relazionale */
            (call(Exp) -> Y = tt ; Y = ff)))
    ;                             /* ==> op booleano */
        (Eval =.. [F, Arg1, Arg2, Y], call(Eval))).

e1t(E, X, S, Y, S1) :-
    X =.. [O, A, B],
    tbop(O, _, _, _, _),
    isConst(A),
    expStar(E, B, S, B1, S1),
    Y =.. [O, A, B1].

e1t(E, X, S, Y, S1) :-
    X =.. [O, A, B],
    tbop(O, _, _, _, _),
    expStar(E, A, S, A1, S1),
    Y =.. [O, A1, B].

/*
**    Espressione condizionale.
**    -------------------------
*/

e1t(_, if(tt, Y, _), S, Y, S).
e1t(_, if(ff, _, Z), S, Z, S).
e1t(E, if(X, Y, Z), S, if(C, Y, Z), S1) :-
    expStar(E, X, S, C, S1).

/*
**    Input.
**    ------
*/

e1t(_, input(X), S, C, S) :-
    doInput(X, C).

/*
**    Espressioni con side-effects.
**    -----------------------------
*/

e1t(_, side(D, halt, K), S, K, S1) :-
    isConst(K),
    dispose(D, S, S1).
e1t(E, side(D, halt, Exp), S, side(D, halt, Exp1), S1) :-
    compose(E, D, G),
    expStar(G, Exp, S, Exp1, S1).
e1t(E, side(D, C, Exp), S, side(D, C1, Exp), S1) :-
    isEnv(D),
    compose(E, D, G),
    comStar(G, C, S, C1, S1).
e1t(E, side(D, C, Exp), S, side(D1, C, Exp), S1) :-
    declStar(E, D, S, D1, S1).

/*
**    Chiamata di funzione.
**    ---------------------
*/

e1t(E, funcall(F, Ae), S, let(parPass(Formals, Ae), Body), S) :-
    getEnv(E, F, func(Formals, Body)).

/*
**    Blocco delle espressioni.
**    -------------------------
*/

e1t(_, let(D, K), S, K, S1) :-
    isEnv(D),
    isConst(K),
    dispose(D, S, S1).
e1t(E, let(D, Exp), S, let(D, Exp1), S1) :-
    isEnv(D),
    compose(E, D, G),
    expStar(G, Exp, S, Exp1, S1).
e1t(E, let(D, Exp), S, let(D1, Exp), S1) :-
    declStar(E, D, S, D1, S1).

/*
**    Blocco per la chiusura delle astrazioni di espressione.
**    -------------------------------------------------------
**
**    E` del tutto simile al normale blocco delle espressioni con
**    le sequenti differenze:
**
**        1) riceve sempre un ambiente;
**        2) non effettua il dispose delle variabili.
*/

e1t(_, closureE(_, K), S, K, S) :-
    isConst(K).
e1t(E, closureE(D, Exp), S, closureE(D, Exp1), S1) :-
    compose(E, D, G),
    expStar(G, Exp, S, Exp1, S1).

/*
**    Clausole di semantica dinamica delle liste di espressioni.
**    ==========================================================
**
**    el1t(E, El, S, El1, S1)
**        - E       e` un ambiente di valori;
**        - El, El1 sono liste di espressioni;
**        - S, S1   sono store.
**
**    Vero se nell'ambiente E la configurazione di lista di
**    espressioni <El, S> transisce in <El1, S1> in un certo
**    numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

el1t(E, [C | Ae], S, [C | Ae1], S1) :-
    isConst(C),
    el1t(E, Ae, S, Ae1, S1).
el1t(E, [H | Ae], S, [H1 | Ae], S1) :-
    expStar(E, H, S, H1, S1).
el1t(_, [], S, [], S).

/*
**    Clausole di semantica dinamica delle espressioni attuali.
**    =========================================================
**
**    ae1t(E, Fl, El, S, El1, S1)
**        - E       e` un ambiente di valori;
**        - Fl      e` una lista di parametri formali;
**        - El, El1 sono espressioni attuali;
**        - S, S1   sono store.
**
**    Vero se nell'ambiente E con la lista di formali Fl
**    la configurazione di espressione attuale
**    <El, S> transisce in <El1, S1> in un certo numero di passi.
**
**    N.B. Valgono considerazioni analoghe a quelle fatte sopra
**         per quanto riguarda il numero di passi.
*/

/*
**    Call by value.
**    --------------
*/

ae1t(E, [ival(_,_)|Form], [C|Ae], S, [C|Ae1], S1) :-
     isConst(C),
     ae1t(E, Form, Ae, S, Ae1, S1).
ae1t(E, [ival(_,_)|_], [C|Ae], S, [C1|Ae], S1) :-
     expStar(E, C, S, C1, S1).

/*
**    Call by name.
**    -------------
*/

ae1t(E, [name(_,_)|Form], [Exp|Ae], S, [NewExp|Ae1], S1) :-
    eFV(Exp, V),
    (V=[] ->
        NewExp = Exp
    ;
        (projection(E, V, E0), NewExp = closureE(E0, Exp))),
    ae1t(E, Form, Ae, S, Ae1, S1).

/*
**    Call by reference.
**    ------------------
*/

ae1t(E, [ref(_,_)|Form], [i(X)|Ae], S, [L|Ae1], S1) :-
    getEnv(E, X, H),
    (H=l(L) ; H=L),
    ae1t(E, Form, Ae, S, Ae1, S1).

ae1t(E, [ref(_,_)|Form], [aref(X, El)|Ae], S, [L|Ae1], S1) :-
    isConstList(El),
    getEnv(E, X, a(Limits, Base)),
    arrayIndex(Limits, El, Offset),
    L is Base+Offset,
    ae1t(E, Form, Ae, S, Ae1, S1).

ae1t(E, [ref(_,_)|_], [aref(X, El)|Ae], S, [aref(X, El1)|Ae], S1) :-
    el1t(E, El, S, El1, S1).

/*
**    Call by constant.
**    -----------------
*/

ae1t(E, [const(_,_)|Form], [C|Ae], S, [C|Ae1], S1) :-
     isConst(C),
     ae1t(E, Form, Ae, S, Ae1, S1).
ae1t(E, [const(_,_)|_], [C|Ae], S, [C1|Ae], S1) :-
     expStar(E, C, S, C1, S1).

/*
**    Call by value-result (copy in-copy out).
**    ----------------------------------------
*/

ae1t(E, [copy(_,_)|Form], [i(X)|Ae], S, [lr(L, R)|Ae1], S1) :-
    getEnv(E, X, l(L)),
    getStore(S, L, R),
    ae1t(E, Form, Ae, S, Ae1, S1).

ae1t(E, [copy(_,_)|_], [C|Ae], S, [C1|Ae], S1) :-
    expStar(E, C, S, C1, S1).

/*
**    Parametri funzionali.
**    ---------------------
*/

ae1t(E, [funcpar(_,_,_)|Form], [func(F, B)|Ae], S, [func(F, B)|Ae1], S1) :-
    ae1t(E, Form, Ae, S, Ae1, S1).
ae1t(E, [funcpar(_,_,_)|Form], [i(FuncId)|Ae], S, [func(F, B)|Ae1], S1) :-
    getEnv(E, FuncId, func(F, B)),
    ae1t(E, Form, Ae, S, Ae1, S1).

/*
**    Parametri procedurali.
**    ----------------------
*/

ae1t(E, [procpar(_,_)|Form], [proc(F, B)|Ae], S, [proc(F, B)|Ae1], S1) :-
    ae1t(E, Form, Ae, S, Ae1, S1).
ae1t(E, [procpar(_,_)|Form], [i(ProcId)|Ae], S, [proc(F, B)|Ae1], S1) :-
    getEnv(E, ProcId, proc(F, B)),
    ae1t(E, Form, Ae, S, Ae1, S1).

ae1t(_, [], [], S, [], S).

/*
**    isConstList(El)
**        - El e` una lista di espressioni.
**
**    Vero se El e` una lista di costanti.
*/

isConstList([C | Ac]) :-
    isConst(C),
    isConstList(Ac).
isConstList([]).

/*
**    comTrace(E, C, S, Cf, Sf)
**        - E     e` un ambiente di valori;
**        - C, Cf sono comandi;
**        - S, Sf sono store.
**
**    Vero se nell'ambiente E la configurazione di comando <C, S>
**    transisce nella configurazione (terminale) <Cf, Sf>.
**    Come side-effect provoca il tracciamento dell'esecuzione
**    del comando.
*/

comTrace(_, halt, S, halt, S) :- !,
        outputConfig(halt),
        outputStore(S).

comTrace(E, C, S, Cf, Sf) :-
        outputConfig(C),
        outputStore(S),
        c1t(E, C, S, C1, S1), !, comTrace(E, C1, S1, Cf, Sf).

/*
**    Clausole di semantica dinamica dipendenti dallo stato del tracer.
**    =================================================================
*/

/*
**    expStar(A, E, S, E1, S1)
**        - A     e` un ambiente di valori;
**        - E, E1 sono espressioni;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente A la configurazione di espressione
**    <E, S> transisce nella configurazione <E1, S1>.
**    Se il tracer e` attivo expStar/5 e` equivalente a e1t/5,
**    altrimenti expStar/5 invoca ricorsivamente se stesso
**    finche` E1 non e` una costante.
*/

traceDependentClause(on, (
    expStar(E, X, S, X1, S1) :- e1t(E, X, S, X1, S1), !
)).

traceDependentClause(off, (
    expStar(_, X, S, X, S) :- isConst(X), !
)).

traceDependentClause(off, (
    expStar(E, X, S, Y, Sf) :-
        e1t(E, X, S, X1, S1), !,
        expStar(E, X1, S1, Y, Sf)
)).

/*
**    declStar(E, D, S, D1, S1)
**        - E     e` un ambiente di valori;
**        - D, D1 sono dichiarazioni;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente E la configurazione di dichiarazione
**    <D, S> transisce nella configurazione <D1, S1>.
**    Se il tracer e` attivo declStar/5 e` equivalente a d1t/5,
**    altrimenti declStar/5 invoca ricorsivamente se stesso
**    finche` D1 non e` un ambiente.
*/

traceDependentClause(on, (
    declStar(E, D, S, D1, S1) :- d1t(E, D, S, D1, S1), !
)).

traceDependentClause(off, (
    declStar(_, X, S, X, S) :- isEnv(X), !
)).

traceDependentClause(off, (
    declStar(E, X, S, Y, Sf) :-
        d1t(E, X, S, X1, S1), !,
        declStar(E, X1, S1, Y, Sf)
)).

/*
**    comStar(E, C, S, C1, S1)
**        - E     e` un ambiente di valori;
**        - C, C1 sono comandi;
**        - S, S1 sono store.
**
**    Vero se nell'ambiente E la configurazione di comando
**    <C, S> transisce nella configurazione <C1, S1>.
**    Se il tracer e` attivo comStar/5 e` equivalente a c1t/5,
**    altrimenti comStar/5 invoca ricorsivamente se stesso
**    finche` C1 non e` `halt' (<halt, S> e` una configurazione
**    terminale dei comandi).
*/

traceDependentClause(on, (
    comStar(E, C, S, C1, S1) :- c1t(E, C, S, C1, S1), !
)).

traceDependentClause(off, (
    comStar(_, halt, S, halt, S) :- !
)).

traceDependentClause(off, (
    comStar(E, X, S, Y, Sf) :-
        c1t(E, X, S, X1, S1), !,
        comStar(E, X1, S1, Y, Sf)
)).

/*
**    topDynamicImperative(C)
**        - C e` un comando.
**
**    Vero se esiste una sequenza (finita) di transizioni che,
**    nell'ambiente vuoto, porta la configurazione di comando
**    <C, []> in <halt, S>, dove [] e` lo store vuoto ed S un
**    qualunque store.
**
**    (In realta` S=[] dato che viene esequito il
**     dispose delle locazioni all'uscita dei blocchi).
*/

traceDependentClause(off, (
    topDynamicImperative(C) :-
        comStar([], C, [], halt, _)
)).

traceDependentClause(on, (
    topDynamicImperative(C) :-
        comTrace([], C, [], halt, _)
)).

/*
**    SLice: modulo `runtime'
**
**    Questo modulo contiene i predicati che implementano il supporto
**    a tempo di esecuzione dei linguaggi di SLice.
**
**    Predicati definiti in questo modulo:
**
**    arrayIndex/3, atomChar/1, bomb/0, changeStore/4, delete/3,
**    digit/2, dispose/3, disposeArray/4, doInput/2, doOutput/1,
**    doPrint/2, doPrint1/2, getNewLoc/1, getStore/3, makeArray/4,
**    makeEnv/3, makeEnvStore/4, makeRArray/3, makeRecEnv/3,
**    numElem/2, readAtom/1, readAtom_1/2, readInteger/1, readInteger_2/2,
**    readInteger_3/3, readString/1, readString_1/2, returnAtom/2,
**    semAnd/3, semOr/3. 
*/

/*
**    Supporto runtime per l'espressione `input'.
**    -------------------------------------------
**
**    doInput(T, K)
**        - T e` istanziato a `int' oppure `bool';
**        - K e` una variabile non istanziata.
**
**    Legge una costante dallo stream di input e istanzia K alla
**    relativa rappresentazione.
**
**    Se T=int viene richiesto un intero e K viene istanziato
**    corrispondentemente: ad esempio
**
**        l'utente digita "1" "2" <CR> ==> K viene istanziato a 12.
**
**    Se T=bool viene richiesto un valore booleano (true o false) e
**    K viene istanziato corrispondentemente (rispettivamente a tt o ff).
**
**    Se l'input non corrisponde al tipo doInput/2 invoca ricorsivamente
**    se stesso fino a che` non viene digitato qualcosa di legale.
*/
 
doInput(int, N) :- prompt(Old, 'Give me an integer> '),
    readInteger(N), prompt(_, Old).

doInput(bool, B) :- prompt(Old, 'Give me a boolean> '),
    readAtom(A),
    ((((A=true, !, B=tt); (A=false, !, B=ff)), prompt(_, Old));
        (print('Sorry, but a boolean is expected.'), nl, doInput(bool, B))).

/*
**    Supporto runtime per il comando `print'.
**    ----------------------------------------
**
**    doPrint(F, El)
**        - F  e` una stringa (lista di codici ASCII);
**        - El e` una lista di costanti (possibilmente vuota).
**
**    F e` una stringa di caratteri che contiene due tipi di oggetti:
**    caratteri normali che sono semplicemente visualizzati sul
**    terminale, e specifiche di uscita.
**    Ogni specifica di uscita e` introdotta dal carattere pound (#).
**    Dopo al carattere  `#' puo` comparire:
**
**        1) il carattere `n':
**             viene iniziata una nuova linea (newline);
**
**        2) il carattere `e':
**             viene stampata la corrispondente costante in El;
**
**        3) il carattere `#':
**             viene stampato `#';
**
**        4) ogni altro carattere C:
**             viene stampato `#C'.
*/

doPrint(FormAtom, Cl) :-
    name(FormAtom, Format),
    doPrint1(Format, Cl).

doPrint1([35, 35 |Rest], Cl) :-
    ttyput(35),
    doPrint1(Rest, Cl).
doPrint1([35, 110|Rest], Cl) :- !,
    ttynl,
    doPrint1(Rest, Cl).
doPrint1([35, 101|Rest], [C|Cl]) :- !,
    doOutput(C),
    doPrint1(Rest, Cl).
doPrint1([35, 101|Rest], []) :- !,
    display('*no value*'),
    doPrint1(Rest, []).
doPrint1([Char|Chars], Cl) :- !,
    ttyput(Char),
    doPrint1(Chars, Cl).
doPrint1([], _).

/*
**    doOutput(X)
**        - X e` una costante (tt, ff o intero).
**
**    Stampa `true' se X=tt, `false' se X=ff, altrimenti stampa
**    l'intero X.
*/

doOutput(tt) :- display(true).
doOutput(ff) :- display(false).
doOutput(X) :- display(X).

/*
**    Supporto run-time per gli operatori booleani.
**    ---------------------------------------------
*/

semOr(ff, ff, ff) :- !.
semOr(_, _, tt).

semAnd(tt, tt, tt) :- !.
semAnd(_, _, ff).

/*
**    Supporto run-time per gli array.
**    --------------------------------
*/

/*
**    numElem(Limits, N)
**       - Limits e` una lista di limiti di array ([b(L1, H1), ...]);
**       - N e` un intero.
**
**    Vero se N e` il numero di elementi di un array con limiti Limits.
**    Per il calcolo viene usata una semplice relazione ricorrente.
*/

numElem([b(L, H)|Tail], N) :- numElem(Tail, M), N is (H-L+1)*M.
numElem([], 1).

/*
**    makeArray(Limits, Base, Init, Store)
**       - Limits e` una lista di limiti di array ([b(L1, H1), ...]);
**       - Base   e` un intero;
**       - Init   e` una costante (tt, ff o intero);
**       - Store  e` uno store.
**
**    Dati Limits e Init istanzia le restanti variabili in modo che
**    Store e` un (piccolo) store che contiene tante locazioni
**    consecutive (*) quante ne servono per memorizzare un array con
**    limiti Limits (vedi numElem/2), tutte contenenti la costante Init,
**    la prima delle quali e` Base.
**
**    (*) Per l'implementazione degli array si e` scelto di usare
**        interi per denotare locazioni. La consecutivita` e` una
**        proprieta` di getNewLoc/1.
*/

makeArray(Limits, Base, Init, [s(Base, Init)|Rest]) :-
    getNewLoc(Base), numElem(Limits, N), makeRArray(N, Init, Rest).

makeRArray(1, _, []).
makeRArray(N, Init, [s(L, Init)|Rest]) :-
    getNewLoc(L), M is N-1, makeRArray(M, Init, Rest).

/*
**    arrayIndex(Limits, Indexes, Offset)
**       - Limits  e` una lista di limiti di array ([b(L1, H1), ...]);
**       - Indexes e` una lista di indici interi;
**       - Offset  e` un intero.
**
**    Calcola l'offset della cella di un array con limiti Limits
**    indicizzato con gli indici Indexes. Viene utilizzata la
**    seguente relazione ricorrente:
**
**    siano L1..H1, ..., Lm..Hm i limiti di un array;
**          I1, ..., Im         la lista degli indici;
**          Ni = Hi-Li+1        per ogni 1 <= i <= m;
**
**    allora Offset = Em dove
**
**        E0 = 0
**        Ek = E(k-1)*Nk + Ik-Lk
*/

arrayIndex([b(L, H)|T1], [I|T2], Offset) :-
    ((I >= L, I =< H) ->
        (arrayIndex(T1, T2, R), Offset is R*(H-L+1)+I-L)
    ;
        (bomb, !, fail)).

arrayIndex([], [], 0).

/*
**    bomb
**
**    Viene invocata da arrayIndex/3, in caso che gli indici di un
**    riferimento di array eccedano i limiti dell'array stesso.
**    E` un po` brutale...
*/

bomb :-
    ttynl, display('Panic: array index out of bounds!!!'), ttynl, abort.

/*
**    Supporto run-time per le dichiarazioni ricorsive.
**    -------------------------------------------------
**
**    makeRecEnv(X, Y, Z) -
**        - X e` un ambiente da rendere ricorsivo;
**        - Y e` l'ambiente ancora da trasformare;
**        - Z e` l'ambiente risultante.
**
**    Implementa la fase finale dell'elaborazione delle
**    dichiarazioni ricorsive (vedi clausole semantiche).
*/

/*
**    Funzioni.
*/

makeRecEnv(E,
    [e(X, func(Formals, Body)) | T1],
    [e(X, func(Formals, let(rec(E1), Body))) | T2]) :-
        formDV(Formals, V), mask(E, V, E1), makeRecEnv(E, T1, T2).

/*
**    Procedure.
*/

makeRecEnv(E,
    [e(X, proc(Formals, Body)) | T1],
    [e(X, proc(Formals, block(rec(E1), Body))) | T2]) :-
        formDV(Formals, V), mask(E, V, E1), makeRecEnv(E, T1, T2).

/*
**    Altri valori denotabili.
*/

makeRecEnv(E, [H | T1], [H | T2]) :-
    makeRecEnv(E, T1, T2).

makeRecEnv(_, [], []).

/*
**    Supporto run-time per il passaggio dei parametri.
**    -------------------------------------------------
**
**    Linguaggio funzionale.
**
**    makeEnv(Form, Act, Env)
**        - Form e` una lista di parametri formali;
**        - Act  e` una lista di parametri attuali gia` elaborati;
**        - Env  e` un ambiente.
**
**    Vero se Env e` il piccolo ambiente ottenuto associando ad
**    ogni parametro formale il corrispondente attuale.
**    Implementa la fase finale del passaggio dei parametri nel
**    linguaggio funzionale.
*/

/*
**    Call by value.
*/

makeEnv([fval(X, _) | Form], [C | Ac], [e(X, C) | Env]) :-
    makeEnv(Form, Ac, Env).

/*
**    Call by name.
*/

makeEnv([name(X, _) | Form], [C | Ac], [e(X, C) | Env]) :-
    makeEnv(Form, Ac, Env).

/*
**    Parametri funzionali.
*/

makeEnv([funcpar(F, _, _) | Form], [Func | Ac],  [e(F, Func) | Env]) :-
    makeEnv(Form, Ac, Env).

makeEnv([], [], []).

/*
**    Supporto run-time per il passaggio dei parametri.
**    -------------------------------------------------
**
**    Linguaggio imperativo.
**
**    makeEnvStore(Form, Act, Env, Store)
**        - Form  e` una lista di parametri formali;
**        - Act   e` una lista di parametri attuali gia` elaborati;
**        - Env   e` un ambiente;
**        - Store e` uno store.
**
**    Implementa la fase finale del passaggio dei parametri nel
**    linguaggio funzionale.
**    Vero se Env e Store sono, rispettivamente, l'ambiente e
**    lo store ottenuti eseguendo il passagio dei parametri
**    Form <- Act (vedi clausole semantiche).
*/

/*
**    Call by value.
*/

makeEnvStore([ival(X, _) | Form], [C | Ac],
             [e(X, l(L)) | Env],  [s(L, C) | Store]) :-
    getNewLoc(L), makeEnvStore(Form, Ac, Env, Store).

/*
**    Call by name.
*/

makeEnvStore([name(X, _) | Form], [C | Ac],
             [e(X, C) | Env], Store) :-
    makeEnvStore(Form, Ac, Env, Store).

/*
**    Call by reference.
*/

makeEnvStore([ref(X, _) | Form], [L | Ac],
             [e(X, r(L)) | Env], Store) :-
    makeEnvStore(Form, Ac, Env, Store).

/*
**    Call by constant.
*/

makeEnvStore([const(X, _) | Form], [C | Ac],
             [e(X, C) | Env], Store) :-
    makeEnvStore(Form, Ac, Env, Store).

/*
**    Call by value-result (copy in-copy out).
*/

makeEnvStore([copy(X, _) | Form], [lr(L1, C) | Ac],
             [e(X, c(L1, L2)) | Env], [s(L2, C) | Store]) :-
    getNewLoc(L2),
    makeEnvStore(Form, Ac, Env, Store).

/*
**    Parametri funzionali.
*/

makeEnvStore([funcpar(F, _, _) | Form], [Func | Ac],
             [e(F, Func) | Env], Store) :-
    makeEnvStore(Form, Ac, Env, Store).

/*
**    Parametri procedurali.
*/

makeEnvStore([procpar(P, _, _) | Form], [Proc | Ac],
             [e(P, Proc) | Env], Store) :-
    makeEnvStore(Form, Ac, Env, Store).

makeEnvStore([], [], [], []).

/*
**    Supporto run-time per la gestione degli store.
**    ----------------------------------------------
*/

/*
**    getNewLoc(N)
**        - N e` un intero.
**
**    Istanzia N ad un intero, invocazioni successive danno luogo
**    alla generazione di interi successivi. La caratteristica
**    di generare interi successivi e` fondamentale per
**    l'implementazione degli array.
**    La numerazione viene fatta ripartire da 0 dal comando run
**    (vedi `commands').
*/

getNewLoc(Num) :-
    retract(currentLocNum(Num1)), !,
    Num is Num1+1,
    asserta(currentLocNum(Num)).
getNewLoc(0) :- asserta(currentLocNum(0)). /* Non usata */

/*
**    getStore(Store, Loc, Val)
**        - Store e` uno store ([s(l1, v1), ..., s(lk, vk)]);
**        - Loc   e` una locazione (un intero);
**        - Val   e` una costante (tt, ff o intero).
**
**    Vero se in Store alla locazione Loc e` associato il valore
**    memorizzabile Val.
*/

getStore([s(L, V)|_], L, V) :- !.
getStore([_|T], L, V) :- getStore(T, L, V).

/*
**    changeStore(Store, Loc, NewVal, NewStore)
**        - Store, NewStore sono store;
**        - Loc             e` una locazione;
**        - NewVal          e` un valore memorizzabile (tt, ff o intero).
**
**    Vero se NewStore e` ottenuto da Store cambiando l'associazione
**    <Loc, Store(Loc)> in <Loc, Val>.
*/

changeStore(S, Loc, Val, [s(Loc, Val)|S1]) :-
    delete(s(Loc, _), S, S1).

/*
**    dispose(Env, Store, NewStore) -
**        - Env             e` un ambiente di valori;
**        - Store, NewStore sono store.
**
**    Questo predicato e` invocato al termine dell'elaborazione dei
**    blocchi "d ; c" (sintassi astratta), e provvede alla
**    post-elaborazione degli stessi.
**    Vero se NewStore e` ottenuto da Store:
**
**        1) rimuovendo le associazioni <Loc, SVal>
**           tali che <_, l(Loc)> e` un elemento di Env.
**           Per quanto detto sopra cio` equivale a dire che
**           tutte le variabili dichiarate nel blocco vengono
**           rimosse.
**
**        2) rimuovendo le associazioni <Loc, SVal> che appartengono
**           agli array nel codominio di Env.
**           Come sopra: tutte le locazioni di array vengono rimosse.
**
**        3) ricopiando Store(L2) in Store(L1) e rimuovendo
**           l'associazione <L2, SVal> per ogni parametro
**           passato per value-result c(L1, L2) che compare
**           in Env.
**           Mentre ai punti 1) e 2) le azioni svolte da dispose/3
**           erano una semplice ottimizzazione, qui viene implementata
**           la fase finale del passaggio dei parametri per value-result.
*/

dispose([e(_, l(L)) | T], S, S1) :-
    delete(s(L, _), S, S2),
    dispose(T, S2, S1).

dispose([e(_, a(L, B)) | T], S, S1) :-
    numElem(L, N),
    disposeArray(B, N, S, S2),
    dispose(T, S2, S1).

dispose([e(_, c(L1, L2)) | T], S, S1) :-
    getStore(S, L2, C),
    delete(s(L2, _), S, S2),
    changeStore(S2, L1, C, S3),
    dispose(T, S3, S1).

dispose([_ | T], S, S1) :-
    dispose(T, S, S1).

dispose([], S, S).

disposeArray(_, 0, S, S).
disposeArray(B, N, S, S1) :- delete(s(B, _), S, S2), C is B+1, M is N-1,
    disposeArray(C, M, S2, S1).

/*
**    delete(Elem, List, NewList)
**        - Elem          e` un termine;
**        - List, NewList sono liste.
**
**    Vero se la lista NewList e` ottenuta da List rimuovendo il
**    primo elemento che unifica con Elem.
*/

delete(X, [X|Y], Y).
delete(X, [Y|L], [Y|NL]) :- delete(X, L, NL).

/*
**    readInteger(X)
**
**    Legge un intero dallo stream di input corrente.
*/

readInteger(Res) :- get0(Char), ((Char = 45) ->
                                     (get0(Char1),
                                     readInteger_2(Char1,Res1),
                                     Res is -Res1)
                                 ;
                                     readInteger_2(Char, Res)).

readInteger_2(Char,Result) :-
    digit(Char,_), ! ,
    readInteger_3(Char,0,Result).
readInteger_2(Char,Result) :-
    readString_1(Char,_),    /* Ignora fino a fine linea */
    print('Sorry, but an integer is expected.'), nl,
    readInteger(Result).

readInteger_3(Digit,Accu,Result) :-
    digit(Digit,Digit_weight), ! ,
    get0(Inter_char), /* overtaking 'Digit' */
    Inter_accu is Accu*10+Digit_weight,
    readInteger_3(Inter_char,Inter_accu,Result).
readInteger_3(_,Accu,Accu).

digit(X,Weight) :- X >= 48, X =< 57, Weight is X-48.

/*
**    readString(S)
**
**    Legge una stringa (fino a fine linea) dello stream di input corrente.
*/

readString(String) :- get0(Char), readString_1(Char,String).

/* ascii(<CR>,10) */

readString_1(10,[]) :- ! .
readString_1(Char,[Char|Tail]) :-
    get0(Next), readString_1(Next,Tail).


/*
**    readAtom(A)
**
**    Legge un atomo Prolog dal corrente stream di ingresso.
*/

readAtom(Atom) :-
    get0(Char),
    readAtom_1(Char,String),
    returnAtom(String,Atom).

readAtom_1(Char,[Char|Tail]) :-
    atomChar(Char), ! ,
    get0(Next), readAtom_1(Next,Tail).
readAtom_1(_,[]) :- ! .

atomChar(C) :- C >= 97, C =< 122 .  /* le lettere minuscole */
atomChar(C) :- C >= 64, C =< 90 .   /* le lettere maiuscole */
atomChar(C) :- C >= 47, C =< 57 .   /* le cifre */
atomChar(39). /* ascii(39,'''') */
atomChar(45). /* ascii(45,'-') */
atomChar(95). /* ascii(95,'_') */

returnAtom([],[]) :- ! .
returnAtom(String,Atom) :- name(Atom,String).

/*
**    Slice: modulo `trace'
**
**    Questo modulo contiene i predicati che implementano il
**    pretty printing dell'albero astratto del programma, generato
**    dal parser e successivamente modificato dai predicati di semantica
**    dinamica. Grazie a questo modulo l'output della traccia
**    dell'esecuzione di un programma e` sufficientemente leggibile.
**    I dettagli implementativi di questo modulo sono irrilevanti,
**    i commenti sono percio` piuttosto stringati.
**
**    Predicati definiti in questo modulo:
**
**    nextStep/1, outputConfig/1, outputStore/1, pS/2, pST/2, pStore/1,
**    pp/1, ppAct/2, ppStore/1, ppae/2, ppc/3, ppd/3, ppe/3, ppel/2,
**    ppenv/1, ppenvstuff/1, ppf/1, ppfs/1, ppin/1, ppop/1, ppt/1,
**    s/1, tabS/2. 
*/

/*
**    outputConfig(X)
**
**    Agginge il pretty print della configurazione X al corrente
**    file di tracciamento.
*/

outputConfig(_) :-
    traceConfig(no), !.
outputConfig(X) :-
    telling(What),
    traceFile(F),
    append(F),
    nextStep(N),
    (N=\=0 ->
        (write('=============================== step '),
        write(N), write(' ==============================='), nl)
    ;
        true),
    pp(X), !,
    told,
    tell(What).

/*
**    outputStore(S)
**
**    Agginge il pretty print dello store S al corrente
**    file di tracciamento.
*/

outputStore(_) :-
    traceStore(no), !.
outputStore([]) :- !.
outputStore(X) :-
    telling(What),
    traceFile(F),
    append(F),
    write('-----------------------------------'),
    write('-----------------------------------'), nl,
    ppStore(X), !,
    told,
    tell(What).

/*
**    pp(X)
**
**    Stampa la configurazione X invocando i predicati opportuni,
**    a seconda del linguaggio correntemente in uso.
*/

pp(X) :-
    (language(functional) ->
        ppe(0, 0, X)
    ;
        ppc(0, 0, X)),
    nl.

/*
**    s(N)
**
**    Tabulatore: null-effect se N=0, altrimenti inizia una nuova linea
**    sul corrent file di tracciamento e vi aggiunge N spazi.
*/

s(0) :- !.
s(N) :- nl, tab(N).

/*
**    ppt(Type)
**
**    Pretty printing del tipo Type.
*/

ppt(int) :-
     write('integer').
ppt(bool) :-
     write('boolean').
ppt(array(L, T)) :-
     write('array ['),
     ppin(L),
     write('] of '),
     ppt(T).

/*
**    ppin(Limits)
**
**    Pretty printing dei limiti di un array Limits.
*/

ppin([b(B,L)]) :-
    write(B), write('..'), write(L).
ppin([H|T]) :-
    ppin([H]),
    write(','),
    ppin(T).
ppin([]).

/*
**    ppel(El)
**
**    Pretty printing della lista El di espressioni.
*/
 
ppel(W, [E]) :-
    ppe(W, 0, E).
ppel(W, [E|El]) :-
    ppe(W, 0, E),
    write(','),
    ppel(W, El).
ppel(_, []).

/*
**    Pretty printing delle espressioni.
*/

/*
**    ppe(W, N, E)
**      - W e` l'indentazione corrente;
**      - N e` 0 se la stampa deve avvenire normalmente,
**          altrimenti la stampa deve essere preceduta da 
**          newline e da N spazi;
**      - E e` l'espressione da stampare.
*/

/*
**    Costanti booleane.
*/

ppe(_, N, tt) :-
    s(N),
    write(true).
ppe(_, N, ff) :-
    s(N),
    write(false).

/*
**    Identificatori.
*/

ppe(_, N, i(X)) :-
    s(N),
    write(X).
ppe(W, N, aref(X, El)) :- 
    s(N),
    write(X), write('['),
    ppel(W, El),
    write(']').

/*
**    Negazione booleana.
*/

ppe(W, N, not(X)) :-
    s(N),
    write('not '),
    M is W+2, ppe(M, 0, X).

/*
**    Meno unario.
*/

ppe(W, N, uminus(X)) :-
    s(N),
    write('-'),
    M is W+2, ppe(M, 0, X).

/*
**    Blocco delle espressioni.
*/

ppe(W, _, let(D, E)) :-
    s(W),
    write('let'),
    M is W+2, ppd(M, M, D), 
    nl, tab(W), write('in'),
    ppe(M, M, E).

/*
**    Blocco per la chiusura delle astrazioni delle espressioni:
**    funzioni e parametri passati per nome.
*/

ppe(W, _, closureE(D, E)) :-
    s(W),
    write('let /* closure, no dispose */'),
    M is W+2, ppd(M, M, D), 
    nl, tab(W), write('in'),
    ppe(M, M, E).

/*
**    Espressione condizionale.
*/

ppe(W, _, if(E, E1, E2)) :-
    s(W),
    write('(if '),
    M is W+2, ppe(M, 0, E),
    write(' then'),
    ppe(M, M, E1),
    nl, tab(W), write('else'),
    ppe(M, M, E2),
    write(')').

/*
**    Chiamata di funzione.
*/

ppe(W, N, funcall(F, El)) :-
    s(N),
    write(F), write('('),
    M is W+2, ppel(M, El),
    write(')').

/*
**    Input.
*/

ppe(_, N, input(T)) :-
    s(N),
    write('input '),
    ppt(T).

/*
**    Espressioni con side-effects.
*/

ppe(W, _, side(D, C, E)) :-
    s(W),
    write('expr'),
    M is W+2, ppd(M, M, D),
    write(';'), 
    ppc(M, M, C),
    nl, tab(W), write('result '),
    ppe(M, M, E).

/*
**    Operatori binari.
*/

ppe(W, N, X) :-
    X =.. [O, Arg1, Arg2],
    tbop(O, _, _, _, _),
    (Arg1=if(_,_,_) ; Arg1=let(_,_) ; Arg1=side(_,_,_) ; s(N)),
    M is W+2, ppe(M, 0, Arg1),
    ppop(O),
    ppe(M, 0, Arg2).

/*
**    Costante intera.
*/

ppe(_, N, X) :-
    s(N),
    write(X).

/*
**    ppop(Op)
**
**    Pretty printing del singolo operatore Op.
*/

ppop(plus) :- write('+').
ppop(minus) :- write('-').
ppop(prod) :- write('*').
ppop(or) :- write(' or ').
ppop(div) :- write(' div ').
ppop(mod) :- write(' mod ').
ppop(and) :- write(' and ').
ppop(eq) :- write('==').
ppop(ne) :- write('<>').
ppop(lt) :- write('<').
ppop(le) :- write('<=').
ppop(ge) :- write('>=').
ppop(gt) :- write('>').

/*
**    Pretty printing delle dichiarazioni.
*/

/*
**    ppd(W, N, D)
**      - W e` l'indentazione corrente;
**      - N e` 0 se la stampa deve avvenire normalmente,
**          altrimenti la stampa deve essere preceduta da 
**          newline e da N spazi;
**      - E e` la dichiarazione da stampare.
*/

/*
**    Nil.
*/

ppd(_, N, nil) :-
    s(N),
    write('nil').

/*
**    Dichiarazione semplice.
*/

ppd(W, N, def(X,T,Y)) :-
    s(N),
    write(X), write(:),
    ppt(T),
    write('='),
    M is W+2, ppe(M, 0, Y).

/*
**    Dichiarazione di costante.
*/

ppd(W, N, con(X,T,Y)) :-
    s(N),
    write('const '), write(X), write(':'),
    ppt(T),
    M is W+2, write('='), ppe(M, 0, Y).

/*
**    Dichiarazione di variabile.
*/

ppd(W, N, var(X,T,Y)) :-
    s(N),
    write('var '), write(X), write(':'),
    ppt(T),
    M is W+2, write('='), ppe(M, 0, Y).

/*
**    Composizione sequenziale.
*/

ppd(W, N, seq(D1, D2)) :-
    ppd(W, N, D1),
    write(';'),
    ppd(W, N, D2).

/*
**    Composizione parallela.
*/

ppd(W, N, parallel(D1, D2)) :-
    ppd(W, N, D1),
    write(' |'),
    ppd(W, N, D2).

/*
**    Composizione privata.
*/

ppd(W, N, private(D1, D2)) :-
    ppd(W, N, D1),
    write('->'),
    M is N+2, ppd(M, M, D2).

/*
**    Dichiarazione di funzione.
*/

ppd(W, N, fundef(F, Form, T, E)) :-
    s(N),
    write('function '), write(F), write('('),
    ppfs(Form),
    write('):'),
    ppt(T),
    write(' ='),
    M is W+2, ppe(M, M, E).

/*
**    Dichiarazione di procedura.
*/

ppd(W, N, procdef(P, Form, C)) :-
    s(N),
    write('procedure '), write(P), write('('),
    ppfs(Form),
    write(')'),
    M is W+2, ppc(M, M, C).

/*
**    Dichiarazione ricorsiva.
*/

ppd(W, N, rec(D)) :-
    s(N),
    write('rec'),
    M is W+2, ppd(M, M, D).

/*
**    Passaggio dei parametri.
*/

ppd(W, N, parPass(Fl, Ae)) :-
    s(N),
    write('<'),
    ppfs(Fl),
    write('>=<'),
    M is W+2, ppae(M, Ae),
    write('>').

/*
**    Ambiente.
*/

ppd(_, N, E) :-
    s(N),
    isEnv(E),
    write('{'),
    ppenv(E),
    write('}').

ppd(_, N, X) :-
    s(N),
    write(X).

/*
**    ppenv(Env)
**
**    Pretty printing dell'ambiente Env.
*/

ppenv([e(X, V)]) :-
    write(X), write('='),
    ppenvstuff(V).
ppenv([H|T]) :-
    ppenv([H]),
    write(','),
    ppenv(T).
ppenv([]).

/*
**    ppenvstuff(Dt)
**
**    Pretty printing dei valori denotabili.
*/

ppenvstuff(l(L)) :-               /* locazione */
    write('L'), write(L).
ppenvstuff(r(L)) :-               /* parametro passato per riferimento */
    write('L'), write(L).
ppenvstuff(c(_, L)) :-            /* parametro passato per value-result */
    write('L'), write(L).
ppenvstuff(func(_, _)) :-         /* funzione */
    write('*F*').
ppenvstuff(proc(_, _)) :-         /* procedura */
    write('*P*').
ppenvstuff(a(_, _)) :-            /* array */
    write('*A*').
ppenvstuff(X) :-                  /* costante */
    isConst(X),
    write(X).
ppenvstuff(_) :-                  /* parametro passato per nome */
    write('*N*').

/*
**    ppae(W, Ae)
**   
**    Pretty printing delle liste di parametri attuali Ae.
*/

ppae(W, [Ae]) :-
    ppAct(W, Ae).
ppae(W, [H | Ae]) :-
    ppae(W, [H]),
    write(','),
    ppae(W, Ae).
ppae(_, []).

/*
**    ppAct(W, E)
** 
**    Pretty printing del parametro attuale E.
*/

ppAct(_, func(_,_)) :-
    write('*F*').
ppAct(_, proc(_,_)) :-
    write('*P*').
ppAct(W, Exp) :-
    ppe(W, 0, Exp).
ppAct(_, C) :-
    isConst(C),
    write(C).

/*
**    ppfs(Form)
**
**    Pretty printing dei parametri formali Form.
*/

ppfs([F]) :-
    ppf(F).
ppfs([F|Fl]) :-
    ppfs([F]),
    write(','),
    ppfs(Fl).
ppfs([]).

ppf(ival(X, T)) :-                          /* parametro per valore  */
    write(X), write(':'),                   /* linguaggio imperativo */
    ppt(T).
ppf(fval(X, T)) :-                          /* parametro per valore  */
    write(X), write(':'),                   /* linguaggio funzionale       */
    ppt(T).
ppf(name(X, T)) :-                          /* parametro per nome          */
    write('name '), write(X), write(':'),
    ppt(T).
ppf(ref(X, T)) :-                           /* parametro per riferimento   */
    write('ref '), write(X), write(':'),
    ppt(T).
ppf(const(X, T)) :-                         /* parametro per costante      */ 
    write('const '), write(X), write(':'),
    ppt(T).
ppf(copy(X, T)) :-                          /* parametro per value-result  */
    write('copy '), write(X), write(':'),
    ppt(T).
ppf(funcpar(N, F, T)) :-                    /* parametro funzionale        */
    write('function '), write(N), write('('),
    ppfs(F),
    write('):'),
    ppt(T).
ppf(procpar(N, F)) :-                       /* parametro procedurale      */
    write('procedure '), write(N), write('('),
    ppfs(F),
    write(')').

/*
**    Pretty printing dei comandi.
*/

/*
**    ppc(W, N, C)
**      - W e` l'indentazione corrente;
**      - N e` 0 se la stampa deve avvenire normalmente,
**          altrimenti la stampa deve essere preceduta da 
**          newline e da N spazi;
**      - E e` il comando da stampare.
*/

/*
**    Nop.
*/

ppc(_, N, nop) :-
    s(N),
    write('nop').

/*
**    Assegnamento.
*/

ppc(W, N, ass(i(X), E)) :- 
    s(N),
    write(X), write(':='),
    M is W+2, ppe(M, 0, E).

ppc(W, N, ass(aref(X, El), E)) :-
    s(N),
    write(X), write('['),
    ppel(W, El),
    write(']:='),
    M is W+2, ppe(M, 0, E). 

/*
**    Composizione sequenziale.
*/

ppc(W, N, seq(C1, C2)) :-
    ppc(W, N, C1),
    write(';'),
    ppc(W, N, C2).

/*
**    Comando condizionale.
*/

ppc(W, _, if(E, C1, C2)) :-
    s(W),
    write('if '),
    M is W+2, ppe(M, 0, E),
    write(' then'),
    ppc(M, M, C1),
    nl, tab(W), write('else'),
    ppc(M, M, C2).

/*
**    While.
*/

ppc(W, N, while(E,C)) :- 
    s(N),
    write('while '),
    M is W+2, ppe(M, 0, E),
    write(' do '),
    ppc(M, M, C).

/*
**    Comando di output 'print'.
*/

ppc(_, N, print(F, [])) :- !,
    s(N),
    write('print("'), write(F), write('")').

ppc(W, N, print(F, El)) :-
    s(N),
    write('print("'), write(F), write('", '),
    M is W+2, ppel(M, El),
    write(')').

/*
**    Chiamata di procedura.
*/

ppc(W, N, proccall(P, El)) :-
    s(N),
    write(P), write('('), 
    M is W+2, ppel(M, El),
    write(')').

/*
**    Blocco.
*/

ppc(_, N, block(D, C)) :-
    s(N),
    write('begin'),
    M is N+2, ppd(M, M, D),
    write(';'), 
    ppc(M, M, C),
    nl, tab(N), write('end').

/*
**    Blocco per la chiusura delle astrazioni procedurali.
*/

ppc(_, N, closureC(D, C)) :-
    s(N),
    write('begin /* closure */'),
    M is N+2, ppd(M, M, D),
    write(';'), 
    ppc(M, M, C),
    nl, tab(N), write('end /* no dispose */').

/*
**    Halt.
*/

ppc(_, N, halt) :-
    s(N),
    write('halt').

ppc(_, N, X) :-
    s(N),
    write(X).

/*
**    Pretty printing di uno store.
*/

ppStore(S) :-
    sort(S, S1),
    pStore(S1).

pStore([s(L1, V1), s(L2, V2), s(L3, V3), s(L4, V4)|Rest]) :- !,
    pST(L1, V1), pST(L2, V2), pST(L3, V3), pS(L4, V4), nl,
    pStore(Rest).

pStore([s(L, V)]) :- !,
    pS(L, V), nl.

pStore([s(L, V)|Rest]) :- !,
    pST(L, V),
    pStore(Rest).

pStore([]).

pST(L, V) :-
    pS(L, V),
    tabS(L, V).

pS(L, V) :-
    write('L'), write(L), write(':'), write(V).

tabS(L, V) :-
    name(L, N1), name(V, N2),
    length(N1, L1), length(N2, L2),
    TabNeeded is 20-(L1+L2+2),
    tab(TabNeeded).

/*
**    nextStep(N)
**
**    Istanzia N al numero del prossimo passo effettuato
**    nell'interpretazione.
**    La numerazione viene fatta ripartire da 0 dal comando run
**    (vedi `commands').
*/

nextStep(Num) :-
    retract(currentStep(Num1)), !,
    Num is Num1+1,
    asserta(currentStep(Num)).
nextStep(0) :- asserta(currentStep(0)). /* Non usata */

my_compile(X) :-
    telling(Old),
    tell('sliceCompiled.mc'),
    write('void compiledSlice() {'), nl,
    compE([], 0, X),
    write('}'),
    told,
    tell(Old).

cbop(eq, 'EQ').
cbop(ne, 'NE').
cbop(lt, 'LT').
cbop(le, 'LE').
cbop(ge, 'GE').
cbop(gt, 'GT').

cbop(plus,  'PLUS').
cbop(minus, 'MINUS').
cbop(or,    'OR').

cbop(prod, 'PROD').
cbop(div,  'DIV').
cbop(mod,  'MOD').
cbop(and,  'AND').

emit(Opc) :-
    write(' '), write(Opc), nl.

emit(Opc, X) :-
    write(' '), write(Opc), write('('), write(X), write(')'), nl.

emit(Opc, X, Y) :-
    write(' '), write(Opc), write('('), write(X), write(', '),
    write(Y), write(')'), nl.

emitLab(L) :-
    write(L), write(': ;'), nl.

compE(Env, N, i(X)) :-
    getEnv(Env, X, M, Offset),
    NLinks is N-M,
    emit('VAR', NLinks, Offset).

compE(_, _, tt) :-
    emit('PUSH', 1).

compE(_, _, ff) :-
    emit('PUSH', 0).

compE(_, _, X) :-
    integer(X), emit('PUSH', X).

compE(Env, N, let(D, E)) :-
    dDV(D, V),
    M is N+1,
    assign(V, M, Env1),
    length(V, L),
    emit('FRAME', L, -1),
    compD(Env, N, Env1, D, Env2),
    compE(Env2, M, E),
    emit('DROP', 1).

compE(Env, N, X) :-
    X =.. [O, E1, E2], cbop(O, M),
    compE(Env, N, E1),
    compE(Env, N, E2), emit(M).

compE(Env, N, if(E, E1, E2)) :-
     newLabel(L1), newLabel(L2),
     compE(Env, N, E),
     emit('GOFALSE', L1),
     compE(Env, N, E1),
     emit('GO', L2),
     emitLab(L1),
     compE(Env, N, E2),
     emitLab(L2).

newLabel(L) :-
    newSymbol(l, L).

newSymbol(Root, S) :-
    retract(currentSymbolNum(Root, Num1)), !,
    Num is Num1+1,
    asserta(currentSymbolNum(Root, Num)),
    name(Num, LNum),
    name(Root, LRoot),
    append(LRoot, LNum, L),
    name(S, L).

newSymbol(Root, S) :-
    asserta(currentSymbolNum(Root, 0)),
    name(Root, LRoot),
    append(LRoot, [48], L),
    name(S, L).


getEnv([e(X, N, O)|_], X, N, O) :- !.
getEnv([_|T], X, N, O) :-
    getEnv(T, X, N, O).

assign(V, N, Env1) :-
    assign1(V, N, Env1, 0).

assign1([X | V], N, [e(X, N, Offset) | Env], Offset) :-
    Offset1 is Offset+1,
    assign1(V, N, Env, Offset1).
assign1([], _, [], _).

compD(Env, N, Env1, def(X, _, E), [e(X, N, Offset)]) :-
    compE(Env, N, E),
    getEnv(Env1, X, M, Offset),
    emit('STORE', M, Offset).

compD(Env, N, Env1, seq(D1, D2), Env2) :-
    compD(Env, N, Env1, D1, Env3),
    compose(Env, Env3, Env4),
    compD(Env4, N, Env1, D2, Env5),
    compose(Env3, Env5, Env2).
