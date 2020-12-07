% File READAT2.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix B

% Ready-to-run version of read_atomics/1 for Prologs in which
% name/2 does not handle numbers correctly.  See text.


% read_atomics(-Atomics)
%  Reads a line of text, breaking it into a
%  list of atomic terms: [this,is,an,example].

read_atomics(Atomics) :-
   read_char(FirstC,FirstT),
   complete_line(FirstC,FirstT,Atomics).


% read_char(-Char,-Type)
%  Reads a character and runs it through char_type/1.

read_char(Char,Type) :-
   get0(C),
   char_type(C,Type,Char).


% complete_line(+FirstC,+FirstT,-Atomics)
%  Given FirstC (the first character) and FirstT (its type), reads
%  and tokenizes the rest of the line into atoms and numbers.

complete_line(_,end,[]) :- !.                  % stop at end

complete_line(_,blank,Atomics) :-              % skip blanks
   !,
   read_atomics(Atomics).

complete_line(FirstC,special,[A|Atomics]) :-   % special char
   !,
   name(A,[FirstC]),
   read_atomics(Atomics).

complete_line(FirstC,alpha,[A|Atomics]) :-     % begin word
   complete_word(FirstC,alpha,Word,NextC,NextT),
   name_num(A,Word),
   complete_line(NextC,NextT,Atomics).


% complete_word(+FirstC,+FirstT,-List,-FollC,-FollT)
%  Given FirstC (the first character) and FirstT (its type),
%  reads the rest of a word, putting its characters into List.

complete_word(FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   read_char(NextC,NextT),
   complete_word(NextC,NextT,List,FollC,FollT).

complete_word(FirstC,FirstT,[],FirstC,FirstT).
   % where FirstT is not alpha


% char_type(+Code,?Type,-NewCode)
%  Given an ASCII code, classifies the character as
%  'end' (of line/file), 'blank', 'alpha'(numeric), or 'special',
%  and changes it to a potentially different character (NewCode).

char_type(10,end,10) :- !.         % UNIX end of line mark
char_type(13,end,13) :- !.         % DOS end of line mark
char_type(-1,end,-1) :- !.         % get0 end of file code

char_type(Code,blank,32) :-        % blanks, other ctrl codes
  Code =< 32,
  !.

char_type(Code,alpha,Code) :-      % digits
  48 =< Code, Code =< 57,
  !.

char_type(Code,alpha,Code) :-      % lower-case letters

  97 =< Code, Code =< 122,
  !.

char_type(Code,alpha,NewCode) :-   % upper-case letters
  65 =< Code, Code =< 90,
  !,
  NewCode is Code + 32.            %  (translate to lower case)

char_type(Code,special,Code).      % all others


% name_num(-AtomOrNumber,+String)
%  Used in place of name/2 in last clause of complete_line
%  in versions of Prolog where name/2 does not recognize numbers.

name_num(Number,String) :-
  nonvar(String),
  string_number(String,Number),
  !.

name_num(Atom,String) :-
  name(Atom,String).


% string_number(+S,-N)
%  Converts string to corresponding number, e.g. "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_number(S,N) :-
   string_number_aux(S,0,N).

string_number_aux([D|Digits],Total,Result) :-
   digit_value(D,V),
   NewTotal is 10*Total + V,
   string_number_aux(Digits,NewTotal,Result).

string_number_aux([],Result,Result).


% digit_value(+Code,-Value)
%  Maps ASCII code for a digit ("0"..."9") onto value (0...9).

digit_value(Code,Value) :-
   48 =< Code, Code =< 57,
   Value is Code - 48.

