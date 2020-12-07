% Computational Intelligence: a logical approach. 
% Prolog Code. EBL Example 11.21
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Here is the example from the book

% Training example
% grade(St,Course,Mark) is true if student St achieved Mark in Course.
fact(grade(david,phys101,89)).
fact(grade(david,hist101,77)).
fact(grade(david,cs126,84)).
fact(grade(david,cs202,88)).
fact(grade(david,cs333,77)).
fact(grade(david,cs312,74)).
fact(grade(david,math302,41)).
fact(grade(david,math302,87)).
fact(grade(david,cs304,79)).
fact(grade(david,cs804,80)).
fact(grade(david,psyc303,85)).
fact(grade(david,stats324,91)).
fact(grade(david,cs405,77)).

% domain theory
1:: fulfilled_electives(St,SciDept) <-
     dept(SciDept,science)&
     has_arts_elective(St)&
     has_sci_elective(St,SciDept).

% has_arts_elective(St) is true if student St has passed an arts course 
2:: has_arts_elective(St) <-
     passed(St,ArtsEl,49)&
     course(ArtsEl,ArtsDept,_)&
     dept(ArtsDept,arts).

% has_sci_elective(St,Major) is true if St has over 59 in a third or fourth 
% year science course from a science department other than department Major.
3:: has_sci_elective(St,Major) <-
     passed(St,SciC,59)&
     course(SciC,Dept,Level)&
     dept(Dept,science)&
     diff_dept(Dept, Major)&
     member(Level,[third,fourth]).
4:: passed(St,C,MinPass) <-
     grade(St,C,Gr)&
     Gr > MinPass.
5:: member(X,[X|_]) <- true.
6:: member(X,[_|Y]) <- member(X,Y).
7:: dept(history,arts) <- true.
8:: dept(english,arts) <- true.
9:: dept(psychology,science) <- true.
10:: dept(statistics,science) <- true.
11:: dept(mathematics,science) <- true.
12:: dept(cs,science) <- true.
13:: course(hist101,history,first) <- true.
14:: course(psyc303,psychology,third) <- true.
15:: course(stats324,statistics,third) <- true.
16:: course(math302,mathematics,third) <- true.
17:: course(phys101,physics,first) <- true.
18:: diff_dept(psychology,cs) <- true.
19:: diff_dept(statistics,cs) <- true.
20:: diff_dept(mathematics,cs) <- true.

built_in((_>_)).

% Example query:
% ? ebl(fulfilled_electives(david,cs),H,[],B).
