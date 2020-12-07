% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learning data from Figure 11.2 (Boolean attributes)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%%%%%%%%%%%%%%%%%%
%  DATA
%%%%%%%%%%%%%%%%%%

% val(Object,Attribute,Value) - here is the data
val(e1,reads,false).   val(e1,known,true).  val(e1,new,true). 
     val(e1,short,false). val(e1,home,true ).
val(e2,reads,true).   val(e2,known,false). val(e2,new,true).  
     val(e2,short,true). val(e2,home,false).
val(e3,reads,false).  val(e3,known,false). val(e3,new,false). 
     val(e3,short,false). val(e3,home,false).
val(e4,reads,false).   val(e4,known,true).  val(e4,new,false). 
     val(e4,short,false). val(e4,home, true).
val(e5,reads,true).   val(e5,known,true).  val(e5,new,true). 
     val(e5,short,true). val(e5,home, true).
val(e6,reads,false).   val(e6,known,true).  val(e6,new,false). 
     val(e6,short,false). val(e6,home,false).
val(e7,reads,false).  val(e7,known,false). val(e7,new,false). 
     val(e7,short,true). val(e7,home,false).
val(e8,reads,true).   val(e8,known,false). val(e8,new,true). 
     val(e8,short,true). val(e8,home,false).
val(e9,reads,false).   val(e9,known,true).  val(e9,new,false). 
     val(e9,short,false). val(e9,home,true ).
val(e10,reads,false).  val(e10,known,true). val(e10,new,true). 
     val(e10,short,false). val(e10,home,false).
val(e11,reads,false). val(e11,known,false). val(e11,new,false). 
     val(e11,short,true). val(e11,home,true ).
val(e12,reads,false).  val(e12,known,true). val(e12,new,true). 
     val(e12,short,false). val(e12,home,false).
val(e13,reads,true).  val(e13,known,true). val(e13,new,false). 
     val(e13,short,true). val(e13,home,true ).
val(e14,reads,true).  val(e14,known,true). val(e14,new,true). 
     val(e14,short,true). val(e14,home,false).
val(e15,reads,true).  val(e15,known,true). val(e15,new,true). 
     val(e15,short,true). val(e15,home,true).
val(e16,reads,true).  val(e16,known,true). val(e16,new,false). 
     val(e16,short,true). val(e16,home,false).
val(e17,reads,true).  val(e17,known,true). val(e17,new,true). 
     val(e17,short,true). val(e17,home,true).
val(e18,reads,true).  val(e18,known,false). val(e18,new,true). 
     val(e18,short,true). val(e18,home,false).

% 

pre(Obj,reads,V) :-
     val(Obj,known,true), val(Obj,short,V);
     val(Obj,known,false), val(Obj,new,V).

% dtlearn(reads, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18], [known,new,short,home], DT).
