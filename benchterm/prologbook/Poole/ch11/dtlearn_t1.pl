% Computational Intelligence: a logical approach. 
% Prolog Code.
% Decision-tree learning data from Figure 11.2
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%%%%%%%%%%%%%%%%%%
%  DATA
%%%%%%%%%%%%%%%%%%

% val(Object,Attribute,Value) - here is the data
val(e1,user_action,skips).   val(e1,author,known).  val(e1,thread,new). 
     val(e1,length,long). val(e1,where_read,home ).
val(e2,user_action,reads).   val(e2,author,unknown). val(e2,thread,new).  
     val(e2,length,short). val(e2,where_read,work).
val(e3,user_action,skips).  val(e3,author,unknown). val(e3,thread,follow_up). 
     val(e3,length,long). val(e3,where_read,work).
val(e4,user_action,skips).   val(e4,author,known).  val(e4,thread,follow_up). 
     val(e4,length,long). val(e4,where_read,home).
val(e5,user_action,reads).   val(e5,author,known).  val(e5,thread,new). 
     val(e5,length,short). val(e5,where_read,home).
val(e6,user_action,skips).   val(e6,author,known).  val(e6,thread,follow_up). 
     val(e6,length,long). val(e6,where_read,work).
val(e7,user_action,skips).  val(e7,author,unknown). val(e7,thread,follow_up). 
     val(e7,length,short). val(e7,where_read,work).
val(e8,user_action,reads).   val(e8,author,unknown). val(e8,thread,new). 
     val(e8,length,short). val(e8,where_read,work).
val(e9,user_action,skips).   val(e9,author,known).  val(e9,thread,follow_up). 
     val(e9,length,long). val(e9,where_read,home ).
val(e10,user_action,skips).  val(e10,author,known). val(e10,thread,new). 
     val(e10,length,long). val(e10,where_read,work).
val(e11,user_action,skips). val(e11,author,unknown). val(e11,thread,follow_up). 
     val(e11,length,short). val(e11,where_read,home ).
val(e12,user_action,skips).  val(e12,author,known). val(e12,thread,new). 
     val(e12,length,long). val(e12,where_read,work).
val(e13,user_action,reads).  val(e13,author,known). val(e13,thread,follow_up). 
     val(e13,length,short). val(e13,where_read,home ).
val(e14,user_action,reads).  val(e14,author,known). val(e14,thread,new). 
     val(e14,length,short). val(e14,where_read,work).
val(e15,user_action,reads).  val(e15,author,known). val(e15,thread,new). 
     val(e15,length,short). val(e15,where_read,home).
val(e16,user_action,reads).  val(e16,author,known). val(e16,thread,follow_up). 
     val(e16,length,short). val(e16,where_read,work).
val(e17,user_action,reads).  val(e17,author,known). val(e17,thread,new). 
     val(e17,length,short). val(e17,where_read,home).
val(e18,user_action,reads).  val(e18,author,unknown). val(e18,thread,new). 
     val(e18,length,short). val(e18,where_read,work).

% 

pre(Obj,reads,V) :-
     val(Obj,author,known), val(Obj,short,V);
     val(Obj,author,unknown), val(Obj,new,V).

% dtlearn(user_action, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18], [author,thread,length,where_read], DT).
