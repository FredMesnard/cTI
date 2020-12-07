%%%%%%%%%%%%%%%%%%
%  DATA
%%%%%%%%%%%%%%%%%%

% val(Object,Attribute,Value) - here is the data
val(e1,at1,true).   val(e1,at2,true). val(e1,at3,true). val(e1,at4,false). 
val(e2,at1,true).   val(e2,at2,false). val(e2,at3,true). val(e2,at4,true). 
val(e3,at1,false).  val(e3,at2,false). val(e3,at3,false). val(e3,at4,false). 
val(e4,at1,true).   val(e4,at2,true). val(e4,at3,false). val(e4,at4,false). 
val(e5,at1,true).   val(e5,at2,true). val(e5,at3,true). val(e5,at4,true). 
val(e6,at1,true).   val(e6,at2,true). val(e6,at3,false). val(e6,at4,false). 
val(e7,at1,false).  val(e7,at2,false). val(e7,at3,false). val(e7,at4,true). 
val(e8,at1,true).   val(e8,at2,false). val(e8,at3,true). val(e8,at4,true). 
val(e9,at1,true).   val(e9,at2,true). val(e9,at3,false). val(e9,at4,false). 
val(e10,at1,true).  val(e10,at2,true). val(e10,at3,true). val(e10,at4,false).
val(e11,at1,false). val(e11,at2,false). val(e11,at3,false). val(e11,at4,true). 
val(e12,at1,true).  val(e12,at2,true). val(e12,at3,true). val(e12,at4,false). 
val(e13,at1,true).  val(e13,at2,true). val(e13,at3,false). val(e13,at4,true). 
val(e14,at1,true).  val(e14,at2,true). val(e14,at3,true). val(e14,at4,true). 

% NB no data for att1 when att2,3,4 are ftt


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE QUERIES for given data %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ? dtlearn(at1, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at2,at3,at4], DT).

% does the order of the attributes change the answer?
% ? dtlearn(at1, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at4,at3,at2], DT).

% att2 is not a function of the other attributes (e.g., when other attributes 
% are all true). Can it handle this case? 
% dtlearn(at2, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at4,at3,at1], DT).

% dtlearn(at3, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at4,at2,at1], DT).

% dtlearn(at4, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at3,at2,at1], DT).

% dtlearn(at4, [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14], [at3,at2,at4,at1], DT).
