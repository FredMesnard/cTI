% Computational Intelligence: a logical approach. 
% Prolog Code.
% Neural-network learning data from Figure 11.2 (two hidden units)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% prop(Object,Attribute,Value) - here is the data
prop(e1,reads,1) <- true. 
prop(e1,author,1) <- true. 
prop(e1,thread,1) <- true. 
prop(e1,length,0) <- true. 

prop(e2,reads,1) <- true. 
prop(e2,author,0) <- true. 
prop(e2,thread,1) <- true. 
prop(e2,length,1) <- true. 

prop(e3,reads,0) <- true. 
prop(e3,author,0) <- true. 
prop(e3,thread,0) <- true. 
prop(e3,length,0) <- true. 

prop(e4,reads,1) <- true. 
prop(e4,author,1) <- true. 
prop(e4,thread,0) <- true. 
prop(e4,length,0) <- true. 

prop(e5,reads,1) <- true. 
prop(e5,author,1) <- true. 
prop(e5,thread,1) <- true. 
prop(e5,length,1) <- true. 

prop(e6,reads,1) <- true. 
prop(e6,author,1) <- true. 
prop(e6,thread,0) <- true. 
prop(e6,length,0) <- true. 

prop(e7,reads,0) <- true. 
prop(e7,author,0) <- true. 
prop(e7,thread,0) <- true. 
prop(e7,length,1) <- true. 

prop(e8,reads,1) <- true. 
prop(e8,author,0) <- true. 
prop(e8,thread,1) <- true. 
prop(e8,length,1) <- true. 

prop(e9,reads,1) <- true. 
prop(e9,author,1) <- true. 
prop(e9,thread,0) <- true. 
prop(e9,length,0) <- true. 

prop(e10,reads,1) <- true. 
prop(e10,author,1) <- true. 
prop(e10,thread,1) <- true. 
prop(e10,length,0) <- true. 

prop(e11,reads,0) <- true. 
prop(e11,author,0) <- true. 
prop(e11,thread,0) <- true. 
prop(e11,length,1) <- true. 

prop(e12,reads,1) <- true. 
prop(e12,author,1) <- true. 
prop(e12,thread,1) <- true. 
prop(e12,length,0) <- true. 

prop(e13,reads,1) <- true. 
prop(e13,author,1) <- true. 
prop(e13,thread,0) <- true. 
prop(e13,length,1) <- true. 

prop(e14,reads,1) <- true. 
prop(e14,author,1) <- true. 
prop(e14,thread,1) <- true. 
prop(e14,length,1) <- true. 


predicted_prop(Obj,reads,V) <-
   prop(Obj,h_1,I_1) &
   prop(Obj,h_2,I_2) &
   V is sigmoid(w_0+w_1* I_1+w_2* I_2).
prop(Obj,h_1,V) <-
   prop(Obj,author,I_1) &
   prop(Obj,thread,I_2) &
   prop(Obj,length,I_3) &
   V is sigmoid(w_3+w_4* I_1+w_5* I_2+w_6* I_3).
prop(Obj,h_2,V) <-
   prop(Obj,author,I_1) &
   prop(Obj,thread,I_2) &
   prop(Obj,length,I_3) &
   V is sigmoid(w_7+w_8* I_1+w_9* I_2+w_10* I_3).

error(Obj,E) <-
   predicted_prop(Obj,reads,VC) &
   prop(Obj,reads,V) &
   E is (VC-V)*(VC-V).

%  Example Query:
%?nnlearn(50,0.01,1.0,[e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14],[val(w_0,0.2),val(w_1,0.12),val(w_2,0.112),val(w_3,0.22),val(w_4,0.23),val(w_5,0.26),val(w_6,0.27),val(w_7,0.211),val(w_8,0.245),val(w_9,0.152),val(w_10,0.102)],P1).

% The following demonstrates a parameter setting with low error
%?total_error([e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14],[val(w_0,-300),val(w_1,300),val(w_2,300),val(w_3,-30),val(w_4,300),val(w_5,300),val(w_6,-0.25),val(w_7,-30),val(w_8,300),val(w_9,300),val(w_10,-0.3)],0,E).
