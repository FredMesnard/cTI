% Computational Intelligence: a logical approach. 
% Prolog Code.
% Neural-network learning data from Figure 11.2 (two hidden units)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%%%%%%%%%%%%%%%%%%
% DATA
%%%%%%%%%%%%%%%%%%
% prop(Object,Attribute,Value) - here is the data
prop(e1,reads,0) <- true.
prop(e1,known,1) <- true.
prop(e1,new,1) <- true.
prop(e1,short,0) <- true.
prop(e1,home,1) <- true.

prop(e2,reads,1) <- true.
prop(e2,known,0) <- true.
prop(e2,new,1) <- true.
prop(e2,short,1) <- true.
prop(e2,home,0) <- true.

prop(e3,reads,0) <- true.
prop(e3,known,0) <- true.
prop(e3,new,0) <- true.
prop(e3,short,0) <- true.
prop(e3,home,0) <- true.

prop(e4,reads,0) <- true.
prop(e4,known,1) <- true.
prop(e4,new,0) <- true.
prop(e4,short,0) <- true.
prop(e4,home,1) <- true.

prop(e5,reads,1) <- true.
prop(e5,known,1) <- true.
prop(e5,new,1) <- true.
prop(e5,short,1) <- true.
prop(e5,home,1) <- true.

prop(e6,reads,0) <- true.
prop(e6,known,1) <- true.
prop(e6,new,0) <- true.
prop(e6,short,0) <- true.
prop(e6,home,0) <- true.

prop(e7,reads,0) <- true.
prop(e7,known,0) <- true.
prop(e7,new,0) <- true.
prop(e7,short,1) <- true.
prop(e7,home,0) <- true.

prop(e8,reads,1) <- true.
prop(e8,known,0) <- true.
prop(e8,new,1) <- true.
prop(e8,short,1) <- true.
prop(e8,home,0) <- true.

prop(e9,reads,0) <- true.
prop(e9,known,1) <- true.
prop(e9,new,0) <- true.
prop(e9,short,0) <- true.
prop(e9,home,1) <- true.

prop(e10,reads,0) <- true.
prop(e10,known,1) <- true.
prop(e10,new,1) <- true.
prop(e10,short,0) <- true.
prop(e10,home,0) <- true.

prop(e11,reads,0) <- true.
prop(e11,known,0) <- true.
prop(e11,new,0) <- true.
prop(e11,short,1) <- true.
prop(e11,home,1) <- true.

prop(e12,reads,0) <- true.
prop(e12,known,1) <- true.
prop(e12,new,1) <- true.
prop(e12,short,0) <- true.
prop(e12,home,0) <- true.

prop(e13,reads,1) <- true.
prop(e13,known,1) <- true.
prop(e13,new,0) <- true.
prop(e13,short,1) <- true.
prop(e13,home,1) <- true.

prop(e14,reads,1) <- true.
prop(e14,known,1) <- true.
prop(e14,new,1) <- true.
prop(e14,short,1) <- true.
prop(e14,home,0) <- true.

prop(e15,reads,1) <- true.
prop(e15,known,1) <- true.
prop(e15,new,1) <- true.
prop(e15,short,1) <- true.
prop(e15,home,1) <- true.

prop(e16,reads,1) <- true.
prop(e16,known,1) <- true.
prop(e16,new,0) <- true.
prop(e16,short,1) <- true.
prop(e16,home,0) <- true.

prop(e17,reads,1) <- true.
prop(e17,known,1) <- true.
prop(e17,new,1) <- true.
prop(e17,short,1) <- true.
prop(e17,home,1) <- true.

prop(e18,reads,1) <- true.
prop(e18,known,0) <- true.
prop(e18,new,1) <- true.
prop(e18,short,1) <- true.
prop(e18,home,0) <- true.

% Neural network with two hidden units
predicted_prop(Obj,reads,V) <-
   prop(Obj,h_1,I_1) &
   prop(Obj,h_2,I_2) &
   V is sigmoid(w_0+w_1* I_1+w_2* I_2).
%   V is w_0+w_1* I_1+w_2* I_2.        % linear threshold
prop(Obj,h_1,V) <-
   prop(Obj,known,I_1) &
   prop(Obj,new,I_2) &
   prop(Obj,short,I_3) &
   prop(Obj,home,I_4) &
   V is sigmoid(w_3+w_4* I_1+w_5* I_2+w_6* I_3+ w_7*I_4).
prop(Obj,h_2,V) <-
   prop(Obj,known,I_1) &
   prop(Obj,new,I_2) &
   prop(Obj,short,I_3) &
   prop(Obj,home,I_4) &
   V is sigmoid(w_8+w_9* I_1+w_10* I_2+w_11* I_3+w_12*I_4).
error(Obj,E) <-
   predicted_prop(Obj,reads,VC) &
   prop(Obj,reads,V) &
   E is (VC-V)*(VC-V).

% Example Query:
%?nnlearn(80,0.001,0.5,[e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18],[val(w_0,0.2),val(w_1,0.12),val(w_2,0.112),val(w_3,0.22),val(w_4,0.23),val(w_5,0.26),val(w_6,0.27),val(w_7,0.211),val(w_8,0.245),val(w_9,0.152),val(w_10,0.102),val(w_11,0.105),val(w_12,0.205)],P1).

% The following queries change the learning rate:
%?nnlearn(200,0.001,0.1,[e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18],[val(w_0,0.2),val(w_1,0.12),val(w_2,0.112),val(w_3,0.22),val(w_4,0.23),val(w_5,0.26),val(w_6,0.27),val(w_7,0.211),val(w_8,0.245),val(w_9,0.152),val(w_10,0.102),val(w_11,0.105),val(w_12,0.205)],P1).
%?nnlearn(100,0.001,1.0,[e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18],[val(w_0,0.2),val(w_1,0.12),val(w_2,0.112),val(w_3,0.22),val(w_4,0.23),val(w_5,0.26),val(w_6,0.27),val(w_7,0.211),val(w_8,0.245),val(w_9,0.152),val(w_10,0.102),val(w_11,0.105),val(w_12,0.205)],P1).
%?nnlearn(100,0.001,5.0,[e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18],[val(w_0,0.2),val(w_1,0.12),val(w_2,0.112),val(w_3,0.22),val(w_4,0.23),val(w_5,0.26),val(w_6,0.27),val(w_7,0.211),val(w_8,0.245),val(w_9,0.152),val(w_10,0.102),val(w_11,0.105),val(w_12,0.205)],P1).

% New case with unknown user action:
prop(e19,known,0) <- true.
prop(e19,new,1) <- true.
prop(e19,short,0) <- true.
prop(e19,home,0) <- true.

% Example query, for the new case:
%?pprove(predicted_prop(e19,reads,V),[val(w_0,-3.128),val(w_1,7.462),val(w_2,-2.295),val(w_3,-5.615),val(w_4,2.0867),val(w_5,2.006468),val(w_6,4.942),val(w_7,-0.2936),val(w_8,0.64746),val(w_9,-1.063),val(w_10,-1.1199),val(w_11,-0.7881),val(w_12,0.1345)]).
