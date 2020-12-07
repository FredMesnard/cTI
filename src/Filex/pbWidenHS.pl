/*

sur cet exemple, il me semble qu'il y a un pb avec le
widening propose dans la these de Saglam :
- ou bien son widening est foireux
- ou bien mon codage est erronne
- ou bien il y a bug ailleurs

C'est pour ca que dans itp pour CLP(Q),
je fais :
8 fois max le widening de Saglam
(car bonne qualite),
puis widening standard
(pour terminaison)

*/

p(X,E,E).      % si on enleve cette clause, ca termine
p(X,E,[X|E]).  % si on enleve cette clause, ca termine. Si p(X,E,[a|E]) a la place, ca termine
p(X,E1,E2):-cti:{n(Y)=3+n(X1)+n(X2),n(Y)>=n(X),2*n(X)+1>=n(Y)},p(X1,E1,E3),p(X2,E3,E2).



