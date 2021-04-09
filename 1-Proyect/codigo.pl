%:- module(_,_,['bf/bfall']).
%:- module(_,_,[classic]).
:- module(_,_,[assertions,regtypes]).

alumno_prode('Serrano', 'Arrese', 'Francisco Javier', 'a180487').

% Parte 1
% Dado un natural N par, construir dos listas de longitud N/2, cuyos elementos
% sumen lo mismo.

% 1.1 -> Definir un predicado nums(N, L) tal que N es un numero natural y L es
%   la lista de numeros en orden decendente de N a 1.
%
% My Approach: he decidido restar la lista (1, 2, ..., N) a N en cada elemento
% de la lista solucion, de esta manera, la lista queda invertida respecto a la
% llamada a la funcion between(1, N, Res).
nums(0,[]).
nums(s(N),[s(N)|L]) :- 
  nat(N),
	nums(N,L).
% 1.2 -> Definir un predicado sumlist(L, S) tal que L es una lista de numeros
%   naturales y S es la suma de todos los elementos de L.
%
% My Approach: separar la lista en Head y Tail, llamar recursivamente a sum/2
% con la Tail y la suma actual, hasta que se llega a una lista de size ==
% 1 y se devuelve el valor del unico elemento de dicha lista.
sumlist([X], X).

sumlist([H|T], S) :-
  sumlist(T, X), suma(H, X, S).

% 1.3 -> Definir un predicado choose_one(E, L, R) tal que L es una lista, E es
%   un elemento cualquiera de L, y R es lo que queda de la lista, despues de
%   quitar E. Dada una lista L, una llamada a choose_one/3 debe devolver en E,
%   como alternativas al pedir mas soluciones, sucesivamente todos los
%   elementos de E, y en R todos los restos.
%
% My approach: this is my approach.
choose_one(A, [A|B], B).
choose_one(A, [B, C|D], [B|E]) :-
  choose_one(A, [C|D], E).  

% 1.4 -> Usando el predicado choose_one/3, escribir el predicado perm(L, LP),
%   tal que L es una lista y LP es una permutacion de L (es decir, una lista
%   con los mismos elementos que L, en distinto orden). Por ejemplo, dada una
%   lista L, perm(L, LP) debe generar como alternativas en LP todas las
%   permutaciones de L.
%
% My approach: meto la lista en la funcion choose_one y el elemento E, lo
% introduzco al inicio de la lista LP. Recursivamente voy llamando a choose_one
% usando de lista el resto de la operacion anterior hasta que consigo todas
% las combinaciones de tamanyo length(L) de forma ordenada.
perm([L], [L]).

perm(L, LP) :-
  choose_one(E, L, R),
  perm(R, X), my_append([E], X, LP).

% 1.5 -> Definir un predicado split(L, L1, L2) tal que L es una lista de
%   longitud N, N es par, L1 contiene los N/2 elementos en posicion impar de
%   L y L2 los en posicion par. Es decir: split([a,b,c,d], X, Y) devolveria
%   X = [a,c], Y = [b, d].
%
%   My approach: desde split llamo a la funcion auxiliar impar al ser el primer
%   elemento el elemento 1, aqui extraigo el elemento Head (H) de la lista
%   y llamo a la funcion auxiliar par, la cual hace exactamente lo mismo que
%   impar pero una iteracion despues. La ejecucion finaliza cuando la lista
%   List (primer parametro de entrada) queda vacia.
split(List,Impar,Par):-impar(List,Impar,Par).

impar([H|T],[H|Impar],Par):-par(T,Impar,Par).
impar([],[],[]).

par([H|T],Impar,[H|Par]):-impar(T,Impar,Par).       
par([],[],[]). 

% 1.6 -> Para completar nuestro primer objetivo, esctibir, usando los
%   predicados anteriores, un predicado sumlists(N,L1,L2,S) tal que N es par,
%   L1 y L2 son dos listas de longitud N/2, que contienen entre ellas todos los
%   numeros de Peano de 1 a N, y L1 y L2 suman lo mismo. S debe ser el valor de
%   dicha suma.
sumlists(N, L1, L2, S) :-
  nums(N, L),
  split(L, LODD, LEVEN),
  my_reverse(LEVEN, RLEVEN),
  merge_lists(LODD, RLEVEN, L1, L2),
  sumlist(L1, S).

merge_lists(ODD,REVEN,L1,L2):-merge_impar(ODD,REVEN,L1,L2).

merge_impar([H1|T1],[H2|T2],[H1,H2|L1],L2):-merge_par(T1,T2,L1,L2).
merge_impar([],[],[],[]).

merge_par([H1|T1],[H2|T2],L1,[H1,H2|L2]):-merge_impar(T1,T2,L1,L2).
merge_par([],[],[],[]).
%%%%%%%%%%%%%%%%%%%%%%
%%METODOS AUXILIARES%%
%%%%%%%%%%%%%%%%%%%%%%
% my_append/3 -> Concatenates two lists in a third one:
my_append([],Ys ,Ys).
my_append([X|Xs],Ys ,[X|Zs]) :- my_append(Xs ,Ys ,Zs).
% igual/2 -> asignamos a la variable el valor del parametro introducido en el
% otro termino:
igual(X, X).
% nat/1 -> comprobamos de forma recursiva que un valor es natural
nat(0).
nat(s(X)) :- nat(X).
% es_par/1 -> comprobamos de forma recursiva que el parametro tiene un valor de
% 2*k superior a 0, para k siendo un natural:
es_par(0).
es_par(s(s(X))) :- es_par(X).
% suma/3 -> asignmaos al tercer parametro la suma de los dos primeros de tal
% manera que se cumple la igualdad establecida:
suma(0,X,X).
suma(s(X),Y,s(Z)) :- suma(X,Y,Z).
% longitud/2 -> eliminamos recursivamente los elementos Head de la lista pasada
% como primer parametro y sumamos el sucesor en cada iteracion al segundo:
longitud([],0).
longitud([_|T],s(N)) :- longitud(T,N).
% miembro/2 -> recorremos recursivamente la lista pasada como segundo parametro
% y si el elemento Head de esta coicide con el primer parametro, es true:
%miembro(X,[X|Xs]).
%miembro(X,[Y|Ys]) :- miembro(X,Ys).
% eliminar/3 -> recorremos la lista pasada como segundo parametro y si el
% elemeto pasado como primero coincide, se elimina este de la lista, de la cual
% se copia el resto en el tercer parametro, que es el resultado:
eliminar(X,[X|Xs], Xs).
eliminar(X, [Y|Ys], [Y|Zs]):- eliminar(X,Ys,Zs).
% my_reverse/2 -> se introduce en el primer parametro la lista que se desea
% invertir (en cuanto a posicion) y se devuelve en el segundo parametro:
my_reverse(L1,L2) :- my_rev(L1,L2,[]).

my_rev([],L2,L2) :- !.
my_rev([X|Xs],L2,Acc) :- my_rev(Xs,L2,[X|Acc]).
%%%%%%%%%%%%%%%%%%%%%%%% Funciones Auxiliares %%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% nums(N,L) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%nums(0,[]).
%nums(s(N),[s(N)|L]) :- 
%	nat(N),
%	nums(N,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% sumlist %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sumlist([],0). 				%antes estab al reves y no funcionab orque m no tenia ningun 						%valor iniclializado, pero al estar asi primero llega acer se 						%iguala y ya M tiene le valor 0 joder

%sumlist([H|T],S) :-
%	sumlist(T,M),
%	suma(H,M,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%









