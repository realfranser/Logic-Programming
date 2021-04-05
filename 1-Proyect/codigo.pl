:- module(_,_,['bf/bfall']).

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
nums(N, L) :-
  between(0, N-1, Y),
  L is N - Y.

% 1.2 -> Definir un predicado sumlist(L, S) tal que L es una lista de numeros
%   naturales y S es la suma de todos los elementos de L.
%
% My Approach: separar la lista en Head y Tail, llamar recursivamente a sum/2
% con la Tail y la suma actual, hasta que se llega a una lista de size ==
% 1 y se devuelve el valor del unico elemento de dicha lista.
sum([X], X).

sum([H|T], S) :-
  sum(T, X), S is H + X.

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
  perm(R, X), LP = [E|X].

% 1.5 -> Definir un predicado split(L, L1, L2) tal que L es una lista de
%   longitud N, N es par, L1 contiene los N/2 elementos en posicion impar de
%   L y L2 los en posicion par. Es decir: split([a,b,c,d], X, Y) devolveria
%   X = [a,c], Y = [b, d].
%
%   My approach: this is my approach
split([], L1, L2).

split([H1, H2|L], L1, L2) :-
  split(L, [L1|H1], [L2|H2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%AUXILIARES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between/3 -> Returns a list of numbers between N and M in K:
between(N, M, K) :- N =< M, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).
% append/3 -> Concatenates two lists in a third one:
append([],Ys ,Ys).
append([X|Xs],Ys ,[X|Zs]) :- append(Xs ,Ys ,Zs).
