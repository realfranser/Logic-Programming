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
choose_one(A, [A|B], B).
choose_one(A, [B, C|D], [B|E]) :-
  choose_one(A, [C|D], E).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%AUXILIARES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between/3 -> Returns a list of numbers between N and M in K:
between(N, M, K) :- N =< M, K = N.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).
