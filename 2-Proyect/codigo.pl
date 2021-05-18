:- module(_, _, [classic,assertions]).

:- use_module(library(iso_misc)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(assoc)).


alumno_prode('Serrano','Arrese','Francisco Javier','A180487').

% Preliminares

/* Comprimir
comprimir(Inicial, Comprimida) :-
  limpia_memo,
  compresion_recursiva(Inicial, Comprimida).
*/

limpia_memo.

:- dynamic
%compresion_map/2.
    found_result/1.

  %store_list(Key, Value) :- assert(compresion_map(Key, Value)).
store_result(X) :- assert(found_result(X)).


compresion_recursiva(Inicial, Comprimido) :-
  retractall(found_result(_)),
  %mejor_compresion(Inicial, Comprimido).
  get_all_compresions(Inicial, Comprimido).

mejor_compresion(Inicial, Comprimido) :-
  get_all_compresions(Inicial, _),
  findall(Comp, found_result(Comp), Comprimido).


get_all_compresions(Inicial, Comprimido) :-
  sub_compresion_recursiva(Inicial, Comprimido),
  ( found_result(Comprimido) ->
    fail
  ;
    store_result(Comprimido)
  ).

  %compresion_map(Inicial, Comprimido).
  %repeticion(Inicial, Comprimido).
  %division(Inicial, Comprimido).

% No compresion posible:


sub_compresion_recursiva(Inicial, Comprimido) :-
  comprimir(Inicial, Comprimido).

sub_compresion_recursiva(Inicial, Inicial).

% Partir
/* Verifica que Parte1 y Parte2 son dos secuencias que concatenadas forman Todo */
partir(Todo, Parte1, Parte2) :-
  Parte1 = [_|_],
  Parte2 = [_|_],
  append(Parte1, Parte2, Todo).

% Parentesis
/* Params:
 *  - ParteNum: lista completa de caracteres
 *  - Parte: lista parcial de caracteres
 *  - Num: numero de repeticiones de lista parcial
 *
 * Note: si parte tiene 2 o mas elementos, add parentesis
*/

parentesis(Parte, Num, ParteNum) :-
  number(Num),
  length(Parte, 1),
  !, % If the length of the list is 1, cut for not entering the other literal
  append(Parte, [Num], ParteNum).

parentesis(Parte, Num, ParteNum) :-
  number(Num),
  append(['('],Parte,ParteAux),
  append(ParteAux,[')',Num],ParteNum).

% Se repite
/* Params:
 *  - Cs: lista completa de caracteres
 *  - Parte: lista parcial de caracteres
 *  - Num0: contador de repeticiones
 *  - Num: veces que Parte se repite en Cs
 *
 * Note: si no se puede obtener con repeticiones, es false.
 *       si es lista vacia, Num sera 0 y true
*/
se_repite([], _, Num0, Num0).

se_repite(Cs, Parte, Num0, Num) :-
  append(Parte,X,Cs),
  Num1 is Num0 + 1,
  se_repite(X,Parte,Num1,Num).

% Repeticion:
% Basarse en partir/3 y se_repite/4, indentificar un prefijo (una parte) que
% nos de por repeticion la secuencia inicial. Antes de seguir, esta parte hay
% que comprimirla de forma recursiva mediante una llamada
% a compresion_recursiva/2. Finalmente componer la parte (comprimida
% recursivamente) con el numero de repeticiones usando el predicado
% parentesis/3.
%
% Estar atento a los tests por si devuelve una solucion de mas puede que haga
% falta meter cut
repeticion(Inicial, Comprimida) :-
  partir(Inicial, Parte1, _),
  se_repite(Inicial, Parte1, 0, Num),
  %compresion_recursiva(Parte1,X),
  sub_compresion_recursiva(Parte1,X),
  parentesis(X, Num, Comprimida).


% Compresion:
% Obtener todas las repeticiones optimas o no optimas repitiendo
% o dividiendo.
% compresion/2: tendra dos alternativas, llamar al predicado repeticion/2 ya
% implementado o a un nuevo predicado division/2. El predicado division/2 debe
% partir la lista inicial en dos partes y llamar a compresion_recursiva/2 de
% forma recursiva para finalmente concatenar los resultados.
%
% Es decir, ademas de considerar las repeticiones, podremos dividir la lista
% inicial en dos partes y aplecar el algoritmao a cada una de ellas por
% separado (dnado mas posibilidades a encontrar repeticiones).

comprimir([X|[]], [X]) :- !.

comprimir(Inicial, Comprimida) :-
  division(Inicial, Comprimida).

comprimir(Inicial, Comprimida) :-
  repeticion(Inicial, Comprimida).


% Division:
% Parte la lista inicial en dos partes y llama a compresion_recursiva/2 de
% forma recursiva apara finalmente concatenar los resultados.
division(Inicial, Comprimida) :-
  partir(Inicial, X, Y),
  %compresion_recursiva(X, X1),
  %compresion_recursiva(Y, Y1),
  sub_compresion_recursiva(X, X1),
  sub_compresion_recursiva(Y, Y1),
  append(X1, Y1, Comprimida).

  

%
%%
%%%
%%
%

find_shortest_length([X|[]], X) :- !.

find_shortest_length([X|Y], List) :-
  length(X, Current),
  length(List, Min),
  ( Current <  Min ->
    find_shortest_length(Y, X)
  ;
    find_shortest_length(Y, List)
  ).

pax(1, "Hola").
pax(2, "Adios").
pax(3, "Luego").

%order_by([asc(X)],pax(X,G)).



get_length([], 0).

get_length([_|Y], N) :-
  get_length(Y, N1),
  N is N1 +1.

head([H|_], H).




















