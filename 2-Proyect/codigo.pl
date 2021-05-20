:- module(_, _, [classic,assertions]).

:- use_module(library(iso_misc)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(assoc)).
:- use_module(library(librowser)).
:- use_module(library(assertions/native_props)).

alumno_prode('Serrano','Arrese','Francisco Javier','A180487').

% Preliminares

/* compresion
compresion(Inicial, Comprimida) :-
  limpia_memo,
  compresion_recursiva(Inicial, Comprimida).
*/

limpia_memo.

:- dynamic
%compresion_map/2.
    found_result/1,
    memo/2.

limpia_memo :- retractall(memo(_,_)).

  %store_list(Key, Value) :- assert(compresion_map(Key, Value)).
store_result(X) :- assert(found_result(X)).


compresion_recursiva(Inicial, Comprimido) :-
  retractall(found_result(_)),
  mejor_compresion(Inicial, Comprimido).
  %get_all_compresions(Inicial, Comprimido).

mejor_compresion_memo(Inicial, Comprimido) :-
  limpia_memo,
  mejor_compresion(Inicial, Comprimido),
  !.

comprimir(Inicial, Comprimido) :-
  retractall(found_result(_)),
  mejor_compresion(Inicial, Comprimido).

mejor_compresion(Inicial, Comprimido) :-
  findall(Y, get_all_compresions(Inicial, Y), Comp_List),
  sort(Comp_List, Sorted_List),
  head(Sorted_List, [_-Comprimido]).
  




get_all_compresions(Inicial, Comprimido) :-
  sub_compresion_recursiva(Inicial, New_Comp),
  ( found_result(New_Comp) ->
    fail
  ;
    store_result(New_Comp),
    length(New_Comp, CL),
    Comprimido = [CL-New_Comp]
  ).



put_length([Size], List, Res) :-
  append(Size, List, Res).


sub_compresion_recursiva(Inicial, Comprimido) :-
  compresion(Inicial, Comprimido).

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
% que compresionla de forma recursiva mediante una llamada
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
% inicial en dos partes y aplecar el algoritmo a cada una de ellas por
% separado (dnado mas posibilidades a encontrar repeticiones).

compresion([X|[]], [X]) :- !.

compresion(Inicial, Comprimida) :-
  division(Inicial, Comprimida).

compresion(Inicial, Comprimida) :-
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Documentacion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(title, "Compresion de Secuencias").
:- doc(author, "Francisco Javier Serrano Arrese 180487").

:- doc(module, "Compresion de Secuencias es un programa desarrollado con el lenguaje de programacion Ciao  Prolog. Este lenguaje de programacion forma parte del paradigma de la programacion logica el cual se estudia en la asignatura de Programacion Declarativa: Logica y restricciones de la ETSIINF UPM.

El proyecto consiste en la contruccion de un programa que permita comprimir secuencias de caracteres en secuencias que definan de forma mas compacta las mismas.
Así, la secuencia aaaaaaa se comprime en a7. La secuencia original es de longitud siete, la comprimida tiene solo longitud dos. La secuencia ababab, de longitud seis, se comprime en (ab)3, de longitud cinco. Nótese
que los paréntesis se cuentan también como caracteres de la secuencia. Solo hacen falta paréntesis si la subsecuencia que se repite es de más de un carácter. Para comprimir secuencias complejas,
se hace una division de tal manera que se pueda comprimir las partes divididas para luego juntarlas en el resultado final de la compresion. Por ejemplo, la secuencia aaaaaaa se comprime en la secuencia a7.
En la compresión por división se divide la secuencia original en dos partes que a su vez se comprimen por separado y se unen los resultados. Por ejemplo, la secuencia aaaaaaabbbbbbb se comprime en a7b7 (comprimiendo cada parte, a su vez, por
repetición). La secuencia aaabaaab se comprime por repetición en (a3b)2 donde la subsecuencia aaab se ha comprimido en a3b por división (y a su vez aaa en a3 por repetición).
Los resultados de la compresión (tanto de la secuencia original como de sus subsecuencias) han de ser más cortos que las secuencias iniciales. Así, no es admisible comprimir aa en a2, porque tienen la misma longitud, ni abab en (ab)2, porque esta última es más larga.
Las secuencias se representaran como listas de caracteres. Por ejemplo, aaa es [a,a,a] y (ab)3 es [’(’,a,b,’)’,3]. En estas listas los números ocupan una única posición, tengan el numero de dígitos que tengan. Así, a12 es la lista [a,12] y tiene longitud dos (no tres).

A continuacion, se detalla el codigo y los predicados utilizados para la resolucion de el enunciado anteriormente expuesto junto con un set de pruebas para asegurar el correcto funcionamiento del codigo.

@section{Predicados y explicacion de algoritmos}
@subsection{partir(Todo, Parte1, Parte2)}
@begin{verbatim}
:- pred partir(Todo, Parte1, Parte2)

Se verifica si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacias que concatenadas forman la secuencia @var{Todo}.@includedef{partir/3}
@end{verbatim}


@subsection{Explicacion partir(Todo, Parte1, Parte2)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Comprobacion de que @var{Parte1} no es una lista vacia.
  2) Comprobacion de que @var{Parte2} no es una lista vacia.
  3) Utilizar el predicado @var{append/2} para encontrar las listas que concatenadas formen la lista @var{Todo}.
@end{verbatim}

@subsection{parentesis(Parte, Num,  ParteNum)}
@begin{verbatim}
:- pred parentesis(Parte, Num, ParteNum)

Compone la lista de elementos @var{Parte} con el numero de repeticiones dados por la variable @var{Num} envolviendo a esta lista con parentesis solo si esta tiene 2 elementos o mas. @includedef{parentesis/3}
@end{verbatim}

@subsection{Explicacion parentesis(Parte, Num, ParteNum)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se checkea que @var{Num} sea efectivamente un numero entero positivo.
  2) Se comprueba el size de la lista @var{Parte} y se bifurca entre size = 1 o size > 1.
  3) Si @var{Parte} es de size 1, se hace un corte para no envolverla en parentesis y se concatena el valor de @var{Parte} con el numero de @var{Num} en @var{ParteNum}.
  4) Si @var{Parte} es de size > 1, se concatenan parentesis por delante y por detras de la lista en @var{ParteNum}.
@end{verbatim}

@subsection{se_repite(Cs, Parte, Num0, Num)}
@begin{verbatim}
:- pred se_repite(Cs, Parte, Num0, Num)

Se verifica si @var{Cs} se obtiene por repetir N veces la secuencia @var{Parte}. El argumento @var{Num} incrementa @var{Num0} en N. @includedef{se_repite/4}
@end{verbatim}

@subsection{Explicacion se_repite(Cs, Parte, Num0, Num)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se obtiene en @var{X} la lista que concatenada a @var{Parte} forma la lista completa @var{Cs}.
  2) Se incrementa en @var{Num1} el valor de @var{Num0}.
  3) Se llama recursivamente a se_repite/4 con las variables @var{X}, @var{Parte}, @var{Num1}, @var{Num}.
  4) Cuando @var{Cs} esta vacia, se iguala los valores de @var{Num0} y @var{Num}.
@end{verbatim}

@subsection{repeticion(Inicial, Comprimida)}
@begin{verbatim}
:- pred repeticion(Inicial, Comprimida)

Se basa en los predicados anteriormente detallados @var{partir/3} y @var{se_repite/4}. Este predicado identifica un prefijo que nos de por repeticion la secuencia inicial.
Se comprime de forma recursiva mediante un llamada a @var{compresion_recursiva/2}. Finalmente se debe componer la parte (comprimida recursivamente) con el numero de repeticiones usando el predicado @var{parentesis/3}. @includedef{repeticion/2}
@end{verbatim}

@subsection{Explicacion repeticion(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama al predicado @var{partir/2} obteniendo asi una seccion de @var{Inicial} en @var{Parte1}.
  2) Se llama a @var{se_repite/4} para identificar si la seccion @var{Parte1} se repite en el conjunto de @var{Inicial} y de ser asi, cuantas veces lo hace en @var{Num}.
  3) Se llama a @var{compresion_recursiva/2} (posteriormente se hace una llamada a un predicado auxiliar por motivos de eficiencia), obteniendo en @var{X} la secuencia @var{Parte1} comprimida.
  4) Por ultimo se hace una llamada a @var{parentesis/3} para envolver a la secuencia @var{X} con su numero de repeticiones @var{Num} y devolverlo en @var{Comprimida}.
@end{verbatim}

@subsection{divison(Inicial, Comprimida)}
@begin{verbatim}
:- pred division(Inicial, Comprimida)

Verifica que la lista @var{Inicial} queda dividida en dos partes y llama a @var{compresion_recursiva/2} de forma recursiva para posteriormente concatenar los resultados obtenidos. @includedef{division/2}
@end{verbatim}

@subsection{Explicacion division(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{partir/2} de tal forma que se divide @var{Inicial} en las sublistas @var{X} e @var{Y}.
  2) Se llama a @var{compresion_recursiva/2} obteniendo en @var{X1} la compresion de @var{X}.
  3) Se llama a @var{compresion_recursiva/2} obteniendo en @var{Y1} la compresion de @var{Y}.
  4) Por ultimo, se concatenan las listas @var{X1} e @var{Y1} en la lista @var{Comprimida}.
@end{verbatim}

@subsection{compresion(Inicial, Comprimida)}
@begin{verbatim}
:- pred compresion(Inicial, Comprimida)

Llama alternativamente a los predicados @var{repeticion/2} y a @var{division/2} detallados anteriormente.
Esto implica que ademas de considerar las repeticiones, podremos dividir la lista inicial en dos partes y aplicar el algoritmo a cada una de ellas por separado, de esto modo consiguiendo mas posibilidades de encontrar una repeticion. @includedef{compresion/2}
@end{verbatim}

@subsection{Explicacion compresion(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Si la lista @var{Inicial} cuenta con un solo elemento, @var{Comprimida} pasa a ser este elemento.
  2) Se llama a @var{division/2} con los parametros de entrada de este predicado.
  3) Por otro lado se llama a @var{repeticion/2} con los mismos parametros de entrada de este predicado.
@end{verbatim}

@subsection{mejor_compresion(Inicial, Comprimida)}
@begin{verbatim}
:- pred mejor_compresion(Inicial, Comprimida)

Verifica la busqueda de las compresiones que reduzcan el tamaño. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para postenirmente filtrar la solucion mas corta. @includedef{mejor_compresion/2}.
@end{verbatim}

@subsection{Explicacion mejor_compresion(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se usa el predicado @var{findall/3} desde el que se llama a @var{get_all_compresions/2} para obtener todas las compresiones posibles en @var{Comp_List}.
  2) Se hace uso de @var{sort/2} para odenar las comrpesiones de @var{Comp_List} en funcion del tamaño de sus elementos.
  3) Por ultimo hacemos uso del predicado auxiliar @var{head/2} para obtener el primer elemento de @var{Sorted_List} obteniendo asi la lista con menor tamaño en @var{Comprimido}.
@end{verbatim}

@subsection{mejor_compresion_memo(Inicial, Comprimida)}
@begin{verbatim}
:- pred mejor_compresion_memo(Inicial, Comprimida)

Verifica la busqueda de las compresiones que reduzcan el tamaño implementado un esquema de memorizacion de lemas. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para posteriormente filtrar la solucion mas corta. @includedef{mejor_compresion_memo/2}
@end{verbatim}

@subsection{Explicacion mejor_compresion_memo(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{limpia_memo/0} para borrar los lemas del predicado @var{memo/2}
  1) Se llama a @var{mejor_compresion/2} para encontrar la mejor compresion.
  2) Se hace un corte para no obtener mas compresiones. De no introducir este corte se obtendia una lista de compresiones ordenadas en funcion de su tamaño, lo que es interesante pero no es lo que se pide. @includedef{mejor_compresion_memo}
  NOTA: es importante denotar que el predicado @var{mejor_compresion/2} ya hace uso de la memorizacion de lemas gracias a @var{get_all_compresions/2} por lo que este proceso sera detallado posteriormente en la seccion de predicados auxiliares.
@end{verbatim}

@section{Predicados Auxiliares}
@subsection{get_all_compresions(Inicial, Comprimido)}
@begin{verbatim}
:- pred get_all_compresions(Inicial, Comprimido)

Verifica la busqueda de compresiones de la lista @var{Inicial} y guarda de forma dinamica los resultados encontrados. En caso de que una compresion ya haya sido obtenida anteriormente,
esta se descarta. Sim embargo, si estamos ante una nueva compresion, este se guardara dinamicamente como lema del predicado @var{found_result/1}. Se ha optado por guardar la compresion en formato Key, Value siendo Key el tamaño de la lista y value la lista en si. @includedef{get_all_compresions/2}
@end{verbatim}

@subsection{Explicacion get_all_compresions(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{sub_compresion_recursiva/2} obteniendo una compresion en @var{New_Comp}.
  2) Si @var{New_Comp} ya ha sido encontrada previamente se failea la busqueda de esta compresion en concreto.
  3) Si @var{New_Comp} no ha sido encontrada previamente, se guarda en @var{found_result/1} haciendo uso del predicado @var{store_result/1}
  4) Se haya el tamaño de la lista @var{New_Comp} en @var{CL} haciendo uso del predicado @var{length/2}.
  5) Se verifica que la variable de entrada @var{Comprimido} sea igual al par @var{CL} - @var{New_Comp}.
@end{verbatim}

@subsection{store_result(X)}
@begin{verbatim}
:- pred store_result(X)

Guarda de forma dinamica @var{X} como lema del predicado @var{found_result/1}.@includedef{store_result/1}
@end{verbatim}

@subsection{Explicacion store_result(X)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{assert/1} para almacenar dinamicamente el lema @var{X} en el predicado @var{found_result/1}.
@end{verbatim}

@subsection{limpia_memo}
@begin{verbatim}
:- pred limpia_memo

Elimina la memoria dinamica asiganada a los lemas del predicado @var{found_result/1}.@includedef{limpia_memo/0}
@end{verbatim}

@subsection{Explicacion limpia_memo}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{retractall/1} para eliminar la memoria asiganada los lemas del predicado @var{found_result/1}.
@end{verbatim}

@subsection{head(List, Head)}
@begin{verbatim}
:- pred head(List, Head)

Obtiene en @var{Head} el primer elemento de una lista no vacia @var{List}.@includedef{head/2}
@end{verbatim}

@subsection{Explicacion head(List, Head)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Establecer una igualdad entre el primer elemeto de @var{Lista} y @var{Head}.
@end{verbatim}


@section{Consultas realizadas}
@subsection{eliminar_comodines(X,R,L)}
@begin{verbatim}

Se comprueba que da fallo cuando la CPU tiene menos de 2 registros

:- test eliminar_comodines(X,R,L) : (X=regs(1)) + fails #''La CPU tiene que tener de 2 a N registros''.

Se comprueba que no da fallo cuando todos los registros tienen constantes

:- test eliminar_comodines(X,R,L) : (X=regs(12,3,4,a,t,+,*,p)) + not_fails #''Registros de la CPU correctos''.

Se comprueba que da fallo cuando algun registro tiene una variable

:- test eliminar_comodines(X,R,L) : (X=regs(1,32,X,u,i9)) + fails #''Algun registro de la CPU tiene una variable''.

Se comprueba que no da fallo cuando no hay variables

:- test eliminar_comodines(X,R,L) : (X=regs(!,2)) + not_fails #''Registros de la CPU correctos''.

Se comprueba que da fallo cuando algun registro tiene un elemento que no es una constante

:- test eliminar_comodines(X,R,L) : (X=regs(1,32,4<5)) + fails #''Algun registro de la CPU tiene un elemento que no es una constante''.

Se comprueba que da fallo cuando no se elimina el comodin

:- test eliminar_comodines(X,R,L) : (X=regs(0,*),R=regs(0,*)) + fails #''No se ha eliminado el comodin''.

Se comprueba que da fallo cuando no se elimina el comodin

:- test eliminar_comodines(X,R,L) : (X=regs(*,0),R=regs(*,0)) + fails #''No se ha eliminado el comodin''.

Se comprueba que da fallo cuando no se elimina el comodin

:- test eliminar_comodines(X,R,L) : (X=regs(0,1,4,+,2,*),R=regs(0,1,4,+,2,*)) + fails #''No se ha eliminado el comodin''.

Se comprueba que da fallo cuando no se elimina el comodin

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3),R=regs(*,1,4,+,2,3)) + fails #''No se ha eliminado el comodin''.

Se comprueba que da fallo cuando no se eliminan todos los comodines

:- test eliminar_comodines(X,R,L) : (X=regs(*,*),R=regs(_,*)) + fails #''No se han eliminado todos los comodines''.

Se comprueba que da fallo cuando no se eliminan todos los comodines

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*),R=regs(0,W,*)) + fails #''No se han eliminado todos los comodines''.

Se comprueba que da fallo cuando no se eliminan todos los comodines

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*),R=regs(*,0,0,_L)) + fails #''No se han eliminado todos los comodines''.

Se comprueba que da fallo cuando no se eliminan todos los comodines

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*),R=regs(*,1,*,+,2,*)) + fails #''No se han eliminado todos los comodines''.

Se comprueba que da fallo cuando no se sustituye el comodin por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(0,*),R=regs(0,a)) + fails #''No se ha sustituido el comodin por una variable''.

Se comprueba que da fallo cuando no se sustituye el comodin por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,0),R=regs(1,0)) + fails #''No se ha sustituido el comodin por una variable''.

Se comprueba que da fallo cuando no se sustituye el comodin por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(0,1,4,+,2,*),R=regs(0,1,4,+,2,<)) + fails #''No se ha sustituido el comodin por una variable''.

Se comprueba que no da fallo cuando se sustituye el comodin por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3)) => (R=regs(_,1,4,+,2,3)) + not_fails #''Se ha sustituido por variable''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,*)) => (R=regs(_,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*)) => (R=regs(0,_,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*)) => (R=regs(_,0,0,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*)) => (R=regs(_,1,_,+,2,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituye el comodin por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3),R=regs(_,1,4,+,2,3)) + not_fails #''Se ha sustituido por variable''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,*),R=regs(_Z,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*),R=regs(0,_Hola,Z)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*),R=regs(P,0,0,_)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se sustituyen los comodines por una variable

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*),R=regs(_Ea,1,_,+,2,W)) + not_fails #''Se han sustituido por variables''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(1,1,+,5,*)) => (R=regs(1,1,+,5,_),L=[1,1,+,5]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(0,*)) => (R=regs(0,_),L=[0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,0)) => (R=regs(_,0),L=[0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(0,1,4,+,2,*)) => (R=regs(0,1,4,+,2,_),L=[0,1,4,+,2]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3)) => (R=regs(_,1,4,+,2,3),L=[1,4,+,2,3]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,*)) => (R=regs(_,_),L=[]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*)) => (R=regs(0,_,_),L=[0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*)) => (R=regs(_,0,0,_),L=[0,0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*)) => (R=regs(_,1,_,+,2,_),L=[1,+,2]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3)) => (R=regs(_,1,4,+,2,3),L=[1,4,+,2,3]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,*)) => (R=regs(_Z,_),L=[]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*)) => (R=regs(0,_Hola,Z),L=[0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*)) => (R=regs(P,0,0,_),L=[0,0]) + not_fails #''Lista generada correctamente''.

Se comprueba que no da fallo cuando se genera la lista correctamente

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*)) => (R=regs(_Ea,1,_,+,2,W),L=[1,+,2]) + not_fails #''Lista generada correctamente''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(1,1,+,5,*),R=regs(1,1,+,5,_),L=[1,1,+]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(0,*),R=regs(0,_),L=[0,1]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,0),R=regs(_,0),L=[0,2]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(0,1,4,+,2,*),R=regs(0,1,4,+,2,_),L=[0,1,4,+,2,3]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3),R=regs(_,1,4,+,2,3),L=[1,4,+,2]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,*),R=regs(_,_),L=[1]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*),R=regs(0,_,_),L=[]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*),R=regs(_,0,0,_),L=[0,0,3]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*),R=regs(_,1,_,+,2,_),L=[1,+,2,5]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,4,+,2,3),R=regs(_,1,4,+,2,3),L=[1,4,+,2]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,*),R=regs(_Z,_),L=[1]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(0,*,*),R=regs(0,_Hola,Z),L=[0,5]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,0,0,*),R=regs(P,0,0,_),L=[0,0,0]) + fails #''Lista generada de forma incorrecta''.

Se comprueba que da fallo cuando se genera la lista

:- test eliminar_comodines(X,R,L) : (X=regs(*,1,*,+,2,*),R=regs(_Ea,1,_,+,2,W),L=[1,+,2,0]) + fails #''Lista generada de forma incorrecta''.
@end{verbatim}

@subsection{ejecutar_instruccion(EA,I,ES)}
@begin{verbatim}
Se comprueba que da fallo cuando la CPU tiene menos de 2 registros

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1)) + fails #''La CPU tiene que tener de 2 a N registros''.

Se comprueba que no da fallo cuando todos los registros tienen constantes

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(12,3,4,a,t,+,*,p),I=move(1)) + not_fails #''Registros de la CPU correctos''.

Se comprueba que da fallo cuando algun registro tiene una variable

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,32,X,u,i9),I=swap(1,7)) + fails #''Algun registro de la CPU tiene una variable''.

Se comprueba que no da fallo cuando no hay variables

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(!,2),I=swap(1,2)) + not_fails #''Registros de la CPU correctos''.

Se comprueba que da fallo cuando algun registro tiene un elemento que no es una constante

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,32,4<5),I=move(1)) + fails #''Algun registro de la CPU tiene un elemento que no es una constante''.

Se comprueba que da fallo cuando se meten parametros al swap que no son numeros

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1,a)) + fails #''Parametro no valido en swap''.

Se comprueba que da fallo cuando se meten parametros al swap que no son numeros

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(b,2)) + fails #''Parametro no valido en swap''.

Se comprueba que da fallo cuando se meten parametros al swap que no son numeros

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1,'a')) + fails #''Parametro no valido en swap''.

Se comprueba que da fallo cuando se meten parametros al swap que no son numeros

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1,''a'')) + fails #''Parametro no valido en swap''.

Se comprueba que da fallo cuando se meten mas de 2 numeros al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1,2,4)) + fails #''Swap solo tiene 2 parametros''.

Se comprueba que da fallo cuando se mete solo 1 numero al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1)) + fails #''Swap necesita 2 parametros''.

Se comprueba que da fallo cuando primer parametro es igual que el segundo en swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(5,5)) + fails #''i tiene que ser menor que j en swap''.

Se comprueba que da fallo cuando se mete 0 al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(0,1)) + fails #''No hay registro 0''.

Se comprueba que da fallo cuando se mete 1 numero negativo al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(-1,1)) + fails #''No hay registros negativos''.

Se comprueba que da fallo cuando se mete algun numero mayor que el numero de registros al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(5,1)) + fails #''Los numeros de swap tienen que ser menores que el numero de registro''.

Se comprueba que da fallo cuando se mete algun numero mayor que el numero de registros al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(1,5)) + fails #''Los numeros de swap tienen que ser menores que el numero de registro''.

Se comprueba que da fallo cuando se mete algun numero mayor que el numero de registros al swap

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=swap(5,7)) + fails #''Los numeros de swap tienen que ser menores que el numero de registro''.

Se comprueba que da fallo cuando se mete mas de 1 numero al move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(1,2)) + fails #''Move solo tiene 1 parametro''.

Se comprueba que da fallo cuando se mete mas de 1 numero al move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(1,2,3)) + fails #''Move solo tiene 1 parametro''.

Se comprueba que da fallo cuando se mete 1 numero negativo a move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(-1)) + fails #''El numero de move tiene que ser positivo''.

Se comprueba que da fallo cuando se mete 0 a move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(0)) + fails #''El numero de move tiene que ser positivo''.

Se comprueba que da fallo cuando se mete 1 numero mayor que numero de registros a move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(4)) + fails #''El numero de move tiene que ser menor que el numero de registros''.

Se comprueba que da fallo cuando se mete 1 parametro que no es un numero al move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(a)) + fails #''Parametro de move solo puede ser 1 numero''.

Se comprueba que da fallo cuando se mete 1 parametro que no es un numero al move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move(''a'')) + fails #''Parametro de move solo puede ser 1 numero''.

Se comprueba que da fallo cuando se mete 1 parametro que no es un numero al move

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,4),I=move('a')) + fails #''Parametro de move solo puede ser 1 numero''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2), I=swap(1,2)) => (ES=regs(2,1)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,*,<), I=swap(2,3)) => (ES=regs(1,<,*)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(2,*,<,-1), I=swap(1,4)) => (ES=regs(-1,*,<,2)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,+,5,*), I=swap(1,2)) => (ES=regs(2,1,+,5,*)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2), I=swap(1,2), ES=regs(1,2)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,*,<), I=swap(2,3), ES=regs(*,1,<)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(2,*,<,-1), I=swap(1,4), ES=regs(*,<,-1,2)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,+,5,*), I=swap(1,2), ES=regs(*,1,2,5,+)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2), I=move(1)) => (ES=regs(1,1)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,*,<), I=move(2)) => (ES=regs(1,*,*)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,*,<,1), I=move(3)) => (ES=regs(1,*,<,<)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(2,*,<,-1), I=move(4)) => (ES=regs(-1,*,<,-1)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,+,5,*), I=move(5)) => (ES=regs(*,2,+,5,*)) + not_fails #''Instruccion ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2), I=move(1), ES=regs(1,2)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,*,<), I=move(3), ES=regs(1,<,1)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(2,*,<,-1), I=move(4), ES=regs(2,*,*,-1)) + fails #''Instruccion no ejecutada correctamente''.

Se comprueba que no se ejecuta la instruccion correctamente

:- test ejecutar_instruccion(EA,I,ES) : (EA=regs(1,2,+,5,*), I=move(2), ES=regs(1,1,+,5,*)) + fails #''Instruccion no ejecutada correctamente''.
@end{verbatim}

@subsection{generador_de_codigo(EI,EF,L)}
@begin{verbatim}
Se comprueba que da fallo cuando EI tiene menos de 2 registros

:- test generador_de_codigo(EI,EF,L) : (EI=regs(1)) + fails  #''EI tiene que tener de 2 a N registros ''.

Se comprueba que da fallo cuando EF tiene menos de 2 registros

:- test generador_de_codigo(EI,EF,L) : (EI=regs(1,2),EF=regs(1)) + fails  #''EF tiene que tener de 2 a N registros ''.

Se comprueba que da fallo cuando algun registro tiene una variable

:- test generador_de_codigo(EI,EF,L) : (EI=regs(1,32,X,u,i9),EF=regs(i9,32,X,u,1)) + fails  #''Algun registro de la CPU tiene una variable''.

Se comprueba que no da fallo cuando no hay variables

:- test generador_de_codigo(EI,EF,L) : (EI=regs(!,2),EF=regs(2,!)) + not_fails  #''Registros de la CPU correctos''.

Se comprueba que da fallo cuando algun registro tiene un elemento que no es una constante

:- test generador_de_codigo(EI,EF,L) : (IA=regs(1,32,4<5),EF=regs(32,1,4<5)) + fails  #''Algun registro de la CPU tiene un elemento que no es una constante''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(<,2),EF=regs(2,<)) => (L = [swap(1,2)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(<,2),EF=regs(*,*)) => (L = []) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(<,2),EF=regs(<,<)) => (L = [move(1)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(*,*),EF=regs(a,b)) + fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c),EF=regs(b,c,b)) => (L =[swap(2,3),move(3)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,c,*),EF=regs(c,a,*)) => (L =[swap(1,2)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(1,2,3),EF=regs(1,1,1)) => (L =[move(1),move(2)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(1,<,z),EF=regs(<,<,<)) => (L =[move(2),move(3)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c),EF=regs(a,a,*)) => (L =[move(1)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(*,*,*),EF=regs(*,*,*)) => (L =[]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(*,a,*),EF=regs(a,*,*)) => (L =[swap(1,2)]) + not_fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con el tamaño minimo de instrucciones

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c,d,*),EF=regs(a,b,c,d,e)) + fails #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,*,c),EF=regs(c,a,*)) => (L=[move(1),move(3)];L=[move(1),swap(1,3)];L=[swap(1,2),move(3)];L=[swap(1,2),swap(1,3)];L=[swap(1,3),swap(2,3)];L=[swap(2,3),swap(1,2)]) + (try_sols(6), not_fails) #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c,d),EF=regs(a,d,a,b)) => (L=[move(2),move(1),swap(2,3),swap(2,4)];L=[move(2),move(1),swap(2,4),swap(3,4)];L=[move(2),move(1),swap(3,4),swap(2,3)];L=[move(2),swap(3,4),move(1),swap(2,3)];L=[swap(1,2),move(2),swap(1,2),swap(2,4)];L=[swap(1,2),move(2),swap(1,4),swap(1,2)];L=[swap(1,2),move(2),swap(2,4),swap(1,4)];L=[swap(1,2),swap(1,4),move(2),swap(1,2)];L=[swap(2,3),move(1),swap(2,3),swap(2,4)];L=[swap(2,3),move(1),swap(2,4),swap(3,4)];L=[swap(2,3),move(1),swap(3,4),swap(2,3)];L=[swap(2,3),swap(3,4),move(1),swap(2,3)];L=[swap(1,4),swap(2,4),move(2),swap(1,2)];L=[swap(2,4),move(2),move(1),swap(2,3)];L=[swap(2,4),swap(1,2),move(2),swap(1,2)];L=[swap(2,4),swap(2,3),move(1),swap(2,3)];L=[swap(3,4),swap(2,4),move(1),swap(2,3)]) + (try_sols(17), not_fails) #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c),EF=regs(a,a,*)) => (L=[move(1)]) + (try_sols(1), not_fails) #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(a,b,c,d,*,e,*),EF=regs(a,*,*,a,b,e,e)) => (L = [move(6),swap(2,5),move(1),swap(2,4)];L=[swap(2,5),move(6),move(1),swap(2,4)];L=[swap(2,5),move(1),move(6),swap(2,4)];L=[swap(2,5),move(1),swap(2,4),move(6)]) + (try_sols(4), not_fails) #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(p,r,t,*,*,w,o,w),EF=regs(*,*,*,*,*,*,p,t)) => (L=[swap(1,7),swap(3,8)];L=[swap(3,8),swap(1,7)]) + (try_sols(6), not_fails) #''Lista minima generada correctamente''.

Se comprueba que se genera una lista con todas las soluciones que tienen el tamaño minimo de movimientos

:- test generador_de_codigo(EI,EF,L) : (EI=regs(p,r,t,*,*,w,o,w),EF=regs(*,p,w,*,o,*,p,t)) => (L=[move(1),swap(1,5),swap(5,7),swap(3,8)];L=[move(1),swap(1,5),swap(3,8),swap(5,7)];L=[move(1),swap(1,7),swap(1,5),swap(3,8)];L=[move(1),swap(1,7),swap(3,8),swap(1,5)];L=[move(1),swap(5,7),swap(1,7),swap(3,8)];L=[move(1),swap(5,7),swap(3,8),swap(1,7)];L=[move(1),swap(3,8),swap(1,5),swap(5,7)];L=[move(1),swap(3,8),swap(1,7),swap(1,5)];L=[move(1),swap(3,8),swap(5,7),swap(1,7)];L=[swap(5,7),move(1),swap(1,7),swap(3,8)];L=[swap(5,7),move(1),swap(3,8),swap(1,7)];L=[swap(5,7),swap(3,8),move(1),swap(1,7)];L=[swap(3,8),move(1),swap(1,5),swap(5,7)];L=[swap(3,8),move(1),swap(1,7),swap(1,5)];L=[swap(3,8),move(1),swap(5,7),swap(1,7)];L=[swap(3,8),swap(5,7),move(1),swap(1,7)]) + (try_sols(6), not_fails) #''Lista minima generada correctamente''.
@end{verbatim}

 ").



















