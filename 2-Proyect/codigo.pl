:- module(_, _, [assertions]).

:- use_module(library(iso_misc)).
:- use_module(library(lists)).
:- use_module(library(sort)).
%:- use_module(library(assertions/native_props)).

alumno_prode('Serrano','Arrese','Francisco Javier','A180487').

:- dynamic
#"Guarda dinamicamente en memoria las compresiones para la optimizacion de la busqueda. @includedef{dynamic/0}".
    memo/1.

%%% LIMPIA MEMO %%%
/*Elimina la memoria dinamica asiganada a los lemas del predicado @var{memo/1}.@includedef{limpia_memo/0}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{retractall/1} para eliminar la memoria asiganada los lemas del predicado @var{memo/1}.*/
:- pred limpia_memo
#"Elimina los lemas del predicado memo, que son las compresiones encontradas para un secuencia dada como input. @includedef{limpia_memo/0}".
limpia_memo :- retractall(memo(_)).

%%% STORE RESULT %%%
/*Guarda de forma dinamica @var{X} como lema del predicado @var{memo/1}.@includedef{store_result/1}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{assert/1} para almacenar dinamicamente el lema @var{X} en el predicado @var{memo/1}.*/
:- pred store_result(X)
#"Guarda de forma dinamica @var{X} como lema del predicado @var{memo/1}.@includedef{store_result/1}".
store_result(X) :- assert(memo(X)).

:- pred compresion_recursiva(Inicial, Comprimido)
#"Comprime las listas y recibe llamadas de forma recursiva. @includedef{compresion_recursiva/2}".
compresion_recursiva(Inicial, Comprimido) :-
  limpia_memo,
  mejor_compresion(Inicial, Comprimido).

%%% MEJOR COMPRESION MEMO %%%
/* Verifica la busqueda de las compresiones que reduzcan el tamaño implementado un esquema de memorizacion de lemas. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para posteriormente filtrar la solucion mas corta. @includedef{mejor_compresion_memo/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{limpia_memo/0} para borrar los lemas del predicado @var{memo/2}
  1) Se llama a @var{mejor_compresion/2} para encontrar la mejor compresion.
  2) Se hace un corte para no obtener mas compresiones. De no introducir este corte se obtendia una lista de compresiones ordenadas en funcion de su tamaño, lo que es interesante pero no es lo que se pide. @includedef{mejor_compresion_memo}
  NOTA: es importante denotar que el predicado @var{mejor_compresion/2} ya hace uso de la memorizacion de lemas gracias a @var{get_all_compresions/2} por lo que este proceso sera detallado posteriormente en la seccion de predicados auxiliares.*/
:- pred mejor_compresion_memo(Inicial, Comprimido)
#"Verifica la busqueda de las compresiones que reduzcan el tamaño implementado un esquema de memorizacion de lemas. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para posteriormente filtrar la solucion mas corta. @includedef{mejor_compresion_memo/2}".
mejor_compresion_memo(Inicial, Comprimido) :-
  limpia_memo,
  mejor_compresion(Inicial, Comprimido),
  !.

:- pred comprimir(Inicial, Comprimido)
#"Compresion auxiliar. @includedef{comprimir/2}".
comprimir(Inicial, Comprimido) :-
  limpia_memo,
  mejor_compresion(Inicial, Comprimido).

%%% MEJOR COMPRESION %%%
/*Verifica la busqueda de las compresiones que reduzcan el tamaño. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para postenirmente filtrar la solucion mas corta. @includedef{mejor_compresion/2}.
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se usa el predicado @var{findall/3} desde el que se llama a @var{get_all_compresions/2} para obtener todas las compresiones posibles en @var{Comp_List}.
  2) Se hace uso de @var{sort/2} para odenar las comrpesiones de @var{Comp_List} en funcion del tamaño de sus elementos.
  3) Por ultimo hacemos uso del predicado auxiliar @var{head/2} para obtener el primer elemento de @var{Sorted_List} obteniendo asi la lista con menor tamaño en @var{Comprimido}.*/
:- pred mejor_compresion(Inicial, Comprimido)
#"Verifica la busqueda de las compresiones que reduzcan el tamaño. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para postenirmente filtrar la solucion mas corta. @includedef{mejor_compresion/2}".
mejor_compresion(Inicial, Comprimido) :-
  findall(Y, get_all_compresions(Inicial, Y), Comp_List),
  sort(Comp_List, Sorted_List),
  head(Sorted_List, [_-Comprimido]).
  
%%% GET ALL COMPRESIONS %%%
/*Verifica la busqueda de compresiones de la lista @var{Inicial} y guarda de forma dinamica los resultados encontrados. En caso de que una compresion ya haya sido obtenida anteriormente,
esta se descarta. Sim embargo, si estamos ante una nueva compresion, este se guardara dinamicamente como lema del predicado @var{memo/1}. Se ha optado por guardar la compresion en formato Key, Value siendo Key el tamaño de la lista y value la lista en si. @includedef{get_all_compresions/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{sub_compresion_recursiva/2} obteniendo una compresion en @var{New_Comp}.
  2) Si @var{New_Comp} ya ha sido encontrada previamente se failea la busqueda de esta compresion en concreto.
  3) Si @var{New_Comp} no ha sido encontrada previamente, se guarda en @var{memo/1} haciendo uso del predicado @var{store_result/1}
  4) Se haya el tamaño de la lista @var{New_Comp} en @var{CL} haciendo uso del predicado @var{length/2}.
  5) Se verifica que la variable de entrada @var{Comprimido} sea igual al par @var{CL} - @var{New_Comp}.*/
:- pred get_all_compresions(Inicial, Comprimido)
#"Verifica la busqueda de compresiones de la lista @var{Inicial} y guarda de forma dinamica los resultados encontrados. En caso de que una compresion ya haya sido obtenida anteriormente, esta se descarta. Sim embargo, si estamos ante una nueva compresion, este se guardara dinamicamente como lema del predicado @var{memo/1}. Se ha optado por guardar la compresion en formato Key, Value siendo Key el tamaño de la lista y value la lista en si. @includedef{get_all_compresions/2}".
get_all_compresions(Inicial, Comprimido) :-
  sub_compresion_recursiva(Inicial, New_Comp),
  ( memo(New_Comp) ->
    fail
  ;
    store_result(New_Comp),
    length(New_Comp, CL),
    Comprimido = [CL-New_Comp]
  ).

:- pred sub_compresion_recursiva(Inicial, Comprimido)
#"Obtiene una compresion unicamente @includedef{sub_compresion_recursiva/2}".
sub_compresion_recursiva(Inicial, Comprimido) :-
  compresion(Inicial, Comprimido).

sub_compresion_recursiva(Inicial, Inicial).

%%% PARTIR %%%
/*Se verifica si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacias que concatenadas forman la secuencia @var{Todo}.@includedef{partir/3}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Comprobacion de que @var{Parte1} no es una lista vacia.
  2) Comprobacion de que @var{Parte2} no es una lista vacia.
  3) Utilizar el predicado @var{append/2} para encontrar las listas que concatenadas formen la lista @var{Todo}.*/
:- pred partir(Todo, Parte1, Parte2)
#"Se verifica si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacias que concatenadas forman la secuencia @var{Todo}.@includedef{partir/3}".
partir(Todo, Parte1, Parte2) :-
  Parte1 = [_|_],
  Parte2 = [_|_],
  append(Parte1, Parte2, Todo).

%%% PARENTESIS %%%
/*Compone la lista de elementos @var{Parte} con el numero de repeticiones dados por la variable @var{Num} envolviendo a esta lista con parentesis solo si esta tiene 2 elementos o mas. @includedef{parentesis/3}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se checkea que @var{Num} sea efectivamente un numero entero positivo.
  2) Se comprueba el size de la lista @var{Parte} y se bifurca entre size = 1 o size > 1.
  3) Si @var{Parte} es de size 1, se hace un corte para no envolverla en parentesis y se concatena el valor de @var{Parte} con el numero de @var{Num} en @var{ParteNum}.
  4) Si @var{Parte} es de size > 1, se concatenan parentesis por delante y por detras de la lista en @var{ParteNum}.*/
:- pred parentesis(Parte, Num, ParteNum)
#"Compone la lista de elementos @var{Parte} con el numero de repeticiones dados por la variable @var{Num} envolviendo a esta lista con parentesis solo si esta tiene 2 elementos o mas. @includedef{parentesis/3}".
parentesis(Parte, Num, ParteNum) :-
  number(Num),
  length(Parte, 1),
  !,
  append(Parte, [Num], ParteNum).

parentesis(Parte, Num, ParteNum) :-
  number(Num),
  append(['('],Parte,ParteAux),
  append(ParteAux,[')',Num],ParteNum).

%%% SE REPITE %%%
/*Se verifica si @var{Cs} se obtiene por repetir N veces la secuencia @var{Parte}. El argumento @var{Num} incrementa @var{Num0} en N. @includedef{se_repite/4}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se obtiene en @var{X} la lista que concatenada a @var{Parte} forma la lista completa @var{Cs}.
  2) Se incrementa en @var{Num1} el valor de @var{Num0}.
  3) Se llama recursivamente a se_repite/4 con las variables @var{X}, @var{Parte}, @var{Num1}, @var{Num}.
  4) Cuando @var{Cs} esta vacia, se iguala los valores de @var{Num0} y @var{Num}.*/
:- pred se_repite([], _, Num0, Num0)
#"Se verifica si @var{Cs} se obtiene por repetir N veces la secuencia @var{Parte}. El argumento @var{Num} incrementa @var{Num0} en N. @includedef{se_repite/4}".
se_repite([], _, Num0, Num0).

se_repite(Cs, Parte, Num0, Num) :-
  append(Parte,X,Cs),
  Num1 is Num0 + 1,
  se_repite(X,Parte,Num1,Num).

%%% REPETICION %%%
/*Se basa en los predicados anteriormente detallados @var{partir/3} y @var{se_repite/4}. Este predicado identifica un prefijo que nos de por repeticion la secuencia inicial.
Se comprime de forma recursiva mediante un llamada a @var{compresion_recursiva/2}. Finalmente se debe componer la parte (comprimida recursivamente) con el numero de repeticiones usando el predicado @var{parentesis/3}. @includedef{repeticion/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama al predicado @var{partir/2} obteniendo asi una seccion de @var{Inicial} en @var{Parte1}.
  2) Se llama a @var{se_repite/4} para identificar si la seccion @var{Parte1} se repite en el conjunto de @var{Inicial} y de ser asi, cuantas veces lo hace en @var{Num}.
  3) Se llama a @var{compresion_recursiva/2} (posteriormente se hace una llamada a un predicado auxiliar por motivos de eficiencia), obteniendo en @var{X} la secuencia @var{Parte1} comprimida.
  4) Por ultimo se hace una llamada a @var{parentesis/3} para envolver a la secuencia @var{X} con su numero de repeticiones @var{Num} y devolverlo en @var{Comprimida}.*/
:- pred repeticion(Inicial, Comprimida)
#"Se basa en los predicados anteriormente detallados @var{partir/3} y @var{se_repite/4}. Este predicado identifica un prefijo que nos de por repeticion la secuencia inicial. Se comprime de forma recursiva mediante un llamada a @var{compresion_recursiva/2}. Finalmente se debe componer la parte (comprimida recursivamente) con el numero de repeticiones usando el predicado @var{parentesis/3}. @includedef{repeticion/2}".
repeticion(Inicial, Comprimida) :-
  partir(Inicial, Parte1, _),
  se_repite(Inicial, Parte1, 0, Num),
  sub_compresion_recursiva(Parte1,X),
  parentesis(X, Num, Comprimida).

%%% COMPRESION %%%
/*Llama alternativamente a los predicados @var{repeticion/2} y a @var{division/2} detallados anteriormente.
Esto implica que ademas de considerar las repeticiones, podremos dividir la lista inicial en dos partes y aplicar el algoritmo a cada una de ellas por separado, de esto modo consiguiendo mas posibilidades de encontrar una repeticion. @includedef{compresion/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Si la lista @var{Inicial} cuenta con un solo elemento, @var{Comprimida} pasa a ser este elemento.
  2) Se llama a @var{division/2} con los parametros de entrada de este predicado.
  3) Por otro lado se llama a @var{repeticion/2} con los mismos parametros de entrada de este predicado.*/
:- pred compresion([X|[]], [X])
#"Llama alternativamente a los predicados @var{repeticion/2} y a @var{division/2} detallados anteriormente. Esto implica que ademas de considerar las repeticiones, podremos dividir la lista inicial en dos partes y aplicar el algoritmo a cada una de ellas por separado, de esto modo consiguiendo mas posibilidades de encontrar una repeticion. @includedef{compresion/2}".
compresion([X|[]], [X]) :- !.

compresion(Inicial, Comprimida) :-
  division(Inicial, Comprimida).

compresion(Inicial, Comprimida) :-
  repeticion(Inicial, Comprimida).

%%% DIVISION %%%
/*Verifica que la lista @var{Inicial} queda dividida en dos partes y llama a @var{compresion_recursiva/2} de forma recursiva para posteriormente concatenar los resultados obtenidos. @includedef{division/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{partir/2} de tal forma que se divide @var{Inicial} en las sublistas @var{X} e @var{Y}.
  2) Se llama a @var{compresion_recursiva/2} obteniendo en @var{X1} la compresion de @var{X}.
  3) Se llama a @var{compresion_recursiva/2} obteniendo en @var{Y1} la compresion de @var{Y}.
  4) Por ultimo, se concatenan las listas @var{X1} e @var{Y1} en la lista @var{Comprimida}.*/
:- pred division(Inicial, Comprimida)
#"Verifica que la lista @var{Inicial} queda dividida en dos partes y llama a @var{compresion_recursiva/2} de forma recursiva para posteriormente concatenar los resultados obtenidos. @includedef{division/2}".
division(Inicial, Comprimida) :-
  partir(Inicial, X, Y),
  sub_compresion_recursiva(X, X1),
  sub_compresion_recursiva(Y, Y1),
  append(X1, Y1, Comprimida).

%%% HEAD %%%
/*Obtiene en @var{Head} el primer elemento de una lista no vacia @var{List}.@includedef{head/2}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Establecer una igualdad entre el primer elemeto de @var{Lista} y @var{Head}.*/
:- pred head([H|_], H)
#"Obtiene en @var{Head} el primer elemento de una lista no vacia @var{List}.@includedef{head/2}".
head([H|_], H).

%%%%%%%%%%%%%%%%%
%%%           %%%
%     Tests     %
%%%           %%%
%%%%%%%%%%%%%%%%%

% tests partir(Todo, Parte1, Parte2)

% Se comprueba caso erroneo en el que Todo es una lista vacia

:- test partir(Todo, Parte1, Parte2) : (Todo=[]) + fails #"No se puede partir una lista vacia".

% Se comprueba caso erroneo en el que Todo es una lista de un elemento

:- test partir(Todo, Parte1, Parte2) : (Todo=[a]) + fails #"No se puede partir una lista con un solo elemento".

% Se comprueba caso valido en el que Todo es una lista con dos elementos (solo debe dar una solucion).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b]) + not_fails #"Lista dividida satisfactoriamente".

% Se comprueba caso valido en el que Todo es una lista con 4 elementos (debe dar todas las soluciones).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b,c,d]) + not_fails #"Lista dividida satisfactoriamente".

% tests parentesis(Parte, Num, ParteNum)

% Se comprueba caso erroneo en el que Parte no es una lista.

:- test parentesis(Parte, Num, ParteNum) : (Parte=1,Num=1) + fails #"No se puede aplicar parentesis".

% Se comprueba caso erroneo en el que Num no sea un numero.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=a) + fails #"No se puede aplicar parentesis".

% Se comprueba caso valido para el primer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=3) + not_fails #"Parentesis aplicado satisfactoriamente."

% Se comprueba caso valido para el segundo ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b],Num=2) + not_fails #"Parentesis aplicado satisfactoriamento".

% Se comprueba caso valido para el tercer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a],Num=2) + not_fails #"Parentesis aplicado satisfactoriamente".

% tests se_repite(Cs, Parte, Num0, Num)

% Se comprueba caso erroneo en el que Cs no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=1,Parte=[a,b],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia".

% Se comprueba caso erroneo en el que Parte no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b],Parte=1,Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia".

% Se comprueba caso erroneo en el que Parte no es subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[c,d],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia".

% Se comprueba caso erroneo en el que Parte no es la unica subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b,c],Parte=[a,b],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia".

% Se comprueba caso valido en el que se reconoce una secuencia simple.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,a],Parte=[a],Num0=0) + not_fails #"Secuencia reconocida satisfactoriamente".

% Se comprueba caso valido en el que se reconoce una secuencia de varios caracteres.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=0) + not_fails #"Secuencia reconocida satisfactoriamente".

% Se comprueba caso valido en el que se reconoce una secuencia con para Num0 distinto de 0.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=5) + not_fails #"Secuencia reconocida satisfactoriamente".

% tests repeticion(Inicial, Comprimida)

% Se comprueba caso erroneo en el que no hay repeticiones.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + fails #"No se pudo comprimir la lista por repeticion".

% Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #"Lista comprimida por repeticion satisfactoriamente".

% Se comprueba caso valido en el que se repite una secuencia del mismo caracter.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,a,a,a,a,a]) + not_fails #"Lista comprimida por repeticion satisfactoriamente".

% Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #"Lista comprimida por repeticion satisfactoriamente".

% tests compresion(Inicial, Comprimida)

% Junto con las comprobaciones de compresion se puede testear el correcto funcionamiento de division.

% Se comprueba caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + not_fails #"Secuencia comprimida satisfactoriamente (es la misma que la de entrada)".

% Se comprueba caso valido en el que se comprime una secuencia de 3 caracteres 2 veces.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Secuencia comprimida satisfactoriamente".

% Se comprueba caso valido en el que se combina repeticion y division.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,a,b,a,b,c,c,c]) + not_fails #"Secuencia comprimida satisfactoriamente (es la misma que la de entrada)".

% tests comprimir(Inicial, Comprimido)}

% Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% tests mejor_compresion_memo(Inicial, Comprimido)}

% Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima".

% Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima".




%%%%%%%%%%%%%%%%%
%%%           %%%
% Documentacion %
%%%           %%%
%%%%%%%%%%%%%%%%%

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
esta se descarta. Sim embargo, si estamos ante una nueva compresion, este se guardara dinamicamente como lema del predicado @var{memo/1}. Se ha optado por guardar la compresion en formato Key, Value siendo Key el tamaño de la lista y value la lista en si. @includedef{get_all_compresions/2}
@end{verbatim}

@subsection{Explicacion get_all_compresions(Inicial, Comprimida)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{sub_compresion_recursiva/2} obteniendo una compresion en @var{New_Comp}.
  2) Si @var{New_Comp} ya ha sido encontrada previamente se failea la busqueda de esta compresion en concreto.
  3) Si @var{New_Comp} no ha sido encontrada previamente, se guarda en @var{memo/1} haciendo uso del predicado @var{store_result/1}
  4) Se haya el tamaño de la lista @var{New_Comp} en @var{CL} haciendo uso del predicado @var{length/2}.
  5) Se verifica que la variable de entrada @var{Comprimido} sea igual al par @var{CL} - @var{New_Comp}.
@end{verbatim}

@subsection{store_result(X)}
@begin{verbatim}
:- pred store_result(X)

Guarda de forma dinamica @var{X} como lema del predicado @var{memo/1}.@includedef{store_result/1}
@end{verbatim}

@subsection{Explicacion store_result(X)}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{assert/1} para almacenar dinamicamente el lema @var{X} en el predicado @var{memo/1}.
@end{verbatim}

@subsection{limpia_memo}
@begin{verbatim}
:- pred limpia_memo

Elimina la memoria dinamica asiganada a los lemas del predicado @var{memo/1}.@includedef{limpia_memo/0}
@end{verbatim}

@subsection{Explicacion limpia_memo}
@begin{verbatim}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{retractall/1} para eliminar la memoria asiganada los lemas del predicado @var{memo/1}.
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

@section{Casos de prueba}
@subsection{partir(Todo, Parte1, Parte2)}
@begin{verbatim}

Se comprueba caso erroneo en el que Todo es una lista vacia

:- test partir(Todo, Parte1, Parte2) : (Todo=[]) + fails #"No se puede partir una lista vacia."

Se comprueba caso erroneo en el que Todo es una lista de un elemento

:- test partir(Todo, Parte1, Parte2) : (Todo=[a]) + fails #"No se puede partir una lista con un solo elemento."

Se comprueba caso valido en el que Todo es una lista con dos elementos (solo debe dar una solucion).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b]) + not_fails #"Lista dividida satisfactoriamente."

Se comprueba caso valido en el que Todo es una lista con 4 elementos (debe dar todas las soluciones).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b,c,d]) + not_fails #"Lista dividida satisfactoriamente."
@end{verbatim}

@subsection{parentesis(Parte, Num, ParteNum)}
@begin{verbatim}

Se comprueba caso erroneo en el que Parte no es una lista.

:- test parentesis(Parte, Num, ParteNum) : (Parte=1,Num=1) + fails #"No se puede aplicar parentesis."

Se comprueba caso erroneo en el que Num no sea un numero.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=a) + fails #"No se puede aplicar parentesis."

Se comprueba caso valido para el primer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=3) + not_fails #"Parentesis aplicado satisfactoriamente."

Se comprueba caso valido para el segundo ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b],Num=2) + not_fails #"Parentesis aplicado satisfactoriamente."

Se comprueba caso valido para el tercer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a],Num=2) + not_fails #"Parentesis aplicado satisfactoriamente."
@end{verbatim}

@subsection{se_repite(Cs, Parte, Num0, Num)}
@begin{verbatim}

Se comprueba caso erroneo en el que Cs no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=1,Parte=[a,b],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia."

Se comprueba caso erroneo en el que Parte no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b],Parte=1,Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia."

Se comprueba caso erroneo en el que Parte no es subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[c,d],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia."

Se comprueba caso erroneo en el que Parte no es la unica subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b,c],Parte=[a,b],Num0=0) + fails #"No se pudo contar el numero de repeticiones de la subsecuencia."

Se comprueba caso valido en el que se reconoce una secuencia simple.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,a],Parte=[a],Num0=0) + not_fails #"Secuencia reconocida satisfactoriamente."

Se comprueba caso valido en el que se reconoce una secuencia de varios caracteres.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=0) + not_fails #"Secuencia reconocida satisfactoriamente."

Se comprueba caso valido en el que se reconoce una secuencia con para Num0 distinto de 0.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=5) + not_fails #"Secuencia reconocida satisfactoriamente."
@end{verbatim}

@subsection{repeticion(Inicial, Comprimida)}
@begin{verbatim}

Se comprueba caso erroneo en el que no hay repeticiones.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + fails #"No se pudo comprimir la lista por repeticion."

Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #"Lista comprimida por repeticion satisfactoriamente."

Se comprueba caso valido en el que se repite una secuencia del mismo caracter.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,a,a,a,a,a]) + not_fails #"Lista comprimida por repeticion satisfactoriamente."

Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #"Lista comprimida por repeticion satisfactoriamente."
@end{verbatim}

@subsection{compresion(Inicial, Comprimida)}
@begin{verbatim}

Junto con las comprobaciones de compresion se puede testear el correcto funcionamiento de division.

Se comprueba caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + not_fails #"Secuencia comprimida satisfactoriamente (es la misma que la de entrada)."

Se comprueba caso valido en el que se comprime una secuencia de 3 caracteres 2 veces.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Secuencia comprimida satisfactoriamente."

Se comprueba caso valido en el que se combina repeticion y division.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,a,b,a,b,c,c,c]) + not_fails #"Secuencia comprimida satisfactoriamente (es la misma que la de entrada)."
@end{verbatim}

@subsection{comprimir(Inicial, Comprimido)}
@begin{verbatim}


Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima."
@end{verbatim}

@subsection{mejor_compresion_memo(Inicial, Comprimido)}
@begin{verbatim}

Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima."

Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #"Se ha obtenido la secuencia comprimida optima."
@end{verbatim}

 ").









