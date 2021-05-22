[☰](#)

[↑](codigo.html)[←](codigofulltoc.html)[→](codigorefs.html)[🔍](codigosearch.html)[TOC](codigofulltoc.html)

* * * * *

-   [Compresion de Secuencias](codigo.html) »\
-   [**codigo**]()

* * * * *

*ON THIS PAGE*

[Predicados y explicacion de
algoritmos](#Predicados%20y%20explicacion%20de%20algoritmos)

-   [partir(Todo, Parte1, Parte2)](#partir(Todo,%20Parte1,%20Parte2))
-   [Explicacion partir(Todo, Parte1,
    Parte2)](#Explicacion%20partir(Todo,%20Parte1,%20Parte2))
-   [parentesis(Parte, Num,
    ParteNum)](#parentesis(Parte,%20Num,%20ParteNum))
-   [Explicacion parentesis(Parte, Num,
    ParteNum)](#Explicacion%20parentesis(Parte,%20Num,%20ParteNum))
-   [se\_repite(Cs, Parte, Num0,
    Num)](#se_repite(Cs,%20Parte,%20Num0,%20Num))
-   [Explicacion se\_repite(Cs, Parte, Num0,
    Num)](#Explicacion%20se_repite(Cs,%20Parte,%20Num0,%20Num))
-   [repeticion(Inicial,
    Comprimida)](#repeticion(Inicial,%20Comprimida))
-   [Explicacion repeticion(Inicial,
    Comprimida)](#Explicacion%20repeticion(Inicial,%20Comprimida))
-   [divison(Inicial, Comprimida)](#divison(Inicial,%20Comprimida))
-   [Explicacion division(Inicial,
    Comprimida)](#Explicacion%20division(Inicial,%20Comprimida))
-   [compresion(Inicial,
    Comprimida)](#compresion(Inicial,%20Comprimida))
-   [Explicacion compresion(Inicial,
    Comprimida)](#Explicacion%20compresion(Inicial,%20Comprimida))
-   [mejor\_compresion(Inicial,
    Comprimida)](#mejor_compresion(Inicial,%20Comprimida))
-   [Explicacion mejor\_compresion(Inicial,
    Comprimida)](#Explicacion%20mejor_compresion(Inicial,%20Comprimida))
-   [mejor\_compresion\_memo(Inicial,
    Comprimida)](#mejor_compresion_memo(Inicial,%20Comprimida))
-   [Explicacion mejor\_compresion\_memo(Inicial,
    Comprimida)](#Explicacion%20mejor_compresion_memo(Inicial,%20Comprimida))

[Predicados Auxiliares](#Predicados%20Auxiliares)

-   [get\_all\_compresions(Inicial,
    Comprimido)](#get_all_compresions(Inicial,%20Comprimido))
-   [Explicacion get\_all\_compresions(Inicial,
    Comprimida)](#Explicacion%20get_all_compresions(Inicial,%20Comprimida))
-   [store\_result(X)](#store_result(X))
-   [Explicacion store\_result(X)](#Explicacion%20store_result(X))
-   [limpia\_memo](#limpia_memo)
-   [Explicacion limpia\_memo](#Explicacion%20limpia_memo)
-   [head(List, Head)](#head(List,%20Head))
-   [Explicacion head(List, Head)](#Explicacion%20head(List,%20Head))

[Casos de prueba](#Casos%20de%20prueba)

-   [partir(Todo, Parte1, Parte2)](#partir(Todo,%20Parte1,%20Parte2))
-   [parentesis(Parte, Num,
    ParteNum)](#parentesis(Parte,%20Num,%20ParteNum))
-   [se\_repite(Cs, Parte, Num0,
    Num)](#se_repite(Cs,%20Parte,%20Num0,%20Num))
-   [repeticion(Inicial,
    Comprimida)](#repeticion(Inicial,%20Comprimida))
-   [compresion(Inicial,
    Comprimida)](#compresion(Inicial,%20Comprimida))
-   [comprimir(Inicial, Comprimido)](#comprimir(Inicial,%20Comprimido))
-   [mejor\_compresion\_memo(Inicial,
    Comprimido)](#mejor_compresion_memo(Inicial,%20Comprimido))

[Usage and interface](#Usage%20and%20interface)

[Documentation on exports](#Documentation%20on%20exports)

[Documentation on multifiles](#Documentation%20on%20multifiles)

[Documentation on imports](#Documentation%20on%20imports)

codigo
======

Compresion de Secuencias es un programa desarrollado con el lenguaje de
programacion Ciao Prolog. Este lenguaje de programacion forma parte del
paradigma de la programacion logica el cual se estudia en la asignatura
de Programacion Declarativa: Logica y restricciones de la ETSIINF UPM.

El proyecto consiste en la contruccion de un programa que permita
comprimir secuencias de caracteres en secuencias que definan de forma
mas compacta las mismas. Así, la secuencia aaaaaaa se comprime en a7. La
secuencia original es de longitud siete, la comprimida tiene solo
longitud dos. La secuencia ababab, de longitud seis, se comprime en
(ab)3, de longitud cinco. Nótese que los paréntesis se cuentan también
como caracteres de la secuencia. Solo hacen falta paréntesis si la
subsecuencia que se repite es de más de un carácter. Para comprimir
secuencias complejas, se hace una division de tal manera que se pueda
comprimir las partes divididas para luego juntarlas en el resultado
final de la compresion. Por ejemplo, la secuencia aaaaaaa se comprime en
la secuencia a7. En la compresión por división se divide la secuencia
original en dos partes que a su vez se comprimen por separado y se unen
los resultados. Por ejemplo, la secuencia aaaaaaabbbbbbb se comprime en
a7b7 (comprimiendo cada parte, a su vez, por repetición). La secuencia
aaabaaab se comprime por repetición en (a3b)2 donde la subsecuencia aaab
se ha comprimido en a3b por división (y a su vez aaa en a3 por
repetición). Los resultados de la compresión (tanto de la secuencia
original como de sus subsecuencias) han de ser más cortos que las
secuencias iniciales. Así, no es admisible comprimir aa en a2, porque
tienen la misma longitud, ni abab en (ab)2, porque esta última es más
larga. Las secuencias se representaran como listas de caracteres. Por
ejemplo, aaa es [a,a,a] y (ab)3 es [’(’,a,b,’)’,3]. En estas listas los
números ocupan una única posición, tengan el numero de dígitos que
tengan. Así, a12 es la lista [a,12] y tiene longitud dos (no tres).

A continuacion, se detalla el codigo y los predicados utilizados para la
resolucion de el enunciado anteriormente expuesto junto con un set de
pruebas para asegurar el correcto funcionamiento del codigo.

Predicados y explicacion de algoritmos
--------------------------------------

### partir(Todo, Parte1, Parte2)

``` {.lpdoc-codeblock}
:- pred partir(Todo, Parte1, Parte2)

Se verifica si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacias que concatenadas forman la secuencia @var{Todo}.@includedef{partir/3}
```

### Explicacion partir(Todo, Parte1, Parte2)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Comprobacion de que @var{Parte1} no es una lista vacia.
  2) Comprobacion de que @var{Parte2} no es una lista vacia.
  3) Utilizar el predicado @var{append/2} para encontrar las listas que concatenadas formen la lista @var{Todo}.
```

### parentesis(Parte, Num, ParteNum)

``` {.lpdoc-codeblock}
:- pred parentesis(Parte, Num, ParteNum)

Compone la lista de elementos @var{Parte} con el numero de repeticiones dados por la variable @var{Num} envolviendo a esta lista con parentesis solo si esta tiene 2 elementos o mas. @includedef{parentesis/3}
```

### Explicacion parentesis(Parte, Num, ParteNum)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se checkea que @var{Num} sea efectivamente un numero entero positivo.
  2) Se comprueba el size de la lista @var{Parte} y se bifurca entre size = 1 o size > 1.
  3) Si @var{Parte} es de size 1, se hace un corte para no envolverla en parentesis y se concatena el valor de @var{Parte} con el numero de @var{Num} en @var{ParteNum}.
  4) Si @var{Parte} es de size > 1, se concatenan parentesis por delante y por detras de la lista en @var{ParteNum}.
```

### se\_repite(Cs, Parte, Num0, Num)

``` {.lpdoc-codeblock}
:- pred se_repite(Cs, Parte, Num0, Num)

Se verifica si @var{Cs} se obtiene por repetir N veces la secuencia @var{Parte}. El argumento @var{Num} incrementa @var{Num0} en N. @includedef{se_repite/4}
```

### Explicacion se\_repite(Cs, Parte, Num0, Num)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se obtiene en @var{X} la lista que concatenada a @var{Parte} forma la lista completa @var{Cs}.
  2) Se incrementa en @var{Num1} el valor de @var{Num0}.
  3) Se llama recursivamente a se_repite/4 con las variables @var{X}, @var{Parte}, @var{Num1}, @var{Num}.
  4) Cuando @var{Cs} esta vacia, se iguala los valores de @var{Num0} y @var{Num}.
```

### repeticion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
:- pred repeticion(Inicial, Comprimida)

Se basa en los predicados anteriormente detallados @var{partir/3} y @var{se_repite/4}. Este predicado identifica un prefijo que nos de por repeticion la secuencia inicial.
Se comprime de forma recursiva mediante un llamada a @var{compresion_recursiva/2}. Finalmente se debe componer la parte (comprimida recursivamente) con el numero de repeticiones usando el predicado @var{parentesis/3}. @includedef{repeticion/2}
```

### Explicacion repeticion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama al predicado @var{partir/2} obteniendo asi una seccion de @var{Inicial} en @var{Parte1}.
  2) Se llama a @var{se_repite/4} para identificar si la seccion @var{Parte1} se repite en el conjunto de @var{Inicial} y de ser asi, cuantas veces lo hace en @var{Num}.
  3) Se llama a @var{compresion_recursiva/2} (posteriormente se hace una llamada a un predicado auxiliar por motivos de eficiencia), obteniendo en @var{X} la secuencia @var{Parte1} comprimida.
  4) Por ultimo se hace una llamada a @var{parentesis/3} para envolver a la secuencia @var{X} con su numero de repeticiones @var{Num} y devolverlo en @var{Comprimida}.
```

### divison(Inicial, Comprimida)

``` {.lpdoc-codeblock}
:- pred division(Inicial, Comprimida)

Verifica que la lista @var{Inicial} queda dividida en dos partes y llama a @var{compresion_recursiva/2} de forma recursiva para posteriormente concatenar los resultados obtenidos. @includedef{division/2}
```

### Explicacion division(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{partir/2} de tal forma que se divide @var{Inicial} en las sublistas @var{X} e @var{Y}.
  2) Se llama a @var{compresion_recursiva/2} obteniendo en @var{X1} la compresion de @var{X}.
  3) Se llama a @var{compresion_recursiva/2} obteniendo en @var{Y1} la compresion de @var{Y}.
  4) Por ultimo, se concatenan las listas @var{X1} e @var{Y1} en la lista @var{Comprimida}.
```

### compresion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
:- pred compresion(Inicial, Comprimida)

Llama alternativamente a los predicados @var{repeticion/2} y a @var{division/2} detallados anteriormente.
Esto implica que ademas de considerar las repeticiones, podremos dividir la lista inicial en dos partes y aplicar el algoritmo a cada una de ellas por separado, de esto modo consiguiendo mas posibilidades de encontrar una repeticion. @includedef{compresion/2}
```

### Explicacion compresion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Si la lista @var{Inicial} cuenta con un solo elemento, @var{Comprimida} pasa a ser este elemento.
  2) Se llama a @var{division/2} con los parametros de entrada de este predicado.
  3) Por otro lado se llama a @var{repeticion/2} con los mismos parametros de entrada de este predicado.
```

### mejor\_compresion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
:- pred mejor_compresion(Inicial, Comprimida)

Verifica la busqueda de las compresiones que reduzcan el tamaño. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para postenirmente filtrar la solucion mas corta. @includedef{mejor_compresion/2}.
```

### Explicacion mejor\_compresion(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se usa el predicado @var{findall/3} desde el que se llama a @var{get_all_compresions/2} para obtener todas las compresiones posibles en @var{Comp_List}.
  2) Se hace uso de @var{sort/2} para odenar las comrpesiones de @var{Comp_List} en funcion del tamaño de sus elementos.
  3) Por ultimo hacemos uso del predicado auxiliar @var{head/2} para obtener el primer elemento de @var{Sorted_List} obteniendo asi la lista con menor tamaño en @var{Comprimido}.
```

### mejor\_compresion\_memo(Inicial, Comprimida)

``` {.lpdoc-codeblock}
:- pred mejor_compresion_memo(Inicial, Comprimida)

Verifica la busqueda de las compresiones que reduzcan el tamaño implementado un esquema de memorizacion de lemas. Denotar que en casos de que no sea posible la compresion, se obtendra en @var{Compresion} la lista de entrada @var{Inicial}. Se hace uso del predicado de agregacion @var{findall/3} obteniendo todas las soluciones para posteriormente filtrar la solucion mas corta. @includedef{mejor_compresion_memo/2}
```

### Explicacion mejor\_compresion\_memo(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{limpia_memo/0} para borrar los lemas del predicado @var{memo/2}
  1) Se llama a @var{mejor_compresion/2} para encontrar la mejor compresion.
  2) Se hace un corte para no obtener mas compresiones. De no introducir este corte se obtendia una lista de compresiones ordenadas en funcion de su tamaño, lo que es interesante pero no es lo que se pide. @includedef{mejor_compresion_memo}
  NOTA: es importante denotar que el predicado @var{mejor_compresion/2} ya hace uso de la memorizacion de lemas gracias a @var{get_all_compresions/2} por lo que este proceso sera detallado posteriormente en la seccion de predicados auxiliares.
```

Predicados Auxiliares
---------------------

### get\_all\_compresions(Inicial, Comprimido)

``` {.lpdoc-codeblock}
:- pred get_all_compresions(Inicial, Comprimido)

Verifica la busqueda de compresiones de la lista @var{Inicial} y guarda de forma dinamica los resultados encontrados. En caso de que una compresion ya haya sido obtenida anteriormente,
esta se descarta. Sim embargo, si estamos ante una nueva compresion, este se guardara dinamicamente como lema del predicado @var{memo/1}. Se ha optado por guardar la compresion en formato Key, Value siendo Key el tamaño de la lista y value la lista en si. @includedef{get_all_compresions/2}
```

### Explicacion get\_all\_compresions(Inicial, Comprimida)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Se llama a @var{sub_compresion_recursiva/2} obteniendo una compresion en @var{New_Comp}.
  2) Si @var{New_Comp} ya ha sido encontrada previamente se failea la busqueda de esta compresion en concreto.
  3) Si @var{New_Comp} no ha sido encontrada previamente, se guarda en @var{memo/1} haciendo uso del predicado @var{store_result/1}
  4) Se haya el tamaño de la lista @var{New_Comp} en @var{CL} haciendo uso del predicado @var{length/2}.
  5) Se verifica que la variable de entrada @var{Comprimido} sea igual al par @var{CL} - @var{New_Comp}.
```

### store\_result(X)

``` {.lpdoc-codeblock}
:- pred store_result(X)

Guarda de forma dinamica @var{X} como lema del predicado @var{memo/1}.@includedef{store_result/1}
```

### Explicacion store\_result(X)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{assert/1} para almacenar dinamicamente el lema @var{X} en el predicado @var{memo/1}.
```

### limpia\_memo

``` {.lpdoc-codeblock}
:- pred limpia_memo

Elimina la memoria dinamica asiganada a los lemas del predicado @var{memo/1}.@includedef{limpia_memo/0}
```

### Explicacion limpia\_memo

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Hacer uso de @var{retractall/1} para eliminar la memoria asiganada los lemas del predicado @var{memo/1}.
```

### head(List, Head)

``` {.lpdoc-codeblock}
:- pred head(List, Head)

Obtiene en @var{Head} el primer elemento de una lista no vacia @var{List}.@includedef{head/2}
```

### Explicacion head(List, Head)

``` {.lpdoc-codeblock}
El algoritmo utilizado para este predicado cuenta con los siguientes pasos:
  1) Establecer una igualdad entre el primer elemeto de @var{Lista} y @var{Head}.
```

Casos de prueba
---------------

### partir(Todo, Parte1, Parte2)

``` {.lpdoc-codeblock}

Se comprueba caso erroneo en el que Todo es una lista vacia

:- test partir(Todo, Parte1, Parte2) : (Todo=[]) + fails #”No se puede partir una lista vacia”.

Se comprueba caso erroneo en el que Todo es una lista de un elemento

:- test partir(Todo, Parte1, Parte2) : (Todo=[a]) + fails #”No se puede partir una lista con un solo elemento”.

Se comprueba caso valido en el que Todo es una lista con dos elementos (solo debe dar una solucion).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b]) + not_fails #”Lista dividida satisfactoriamente”.

Se comprueba caso valido en el que Todo es una lista con 4 elementos (debe dar todas las soluciones).

:- test partir(Todo, Parte1, Parte2) : (Todo=[a,b,c,d]) + not_fails #”Lista dividida satisfactoriamente”.
```

### parentesis(Parte, Num, ParteNum)

``` {.lpdoc-codeblock}

Se comprueba caso erroneo en el que Parte no es una lista.

:- test parentesis(Parte, Num, ParteNum) : (Parte=1,Num=1) + fails #”No se puede aplicar parentesis”.

Se comprueba caso erroneo en el que Num no sea un numero.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=a) + fails #”No se puede aplicar parentesis”.

Se comprueba caso valido para el primer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b,c],Num=3) + not_fails #”Parentesis aplicado satisfactoriamente”.

Se comprueba caso valido para el segundo ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a,b],Num=2) + not_fails #”Parentesis aplicado satisfactoriamente”.

Se comprueba caso valido para el tercer ejemplo del enunciado.

:- test parentesis(Parte, Num, ParteNum) : (Parte=[a],Num=2) + not_fails #”Parentesis aplicado satisfactoriamente”.
```

### se\_repite(Cs, Parte, Num0, Num)

``` {.lpdoc-codeblock}

Se comprueba caso erroneo en el que Cs no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=1,Parte=[a,b],Num0=0) + fails #”No se pudo contar el numero de repeticiones de la subsecuencia”.

Se comprueba caso erroneo en el que Parte no es una lista.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b],Parte=1,Num0=0) + fails #”No se pudo contar el numero de repeticiones de la subsecuencia”.

Se comprueba caso erroneo en el que Parte no es subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[c,d],Num0=0) + fails #”No se pudo contar el numero de repeticiones de la subsecuencia”.

Se comprueba caso erroneo en el que Parte no es la unica subsecuencia de Cs.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b,c],Parte=[a,b],Num0=0) + fails #”No se pudo contar el numero de repeticiones de la subsecuencia”.

Se comprueba caso valido en el que se reconoce una secuencia simple.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,a],Parte=[a],Num0=0) + not_fails #”Secuencia reconocida satisfactoriamente”.

Se comprueba caso valido en el que se reconoce una secuencia de varios caracteres.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=0) + not_fails #”Secuencia reconocida satisfactoriamente”.

Se comprueba caso valido en el que se reconoce una secuencia con para Num0 distinto de 0.

:- test se_repite(Cs, Parte, Num0, Num) : (Cs=[a,b,a,b],Parte=[a,b],Num0=5) + not_fails #”Secuencia reconocida satisfactoriamente”.
```

### repeticion(Inicial, Comprimida)

``` {.lpdoc-codeblock}

Se comprueba caso erroneo en el que no hay repeticiones.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + fails #”No se pudo comprimir la lista por repeticion”.

Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #”Lista comprimida por repeticion satisfactoriamente”.

Se comprueba caso valido en el que se repite una secuencia del mismo caracter.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,a,a,a,a,a]) + not_fails #”Lista comprimida por repeticion satisfactoriamente”.

Se comprueba caso valido en el que se repite una secuencia de patrones de 3 caracteres.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c,a,b,c]) + not_fails #”Lista comprimida por repeticion satisfactoriamente”.
```

### compresion(Inicial, Comprimida)

``` {.lpdoc-codeblock}

Junto con las comprobaciones de compresion se puede testear el correcto funcionamiento de division.

Se comprueba caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c]) + not_fails #”Secuencia comprimida satisfactoriamente (es la misma que la de entrada)”.

Se comprueba caso valido en el que se comprime una secuencia de 3 caracteres 2 veces.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,c,a,b,c]) + not_fails #”Secuencia comprimida satisfactoriamente”.

Se comprueba caso valido en el que se combina repeticion y division.

:- test repeticion(Inicial, Comprimida) : (Inicial=[a,b,a,b,a,b,c,c,c]) + not_fails #”Secuencia comprimida satisfactoriamente (es la misma que la de entrada)”.
```

### comprimir(Inicial, Comprimido)

``` {.lpdoc-codeblock}


Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test comprimir(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.
```

### mejor\_compresion\_memo(Inicial, Comprimido)

``` {.lpdoc-codeblock}

Se prueba un caso valido en el que no se puede comprimir por lo que se devuelve la misma secuencia.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprimen secuencias repetidas de 3 caracateres.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,c,a,b,c]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprime por repeticion un solo elemento varias veces.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,a,a,a,a]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.

Se prueba un caso valido en el que se comprime por repeticion y por division varios elementos.

:- test mejor_compresion_memo(Inicial, Comprimido) : (Inicial=[a,b,a,b,a,a,a]) + not_fails #”Se ha obtenido la secuencia comprimida optima”.
```

\

Usage and interface
-------------------

-   **Library usage:**\
    `:- use_module(/home/franser/Documents/uni/3o/Prolog/2-Proyect/codigo.pl).`
-   **Exports:**\
    -   *Predicates:*\
        [`alumno_prode/4`](#alumno_prode/4),
        [`limpia_memo/0`](#limpia_memo/0),
        [`store_result/1`](#store_result/1),
        [`compresion_recursiva/2`](#compresion_recursiva/2),
        [`mejor_compresion_memo/2`](#mejor_compresion_memo/2),
        [`comprimir/2`](#comprimir/2),
        [`mejor_compresion/2`](#mejor_compresion/2),
        [`get_all_compresions/2`](#get_all_compresions/2),
        [`sub_compresion_recursiva/2`](#sub_compresion_recursiva/2),
        [`partir/3`](#partir/3), [`parentesis/3`](#parentesis/3),
        [`se_repite/4`](#se_repite/4), [`repeticion/2`](#repeticion/2),
        [`compresion/2`](#compresion/2), [`division/2`](#division/2),
        [`head/2`](#head/2).
    -   *Multifiles:*\
        [`call_in_module/2`](#call_in_module/2).

Documentation on exports
------------------------

PREDICATE[alumno\_prode/4](codigosearch.html#alumno_prode/4)

No further documentation available for this predicate.

PREDICATE[memo/1](codigosearch.html#memo/1)

No further documentation available for this predicate. The predicate is
of type *dynamic*.\

PREDICATE[limpia\_memo/0](codigosearch.html#limpia_memo/0)

Usage:

Elimina los lemas del predicado memo, que son las compresiones
encontradas para un secuencia dada como input.

``` {.lpdoc-codeblock}
limpia_memo :-
    retractall(memo(_1)).
```

PREDICATE[store\_result/1](codigosearch.html#store_result/1)

Usage:`store_result(X)`

Guarda de forma dinamica X como lema del predicado memo/1.

``` {.lpdoc-codeblock}
store_result(X) :-
    assert(memo(X)).
```

PREDICATE[compresion\_recursiva/2](codigosearch.html#compresion_recursiva/2)

Usage:`compresion_recursiva(Inicial,Comprimido)`

Comprime las listas y recibe llamadas de forma recursiva.

``` {.lpdoc-codeblock}
compresion_recursiva(Inicial,Comprimido) :-
    limpia_memo,
    mejor_compresion(Inicial,Comprimido).
```

PREDICATE[mejor\_compresion\_memo/2](codigosearch.html#mejor_compresion_memo/2)

Usage:`mejor_compresion_memo(Inicial,Comprimido)`

Verifica la busqueda de las compresiones que reduzcan el tamaño
implementado un esquema de memorizacion de lemas. Denotar que en casos
de que no sea posible la compresion, se obtendra en Compresion la lista
de entrada Inicial. Se hace uso del predicado de agregacion findall/3
obteniendo todas las soluciones para posteriormente filtrar la solucion
mas corta.

``` {.lpdoc-codeblock}
mejor_compresion_memo(Inicial,Comprimido) :-
    limpia_memo,
    mejor_compresion(Inicial,Comprimido),
    !.
```

PREDICATE[comprimir/2](codigosearch.html#comprimir/2)

Usage:`comprimir(Inicial,Comprimido)`

Compresion auxiliar.

``` {.lpdoc-codeblock}
comprimir(Inicial,Comprimido) :-
    limpia_memo,
    mejor_compresion(Inicial,Comprimido).
```

PREDICATE[mejor\_compresion/2](codigosearch.html#mejor_compresion/2)

Usage:`mejor_compresion(Inicial,Comprimido)`

Verifica la busqueda de las compresiones que reduzcan el tamaño. Denotar
que en casos de que no sea posible la compresion, se obtendra en
Compresion la lista de entrada Inicial. Se hace uso del predicado de
agregacion findall/3 obteniendo todas las soluciones para postenirmente
filtrar la solucion mas corta.

``` {.lpdoc-codeblock}
mejor_compresion(Inicial,Comprimido) :-
    findall(Y,get_all_compresions(Inicial,Y),Comp_List),
    sort(Comp_List,Sorted_List),
    head(Sorted_List,[_1-Comprimido]).
```

PREDICATE[get\_all\_compresions/2](codigosearch.html#get_all_compresions/2)

Usage:`get_all_compresions(Inicial,Comprimido)`

Verifica la busqueda de compresiones de la lista Inicial y guarda de
forma dinamica los resultados encontrados. En caso de que una compresion
ya haya sido obtenida anteriormente, esta se descarta. Sim embargo, si
estamos ante una nueva compresion, este se guardara dinamicamente como
lema del predicado memo/1. Se ha optado por guardar la compresion en
formato Key, Value siendo Key el tamaño de la lista y value la lista en
si.

``` {.lpdoc-codeblock}
get_all_compresions(Inicial,Comprimido) :-
    sub_compresion_recursiva(Inicial,New_Comp),
    ( memo(New_Comp) ->
        fail
    ; store_result(New_Comp),
      length(New_Comp,CL),
      Comprimido=[CL-New_Comp]
    ).
```

PREDICATE[sub\_compresion\_recursiva/2](codigosearch.html#sub_compresion_recursiva/2)

Usage:`sub_compresion_recursiva(Inicial,Comprimido)`

Obtiene una compresion unicamente

``` {.lpdoc-codeblock}
sub_compresion_recursiva(Inicial,Comprimido) :-
    compresion(Inicial,Comprimido).
sub_compresion_recursiva(Inicial,Inicial).
```

PREDICATE[partir/3](codigosearch.html#partir/3)

Usage:`partir(Todo,Parte1,Parte2)`

Se verifica si Parte1 y Parte2 son dos subsecuencias no vacias que
concatenadas forman la secuencia Todo.

``` {.lpdoc-codeblock}
partir(Todo,Parte1,Parte2) :-
    Parte1=[_1|_2],
    Parte2=[_3|_4],
    append(Parte1,Parte2,Todo).
```

PREDICATE[parentesis/3](codigosearch.html#parentesis/3)

Usage:`parentesis(Parte,Num,ParteNum)`

Compone la lista de elementos Parte con el numero de repeticiones dados
por la variable Num envolviendo a esta lista con parentesis solo si esta
tiene 2 elementos o mas.

``` {.lpdoc-codeblock}
parentesis(Parte,Num,ParteNum) :-
    number(Num),
    length(Parte,1),
    !,
    append(Parte,[Num],ParteNum).
parentesis(Parte,Num,ParteNum) :-
    number(Num),
    append(['('],Parte,ParteAux),
    append(ParteAux,[')',Num],ParteNum).
```

PREDICATE[se\_repite/4](codigosearch.html#se_repite/4)

Usage:`se_repite([],Arg2,Num0,Num0)`

Se verifica si Cs se obtiene por repetir N veces la secuencia Parte. El
argumento Num incrementa Num0 en N.

``` {.lpdoc-codeblock}
se_repite([],_1,Num0,Num0).
se_repite(Cs,Parte,Num0,Num) :-
    append(Parte,X,Cs),
    Num1 is Num0+1,
    se_repite(X,Parte,Num1,Num).
```

PREDICATE[repeticion/2](codigosearch.html#repeticion/2)

Usage:`repeticion(Inicial,Comprimida)`

Se basa en los predicados anteriormente detallados partir/3 y
se\_repite/4. Este predicado identifica un prefijo que nos de por
repeticion la secuencia inicial. Se comprime de forma recursiva mediante
un llamada a compresion\_recursiva/2. Finalmente se debe componer la
parte (comprimida recursivamente) con el numero de repeticiones usando
el predicado parentesis/3.

``` {.lpdoc-codeblock}
repeticion(Inicial,Comprimida) :-
    partir(Inicial,Parte1,_1),
    se_repite(Inicial,Parte1,0,Num),
    sub_compresion_recursiva(Parte1,X),
    parentesis(X,Num,Comprimida).
```

PREDICATE[compresion/2](codigosearch.html#compresion/2)

Usage:`compresion([X],[X])`

Llama alternativamente a los predicados repeticion/2 y a division/2
detallados anteriormente. Esto implica que ademas de considerar las
repeticiones, podremos dividir la lista inicial en dos partes y aplicar
el algoritmo a cada una de ellas por separado, de esto modo consiguiendo
mas posibilidades de encontrar una repeticion.

``` {.lpdoc-codeblock}
compresion([X],[X]) :- !.
compresion(Inicial,Comprimida) :-
    division(Inicial,Comprimida).
compresion(Inicial,Comprimida) :-
    repeticion(Inicial,Comprimida).
```

PREDICATE[division/2](codigosearch.html#division/2)

Usage:`division(Inicial,Comprimida)`

Verifica que la lista Inicial queda dividida en dos partes y llama a
compresion\_recursiva/2 de forma recursiva para posteriormente
concatenar los resultados obtenidos.

``` {.lpdoc-codeblock}
division(Inicial,Comprimida) :-
    partir(Inicial,X,Y),
    sub_compresion_recursiva(X,X1),
    sub_compresion_recursiva(Y,Y1),
    append(X1,Y1,Comprimida).
```

PREDICATE[head/2](codigosearch.html#head/2)

Usage:`head([H|_33642],H)`

Obtiene en Head el primer elemento de una lista no vacia List.

``` {.lpdoc-codeblock}
head([H|_1],H).
```

Documentation on multifiles
---------------------------

PREDICATE[call\_in\_module/2](codigosearch.html#call_in_module/2)

No further documentation available for this predicate. The predicate is
*multifile*.\

Documentation on imports
------------------------

This module has the following direct dependencies:

-   *Application modules:*\
    [`operators`](codigosearch.html#operators),
    [`dcg_phrase_rt`](codigosearch.html#dcg_phrase_rt),
    [`datafacts_rt`](codigosearch.html#datafacts_rt),
    [`dynamic_rt`](codigosearch.html#dynamic_rt),
    [`classic_predicates`](codigosearch.html#classic_predicates),
    [`iso_misc`](codigosearch.html#iso_misc),
    [`lists`](codigosearch.html#lists),
    [`sort`](codigosearch.html#sort).
-   *Internal (engine) modules:*\
    [`term_basic`](codigosearch.html#term_basic),
    [`arithmetic`](codigosearch.html#arithmetic),
    [`atomic_basic`](codigosearch.html#atomic_basic),
    [`basiccontrol`](codigosearch.html#basiccontrol),
    [`exceptions`](codigosearch.html#exceptions),
    [`term_compare`](codigosearch.html#term_compare),
    [`term_typing`](codigosearch.html#term_typing),
    [`debugger_support`](codigosearch.html#debugger_support),
    [`hiord_rt`](codigosearch.html#hiord_rt),
    [`stream_basic`](codigosearch.html#stream_basic),
    [`io_basic`](codigosearch.html#io_basic),
    [`runtime_control`](codigosearch.html#runtime_control),
    [`basic_props`](codigosearch.html#basic_props).
-   *Packages:*\
    [`prelude`](codigosearch.html#prelude),
    [`initial`](codigosearch.html#initial),
    [`condcomp`](codigosearch.html#condcomp),
    [`classic`](codigosearch.html#classic),
    [`runtime_ops`](codigosearch.html#runtime_ops),
    [`dcg`](codigosearch.html#dcg),
    [`dcg/dcg_phrase`](codigosearch.html#dcg/dcg_phrase),
    [`dynamic`](codigosearch.html#dynamic),
    [`datafacts`](codigosearch.html#datafacts),
    [`assertions`](codigosearch.html#assertions),
    [`assertions/assertions_basic`](codigosearch.html#assertions/assertions_basic).

Generated with LPdoc using Ciao