:- module(_,_,[assertions,regtypes]).
alumno_prode('Canibano','Lopez','Jose Alberto','a180192').


%%%%%%%%%%%%%%%%%%%%%%%% Funciones Auxiliares %%%%%%%%%%%%%%%%%%

igual(X,X).

nat(0).
nat(s(X)) :- nat(X).

esPar(0).
esPar(s(s(X))) :- esPar(X).

suma(0,X,X).
suma(s(X),Y,s(Z)) :- suma(X,Y,Z).

longitud([],0).
longitud([_|T],s(N)) :- longitud(T,N).

miembro(X,[X|Xs]).
miembro(X,[Y|Ys]) :- miembro(X,Ys).

eliminar(X,[X|Xs], Xs).
eliminar(X, [Y|Ys], [Y|Zs]):- eliminar(X,Ys,Zs).

agregar_inicio(E,L1,[E|L1]).

%%%%%%%%%%%%%%%%%%%%%%%% Funciones Auxiliares %%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% nums(N,L) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nums(0,[]).
nums(s(N),[s(N)|L]) :- 
	nat(N),
	nums(N,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%% sumlist %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sumlist([],0). 				%antes estab al reves y no funcionab orque m no tenia ningun 						%valor iniclializado, pero al estar asi primero llega acer se 						%iguala y ya M tiene le valor 0 joder

sumlist([H|T],S) :-
	sumlist(T,M),
	suma(H,M,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
































