

% Ejercicio 1


%matriz(+F, +C, -M)


%conLongitud(L, Fila)
conLongitud(L, Fila) :- length(Fila, L).

%%%%% Implementacion 2 matriz %%%%%

crearLista([],0).
crearLista([_|XS],N) :- N>0, N2 is N-1, crearLista(XS,N2).

matriz(0,_,[]).
matriz(F,C,M):- F>0, F2 is F-1, crearLista(L2, C), matriz(F2, C, L3), append([L2],L3,M).


% Ejercicio 2
%%%%% Implementacion 2 replicar %%%%%
replicar(_,0,[]).
replicar(X, N, [X|XS]) :- N>0, N2 is N-1, replicar(X,N2,XS).


% Ejercicio 3

%%%%% Implementacion 1 transponer %%%%%
transponer([], []).
transponer(M, []) :- filasVacias(M).
transponer(M, [Heads|MTs]) :- extraerColumna(M, Heads, Tails), transponer(Tails, MTs).

extraerColumna([], [], []).
extraerColumna([[H|T]|Ms], [H|Hs], [T|Ts]) :- extraerColumna(Ms, Hs, Ts).

filasVacias([]).
filasVacias([[]|Ms]) :- filasVacias(Ms).

% columnas_a_filas(+Matriz, -Fila, -RestoMatriz)
columnas_a_filas([], [], []).
columnas_a_filas([[H | T] | RestoMatriz], [H | RestoFilaTranspuesta], [T | RestoMatrizTranspuesta]) :-
    columnas_a_filas(RestoMatriz, RestoFilaTranspuesta, RestoMatrizTranspuesta).

% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).


% Ejercicio 4

%%%%% Implementacion 1 pintadasValidas %%%%%

pintadasValidas(r([], L)) :- length(L, Len), replicar(o, Len, L).

pintadasValidas(r([E], L)) :- length(L, Len),
                              E =< Len,
                              CantidadLibre is Len - E,
                              between(0, CantidadLibre, Inicio),
                              replicar(o, Inicio, PrefijoSinPintar),
                              replicar(x, E, ListaPintada),
                              append(PrefijoSinPintar, ListaPintada, InicioListaFinal),
                              RestLen is Len - Inicio - E,
                              replicar(o, RestLen, RestoLista),
                              append(InicioListaFinal, RestoLista, L).

pintadasValidas(r([E1,E2|Es], L)) :-  length(L, N),
                                      sumlist([E1,E2|Es], CantidadX),
                                      length([E1,E2|Es], CantidadBloquesX),
                                      Separadores is CantidadBloquesX - 1,
                                      MaxIndiceInicio is N - (CantidadX + Separadores),
                                      MaxIndiceInicio >= 0,
                                      between(0, MaxIndiceInicio, Inicio),
                                      replicar(o, Inicio, PrefijoSinPintar),
                                      replicar(x, E1, ListaPintada),
                                      append(PrefijoSinPintar, ListaPintada, InicioListaFinal),
                                      append(InicioListaFinal, [o|RestoL], L),
                                      pintadasValidas(r([E2|Es], RestoL)).

%listasDeXs([],[]).
%listasDeXs([X|XS],[L|LS]) :- replicar('x',X,L), listasDeXs(XS,LS).

%pintadasValidas(r(R,L)) :- 
%	listasDeXs(R,XS), 
%	length(L, Total),
%	length(R, CantSegmentos),
%	sumlist(R, CantXs),
%	CantOs is Total-CantXs,
%	pintadasValidasAux(XS, CantOs, CantSegmentos,L1). 

%pintadasValidasAux([X1,X2|XS], CantOs, CantSegmentos, L) :- 
%	between(CantSegmentos,CantOs,CO),
%	replicar('o',CO,OS),
%	append()


%%%%% Implementacion 2 pintadasValidas %%%%%

% pintadasValidas(+R)
% length(L,5), pintadasValidas(r([3], L)), mostrarFila(L)

%prefijo(?P,+L)
prefijo(P,L):- append(P, _ ,L).

%sufijo(?S,+L)
sufijo(S,L):- append(_, S, L).

%sublista(?S,+L)
sublista(S,L):- prefijo(P,L), sufijo(S,P).

% Ejercicio 5
resolverNaive(nono(_, RS)) :- maplist(pintadasValidas, RS).


% Ejercicio 6
% idea: genero todas las pintadas validas con findall (asumo que tiene sentido) 
% y despues voy procesando de a 2 listas y combinando posicion a posicion con CombinarCelda.
% otra que se me acaba de ocurrir pero no la veo clara es que si genero las pintadas validas, 
% el resultado lo trato como una matriz, traspongo la matriz y veo una lista que tiene todo x, 
% entonces eso deberia ser una posicion con un c. (ver seccion 1.3 del enunciado)
%

pintarObligatorias(r(R, L)) :- 
	findall(L, pintadasValidas(r(R, L)), TodasLasPintadasValidas), 
	combinar(TodasLasPintadasValidas, L).


combinar([L],L).
combinar([E1,E2],L):- maplist(combinarCelda, E1, E2, L).
combinar([E1,E2|ES], L) :- length(ES, N), N =\= 0, maplist(combinarCelda, E1, E2, L1), combinar([L1|ES], L).

% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
deducir1Pasada(nono(M, RS)) :- maplist(pintarObligatorias, RS).

% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo maSs cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).

% Ejercicio 8


% restriccionConMenosLibres(+NN,-R)

restriccionConMenosLibres(nono(NN,RS),R) :- 
	nono(NN1, RS1) = deducirVariasPasadas(nono(NN,RS)), 
	member(R, RS1), member(R2, RS1), R2 =\= R, not(cantidadVariablesLibres(R2) < cantidadVariablesLibres(R)).
	%CantVL1 is cantidadVariablesLibres()
	%not(CantVL is cantidadVariablesLibres(R) < deducir1Pasada(L, RES), cantidadVariablesLibres(RES)). 
% Ejercicio 9
resolverDeduciendo(NN) :- completar("Ejercicio 9").
% Ejercicio 10
solucionUnica(NN) :- completar("Ejercicio 10").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').
