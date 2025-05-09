:- module(_,_,[classic,assertions,regtypes]).
:- use_module(library(unittest)).
:- use_package(tabling).

author_data('Most','Tazón','Christian','220220').

% PREDICADO SPLIT
:- table split/3.
:- pred split(+Todo, +Parte1, +Parte2): true
    # "Es cierto si Parte1 y Parte2 son dos subsecuencias no vacías que concatenadas forman la secuencia Todo".
:- doc(split/3, "Comprueba si la lista Todo se puede conseguir mediante una concatenadion de Pred1+Pred2 o Pred2+Pred1").


	%AUX FUNC CONCATENAR
concatenar([],L,L).
concatenar([X|Y],Z,[X|U]) :- concatenar(Y,Z,U).

	%AUX FUNC ASIG
asig(A, A).

% 	%AUX FUNC LENGTH
% length([],0).
% length([_|H], Length):-
% 	length(H, LengthH),
% 	Length is LengthH + 1.

split(Todo, Parte1, Parte2) :-
	concatenar(Parte1, Parte2, Todo),
	Parte1 \= [],
    Parte2 \= [].

:- test split([a,b,c,d,e,f,g,h,i],[a,b,c,d],[e,f,g,h,i])
    : true => true + (not_fails).
:- test split([a,b,12,d,e,f,23,h,i],[a,b,12,d],[e,f,23,h,i])
    : true => true + (not_fails).
:- test split([a,b,c,d,e],[a,b],[c,d,e,f])
    : true => true + fails.

% PREDICADO GROUP
:- table group/3.
:- pred group(+Parte, +Num, +Grupo): true
    # "Grupo es una lista de caracteres y un número que es el resultado de componer la lista Parte con un numero de repeticiones Num,
	Añadiendo parentesis solo si Parte tiene 2 elementos o más".
:- doc(group/3, "Crea la anotación comprimida para una repeticion de un caracter o grupo de caracteres poniendo <...> si es un grupo
de varios caracteres y sin <> si es uno único. Ejemplo: a7 o <abc>19").

group([Element], Num, [Element, Num]) :-
	number(Num).

group([H1|[H2|T]], Num, R) :-
	concatenar([<|[H1|[H2|T]]], [>,Num], R),
	number(Num).


:- test group([a],7,[a,7])
    : true => true + (not_fails, is_det).
:- test group([a],7,X)
    : true => true + (not_fails, is_det).
:- test group(X,7,[a,7])
    : true => true + (not_fails, is_det).
:- test group([a,b,c],7,[<,a,b,c,>,7])
    : true => true + (not_fails, is_det).
:- test group([a,b,c],7,X)
    : true => true + (not_fails, is_det).
:- test group(X,7,[<,a,b,c,>,7])
    : true => true + (not_fails, is_det).
:- test group([a,b,c],X,[<,a,b,c,>,7])
    : true => true + (not_fails, is_det).
:- test group([a],10,[<,a,>,7])
    : true => true + (fails).


% PRED IS_REPEATED
:- table is_repeated/3.
:- pred is_repeated(+Cs, +Parte, +Num): true
    # "Tiene exito si Cs es la secuencia Parte repetida N veces".
:- doc(is_repeated/3, "Crea una lista que es el patron repetido N veces, infiere el patron de una lista y 
el num de repeticiones y mira cuantas veces se ha repetido dado un patron y una lista. Tambien es capaz de sacar posibles
patrones y numeros de repeticion dada una lista.").
is_repeated([], _, 0).
is_repeated([X], [X], 1).
is_repeated(Cs, Parte, Num) :- % falla si Num es incognita
	number(Num),
	Num > 0,
	N1 is Num - 1,
    append(Parte, R, Cs),
    is_repeated(R, Parte, N1).

is_repeated(Cs, Parte, Num) :-
	var(Num),
    append(Parte, R, Cs),
    is_repeated(R, Parte, N1),
    Num is N1 + 1.

:- test is_repeated([a,a,a,a,a],[a],5)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,a,a,a,a,a],[a],6)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,a,a,a,a,a],[a,a],3)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,a,a,a,a,a],[a,a,a],2)
    : true => true + (not_fails, is_det).
:- test is_repeated(X,[a,a,a],2)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,a,a,a,a,a],X,2)
    : true => true + (not_fails, is_det).

:- test is_repeated([a,b,c,a,b,c],[a,b,c],2)
    : true => true + (not_fails, is_det).
:- test is_repeated(X,[a,b,c],2)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,b,c,a,b,c],X,2)
    : true => true + (not_fails, is_det).
:- test is_repeated([a,b,c,a,b,c],[a,b,c],X)
    : true => true + (not_fails, is_det).



% PRED SIMPLE_REPETITION
:- table simple_repetition/2.
:- pred simple_repetition(+Inicial, +Comprimida): true
    # "Es cierto si la secuencia Comprimida es la secuencia Inicial comprimida a solo un grupo de repetición".
:- doc(simple_repetition/2, "Devuelve todas las formas de comprimir posibles (de > 1 repeticion). 
Tambien es capaz de descomprimir.").
simple_repetition(I,C) :-
	split(I,P1,_),
	is_repeated(I,P1,Num),
	Num > 1,
	group(P1,Num,C).

:- test simple_repetition([a,a,a,a,a,a],C)
    : true => true + (not_fails, nondet).

:- test simple_repetition([a,a,a,a],C)
    : true => true + (not_fails, nondet).

% COMPRESION FASE A

%PRED COMPRESS 
:- pred compress(+Inicial, +Comprimida): true
    # "Comprime un texto basado en repeticiones".
:- doc(compress/2, "Calcula todas las compresiones posibles y escoge la compresión más corta").

compress(Inicial, Comprimida) :-
	clean_memo,
	recursive_compression(Inicial, Comprimida).


:- pred recursive_compression(+Inicial, +Comprimida): true
    # "Comprime todo lo que puede y si no puede deja la secuencia sin comprimir".
:- doc(recursive_compression/2, "").

% Probar por repeticiones :
recursive_compression(Inicial, Comprimida) :-
	better_compression_memo(Inicial, C),
	length(C,LC),
	length(Inicial,LI),
	(LC < LI, asig(Comprimida, C), !);
	(asig(Comprimida, Inicial)).

repetition(I,C) :-
	split(I,P1,_),
	is_repeated(I,P1,Num),
	Num > 1,
	recursive_compression(P1, X),
	group(X,Num,C).

:- test recursive_compression([a,a,a,a],[a,4])
    : true => true + (not_fails, is_det).
% :- test recursive_compression([a,a,a,a],[<,a,2,>,2])
%     : true => true + (not_fails, is_det).
% :- test recursive_compression([a,a,a,a],[<,a,a,>,2])
%     : true => true + (not_fails, is_det).
% :- test recursive_compression([a,a,a,a],[a,a,a,a])
%     : true => true + (not_fails, is_det).

:- test recursive_compression([a,a,a,a],C)
    : true => true + (not_fails, nondet).

compression(Inicial, Comprimida) :-
	repetition(Inicial, Comprimida).

compression(Inicial, Comprimida) :-
	division(Inicial, Comprimida).

division(Inicial, Comprimida) :-
	split(Inicial, P1, P2),
	recursive_compression(P1,C1),
	recursive_compression(P2,C2),
	append(C1,C2,Comprimida).


:- test recursive_compression([a,b,b,b,b,a,b,b,b,b],[<,a,b,4,>,2])
    : true => true + (not_fails, is_det).

:- test recursive_compression([a,b,b,b,b,b,b,a,b,b,b,b,b,b],X)
    : true => true + (not_fails, nondet).

:- test recursive_compression([a,b,c,a,b,c,b,b,b,b,a,b,c,a,b,c,b,b,b,b],[<,a,b,c,a,b,c,b,4,>,2])
    : true => true + (not_fails, is_det).

better_compression(Inicial, Comprimida) :-
	findall(ComprimidaCualiquiera, compression(Inicial, ComprimidaCualiquiera), All),
	shortest(All, Comprimida).


shortest([H|T], Mejor):-
	shorterThan(H, T, Mejor).

shorterThan(E, [], E).
shorterThan(E, [H|T], Mejor):-
	length(E, LE),
	length(H, LH),
	LE =< LH,
	shorterThan(E, T, Mejor).
shorterThan(E, [H|T], Mejor):-
	length(E, LE),
	length(H, LH),
	LE > LH,
	shorterThan(H, T, Mejor).

:- dynamic memo/2.
clean_memo :- retractall(memo(_,_)).


better_compression_memo(Inicial, Comprimida) :-
	memo(Inicial, Comprimida), !. % Memorized case -> Dont recaculate

better_compression_memo(Inicial, Comprimida) :-
	better_compression(Inicial, Comprimida),
	assertz(memo(Inicial, Comprimida)). % Guardar respuesta para el futuro

decompress([],[]).

decompress([Char, Num | T], Descomprimida) :-
	is_repeated(X, [Char], Num),
	starts_with(X, Descomprimida, R),
	decompress(T, R),
	number(Num),
	!.

decompress([Char | T], [Char | T2]) :-
	Char \= <,
	Char \= >,
	decompress(T, T2),
	!.

decompress([< | T], Descomprimida):-
	get_pattern(T,1,X), % X es lo de dentro de las < > ( ej a,b,<,a,b,>,2 sacado de <,a,v,<,a,b,>,2,>,3)
	get_rest_of_list(X,T,[>,Num|Rest]), % Y es el resto de la lista (Num,...)
	write([Num|Rest]),
	is_repeated(Y,X,Num),
	decompress(Y, Z), % Z es el descomprimido total del patron de dentro
	decompress(Rest, DescomprimidoResto),
	concatenar(Z,DescomprimidoResto, Descomprimida),
	!.


starts_with([], R, R).
starts_with([L | T1], [L | T2], R) :-
	starts_with(T1, T2, R).


get_pattern(_,0,[]).

get_pattern([>|T],Num, [>|R]) :-
	Num > 1,
	Z is Num - 1,
	get_pattern(T, Z, R).

get_pattern([<|T],Num, [<|R]) :-
	Num > 0,
	Z is Num + 1,
	get_pattern(T, Z, R).

get_pattern([>|T],Num, R) :-
	Num =:= 1,
	Z is Num - 1,
	get_pattern(T, Z, R).

get_pattern([Char|T],Num, [Char|R]) :-
	Char \= '<',
	Char \= '>',
	Num > 0,
	get_pattern(T, Num, R).


get_rest_of_list([], X, X).
get_rest_of_list([A|L1], [A|L2], X) :- %L1 is pattern and L2 is the list that contains it and we want to advance
	get_rest_of_list(L1,L2,X).
	