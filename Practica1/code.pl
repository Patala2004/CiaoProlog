:- module(_,_,[pure,assertions,regtypes]).
author_data('Most','Tazón','Christian','220220').

:- use_module(library(unittest)).
:- use_package(assertions).


% Settings para lpdoc

% Esconder algunos predicados auxiliares de la documentación
:- comment(hide, [author_data/4,separate_nibbles/2,check_individual_nibble/2,get_nth_bit_aux/3,asig/2,concatenar/3,last/2,remove_last/2,byte_xor_binary/3]).

% Datos básicos del documento
:- doc(title,"Práctica 1: En Binario y Hexadecimal").
:- doc(author, "Christian Most Tazon").
:- doc(module, "Documentacion de la práctica 1 de Christian Most Tazon. Esta práctica trata con operaciones de bytes y listas de bytes, siendo los bytes o una lista de 8 bits o una representacion hexadecimal de estos.").


% Define a binary bit.
:- pred bit(+BinaryValue) : true
    # "define los valores posibles de un bit como b(0) o b(1)".
bit(b(0)).
bit(b(1)).

% Define a binary byte as a list of 8 binary digits.
:- pred binary_byte(+[B1, B2, B3, B4, B5, B6, B7, B8]) : true
    # "define un byte binario como una lista de 8 bits".
:- doc(binary_byte/1, "Se usa para comprobar si un byte está escrito en formato binario").
binary_byte([B7 , B6 , B5 , B4 , B3 , B2 , B1 , B0]) :-
    bit(B7),
    bit(B6),
    bit(B5),
    bit(B4),
    bit(B3),
    bit(B2),
    bit(B1),
    bit(B0).

% Define a hex digit (nibble ) type.
:- pred hexd(+HexValue) : true
    # "define la representacion de valores hexadecimales como h(0), ..., h(f)".
hexd(h(0)).
hexd(h(1)).
hexd(h(2)).
hexd(h(3)).
hexd(h(4)).
hexd(h(5)).
hexd(h(6)).
hexd(h(7)).
hexd(h(8)).
hexd(h(9)).
hexd(h(a)).
hexd(h(b)).
hexd(h(c)).
hexd(h(d)).
hexd(h(e)).
hexd(h(f)).


% Define a hex byte as a list of two hex digits (two nibbles ).
:- pred hex_byte(+[NIBBLE1, NIBBLE2]) : true
    # "define un byte hexadecimal como una lista de dos nibbles / digitos hexadecimales".
:- doc(hex_byte/1, "Se usa para comprobar si un byte está escrito en formato hexadecimal").
hex_byte([H1 , H0]) :-
    hexd(H1),
    hexd(H0).

% PRED 1 -> byte_list(L) -> cierto si L es una lista de bytes (ya sean binarios o hex)

:- pred byte_list(+LISTA_BYTES) : true
    # "Es cierto si @var{LISTA_BYTES} es una lista formada por bytes, ya sean hexadecimales o binarios".
:- doc(byte_list/1, "Predicado usado para comprobar si una lista es una lista de bytes validos").

byte_list([]).
byte_list([H|L]) :- 
    (binary_byte(H) ; hex_byte(H)),
    byte_list(L).

%test byte list
:- test byte_list([]) : true => true + (not_fails, example).
:- test byte_list([[b(0), b(1), b(0), b(1), b(0), b(1), b(1), b(0)]]) : true => true + (not_fails, example).
:- test byte_list([[h(1), h(c)]]) : true => true + (not_fails, example).
:- test byte_list([[b(1), b(1), b(0), b(1), b(0), b(1), b(0), b(0)], [h(c),h(1)]]) : true => true + (not_fails, example).


% PRED 2 -> byte_convert(HexByte, BinByte) -> es cierto si el byte hexadecimal hexbyte
% tiene como representacion binaria binbyte

:- pred byte_convert(+ByteHexadecimal, +ByteBinario) : true
    # "Es cierto si @var{ByteHexadecimal} y @var{ByteBinario} son equivalentes".
:- doc(byte_convert/2, "Predicado usado para comprobar si dos bytes, uno binario y otro decimal, son equivalentes, y también usado para cambiar el formato de un byte").


byte_convert(HexByte, BinByte) :-
    hex_byte(HexByte), binary_byte(BinByte),
    separate_nibbles(HexByte, BinByte).


separate_nibbles([H1, H2], [B1, B2, B3, B4, B5, B6, B7, B8]) :-
    check_individual_nibble(H1, [B1, B2, B3, B4]),
    check_individual_nibble(H2, [B5, B6, B7, B8]).


check_individual_nibble(h(0), [b(0), b(0), b(0), b(0)]).
check_individual_nibble(h(1), [b(0), b(0), b(0), b(1)]).
check_individual_nibble(h(2), [b(0), b(0), b(1), b(0)]).
check_individual_nibble(h(3), [b(0), b(0), b(1), b(1)]).
check_individual_nibble(h(4), [b(0), b(1), b(0), b(0)]).
check_individual_nibble(h(5), [b(0), b(1), b(0), b(1)]).
check_individual_nibble(h(6), [b(0), b(1), b(1), b(0)]).
check_individual_nibble(h(7), [b(0), b(1), b(1), b(1)]).
check_individual_nibble(h(8), [b(1), b(0), b(0), b(0)]).
check_individual_nibble(h(9), [b(1), b(0), b(0), b(1)]).
check_individual_nibble(h(a), [b(1), b(0), b(1), b(0)]).
check_individual_nibble(h(b), [b(1), b(0), b(1), b(1)]).
check_individual_nibble(h(c), [b(1), b(1), b(0), b(0)]).
check_individual_nibble(h(d), [b(1), b(1), b(0), b(1)]).
check_individual_nibble(h(e), [b(1), b(1), b(1), b(0)]).
check_individual_nibble(h(f), [b(1), b(1), b(1), b(1)]).

%tests byte convert

:- test byte_convert([h(0), h(0)], [b(0), b(0), b(0), b(0), b(0), b(0), b(0), b(0)]) : true => true + (not_fails, example).
:- test byte_convert([h(f), h(f)], [b(1), b(1), b(1), b(1), b(1), b(1), b(1), b(1)]) : true => true + (not_fails, example).
:- test byte_convert([h(1), h(2)], [b(0), b(0), b(0), b(1), b(0), b(0), b(1), b(0)]) : true => true + (not_fails, example).
:- test byte_convert([h(3), h(4)], [b(0), b(0), b(1), b(1), b(0), b(1), b(0), b(0)]) : true => true + (not_fails, example).
:- test byte_convert([h(5), h(6)], [b(0), b(1), b(0), b(1), b(0), b(1), b(1), b(0)]) : true => true + (not_fails, example).



% PRED 3 -> byte_list_convert(HL, BL) -> es cierto si la representacion binaria de la lista de hexadecimales
% HL es la lista de representaciones binarias BL

:- pred byte_list_convert(+HexadecimalByteList, +BinaryByteList) : true
    # "Es cierto si @var{HexadecimalByteList} y @var{BinaryByteList} son listas de bytes equivalentes".
:- doc(byte_list_convert/2, "Predicado usado para transformar una lista de bytes de un formato al otro formato. Se usa en predicados que tienen que realizar operaciones binarias sobre bytes que pueden ser hexadecimales").
byte_list_convert([], []).
byte_list_convert([H|HL], [B|BL]) :-
    byte_convert(H,B), byte_list_convert(HL, BL).

:- test byte_list_convert([[h(0),h(1)],[h(f),h(e)]], [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(1)],[b(1),b(1),b(1),b(1),b(1),b(1),b(1),b(0)]])
    : true => true + (not_fails, example).


% PRED 4 -> get_nth_bit_from_byte(N, B, BN) -> es cierto si BN es el bit numero N de B
:- pred get_nth_bit_from_byte(+Index, +Byte, +NthBit) : true
    # "Es cierto si @var{NthBit} es el bit numero @var{Index} del byte @var{Byte}. Se cuenta desde el byte menos significativo (desde la derecha)".
:- doc(get_nth_bit_from_byte/2, "Saca el N-esimo bit menos significativo del byte y lo devuelve en una variable de salida.").
get_nth_bit_from_byte(N, B, BN) :-
    (hex_byte(B) ; binary_byte(B)), bit(BN),
    ((hex_byte(B), byte_convert(B, X));(binary_byte(B), asig(B,X))),
    reverse_byte(X,Y),
    get_nth_bit_aux(N,Y,BN).

get_nth_bit_aux(0, [H|_], H).
get_nth_bit_aux(s(N), [_|B], X) :-
    get_nth_bit_aux(N,B,X). 

asig(A, A).

:- pred reverse_byte(+Byte, +ReversedByte) : true
    # "Es cierto si @var{ReversedByte} es @var{Byte} dado la vuelta".
:- doc(reverse_byte/2, "Da la vuelta a un byte, invirtiendo la significancia de cada bit, y lo devuelve en un parametro de salida.").
reverse_byte([B1, B2, B3, B4, B5, B6, B7, B8], [B8, B7, B6, B5, B4, B3, B2, B1]).

:- test get_nth_bit_from_byte(0,[h(0), h(0)], b(0))
    : true => true + (not_fails, example).

:- test get_nth_bit_from_byte(s(0),[h(0), h(0)], b(0))
    : true => true + (not_fails, example).

:- test get_nth_bit_from_byte(0,[h(f), h(f)], b(0))
    : true => true + (fails, example).

:- test get_nth_bit_from_byte(0,[h(f), h(f)], b(1))
    : true => true + (not_fails, example).

:- test get_nth_bit_from_byte(s(s(s(0))),[b(1), b(1), b(1), b(1), b(0), b(1), b(1), b(1)], b(0))
    : true => true + (not_fails, example).




% auxiliares para pred 5 y 6 byte_list_clsh/crsh

:- pred convert_binary_to_bit_list(+ByteList, +BitList) : true
    # "Es cierto si @var{BitList} es una lista de los bits que forman los bytes de la lista de bytes binarios @var{ByteList}".
:- doc(convert_binary_to_bit_list/2, "Es una funcion auxiliar de 'convert_to_bit_list/2' que solo acepta bytes en formato binario. Saca los bits de los bytes de una lista de bytes y devuelve una lista de estos bits. Se puede entender como que 'unifica' los bits de una lista de bytes en una única entidad para facilitar operaciones a nivel bit sobre un conjunto seguido de bytes.").
convert_binary_to_bit_list([],[]).
convert_binary_to_bit_list([[B1, B2, B3, B4, B5, B6, B7, B8]|BYTL], [B1, B2, B3, B4, B5, B6, B7, B8|BITL]) :-
    convert_to_bit_list(BYTL,BITL).

:- pred convert_to_bit_list(+ByteList, +BitList) : true
    # "Es cierto si @var{BitList} es una lista de los bits que forman los bytes de la lista de bytes en cualquier formato @var{ByteList}".
:- doc(convert_to_bit_list/2, "Saca los bits de los bytes de una lista de bytes y devuelve una lista de estos bits. Se puede entender como que 'unifica' los bits de una lista de bytes en una única entidad para facilitar operaciones a nivel bit sobre un conjunto seguido de bytes. Usa la funcion convert_binary_to_bit_list/2 y convert_to_binary_list/2 para transformar listas de cualquier formato de byte a bits.").
convert_to_bit_list(L1, L2) :-
    convert_to_binary_list(L1, X),
    convert_binary_to_bit_list(X,L2).

:- pred convert_to_binary_list(+ByteList, +BinaryByteList) : true
    # "Es cierto si @var{BinaryByteList} es una lista de bytes en formato binario equivalentes a la lista de bytes @var{ByteList}".
:- doc(convert_to_binary_list/2, "Transforma una lista de bytes a una lista de bytes en formato binario. Para hacer esto transforma bytes de formato hexadecimal a decimal y copia bytes que ya están en formato binario. Debido a esto, la función devuelve siempre lo mismo sin necesidad de revisar que la lista a transformar no está ya en formato binario.").
convert_to_binary_list([],[]).
convert_to_binary_list([E1|L1],[E2|L2]) :-
    ((hex_byte(E1), byte_convert(E1,E2));(asig(E1,E2))),
    convert_to_binary_list(L1,L2).


:- pred convert_to_byte_list(+BitList, +ByteList) : true
    # "Es cierto si @var{ByteList} es una lista de bytes con los mismos bits que @var{BitList}, en el mismo orden".
:- doc(convert_to_byte_list/2, "Junta los bits de BitList en grupos de 8 bits, formando bytes binarios, y devuelve una lista de estos bytes").
convert_to_byte_list([], []).
convert_to_byte_list([B1,B2,B3,B4,B5,B6,B7,B8 | L1], [[B1,B2,B3,B4,B5,B6,B7,B8]| L2]) :-
    convert_to_byte_list(L1, L2).


:- pred convert_binary_list_to_hex(+BinaryByteList, +HexadecimalByteList) : true
    # "Es cierto si @var{HexadecimalByteList} es una lista de bytes en formato hexadecimal equivalente a la lista de bytes en formato binario @var{BinaryByteList} ".
:- doc(convert_binary_list_to_hex/2, "Transforma el formato de los bytes de una lista de bytes al otro. Es util para cuando hay que realizar operaciones binarias sobre listas de bytes en formato hexadecimal.").
convert_binary_list_to_hex([],[]).
convert_binary_list_to_hex([B1|L1], [B2|L2]) :-
    byte_convert(B2,B1),
    convert_binary_list_to_hex(L1,L2).

concatenar([],L,L).
concatenar([X|Y],Z,[X|U]) :- concatenar(Y,Z,U).


:- pred displace_bit_list_to_left(+BitList, +DisplacedList) : true
    # "Es cierto si @var{DisplacedList} es una lista de bits equivalente a @var{BitList} tras un desplazamiento lógico a la izquierda.".
:- doc(displace_bit_list_to_left/2, "Produce un desplazamiento logico a la izquierda de una lista de bits. Mueve el primer bit al final de la lista.").
displace_bit_list_to_left([B|L], L2):-
    concatenar(L, [B], L2).

:- pred displace_bit_list_to_right(+BitList, +DisplacedList) : true
    # "Es cierto si @var{DisplacedList} es una lista de bits equivalente a @var{BitList} tras un desplazamiento lógico a la derecha.".
:- doc(displace_bit_list_to_right/2, "Produce un desplazamiento logico a la derecha de una lista de bits. Mueve el ultimo bit al principio de la lista.").
displace_bit_list_to_right(L1, L2) :-
    last(L1,X),
    concatenar([X], L1, Y),
    remove_last(Y,L2).

last([E],E).
last([_|T], L) :- 
    last(T, L).

remove_last([_], []).
remove_last([H|L1], [H|L2]) :-
    remove_last(L1, L2).



% PRED 5 -> byte_list_clsh(L, CLShL) -> es cierto si CLShL es L trás un desplazamiento lógico a la izquierda
% PRED 6 -> byte_list_crsh(L, CLShL) -> es cierto si CLShL es L trás un desplazamiento lógico a la derecha


:- pred byte_list_clsh(+ByteList, +ByteListLeftShift) : true
    # "Es cierto si la lista de bytes en cualquier formato @var{ByteListLeftShift} es @var{ByteList} tras un desplazamiento lógico a la izquierda a nivel bit.".
:- doc(byte_list_clsh/2, "Produce un desplazamiento lógico a la izquierda a nivel bit de la lista de bytes ByteList. El resultado mantiene el formato de bytes de la entrada, pero la lista tiene que estar formada por bytes de un único tipo de formato.").
byte_list_clsh([B|L], CLshL) :-
    (hex_byte(B) ; binary_byte(B)),
    convert_to_bit_list([B|L], X),
    displace_bit_list_to_left(X, Y),
    convert_to_byte_list(Y,Z),
    ((hex_byte(B), convert_binary_list_to_hex(Z, CLshL)); (binary_byte(B), asig(Z, CLshL))).


:- pred byte_list_crsh(+ByteList, +ByteListRightShift) : true
    # "Es cierto si la lista de bytes en cualquier formato @var{ByteListRightShift} es @var{ByteList} tras un desplazamiento lógico a la derecha a nivel bit.".
:- doc(byte_list_crsh/2, "Produce un desplazamiento lógico a la derecha a nivel bit de la lista de bytes ByteList. El resultado mantiene el formato de bytes de la entrada, pero la lista tiene que estar formada por bytes de un único tipo de formato.").
byte_list_crsh([B|L], CLshL) :-
    (hex_byte(B) ; binary_byte(B)),
    convert_to_bit_list([B|L], X),
    displace_bit_list_to_right(X, Y),
    convert_to_byte_list(Y,Z),
    ((hex_byte(B), convert_binary_list_to_hex(Z, CLshL)); (binary_byte(B), asig(Z, CLshL))).


:- test byte_list_clsh([[h(f),h(0)],[h(0),h(f)]], [[h(e),h(0)],[h(1),h(f)]])
    : true => true + (not_fails).

:- test byte_list_crsh([[h(f),h(0)],[h(0),h(f)]], [[h(f),h(8)],[h(0),h(7)]])
    : true => true + (not_fails).



% PREDICADO 7 -> byte_xor(B1, B2, B3) -> es cierto si B3 es el resultado de B1 xor B2

byte_xor_binary([B11,B12,B13,B14,B15,B16,B17,B18]
, [B21,B22,B23,B24,B25,B26,B27,B28], [B31,B32,B33,B34,B35,B36,B37,B38]):-
    bit_xor(B11,B21,B31),
    bit_xor(B12,B22,B32),
    bit_xor(B13,B23,B33),
    bit_xor(B14,B24,B34),
    bit_xor(B15,B25,B35),
    bit_xor(B16,B26,B36),
    bit_xor(B17,B27,B37),
    bit_xor(B18,B28,B38).


:- pred byte_xor(+Byte1, +Byte2, +ResultByte) : true
    # "devuelve cierto si @var{ResultByte} es el byte resultante de la operación 'XOR' entre los bytes @var{Byte1} y @var{Byte2}".
:- doc(byte_xor/3, "Realiza la operacion 'XOR' entre dos bytes en cualquier formato y devuelve el resultado de la operacion en un parametro de salida, mantendiendo el formato de los bytes. Todos los bytes tienen que tener el mismo formato.").
byte_xor(B1, B2, B3) :-
    (binary_byte(B1), binary_byte(B2), binary_byte(B3),
    byte_xor_binary(B1,B2,B3));
    (hex_byte(B1), hex_byte(B2), hex_byte(B3),
    byte_convert(B1, X1), byte_convert(B2,X2),
    byte_xor_binary(X1,X2,X3),
    byte_convert(B3,X3)).




:- pred bit_xor(+Bit1, +Bit2, +ResultBit) : true
    # "devuelve cierto si @var{ResultBit} es el but resultante de la operación 'XOR' entre los bit @var{Bit1} y @var{Bit2}".
:- doc(bit_xor/3, "Realiza la operacion lógica 'XOR' entre dos bits y devuelve el bit resultante. Este predicado define la tabla lógica de la operación 'XOR'.").
bit_xor(b(0), b(0), b(0)).
bit_xor(b(0), b(1), b(1)).
bit_xor(b(1), b(0), b(1)).
bit_xor(b(1), b(1), b(0)).

:- test byte_xor([h(f),h(f)],[h(f),h(f)],[h(0),h(0)])
    : true => true + (not_fails).
:- test byte_xor([b(1),b(0),b(1),b(1),b(1),b(1),b(1),b(0)],
                [b(0),b(0),b(1),b(1),b(1),b(0),b(1),b(1)],
                [b(1),b(0),b(0),b(0),b(0),b(1),b(0),b(1)])
    : true => true + (not_fails).

:- test byte_xor([h(0),h(b)],[h(a),h(d)],[h(a),h(6)])
    : true => true + (not_fails).
:- test byte_xor([h(5),h(a)],[h(a),h(d)],[h(f),h(7)])
    : true => true + (not_fails).







