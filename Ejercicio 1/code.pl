:- module(_, _, [pure]).
:- use_module(library(unittest)).
:- use_package(assertions).
:- use_module(library(lists)).

 
author_data('Most','Tazón','Christian','220220').


% Settings para lpdoc
:- doc(title,"Ejercicio 1").
:- doc(author, "Christian Most Tazon").
:- doc(module, "Documentación de los predicados y los testeos del ejercicio 1").


:- pred author_data(+Apellido1, +Apellido2, +Nombre, +NumMatricula) : true
   # " Es cierto si @var{Nombre} @var{Apellido1} @var{Apellido2} con número de matricula @var{NumMatricula} es el autor".
:- doc(author_data/4, "Predicado usado para pasar los datos del autor a deliverit").

:- pred controla(+Persona, +Lugar) : true
   # "Es cierto si @var{Persona} tiene control sobre @var{Lugar}.".
:- pred controla(+Persona, +Negocio) : true
   # "Es cierto si @var{Persona} controla el negocio de @var{Negocio}.".
:- doc(controla/2, "Determina si la persona pasada como primer argumento controla al lugar o negocio pasado como segundo argumento.
\nLos hechos del predicado son que 'corleone' controla 'apuestas' y 'solozzo' controla 'drogas'
\nLas reglas son que si roth apoya a corleone el controlará manhattan y brooklyn, mientras que si
roth apoya a solozzo el controlará bronx y harlem. Además quien controle las apuestas controlará a la policía.").

:- pred elimina(+Persona1, +Persona2) : true
   # "Es cierto si @var{Persona1} puede eliminar a @var{Persona2}.".
:- doc(elimina/2, "Determina si la persona del primer argumento elimina a la persona del segundo.
\nLas reglas son que corleone elimina a solozzo si el controla a manhattan y a brooklyn mientras que solozzo
elimina a corleone si controla las drogas y tiene el apoyo de roth.
\nSolo es cierto que corleone elimina a solozzo.").

:- pred apoya(+Persona1, +Persona2) : true
   # "Es cierto si @var{Persona1} presta apoyo a @var{Persona2}.".
:- doc(apoya/2, "Determina si la persona del primer argumento apoya a la persona del segundo.
\nImportante porque el apoyo de roth proporciona control sobre dos lugares distintos a corleone y solozzo.
\nLa regla es que roth apoya a quien sea que le garantize impunidad.").

:- pred garantiza_impunidad(+Persona) : true
   # "Es cierto si @var{Persona1} puede garantizar impunidad".
:- doc(garantiza_impunidad/1, "Determina si la persona pasada como argumento puede garantizar inmunidad.
\nLa regla es que el que controle a la policia puede garantizar inmunidad.").


% predicado controla
% hechos
controla(corleone, apuestas).
controla(solozzo, drogas).

% reglas
controla(corleone, manhattan) :- apoya(roth, corleone).
controla(corleone, brooklyn) :- apoya(roth, corleone).
controla(solozzo, bronx) :- apoya(roth, solozzo).
controla(solozzo, harlem) :- apoya(roth, solozzo).
controla(X, policia) :- controla(X, apuestas).


% reglas otros predicados
elimina(corleone, solozzo) :- controla(corleone, manhattan), controla(corleone, brooklyn).
elimina(solozzo, corleone) :- controla(solozzo, drogas), apoya(roth, solozzo).

apoya(roth, X) :- garantiza_impunidad(X).

garantiza_impunidad(X) :- controla(X, policia).
% Tests

% Test de hechos básicos y supuestos directos
:- test controla(corleone, apuestas) 
: true => true 
+ (not_fails, is_det, example) 
# "Test de hecho corleone controla apuestas -> Espera verdadero".
:- test controla(solozzo, drogas) : true => true + (not_fails, is_det, example) # "Test de hecho solozzo controla drogas -> Espera verdadero".
:- test controla(solozzo, apuestas) : true => true + (fails, is_det, example) # "Test sobre si solozzo controla apuestas -> Espera falso".
:- test controla(corleone, drogas) : true => true + (fails, is_det, example) # "Test sobre si corleone controla el negocio de drogas -> Espera falso".
:- test controla(roth, apuestas) : true => true + (fails, is_det, example) # "Test sobre si roth controla el negocio de apuestas -> Espera falso".
:- test controla(roth, drogas) : true => true + (fails, is_det, example) # "Test sobre si roth controla el negocio de las drogas -> Espera falso".
:- test controla(personaquenohasalidoantes, apuestas) : true => true + (fails, is_det, example) # "Test sobre si alguien sin declarar controla el negocio de apuestas -> Espera falso".
:- test controla(corleone, cosaquenohasalidoantes) : true => true + (fails, is_det, example) # "Test sobre si alguien sin declarar controla un lugar o negocio sin declarar -> Espera falso".

:- test controla(corleone, policia) : true => true + (not_fails, is_det, example) # "Test sobre si corleone controla la policia -> Es un hecho que corleone controla las apuestas -> Espera verdadero".
:- test garantiza_impunidad(corleone) : true => true + (not_fails, is_det, example) # "Test sobre si corleone puede garantizar impunidad -> Controla la policia -> Espera verdadero".

:- test controla(solozzo, policia) : true => true + (fails, is_det, example).
:- test garantiza_impunidad(solozzo) : true => true + (fails, is_det, example).
:- test controla(roth, policia) : true => true + (fails, is_det, example).
:- test garantiza_impunidad(roth) : true => true + (fails, is_det, example).
