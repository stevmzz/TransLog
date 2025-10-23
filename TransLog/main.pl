:- encoding(utf8).
:- consult('database.pl').
:- consult('logic.pl').
:- consult('BNF.pl').

% ======================================================
% PREDICADO PRINCIPAL
% ======================================================

start :-
    nl,
    write('TransLog v1.0'), nl,
    write('Sistema de Traduccion Logica'), nl,
    nl,
    bucle_menu.

% ======================================================
% BUCLE DEL MENÚ
% ======================================================

bucle_menu :-
    nl,
    write('MENU PRINCIPAL'), nl,
    write('1. Traducir Espanol a Ingles'), nl,
    write('2. Traducir Ingles a Espanol'), nl,
    write('3. Salir'), nl,
    nl,
    write('Selecciona una opcion: '),
    flush_output,
    read_string(user_input, "\n", "\n", _, OpcionStr),
    string_lower(OpcionStr, OpcionLower),
    atom_string(OpcionAtom, OpcionLower),
    procesar_opcion(OpcionAtom),
    bucle_menu.

% ======================================================
% PROCESAMIENTO DE OPCIONES
% ======================================================

procesar_opcion('1') :-
    !,
    nl,
    write('TRADUCIR ESPANOL A INGLES'), nl,
    traducir_modo(ei).

procesar_opcion('2') :-
    !,
    nl,
    write('TRADUCIR INGLES A ESPANOL'), nl,
    traducir_modo(ie).

procesar_opcion('3') :-
    !,
    nl,
    write('Gracias por usar TransLog.'), nl,
    nl,
    halt.

procesar_opcion(_) :-
    nl,
    write('Opcion no valida. Intenta de nuevo.'), nl,
    nl.

% ======================================================
% MODO DE TRADUCCIÓN
% ======================================================

traducir_modo(Modo) :-
    write('Escribe una oracion (o escribe volver para regresar):'), nl,
    write('> '),
    flush_output,
    read_string(user_input, "\n", "\n", _, Entrada),
    string_lower(Entrada, EntradaLower),
    atom_string(EntradaAtom, EntradaLower),
    procesar_oracion(EntradaAtom, Modo).

procesar_oracion(volver, _) :-
    !,
    nl.

procesar_oracion(Entrada, Modo) :-
    atom_string(Entrada, String),
    string_codes(String, Codes),
    atom_codes(AtomLimpio, Codes),
    atom_string(AtomLimpio, StringLimpio),
    split_string(StringLimpio, " ", " \t\n\r", PalabrasString),
    maplist(string_lower, PalabrasString, PalabrasLower),
    maplist(atom_string, PalabrasAtom, PalabrasLower),
    (   validar_entrada(PalabrasAtom)
    ->  procesar_traduccion(PalabrasAtom, Modo)
    ;   nl,
        write('Entrada vacia o invalida.'), nl,
        nl,
        traducir_modo(Modo)
    ).

% ======================================================
% PROCESAR TRADUCCIÓN
% ======================================================

procesar_traduccion(PalabrasAtom, ei) :-
    !,
    (   traducir_esp_eng(PalabrasAtom, Traduccion)
    ->  nl,
        write('Traduccion: '),
        atomic_list_concat(Traduccion, ' ', ResultadoStr),
        write(ResultadoStr),
        nl,
        nl,
        traducir_modo(ei)
    ;   nl,
        write('Error en la traduccion.'), nl,
        nl,
        traducir_modo(ei)
    ).

procesar_traduccion(PalabrasAtom, ie) :-
    !,
    (   traducir_eng_esp(PalabrasAtom, Traduccion)
    ->  nl,
        write('Traduccion: '),
        atomic_list_concat(Traduccion, ' ', ResultadoStr),
        write(ResultadoStr),
        nl,
        nl,
        traducir_modo(ie)
    ;   nl,
        write('Error en la traduccion.'), nl,
        nl,
        traducir_modo(ie)
    ).

% ======================================================
% VALIDACIÓN
% ======================================================

validar_entrada([]) :- !, fail.
validar_entrada(Palabras) :-
    length(Palabras, Len),
    Len > 0.