:- encoding(utf8).

:- module(bnf, [traducir_oracion/3]).

:- use_module(database).
:- use_module(logic).

% ======================================================
% Gramática libre de contexto (CFG / DCG)
% ======================================================

% Oración: un Sintagma Nominal (SN) seguido de un Sintagma Verbal (SV)
oracion(IdiomaOrigen, IdiomaDestino, TradEsp, TradIng) -->
    sn(IdiomaOrigen, IdiomaDestino, TradEspSN, TradIngSN),
    sv(IdiomaOrigen, IdiomaDestino, TradEspSV, TradIngSV),
    { append(TradEspSN, TradEspSV, TradEsp),
      append(TradIngSN, TradIngSV, TradIng) }.

% ======================================================
% Sintagma Nominal (SN)
% ======================================================
sn(IdiomaOrigen, IdiomaDestino, [ArtEsp|RestoEsp], [ArtIng|RestoIng]) -->
    [ArtEsp],
    { articulo(IdiomaOrigen, ArtEsp, _, _),
      articulo(IdiomaDestino, ArtIng, _, _) },
    opcion_adjetivo(IdiomaOrigen, IdiomaDestino, RestoEsp, RestoIng).

opcion_adjetivo(IdiomaOrigen, IdiomaDestino, [AdjEsp, SustEsp], [AdjIng, SustIng]) -->
    [AdjEsp, SustEsp],
    { adjetivo(IdiomaOrigen, AdjEsp, _, _),
      adjetivo(IdiomaDestino, AdjIng, _, _),
      sustantivo(IdiomaOrigen, SustEsp, _, _),
      sustantivo(IdiomaDestino, SustIng, _, _) }.

opcion_adjetivo(IdiomaOrigen, IdiomaDestino, [SustEsp], [SustIng]) -->
    [SustEsp],
    { sustantivo(IdiomaOrigen, SustEsp, _, _),
      sustantivo(IdiomaDestino, SustIng, _, _) }.

% ======================================================
% Sintagma Verbal (SV)
% ======================================================
sv(IdiomaOrigen, IdiomaDestino, [VerEsp|RestoEsp], [VerIng|RestoIng]) -->
    [VerEsp],
    { verbo(IdiomaOrigen, VerEsp, _, _),
      verbo(IdiomaDestino, VerIng, _, _) },
    opcional_sn(IdiomaOrigen, IdiomaDestino, RestoEsp, RestoIng).
sv(_, _, [], []) --> [].

opcional_sn(IdiomaOrigen, IdiomaDestino, TradEsp, TradIng) -->
    sn(IdiomaOrigen, IdiomaDestino, TradEsp, TradIng).
opcional_sn(_, _, [], []) --> [].

% ======================================================
% Predicado principal
% ======================================================
% Traduce una oración entre español e inglés
% Ejemplo: traducir_oracion([el, perro, corre], esp, T).

traducir_oracion(ListaPalabras, IdiomaOrigen, Traduccion) :-
    (IdiomaOrigen == esp -> IdiomaDestino = eng ; IdiomaDestino = esp),
    phrase(oracion(IdiomaOrigen, IdiomaDestino, _, Traduccion), ListaPalabras).
