:- encoding(utf8).
:- module(bnf, [traducir_oracion/3]).
:- use_module(database).
:- use_module(logic).

% ======================================================
% Gramática libre de contexto (DCG)
% ======================================================

oracion(IdiomaOrigen, IdiomaDestino, TradEsp, TradIng) -->
    sn(IdiomaOrigen, IdiomaDestino, TradEspSN, TradIngSN),
    sv(IdiomaOrigen, IdiomaDestino, TradEspSV, TradIngSV),
    { append(TradEspSN, TradEspSV, TradEsp),
      append(TradIngSN, TradIngSV, TradIng) }.

% ======================================================
% Sintagma Nominal (SN)
% ======================================================

sn(IdiomaOrigen, IdiomaDestino, [ArtEsp, SustEsp], [ArtIng, SustIng]) -->
    [ArtEsp, SustEsp],
    {
        articulo(IdiomaOrigen, ArtEsp, Gen, Num),
        traducir_articulo_ctx(ArtIng, ArtEsp, Gen, Num),
        sustantivo(IdiomaOrigen, SustEsp, Gen, Num),
        traducir_sustantivo(SustEsp, SustIng)
    }.

% ======================================================
% Sintagma Verbal (SV)
% ======================================================

sv(IdiomaOrigen, IdiomaDestino, [VerEsp], [VerIng]) -->
    [VerEsp],
    {
        verbo(IdiomaOrigen, VerEsp, Persona, Num),
        traducir_verbo(VerEsp, VerIng)
    }.

% ======================================================
% Predicado principal
% ======================================================

traducir_oracion(ListaPalabras, IdiomaOrigen, Traduccion) :-
    (IdiomaOrigen == esp -> IdiomaDestino = eng ; IdiomaDestino = esp),
    phrase(oracion(IdiomaOrigen, IdiomaDestino, _, Traduccion), ListaPalabras).
