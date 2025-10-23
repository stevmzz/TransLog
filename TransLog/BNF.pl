:- encoding(utf8).
:- consult('database.pl').
:- consult('logic.pl').

% ======================================================
% Gram�tica libre de contexto (DCG)
% ======================================================

traducir_esp_eng(ListaPalabras, Traduccion) :-
    parsear_oracion(ListaPalabras, esp, Oracion, []),
    traducir_oracion(Oracion, esp, eng, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

traducir_eng_esp(ListaPalabras, Traduccion) :-
    parsear_oracion(ListaPalabras, eng, Oracion, []),
    traducir_oracion(Oracion, eng, esp, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

% ======================================================
% Predicados auxiliares de reconstrucción
% ======================================================

oracion_a_lista(oracion(SN, SV), Resultado) :-
    sn_a_lista(SN, ListaSN),
    sv_a_lista(SV, ListaSV),
    append(ListaSN, ListaSV, Resultado).

% ======================================================
% Sintagma Nominal (SN)
% ======================================================

sn_a_lista(sn(Art, Adj1, Sust, Adj2), Resultado) :-
    ( Art = vacio -> ListaArt = [] ; ListaArt = [Art] ),
    append(ListaArt, Adj1, Temp1),
    append(Temp1, [Sust], Temp2),
    append(Temp2, Adj2, Resultado).

% ======================================================
% Sintagma Verbal (SV)
% ======================================================

sv_a_lista(sv(Verbo), [Verbo]).