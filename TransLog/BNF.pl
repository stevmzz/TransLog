:- encoding(utf8).
:- consult('database.pl').
:- consult('logic.pl').

% ======================================================
% Gramática libre de contexto (DCG)
% ======================================================

% === TRADUCCIÓN BÁSICA (ORACIONES NORMALES) ===

traducir_esp_eng(ListaPalabras, Traduccion) :-
    parsear_oracion(ListaPalabras, esp, Oracion, []),
    traducir_oracion(Oracion, esp, eng, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

traducir_eng_esp(ListaPalabras, Traduccion) :-
    parsear_oracion(ListaPalabras, eng, Oracion, []),
    traducir_oracion(Oracion, eng, esp, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

% === TRADUCCIÓN CON NEGACIÓN ===

% traducir_esp_eng_con_negacion(+ListaPalabras, -Traduccion)
% Traduce oraciones con negación de español a inglés

traducir_esp_eng_con_negacion(ListaPalabras, Traduccion) :-
    parsear_oracion_con_negacion(ListaPalabras, esp, Oracion, []),
    traducir_oracion_con_negacion(Oracion, esp, eng, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

% traducir_eng_esp_con_negacion(+ListaPalabras, -Traduccion)
% Traduce oraciones con negación de inglés a español

traducir_eng_esp_con_negacion(ListaPalabras, Traduccion) :-
    parsear_oracion_con_negacion(ListaPalabras, eng, Oracion, []),
    traducir_oracion_con_negacion(Oracion, eng, esp, OracionTrad),
    oracion_a_lista(OracionTrad, Traduccion).

% === TRADUCCIÓN DE PREGUNTAS ===

% traducir_esp_eng_pregunta(+ListaPalabras, -Traduccion)
% Traduce preguntas de español a inglés

traducir_esp_eng_pregunta(ListaPalabras, Traduccion) :-
    parsear_pregunta(ListaPalabras, esp, Pregunta, []),
    traducir_pregunta(Pregunta, esp, eng, PreguntaTrad),
    pregunta_a_lista(PreguntaTrad, Traduccion).

% traducir_eng_esp_pregunta(+ListaPalabras, -Traduccion)
% Traduce preguntas de inglés a español

traducir_eng_esp_pregunta(ListaPalabras, Traduccion) :-
    parsear_pregunta(ListaPalabras, eng, Pregunta, []),
    traducir_pregunta(Pregunta, eng, esp, PreguntaTrad),
    pregunta_a_lista(PreguntaTrad, Traduccion).

% ======================================================
% Predicados auxiliares de reconstrucción
% ======================================================

oracion_a_lista(oracion(SN, SV), Resultado) :-
    sn_a_lista(SN, ListaSN),
    sv_a_lista_con_negacion(SV, ListaSV),
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

% === SINTAGMA VERBAL CON NEGACIÓN ===

% sv_a_lista_con_negacion(+SV, -Lista)
% Convierte SV (normal o negativo) a lista

sv_a_lista_con_negacion(sv_neg(Negacion, Verbo), [Negacion, Verbo]).

sv_a_lista_con_negacion(sv(Verbo), [Verbo]).

% ======================================================
% Conversión de Preguntas a Listas
% ======================================================

% pregunta_a_lista(+Pregunta, -Lista)
% Convierte una pregunta a lista de palabras

pregunta_a_lista(pregunta(SI, SN, SV), Resultado) :-
    SI_lista = [SI],
    sn_a_lista(SN, ListaSN),
    sv_a_lista_con_negacion(SV, ListaSV),
    append(SI_lista, ListaSN, Temp),
    append(Temp, ListaSV, Resultado).