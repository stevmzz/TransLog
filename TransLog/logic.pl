:- encoding(utf8).

% === SINTAGMA NOMINAL (SN) ===
% sn_esp(-Genero, -Numero, -Estructura)//
% Reconoce un SN completo en español

sn_esp(Gen, Num, sn(Art, Adj1, Sust, Adj2)) -->
    articulo_opt_esp(Gen, Num, Art),
    adjetivos_esp(Gen, Num, Adj1),
    sustantivo_esp(Gen, Num, Sust),
    adjetivos_esp(Gen, Num, Adj2).

% sn_eng(-Genero, -Numero, -Estructura)//
% Reconoce un SN completo en inglés

sn_eng(Gen, Num, sn(Art, Adj1, Sust, Adj2)) -->
    articulo_opt_eng(Gen, Num, Art),
    adjetivos_eng(Gen, Num, Adj1),
    sustantivo_eng(Gen, Num, Sust),
    adjetivos_eng(Gen, Num, Adj2).

% === SINTAGMA VERBAL (SV) ===
% sv_esp(-Estructura)//
% Reconoce un SV completo en español (verbo conjugado)

sv_esp(sv(Verbo)) -->
    verbo_esp(Verbo).

% sv_eng(-Estructura)//
% Reconoce un SV completo en inglés (verbo conjugado)

sv_eng(sv(Verbo)) -->
    verbo_eng(Verbo).

% === SINTAGMA VERBAL CON NEGACIÓN ===
% sv_esp_negativo(-Estructura)//
% Reconoce un SV negativo en español: "no" + verbo

sv_esp_negativo(sv_neg(Negacion, Verbo)) -->
    negacion_esp(Negacion),
    verbo_esp(Verbo).

% sv_eng_negativo(-Estructura)//
% Reconoce un SV negativo en inglés: "not" + verbo

sv_eng_negativo(sv_neg(Negacion, Verbo)) -->
    negacion_eng(Negacion),
    verbo_eng(Verbo).

% === SINTAGMA INTERROGATIVO ===
% si_esp(-Interrogativo)//
% Reconoce un sintagma interrogativo en español

si_esp(Interrogativo) -->
    [Palabra],
    { interrogativo(esp, Palabra), Interrogativo = Palabra }.

% si_eng(-Interrogativo)//
% Reconoce un sintagma interrogativo en inglés

si_eng(Interrogativo) -->
    [Palabra],
    { interrogativo(eng, Palabra), Interrogativo = Palabra }.

% === ARTÍCULOS (OPCIONALES) ===
% articulo_opt_esp(-Genero, -Numero, -Articulo)//

articulo_opt_esp(Gen, Num, Art) -->
    [Palabra],
    { articulo(esp, Palabra, Gen, Num), Art = Palabra }.

articulo_opt_esp(_, _, vacio) -->
    [].

% articulo_opt_eng(-Genero, -Numero, -Articulo)//

articulo_opt_eng(Gen, Num, Art) -->
    [Palabra],
    { articulo(eng, Palabra, Gen, Num), Art = Palabra }.

articulo_opt_eng(_, _, vacio) -->
    [].

% === ADJETIVOS (OPCIONALES Y MÚLTIPLES) ===
% adjetivos_esp(-Genero, -Numero, -ListaAdjetivos)//
% Parsea cero o más adjetivos en español, respetando concordancia

adjetivos_esp(Gen, Num, [Adj|Rest]) -->
    [Palabra],
    { adjetivo(esp, Palabra, Gen, _) },
    !,
    { Adj = Palabra },
    adjetivos_esp(Gen, Num, Rest).

adjetivos_esp(_, _, []) -->
    [].

% adjetivos_eng(-Genero, -Numero, -ListaAdjetivos)//
% Parsea cero o más adjetivos en inglés

adjetivos_eng(Gen, Num, [Adj|Rest]) -->
    [Palabra],
    { adjetivo(eng, Palabra, Gen, _) },
    !,
    { Adj = Palabra },
    adjetivos_eng(Gen, Num, Rest).

adjetivos_eng(_, _, []) -->
    [].

% === SUSTANTIVOS ===
% sustantivo_esp(-Genero, -Numero, -Sustantivo)//

sustantivo_esp(Gen, Num, Sust) -->
    [Palabra],
    { sustantivo(esp, Palabra, Gen, Num), Sust = Palabra }.

% sustantivo_eng(-Genero, -Numero, -Sustantivo)//

sustantivo_eng(Gen, Num, Sust) -->
    [Palabra],
    { sustantivo(eng, Palabra, Gen, Num), Sust = Palabra }.

% === VERBOS ===
% verbo_esp(-Verbo)//
% Reconoce un verbo conjugado en español

verbo_esp(Verbo) -->
    [Palabra],
    { verbo(esp, Palabra, _, _), Verbo = Palabra }.

% verbo_eng(-Verbo)//
% Reconoce un verbo conjugado en inglés

verbo_eng(Verbo) -->
    [Palabra],
    { verbo(eng, Palabra, _, _), Verbo = Palabra }.

% === NEGACIONES ===
% negacion_esp(-Negacion)//

negacion_esp(Negacion) -->
    [Palabra],
    { negacion(esp, Palabra), Negacion = Palabra }.

% negacion_eng(-Negacion)//

negacion_eng(Negacion) -->
    [Palabra],
    { negacion(eng, Palabra), Negacion = Palabra }.

% === PREDICADOS DE PARSING (SN) ===
% parsear_sn(+Lista, +Idioma, -SN, -Resto)
% Intenta parsear un SN desde una lista de palabras

parsear_sn(Lista, esp, SN, Resto) :-
    phrase(sn_esp(_, _, SN), Lista, Resto).

parsear_sn(Lista, eng, SN, Resto) :-
    phrase(sn_eng(_, _, SN), Lista, Resto).

% === PREDICADOS DE PARSING (SV) ===
% parsear_sv(+Lista, +Idioma, -SV, -Resto)
% Intenta parsear un SV desde una lista de palabras

parsear_sv(Lista, esp, SV, Resto) :-
    phrase(sv_esp(SV), Lista, Resto).

parsear_sv(Lista, eng, SV, Resto) :-
    phrase(sv_eng(SV), Lista, Resto).

% === PREDICADOS DE PARSING (SV CON O SIN NEGACIÓN) ===
% parsear_sv_con_negacion(+Lista, +Idioma, -SV, -Resto)
% Parsea SV que puede ser negativo o normal

parsear_sv_con_negacion(Lista, esp, SV, Resto) :-
    (   phrase(sv_esp_negativo(SV), Lista, Resto)
    ;   phrase(sv_esp(SV), Lista, Resto)
    ).

parsear_sv_con_negacion(Lista, eng, SV, Resto) :-
    (   phrase(sv_eng_negativo(SV), Lista, Resto)
    ;   phrase(sv_eng(SV), Lista, Resto)
    ).

% === PREDICADOS DE DESCOMPOSICIÓN (SN) ===
% descomponer_sn(+SN, -Articulo, -Adjetivos, -Sustantivo, -Genero, -Numero, +Idioma)
% Descompone un SN parseado en sus componentes

descomponer_sn(sn(Art, Adj1, Sust, Adj2), Articulo, Adjetivos, Sustantivo, Genero, Numero, Idioma) :-
    % Extraer artículo
    ( Art = vacio -> Articulo = ninguno ; Articulo = Art ),
    % Extraer todos los adjetivos
    append(Adj1, Adj2, Adjetivos),
    % Extraer sustantivo
    Sustantivo = Sust,
    % Obtener género y número del sustantivo
    sustantivo(Idioma, Sust, Genero, Numero).

% === PREDICADOS DE DESCOMPOSICIÓN (SV) ===
% descomponer_sv(+SV, -Verbo, +Idioma)
% Descompone un SV parseado en sus componentes

descomponer_sv(sv(Verbo), Verbo, Idioma) :-
    verbo(Idioma, Verbo, _, _).

% === PREDICADOS DE DESCOMPOSICIÓN (SV NEGATIVO) ===
% descomponer_sv_negativo(+SV, -Negacion, -Verbo, +Idioma)
% Descompone un SV negativo en sus componentes

descomponer_sv_negativo(sv_neg(Negacion, Verbo), Negacion, Verbo, Idioma) :-
    negacion(Idioma, Negacion),
    verbo(Idioma, Verbo, _, _).

% === PREDICADOS DE DESCOMPOSICIÓN (INTERROGATIVOS) ===
% descomponer_si(+SI, -Interrogativo)
% Descompone un SI en sus componentes

descomponer_si(Interrogativo, Interrogativo) :-
    (interrogativo(esp, Interrogativo) ; interrogativo(eng, Interrogativo)).

% === PREDICADOS DE VALIDACIÓN (SN) ===
% es_sn_valido(+SN, +Idioma)
% Verifica que un SN sea válido en el idioma especificado

es_sn_valido(sn(Art, Adjs1, Sust, Adjs2), Idioma) :-
    % Validar artículo (si existe)
    ( Art = vacio ; articulo(Idioma, Art, _, _) ),
    % Validar todos los adjetivos pre-sustantivo
    validar_lista_adjetivos(Adjs1, Idioma),
    % Validar sustantivo
    sustantivo(Idioma, Sust, _, _),
    % Validar todos los adjetivos post-sustantivo
    validar_lista_adjetivos(Adjs2, Idioma),
    % Validar concordancia entre artículo y sustantivo
    validar_concordancia_sn(Art, Sust, Idioma).

% validar_concordancia_sn(+Articulo, +Sustantivo, +Idioma)
% Verifica concordancia entre artículo y sustantivo

validar_concordancia_sn(vacio, _, _) :- !.

validar_concordancia_sn(Art, Sust, Idioma) :-
    articulo(Idioma, Art, GenArt, NumArt),
    sustantivo(Idioma, Sust, GenSust, NumSust),
    GenArt = GenSust,
    NumArt = NumSust.

% validar_lista_adjetivos(+Lista, +Idioma)
% Verifica que todos los adjetivos en una lista sean válidos

validar_lista_adjetivos([], _).

validar_lista_adjetivos([Adj|Rest], Idioma) :-
    adjetivo(Idioma, Adj, _, _),
    validar_lista_adjetivos(Rest, Idioma).

% === PREDICADOS DE VALIDACIÓN (SV) ===
% es_sv_valido(+SV, +Idioma)
% Verifica que un SV sea válido en el idioma especificado

es_sv_valido(sv(Verbo), Idioma) :-
    verbo(Idioma, Verbo, _, _).

% === PREDICADOS DE VALIDACIÓN (SV NEGATIVO) ===
% es_sv_negativo_valido(+SV, +Idioma)
% Verifica que un SV negativo sea válido

es_sv_negativo_valido(sv_neg(Negacion, Verbo), Idioma) :-
    negacion(Idioma, Negacion),
    verbo(Idioma, Verbo, _, _).

% === PREDICADOS DE RECONSTRUCCIÓN (SN) ===
% reconstruir_sn(+Articulo, +Adjetivos, +Sustantivo, +Idioma, -SN)
% Reconstruye un SN desde sus componentes

reconstruir_sn(Articulo, Adjetivos, Sustantivo, Idioma, sn(Art, Adjetivos, Sustantivo, [])) :-
    ( Articulo = ninguno -> Art = vacio ; Art = Articulo ),
    es_sn_valido(sn(Art, Adjetivos, Sustantivo, []), Idioma).

% === PREDICADOS DE RECONSTRUCCIÓN (SV) ===
% reconstruir_sv(+Verbo, +Idioma, -SV)
% Reconstruye un SV desde sus componentes

reconstruir_sv(Verbo, Idioma, sv(Verbo)) :-
    es_sv_valido(sv(Verbo), Idioma).

% === PREDICADOS DE RECONSTRUCCIÓN (SV NEGATIVO) ===
% reconstruir_sv_negativo(+Negacion, +Verbo, +Idioma, -SV)
% Reconstruye un SV negativo desde sus componentes

reconstruir_sv_negativo(Negacion, Verbo, Idioma, sv_neg(Negacion, Verbo)) :-
    es_sv_negativo_valido(sv_neg(Negacion, Verbo), Idioma).

% === PREDICADOS DE TRADUCCIÓN (SN) ===
% traducir_sn(+SNOrigen, +IdiomaOrigen, +IdiomaDestino, -SNDestino)
% Traduce un SN completo de un idioma a otro

traducir_sn(SNOrigen, IdiomaOrigen, IdiomaDestino, SNDestino) :-
    % Descomponer el SN origen
    descomponer_sn(SNOrigen, Articulo, Adjetivos, Sustantivo, Genero, Numero, IdiomaOrigen),
    % Traducir artículo
    traducir_articulo_con_contexto(Articulo, Genero, Numero, IdiomaOrigen, IdiomaDestino, ArticuloTrad),
    % Traducir adjetivos
    maplist(traducir_palabra(adjetivo), Adjetivos, AdjetivosTrad),
    % Traducir sustantivo
    traducir_palabra(sustantivo, Sustantivo, SustantivoTrad),
    % Reconstruir el SN en el idioma destino
    reconstruir_sn(ArticuloTrad, AdjetivosTrad, SustantivoTrad, IdiomaDestino, SNDestino).

% === PREDICADOS DE TRADUCCIÓN (SV) ===
% traducir_sv(+SVOrigen, +IdiomaOrigen, +IdiomaDestino, -SVDestino)
% Traduce un SV completo de un idioma a otro

traducir_sv(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino) :-
    % Descomponer el SV origen
    descomponer_sv(SVOrigen, Verbo, IdiomaOrigen),
    % Traducir el verbo directamente (ya conjugado)
    traducir_palabra(verbo, Verbo, VerboCorrecto),
    % Reconstruir el SV en el idioma destino
    reconstruir_sv(VerboCorrecto, IdiomaDestino, SVDestino).

% === PREDICADOS DE TRADUCCIÓN (SV NEGATIVO) ===
% traducir_sv_negativo(+SVOrigen, +IdiomaOrigen, +IdiomaDestino, -SVDestino)
% Traduce un SV negativo completo

traducir_sv_negativo(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino) :-
    descomponer_sv_negativo(SVOrigen, Negacion, Verbo, IdiomaOrigen),
    traducir_negacion(Negacion, NegacionTrad),
    traducir_palabra(verbo, Verbo, VerboCorrecto),
    reconstruir_sv_negativo(NegacionTrad, VerboCorrecto, IdiomaDestino, SVDestino).

% === PREDICADOS DE TRADUCCIÓN (INTERROGATIVOS) ===
% traducir_si(+Interrogativo, +IdiomaOrigen, +IdiomaDestino, -InterrogativoTrad)
% Traduce un sintagma interrogativo

traducir_si(Interrogativo, IdiomaOrigen, IdiomaDestino, InterrogativoTrad) :-
    interrogativo(IdiomaOrigen, Interrogativo),
    traducir_interrogativo(Interrogativo, InterrogativoTrad),
    interrogativo(IdiomaDestino, InterrogativoTrad).

% === PREDICADOS AUXILIARES DE TRADUCCIÓN ===
% traducir_articulo_con_contexto(+Articulo, +Genero, +Numero, +IdiomaOrigen, +IdiomaDestino, -ArticuloTrad)
% Traduce un artículo considerando género y número

traducir_articulo_con_contexto(ninguno, _, _, _, _, ninguno) :- !.

traducir_articulo_con_contexto(Articulo, _, _, esp, eng, ArticuloTrad) :-
    traducir_articulo(Articulo, ArticuloTrad).

traducir_articulo_con_contexto(Articulo, Genero, Numero, eng, esp, ArticuloTrad) :-
    traducir_articulo_ctx(Articulo, ArticuloTrad, Genero, Numero).

% traducir_palabra(+Tipo, +Palabra, -PalabraTrad)
% Predicado genérico para traducir palabras según su tipo

traducir_palabra(adjetivo, Palabra, PalabraTrad) :-
    traducir_adjetivo(Palabra, PalabraTrad).

traducir_palabra(sustantivo, Palabra, PalabraTrad) :-
    traducir_sustantivo(Palabra, PalabraTrad).

traducir_palabra(verbo, Palabra, PalabraTrad) :-
    traducir_verbo(Palabra, PalabraTrad).

% === GRAMÁTICAS DE ORACIÓN ===
oracion_esp(oracion(SN, SV)) -->
    sn_esp(_, _, SN),
    sv_esp(SV).

oracion_eng(oracion(SN, SV)) -->
    sn_eng(_, _, SN),
    sv_eng(SV).

% === GRAMÁTICAS ACTUALIZADAS (CON NEGACIÓN) ===
% oracion_esp_con_negacion(-Estructura)//
% Oración en español que puede tener negación

oracion_esp_con_negacion(oracion(SN, SV)) -->
    sn_esp(_, _, SN),
    (   sv_esp_negativo(SV)
    ;   sv_esp(SV)
    ).

% oracion_eng_con_negacion(-Estructura)//
% Oración en inglés que puede tener negación

oracion_eng_con_negacion(oracion(SN, SV)) -->
    sn_eng(_, _, SN),
    (   sv_eng_negativo(SV)
    ;   sv_eng(SV)
    ).

% === GRAMÁTICAS PARA PREGUNTAS ===
% pregunta_esp(-Estructura)//
% Pregunta en español: SI + SN + SV

pregunta_esp(pregunta(SI, SN, SV)) -->
    si_esp(SI),
    sn_esp(_, _, SN),
    (   sv_esp_negativo(SV)
    ;   sv_esp(SV)
    ).

% pregunta_eng(-Estructura)//
% Pregunta en inglés: SI + SN + SV

pregunta_eng(pregunta(SI, SN, SV)) -->
    si_eng(SI),
    sn_eng(_, _, SN),
    (   sv_eng_negativo(SV)
    ;   sv_eng(SV)
    ).

% === PREDICADOS DE PARSING (ORACIÓN) ===
parsear_oracion(Lista, esp, Oracion, Resto) :-
    phrase(oracion_esp(Oracion), Lista, Resto).

parsear_oracion(Lista, eng, Oracion, Resto) :-
    phrase(oracion_eng(Oracion), Lista, Resto).

% === PREDICADOS DE PARSING ACTUALIZADOS ===
% parsear_oracion_con_negacion(+Lista, +Idioma, -Oracion, -Resto)
% Parsea oración que puede tener negación

parsear_oracion_con_negacion(Lista, esp, Oracion, Resto) :-
    phrase(oracion_esp_con_negacion(Oracion), Lista, Resto).

parsear_oracion_con_negacion(Lista, eng, Oracion, Resto) :-
    phrase(oracion_eng_con_negacion(Oracion), Lista, Resto).

% parsear_pregunta(+Lista, +Idioma, -Pregunta, -Resto)
% Parsea una pregunta

parsear_pregunta(Lista, esp, Pregunta, Resto) :-
    phrase(pregunta_esp(Pregunta), Lista, Resto).

parsear_pregunta(Lista, eng, Pregunta, Resto) :-
    phrase(pregunta_eng(Pregunta), Lista, Resto).

% === PREDICADOS DE TRADUCCIÓN (ORACIÓN) ===
traducir_oracion(oracion(SNOrigen, SVOrigen), IdiomaOrigen, IdiomaDestino, oracion(SNDestino, SVDestino)) :-
    traducir_sn(SNOrigen, IdiomaOrigen, IdiomaDestino, SNDestino),
    traducir_sv(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino).

% === PREDICADOS DE TRADUCCIÓN PARA ORACIONES CON NEGACIÓN ===
% traducir_oracion_con_negacion(+OracionOrigen, +IdiomaOrigen, +IdiomaDestino, -OracionDestino)
% Traduce oración completa considerando negación

traducir_oracion_con_negacion(oracion(SNOrigen, SVOrigen), IdiomaOrigen, IdiomaDestino, oracion(SNDestino, SVDestino)) :-
    traducir_sn(SNOrigen, IdiomaOrigen, IdiomaDestino, SNDestino),
    (   SVOrigen = sv_neg(_, _)
    ->  traducir_sv_negativo(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino)
    ;   traducir_sv(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino)
    ).

% === PREDICADOS DE TRADUCCIÓN PARA PREGUNTAS ===
% traducir_pregunta(+PreguntaOrigen, +IdiomaOrigen, +IdiomaDestino, -PreguntaDestino)
% Traduce una pregunta completa

traducir_pregunta(pregunta(SIOrigen, SNOrigen, SVOrigen), IdiomaOrigen, IdiomaDestino, pregunta(SIDestino, SNDestino, SVDestino)) :-
    traducir_si(SIOrigen, IdiomaOrigen, IdiomaDestino, SIDestino),
    traducir_sn(SNOrigen, IdiomaOrigen, IdiomaDestino, SNDestino),
    (   SVOrigen = sv_neg(_, _)
    ->  traducir_sv_negativo(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino)
    ;   traducir_sv(SVOrigen, IdiomaOrigen, IdiomaDestino, SVDestino)
    ).