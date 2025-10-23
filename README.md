# TransLog

Un sistema experto de traducción bidireccional desarrollado en Prolog usando programación lógica pura.

## Descripción

TransLog es un sistema experto que traduce oraciones simples entre Español e Inglés en tiempo presente, implementado como parte de la Tarea #3 del curso Paradigmas de Programación (CE3104) del Instituto Tecnológico de Costa Rica.

## Características

- **Traducción bidireccional**: Español ↔ Inglés
- **Análisis sintáctico DCG**: Descomposición de oraciones en sintagmas nominales y verbales
- **Base de datos extensible**: Gran cantidad de palabras
- **Soporte para adjetivos**: Concordancia de género y número
- **Manejo de verbos conjugados**: Presente indicativo simple
- **Validación gramatical**: Concordancia automática de género y número
- **Interfaz interactiva**: Menú intuitivo de línea de comandos

## Estructura del Proyecto

```
TransLog/
├── main.pl              # Interfaz principal y navegación
├── database.pl          # Base de datos de palabras y traducciones
├── logic.pl             # Gramáticas DCG y análisis sintáctico
└── BNF.pl               # Predicados de traducción de oraciones
```

## Instalación

1. Asegúrate de tener SWI-Prolog instalado en tu sistema
2. Clona este repositorio:
   ```bash
   git clone https://github.com/usuario/TransLog.git
   cd TransLog
   ```

## Ejecución

Ejecuta el archivo principal desde la línea de comandos:

```bash
swipl main.pl
```

Luego en la consola de Prolog:

```prolog
?- start.
```

O carga directamente en SWI-Prolog:

```prolog
?- [main].
?- start.
```

## Cómo Usar

1. **Menú Principal**: Selecciona entre traducir español a inglés, inglés a español o salir
2. **Estructura de oraciones**:
   - **Formato**: Artículo + Sustantivo + Verbo (o con adjetivos)
   - **Ejemplo**: "el perro corre" → "the dog runs"
3. **Entrada de datos**: Escribe las oraciones en minúsculas sin puntuación
4. **Regreso**: Escribe "volver" para regresar al menú principal

## Tecnologías

- **Lenguaje**: Prolog
- **Paradigma**: Programación lógica y declarativa
- **Gramáticas**: Gramáticas libres de contexto (DCG)
- **Análisis**: Descomposición sintáctica y unificación

## Desarrollo

**Autores:**
- Steven Aguilar Alvarez
- Allan Zheng Tang  
- Javier Mora Masis

**Institución:** Instituto Tecnológico de Costa Rica  
**Escuela:** Ingeniería en Computadores  
**Curso:** CE1106 - Paradigmas de Programación  
**Semestre:** II-2025

## Documentación Adicional

Para información detallada sobre el uso del sistema, consulte el Manual de Usuario incluido en la carpeta `docs/`.
