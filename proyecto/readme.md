# Release Plan — LogicFusion (4 semanas, 4 personas)

A continuación te propongo un plan de liberación (release plan) realista, dividido en 4 sprints semanales y con roles asignados. La última semana queda dedicada a integración, pruebas y corrección de errores, tal como pediste.

> **Supuesto de trabajo:** cada persona tiene ~10–12 horas semanales disponibles. El plan asume que el repositorio, la estructura del proyecto y la librería SLLGEN ya están configurados al inicio de la semana 1.

---

## Resumen visual del plan

| Semana | Foco | Entregable principal | Responsable líder |
|--------|------|----------------------|-------------------|
| 1 | Léxico, gramática base y ambiente | Parser funcional + AST | Persona A |
| 2 | Valores, tipos y primitivas de listas/vectores/dicts | Evaluador de expresiones + estructuras de datos | Persona B |
| 3 | Control de flujo, funciones recursivas y SAT | `if/switch/while/for`, `proc`, `solve` | Persona C |
| 4 | Integración, pruebas, errores y sustentación | Interpretador estable + documentación | Persona D |

---

## Semana 1 — Fundamentos: léxico, gramática y ambiente

**Objetivo:** tener un parser que reconozca el lenguaje y un AST navegable, con el ambiente global funcionando.

| Persona | Tareas | Entregable verificable |
|---------|--------|------------------------|
| **A — Líder de sintaxis** | Especificación léxica (identificadores, enteros, flotantes, strings, booleanos, operadores `->`, `and`, `or`, `FNC`, `lambda`, `proc`, `return`, etc.). Gramática en SLLGEN para `<expression>`, definiciones, `GLOBALS` y `PROGRAM`. | Archivo de gramática + `scan&parse` de ejemplos del PDF (variables, constantes, `proc`, `lambda`). |
| **B — Ambiente y tipos** | Diseño de la estructura de ambientes (GLOBALS → LOCALS anidados). Definición de tipos: `int`, `float`, `string`, `bool`, `void`, `list<T>`, `vector<T>`, `dict<T,U>`, `sat`, `proc`. Validación de tipo en creación y asignación (`->`). | Módulo de ambientes + tests de validación de tipos (errores esperados). |
| **C — Valores y datos** | Definir la representación interna de valores (enteros, flotantes, strings, booleanos, listas, vectores, diccionarios, instancias SAT). Iniciar primitivas de listas: `empty`, `empty?`, `make-list`, `head`, `tail`, `length`, `ref-list`, `append`. | Módulo de valores + primitivas de listas con ejemplos del adendo. |
| **D — Infraestructura y pruebas** | Crear repo `LogicFusion`, README con integrantes, estructura de carpetas, CI mínimo (o script de ejecución), ejemplos `scan&parse` de cada producción. Coordinar la integración inicial. | Repo configurado + README + carpeta `examples/` con casos de prueba. |

**Riesgo principal:** definir mal la gramática en SLLGEN. **Mitigación:** A y B revisan juntos la gramática antes de cerrar la semana.

---

## Semana 2 — Evaluador, estructuras de datos y primitivas

**Objetivo:** evaluar expresiones completas y tener operativas listas, vectores y diccionarios con sus primitivas.

| Persona | Tareas | Entregable verificable |
|---------|--------|------------------------|
| **A — Expresiones y operadores** | Evaluador de expresiones aritméticas (`+ - * /`), relacionales (`== != > < >= <=`), asignación `->`, concatenación de strings y `longitud`. | `scan&parse` + evaluación de expresiones con ejemplos del PDF. |
| **B — Vectores** | Primitivas: `vector?`, `make-vector`, `ref-vector`, `set-vector`, `append-vector`, `delete-val-vector`, `insert-val-vector`. Validación de tipo en mutaciones. | Tests de vectores con los ejemplos del adendo (sección 7). |
| **C — Diccionarios** | Primitivas: `create-dict`, `dict?`, `ref-dict`, `set-dict`, `keys`, `values`. Validación de claves únicas y tipos `dict<T,U>`. | Tests de diccionarios con los ejemplos del adendo (sección 6). |
| **D — Primitivas de listas restantes + pruebas** | Completar `list?`, `append`, `ref-list`; escribir batería de pruebas de listas inmutables (verificar que no se modifican). Apoyar a C con diccionarios si hay retraso. | Suite de pruebas de listas + reporte de cobertura. |

**Riesgo principal:** inconsistencias entre el sistema de tipos y las primitivas. **Mitigación:** B y C usan la misma función de validación de tipos definida en semana 1.

---

## Semana 3 — Control de flujo, funciones y SAT

**Objetivo:** cerrar la semántica del lenguaje (bloques, control, recursión) e implementar `solve` para SAT.

| Persona | Tareas | Entregable verificable |
|---------|--------|------------------------|
| **A — Secuencias y control** | Implementar `BLOCK`, `LOCALS` (con declaraciones locales y cuerpo), `if/else` (0 = falso, ≠0 = verdadero), `switch/case/default`, `while`, `for`. | Ejemplos del PDF: `factorial`, `triangle`, `while` con `count`, `switch` de colores. |
| **B — Funciones y recursión** | `proc ... = lambda(...) { ... return ... }`, clausuras, paso por valor, recursión (factorial recursivo). Llamadas a procedimientos. | Test de factorial recursivo + `square(a)` del ejemplo global. |
| **C — SAT** | Estructura de instancia SAT (`FNC n (cláusula and ...)`), parser de la construcción, primitiva `solve` con búsqueda exhaustiva (2ⁿ), retorno `(satisfactible (False True ...))` o `(insatisfactible ())`. Validaciones: n positivo, índices 1..n, cláusulas no vacías. | Los dos ejemplos del PDF (satisfactible e insatisfactible) funcionando. |
| **D — Integración temprana** | Integrar módulos de A, B y C en el evaluador principal. Detectar choques de nombres y dependencias circulares. Empezar documentación de uso. | Interpretador que corre los ejemplos de `GLOBALS`/`PROGRAM` del PDF. |

**Riesgo principal:** `solve` mal integrado con el evaluador (tipo `sat` no reconocido). **Mitigación:** C y D definen juntos la interfaz de `solve` al inicio de la semana.

---

## Semana 4 — Integración, pruebas y corrección de errores

**Objetivo:** estabilizar el interpretador, corregir errores y preparar la sustentación. **No se agregan funcionalidades nuevas** salvo ajustes menores justificados.

| Persona | Tareas | Entregable verificable |
|---------|--------|------------------------|
| **A — QA de sintaxis y semántica** | Revisar que todos los ejemplos del PDF corran sin errores. Verificar mensajes de error (tipo incorrecto en creación/asignación, variable inexistente, `const` modificada, índice fuera de rango). | Reporte de bugs + fixes. |
| **B — QA de estructuras de datos** | Probar listas inmutables, vectores mutables y diccionarios con casos límite (vacío, un elemento, tipos `void`). Verificar que `solve` no modifique variables/listas/vectores/dicts. | Suite de regresión completa. |
| **C — QA de control y SAT** | Probar `if/else`, `switch`, `while`, `for`, recursión profunda y ambos ejemplos SAT. Verificar alcance de variables (LOCALS → GLOBALS). | Reporte de bugs de control + fixes. |
| **D — Cierre y sustentación** | README final con instrucciones, ejemplos de `scan&parse` por producción, historial de commits rastreable, preparar demo de sustentación. Coordinar acceso al repo. | Repo listo para entrega + guion de demo. |

**Criterio de salida (Definition of Done):**
- Todos los ejemplos del PDF ejecutan correctamente.
- Los dos ejemplos SAT retornan el resultado esperado.
- Los mensajes de error de tipos y variables funcionan.
- README completo con integrantes y ejemplos.
- Historial de commits muestra participación de las 4 personas.

---

## Notas de coordinación

- **Reunión semanal (30 min):** cierre de sprint, revisión de entregables y ajuste de tareas.
- **Rama por persona:** `feature/nombre` con PR revisado por al menos otra persona antes de mergear a `main` (esto también sirve como evidencia de participación rastreable).
- **Contrato de interfaces:** al final de la semana 1, definir las firmas de las funciones principales (ambiente, valores, primitivas) para que los módulos encajen sin retrabajo.
- **Riesgo global:** si la semana 1 se retrasa, recortar alcance en semana 2 priorizando listas y vectores sobre diccionarios (los diccionarios son 10 pts del adendo, pero las listas y vectores son base del lenguaje).

