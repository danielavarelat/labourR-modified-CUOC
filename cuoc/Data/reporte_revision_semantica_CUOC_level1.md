# Reporte de revisión semántica de términos domain-specific para CUOC Level 1

**Proyecto:** `labourR-modified-CUOC`  
**Componente:** Predicción e imputación de ocupaciones CUOC — clasificación Level 1  
**Versión del reporte:** 1.0  
**Fecha de ejecución:** 29 de julio de 2026  
**Zona horaria:** America/Bogota  
**Modelo utilizado:** GPT-5.6 Thinking  

---

## 1. Resumen ejecutivo

Se realizó una revisión semántica asistida por modelo de lenguaje sobre **4.054 términos candidatos** extraídos de la hoja **`Denominaciones CUOC 2022`**. Cada término aparecía únicamente en uno de los diez grandes grupos o niveles 1 de CUOC.

El objetivo fue determinar si la exclusividad observada dentro del catálogo CUOC también representaba una señal semántica suficientemente específica para utilizar el término como regla previa al TF-IDF de Level 1.

La revisión fue deliberadamente conservadora. Se priorizaron como `keep` los títulos ocupacionales explícitos y las señales de dominio con bajo riesgo de falsos positivos. Los términos contextuales, técnicos, polisémicos o con evidencia insuficiente fueron enviados a `review`, en vez de forzar una decisión automática.

### Resultado general

| Decisión | Cantidad | Porcentaje |
|---|---:|---:|
| `keep` | 1.203 | 29,67 % |
| `review` | 2.755 | 67,96 % |
| `drop` | 96 | 2,37 % |
| **Total** | **4.054** | **100 %** |

Este resultado no debe considerarse todavía un *gold standard* definitivo. Constituye una primera clasificación semántica reproducible y una cola priorizada para validación humana.

---

## 2. Contexto del pipeline

El proyecto busca construir un mecanismo de imputación de ocupaciones basado en la **Clasificación Única de Ocupaciones para Colombia — CUOC 2022**.

El flujo propuesto es:

1. **Level 1:** clasificación amplia entre los diez grandes grupos CUOC, utilizando las denominaciones ocupacionales y TF-IDF.
2. **Level 5:** clasificación granular utilizando la hoja `Descripciones CUOC 2022`, con 678 códigos ocupacionales.
3. **Reglas domain-specific previas:** antes de ejecutar TF-IDF en Level 1, detectar términos altamente específicos que permitan asignar directamente un gran grupo cuando exista una señal inequívoca.

La revisión documentada aquí corresponde exclusivamente al tercer componente.

---

## 3. Objetivo del ejercicio

Clasificar cada término candidato según su utilidad como señal domain-specific para un Level 1 CUOC.

Para cada término se evaluó:

- Si pertenece claramente a un único Level 1.
- Si representa un título ocupacional o una señal directa del dominio.
- Si es demasiado genérico, ambiguo o transversal.
- Si parece ruido, fragmento, error tipográfico o sigla no interpretable.
- Si su utilización aislada podría producir falsos positivos en vacantes reales.
- Si debe conservarse, descartarse o enviarse a revisión manual.

---

## 4. Archivos de entrada

### 4.1. Candidatos

**Archivo:** `terms_denom_freq_level1_one.csv`

Dimensiones:

- 4.054 filas.
- 3 columnas originales:
  - `term`
  - `count`
  - `levels`

Cada fila representa un término que, en el conjunto de denominaciones utilizado, fue observado únicamente en un gran grupo CUOC.

### 4.2. Referencia CUOC

**Archivo:** `Correlativa_CUOC-2022_Vs_CNO-2022.xlsx`

Hojas utilizadas como referencia principal:

- `Denominaciones CUOC 2022`
- `Ocupaciones CUOC 2022`
- `Grupos Primarios - CUOC 2022`

La hoja `Grupos Primarios - CUOC 2022` permitió asociar cada código de gran grupo con sus grupos primarios y nombres oficiales. Las hojas de ocupaciones y denominaciones permitieron recuperar el contexto en el que aparecía cada término.

---

## 5. Prompt específico utilizado

El siguiente fue el prompt de tarea suministrado para el ejercicio:

```text
Necesito que me ayudes con una revisión semántica para el pipeline CUOC del proyecto labourR-modified-CUOC.
Contexto del proyecto
Estamos construyendo un mecanismo de imputación de ocupaciones CUOC para vacantes.
El flujo general es:
level1: usar la hoja Denominaciones CUOC 2022 para hacer TF-IDF y clasificación amplia.
level5: usar la hoja Descripciones CUOC 2022 con 678 códigos para la parte más granular.
Antes de hacer TF-IDF en level1, queremos detectar palabras domain-specific que permitan saltar directo a un level1 específico.
## Tarea específica
Necesito que revises un conjunto de aproximadamente 4054 palabras que aparecen solo en un nivel CUOC, extraídas desde la hoja Denominaciones CUOC 2022.
Esas palabras están en un archivo que te voy a pasar en formato terms_denom_freq_level1_one.csv.
Además, te voy a incluir el Excel Correlativa_CUOC-2022_Vs_CNO-2022.xlsx, especialmente la hoja:
Grupos Primarios - CUOC 2022
Esa hoja contiene la referencia de los level1 con sus nombres oficiales.
## Objetivo de la revisión =
Quiero que clasifiques cada palabra candidata según si realmente debe considerarse domain-specific para un level1 o no.
La idea no es solo filtrar ruido, sino identificar palabras que sí representen señal útil para el nivel 1 y puedan servir para el pipeline de predicción.

## Para cada término, evalúa:
si pertenece claramente a un level1 específico
si es demasiado ambiguo o genérico
si parece ruido, sigla, fragmento o token no útil
si conviene mantenerlo como domain-specific
si conviene descartarlo

## Criterios esperados
Toma como referencia:
el nombre oficial del level1
el significado del término dentro del contexto CUOC
si el término parece útil para clasificar vacantes dentro de este nivel
si puede producir falsos positivos

No asumas que todo token de 3 letras es útil. Algunos sí lo son, pero muchos son ruido o abreviaturas ambiguas.
## Entregable esperado =
- Quiero una salida en tabla con, idealmente, estas columnas:
term
level1_candidate
decision con valores tipo keep, drop, review
reason
confidence

Si puedes, también agrega una columna opcional:
notes o examples
## Reglas importantes
No inventes definiciones que no estén respaldadas por el Excel o por el contexto CUOC.
Si un término es ambiguo, márcalo como review en lugar de forzar una decisión.
Si un término parece claramente ruido, márcalo como drop.
Si un término es una abreviación válida y útil para un level1, márcalo como keep.
```

### Nota sobre el prompt

No se utilizó un segundo prompt externo independiente almacenado como artefacto. La ejecución estuvo guiada por:

1. El prompt anterior.
2. El contexto general del proyecto `labourR-modified-CUOC` disponible en la conversación.
3. Las instrucciones de ser conservador y no inventar definiciones no respaldadas por los archivos.

No se incluyen instrucciones internas del sistema, ya que no forman parte del protocolo de clasificación definido por el proyecto.

---

## 6. Modelo y configuración de ejecución

| Parámetro | Valor |
|---|---|
| Modelo | GPT-5.6 Thinking |
| Interfaz | ChatGPT |
| Fecha | 29 de julio de 2026 |
| Idioma principal | Español |
| Fuentes externas en internet | No utilizadas |
| Archivos suministrados | CSV de candidatos y Excel CUOC 2022 |
| Temperatura | No expuesta por la interfaz |
| Seed | No expuesto por la interfaz |
| Identificador interno exacto del snapshot | No expuesto por la interfaz |

Debido a que la temperatura, el seed y el identificador exacto del snapshot no son configurables ni visibles en esta ejecución, no puede garantizarse que una nueva llamada al modelo reproduzca palabra por palabra las mismas razones o puntuaciones. La reproducibilidad práctica debe apoyarse en el archivo de salida versionado, las reglas descritas en este reporte y una posterior conversión de las decisiones validadas a reglas deterministas.

---

## 7. Metodología

### 7.1. Carga y vinculación de fuentes

Se cargó el CSV de candidatos y se vinculó cada término con la información disponible en el Excel CUOC:

- Gran grupo candidato.
- Nombre oficial del Level 1.
- Denominaciones en las que aparece.
- Ocupaciones y grupos primarios relacionados.

### 7.2. Normalización léxica

Para facilitar la vinculación se utilizaron formas normalizadas, principalmente:

- Conversión a minúsculas.
- Comparación insensible a tildes para el enlace de términos.
- Conservación de las variantes originales en la columna `original_forms`.

La forma normalizada se utilizó para análisis y matching, mientras que las grafías observadas en CUOC se conservaron como evidencia.

### 7.3. Enriquecimiento contextual

Cada término fue enriquecido con variables que permitieran evitar una evaluación completamente aislada:

- `denomination_occurrences`: número de denominaciones en las que aparece.
- `primary_groups`: cantidad de grupos primarios asociados.
- `occupations`: cantidad de ocupaciones asociadas.
- `standalone_title`: indica si aparece como una denominación ocupacional independiente.
- `first_token_ratio`: proporción de apariciones en las que encabeza la denominación.
- `in_official_occupation_name`: presencia en el nombre oficial de una ocupación.
- `in_official_primary_group_name`: presencia en el nombre de un grupo primario.
- `notes_examples`: ejemplos reales de denominaciones CUOC.
- `original_forms`: variantes ortográficas originales observadas.

### 7.4. Criterio semántico conservador

La decisión no se basó únicamente en que el término apareciera en un solo nivel dentro del catálogo. Se distinguió entre:

- **Núcleo ocupacional:** nombre directo de una ocupación o palabra que claramente identifica al trabajador.
- **Señal inequívoca de dominio:** término estrechamente vinculado con un gran grupo y con bajo riesgo de uso transversal.
- **Especialidad o contexto:** objeto, proceso, industria, institución, material o campo técnico que podría aparecer en vacantes de varios niveles.
- **Palabra genérica o modificadora:** término que no permite inferir una ocupación por sí solo.
- **Ruido léxico:** fragmento, typo, anglicismo incompleto o token inestable.
- **Sigla:** conservada únicamente cuando existía evidencia clara de que era válida, interpretable y útil para discriminar el nivel.

### 7.5. Jerarquía de decisiones

#### `keep`

Se asignó cuando el término funcionaba como:

- Título ocupacional explícito.
- Núcleo inicial consistente de denominaciones ocupacionales.
- Señal domain-specific inequívoca y respaldada por CUOC.
- Sigla válida con interpretación clara dentro del nivel.

Ejemplos observados:

| Término | Level 1 | Razón resumida |
|---|---:|---|
| `almirante` | 0 | Título militar inequívoco. |
| `alcalde` | 1 | Cargo directivo/gubernamental explícito. |
| `electrotecnico` | 3 | Título ocupacional explícito. |
| `oficinista` | 4 | Título ocupacional explícito. |
| `mampostero` | 7 | Oficio claramente identificable. |
| `lavandero` | 9 | Título de ocupación elemental explícito. |

#### `drop`

Se asignó cuando el término era:

- Genérico o institucional.
- Modificador sin valor ocupacional autónomo.
- Engañoso al ser usado aisladamente.
- Fragmento, error tipográfico o token inestable.

Ejemplos observados:

| Término | Level 1 candidato | Riesgo identificado |
|---|---:|---|
| `fuerzas` | 0 | Palabra genérica; aislada no implica ocupación militar. |
| `tercero` | 0 | Ordinal sin significado ocupacional autónomo. |
| `corporativa` | 1 | Modificador transversal. |
| `datain` | 4 | Posible fragmento o error tipográfico. |
| `puerta` | 5 | Objeto genérico que puede producir falsos positivos. |
| `partes` | 8 | Palabra genérica y transversal. |

#### `review`

Se asignó cuando:

- El término tenía posible valor técnico, pero podía cruzar niveles.
- Aparecía como especialidad, objeto, proceso o contexto y no como cargo.
- La evidencia en CUOC era escasa.
- Era polisémico.
- Podía ser una sigla válida, pero requería confirmación humana.

Ejemplos observados:

| Término | Level 1 candidato | Motivo de revisión |
|---|---:|---|
| `corbeta` | 0 | Contexto militar válido, pero no identifica por sí solo al trabajador. |
| `comercios` | 1 | Aparece en contexto directivo, pero es transversal. |
| `medioambiental` | 2 | Especialidad útil, aunque puede aparecer en ocupaciones de otros niveles. |
| `metrologia` | 3 | Campo técnico, no necesariamente título ocupacional. |
| `viveros` | 6 | Contexto sectorial, con riesgo de uso en cargos de diferente nivel. |
| `armado` | 7 | Proceso o especialidad polisémica. |

---

## 8. Escala de confianza

La salida contiene una puntuación numérica y una etiqueta de confianza.

| Etiqueta | Rango observado | Interpretación |
|---|---:|---|
| `high` | 0,88–0,98 | Evidencia fuerte para una decisión automática `keep` o `drop`. |
| `medium` | 0,66–0,74 | Evidencia parcial; normalmente requiere revisión. |
| `low` | 0,55–0,64 | Evidencia insuficiente o alta ambigüedad. |

Distribución:

| Confianza | Cantidad |
|---|---:|
| `high` | 1.299 |
| `medium` | 536 |
| `low` | 2.219 |

Las puntuaciones deben interpretarse como una priorización interna del ejercicio, no como probabilidades calibradas estadísticamente. No provienen de un modelo supervisado entrenado con etiquetas humanas.

---

## 9. Resultados por Level 1

| Level 1 | Nombre oficial | `keep` | `review` | `drop` | Total |
|---:|---|---:|---:|---:|---:|
| 0 | Ocupaciones de las fuerzas militares | 12 | 8 | 5 | 25 |
| 1 | Directores y gerentes | 25 | 99 | 13 | 137 |
| 2 | Profesionales, científicos e intelectuales | 399 | 731 | 37 | 1.167 |
| 3 | Técnicos y profesionales de nivel medio | 146 | 509 | 23 | 678 |
| 4 | Personal de apoyo administrativo | 33 | 84 | 6 | 123 |
| 5 | Trabajadores de los servicios y vendedores de comercios y mercados | 85 | 126 | 5 | 216 |
| 6 | Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros | 48 | 112 | 1 | 161 |
| 7 | Oficiales, operarios, artesanos y trabajadores de ocupaciones afines | 274 | 480 | 2 | 756 |
| 8 | Operadores de instalaciones y máquinas y ensambladores | 84 | 510 | 2 | 596 |
| 9 | Ocupaciones elementales | 97 | 96 | 2 | 195 |
| **Total** |  | **1.203** | **2.755** | **96** | **4.054** |

El Level 2 concentra la mayor cantidad de candidatos y de términos conservados. Los Levels 3, 7 y 8 presentan una cola amplia de revisión, consistente con la presencia de especialidades técnicas, procesos, materiales y nombres de equipos que pueden ser semánticamente transversales.

---

## 10. Esquema de la tabla de salida

La tabla final contiene las siguientes columnas:

| Columna | Descripción |
|---|---|
| `term` | Forma normalizada del término. |
| `level1_candidate` | Gran grupo CUOC al que estaba asociado en el archivo original. |
| `decision` | `keep`, `drop` o `review`. |
| `reason` | Justificación resumida de la decisión. |
| `confidence` | Puntuación interna de confianza. |
| `notes_examples` | Ejemplos de denominaciones CUOC donde aparece. |
| `level1_name` | Nombre oficial del gran grupo. |
| `confidence_label` | `high`, `medium` o `low`. |
| `denomination_occurrences` | Número de denominaciones relacionadas. |
| `primary_groups` | Número de grupos primarios relacionados. |
| `occupations` | Número de ocupaciones relacionadas. |
| `standalone_title` | Si aparece como denominación independiente. |
| `first_token_ratio` | Frecuencia relativa con la que encabeza una denominación. |
| `in_official_occupation_name` | Presencia en el nombre oficial de una ocupación. |
| `in_official_primary_group_name` | Presencia en el nombre de un grupo primario. |
| `original_forms` | Formas originales observadas, incluidas variantes con tildes. |

---

## 11. Archivos generados

1. **`cuoc_level1_semantic_review.csv`**  
   Tabla completa de 4.054 términos.

2. **`cuoc_level1_semantic_review.xlsx`**  
   Libro con tres hojas:
   - `Semantic review`: tabla completa.
   - `Summary`: resumen general y distribución por Level 1.
   - `Review queue`: los 2.755 términos que requieren validación adicional.

---

## 12. Limitaciones

### 12.1. Exclusividad en CUOC no equivale a exclusividad en vacantes

Que un término aparezca en un solo gran grupo dentro de `Denominaciones CUOC 2022` no significa que, en lenguaje laboral real, solo se utilice para ese nivel.

Por ejemplo, nombres de sectores, materiales, tecnologías o procesos pueden aparecer en cargos profesionales, técnicos, operativos y elementales.

### 12.2. Evaluación de tokens aislados

La clasificación parte de palabras individuales. Al eliminar el contexto de una expresión completa se pierde información importante. Una palabra ambigua puede volverse altamente específica cuando aparece en un bigrama o trigram.

### 12.3. Ausencia de validación contra vacantes etiquetadas

No se calculó precisión, recall ni tasa de falsos positivos sobre un conjunto de vacantes con Level 1 validado manualmente. La utilidad real para el pipeline debe medirse sobre datos de vacantes.

### 12.4. Confianza no calibrada

`confidence` es una señal heurística para priorizar la revisión, no una probabilidad estadística calibrada.

### 12.5. Reproducibilidad del LLM

La interfaz no expuso temperatura, seed ni snapshot exacto del modelo. Por tanto, el archivo generado debe versionarse y utilizarse como artefacto base, en lugar de regenerarse automáticamente en cada ejecución del pipeline.

### 12.6. Posibles errores residuales

Una revisión de 4.054 términos asistida por reglas y modelo puede contener falsos `keep`, falsos `drop` o términos cuya interpretación requiera conocimiento sectorial colombiano. La categoría `review` reduce, pero no elimina, este riesgo.

---

## 13. Recomendación de uso en el pipeline

### 13.1. Integración inicial

- Utilizar únicamente los términos `keep` como candidatos para reglas directas.
- Excluir los términos `drop` del diccionario domain-specific.
- Mantener los términos `review` fuera de producción hasta validación.
- Aplicar coincidencia con límites de palabra, no búsqueda por substring.
- Conservar normalización de mayúsculas, minúsculas y tildes.
- Evaluar variantes morfológicas y plurales con cuidado para no ampliar excesivamente la regla.

### 13.2. Evitar una asignación directa basada en una sola señal débil

Incluso para `keep`, puede ser conveniente definir niveles de fuerza:

1. **Título ocupacional exacto:** permite asignación directa.
2. **Término inequívoco de dominio:** asignación directa o aumento fuerte del score.
3. **Término válido pero contextual:** usar como feature, no como regla absoluta.

### 13.3. Precedencia sugerida

```text
1. Match de título ocupacional exacto o frase altamente específica
2. Match de término domain-specific validado
3. TF-IDF Level 1
4. Clasificación granular Level 5
5. Reglas de consistencia entre Level 1 y Level 5
```

---

## 14. Validación recomendada

Antes de usar el diccionario en producción:

1. Seleccionar una muestra estratificada por Level 1 y decisión.
2. Hacer doble anotación humana independiente.
3. Resolver desacuerdos con una tercera revisión.
4. Medir precisión de los `keep` sobre vacantes reales.
5. Identificar términos que solo son útiles dentro de frases.
6. Convertir `review` de alta prioridad en reglas validadas.
7. Evaluar el impacto del bypass frente al baseline de TF-IDF:
   - accuracy de Level 1;
   - cobertura del bypass;
   - falsos positivos por nivel;
   - cambios en Level 5;
   - casos donde la regla contradice al modelo.

Una meta razonable para una regla que salta directamente el clasificador sería exigir una precisión muy alta, privilegiando precisión sobre cobertura.

---

## 15. Criterio de versionado

Se recomienda almacenar junto al repositorio:

```text
resources/cuoc_2022/domain_terms_level1/
├── cuoc_level1_semantic_review_v1.csv
├── cuoc_level1_keep_v1.csv
├── cuoc_level1_drop_v1.csv
├── cuoc_level1_review_v1.csv
├── report_semantic_review_v1.md
└── CHANGELOG.md
```

Cada cambio manual debería registrar:

- término;
- decisión anterior;
- decisión nueva;
- persona revisora;
- fecha;
- evidencia o ejemplo;
- versión del diccionario.

---

## 16. Conclusión

El ejercicio permitió reducir un conjunto de 4.054 tokens exclusivos en el catálogo a:

- **1.203 señales candidatas de alta precisión** para el diccionario domain-specific;
- **96 términos claramente descartables**;
- **2.755 términos que requieren evaluación contextual o humana**.

El principal hallazgo metodológico es que la exclusividad estadística dentro de las denominaciones CUOC no es suficiente para declarar una palabra domain-specific. Para proteger el pipeline de falsos positivos, deben priorizarse los títulos ocupacionales explícitos y reservar los términos técnicos, sectoriales o contextuales para features suaves, expresiones multipalabra o revisión manual.
