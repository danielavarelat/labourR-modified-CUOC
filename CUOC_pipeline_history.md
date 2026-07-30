# CUOC pipeline history

Fecha de inicio de esta bitácora: 2026-07-29

## Contexto general

Este proyecto usa el pipeline de `labourR` adaptado a CUOC para imputar ocupaciones a vacantes.

La lógica general de predicción es:

1. Nivel 1: usar la hoja **Denominaciones CUOC 2022**.
2. Nivel 5: usar la hoja **Descripciones CUOC 2022** con 678 códigos.

Además, para mejorar la precisión del nivel 1, se construyó un mecanismo de vocabulario `domain-specific`.
La idea es:

- si una vacante contiene una palabra `domain-specific`, esa señal puede llevarla directamente al nivel correspondiente;
- luego, desde ahí, se hace la predicción más granular;
- para esta parte granular se usa solo `title + keywords`.

## Paso 1: TF-IDF para nivel 1

### Objetivo

Construir `tfidf_level1` a partir de la hoja **Denominaciones CUOC 2022**.

### Script

`cuoc/Scripts/Create_corpus_level1.R`

### Decisiones actuales

- El script trabaja con `min_chars_tfidf <- 3`.
- Se conservan términos de 3 caracteres solo si están validados manualmente en `keep_terms`.
- El output se guarda dentro del repo actual en `cuoc/Data/tfidf_tokens_level1_den.rds`.

### Observación importante

La decisión de conservar algunas palabras de 3 caracteres se mantiene porque pueden capturar abreviaciones útiles.
La lista se controla explícitamente desde el inicio del script para que el criterio sea reproducible.

## Paso 2: vocabulario `domain-specific`

### Objetivo

Construir un vocabulario de palabras que aparezcan en un solo nivel CUOC y que además hayan sido validadas manualmente.

### Script

`cuoc/Scripts/Level1_specific_terms.R`

### Flujo lógico

1. Se toma la salida de `Denominaciones CUOC 2022`.
2. Se construye `freq_terms_level1`, donde cada término queda asociado al número de niveles en los que aparece.
3. Se obtienen aproximadamente **4054 términos** que aparecen solo en un nivel.
4. Ese conjunto se revisó manualmente por el equipo.
5. Se conservaron solo los términos con al menos **1 voto**.
6. El resultado final queda como `domain_specific_v1.rds`.

### Capa adicional de revisión semántica

Después de la revisión manual, se incorporó una revisión semántica asistida por LLM a partir de:

- `cuoc/Data/reporte_revision_semantica_CUOC_level1.md`
- `cuoc/Data/cuoc_level1_semantic_review.csv`

La comparación entre ambos criterios dejó estos resultados:

| Resultado | Cantidad |
|---|---:|
| `manual_only` | 79 |
| `semantic_only` | 0 |
| `high_conflict` | 0 |
| `keep_agreed` | 618 |

La decisión final adoptada fue conservar únicamente `keep_agreed = 618`, sin alterar el pipeline downstream.

### Archivos de referencia

- `cuoc/Data/terms_denom_freq_level1.csv`(sin filtar, todos los terms y los niveles en los que aparece. Desde aquí filtramos para el excel.)
- `cuoc/Data/terms_denom_freq_level1_one.csv` (subconjunto con términos exclusivos de un solo level1)
- `cuoc/Data/terms_denom_freq_level_all_v2.xlsx`
- `cuoc/Data/cuoc_level1_semantic_review.csv`
- `cuoc/Data/cuoc_level1_manual_vs_semantic_review.csv`
- `cuoc/Data/cuoc_level1_keep_agreed.csv`
- `cuoc/Data/cuoc_level1_high_conflict.csv`
- `cuoc/Data/cuoc_level1_manual_only.csv`
- `cuoc/Data/cuoc_level1_semantic_only.csv`
- `cuoc/Data/domain_specific_v1.rds`

## Estado actual

Lo que ya quedó organizado:

- `Create_corpus_level1.R` apunta al árbol correcto del repo.
- `Level1_specific_terms.R` también apunta al árbol correcto del repo.
- El vocabulario final se construye sobre la intersección entre revisión manual y revisión semántica.
- La bitácora queda como documento vivo para seguir explicando decisiones y cambios.

## Próximos pasos sugeridos

1. Probar `Level1_specific_terms.R` con las rutas nuevas.
2. Verificar que `terms_denom_freq_level1.csv` se escriba en `cuoc/Data/`.
3. Confirmar que `domain_specific_v1.rds` se reconstruye sin depender de rutas viejas.
4. Documentar el paso 3 del pipeline cuando ya esté estable.
