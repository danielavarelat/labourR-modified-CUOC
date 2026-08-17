<!-- README.md is generated from README.Rmd. Please edit that file -->

# labourR ORIGINAL

The goal of labourR is to map multilingual free-text of occupations,
such as a job title in a Curriculum Vitae, to hierarchical ontologies
provided by [ESCO](https://ec.europa.eu/esco/portal), the multilingual
classification of European Skills, Competences, Qualifications and
Occupations, and
[ISCO](https://ec.europa.eu/esco/portal/escopedia/International_Standard_Classification_of_Occupations__40_ISCO_41_),
the International Standard Classification of Occupations.

<img src="man/figures/ESCO_ISCO_hierarchy.png" title="Fig.1 - ESCO is mapped to the 4th level of the ISCO hierarchical model." alt="Fig.1 - ESCO is mapped to the 4th level of the ISCO hierarchical model." width="80%" />

Computations are vectorised and the `data.table` package is used for
high performance and memory efficiency.

Ir al
[Repositorio](https://eworx-org.github.io/labourR/articles/occupations_retrieval.html)
para más detalles.

-------------------------------------------------------------------------------

# CUOC Adaptation

This repository now includes a CUOC-focused implementation for vacancy-to-occupation mapping.

Start with the project log:

- [CUOC pipeline history](./CUOC_pipeline_history.md)

The working reference for this branch is the CUOC 2022 classification:

- [Clasificación Única de Ocupaciones para Colombia - CUOC 2022](https://www.dane.gov.co/files/sen/nomenclatura/cuoc/documento-clasificacion-unica-ocupaciones-colombia-CUOC-2022.pdf)

## What the CUOC workflow does

The current pipeline follows three main steps:

1. Build a broad `level1` TF-IDF table from `Denominaciones CUOC 2022`.
2. Build the granular occupation TF-IDF table from `Ocupaciones CUOC 2022` and `Descripciones CUOC 2022`.
3. Use a curated `domain-specific` vocabulary to shortcut some `level1` matches before TF-IDF.

The current workflow is documented in detail in the pipeline history file above.

## Key scripts

- `cuoc/Scripts/Create_corpus_level1.R`
- `cuoc/Scripts/Level1_specific_terms.R`
- `cuoc/Scripts/Occupations_tfidf_CUOC.R`
- `cuoc/Scripts/Run_occ_predict.R`

## Main inputs

- `cuoc/Data/Correlativa_CUOC-2022_Vs_CNO-2022.xlsx`
- `cuoc/Data/tfidf_tokens_level1_den.rds`
- `cuoc/Data/tfidf_tokens_cuoc.rds`
- `cuoc/Data/domain_specific_v1.rds`

## Input arguments for the main classifier

- `corpus_one`: vacancy data as a `data.table`
- `id_col`: ID column name
- `text_col1`: text used for the broad `level1` match
- `text_col2`: text used for the granular occupation match
- `num_leaves_final`: number of occupation predictions returned per vacancy

The implementation keeps experimental scratch blocks in the scripts when they help with debugging or comparison, but the authoritative project notes live in the CUOC pipeline history file.

<img width="521" alt="foto" src="https://github.com/danielavarelat/labourR-modified-CUOC/assets/47607161/d4db5a70-8a08-41a9-9509-8939b023e3b5">

