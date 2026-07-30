source('/Users/DVARELAT/Documents/ROSARIO2026/labourR-modified-CUOC/cuoc/Scripts/utils_functions.R')

library(data.table)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(labourR)

### PARAMETERS ----
# We keep 3-character terms only when they are manually validated as useful.
# This avoids dropping helpful abbreviations like "gps" or "web" while filtering
# noisy codes and partial tokens that can appear in CUOC denominaciones.
min_chars_tfidf <- 3
keep_terms <- c(
  "bpo", "bus", "cad", "eps", "gas", "glp", "gps", "ips", "ixd", "noc",
  "oil", "pcb", "red", "sap", "sig", "tcp", "tic", "tig", "vfx", "vid",
  "voz", "wan", "web"
)

### LEER ENTRADAS ----
{
  data_folder <- "/Users/DVARELAT/Documents/ROSARIO2026/labourR-modified-CUOC/cuoc/Data/"
  df_cuoc_denom <- read_excel(paste0(data_folder, 'Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                              sheet = "Denominaciones CUOC 2022")
}

## Organizar ----
dt <- data.table(df_cuoc_denom)
setnames(dt, "Nombre Denominación - CUOC 2022", "Description")
setnames(dt, "Ocupación", "occ")
setnames(dt, "Gran Grupo", "level1")
dt <- dt[, list(level1, occ, Description)]


### Limpieza ----
dt <- prepare_column_custom(dt, column = "Description", stopwords = stopwords_es)
dt <- prepare_column_custom(dt, column = "occ", stopwords = stopwords_es)

tfidf_level1 <- lab_tf_idf(
  dt,
  id_col = "level1",
  text_col = "Description",
  stopwords = stopwords_es,
  tf_weight = "double_norm",
  idf_weight = "idf_smooth",
  min_chars = min_chars_tfidf,
  norm = FALSE
)

### Keep only validated 3-character terms
tfidf_level1 <- tfidf_level1[
  nchar(term) > 3 | term %in% keep_terms
]

file_level1 <- file.path(data_folder, "tfidf_tokens_level1_den.rds")
saveRDS(tfidf_level1, file_level1)

### QA ----
### Compare against the previously saved version only if needed for regression checks.
# x <- readRDS(file_level1)
# unique(tfidf_level1[!tfidf_level1$term %in% x$term, "term"])
