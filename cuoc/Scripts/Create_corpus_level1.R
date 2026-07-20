source('/Users/DVARELAT/Documents/NLP/GreenScore/Scripts/utils_functions.R')

library(data.table)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(labourR)

### LEER ENTRADAS ----
{
  data_folder <- "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/"
  df_cuoc_denom <- read_excel(paste0(data_folder, 'Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                              sheet = "Denominaciones CUOC 2022")
  
}

## Organizar ----
dt <- data.table(df_cuoc_denom)
colnames(desc)
setnames(dt, "Nombre Denominación - CUOC 2022", "Description")
setnames(dt, "Ocupación", "occ")
setnames(dt, "Gran Grupo", "level1")
dt <- dt[, list(level1, occ, Description)]
stopwords_es

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
  min_chars = 3,
  norm = FALSE
)

file_level1 <- "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/tfidf_tokens_level1_den.rds"
saveRDS(tfidf_level1, file_level1)


