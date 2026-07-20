library(data.table)
library(magrittr)
library(readxl)
library(osmdata)
source('/Users/DVARELAT/Documents/NLP/GreenScore/Scripts/utils_functions.R')


### LEER ENTRADAS ----
{
  data_folder <- "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/"
  df_cuoc_occ <- read_excel(paste0(data_folder, 'Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                              sheet = "Ocupaciones CUOC 2022")
  df_cuoc_descr <- read_excel(paste0(data_folder, 'Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                            sheet = "Descripciones CUOC 2022")
  
}

## Juntar nombre con descripción 
cuoc_names <- data.table(df_cuoc_occ)
setnames(cuoc_names,"Nombre Ocupación - CUOC 2022","Nombre")
setnames(cuoc_names, "Ocupación", "CuocCode")
setnames(cuoc_names, "Grupo Primario" , "Primario")
cuoc_names <- cuoc_names[, list(CuocCode, Nombre, Primario)]

cuoc_bundle <- data.table(df_cuoc_descr)
setnames(cuoc_bundle, c("Level1", "CuocCode", "Descripcion", "ignorar"))
cuoc_bundle$ignorar <- NULL
cuoc_bundle <- merge(cuoc_bundle, cuoc_names)

{
  ## GUARDAR BASE ORGANIZADA Y COMPLETA 
  write_csv(cuoc_bundle, '/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/nombre_desc_occ2022.csv')
}

cuoc_bundle[, text := paste(Nombre, Descripcion)]
cuoc_bundle <- prepare_column_custom(cuoc_bundle, column = "text", stopwords = stopwords_es)

tfidf_level5 <- lab_tf_idf(
  cuoc_bundle,
  id_col = "CuocCode",
  text_col = "text",
  stopwords = stopwords_es,
  tf_weight = "double_norm",
  idf_weight = "idf_smooth",
  min_chars = 3,
  norm = FALSE
)

tfidf_level5[, tfIdf := round(tfIdf, 4)]
tfidf_level5[, level1 := substr(class, 1, 1)]
dim(tfidf_level5)
saveRDS(tfidf_level5, "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/tfidf_tokens_cuoc.rds")

tfidf_tokens_cuoc <- readRDS("/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/tfidf_tokens_cuoc.rds")




