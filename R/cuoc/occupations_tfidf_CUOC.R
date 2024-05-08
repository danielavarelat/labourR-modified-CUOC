library(data.table)
library(magrittr)
library(readxl)

stopwords = get_stopwords("es")

file = '/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'

## Juntar nombre con descripción 
cuoc_names <- read_excel(file, sheet = "Ocupaciones CUOC 2022")
cuoc_names <- data.table(cuoc_names)
colnames(cuoc_names)
setnames(cuoc_names,"Nombre Ocupación - CUOC 2022","Nombre")
setnames(cuoc_names, "Ocupación", "CuocCode")
setnames(cuoc_names, "Grupo Primario" , "Primario")
cuoc_names <- cuoc_names[, list(CuocCode, Nombre, Primario)]


cuoc_bundle <- read_excel(file, sheet = "Descripciones CUOC 2022")
cuoc_bundle <- data.table(cuoc_bundle)
colnames(cuoc_bundle)
dim(cuoc_bundle)
setnames(cuoc_bundle, c("Level1", "CuocCode", "Descripcion", "ignorar"))
cuoc_bundle <- merge(cuoc_bundle, cuoc_names)
cuoc_bundle$ignorar <- NULL

## GUARDAR BASE ORGANIZADA Y COMPLETA 
write_csv(cuoc_bundle, '/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/nombre_desc_occ2022.csv')

cuoc_bundle[, text := paste(Nombre, Descripcion)]
cuoc_bundle[, text := labourR::cleansing_corpus(text)]
cuoc_bundle[, text := remove_stopwords_accents(text, get_stopwords("es"))]

colnames(cuoc_bundle)
tfidf_tokens_cuoc <- labourR::tf_idf(cuoc_bundle, 
                                id_col = "CuocCode", 
                                text_col = "text", 
                                stopwords = get_stopwords("es"),
                                tf_weight = "double_norm",
                                idf_weight = "idf_smooth",
                                min_chars = 2,
                                norm = FALSE)

tfidf_tokens_cuoc[, tfIdf := round(tfIdf, 4)]
tfidf_tokens_cuoc[, level1 := substr(class, 1, 1)]


saveRDS(tfidf_tokens_cuoc, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tfidf_tokens_cuoc.rds")

tfidf_tokens_cuoc <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tfidf_tokens_cuoc.rds")
#usethis::use_data(tfidf_tokens, compress = "xz",internal = TRUE, overwrite = TRUE)
