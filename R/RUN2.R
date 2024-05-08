library(stringdist)
library(labourR)
library(data.table)
library(magrittr)
library(stringdist)
library(stringr)

source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")

### INPUTS 
level1_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
granular_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_cuoc.rds")



#### Predicción normal ------- 
pred1 <- classify_occupation_2(
  corpus_in,
  table_tfidf = level1_tfidf,
  id_col = "id",
  text_col = "text",
  num_leaves = 1,
  max_dist = 0.1,
  string_dist = "jw",
  outcode="level1"
)
pred1 <- merge(corpus_in, pred1)
tfidf_subset <- granular_tfidf[granular_tfidf$level1 == pred1$level1[[1]],]
pred2 <- classify_occupation_2(
  pred1,
  table_tfidf = tfidf_subset,
  id_col = "id",
  text_col = "text",
  num_leaves = 5,
  max_dist = 0.1,
  string_dist = "jw"
)
pred2


#### two step - single vacancy ------- 
### da exactamente igual que el de arriba
single_two_steps_classify(
  corpus_one = data.frame(id = 1,text = c("asesor de banco")),
  table_tfidf_broad = level1_tfidf,
  table_tfidf_granular = granular_tfidf,
  id_col = "id",
  text_col1 = "text",
  text_col2 = "text",
  num_leaves_final = 5
)


### predecir varios juntos inventados ---- 
corpus_in <- data.frame(
  id = 1:3,
  text = c("Asesor comercial in-office con telemercadeo", 
           "asckandvlkv", "cajero en  McDonald's")
)
list_preds <-
  apply(corpus_in, 1, function(row)
    single_two_steps_classify(
      as.data.frame(t(row)),
      table_tfidf_broad = level1_tfidf,
      table_tfidf_granular = granular_tfidf,
      num_leaves_final = 5
    ))
DT_predictions <- lapply(list_preds, data.table) %>%
  rbindlist(idcol = FALSE)

### predecir title y corpus con vacante real - solo 1 ---- 
### probar primero title + title y comparar
df_vacantes <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/data/data_vacancy_daniela.csv")
dim(df_vacantes)
single_vacante <- as.data.table(
  df_vacantes[df_vacantes$idvacancy == "123FD4065A47030F61373E686DCF3405",])
colnames(single_vacante)
stopwords_es = labourR::get_stopwords("es")
prepare_column_custom(single_vacante, column = "titulo_2", stopwords = stopwords_es)
prepare_column_custom(single_vacante, column = "descripcion_2", stopwords = stopwords_es)
## probar pegando los dos 
single_vacante[, both := paste(titulo_2, descripcion_2)]
colnames(single_vacante)
pred_single <- single_two_steps_classify(
  corpus_one = data.frame(single_vacante),
  table_tfidf_broad = level1_tfidf,
  table_tfidf_granular = granular_tfidf,
  id_col = "idvacancy",
  text_col1 = "titulo_2",
  text_col2 = "both",
  num_leaves_final = 5
)
setnames(pred_single, "id", "idvacancy")
pred_single <- merge(
  pred_single,
  single_vacante[, list(idvacancy, titulo_2, descripcion_2)]
)
cuoc_bundle <- read_csv('/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/nombre_desc_occ2022.csv')
colnames(cuoc_bundle)
cuoc_bundle <- data.table(cuoc_bundle)
cuoc_bundle <- cuoc_bundle[, list(CuocCode, Level1, Nombre, Descripcion)]
colnames(pred_single)
pred_single <- merge.data.table(pred_single,
                                   cuoc_bundle, by='CuocCode')


### TODO EL CORPUS ---- 
#df_vacantes <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/data/data_vacancy_daniela.csv")
#dim(df_vacantes)
stopwords_es = labourR::get_stopwords("es")

df_vacantes <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning corpus/scraped_html2.csv")
dim(df_vacantes)

corpus_vac <- data.table(df_vacantes)
colnames(corpus_vac)
prepare_column_custom(corpus_vac,  column = "title", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "keywords_process", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "first_descr", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "full_descr", stopwords=stopwords_es)
corpus_vac[, title_kw := paste(title, keywords_process)]
corpus_vac[, title_kw_des := paste(title, keywords_process, full_descr)]


x = corpus_vac[corpus_vac$ID_file == "3A17C6791B4E9C0961373E686DCF3405"]
x

### verificar la palabra "capitán" en esta vacante, debe quedar sin tilde
x = corpus_vac[corpus_vac$ID_file == "3A17C6791B4E9C0961373E686DCF3405"]
x$title_kw

vocabulary_domain <-  readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/domain_specific_v1.rds")


#random_rows <- corpus_vac[sample(.N, 100)]
list_preds <-
  apply(corpus_vac, 1, function(row)
    single_two_steps_classify(
      corpus_one = data.frame(t(row)),
      table_tfidf_broad = level1_tfidf,
      table_tfidf_granular = granular_tfidf,
      id_col = "ID_file",
      text_col1 = "both",
      text_col2 = "both",
      num_leaves_final = 3
    )
)


DT_predictions <- lapply(list_preds, data.table) %>%
  rbindlist(idcol = FALSE)
## PEGAR TITLE VACANTES PARA REVISAR
setnames(DT_predictions, "id", "idvacancy")
colnames(corpus_vac)
setnames(corpus_vac, "ID_file", "idvacancy")

DT_predictions <- merge(
  DT_predictions,
  corpus_vac[, list(idvacancy, title, keywords_process, both)]
)

### PEGAR DESCRIPCION CUOC
cuoc_bundle <- read_csv('/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/nombre_desc_occ2022.csv')
colnames(cuoc_bundle)
cuoc_bundle <- data.table(cuoc_bundle)
cuoc_bundle <- cuoc_bundle[, list(CuocCode, Level1, Nombre, Descripcion)]
DT_predictions <- merge.data.table(DT_predictions,
                                   cuoc_bundle, by='CuocCode')

#write_csv(DT_predictions, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/labourR_2levels_newcorpus_20424.csv")

write_csv(DT_predictions, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/labourR_2levels_newcorpus_04524.csv")



DT_predictions <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tests/labourR_2levels.csv")
DT_predictions$weight_sum <- round(DT_predictions$weight_sum, 4)
DT_predictions$CuocCode <- as.character(DT_predictions$CuocCode)

write_csv(as.data.frame(DT_predictions), "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tests/labourR_2levels.csv")


## Revisar predicciones ------
DT_predictions1 <- read_csv( "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/labourR_2levels_newcorpus_20424.csv")
DT_predictions2 <- read_csv( "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/labourR_2levels_newcorpus_04524.csv")

{
  corpus_vac_one <- corpus_vac[corpus_vac$idvacancy == '003D1F1C0A677BA961373E686DCF3405',]
  corpus_vac_one <- prepare_column_custom(corpus_vac_one, column="both", stopwords=get_stopwords("es"))
  setnames(corpus_vac_one, c('idvacancy', 'both'), c("id", "text"))
  corpus_vac_one$text
  
  freeTextTokensList <- lapply(strsplit(corpus_vac_one$text, split = " "), function(x) x[!x %in% get_stopwords('es')])
  names(freeTextTokensList) <- corpus_vac_one$id
  freeTextTokensList <-
    lapply(freeTextTokensList, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  vocabulary <- unique(granular_tfidf[, list(term)])[order(term)]
  vocaIndexes <- match(freeTextTokensDT$term, vocabulary$term)
  string_dist = "jw"
  if(!is.null(string_dist))
    vocaIndexes[is.na(vocaIndexes)] <- stringdist::amatch(freeTextTokensDT$term[is.na(vocaIndexes)], vocabulary$term, maxDist = 0.1, method = string_dist)
  matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
  matches[, term := unlist(matches$term)]
  DT_predictions1[DT_predictions1$idvacancy == '003D1F1C0A677BA961373E686DCF3405',]
  DT_predictions2[DT_predictions2$idvacancy == '003D1F1C0A677BA961373E686DCF3405',]
  
  merge(
    matches,
    granular_tfidf[granular_tfidf$class == "32540"],
    allow.cartesian = FALSE
  )
}

