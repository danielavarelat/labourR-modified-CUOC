setwd('/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw')
library(data.table)
library(magrittr)
library(readxl)
library(ggplot2)
library(tidyverse)
source("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/predict_skills.R")


file = '/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'
#desc <- read_excel(file, sheet = "Descripciones CUOC 2022")
desc <- read_excel(file, sheet = "Denominaciones CUOC 2022")
dim(desc)
desc[desc$`Gran Grupo` == 5,]
### Proceso: -----
stopwords = get_stopwords("es")

desc <- data.table(desc)
colnames(desc)
#setnames(desc, "Descripción Ocupación - CUOC 2022", "Description")
#setnames(desc, "Ocupación", "occ")
setnames(desc, "Nombre Denominación - CUOC 2022", "Description")
setnames(desc, "Ocupación", "occ")

setnames(desc, "Gran Grupo", "level1")

### Desde aquí todo igual independiente del SHEET!! 
length(unique(desc$level1))

### Limpieza 
desc[, Description := labourR::cleansing_corpus(Description)]
desc[, Description := remove_stopwords_accents(Description, get_stopwords("es"))]
desc <- desc[, list(level1, occ, Description)]

### Agrupar por nivel 1 y convertir en lista ---- 
{
  desc_grouped <- desc[, lapply(.SD, paste0, collapse=" "), by = level1]
  tokensList <- strsplit(desc_grouped[, get("Description")], " ")
  tokensList <- lapply(tokensList, function(sublista) sublista[sublista != ""])
  names(tokensList) <- desc_grouped$level1
  tokensList[[1]]
  tokensDT <- lapply(tokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("level1", "term"))
  tokensDT <- tokensDT[!term %in% stopwords][nchar(term) > 3]
  dim(tokensDT)
  freq_terms_level1 <- tokensDT[, .(count = uniqueN(level1)), by = term]
  length(unique(tokensDT$term))
  freq_terms_level1 <- tokensDT[, .(count = uniqueN(level1), levels = list(unique(level1))), by = term]
  freq_terms_level1[, levels := sapply(levels,  function(x) paste(x, collapse = ","))]
  write_csv(as.data.frame(freq_terms_level1), 
            "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tests/terms_denom_freq_level1.csv")
  freq_terms_level1 <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/terms_denom_freq_level1.csv")
  
  freq_terms_level1[freq_terms_level1$term == "fuerza",]
}

### hacerlo pero dejando también la Occupation ----
tokensList_occ <- strsplit(desc[, get("Description")], " ")
tokensList_occ <- lapply(tokensList_occ, function(sublista) sublista[sublista != ""])
names(tokensList_occ) <- desc$occ
tokensDT_occ <- lapply(tokensList_occ, data.table) %>%
  rbindlist(idcol = TRUE) %>%
  setnames(c("occ", "term"))
tokensDT_occ <- tokensDT_occ[!grepl("^\\d+$", term)]
tokensDT_occ[, level1 := substr(occ, 1, 1)]

##checking - saco algunos terms?? 
{
  level_terms_count <- tokensDT_occ[, .(unique_occ = uniqueN(occ)), by = .(level1, term)][, .(N = sum(unique_occ)), by = .(level1, term)]
  #level_terms_count <- tokensDT_occ[, .N, by=.(level1, term)] -> cuenta todos, no los unique por occ
  setnames(level_terms_count, "N", "N_occ")
  desc[, level1 := as.character(level1)]
  
  level_terms_count <- merge(level_terms_count, desc[, .N, by=.(level1)] )
  level_terms_count[, prop := N_occ / N]
  
  tokensDT_occ[tokensDT_occ$term == "militares"]
  level_terms_count[level_terms_count$term == "militares"]
  resultados[resultados$term == "militares"]
  tokensDT_occ[, .N, by=.(level1)] 

  level_terms_count[level_terms_count$prop > 0.5]
}


## 1. Construir TF: Term Frequency TABLE
tf_weight = "double_norm"
idf_weight = "idf_smooth"

tfDT <- tokensDT_occ[, list(term_count = .N), by = c("level1", "term")]
dim(tfDT)
## Weighting scheme of term frequency.
if(tf_weight == "double_norm")
  tfDT[, tf := 0.5 + 0.5 * term_count / max(term_count, na.rm = TRUE), by = "level1"]
if(tf_weight == "raw_count") tfDT[, tf := term_count]
if(tf_weight == "log_norm") tfDT[, tf := log(1 + term_count)]

ggplot(tfDT, aes(tf)) + geom_density()
summary(tfDT)

## 2. Construir IDF: Inverse document frequency TABLE
idfDT <- tokensDT[!duplicated(tokensDT)][, list(docFreq = .N), by = "term"]
## Weighting scheme of inverse document frequency. 
total_clases = length(unique(tokensDT$level1))
if(idf_weight == "idf_smooth")
  idfDT[, idf :=  log(total_clases / (docFreq + 1)) + 1]
if (idf_weight == "idf")
  idfDT[, idf :=  log(total_clases / docFreq)]
ggplot(idfDT, aes(idf)) + geom_density()
summary(idfDT)

res_tfidf <- merge(tfDT, idfDT)[, tfIdf := tf * idf]#[, list(class, term, tfIdf)]
res_tfidf[, tfIdf := round(tfIdf, 4)]

saveRDS(res_tfidf, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tfidf_tokens_level1_den.rds")

### REVISIÓN SOBRE DENOMINACIONES - 13 MARZO 
res_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tfidf_tokens_level1_den.rds")

class(res_tfidf)

res_tfidff[, .N, by=.(term)] 


### DOMAIN SPECIFIC revision ------
library(readxl)
library(dplyr)

### Este es el que yo les mandé! 
freq_terms_level1 <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tests/terms_denom_freq_level1.csv")
dim(freq_terms_level1[freq_terms_level1$count == 1,])

df <- read_excel("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/data/terms_denom_freq_level_all_v2.xlsx")
df <- df %>% replace(is.na(.), 0)
df$sum <- df$Andres + df$Alex_v2 + df$`Pamela v2`
df <- df[df$count ==1,]

df <- df[df$sum >0,]

table(df$levels)

## revisar las palabras unique que hay supuestamente en cada nivel general? 
df_vacantes <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning corpus/scraped_html2.csv")
dim(df_vacantes)
source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")
stopwords_es = labourR::get_stopwords("es")
corpus_vac <- data.table(df_vacantes)
colnames(corpus_vac)
prepare_column_custom(corpus_vac,  column = "title", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "keywords_process", stopwords=stopwords_es)
corpus_vac[, both := paste(title, keywords_process)]
x = corpus_vac[corpus_vac$ID_file == "3A17C6791B4E9C0961373E686DCF3405"]
x$both

freeTextTokensList <- lapply(strsplit(corpus_vac$both, split = " "), function(x) x[!x %in% stopwords_es])
names(freeTextTokensList) <- corpus_vac$ID_file
freeTextTokensList <-
  lapply(freeTextTokensList, function(sublist)
    Filter(function(x)
      nchar(x) >= 3, sublist))
freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
  rbindlist(idcol = TRUE) %>%
  setnames(c("id", "term"))
freeTextTokensDT <- freeTextTokensDT[!grepl("^\\d+$", term)] 


### Para cada vacante - mirar con cuales nivel 1 hace match
vocabulary <- data.table(df[,c("term", "levels")])
saveRDS(vocabulary, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/domain_specific_v1.rds")
dt_list <- split(freeTextTokensDT, by = "id")


get_level1_exact <- function(dt_vac, vocabulary) {
  # hacer matche entre cada vacante DT y el vocabulary completo 
  dt_vac_match <- merge(dt_vac, vocabulary,  all.x = TRUE)
  lev <- dt_vac_match$levels[!is.na(dt_vac_match$levels)]
  return(lev)
} 
get_level1_exact(dt_list[[1]], vocabulary)
list_l1 <- lapply(dt_list, function(x) {get_level1_exact(x, vocabulary)})
list_l1 <- Filter(function(x) length(x) > 0, list_l1)
length(list_l1)
# mostrar los qur tienen más de 1 categoría
list_l1_multiple <- lapply(list_l1, function(x) {
  if (length(unique(x)) > 1) {
    return(x)
  }
})
list_l1_multiple <- list_l1_multiple[sapply(list_l1_multiple, function(x) !is.null(x))]
length(list_l1_multiple)
### aquí los que solo tienen 1
list_l1_single <- lapply(list_l1, function(x) {
  if (length(unique(x)) == 1) {
    return(x)
  }
})
length(list_l1_single)
list_l1_single <- list_l1_single[sapply(list_l1_single, function(x) !is.null(x))]

list_l1["FEC793881A439C0B61373E686DCF3405"]
list_l1_single["FEC793881A439C0B61373E686DCF3405"]

freeTextTokensDT[id == "D60549CBEAEF93A661373E686DCF3405"]
merge(freeTextTokensDT[id == "D60549CBEAEF93A661373E686DCF3405"], vocabulary)


# revisar los maches--- al final me quedé solo con exacto----
vocaIndexes <- match(dt_vac$term, vocabulary$term)
string_dist = NULL
if(!is.null(string_dist))
  vocaIndexes[is.na(vocaIndexes)] <- amatch(freeTextTokensDT$term[is.na(vocaIndexes)], vocabulary$term, maxDist = 0.05, method = string_dist)
matches <- data.table(id = dt_vac$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]

df_test <- data.frame(dt_vac$term)
df_test$match <- vocabulary$term[vocaIndexes]
x = merge(as.data.frame(freeTextTokensDT), as.data.frame(df)[,c("term", "levels")], by = "term", all.x = TRUE)

xx = data.table(x)[, .(unique_levels = length(unique(levels))), by = .(id, levels)]
df[df$term %in% words,]

xx = data.table(x)[, .(levels_count = .N), by = .(id, levels)]
