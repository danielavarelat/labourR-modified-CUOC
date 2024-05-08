### Code inside the two level implementation

library(data.table)
library(magrittr)
library(labourR)
library(stringr)
library(dplyr)
library(readr)
library(tm)
library(stringi)
library(stringdist)

source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")

### INPUTS 
level1_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
granular_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_cuoc.rds")
vocabulary_domain <-  readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/domain_specific_v1.rds")



### VACANTES
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


### Convert corpus into DT or list of terms (hice una función de esto)--- 
COL = "title_kw"
ID = "ID_file"
{ 
  freeTextTokensList <- strsplit(corpus_vac[[COL]], split = " ")
  names(freeTextTokensList) <- corpus_vac[[ID]]
  freeTextTokensList <-
    lapply(freeTextTokensList, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  freeTextTokensDT <- freeTextTokensDT[!grepl("^\\d+$", term)] 
}

freeTextTokensDT <- corpus_to_dt(corpus_vac, ID= "ID_file", COL = "title_kw")
dt_list <- split(freeTextTokensDT, by = "id")


id_col = "id"
text_col = "text"






