library(stringdist)
library(labourR)
library(data.table)
library(magrittr)
library(stringdist)
library(stringr)

source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")
source("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning_corpus/utils_functions.R")

### INPUTS our labourR -------- 
level1_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
granular_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_cuoc.rds")
vocabulary_domain <-  readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/domain_specific_v1.rds")



### ------ 
df_vacantes <- read_csv("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning_corpus/scraped_html2.csv")
corpus_vac <- data.table(df_vacantes)
prepare_column_custom(corpus_vac,  column = "title", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "keywords_process", stopwords=stopwords_es)
prepare_column_custom(corpus_vac,  column = "full_descr", stopwords=stopwords_es)
corpus_vac[, title_kw := paste(title, keywords_process)]
corpus_vac[, title_kw_des := paste(title, keywords_process, full_descr)]


one_vac = corpus[corpus$ID_file == "A270FA1091A68CAB61373E686DCF3405"]
colnames(one_vac)
pred <- single_two_steps(
  corpus_one = one_vac,
  vocabulary_domain = vocabulary_domain,
  table_tfidf_broad = level1_tfidf,
  table_tfidf_granular = granular_tfidf,
  id_col = 'ID_file',
  text_col1 = 'title_kw',
  text_col2 = 'title_kw_des',
  num_leaves_final = 3,
  print_match=TRUE, 
  noisy_words = places
)
class(pred)
#### LabourR original
pred_lab <-
  classify_occupation(
    corpus = one_vac,
    id_col = 'ID_file',
    text_col = "title_kw_des",
    isco_level = NULL,
    lang = "es",
    num_leaves = 3
  )
? classify_occupation

### completar respuesta 
cuoc_bundle <- data.table(read_csv('/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/nombre_desc_occ2022.csv'))
DT_predictions <- merge.data.table(pred, cuoc_bundle, by='CuocCode')


#### scores!! 
csv_file_path <- "/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/data/scores/"
df_scores <- read_csv(paste0(csv_file_path, "scores_batch_58.csv"))
