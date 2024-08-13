suppressPackageStartupMessages({
  library(stringdist)
  library(data.table)
  library(magrittr)
  library(stringdist)
  library(stringr)
  library(arrow)
})

output_folder <- "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/prediction_new/"

source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")
source("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning_corpus/utils_functions.R")

### INPUTS our labourR -------- 
level1_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
granular_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/tfidf_tokens_cuoc.rds")
vocabulary_domain <-  readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/cuoc/domain_specific_v1.rds")

start_time <- Sys.time()

args <- commandArgs(trailingOnly = TRUE)
input_file <- "/home/qiime2/rosario/processed/processed_data_batch_1.parquet"
input_file <- args[1]

#input_file <- "/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/cleaning_corpus/processed/processed_data_batch_58.parquet"
df_vacantes <- read_parquet(input_file)

### 1. PREPARE CORPUS ----
df_vacantes <- df_vacantes[, c("ID_file", "title", "keywords_process", "full_descr","place")]

corpus <- data.table(df_vacantes)
NAME = str_split(input_file, "/")[[1]][6]
print(NAME)
prepare_column_custom(corpus,  column = "title", stopwords=stopwords_es)
prepare_column_custom(corpus,  column = "keywords_process", stopwords=stopwords_es)
prepare_column_custom(corpus,  column = "full_descr", stopwords=stopwords_es)
prepare_column_custom(corpus,  column = "place", stopwords=stopwords_es)
corpus[, title_kw := paste(title, keywords_process)]
corpus[, title_kw_des := paste(title, keywords_process, full_descr)]
corpus <- corpus[!is.na(title)]
print(paste0("Total vacantes con title -> ", dim(corpus)[[1]]))
duplicate_rows <- duplicated(corpus$ID_file)
corpus <- corpus[!duplicate_rows,]
print(paste0("Total vacantes sin duplicates -> ", dim(corpus)[[1]]))
places <- get_place_words(corpus)

result_dt <- data.table()
for (i in 1:nrow(corpus)) {
  row <- corpus[i, ]
  pred <- single_two_steps(
    corpus_one = row,
    vocabulary_domain = vocabulary_domain,
    table_tfidf_broad = level1_tfidf,
    table_tfidf_granular = granular_tfidf,
    id_col = 'ID_file',
    text_col1 = 'title_kw',
    text_col2 = 'title_kw_des',
    num_leaves_final = 3,
    print_match = FALSE,
    noisy_words = places
  )
  if (!is.null(pred)) {
    result_dt <- rbind(result_dt, pred, fill = TRUE)
  }
}

OUTNAME <- str_replace(str_replace(NAME, ".parquet", ".rds"), "processed_data_", "occ_")
print(OUTNAME)
print(dim(result_dt))
saveRDS(result_dt, paste0(output_folder, OUTNAME))
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("Elapsed time:", elapsed_time))
print("------------------------------------------------")
result_dt <-readRDS(paste0(output_folder,  "test.rds"))
length(unique(result_dt$CuocCode))
df_goji = read_excel("/Users/dvarelat/Documents/PROYECTOS/NLP/mine/New/data/CUOC_GOJI.xlsx")


df_goji$CuocCode <- df_goji$CUOC_5d
x = left_join(as.data.frame(result_dt),df_goji, by="CuocCode" )

