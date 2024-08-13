suppressPackageStartupMessages({
  library(stringdist)
  library(data.table)
  library(magrittr)
  library(stringr)
  library(arrow)
})

output_folder <- "/home/qiime2/rosario/prediction_new/"

source("/home/qiime2/rosario/occupations_classify_new.R")
source("/home/qiime2/rosario/utils_functions.R")

### INPUTS our labourR -------- 
level1_tfidf <- readRDS("/home/qiime2/rosario/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
granular_tfidf <- readRDS("/home/qiime2/rosario/tfidf_tokens_cuoc.rds")
vocabulary_domain <-  readRDS("/home/qiime2/rosario/domain_specific_v1.rds")

start_time <- Sys.time()

args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
df_vacantes <- read_parquet(input_file)

### 1. PREPARE CORPUS ----
df_vacantes <- df_vacantes[, c("ID_file", "title", "keywords_process", "full_descr","place")]

corpus <- data.table(df_vacantes)
NAME = str_split(input_file, "/")[[1]][2]
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



