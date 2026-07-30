# Experimental runner for the CUOC classifier.
# Keep scratch blocks if they help debugging, but the default paths should point
# to the current repo so the script can be recreated without old folders.
project_root <- "/Users/DVARELAT/Documents/ROSARIO2026/labourR-modified-CUOC"
data_folder <- file.path(project_root, "cuoc", "Data")
output_folder <- file.path(project_root, "cuoc", "Results")
# Main pipeline settings:
# - `broad_text_col` is the text used for the broad level-1 match.
# - `granular_text_col` is the text used for the granular CUOC step.
# Keep these configurable so experiments can switch between `title_kw` and
# `title_kw_des` without editing the prediction logic.
broad_text_col <- "title_kw"
granular_text_col <- "title_kw_des"

source(file.path(project_root, "cuoc", "Scripts", "Occupations_classify_new.R"))
source(file.path(project_root, "cuoc", "Scripts", "utils_functions.R"))


### INPUTS our labourR -------- 
{
  level1_tfidf <- readRDS(file.path(data_folder, "tfidf_tokens_level1_den.rds"))
  granular_tfidf <- readRDS(file.path(data_folder, "tfidf_tokens_cuoc.rds"))
  vocabulary_domain <- readRDS(file.path(data_folder, "domain_specific_v1.rds"))
}

## Leer vacantes ---
# This is a scratch sample outside the repo. It is fine for testing, but the
# path is intentionally left as-is because it belongs to the local scrape folder.
df_vacantes <- read.csv("/Users/DVARELAT/Documents/NLP/Scrapping/COMPUTRABAJO/test/sample_55000.csv")
single_vac <- df_vacantes[10,]

# --- Clean Text ---
# `title_kw_des` is a broader experimental variant that includes description.
# The default granular step below uses `title_kw` only, which is the cleaner
# path for the current CUOC pipeline.
single_cleaned <- data.table(
  jobID_file = single_vac$jobID_file,
  title = prepare_text_custom(single_vac$title, stopwords_es),
  keywords_process = prepare_text_custom(single_vac$keywords_process, stopwords_es),
  description = prepare_text_custom(single_vac$description, stopwords_es)
)
single_cleaned[, text := paste(title, keywords_process, description)]
single_cleaned[, title_kw := paste(title, keywords_process)]


res <- single_two_steps( 
  corpus_one = single_cleaned,
  vocabulary_domain = vocabulary_domain,
  table_tfidf_broad = level1_tfidf,
  table_tfidf_granular = granular_tfidf,
  id_col = 'jobID_file',
  text_col1 = broad_text_col,
  text_col2 = granular_text_col,
  num_leaves_final = 5,
  print_match = FALSE)


# --- Scratch comparison block ---
# Kept for debugging / side-by-side inspection of intermediate objects.
single_vac <- df_vacantes[10,]

corpus_one <- single_cleaned
id_col = 'jobID_file'
text_col1 = 'title_kw'
text_col2 = 'text'
noisy_words = c("")
tokens_one_TKW <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col1) 
tokens_one_TKWD <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col2)
tokens_one_TKWD <- tokens_one_TKWD[!tokens_one_TKWD$term %in% noisy_words]
vocabulary_domain <- vocabulary_domain
table_tfidf_broad <- level1_tfidf
table_tfidf_granular <- granular_tfidf

res <- get_level1_exact(tokens_one_TKW, vocabulary_domain, print_match = TRUE)
match_one <- unique(res$levels)
pred1 <- classify_occ_tokens(tokens_one_TKW, 
                             table_tfidf = table_tfidf_broad,
                             num_leaves = 1,
                             max_dist = 0.1,
                             string_dist = 'jw',
                             outcode="level1")


# Manual scratch path to inspect the broad TF-IDF matching behavior.
vocabulary <- unique(table_tfidf_broad[, list(term)])[order(term)]
freeTextTokensDT <- tokens_one_TKW
vocaIndexes <- match(freeTextTokensDT$term, vocabulary$term)
if(!is.null('jw'))
  vocaIndexes[is.na(vocaIndexes)] <- stringdist::amatch(freeTextTokensDT$term[is.na(vocaIndexes)], 
                                                        vocabulary$term, maxDist =  0.1, method = 'jw')
matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
matches[, term := unlist(matches$term)]
table_tfidf[, term := unlist(table_tfidf$term)]
merged <- merge(
  matches,
  table_tfidf,
  by = "term",
  allow.cartesian = TRUE
)
predictions <- merged[, .(
  weight_sum = sum(tfIdf),
  matched_terms = paste(unique(term), collapse = ", ")
), by = .(id, class)]


predictions <- predictions[order(id, -weight_sum)][, head(.SD, num_leaves), by = id]
setnames(predictions, "class", outcode)





### PARALEL 
# Function to clean a single vacancy
library(future.apply)
library(data.table)
library(tictoc)

clean_vacancy <- function(vacancy, stopwords_es) {
  data.table(
    jobID_file = vacancy$jobID_file,
    title = prepare_text_custom(vacancy$title, stopwords_es),
    keywords_process = prepare_text_custom(vacancy$keywords_process, stopwords_es),
    description = prepare_text_custom(vacancy$description, stopwords_es)
  )[, `:=`(
    title_kw = paste(title, keywords_process),
    title_kw_des = paste(title, keywords_process, description)
  )]
}

# Set up parallel plan
plan(multisession, workers = 4)  # Adjust based on your system

tic("Procesando...")
{
  # Split vacancies into a list of rows
  vacantes_list <- split(df_vacantes, seq_len(nrow(df_vacantes)))
  
  # Process in parallel
  resultados <- future_lapply(
    vacantes_list,
    function(row) {
      # Clean the vacancy
      single_cleaned <- clean_vacancy(row, stopwords_es)
      
      # Run single_two_steps
      pred <- single_two_steps(
        corpus_one = single_cleaned,
        vocabulary_domain = vocabulary_domain,
        table_tfidf_broad = level1_tfidf,
        table_tfidf_granular = granular_tfidf,
        id_col = "jobID_file",
        text_col1 = broad_text_col,
        text_col2 = granular_text_col,
        num_leaves_final = 3,
        print_match = FALSE
      )
      
      return(pred)
    },
    future.globals = c("single_two_steps", "clean_vacancy", "prepare_text_custom", 
                       "vocabulary_domain", "level1_tfidf", "granular_tfidf", 
                       "stopwords_es","remove_stopwords_accents", "my_remove_accents",
                       "cleansing_corpus_lab", "corpus_to_dt", "get_level1_exact",
                       "classify_occ_tokens"),
    future.packages = c("data.table", "stringr","tm")
  )
  
  predictions_dt <- rbindlist(resultados, fill = TRUE)
  
  fwrite(predictions_dt, file.path(output_folder, "cuoc_predictions.csv"))
}
toc()

# Clean up
plan(sequential)

# Inspect results ----- 
head(predictions_dt)
predictions_dt[!predictions_dt$id %in% df_vacantes$jobID_file]
df <- df_vacantes[!df_vacantes$jobID_file %in% predictions_dt$id,]
# Comparison helper: join against the organized CUOC table if you want to inspect
# the labels that the predictions are landing on.
df_couc <- read.csv(file.path(data_folder, "nombre_desc_occ2022.csv"))
colnames(df_couc)
predictions_dt_ <- left_join(predictions_dt, predictions_dt, by="CuocCode")

length(unique(df_vacantes$jobID_file))
