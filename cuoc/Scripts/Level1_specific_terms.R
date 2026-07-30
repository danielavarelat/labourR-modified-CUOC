
source('/Users/DVARELAT/Documents/ROSARIO2026/labourR-modified-CUOC/cuoc/Scripts/utils_functions.R')

library(data.table)
library(dplyr)
library(magrittr)
library(readr)
library(readxl)

### PARAMETERS ----
project_root <- "/Users/DVARELAT/Documents/ROSARIO2026/labourR-modified-CUOC"
data_folder <- file.path(project_root, "cuoc", "Data")
file_freq <- file.path(data_folder, "terms_denom_freq_level1.csv")
file_freq_one <- file.path(data_folder, "terms_denom_freq_level1_one.csv")
file_semantic_review <- file.path(data_folder, "cuoc_level1_semantic_review.csv")
file_vocab <- file.path(data_folder, "domain_specific_v1.rds")
file_review_compare <- file.path(data_folder, "cuoc_level1_manual_vs_semantic_review.csv")
file_keep_agreed <- file.path(data_folder, "cuoc_level1_keep_agreed.csv")
file_high_conflict <- file.path(data_folder, "cuoc_level1_high_conflict.csv")


### LEER ENTRADAS ----
{
  df_cuoc_denom <- read_excel(paste0(data_folder, '/Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                              sheet = "Denominaciones CUOC 2022")
  
}
## Organizar ----
dt <- data.table(df_cuoc_denom)
setnames(dt, "Nombre Denominación - CUOC 2022", "Description")
setnames(dt, "Ocupación", "occ")
setnames(dt, "Gran Grupo", "level1")
dt <- dt[, list(level1, occ, Description)]


### Limpieza ----
dt <- prepare_column_custom(dt, column = "Description", stopwords = stopwords_es)
dt <- prepare_column_custom(dt, column = "occ", stopwords = stopwords_es)

### Agrupar por nivel 1 y convertir en lista ---- 
{
  grouped_level1 <- dt[, lapply(.SD, paste0, collapse=" "), by = level1]
  tokensList <- strsplit(grouped_level1[, get("Description")], " ")
  tokensList <- lapply(tokensList, function(sublista) sublista[sublista != ""])
  names(tokensList) <- grouped_level1$level1
  tokensDT <- lapply(tokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("level1", "term"))
  tokensDT <- tokensDT[!term %in% stopwords_es][nchar(term) > 3]
  freq_terms_level1 <- tokensDT[, .(count = uniqueN(level1), 
                                    levels = list(unique(level1))), by = term]
  freq_terms_level1[, levels := sapply(levels,  function(x) paste(x, collapse = ","))]
  ## save each term and all its levels ------
  write_csv(as.data.frame(freq_terms_level1), file_freq)
}
freq_terms_level1 <- read_csv(file_freq)
freq_terms_level1_unique <- freq_terms_level1[freq_terms_level1$count == 1,]
write_csv(as.data.frame(freq_terms_level1_unique), file_freq_one)



## RESULTADO FINAL basado en revisión manual ----- 
df <- read_excel(file.path(data_folder, "terms_denom_freq_level_all_v2.xlsx"))
df <- data.table(df[df$count ==1,])
df <- df %>% replace(is.na(.), 0)
df$sum <- df$Andres + df$Alex_v2 + df$`Pamela v2`
df <- df[df$sum >0,]
df[, manual_keep := sum > 0]
df[, level1_candidate := as.character(levels)]
df[, levels := as.character(levels)]

### RESULTADO FINAL basado en revisión semántica ----- 
semantic_review <- read_csv(file_semantic_review, show_col_types = FALSE)
setDT(semantic_review)
semantic_review[, semantic_keep := decision == "keep"]
semantic_review[, semantic_drop := decision == "drop"]
semantic_review[, semantic_review_flag := decision == "review"]
semantic_review[, level1_candidate := as.character(level1_candidate)]

### Compare manual vs semantic review ----
review_compare <- merge(
  df,
  semantic_review,
  by = c("term", "level1_candidate"),
  all.x = TRUE,
  suffixes = c("_manual", "_semantic")
)

review_compare[, manual_keep_flag := manual_keep]
review_compare[, semantic_keep_flag := semantic_keep]
review_compare[, agreed := manual_keep_flag == semantic_keep_flag]
review_compare[, high_conflict := confidence_label == "high" & manual_keep_flag != semantic_keep_flag]

fwrite(review_compare, file_review_compare)

### Useful subsets for the next human pass ----
# Keep terms where manual review and semantic review agree on keeping them.
keep_agreed <- review_compare[manual_keep_flag == TRUE & semantic_keep_flag == TRUE]
fwrite(keep_agreed, file_keep_agreed)

# Terms with high semantic confidence but disagreement with manual review.
# This is the queue you asked to inspect manually before deciding the final rule.
high_conflict <- review_compare[high_conflict == TRUE]
fwrite(high_conflict, file_high_conflict)

# Manual-only terms and semantic-only terms can be inspected separately if you
# want to decide whether the final vocabulary should be conservative or broad.
manual_only <- review_compare[manual_keep_flag == TRUE & semantic_keep_flag != TRUE]
semantic_only <- review_compare[manual_keep_flag != TRUE & semantic_keep_flag == TRUE]
fwrite(manual_only, file.path(data_folder, "cuoc_level1_manual_only.csv"))
fwrite(semantic_only, file.path(data_folder, "cuoc_level1_semantic_only.csv"))

### Final vocabulary candidates ----
# Default conservative candidate set derived from the intersection of the
# manual review and the semantic review.
vocabulary <- data.table(keep_agreed[, c("term", "levels")])
dim(vocabulary)
saveRDS(vocabulary, file_vocab)
