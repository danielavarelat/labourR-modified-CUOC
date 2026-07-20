
source('/Users/DVARELAT/Documents/NLP/GreenScore/Scripts/utils_functions.R')


### LEER ENTRADAS ----
{
  data_folder <- "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/"
  df_cuoc_denom <- read_excel(paste0(data_folder, 'Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'), 
                              sheet = "Denominaciones CUOC 2022")
  
}
## Organizar ----
dt <- data.table(df_cuoc_denom)
colnames(desc)
setnames(dt, "Nombre Denominación - CUOC 2022", "Description")
setnames(dt, "Ocupación", "occ")
setnames(dt, "Gran Grupo", "level1")
dt <- dt[, list(level1, occ, Description)]
stopwords_es

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
  file_freq <- "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/tests/terms_denom_freq_level1.csv"
  write_csv(as.data.frame(freq_terms_level1), file_freq)
}
freq_terms_level1 <- read_csv(file_freq)
freq_terms_level1 <- freq_terms_level1[freq_terms_level1$count == 1,]


## RESULTADO FINAL ----- 
df <- read_excel("/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/terms_denom_freq_level_all_v2.xlsx")
df <- df[df$count ==1,]
df <- df %>% replace(is.na(.), 0)
df$sum <- df$Andres + df$Alex_v2 + df$`Pamela v2`
df <- df[df$sum >0,]
vocabulary <- data.table(df[,c("term", "levels")])
dim(vocabulary)
saveRDS(vocabulary, "/Users/DVARELAT/Documents/NLP/labourR/R/cuoc/Data/domain_specific_v1.rds")





