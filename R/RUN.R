## RUN LABOUR NORMAL 

library(labourR)
library(data.table)
library(magrittr)
library(stringdist)

load("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/sysdata.rda")
corpus_in <- data.frame(
  id = 1:3,
  text = c("Asesor comercial in-office con telemercadeo", 
           "arquitecto de datos junior", "cajero en  McDonald's")
)
classify_occupation(corpus = corpus_in, isco_level = 2,  lang = "es", num_leaves = 3)

### probar con vacante real 
library(labourR)
library(devtools)
reload(pkgload::inst("labourR"))
library(labourR)

vac = "123FD4065A47030F61373E686DCF3405"
vacante <- corpus_ready[corpus_ready$idvacancy == vac]
colnames(vacante)

original_lab <- classify_occupation(
  corpus_in,
  id_col = "id",
  text_col = "text",
  lang = "es",
  num_leaves = 5,
  isco_level = 1,
  max_dist = 0.1,
  string_dist = NULL
)


## NUESTRA FUNCIÓN!! - con cualquier base tfidf - pero solo 1 ------
corpus_in <- data.frame(
  id = 1:4,
  text = c("auxiliar de laboratorio ingeniero en biomedicina electronico", 
           "director de mercadeo", "cajero en  McDonald's",
           "gerente de banco")
)

level1_tfidf <- readRDS("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tfidf_tokens_level1_den.rds")
setnames(level1_tfidf, "level1", "class")
file = '/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/Correlativa_CUOC-2022_Vs_CNO-2022.xlsx'
desc <- read_excel(file, sheet = "Descripciones CUOC 2022")
random_rows <- corpus_ready[sample(.N, 100)]
colnames(corpus_ready)
source("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/occupations_classify_new.R")
colnames(vacante)

classify_occupation_2(
  corpus_in,
  table_tfidf = level1_tfidf,
  id_col = "id",
  text_col = "text",
  num_leaves = 1,
  max_dist = 0.1,
  string_dist = "jw"
)
length(random_rows[1,]$text)

cuoc_level1 <- classify_occupation_2(
  random_rows,
  table_tfidf = res_tfidf,
  id_col = "idvacancy",
  text_col = "text",
  num_leaves = 1,
  max_dist = 0.1,
  string_dist = "jw"
)

setnames(cuoc_level1,"id", "idvacancy")
colnames(corpus_ready)
cuoc_level1 <- merge(
  cuoc_level1,
  corpus_ready[, list(idvacancy, descripcion, title, text)],
  on = "idvacancy"
)

write_csv(cuoc_level1, "/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/data-raw/cuoc/tests/cuoc_level1_from_denom.csv")


## NUEVA FUNCTION - MULTISTEPS ------ 

### ESTO ES LO QUE SE ESTÁ HACINEDO EL ORIGINAL CON LOS ISCO LEVELS!! !! -----  
corpus <- data.table(corpus_in)
id_col = "id"
text_col = "text"
num_leaves = 3
max_dist = 0.1
string_dist = NULL
occupations_bundle <- occupations_bundle #english
isco_occupations_bundle <- isco_occupations_bundle  #english
load("/Users/dvarelat/Documents/PROYECTOS/NLP/labourR/R/sysdata.rda") # esto carga tfidf_tokens
lang = "es"


setnames(corpus, c(id_col, text_col), c("id", "text"))
weightTokens <- tfidf_tokens[language == lang]
if(nrow(weightTokens) == 0)
  stop(paste0(lang, " is not an acceptable language."))
vocabulary <- unique(tfidf_tokens[language == lang][, list(term)])[order(term)]
corpus[, text := cleansing_corpus(as.character(text))]
freeTextTokensList <- lapply(strsplit(corpus$text, split = " "), function(x) x[!x %in% get_stopwords(lang)])
names(freeTextTokensList) <- corpus$id
freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
  rbindlist(idcol = TRUE) %>%
  setnames(c("id", "term"))
vocaIndexes <- match(freeTextTokensDT$term, vocabulary$term)
if(!is.null(string_dist))
  vocaIndexes[is.na(vocaIndexes)] <- amatch(freeTextTokensDT$term[is.na(vocaIndexes)], vocabulary$term, maxDist = max_dist, method = string_dist)
matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
predictions <- merge(
  matches,
  weightTokens,
  allow.cartesian = TRUE
)[, list(weight_sum = sum(tfIdf)), by = c("id", "class")][order(id, -weight_sum)][, head(.SD, num_leaves), by = "id"]
setnames(predictions, "class", "conceptUri")

isco_level = 2
if(!is.null(isco_level)) { ## SI HAY ISCO LEVEL
  predictions <- merge(predictions, occupations_bundle[, list(conceptUri, iscoGroup)], on = "conceptUri")
  predictions[, iscoGroup := substr(iscoGroup, 0, isco_level)]
  predictions <- predictions[, list(isco_nn = .N), by = c("id", "iscoGroup")
  ][order(id, -isco_nn)
  ][, head(.SD, 1), by = "id"]
  # Add new variable, the preferred label of the ISCO occupation.
  predictions <- merge(
    predictions,
    isco_occupations_bundle,
    on = "iscoGroup"
  )[order(id, -isco_nn)][, list(id, iscoGroup, preferredLabel)]
} else {
  # Add new variable, the preferred label of the ESCO occupations.
  predictions <- merge(
    predictions,
    occupations_bundle[, list(conceptUri, preferredLabel)],
    on = "conceptUri"
  )[order(id, -weight_sum)][, list(id, conceptUri, preferredLabel)]
}
setnames(predictions, "id", id_col)
