suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
  library(stopwords)
  library(stringr)
  library(dplyr)
  #library(readr)
  library(tm)
  library(stringi)
  #library(stringdist)

})
stopwords_es = stopwords::stopwords('es', source = "stopwords-iso")

my_remove_accents <- function(text) {
  #elimina acentos y caracteres especiales 
  return(stringi::stri_trans_general(text, "Latin-ASCII"))
}

cleansing_corpus_lab <- function (text, escape_chars = TRUE, nonalphanum = TRUE, longwords = TRUE, 
          whitespace = TRUE, tolower = TRUE) { # Función copiada de labourR
  if (class(text) != "character") 
    stop("text must be character vector")
  if (escape_chars) 
    text <- gsub("[\r\n\t]", " ", text)
  if (nonalphanum) 
    text <- gsub("[^[:alnum:]]", " ", text)
  if (longwords) 
    text <- gsub("\\w{35,}", " ", text)
  if (whitespace) 
    text <- gsub("\\s+", " ", text)
  if (tolower) 
    text <- tolower(text)
  
  text <- gsub("\\bNA\\b", "", text)
  text <- trimws(text)
  return(text)
  
}

remove_stopwords_accents <- function(text, words) {
  #Elimina stopwords (palabras vacías) específicas de un texto y luego remueve acentos
  corpus <- tm::Corpus(VectorSource(text))
  suppressWarnings(corpus <- tm::tm_map(corpus, removeWords, words))
  #suppressWarnings(tm::tm_map(corpus, my_remove_accents))
  cleaned_text <- unlist(sapply(corpus, as.character))
  return(my_remove_accents(cleaned_text))
}
#remove_accents("capitán sbcbs holça holà")
#remove_stopwords_accents("capitán adscnanaicdd. sd", "sd")

prepare_column_custom <- function(corpus, column, stopwords) {
  corpus[, (column) := remove_stopwords_accents(get(column), stopwords)]
  corpus[, (column) := cleansing_corpus_lab(as.character(get(column)))]
  #corpus[, (column) := remove_accents(get(column), stopwords)]
  return(corpus)
}

prepare_text_custom <- function(text, stopwords) {
  cleaned <- remove_stopwords_accents(text, stopwords)
  cleaned <- cleansing_corpus_lab(as.character(cleaned))
  return(cleaned)
}


corpus_to_dt <- function(corpus, ID, COL) {
  freeTextTokensList <- strsplit(corpus[[COL]], split = " ")
  names(freeTextTokensList) <- corpus[[ID]]
  freeTextTokensList <-
    lapply(freeTextTokensList, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  freeTextTokensDT <- freeTextTokensDT[!grepl("^\\d+$", term)] 
  return(freeTextTokensDT)
}

get_place_words <- function(corpus){
  freeTextTokensList_place <- lapply(strsplit(corpus$place, split = " "), function(x) x[!x %in% stopwords_es])
  freeTextTokensList_place <-
    lapply(freeTextTokensList_place, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  names(freeTextTokensList_place) <- corpus$id
  freeTextTokensDT_place <- lapply(freeTextTokensList_place, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  freq <- freeTextTokensDT_place[, .(unique_terms = uniqueN(id)), by = term]
  vocabulary_places <- unique(freeTextTokensDT_place$term)
  return(vocabulary_places)
}

# verificar_coincidencia <- function(corpus, bigrama_o_lista) {
#   coincidencias <- lapply(bigrama_o_lista, function(bigram) {
#     result <- grep(bigram, corpus)
#     if(length(result) > 0) {
#       return(bigram)
#       #return(list(bigram = bigram))
#     }
#   })
#   coincidencias <- Filter(Negate(is.null), coincidencias)
#   if (length(coincidencias) == 0) {
#     return(list())
#   } else {
#     return(coincidencias)
#   }
# }


verificar_coincidencia <- function(texto, terminos, use_word_boundaries = FALSE) {
  coincidencias <- Filter(function(t) {
    if (use_word_boundaries) {
      pattern <- paste0("\\b", t, "\\b")
      grepl(pattern, texto, ignore.case = TRUE)
    } else {
      grepl(t, texto, fixed = TRUE)
    }
  }, terminos)
  
  return(coincidencias)
}

verificar_coincidencia_fast <- function(texto, bigramas) {
  matches <- bigramas[stri_detect_fixed(texto, bigramas)]
  return(matches)
}


### PARA TFIDF ----
lab_tf_idf <- function (corpus,
                        stopwords = stopwords_es,
                        id_col = "id",
                        text_col = "text",
                        tf_weight = "double_norm",
                        idf_weight = "idf_smooth",
                        min_chars = 2,
                        norm = TRUE)
{
  print(paste0("using tf_weight: ", tf_weight))
  print(paste0("using idf_weight: ", idf_weight))
  tfIdf <- docFreq <- idf <- term_count <- tf <- term <- NULL
  corpus <- data.table(corpus)
  tokensList <- strsplit(corpus[, get(text_col)], " ")
  names(tokensList) <- corpus[, get(id_col)]
  tokensDT <- lapply(tokensList, data.table) %>% rbindlist(idcol = TRUE) %>%
    setnames(c("class", "term"))
  tokensDT <- tokensDT[!term %in% stopwords][nchar(term) >=
                                               min_chars]
  tfDT <- tokensDT[, list(term_count = .N), by = c("class", "term")]
  if (tf_weight == "double_norm")
    tfDT[, `:=`(tf, 0.5 + 0.5 * term_count / max(term_count, na.rm = TRUE)), by = "class"]
  if (tf_weight == "raw_count")
    tfDT[, `:=`(tf, term_count)]
  if (tf_weight == "log_norm")
    tfDT[, `:=`(tf, log(1 + term_count))]
  idfDT <- tokensDT[!duplicated(tokensDT)][, list(docFreq = .N), by = "term"]
  if (idf_weight == "idf_smooth")
    idfDT[, `:=`(idf, log(length(unique(tokensDT$class)) / (docFreq +
                                                              1)) + 1)]
  if (idf_weight == "idf")
    idfDT[, `:=`(idf, log(length(unique(tokensDT$class)) / docFreq))]
  res <- merge(tfDT, idfDT)[, `:=`(tfIdf, tf * idf)][, list(class, term, tfIdf)]
  if (norm)
    res[, `:=`(tfIdf, tfIdf / sum(tfIdf)), by = "class"]
  res
}

