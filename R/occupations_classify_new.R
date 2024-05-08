library(data.table)
library(magrittr)
library(labourR)
library(stringr)
library(dplyr)
library(readr)
library(tm)
library(stringi)
library(stringdist)

my_remove_accents <- function(text) {
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
  trimws(text)
}

remove_stopwords_accents <- function(text, words) {
  corpus <- tm::Corpus(VectorSource(text))
  suppressWarnings(corpus <- tm::tm_map(corpus, removeWords, words))
  #suppressWarnings(tm::tm_map(corpus, my_remove_accents))
  cleaned_text <- unlist(sapply(corpus, as.character))
  return(my_remove_accents(cleaned_text))
}
#remove_accents("capitán sbcbs holça holà")
#remove_stopwords_accents("capitán adscnanaicdd. sd", "sd")

prepare_column_custom <- function(corpus, column, stopwords) {
  corpus[, (column) := cleansing_corpus_lab(as.character(get(column)))]
  corpus[, (column) := remove_stopwords_accents(get(column), stopwords)]
  #corpus[, (column) := remove_accents(get(column), stopwords)]
  return(corpus)
}

remove_stopwords_accents("capitán", "")
classify_occupation_2 <- function(corpus, 
                                  table_tfidf, 
                                  id_col = "id", 
                                  text_col = "text",
                                  outcode="CuocCode",  
                                  num_leaves = 5, 
                                  max_dist = 0.1, 
                                  string_dist = "jw") {
  ## Función para predecir a partir de un corpus de entrada ya listo y una tabla tfidf
  ## Input table_tfidf --> class + term + tfIdf 
  if(!any("data.frame" %in% class(corpus)))
    stop("Corpus must be either a data.frame or a data.table.")
  if(!all(c(id_col, text_col) %in% names(corpus)))
    stop(paste0("Corpus must contain the specified variables: ", id_col, " and ", text_col, "."))
  # Prepare corpus.
  corpus <- data.table(corpus)
  setnames(corpus, c(id_col, text_col), c("id", "text"))
  # Prepare the weighted tokens and the vocabulary.
  vocabulary <- unique(table_tfidf[, list(term)])[order(term)]
  # Cleanse, tokenize free-text and remove stopwords.
  # corpus <- prepare_column_custom(corpus, column="text", stopwords=get_stopwords("es"))
  #corpus[, text := cleansing_corpus(as.character(text))]
  #corpus[, text := remove_stopwords_accents(text, get_stopwords("es"))] ## NEW
  freeTextTokensList <- lapply(strsplit(corpus$text, split = " "), function(x) x[!x %in% get_stopwords('es')])
  names(freeTextTokensList) <- corpus$id
  freeTextTokensList <-
    lapply(freeTextTokensList, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  # Match free-text with the vocabulary.
  vocaIndexes <- match(freeTextTokensDT$term, vocabulary$term)
  if(!is.null(string_dist))
    vocaIndexes[is.na(vocaIndexes)] <- stringdist::amatch(freeTextTokensDT$term[is.na(vocaIndexes)], vocabulary$term, maxDist = max_dist, method = string_dist)
  matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
  # Join the free-text matches with the tfidf weighted tokens and keep the top num_leaves using a weighted sum model.
  matches[, term := unlist(matches$term)]
  table_tfidf[, term := unlist(table_tfidf$term)]
  predictions <- NULL
  if (dim(matches)[[1]] > 0 ){
    predictions <- merge(
      matches,
      table_tfidf,
      allow.cartesian = TRUE
    )[, list(weight_sum = sum(tfIdf)), by = c("id", "class")][order(id, -weight_sum)][, head(.SD, num_leaves), by = "id"]
    setnames(predictions, "class", outcode)
  }
  return(predictions)
  
}


single_two_steps_classify <- function(corpus_one, 
                                      table_tfidf_broad,
                                      table_tfidf_granular, 
                                      id_col = "id", 
                                      text_col1 = "text",
                                      text_col2 = "text",  
                                      num_leaves_final = 5, 
                                      max_dist = 0.1, 
                                      string_dist = "jw") {
  # Función para predecir a partir de una sola vacante ya limpia y un tfidf
  # table_tfidf_broad--> class + term + tfIdf 
  # table_tfidf_granular--> class + term + tfIdf 
  if (dim(corpus_one)[[1]] ==1){
    pred1 <- classify_occupation_2(
      corpus_one,
      table_tfidf = table_tfidf_broad,
      id_col = id_col,
      text_col = text_col1,
      num_leaves = 1,
      max_dist = max_dist,
      string_dist = string_dist,
      outcode="level1"
    )
    if (!is.null(pred1)){
      pred1 <- merge(corpus_one, pred1)
      tfidf_subset <- table_tfidf_granular[table_tfidf_granular$level1 == pred1$level1[[1]],]
      pred2 <- classify_occupation_2(
        pred1,
        table_tfidf = tfidf_subset,
        id_col = id_col,
        text_col = text_col2,
        num_leaves = num_leaves_final,
        max_dist = max_dist,
        string_dist = string_dist,
        outcode="CuocCode"
      )
      return(pred2)
    }

  } else {
    print("Not built for more than one input")
    return(NULL)
    
    }
  
}

get_level1_exact <- function(dt_vac, vocabulary) {
  # hacer match entre cada vacante DT y el vocabulary domain specific 
  dt_vac_match <- merge(dt_vac, vocabulary,  all.x = TRUE)
  lev <- dt_vac_match$levels[!is.na(dt_vac_match$levels)]
  return(lev)
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
  
