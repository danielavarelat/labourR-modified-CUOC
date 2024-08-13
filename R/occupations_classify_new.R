suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
  library(labourR)
  library(stringr)
  library(dplyr)
  library(readr)
  library(tm)
  library(stringi)
  library(stringdist)
})

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

classify_occ_tokens <- function(freeTextTokensDT,
                                   table_tfidf, 
                                   outcode, 
                                   num_leaves = 5, 
                                   max_dist = 0.1, 
                                   string_dist = "jw"){
  ## ya entra aquí la tabla freeTextTokensDT que solo tiene ID+TERM
  ## es una versión más corta de classify_occupation_2
  vocabulary <- unique(table_tfidf[, list(term)])[order(term)]
  vocaIndexes <- match(freeTextTokensDT$term, vocabulary$term)
  if(!is.null(string_dist))
    vocaIndexes[is.na(vocaIndexes)] <- stringdist::amatch(freeTextTokensDT$term[is.na(vocaIndexes)], vocabulary$term, maxDist = max_dist, method = string_dist)
  matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
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
  corpus_ <- data.table(corpus)
  setnames(corpus_, c(id_col, text_col), c("id", "text"))
  
  freeTextTokensDT <- corpus_to_dt(corpus_, ID="id", COL = "text")
  
  # Match free-text with the vocabulary.
  vocabulary <- unique(table_tfidf[, list(term)])[order(term)]
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

get_level1_exact <- function(dt_vac, vocabulary, print_match=FALSE) {
  # hacer match entre cada vacante DT y el vocabulary domain specific 
  dt_vac_match <- merge(dt_vac, vocabulary,  all.x = TRUE)
  dt_vac_match <- dt_vac_match[!is.na(dt_vac_match$levels),]
  if (print_match){
    print(dt_vac_match)
  }
  return(dt_vac_match)
} 


single_two_steps <- function(corpus_one, 
                             vocabulary_domain, 
                             table_tfidf_broad,
                             table_tfidf_granular,
                             id_col='ID_file', 
                             text_col1='title_kw', 
                             text_col2='title_kw_des',
                             num_leaves_final = 5, 
                             max_dist = 0.1, 
                             string_dist = "jw", 
                             print_match=FALSE, 
                             noisy_words = noisy_words){
  
  if (dim(corpus_one)[[1]] ==1){ 
    tokens_one_TKW <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col1) 
    tokens_one_TKWD <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col2)
    tokens_one_TKWD <- tokens_one_TKWD[!tokens_one_TKWD$term %in% noisy_words]
    
    
    res <- get_level1_exact(tokens_one_TKW, vocabulary_domain, print_match=print_match)
    match_one <- unique(res$levels)
    
    if (length(match_one) == 1){
      if (print_match){
        print(paste0("Matched domain specific word in level = ", match_one))
      }
      pred1 <- as.data.frame(list(id="vac", level1=match_one))
      domain_spec_term <- unique(res$term)
      if (length(domain_spec_term) > 1) {
        domain_spec_term <- paste(domain_spec_term, collapse = ",")
      }
      
    } else {
      domain_spec_term <- ""
      if (print_match){
        print("DID NOT match an specific word")
      }
      pred1 <- classify_occ_tokens(tokens_one_TKW, 
                                   table_tfidf = table_tfidf_broad,
                                   num_leaves = 1,
                                   max_dist = 0.1,
                                   string_dist = 'jw',
                                   outcode="level1")
      #pred1$weight_sum <- NULL
    }
    if (!is.null(pred1)){
      tfidf_subset <- table_tfidf_granular[table_tfidf_granular$level1 == pred1$level1[[1]],]
      pred2 <- classify_occ_tokens(tokens_one_TKWD, 
                                   table_tfidf = tfidf_subset,
                                   num_leaves = num_leaves_final,
                                   max_dist = max_dist,
                                   string_dist = string_dist,
                                   outcode="CuocCode")
      pred2$TermLevel1 <- domain_spec_term
      return(pred2)
    } else {return(NULL)}
  }
  else {
    #print("Not built for more than one input")
    return(NULL)
  }
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
    #print(colnames(corpus_one))
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
    #print(pred1)
    if (!is.null(pred1)){
      #pred1 <- merge(corpus_one, pred1)
      tfidf_subset <- table_tfidf_granular[table_tfidf_granular$level1 == pred1$level1[[1]],]
      #print(colnames(tfidf_subset))
      pred2 <- classify_occupation_2(
        corpus_one,
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
  
