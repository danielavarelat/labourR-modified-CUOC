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

# Helper to tokenize cleaned text into a long data.table of id-term pairs.
# This is shared by the test scripts and the reusable prediction functions.
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
  if (!is.null(string_dist))
    vocaIndexes[is.na(vocaIndexes)] <- stringdist::amatch(freeTextTokensDT$term[is.na(vocaIndexes)],
                                                          vocabulary$term,
                                                          maxDist = max_dist,
                                                          method = string_dist)

  matches <- data.table(id = freeTextTokensDT$id, term = vocabulary[vocaIndexes]$term)[!is.na(term)]
  # Si no hay términos que hicieron match, devolver tabla vacía con columnas correctas
  if (nrow(matches) == 0) {
    dt <- data.table(
      id = character(),
      weight_sum = numeric(),
      matched_terms = character()
    )
    dt[, (outcode) := character()]
    return(dt)
  }
  
  table_tfidf <- copy(table_tfidf) 
  matches[, term := unlist(term)]
  table_tfidf[, term := unlist(term)]
  
  predictions <- NULL
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
  predictions <- classify_occ_tokens(freeTextTokensDT=freeTextTokensDT,
                                     table_tfidf=table_tfidf, 
                                     outcode=outcode, 
                                     num_leaves = num_leaves, 
                                     max_dist = max_dist, 
                                     string_dist = string_dist)
  return(predictions)
  
}

# Exact level-1 shortcut based on the curated domain-specific vocabulary.
# If a vacancy hits one of these terms, we can bypass the broad TF-IDF step.
get_level1_exact <- function(dt_vac, vocabulary, print_match=FALSE) {
  # hacer match entre cada vacante DT y el vocabulary domain specific 
  dt_vac_match <- merge(dt_vac, vocabulary,  all.x = TRUE)
  dt_vac_match <- dt_vac_match[!is.na(dt_vac_match$levels),]
  if (print_match){
    print(dt_vac_match)
  }
  return(dt_vac_match)
} 

# Main two-step classifier.
# Step 1: try exact domain-specific level-1 matches.
# Step 2: otherwise use broad level-1 TF-IDF.
# Step 3: classify granular CUOC codes within the chosen level-1 bucket.
# Note: `text_col2` is configurable on purpose. Some experiments use only
# `title_kw`, while others keep a fuller `title_kw_des` variant for comparison.


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
                             noisy_words = c("")){
  
  if (dim(corpus_one)[[1]] ==1){ 
    tokens_one_TKW <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col1) 
    tokens_one_TKWD <- corpus_to_dt(corpus_one, ID= id_col, COL = text_col2)
    tokens_one_TKWD <- tokens_one_TKWD[!tokens_one_TKWD$term %in% noisy_words]
    tokens_one_TKWD <- tokens_one_TKWD[!tokens_one_TKWD$term %in% stopwords_es]
    
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
      } #else {"No domain specific term"}
      
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
    if (nrow(pred1) > 0) {
      tfidf_subset <- table_tfidf_granular[table_tfidf_granular$level1 == pred1$level1[[1]], ]
      pred2 <- classify_occ_tokens(tokens_one_TKWD, 
                                   table_tfidf = tfidf_subset,
                                   num_leaves = num_leaves_final,
                                   max_dist = max_dist,
                                   string_dist = string_dist,
                                   outcode = "CuocCode")
      pred2$TermLevel1 <- domain_spec_term
      return(pred2)
    } else {
      return(data.table(
        id = character(),
        CuocCode = character(),
        weight_sum = numeric(),
        matched_terms = character(),
        TermLevel1 = character()
      ))
      
    }
}

# Comparison-only branch kept from experimentation.
# It skips the exact level-1 shortcut and goes directly from broad TF-IDF to
# granular TF-IDF. Useful when checking how much the domain-specific shortcut
# changes behavior.
single_two_steps_incomplete <- function(corpus_one, 
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
}
  
