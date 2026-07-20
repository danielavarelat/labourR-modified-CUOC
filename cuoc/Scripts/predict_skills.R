library(data.table)
library(magrittr)
library(labourR)
library(stringr)
library(dplyr)
library(readr)
library(tm)
library(stringi)


predict_skills <- function(corpus, table_tfidf, num_leaves = 10, max_dist = 0.1, string_dist = NULL) {
  # Required columns: idvacancy + text
  if (!any("data.frame" %in% class(corpus)))
    stop("Corpus must be either a data.frame or a data.table.")
  
  if (!"idvacancy" %in% names(corpus))
    stop(paste0(
      "Corpus must contain the specified variables: idvacancy"
    ))
  vocabulary_skills <- unlist(unique(table_tfidf$term))
  # Cleanse, tokenize free-text and remove stopwords.
  freeTextTokensList <-
    lapply(corpus$text, function(x)
      unlist(strsplit(x, " ")))
  names(freeTextTokensList) <- corpus$idvacancy
  freeTextTokensList <-
    lapply(freeTextTokensList, function(sublist)
      Filter(function(x)
        nchar(x) >= 3, sublist))
  freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
    rbindlist(idcol = TRUE) %>%
    setnames(c("id", "term"))
  # Checking matches between vocabularies (vacancies vs skills)
  vocabulary_vac <- unique(freeTextTokensDT$term)
  # By default: exact matches
  vocaIndexes <- match(vocabulary_vac, vocabulary_skills)
  if (!is.null(string_dist)) {
    print(string_dist)
    vocaIndexes[is.na(vocaIndexes)] <-
      amatch(vocabulary_vac[is.na(vocaIndexes)],
             vocabulary_skills,
             maxDist = max_dist,
             method = string_dist)
    
  }
  df <- data.frame(vocabulary_vac)
  df$match <- vocabulary_skills[vocaIndexes]
  df$indices <- match(df$match, vocabulary_skills)
  print(colSums(!is.na(df), na.rm = FALSE))
  
  # According to the metric, get the terms that matched 
  vocabulary_vac_match <- df[!is.na(df$match),]$vocabulary_vac
  
  matches <- freeTextTokensDT[term %in% vocabulary_vac_match]
  matches_ind <-
    merge(matches,
          df[, c("vocabulary_vac", "indices")],
          by.x = "term",
          by.y = "vocabulary_vac",
          all.x = TRUE)
  matches_ind[, term2 := vocabulary_skills[indices]]
  matches_final <- matches_ind[, list(id, term2)]
  setnames(matches_final, c("id", "term"))
  matches_final[, term := unlist(matches_final$term)]
  table_tfidf[, term := unlist(table_tfidf$term)]
  predictions <- merge(matches_final,
                       table_tfidf,
                       allow.cartesian = TRUE)[, list(weight_sum = sum(tfIdf)), by = c("id", "class")][order(id, -weight_sum)][, head(.SD, num_leaves), by = "id"]
  predictions
}

predict_skills_titles <- function(corpus, table_tfidf, num_leaves = 10, max_dist = 0.1, string_dist = NULL, Factor = 50) {
    if (!any("data.frame" %in% class(corpus)))
      stop("Corpus must be either a data.frame or a data.table.")
    
    if (!all(c("idvacancy", "title") %in% names(corpus)))
      stop(paste0(
        "Corpus must contain the specified variables: idvacancy and title"
      ))
    vocabulary_skills <- unlist(unique(table_tfidf$term))
    
    freeTextTokensList <-
      lapply(corpus$text, function(x)
        unlist(strsplit(x, " ")))
    names(freeTextTokensList) <- corpus$idvacancy
    freeTextTokensList <-
      lapply(freeTextTokensList, function(sublist)
        Filter(function(x)
          nchar(x) >= 3, sublist))
    freeTextTokensDT <- lapply(freeTextTokensList, data.table) %>%
      rbindlist(idcol = TRUE) %>%
      setnames(c("id", "term"))
    
    
    freeTextTokensList_title <-
      lapply(corpus$title, function(x)
        unlist(strsplit(x, " ")))
    names(freeTextTokensList_title) <- corpus$idvacancy
    
    freeTextTokensList_title <-
      lapply(freeTextTokensList_title, function(sublist)
        Filter(function(x)
          nchar(x) >= 3, sublist))
    freeTextTokensDT_title <-
      lapply(freeTextTokensList_title, data.table) %>%
      rbindlist(idcol = TRUE) %>%
      setnames(c("id", "term"))
    vocabulary_vac <- unique(freeTextTokensDT$term)
    vocaIndexes <- match(vocabulary_vac, vocabulary_skills)
    vocabulary_vac_title <- unique(freeTextTokensDT_title$term)
    vocaIndexes_list <- match(vocabulary_vac_title, vocabulary_skills)
    if (!is.null(string_dist)) {
      vocaIndexes[is.na(vocaIndexes)] <-
        amatch(vocabulary_vac[is.na(vocaIndexes)],
               vocabulary_skills,
               maxDist = max_dist,
               method = string_dist)
      
      vocaIndexes_list[is.na(vocaIndexes_list)] <-
        amatch(
          vocabulary_vac_title[is.na(vocaIndexes_list)],
          vocabulary_skills,
          maxDist = max_dist,
          method = string_dist
        )
      
      
    }
    
    df <- data.frame(vocabulary_vac)
    df$match <- vocabulary_skills[vocaIndexes]
    df$indices <- match(df$match, vocabulary_skills)
    print("Title + description")
    print(colSums(!is.na(df), na.rm = FALSE))
    print("Title")
    dft <- data.frame(vocabulary_vac_title)
    dft$match <- vocabulary_skills[vocaIndexes_list]
    dft$indices <- match(dft$match, vocabulary_skills)
    print(colSums(!is.na(dft), na.rm = FALSE))
    
    vocabulary_vac_match <- df[!is.na(df$match), ]$vocabulary_vac
    vocabulary_vac_match_title <-
      dft[!is.na(dft$match), ]$vocabulary_vac_title
    
    matches <- freeTextTokensDT[term %in% vocabulary_vac_match]
    matches_title <-
      freeTextTokensDT_title[term %in% vocabulary_vac_match_title]
    
    matches_ind <-
      merge(matches,
            df[, c("vocabulary_vac", "indices")],
            by.x = "term",
            by.y = "vocabulary_vac",
            all.x = TRUE)
    
    matches_ind_title <-
      merge(
        matches_title,
        dft[, c("vocabulary_vac_title", "indices")],
        by.x = "term",
        by.y = "vocabulary_vac_title",
        all.x = TRUE
      )
    
    matches_ind[, term2 := vocabulary_skills[indices]]
    matches_ind_title[, term2 := vocabulary_skills[indices]]
    
    matches_final <- matches_ind[, list(id, term2)]
    matches_final_title <- matches_ind_title[, list(id, term2)]
    
    setnames(matches_final, c("id", "term"))
    setnames(matches_final_title, c("id", "term"))
    
    matches_final[, term := unlist(matches_final$term)]
    matches_final_title[, term := unlist(matches_final_title$term)]
    table_tfidf[, term := unlist(table_tfidf$term)]
    
    pre_merge <- merge(matches_final,
                       table_tfidf,
                       allow.cartesian = TRUE)
    
    matches_final_title$title <- Factor
    merge_scores_titles <- merge(pre_merge,
                                 matches_final_title,
                                 by = c("id", "term"),
                                 all.x = TRUE)
    merge_scores_titles[is.na(merge_scores_titles$title), title := 1]
    merge_scores_titles[, tfIdf := tfIdf * title]
    predictions <-
      merge_scores_titles[, list(weight_sum = sum(tfIdf)), by = c("id", "class")][order(id,-weight_sum)][, head(.SD, num_leaves), by = "id"]
    predictions
    
  }
