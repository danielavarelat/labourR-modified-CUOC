suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(readxl)
  library(future.apply)
  library(tictoc)
})

### CONFIGURACION ----
# Este script está pensado para correr un solo archivo de vacantes por ejecución.
# Recomendación operativa:
# - un archivo por job del cluster
# - procesamiento interno secuencial por defecto
# - paralelismo interno solo si el nodo tiene RAM suficiente

get_script_file <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

script_file <- get_script_file()
if (!is.na(script_file)) {
  script_folder <- dirname(script_file)
} else {
  script_folder <- normalizePath(file.path(getwd(), "cuoc", "Scripts"), winslash = "/", mustWork = FALSE)
}
project_root <- normalizePath(file.path(script_folder, "..", ".."), winslash = "/", mustWork = FALSE)
data_folder <- file.path(project_root, "cuoc", "Data")
output_folder <- file.path(project_root, "cuoc", "Results")

# Columnas base esperadas para este corpus.
# Puedes cambiarlas por argumento de consola si otro archivo usa otros nombres.
id_col <- "ID_file"
title_col <- "title"
keywords_col <- "keywords"
description_col <- "full_descr"

broad_text_col <- "title_kw"
granular_text_col <- "title_kw_des"
num_leaves_final <- 3
use_parallel <- FALSE
n_workers <- 2

### ARGUMENTOS ----
parse_arg <- function(arg_name, default = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  prefix <- paste0("--", arg_name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

parse_flag <- function(arg_name, default = FALSE) {
  value <- parse_arg(arg_name, if (default) "1" else "0")
  value %in% c("1", "TRUE", "true", "yes", "YES")
}

input_csv <- parse_arg("input")
output_csv <- parse_arg("output")
id_col <- parse_arg("id_col", id_col)
title_col <- parse_arg("title_col", title_col)
keywords_col <- parse_arg("keywords_col", keywords_col)
description_col <- parse_arg("description_col", description_col)
broad_text_col <- parse_arg("broad_text_col", broad_text_col)
granular_text_col <- parse_arg("granular_text_col", granular_text_col)
num_leaves_final <- as.integer(parse_arg("num_leaves_final", num_leaves_final))
use_parallel <- parse_flag("use_parallel", use_parallel)
n_workers <- as.integer(parse_arg("workers", n_workers))

if (is.na(num_leaves_final) || num_leaves_final < 1) {
  stop("El argumento --num_leaves_final debe ser un entero mayor o igual a 1.")
}
if (is.na(n_workers) || n_workers < 1) {
  stop("El argumento --workers debe ser un entero mayor o igual a 1.")
}
if (is.null(input_csv) || input_csv == "") {
  stop("Debes indicar el archivo de entrada con --input=/ruta/al/csv")
}

if (is.null(output_csv) || output_csv == "") {
  base_name <- tools::file_path_sans_ext(basename(input_csv))
  output_csv <- file.path(output_folder, paste0(base_name, "_predicciones.csv"))
}

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

### CARGA DE FUNCIONES Y TABLAS ----
source(file.path(script_folder, "Occupations_classify_new.R"))
source(file.path(script_folder, "utils_functions.R"))

level1_tfidf <- readRDS(file.path(data_folder, "tfidf_tokens_level1_den.rds"))
granular_tfidf <- readRDS(file.path(data_folder, "tfidf_tokens_cuoc.rds"))
vocabulary_domain <- readRDS(file.path(data_folder, "domain_specific_v1.rds"))
cuoc_ref <- fread(file.path(data_folder, "nombre_desc_occ2022.csv"))
prim_ref <- data.table(
  read_excel(
    file.path(data_folder, "Correlativa_CUOC-2022_Vs_CNO-2022.xlsx"),
    sheet = "Grupos Primarios - CUOC 2022"
  )
)[, .(
  Primario = `Grupo Primario`,
  NombrePrimario = `Nombre Grupo Primario - CUOC 2022`
)]

### LECTURA DE VACANTES ----
vacantes <- fread(input_csv)

required_cols <- c(id_col, title_col, keywords_col, description_col)
missing_cols <- setdiff(required_cols, names(vacantes))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "El archivo de entrada no tiene las columnas requeridas: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

if (id_col %in% names(vacantes)) {
  n_before <- nrow(vacantes)
  vacantes <- unique(vacantes, by = id_col)
  n_after <- nrow(vacantes)
  if (n_after < n_before) {
    cat("IDs duplicados eliminados por", id_col, ":", n_before - n_after, "\n")
  }
}

### LIMPIEZA ----
clean_vacancy <- function(vacancy_row) {
  data.table(
    jobID_file = vacancy_row[[id_col]],
    title = prepare_text_custom(vacancy_row[[title_col]], stopwords_es),
    keywords_process = prepare_text_custom(vacancy_row[[keywords_col]], stopwords_es),
    description = prepare_text_custom(vacancy_row[[description_col]], stopwords_es)
  )[, `:=`(
    title_kw = paste(title, keywords_process),
    title_kw_des = paste(title, keywords_process, description)
  )]
}

### PREDICCION ----
predecir_una_vacante <- function(vacancy_row) {
  vacante_limpia <- clean_vacancy(vacancy_row)

  single_two_steps(
    corpus_one = vacante_limpia,
    vocabulary_domain = vocabulary_domain,
    table_tfidf_broad = level1_tfidf,
    table_tfidf_granular = granular_tfidf,
    id_col = "jobID_file",
    text_col1 = broad_text_col,
    text_col2 = granular_text_col,
    num_leaves_final = num_leaves_final,
    print_match = FALSE
  )
}

vacantes_lista <- split(vacantes, seq_len(nrow(vacantes)))

tic("Prediccion CUOC")

if (use_parallel) {
  plan(multisession, workers = n_workers)
  predicciones <- future_lapply(
    vacantes_lista,
    predecir_una_vacante,
    future.globals = c(
      "predecir_una_vacante",
      "clean_vacancy",
      "vocabulary_domain",
      "level1_tfidf",
      "granular_tfidf",
      "broad_text_col",
      "granular_text_col",
      "num_leaves_final",
      "prepare_text_custom",
      "stopwords_es",
      "single_two_steps",
      "get_level1_exact",
      "classify_occ_tokens",
      "corpus_to_dt",
      "remove_stopwords_accents",
      "my_remove_accents",
      "cleansing_corpus_lab"
    ),
    future.packages = c("data.table", "stringr", "tm", "stringdist")
  )
  plan(sequential)
} else {
  predicciones <- lapply(vacantes_lista, predecir_una_vacante)
}

predicciones_dt <- rbindlist(predicciones, fill = TRUE)

# Enriquecemos la salida con la tabla maestra CUOC para no depender de un merge posterior.
# Esto agrega nombre oficial, descripción y grupo asociado del código predicho.
predicciones_dt[, CuocCode := as.character(CuocCode)]
cuoc_ref[, CuocCode := as.character(CuocCode)]
predicciones_dt <- merge(
  predicciones_dt,
  cuoc_ref,
  by = "CuocCode",
  all.x = TRUE
)

predicciones_dt[, Primario := as.character(Primario)]
prim_ref[, Primario := as.character(Primario)]
predicciones_dt <- merge(
  predicciones_dt,
  prim_ref,
  by = "Primario",
  all.x = TRUE
)

fwrite(predicciones_dt, output_csv)

toc()

cat("Archivo de entrada:", input_csv, "\n")
cat("Archivo de salida:", output_csv, "\n")
cat("Vacantes procesadas:", nrow(vacantes), "\n")
cat("Columnas usadas: ", id_col, ", ", title_col, ", ", keywords_col, ", ", description_col, "\n", sep = "")
