#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Uso:
  bash run_cuoc_with_modules.sh <project_root> <cuoc_root> <input_dir|input_file> <output_dir> [id_col] [title_col] [keywords_col] [description_col] [broad_text_col] [granular_text_col] [num_leaves_final] [use_parallel] [workers]

Ejemplo:
  nohup bash cuoc/Scripts/run_cuoc_with_modules.sh \
    /home/andres.garcia/data/vacantes \
    /home/andres.garcia/data/vacantes/CompuClean \
    /home/andres.garcia/data/vacantes/CompuClean/input \
    /home/andres.garcia/data/vacantes/CompuClean/Results/exp_cuoc \
    > /home/andres.garcia/data/vacantes/CompuClean/Results/exp_cuoc.log 2>&1 &
EOF
}

if [[ $# -lt 4 ]]; then
  usage
  exit 1
fi

PROJECT_ROOT="$1"
CUOC_ROOT="$2"
INPUT_PATH="$3"
OUTPUT_DIR="$4"
ID_COL="${5:-ID_file}"
TITLE_COL="${6:-title}"
KEYWORDS_COL="${7:-keywords}"
DESCRIPTION_COL="${8:-full_descr}"
BROAD_TEXT_COL="${9:-title_kw}"
GRANULAR_TEXT_COL="${10:-title_kw_des}"
NUM_LEAVES_FINAL="${11:-3}"
USE_PARALLEL_FLAG="${12:-FALSE}"
WORKERS="${13:-2}"
R_SCRIPT="${CUOC_ROOT}/cuoc/Scripts/Run_occ_predict.R"

if [[ ! -d "$PROJECT_ROOT" ]]; then
  echo "No existe project_root: $PROJECT_ROOT" >&2
  exit 1
fi

if [[ ! -d "$CUOC_ROOT" ]]; then
  echo "No existe cuoc_root: $CUOC_ROOT" >&2
  exit 1
fi

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "No encuentro el runner de CUOC en: $R_SCRIPT" >&2
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

TMP_FILES=()

cleanup() {
  for tmp_file in "${TMP_FILES[@]:-}"; do
    if [[ -n "${tmp_file}" && -f "${tmp_file}" ]]; then
      rm -f "$tmp_file"
    fi
  done
}

trap cleanup EXIT

ensure_python3() {
  if ! command -v python3 >/dev/null 2>&1; then
    echo "Necesito python3 para convertir parquet a CSV temporal." >&2
    exit 1
  fi
}

convert_parquet_to_csv() {
  local input_file="$1"
  local output_file="$2"

  ensure_python3
  python3 - "$input_file" "$output_file" <<'PY'
import sys

try:
    import pandas as pd
except Exception as exc:
    raise SystemExit(f"No se pudo importar pandas para leer parquet: {exc}")

input_path = sys.argv[1]
output_path = sys.argv[2]

df = pd.read_parquet(input_path)
df.to_csv(output_path, index=False)
PY
}

process_one_file() {
  local input_file="$1"
  local input_for_r="$input_file"
  local tmp_csv=""
  local input_ext="${input_file##*.}"
  local output_basename
  local output_file

  if [[ "${input_ext,,}" == "parquet" ]]; then
    tmp_csv="$(mktemp "${TMPDIR:-/tmp}/cuoc_input_XXXXXX.csv")"
    TMP_FILES+=("$tmp_csv")
    convert_parquet_to_csv "$input_file" "$tmp_csv"
    input_for_r="$tmp_csv"
  elif [[ "${input_ext,,}" != "csv" ]]; then
    echo "Archivo omitido por extensión no soportada: $input_file" >&2
    return 0
  fi

  output_basename="$(basename "${input_file%.*}")"
  output_file="${OUTPUT_DIR}/${output_basename}_predicciones.csv"

  Rscript "$R_SCRIPT" \
    --input="$input_for_r" \
    --output="$output_file" \
    --id_col="$ID_COL" \
    --title_col="$TITLE_COL" \
    --keywords_col="$KEYWORDS_COL" \
    --description_col="$DESCRIPTION_COL" \
    --broad_text_col="$BROAD_TEXT_COL" \
    --granular_text_col="$GRANULAR_TEXT_COL" \
    --num_leaves_final="$NUM_LEAVES_FINAL" \
    --use_parallel="$USE_PARALLEL_FLAG" \
    --workers="$WORKERS"
}

if [[ -d "$INPUT_PATH" ]]; then
  found_any=0
  while IFS= read -r -d "" input_file; do
    found_any=1
    process_one_file "$input_file"
  done < <(
    find "$INPUT_PATH" -maxdepth 1 -type f \
      \( -iname "*.parquet" -o -iname "*.csv" \) \
      -print0 | sort -z
  )

  if [[ "$found_any" -eq 0 ]]; then
    echo "No encontré archivos .parquet o .csv en: $INPUT_PATH" >&2
    exit 1
  fi
elif [[ -f "$INPUT_PATH" ]]; then
  process_one_file "$INPUT_PATH"
else
  echo "No existe input_dir/input_file: $INPUT_PATH" >&2
  exit 1
fi
