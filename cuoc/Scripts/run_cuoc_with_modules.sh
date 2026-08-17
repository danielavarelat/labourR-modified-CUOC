#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Uso:
  bash run_cuoc_with_modules.sh <project_root> <cuoc_root> <input_file> <output_dir> [id_col] [title_col] [keywords_col] [description_col] [broad_text_col] [granular_text_col] [num_leaves_final] [parallel] [workers]

Ejemplo:
  nohup bash cuoc/Scripts/run_cuoc_with_modules.sh \
    /home/andres.garcia/data/vacantes \
    /home/andres.garcia/data/vacantes/CompuClean \
    /home/andres.garcia/data/vacantes/CompuClean/input/vacantes.parquet \
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
INPUT_FILE="$3"
OUTPUT_DIR="$4"
ID_COL="${5:-ID_file}"
TITLE_COL="${6:-title}"
KEYWORDS_COL="${7:-keywords}"
DESCRIPTION_COL="${8:-full_descr}"
BROAD_TEXT_COL="${9:-title_kw}"
GRANULAR_TEXT_COL="${10:-title_kw_des}"
NUM_LEAVES_FINAL="${11:-3}"
PARALLEL_FLAG="${12:-FALSE}"
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

INPUT_FOR_R="$INPUT_FILE"
TMP_CSV=""

cleanup() {
  if [[ -n "${TMP_CSV}" && -f "${TMP_CSV}" ]]; then
    rm -f "$TMP_CSV"
  fi
}

trap cleanup EXIT

input_ext="${INPUT_FILE##*.}"
if [[ "${input_ext,,}" == "parquet" ]]; then
  if ! command -v python3 >/dev/null 2>&1; then
    echo "Necesito python3 para convertir parquet a CSV temporal." >&2
    exit 1
  fi
  TMP_CSV="$(mktemp "${TMPDIR:-/tmp}/cuoc_input_XXXXXX.csv")"
  python3 - "$INPUT_FILE" "$TMP_CSV" <<'PY'
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
  INPUT_FOR_R="$TMP_CSV"
fi

OUTPUT_BASENAME="$(basename "${INPUT_FILE%.*}")"
OUTPUT_FILE="${OUTPUT_DIR}/${OUTPUT_BASENAME}_predicciones.csv"

Rscript "$R_SCRIPT" \
  --input="$INPUT_FOR_R" \
  --output="$OUTPUT_FILE" \
  --id_col="$ID_COL" \
  --title_col="$TITLE_COL" \
  --keywords_col="$KEYWORDS_COL" \
  --description_col="$DESCRIPTION_COL" \
  --broad_text_col="$BROAD_TEXT_COL" \
  --granular_text_col="$GRANULAR_TEXT_COL" \
  --num_leaves_final="$NUM_LEAVES_FINAL" \
  --parallel="$PARALLEL_FLAG" \
  --workers="$WORKERS"
