# config.R -- shared configuration for the survey-reports RAG tool.
# Sourced by prepare_data.R, build_report_store.R, and app.R so every script
# agrees on paths, the embedding function, and per-dataset design variables.

suppressPackageStartupMessages({
  library(tibble)
  library(ragnar)
})

# --- Paths -------------------------------------------------------------------
ROOT        <- "D:/RAG_reports"
PDF_DIR     <- file.path(ROOT, "pdf")          # published reports
SAV_DIR     <- file.path(ROOT, "sav")          # raw SPSS files
RDS_DIR     <- file.path(ROOT, "rds")          # converted data
DICT_DIR    <- file.path(ROOT, "dictionaries") # generated data dictionaries (md)
STORE_PATH  <- file.path(ROOT, "report_store.duckdb")

# --- Generation model: OpenAI-compatible API ---------------------------------
# Fill in once you have the endpoint. Works with llama.cpp --server, vLLM,
# LM Studio, or Ollama's own OpenAI shim (http://localhost:11434/v1).
BASE_URL  <- "http://localhost:8080/v1"
API_KEY   <- "not-needed"        # local servers usually ignore this
GEN_MODEL <- "CHANGE-ME"         # model name as the server reports it

# --- Embeddings ---------------------------------------------------------------
# MUST be identical at store-build time and query time.
# Option A (known-good, keeps ollama only for embeddings):
EMBED_FUN <- \(x) embed_ollama(x, model = "nomic-embed-text")
# Option B (fully on the API server, if it serves /v1/embeddings):
# EMBED_FUN <- \(x) embed_openai(x, base_url = BASE_URL, api_key = API_KEY,
#                                model = "nomic-embed-text")

# --- Retrieval ----------------------------------------------------------------
TOP_K      <- 6
COS_MARGIN <- 0.15   # drop hits > best_cosine + margin (tangential-chunk filter)

# --- Dataset registry ----------------------------------------------------------
# One row per dataset: where the .rds lives and which columns define the
# complex design. psu/strata may be NA for SRS-with-weights designs.
DATASETS <- tribble(
  ~id,                ~rds,                                     ~weight,  ~strata,   ~psu,
  "example_2024",     file.path(RDS_DIR, "example_2024.rds"),   "weight", "stratum", "psu"
  # add rows as you convert more .sav files
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
