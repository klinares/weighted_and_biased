# build_report_store.R -- build the ragnar DuckDB store over (a) PDF survey
# reports and (b) the generated data dictionaries. Follows the same v2 origin
# pattern proven in build_store.R: MarkdownDocument(text, origin=) -> chunk ->
# insert, with a one-file fail-fast origin check before the full build.
#
# Usage: source("build_report_store.R")   (rebuilds from scratch)

suppressPackageStartupMessages({
  library(ragnar)
  library(tidyverse)
  library(fs)
  library(DBI)
})
source("D:/RAG_reports/config.R")

CHUNK_SIZE    <- 1200
CHUNK_OVERLAP <- 0.20

# --- Collect inputs -----------------------------------------------------------
pdfs  <- dir_ls(PDF_DIR,  glob = "*.pdf",  recurse = TRUE)
dicts <- dir_ls(DICT_DIR, glob = "*_dictionary.md")
files <- c(pdfs, dicts)
stopifnot("No input files found" = length(files) > 0)
message(length(pdfs), " PDF reports, ", length(dicts), " dictionaries.")

# --- Per-file markdown + header ------------------------------------------------
prepare_chunks <- function(path) {
  is_dict <- grepl("_dictionary\\.md$", path)
  md <- if (is_dict) {
    read_file(path)                       # already markdown, header included
  } else {
    md <- tryCatch(read_as_markdown(path), error = \(e) NULL)
    if (is.null(md)) return(NULL)
    # Header so chunks self-identify; mirrors the [course: ...] tag pattern.
    paste0(sprintf("[report: %s]\n\n", path_file(path)), md)
  }
  if (is.null(md) || nchar(str_trim(md)) < 40) return(NULL)

  doc <- MarkdownDocument(md, origin = path_rel(path, ROOT))
  chunks <- tryCatch(
    markdown_chunk(doc, target_size = CHUNK_SIZE, target_overlap = CHUNK_OVERLAP),
    error = \(e) NULL
  )
  if (is.null(chunks) || nrow(chunks) == 0) NULL else chunks
}

# --- Create store ---------------------------------------------------------------
if (file.exists(STORE_PATH)) file.remove(STORE_PATH)
store <- ragnar_store_create(location = STORE_PATH, embed = EMBED_FUN,
                             overwrite = TRUE)

# --- Fail-fast origin check ------------------------------------------------------
test <- NULL
for (p in files) { test <- prepare_chunks(p); if (!is.null(test)) break }
stopifnot("No file produced chunks" = !is.null(test))
ragnar_store_insert(store, test)
oc <- dbGetQuery(store@con,
                 "SELECT origin FROM documents WHERE origin IS NOT NULL LIMIT 1")
stopifnot("FAIL-FAST: origin did not populate" = nrow(oc) > 0)
message("Origin verified: ", oc$origin[1])

# --- Full ingest -------------------------------------------------------------------
skipped <- character(0)
done    <- 1L
walk(files[-1], \(p) {
  message(sprintf("[%d/%d] %s", done + 1L, length(files), path_file(p)))
  tryCatch({
    ch <- prepare_chunks(p)
    if (is.null(ch)) { skipped <<- c(skipped, p); return(invisible()) }
    ragnar_store_insert(store, ch)
    done <<- done + 1L
  }, error = \(e) skipped <<- c(skipped, paste0(p, " :: ", conditionMessage(e))))
})
message(done, " ingested, ", length(skipped), " skipped.")
if (length(skipped)) write_lines(skipped, file.path(ROOT, "skipped.txt"))

# --- Index + smoke test ---------------------------------------------------------------
dbExecute(store@con, "SET preserve_insertion_order = false")
ragnar_store_build_index(store)

walk(c("favorability toward foreign leaders",
       "data dictionary weight strata psu"),
     \(q) {
       hits <- tryCatch(ragnar_retrieve(store, q, top_k = 4), error = \(e) NULL)
       cat("\n--", q, "\n")
       if (!is.null(hits) && nrow(hits) > 0)
         walk(hits$origin, \(o) cat("  ", o %||% "(unknown)", "\n"))
     })

message("\nStore built at ", STORE_PATH)
