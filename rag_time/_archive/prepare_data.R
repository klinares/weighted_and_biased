# prepare_data.R -- convert SPSS .sav files to .rds and generate a data
# dictionary per dataset. The dictionary is written as markdown into DICT_DIR
# so build_report_store.R indexes it alongside the PDF reports -- this is what
# lets a chat query like "favor towards Putin" surface the variable name.
#
# Usage: drop .sav files in SAV_DIR, then  source("prepare_data.R")
# Labelled vectors are kept as-is in the .rds; conversion to factor happens at
# plot time (haven::as_factor) so weights stay numeric and nothing is lost.

suppressPackageStartupMessages({
  library(haven)
  library(labelled)
  library(tidyverse)
  library(fs)
})
source("D:/RAG_reports/config.R")

dir_create(c(RDS_DIR, DICT_DIR))

# Collapse a value-labels vector to "1 = Very favorable; 2 = Somewhat ..."
fmt_values <- function(x) {
  vl <- val_labels(x)
  if (is.null(vl)) return("")
  paste(sprintf("%s = %s", unname(vl), names(vl)), collapse = "; ")
}

dictionary_md <- function(df, id) {
  rows <- imap_chr(df, \(col, nm) {
    lab  <- var_label(col) %||% ""
    vals <- fmt_values(col)
    sprintf("- **%s**: %s%s", nm, lab,
            if (nzchar(vals)) paste0(" [", vals, "]") else "")
  })
  paste0(
    sprintf("[dataset dictionary: %s]\n\n", id),
    sprintf("# Data dictionary: %s\n\n", id),
    sprintf("%d variables, %d cases.\n\n", ncol(df), nrow(df)),
    paste(rows, collapse = "\n"), "\n"
  )
}

convert_one <- function(sav_path) {
  id  <- path_ext_remove(path_file(sav_path))
  df  <- read_sav(sav_path)
  out <- file.path(RDS_DIR, paste0(id, ".rds"))
  write_rds(df, out, compress = "gz")
  write_lines(dictionary_md(df, id),
              file.path(DICT_DIR, paste0(id, "_dictionary.md")))
  message(sprintf("%s -> %s (%d vars, %d cases) + dictionary",
                  path_file(sav_path), path_file(out), ncol(df), nrow(df)))
  invisible(id)
}

sav_files <- dir_ls(SAV_DIR, glob = "*.sav")
if (length(sav_files) == 0) message("No .sav files in ", SAV_DIR)
walk(sav_files, convert_one)

message("\nDone. Remember to add new datasets (weight/strata/psu columns) ",
        "to DATASETS in config.R, then rebuild the store.")
