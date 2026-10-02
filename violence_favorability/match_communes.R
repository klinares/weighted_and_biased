# =============================================================================
# match_communes.R
# Links survey commune names to the official commune list, for the MAPS.
# (The models do not need this; they group respondents by commune name.)
#
# Input:   matching/communes_from_survey.csv   columns: region, commune
#          (export of distinct region + commune from your merge script)
# Output:  matching/crosswalk_draft.csv        proposals for a person to review
#
# Workflow:
#   1. source("match_communes.R")
#   2. Open matching/crosswalk_draft.csv. Rows with status "exact" are done.
#      For every other row, copy the right code from `candidates` into
#      adm3_code (or leave it blank if the commune cannot be placed).
#   3. Save the reviewed file as matching/crosswalk.csv. maps.R uses ONLY that
#      file. This script never overwrites it.
#
# Matching is by name within region:
#   exact     cleaned names identical, and the name is unique in that region
#   ambiguous name exists more than once in that region (pick using cercle)
#   review    no exact match; the 3 closest names in the region are suggested
# =============================================================================

pacman::p_load(glue, tidyverse)
source("R/boundaries.R")

if (!file.exists("matching/communes_from_survey.csv")) {
  stop("Put the distinct region + commune list in matching/communes_from_survey.csv first.")
}
survey_communes <- readr::read_csv("matching/communes_from_survey.csv", show_col_types = FALSE) |>
  dplyr::distinct(region, commune) |>
  dplyr::mutate(adm1_key = region_key(region), adm3_key = clean_name(commune))

dictionary <- read_mali_dictionary()

# ---- 1. Exact matches (region + cleaned name) ----------------------------------------
exact <- survey_communes |>
  dplyr::left_join(dictionary, by = c("adm1_key", "adm3_key"), relationship = "many-to-many") |>
  dplyr::mutate(n_found = sum(!is.na(adm3_code)), .by = c(region, commune))

# ---- 2. Candidates: closest official names in the same region ------------------------
# Distance = edit distance (base R adist) divided by the longer name's length,
# so 0 = identical and 0.2 = about one letter in five differs.
suggest <- function(region_k, name_k) {
  pool = dictionary |> dplyr::filter(adm1_key == region_k)
  d = as.vector(utils::adist(name_k, pool$adm3_key)) / pmax(nchar(name_k), nchar(pool$adm3_key))
  pool |>
    dplyr::mutate(distance = d) |>
    dplyr::slice_min(distance, n = 3, with_ties = FALSE) |>
    glue::glue_data("{adm3_name} ({adm2_name}) [{adm3_code}] d={sprintf('%.2f', distance)}") |>
    paste(collapse = "; ")
}

draft <- exact |>
  dplyr::mutate(status = dplyr::case_when(n_found == 1 ~ "exact",
                                          n_found >  1 ~ "ambiguous",
                                          TRUE         ~ "review")) |>
  # ambiguous rows: list every same-named commune and leave the code blank
  dplyr::mutate(candidates = dplyr::if_else(
    status == "ambiguous",
    paste(glue::glue("{adm3_name} ({adm2_name}) [{adm3_code}]"), collapse = "; "),
    NA_character_), .by = c(region, commune)) |>
  dplyr::distinct(region, commune, .keep_all = TRUE) |>
  dplyr::mutate(
    adm3_code  = dplyr::if_else(status == "exact", adm3_code, NA_character_),
    adm3_name  = dplyr::if_else(status == "exact", adm3_name, NA_character_),
    adm2_name  = dplyr::if_else(status == "exact", adm2_name, NA_character_),
    candidates = dplyr::if_else(status == "review",
                                purrr::map2_chr(adm1_key, adm3_key, suggest),
                                candidates)
  ) |>
  dplyr::select(region, commune, status, adm3_code, adm3_name, adm2_name, candidates) |>
  dplyr::arrange(dplyr::desc(status != "exact"), region, commune)

readr::write_csv(draft, "matching/crosswalk_draft.csv", na = "")

# ---- 3. Report --------------------------------------------------------------------------
print(dplyr::count(draft, status))
print(glue("Draft written to matching/crosswalk_draft.csv. ",
           "Review the non-exact rows and save as matching/crosswalk.csv."))

if (file.exists("matching/crosswalk.csv")) {
  reviewed <- readr::read_csv("matching/crosswalk.csv", show_col_types = FALSE)
  new_names <- dplyr::anti_join(draft, reviewed, by = c("region", "commune"))
  print(glue("matching/crosswalk.csv exists and was NOT changed. ",
             "Survey communes not yet in it: {nrow(new_names)}"))
}
