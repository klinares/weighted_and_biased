# =============================================================================
# testing/simulate_review.R
# TEST ONLY. Stands in for the human review step: fills every non-exact row of
# the draft with the true code and saves matching/crosswalk.csv.
# With real data, a person does this review by hand. Never run this on real data.
# =============================================================================
pacman::p_load(tidyverse)
source("R/boundaries.R")

dictionary <- read_mali_dictionary()
truth <- readr::read_csv("testing/true_commune_codes.csv", show_col_types = FALSE)

readr::read_csv("matching/crosswalk_draft.csv", show_col_types = FALSE) |>
  dplyr::left_join(truth, by = c("region", "commune")) |>
  dplyr::mutate(status    = dplyr::if_else(status == "exact", "exact", "reviewed"),
                adm3_code = dplyr::coalesce(adm3_code, true_adm3_code)) |>
  dplyr::select(region, commune, status, adm3_code) |>
  # Two simulated communes can share a name (e.g. Kapala, Sikasso). A survey with
  # names only cannot separate them, so, like a reviewer, keep one row per name.
  dplyr::distinct(region, commune, .keep_all = TRUE) |>
  dplyr::left_join(dictionary |> dplyr::select(adm3_code, adm3_name, adm2_name), by = "adm3_code") |>
  readr::write_csv("matching/crosswalk.csv", na = "")
