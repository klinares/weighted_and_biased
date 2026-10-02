# =============================================================================
# diagnostics.R
# Run FIRST on new data, before any model:  source("diagnostics.R")
# Checks whether the data can support a commune-level violence effect.
# Sourced by models.R, so the same numbers appear in the report.
# =============================================================================

source("R/prep_data.R")

# ---- Thresholds (edit if you disagree) ----------------------------------------------
max_absorb <- 0.90   # A above this: violence is almost collinear with the dummies
min_within <- 0.10   # W below this: within-commune estimate will be uninformative

# One row per commune-wave in the analysis sample
cells <- ad |>
  dplyr::distinct(commune_id, wave, region, events, v_commune, v_region, v_within, n_waves)

# ---- 0. Data checks ---------------------------------------------------------------
# Every respondent in a commune-wave must have the same event count
events_conflict <- ad |>
  dplyr::summarise(values = dplyr::n_distinct(events), .by = c(commune_id, wave)) |>
  dplyr::filter(values > 1)

# ---- 1. K: exposure units -----------------------------------------------------------
K_cells    <- nrow(cells)
K_any      <- mean(cells$events > 0)

# ---- 2. A: share of violence variation absorbed by region and wave dummies --------
#  Whatever share the dummies explain cannot inform the violence coefficient.
A_region  <- summary(lm(v_region ~ region + wave,
                        data = dplyr::distinct(cells, region, wave, v_region)))$r.squared
A_commune <- summary(lm(v_commune ~ region + wave, data = cells))$r.squared

# ---- 3. P: villages per commune-wave -------------------------------------------------
villages_per_cell <- ad |>
  dplyr::summarise(villages = dplyr::n_distinct(village_w), .by = commune_w) |>
  dplyr::pull(villages)
P_median <- stats::median(villages_per_cell)

# ---- 4. W: within-commune share of violence variation --------------------------------
W_share  <- stats::var(cells$v_within) / stats::var(cells$v_commune)
n_repeat <- dplyr::n_distinct(cells$commune_id[cells$n_waves > 1])

diag_table <- tibble::tibble(
  Diagnostic = c("K", "A, region", "A, commune", "P", "W"),
  Meaning = c(
    "Commune-wave cells (share with at least one event)",
    "Region-wave violence explained by region + wave dummies (Model 1)",
    "Commune-wave violence explained by region + wave dummies (Models 2 to 4)",
    "Median villages per commune-wave",
    "Within-commune share of violence variation (communes surveyed in 2+ waves)"
  ),
  Value = c(
    sprintf("%d (%.0f%%)", K_cells, 100 * K_any),
    sprintf("%.0f%%", 100 * A_region),
    sprintf("%.0f%%", 100 * A_commune),
    sprintf("%.0f", P_median),
    sprintf("%.0f%% (%d communes)", 100 * W_share, n_repeat)
  ),
  Status = c(
    "",
    dplyr::if_else(A_region  > max_absorb, "Problem", "OK"),
    dplyr::if_else(A_commune > max_absorb, "Problem", "OK"),
    "",
    dplyr::if_else(W_share  >= min_within, "OK", "Weak: within estimate is appendix only")
  )
)

print(glue("Commune-waves with conflicting event counts: {nrow(events_conflict)} (should be 0)"))
print(diag_table, width = Inf)
