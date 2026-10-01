# =============================================================================
# diagnostics.R
# Run this FIRST on new data, before fitting anything:  source("diagnostics.R")
#
# Answers two questions:
#   1. Did the commune names match?           (name_checks)
#   2. Is there enough variation in violence   (diag_table: K, A, P, W)
#      to estimate its effect?
#
# Sourced by analysis.qmd, so the same numbers appear in the report.
# =============================================================================

source("R/prep_data.R")

# ---- Thresholds (edit if you disagree) ---------------------------------------
min_cells  <- 100   # K: fewer commune-wave cells than this = little gain over region-wave
max_absorb <- 0.90  # A: dummies absorbing more than this = violence ~ collinear with them
min_within <- 0.10  # W: within-commune share below this = within estimate uninformative

# ---- 1. Name checks ------------------------------------------------------------
# Official commune list (R/boundaries.R): used to tell a spelling problem from a
# commune that truly had no events.
dictionary <- read_mali_dictionary() |>
  dplyr::transmute(region = adm1_name, cercle = adm2_name, commune = adm3_name)
dictionary <- dictionary |> dplyr::mutate(commune_key = make_key(dictionary))

ambiguous_keys <- dictionary |>
  dplyr::count(commune_key) |>
  dplyr::filter(n > 1) |>
  dplyr::pull(commune_key)

survey_keys   <- unique(survey$commune_key)
violence_keys <- unique(violence$commune_key)

name_checks <- tibble::tibble(
  Check = c(
    "Survey communes",
    "Survey communes not in official list (likely spelling)",
    "Violence communes not in official list (likely spelling)",
    "Survey communes with no violence record (set to 0 events)",
    "Survey communes whose name is ambiguous within the key"
  ),
  n = c(
    length(survey_keys),
    length(setdiff(survey_keys, dictionary$commune_key)),
    length(setdiff(violence_keys, dictionary$commune_key)),
    length(setdiff(survey_keys, violence_keys)),
    length(intersect(survey_keys, ambiguous_keys))
  ),
  Examples = c(
    "",
    toString(head(setdiff(survey_keys, dictionary$commune_key), 4)),
    toString(head(setdiff(violence_keys, dictionary$commune_key), 4)),
    toString(head(setdiff(survey_keys, violence_keys), 4)),
    toString(head(intersect(survey_keys, ambiguous_keys), 4))
  )
)

# ---- 2. K, A, P, W --------------------------------------------------------------
# One row per commune-wave cell in the analysis sample
cells <- ad |>
  dplyr::distinct(commune_key, wave, region, v_commune, v_region, v_within, n_waves)

# K: how many exposure units the commune-level models have
K_cells     <- nrow(cells)
K_any_event <- mean(cells$v_commune > 0)

# A: share of exposure variance explained by region + wave dummies
A_commune <- summary(lm(v_commune ~ region + wave, data = cells))$r.squared
A_region  <- summary(lm(v_region  ~ region + wave,
                        data = dplyr::distinct(cells, region, wave, v_region)))$r.squared

# P: PSUs per commune-wave cell (above 1 = commune clustering matters)
psu_per_cell <- ad |>
  dplyr::summarise(psus = dplyr::n_distinct(psu_w), .by = commune_w) |>
  dplyr::pull(psus)
P_median <- stats::median(psu_per_cell)
P_share  <- mean(psu_per_cell > 1)

# W: within-commune share of exposure variance
W_share      <- stats::var(cells$v_within) / stats::var(cells$v_commune)
n_repeat     <- dplyr::n_distinct(cells$commune_key[cells$n_waves > 1])
pct_repeat_r <- mean(ad$n_waves > 1)

diag_table <- tibble::tibble(
  Diagnostic = c("K", "A, region", "A, commune",
                 "P", "W"),
  Meaning = c(
    "Commune-wave cells in the analysis sample (share with at least one event)",
    "Share of region-wave violence explained by region + wave dummies (Model 1)",
    "Share of commune-wave violence explained by region + wave dummies (Models 2 to 4)",
    "Median PSUs per commune-wave cell (share of cells with more than one PSU)",
    "Within-commune share of violence variance (communes surveyed 2+ waves; share of respondents)"
  ),
  Value = c(
    sprintf("%d (%.0f%%)", K_cells, 100 * K_any_event),
    sprintf("%.0f%%", 100 * A_region),
    sprintf("%.0f%%", 100 * A_commune),
    sprintf("%.0f (%.0f%%)", P_median, 100 * P_share),
    sprintf("%.0f%% (%d communes; %.0f%%)", 100 * W_share, n_repeat, 100 * pct_repeat_r)
  ),
  Status = c(
    dplyr::if_else(K_cells >= min_cells, "OK", "Low"),
    dplyr::if_else(A_region > max_absorb, "Problem", "OK"),
    dplyr::if_else(A_commune > max_absorb, "Problem", "OK"),
    dplyr::if_else(P_median > 1, "Cluster at commune", "Commune ~ PSU"),
    dplyr::if_else(W_share >= min_within, "OK", "Weak")
  )
)

# ---- 3. Print ---------------------------------------------------------------------
print(name_checks, width = Inf)
print(diag_table, width = Inf)
