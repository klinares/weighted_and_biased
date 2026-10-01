# =============================================================================
# R/prep_data.R
# Builds the analysis file. THIS IS THE ONLY FILE YOU SHOULD NEED TO EDIT to
# plug in real data: file paths (section 1), the columns that identify a
# commune (section 2), and the column mapping (sections 3 and 4).
#
# Sourced by diagnostics.R, which is sourced by analysis.qmd.
#
# Output objects:
#   survey       survey file with standard column names
#   violence     violence file with standard column names (commune x wave)
#   dat          all respondents with exposures attached
#   ad           analysis sample (complete cases) with design columns
# =============================================================================

pacman::p_load(glue, tidyverse)
source("R/boundaries.R")   # clean_name(), region_key(), region_alias, map files

# ---- 1. File paths -----------------------------------------------------------
# For .csv files use readr::read_csv("path") instead of readRDS("path").
survey_file   <- "data/survey_sim.rds"
violence_file <- "data/violence_commune_wave.rds"

# ---- 2. Columns that identify a commune --------------------------------------
# Communes are matched by NAME. Names repeat across Mali (and 4 repeat within a
# region), so the name is combined with region. If diagnostics.R reports
# ambiguous names and both files have a cercle column, use
# c("region", "cercle", "commune").
key_cols <- c("region", "commune")

# ---- 3. Survey: right-hand side = your column names --------------------------
survey <- readRDS(survey_file) |>
  dplyr::transmute(
    wave      = factor(wave),
    strata    = strata,              # design stratum for that wave
    psu       = psu_id,              # PSU code for that wave
    wt        = wt,                  # final weight for that wave
    region    = region,
    cercle    = cercle_name,         # only needed if "cercle" is in key_cols
    commune   = commune_name,        # commune NAME
    fav       = fav,                 # 1 = favorable view of the leader, 0 = not
    female    = female,
    age       = age,
    ethnicity = ethnicity,
    education = education,
    urban     = urban,
    local_gov = local_gov_approve,
    democracy = democracy_support,
    perceived = perceived_violence,  # possible mediator
    stress    = stress               # possible mediator
  )

# ---- 4. Violence: one row per commune and wave (or per event) ----------------
violence <- readRDS(violence_file) |>
  dplyr::transmute(
    wave    = factor(wave),
    region  = region,
    cercle  = cercle_name,
    commune = commune_name,
    events  = events                 # ACLED event count (use 1 if one row per event)
  )

# ---- 5. Commune match key ------------------------------------------------------
# "Ségou" and "SEGOU" both become "segou"; region spellings pass through
# region_alias (R/boundaries.R) first. Result looks like "segou|markala".
make_key <- function(d) {
  parts = purrr::map(key_cols, \(col) {
    if (col == "region") region_key(d[[col]]) else clean_name(d[[col]])
  })
  purrr::reduce(parts, paste, sep = "|")
}

survey   <- survey   |> dplyr::mutate(commune_key = make_key(survey),
                                      region_key  = region_key(region))
violence <- violence |> dplyr::mutate(commune_key = make_key(violence),
                                      region_key  = region_key(region))

# ---- 6. Exposures ---------------------------------------------------------------
# Commune-wave events. A sampled commune with no row in the violence file gets
# 0 events. That is only correct if names match; diagnostics.R checks this.
events_cw <- violence |>
  dplyr::summarise(events = sum(events), .by = c(commune_key, wave))

# Region-wave events (Model 1): total over ALL communes in the violence file
events_rw <- violence |>
  dplyr::summarise(events_region = sum(events), .by = c(region_key, wave))

dat <- survey |>
  dplyr::left_join(events_cw, by = c("commune_key", "wave")) |>
  dplyr::left_join(events_rw, by = c("region_key", "wave")) |>
  dplyr::mutate(
    events        = tidyr::replace_na(events, 0),
    events_region = tidyr::replace_na(events_region, 0),
    v_region      = log1p(events_region),   # Model 1 exposure
    v_commune     = log1p(events),          # Models 2 to 4 exposure
    any_event     = as.integer(events > 0)  # binary sensitivity
  )

# Within/between split (Mundlak). The commune mean is taken over the waves in
# which the commune was SURVEYED, each wave counted once. A commune surveyed in
# one wave has within = 0 and adds nothing to the within estimate.
commune_means <- dat |>
  dplyr::distinct(commune_key, wave, v_commune, any_event) |>
  dplyr::summarise(v_between   = mean(v_commune),
                   any_between = mean(any_event),
                   n_waves     = dplyr::n(),
                   .by = commune_key)

dat <- dat |>
  dplyr::left_join(commune_means, by = "commune_key") |>
  dplyr::mutate(
    v_within   = v_commune - v_between,
    any_within = any_event - any_between,
    region     = forcats::fct_relevel(factor(region), "Bamako"),  # reference region
    ethnicity  = forcats::fct_infreq(factor(ethnicity)),
    education  = factor(education),
    age10      = (age - mean(age, na.rm = TRUE)) / 10,
    democracy_c = democracy - mean(democracy, na.rm = TRUE),
    perceived_c = perceived - mean(perceived, na.rm = TRUE),
    stress_c    = stress - mean(stress, na.rm = TRUE)
  )

# ---- 7. Analysis sample and design columns ------------------------------------
# Each wave is its own design: strata, PSU, and commune codes are pasted with
# the wave so they never repeat across waves. Weights are rescaled to sum to
# each wave's sample size so no wave dominates the pooled models.
ad <- dat |>
  tidyr::drop_na(fav, wt, female, age10, ethnicity, education, urban,
                 local_gov, democracy_c, perceived_c, stress_c) |>
  dplyr::mutate(
    strata_w  = paste(wave, strata),
    psu_w     = paste(wave, psu),
    commune_w = paste(wave, commune_key),
    wt_scaled = wt * dplyr::n() / sum(wt),
    .by = wave
  )
