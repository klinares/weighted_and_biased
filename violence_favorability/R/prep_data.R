# =============================================================================
# R/prep_data.R
# Builds the analysis file. EDIT THIS FILE to plug in real data:
#   section 1  file path
#   section 2  column names (right-hand side = your names)
#
# Creates:
#   survey   the survey with standard column names
#   ad       analysis sample (complete cases) with violence and design columns
# =============================================================================

source("R/boundaries.R")

# ---- 1. File path and reference groups --------------------------------------------
# For a .csv use readr::read_csv("path") instead of readRDS("path").
survey_file <- "data/survey_sim.rds"
ref_region  <- "Bamako"   # reference region for the dummies (wave 1 is the reference wave)

# ---- 2. Columns: right-hand side = your column names ------------------------------
survey <- readRDS(survey_file) |>
  dplyr::transmute(
    wave      = factor(wave),   # 1, 2, 3; wave 1 is the reference
    region    = region,         # region name (model dummies)
    commune   = commune,        # commune name (model random effect)
    village   = village,        # village name or code (second-stage design unit)
    adm1_key  = adm1_key,       # region key (design stratum, map join)
    adm3_key  = adm3_key,       # commune key (first-stage design unit, map join)
    weight    = weight,         # final weight for that wave
    fav       = fav,            # 1 = favorable view of the leader, 0 = not
    events    = events,         # ACLED violence against civilians, commune-wave, survey window
    female    = female,
    age       = age,
    ethnicity = ethnicity,
    education = education,
    urban     = urban
  )

# ---- 3. Keys and commune ID ----------------------------------------------------------
# Keys are cleaned the same way as the boundary file (accents, case, spaces and
# punctuation removed; Menaka filed under Gao), so the map join is exact. If the
# keys are already clean this changes nothing.
# Commune names repeat across regions, so the model's commune ID is region + commune.
survey <- survey |>
  dplyr::mutate(
    adm1_key   = region_key(adm1_key),
    adm3_key   = clean_name(adm3_key),
    commune_id = paste(region, commune, sep = " | ")
  )

# Same commune must not appear under two spellings: one commune would become two
# random effects. Both counts should match.
n_by_name <- dplyr::n_distinct(survey$commune_id)
n_by_key  <- dplyr::n_distinct(paste(survey$adm1_key, survey$adm3_key))
if (n_by_name != n_by_key) {
  warning("Communes by name: ", n_by_name, "; by key: ", n_by_key,
          ". A commune may be spelled two ways across waves; check before modeling.")
}

# ---- 4. Violence variables ----------------------------------------------------------
# v_commune = log(1 + events). 0 events -> 0; each +0.69 doubles (1 + events).
# Mundlak split (sensitivity model): a commune's usual level, averaged over the
# waves in which it was surveyed (each wave counted once), and the deviation
# from that level in a given wave. Surveyed once -> deviation = 0.
survey <- survey |>
  dplyr::mutate(v_commune = log1p(events))

commune_means <- survey |>
  dplyr::distinct(commune_id, wave, v_commune) |>
  dplyr::summarise(v_between = mean(v_commune), n_waves = dplyr::n(), .by = commune_id)

# ---- 5. Analysis sample and design columns -----------------------------------------
# Complete cases, so every model uses the same respondents (needed for the LRT).
# Each wave is its own design, so stratum, commune, and village codes are pasted
# with the wave. Weights are rescaled to sum to each wave's sample size so no
# wave dominates the pooled design-based model.
ad <- survey |>
  dplyr::left_join(commune_means, by = "commune_id") |>
  tidyr::drop_na(fav, events, weight, female, age, ethnicity, education, urban) |>
  dplyr::mutate(
    v_within  = v_commune - v_between,
    region    = forcats::fct_relevel(factor(region), ref_region),
    ethnicity = forcats::fct_infreq(factor(ethnicity)),        # most common = reference
    education = factor(education),
    age10     = (age - mean(age)) / 10                          # per decade, centered
  ) |>
  dplyr::mutate(
    strata_w  = paste(wave, adm1_key),
    psu_w     = paste(wave, adm1_key, adm3_key),
    village_w = paste(wave, adm1_key, adm3_key, village),
    wt_scaled = weight * dplyr::n() / sum(weight),
    .by = wave
  ) |>
  dplyr::mutate(
    region_wave = paste(region, wave, sep = " | "),        # region-by-wave random intercept (M3b)
    log_wt_c    = base::log(wt_scaled) - mean(base::log(wt_scaled))     # weight as a covariate (M4)
  )
