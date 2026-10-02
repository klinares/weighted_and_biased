# =============================================================================
# R/prep_data.R
# Builds the analysis file. EDIT THIS FILE to plug in real data:
#   section 1  file path
#   section 2  column names (right-hand side = your names)
#
# Sourced by diagnostics.R (which models.R and analysis.qmd source in turn).
#
# Creates:
#   survey   the survey with standard column names
#   dat      all respondents with violence variables
#   ad       analysis sample (complete cases) with survey design columns
# =============================================================================

pacman::p_load(glue, tidyverse)
source("R/boundaries.R")

# ---- 1. File path ----------------------------------------------------------------
# For a .csv use readr::read_csv("path") instead of readRDS("path").
survey_file <- "data/survey_sim.rds"

# ---- 2. Columns: right-hand side = your column names ------------------------------
survey <- readRDS(survey_file) |>
  dplyr::transmute(
    wave      = factor(wave),
    region    = region,     # design stratum
    commune   = commune,    # first-stage sampling unit (PSU); also where violence is measured
    village   = village,    # second-stage sampling unit (SSU)
    weight    = weight,     # final weight for that wave
    fav       = fav,        # 1 = favorable view of the leader, 0 = not
    events    = events,     # ACLED violence against civilians in the commune that wave
    female    = female,
    age       = age,
    ethnicity = ethnicity,
    education = education,
    urban     = urban,
    local_gov = local_gov,  # attitude (sensitivity model only)
    democracy = democracy,  # attitude (sensitivity model only)
    perceived = perceived,  # perceived violence (possible mediator)
    stress    = stress      # possible mediator
  )

# ---- 3. One stable ID per commune -------------------------------------------------
# If the reviewed crosswalk exists, use the official commune code, so that a
# commune spelled differently in two waves is still treated as one commune.
# Otherwise fall back to the cleaned region + commune name.
survey <- survey |>
  dplyr::mutate(commune_id = paste(region_key(region), clean_name(commune), sep = "|"))

if (file.exists("matching/crosswalk.csv")) {
  crosswalk <- readr::read_csv("matching/crosswalk.csv", show_col_types = FALSE) |>
    dplyr::select(region, commune, adm3_code)

  # Each survey name must point to ONE commune. Two communes can share a name
  # in the same region (for example Kapala in Sikasso); if both were surveyed,
  # the name alone cannot tell them apart, and joining would duplicate people.
  repeated <- crosswalk |> dplyr::count(region, commune) |> dplyr::filter(n > 1)
  if (nrow(repeated) > 0) {
    stop("matching/crosswalk.csv has more than one row for: ",
         toString(paste(repeated$region, repeated$commune)),
         ". Keep one row per region + commune (see MODELING_NOTES.md, name matching).")
  }

  survey <- survey |>
    dplyr::left_join(crosswalk, by = c("region", "commune"), relationship = "many-to-one") |>
    dplyr::mutate(commune_id = dplyr::coalesce(adm3_code, commune_id))
}

# ---- 4. Violence variables ----------------------------------------------------------
# v_commune: log(1 + events) for the respondent's commune and wave (Models 2 to 4).
# v_region:  log(1 + events summed over the SURVEYED communes in the region and
#            wave) (Model 1). Only surveyed communes are in the file, so this is
#            not a full regional total.
region_wave_events <- survey |>
  dplyr::distinct(region, wave, commune_id, events) |>
  dplyr::summarise(events_region = sum(events), .by = c(region, wave))

dat <- survey |>
  dplyr::left_join(region_wave_events, by = c("region", "wave")) |>
  dplyr::mutate(
    v_commune = log1p(events),
    v_region  = log1p(events_region)
  )

# Within/between split (appendix only). Commune mean over the waves in which it
# was surveyed, each wave counted once. Surveyed once -> within = 0.
commune_means <- dat |>
  dplyr::distinct(commune_id, wave, v_commune) |>
  dplyr::summarise(v_between = mean(v_commune), n_waves = dplyr::n(), .by = commune_id)

dat <- dat |>
  dplyr::left_join(commune_means, by = "commune_id") |>
  dplyr::mutate(
    v_within    = v_commune - v_between,
    region      = forcats::fct_relevel(factor(region), "Bamako"),   # reference region
    ethnicity   = forcats::fct_infreq(factor(ethnicity)),           # most common = reference
    education   = factor(education),
    age10       = (age - mean(age, na.rm = TRUE)) / 10,             # per decade, centered
    democracy_c = democracy - mean(democracy, na.rm = TRUE),
    perceived_c = perceived - mean(perceived, na.rm = TRUE),
    stress_c    = stress - mean(stress, na.rm = TRUE)
  )

# ---- 5. Analysis sample and design columns -----------------------------------------
# Each wave is its own design, so stratum, commune, and village codes are pasted
# with the wave. Weights are rescaled to sum to each wave's sample size so that
# no wave dominates the pooled models.
ad <- dat |>
  tidyr::drop_na(fav, events, weight, female, age10, ethnicity, education, urban,
                 local_gov, democracy_c, perceived_c, stress_c) |>
  dplyr::mutate(
    strata_w   = paste(wave, region),
    commune_w  = paste(wave, commune_id),
    village_w  = paste(wave, commune_id, village),
    wt_scaled  = weight * dplyr::n() / sum(weight),
    .by = wave
  )
