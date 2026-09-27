# =============================================================================
# simulate_data.R
# Simulated inputs for the regional violence x leader favorability analysis.
#
# Produces TWO files, mirroring what you will have at work:
#   data/survey_sim.rds            respondent-level survey, 3 waves, each wave
#                                  its own stratified two-stage PPS design
#   data/violence_commune_wave.rds ACLED-style event counts, commune x wave
#                                  (only communes with >= 1 event appear,
#                                   exactly like a raw ACLED aggregation)
#
# REPLACE THIS SCRIPT with your real data prep. The analysis only needs the
# columns listed in the data dictionary at the bottom of this file (or a
# rename map in analysis.qmd pointing to your real column names).
#
# Design mimicked, per wave (each wave drawn independently):
#   Stratum  = region (8)
#   PSU      = village (rural commune) or quartier (urban commune),
#              selected PPS systematic on household counts within stratum
#   SSU      = fixed take of households per PSU, one adult per household
#   Weights  = 1 / (pi_psu * pi_hh * pi_adult), nonresponse-adjusted in PSU
# Because PSUs are re-drawn every wave, communes appear in 1, 2, or 3 waves.
# =============================================================================

pacman::p_load(sampling, glue, tidyverse)

set.seed(20260926)

out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE)

# ---- Truth used to generate the outcome (to check recovery later) ----------
truth <- list(
  b_violence_direct = -0.30, # log-odds per 1 unit of log(1 + events)
  b_local_gov       =  0.80,
  b_democracy       = -0.25,
  b_perceived       = -0.15,
  b_urban           = -0.30,
  sd_commune        =  0.35,
  sd_psu            =  0.30
)

# ---- 1. Frame: regions, communes, PSUs ---------------------------------------
waves <- 1:3
wave_shift_violence <- c(0, 0.30, 0.60) # violence trends up across waves
wave_effect_fav     <- c(0, 0.25, 0.45) # secular shift in favorability

regions <- tibble::tibble(
  region      = c("Kayes", "Koulikoro", "Sikasso", "Segou",
                  "Mopti", "Tombouctou", "Gao", "Bamako"),
  region_risk = c(0.3, 0.5, 0.2, 0.8, 1.6, 1.4, 1.5, 0.2),
  n_communes  = c(50, 50, 50, 45, 45, 30, 25, 6),
  m_psu       = c(36, 36, 36, 36, 36, 32, 32, 40), # PSUs drawn per wave
  region_fav  = c(0.10, 0.05, 0.20, 0.00, -0.35, -0.25, -0.30, 0.15),
  p_urban     = c(0.10, 0.10, 0.12, 0.10, 0.08, 0.10, 0.12, 1.00),
  north       = c(0, 0, 0, 0, 1, 1, 1, 0)
)

communes <- regions |>
  dplyr::mutate(commune_n = purrr::map(n_communes, seq_len)) |>
  tidyr::unnest(commune_n) |>
  dplyr::mutate(
    commune_id   = glue("{toupper(substr(region, 1, 3))}_{sprintf('%02d', commune_n)}"),
    commune_name = glue("{region} Commune {sprintf('%02d', commune_n)}"),
    commune_type = dplyr::if_else(stats::runif(dplyr::n()) < p_urban,
                                  "Urban", "Rural"),
    commune_pop  = round(exp(stats::rnorm(dplyr::n(),
                                          dplyr::if_else(commune_type == "Urban", 11.5, 10), 0.5))),
    commune_hot  = stats::rnorm(dplyr::n(), 0, 0.8),  # latent local conflict risk
    commune_re   = stats::rnorm(dplyr::n(), 0, truth$sd_commune)
  ) |>
  dplyr::select(region, region_risk, region_fav, north, commune_id, commune_name,
                commune_type, commune_pop, commune_hot, commune_re)

# PSUs: villages (rural) or quartiers (urban); measure of size = households
psus <- communes |>
  dplyr::mutate(n_psu = sample(8:22, dplyr::n(), replace = TRUE)) |>
  dplyr::mutate(psu_n = purrr::map(n_psu, seq_len)) |>
  tidyr::unnest(psu_n) |>
  dplyr::mutate(
    psu_id  = glue("{commune_id}_{sprintf('%02d', psu_n)}"),
    psu_hh  = pmax(30, round(exp(stats::rnorm(dplyr::n(),
                                              dplyr::if_else(commune_type == "Urban", 6.2, 5.0), 0.6)))),
    psu_re  = stats::rnorm(dplyr::n(), 0, truth$sd_psu)
  )

# ---- 2. ACLED-style violence: commune x wave counts --------------------------
violence_full <- tidyr::expand_grid(communes, wave = waves) |>
  dplyr::mutate(
    mu     = exp(-1.2 + region_risk + wave_shift_violence[wave] + commune_hot),
    events = stats::rnbinom(dplyr::n(), size = 0.5, mu = mu)
  ) |>
  dplyr::select(region, commune_id, commune_name, wave, events)

# ACLED only records events, so zero-event commune-waves are absent
violence_acled <- violence_full |>
  dplyr::filter(events > 0)

# ---- 3. Draw each wave's sample independently (its own design) ----------------
take_hh <- 8          # households approached per PSU
resp_rate <- 0.80     # household response rate (gives 3 to 8 completes per PSU)

draw_wave <- function(w) {
  # Stage 1: PPS systematic within each stratum (region)
  stage1 = psus |>
    dplyr::group_by(region) |>
    dplyr::mutate(
      pi_psu   = sampling::inclusionprobabilities(psu_hh, dplyr::first(
        regions$m_psu[regions$region == dplyr::first(region)])),
      selected = sampling::UPsystematic(pi_psu)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(selected == 1)

  # Stage 2: fixed take of households, then one adult per household
  stage1 |>
    dplyr::mutate(
      pi_hh     = take_hh / psu_hh,
      n_resp    = stats::rbinom(dplyr::n(), take_hh, resp_rate),
      n_resp    = pmax(n_resp, 2)
    ) |>
    dplyr::mutate(resp = purrr::map(n_resp, seq_len)) |>
    tidyr::unnest(resp) |>
    dplyr::mutate(
      wave      = w,
      n_adults  = sample(1:6, dplyr::n(), replace = TRUE,
                         prob = c(.10, .25, .25, .20, .12, .08)),
      pi_adult  = 1 / n_adults,
      nr_adj    = take_hh / n_resp,
      base_wt   = 1 / (pi_psu * pi_hh * pi_adult),
      wt        = base_wt * nr_adj
    )
}

sample_df <- purrr::map(waves, draw_wave) |>
  purrr::list_rbind() |>
  dplyr::left_join(violence_full |> dplyr::select(commune_id, wave, events),
                   by = c("commune_id", "wave"))

# ---- 4. Respondent characteristics and attitudes ------------------------------
ethnic_levels <- c("Bambara", "Fulani", "Songhai", "Tuareg",
                   "Soninke", "Dogon", "Other")

sample_df <- sample_df |>
  dplyr::mutate(
    female    = stats::rbinom(dplyr::n(), 1, 0.50),
    age       = pmin(80, pmax(18, round(stats::rnorm(dplyr::n(), 36, 13)))),
    ethnicity = dplyr::if_else(
      north == 1,
      sample(ethnic_levels, dplyr::n(), replace = TRUE,
             prob = c(.08, .30, .25, .20, .02, .10, .05)),
      sample(ethnic_levels, dplyr::n(), replace = TRUE,
             prob = c(.45, .15, .03, .02, .15, .08, .12))),
    education = sample(c("None", "Primary", "Secondary+"), dplyr::n(),
                       replace = TRUE, prob = c(.45, .30, .25)),
    urban     = as.integer(commune_type == "Urban"),
    v_log     = log1p(events),

    # Perceived violence (1 to 5): driven by actual events (mediator)
    perceived_violence = pmin(5, pmax(1, round(2.3 + 0.45 * v_log +
                                                 stats::rnorm(dplyr::n(), 0, 0.9)))),
    # Stress (0 to 10): downstream of perceived violence (mediator)
    stress = pmin(10, pmax(0, round(3 + 0.6 * perceived_violence +
                                      stats::rnorm(dplyr::n(), 0, 1.8)))),
    # Local government approval (binary): lowered by violence
    local_gov_approve = stats::rbinom(dplyr::n(), 1,
                                      stats::plogis(0.3 - 0.15 * v_log + 0.3 * commune_re)),
    # Support for democracy (1 to 5): education-driven
    democracy_support = pmin(5, pmax(1, round(3 + 0.35 * (education == "Secondary+") -
                                                0.2 * (education == "None") +
                                                stats::rnorm(dplyr::n(), 0, 1))))
  )

# ---- 5. Outcome: binary leader favorability ----------------------------------
sample_df <- sample_df |>
  dplyr::mutate(
    eta = -0.10 +
      wave_effect_fav[wave] + region_fav +
      truth$b_violence_direct * v_log +
      truth$b_local_gov * local_gov_approve +
      truth$b_democracy * (democracy_support - 3) +
      truth$b_perceived * (perceived_violence - 3) +
      truth$b_urban * urban +
      0.10 * female + 0.006 * (age - 36) +
      dplyr::case_when(ethnicity == "Tuareg" ~ -0.50,
                       ethnicity == "Fulani" ~ -0.20,
                       TRUE ~ 0) +
      commune_re + psu_re,
    fav = stats::rbinom(dplyr::n(), 1, stats::plogis(eta))
  )

# ---- 6. Light item nonresponse (so listwise deletion code gets exercised) ----
add_missing <- function(x, rate) {
  x[stats::runif(length(x)) < rate] = NA
  x
}

survey_sim <- sample_df |>
  dplyr::mutate(
    education          = add_missing(education, 0.01),
    democracy_support  = add_missing(democracy_support, 0.02),
    local_gov_approve  = add_missing(local_gov_approve, 0.02),
    stress             = add_missing(stress, 0.01),
    resp_id            = glue("W{wave}_{dplyr::row_number()}"),
    strata             = region
  ) |>
  dplyr::select(
    resp_id, wave, strata, region, psu_id, commune_id, commune_name, commune_type,
    wt, fav, female, age, ethnicity, education, urban,
    local_gov_approve, democracy_support, perceived_violence, stress
  )

attr(survey_sim, "truth") <- truth

saveRDS(survey_sim, file.path(out_dir, "survey_sim.rds"))
saveRDS(violence_acled, file.path(out_dir, "violence_commune_wave.rds"))
# Full commune frame (every admin-3 unit, sampled or not): only used to build
# test boundaries. Real work uses geoBoundaries admin-3 polygons instead.
saveRDS(communes |> dplyr::select(region, commune_id, commune_name),
        file.path(out_dir, "commune_frame.rds"))

# ---- 7. Quick report ---------------------------------------------------------
survey_sim |>
  dplyr::summarise(
    n = dplyr::n(), n_psu = dplyr::n_distinct(psu_id),
    n_commune = dplyr::n_distinct(commune_id), fav = mean(fav),
    .by = wave
  ) |>
  print()

survey_sim |>
  dplyr::distinct(commune_id, wave) |>
  dplyr::count(commune_id, name = "waves_present") |>
  dplyr::count(waves_present, name = "n_communes") |>
  print()

glue("Wrote {nrow(survey_sim)} respondents and {nrow(violence_acled)} commune-wave event rows to {out_dir}/")

# =============================================================================
# DATA DICTIONARY (what analysis.qmd expects; map real names in var_map there)
# survey file
#   resp_id            respondent ID
#   wave               1, 2, 3
#   strata             design stratum (region)
#   region             region name (must match map names; see analysis)
#   psu_id             PSU code, stable across waves if the same village
#   commune_id         admin-3 commune code, the key to the ACLED file
#   commune_name       admin-3 commune name, the key to the boundary file
#   commune_type       Urban / Rural (official commune classification)
#   wt                 final respondent weight for that wave
#   fav                1 = favorable view of leader, 0 = not
#   female             1/0
#   age                years
#   ethnicity          categorical
#   education          None / Primary / Secondary+
#   urban              1/0 respondent or PSU urban flag
#   local_gov_approve  1/0
#   democracy_support  1 to 5
#   perceived_violence 1 to 5 (mediator)
#   stress             0 to 10 (mediator)
# violence file (ACLED aggregate)
#   commune_id, wave, events  (zero-event commune-waves may be absent)
# =============================================================================
