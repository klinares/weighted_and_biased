# =============================================================================
# testing/simulate_data.R
# Simulated survey for testing the pipeline. NOT needed for the real analysis.
# Run from the project root:  source("testing/simulate_data.R")
#
# Mimics the real data as currently understood:
#   - 3 waves, each its own design: strata = region; stage 1 = communes drawn
#     with probability proportional to size (PPS); stage 2 = villages drawn
#     at random within commune; stage 3 = households, one adult each
#   - ACLED events (violence against civilians) already attached to each
#     respondent's row, counted per commune and wave
#   - commune names typed in the survey, some misspelled, to test matching
#
# Writes:
#   data/survey_sim.rds                 one row per respondent
#   matching/communes_from_survey.csv   distinct region + commune names
#   testing/true_commune_codes.csv      the correct code for every survey
#                                       commune name, to grade the matching
# =============================================================================

pacman::p_load(sampling, glue, tidyverse)
source("R/boundaries.R")
set.seed(20261001)

# ---- Truth (used to check that the models recover it) ---------------------------
truth <- list(
  b_violence = -0.30,  # log-odds per 1 unit of log(1 + events)
  sd_commune = 0.35,   # commune random intercept SD
  sd_village = 0.20    # village random intercept SD
)

waves               <- 1:3
wave_shift_violence <- c(0, 0.25, 0.50)   # violence rises across waves
wave_effect_fav     <- c(0, 0.20, 0.35)   # national favorability trend

regions <- tibble::tibble(
  region      = c("Kayes", "Koulikoro", "Sikasso", "Segou",
                  "Mopti", "Tombouctou", "Gao", "Bamako"),
  region_risk = c(-0.4, -0.2, -0.5, 0.2, 1.0, 0.7, 0.8, -0.6),
  region_fav  = c(0.10, 0.05, 0.20, 0.00, -0.35, -0.25, -0.30, 0.15),
  m_communes  = c(18, 18, 18, 18, 18, 14, 12, 6),   # communes drawn per wave
  p_urban     = c(0.10, 0.10, 0.12, 0.10, 0.08, 0.10, 0.12, 1.00),
  north       = c(0, 0, 0, 0, 1, 1, 1, 0)
)

# ---- 1. Frame: real communes, simulated villages ---------------------------------
communes <- read_mali_dictionary() |>
  dplyr::mutate(region = stringi::stri_trans_general(dplyr::recode(adm1_name, !!!region_alias),
                                                   "Latin-ASCII")) |>   # "Ségou" -> "Segou"
  dplyr::inner_join(regions, by = "region") |>          # drops Kidal
  dplyr::transmute(
    region, region_risk, region_fav, m_communes, north,
    adm3_code, commune = adm3_name,
    urban        = as.integer(stats::runif(dplyr::n()) < p_urban),
    commune_hot  = stats::rnorm(dplyr::n(), 0, 0.9),     # local conflict risk
    u_commune    = stats::rnorm(dplyr::n(), 0, truth$sd_commune),
    n_villages   = sample(5:25, dplyr::n(), replace = TRUE)
  )

villages <- communes |>
  dplyr::select(adm3_code, n_villages, urban) |>
  dplyr::mutate(village_n = purrr::map(n_villages, seq_len)) |>
  tidyr::unnest(village_n) |>
  dplyr::mutate(
    village    = glue("V{sprintf('%02d', village_n)}"),
    households = pmax(40, round(exp(stats::rnorm(dplyr::n(), 5 + 1.2 * urban, 0.6)))),
    u_village  = stats::rnorm(dplyr::n(), 0, truth$sd_village)
  )

communes <- communes |>
  dplyr::left_join(villages |> dplyr::summarise(households = sum(households), .by = adm3_code),
                   by = "adm3_code")

# ---- 2. Events: violence against civilians, per commune and wave ---------------------
events <- tidyr::expand_grid(communes |> dplyr::select(adm3_code, region_risk, commune_hot),
                             wave = waves) |>
  dplyr::mutate(events = stats::rnbinom(dplyr::n(), size = 0.6,
                                        mu = exp(-0.3 + region_risk + wave_shift_violence[wave] +
                                                   commune_hot))) |>
  dplyr::select(adm3_code, wave, events)

# ---- 3. Draw one wave ------------------------------------------------------------------
villages_per_commune <- 3
households_per_village <- 6

draw_wave <- function(w) {
  # Stage 1: communes, PPS on households, within each region
  stage1 = communes |>
    dplyr::mutate(pi_commune = sampling::inclusionprobabilities(households, dplyr::first(m_communes)),
                  .by = region) |>
    dplyr::mutate(pick = sampling::UPsystematic(pi_commune), .by = region) |>
    dplyr::filter(pick == 1)

  # Stage 2: villages, simple random sample within each selected commune
  stage2 = villages |>
    dplyr::semi_join(stage1, by = "adm3_code") |>
    dplyr::mutate(take = min(villages_per_commune, dplyr::n()),
                  pi_village = take / dplyr::n(), .by = adm3_code) |>
    dplyr::slice_sample(n = villages_per_commune, by = adm3_code)

  # Stage 3: households, one adult each, about 80% respond
  stage2 |>
    dplyr::left_join(stage1 |> dplyr::select(-urban, -households), by = "adm3_code") |>
    dplyr::mutate(n_resp = pmax(2, stats::rbinom(dplyr::n(), households_per_village, 0.8))) |>
    dplyr::mutate(resp = purrr::map(n_resp, seq_len)) |>
    tidyr::unnest(resp) |>
    dplyr::mutate(
      wave       = w,
      adults     = sample(1:6, dplyr::n(), replace = TRUE),
      pi_house   = households_per_village / households,
      weight     = (1 / (pi_commune * pi_village * pi_house)) * adults *
                   (households_per_village / n_resp)   # nonresponse adjustment
    )
}

sample_all <- purrr::map(waves, draw_wave) |>
  purrr::list_rbind() |>
  dplyr::left_join(events, by = c("adm3_code", "wave"))

# ---- 4. Respondents and outcome ---------------------------------------------------------
ethnic <- c("Bambara", "Fulani", "Songhai", "Tuareg", "Soninke", "Dogon", "Other")

survey <- sample_all |>
  dplyr::mutate(
    female    = stats::rbinom(dplyr::n(), 1, 0.5),
    age       = pmin(80, pmax(18, round(stats::rnorm(dplyr::n(), 36, 13)))),
    ethnicity = dplyr::if_else(
      north == 1,
      sample(ethnic, dplyr::n(), TRUE, c(.08, .30, .25, .20, .02, .10, .05)),
      sample(ethnic, dplyr::n(), TRUE, c(.45, .15, .03, .02, .15, .08, .12))),
    education = sample(c("None", "Primary", "Secondary+"), dplyr::n(), TRUE, c(.45, .30, .25)),
    v         = log1p(events),
    perceived = pmin(5, pmax(1, round(2.3 + 0.4 * v + stats::rnorm(dplyr::n(), 0, 0.9)))),
    stress    = pmin(10, pmax(0, round(3 + 0.6 * perceived + stats::rnorm(dplyr::n(), 0, 1.8)))),
    local_gov = stats::rbinom(dplyr::n(), 1, stats::plogis(0.3 - 0.15 * v)),
    democracy = pmin(5, pmax(1, round(3 + 0.3 * (education == "Secondary+") +
                                        stats::rnorm(dplyr::n(), 0, 1)))),
    eta = -0.10 + wave_effect_fav[wave] + region_fav + truth$b_violence * v +
      0.70 * local_gov - 0.20 * (democracy - 3) - 0.10 * (perceived - 3) -
      0.30 * urban + 0.10 * female + 0.006 * (age - 36) +
      dplyr::case_when(ethnicity == "Tuareg" ~ -0.50, ethnicity == "Fulani" ~ -0.20, TRUE ~ 0) +
      u_commune + u_village,
    fav = stats::rbinom(dplyr::n(), 1, stats::plogis(eta))
  )

# ---- 5. Misspell about 15% of commune names, the same way every wave ---------------------
misspell <- function(x) {
  purrr::map_chr(x, \(s) {
    pick = sample(1:3, 1)
    if (pick == 1 && stringr::str_detect(s, "ou")) return(stringr::str_replace(s, "ou", "w"))
    if (pick == 2 && nchar(s) > 5) return(paste0(substr(s, 1, 3), substr(s, 5, nchar(s))))
    paste0(substr(s, 1, 2), substr(s, 2, nchar(s)))      # doubled letter
  })
}
spelling <- survey |>
  dplyr::distinct(adm3_code, commune) |>
  dplyr::mutate(commune_typed = dplyr::if_else(stats::runif(dplyr::n()) < 0.15,
                                               misspell(commune), commune))

survey <- survey |>
  dplyr::left_join(spelling |> dplyr::select(adm3_code, commune_typed), by = "adm3_code") |>
  dplyr::mutate(commune = commune_typed)

# ---- 6. Light item nonresponse -------------------------------------------------------------
survey <- survey |>
  dplyr::mutate(education = replace(education, stats::runif(dplyr::n()) < 0.01, NA),
                democracy = replace(democracy, stats::runif(dplyr::n()) < 0.02, NA))

# ---- 7. Write --------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
dir.create("matching", showWarnings = FALSE)
survey_out <- survey |>
  dplyr::transmute(wave, region, commune, village, weight, fav, events,
                   female, age, ethnicity, education, urban,
                   local_gov, democracy, perceived, stress)
attr(survey_out, "truth") <- truth
saveRDS(survey_out, "data/survey_sim.rds")

survey_out |>
  dplyr::distinct(region, commune) |>
  dplyr::arrange(region, commune) |>
  readr::write_csv("matching/communes_from_survey.csv")

spelling |>
  dplyr::left_join(communes |> dplyr::select(adm3_code, region), by = "adm3_code") |>
  dplyr::transmute(region, commune = commune_typed, true_adm3_code = adm3_code) |>
  readr::write_csv("testing/true_commune_codes.csv")

print(glue("Respondents: {nrow(survey_out)}; communes: {dplyr::n_distinct(survey$adm3_code)}; ",
           "misspelled names: {sum(spelling$commune_typed != spelling$commune)}"))
