# =============================================================================
# testing/simulate_data.R
# Simulated survey for testing the pipeline. NOT needed for the real analysis.
# Run from the project root:  source("testing/simulate_data.R")
#
# Mimics the real data as currently understood:
#   - 3 waves, each its own design: strata = region; stage 1 = communes;
#     stage 2 = villages; stage 3 = one adult per household
#   - a panel of communes drawn once with probability proportional to size;
#     each wave interviews about 140 of them, so many communes repeat
#   - favorability around 80%; violence varies mostly between communes
#   - ACLED events attached per commune and wave; keys match the boundaries
#
# Writes data/survey_sim.rds (one row per respondent, truth stored as attribute).
# =============================================================================

source("R/boundaries.R")
set.seed(20261002)

# ---- Truth (used to check that the models recover it) ---------------------------
truth <- list(
  b_violence = -0.25,  # log-odds per 1 unit of log(1 + events), within and between
  sd_commune = 0.50,   # commune random intercept SD (tau00 = 0.25)
  sd_slope   = 0,      # no random slope: the LRT should not reject
  sd_village = 0.20,   # village random intercept SD
  sd_region_wave = 0.30  # region-by-wave shock SD (M3b should detect it)
)

waves         <- 1:3
wave_shift_v  <- c(0, 0.15, 0.30)   # violence rises a little across waves
wave_eff_fav  <- c(0, 0.10, -0.10)  # national favorability trend

regions <- tibble::tibble(
  region      = c("Kayes", "Koulikoro", "Sikasso", "S\u00e9gou",
                  "Mopti", "Tombouctou", "Gao", "Bamako"),
  region_risk = c(-0.4, -0.2, -0.5, 0.2, 1.0, 0.7, 0.8, -0.6),
  region_fav  = c(0.10, 0.05, 0.20, 0.00, -0.30, -0.20, -0.25, 0.15),
  panel_n     = c(26, 26, 26, 26, 26, 22, 18, 6),   # communes in the panel
  per_wave    = c(20, 20, 20, 20, 20, 17, 14, 6),   # interviewed each wave (~137)
  p_urban     = c(0.10, 0.10, 0.12, 0.10, 0.08, 0.10, 0.12, 1.00),
  north       = c(0, 0, 0, 0, 1, 1, 1, 0)
)

# ---- 1. Frame: real communes (unique names within region), simulated villages ------
dict <- read_mali_dictionary()
shared <- dict |> dplyr::count(adm1_key, adm3_key) |> dplyr::filter(n > 1)

communes <- dict |>
  dplyr::anti_join(shared, by = c("adm1_key", "adm3_key")) |>
  dplyr::inner_join(regions |> dplyr::mutate(adm1_key = region_key(region)),
                    by = "adm1_key") |>                      # Menaka filed under Gao; drops Kidal
  dplyr::transmute(
    region, region_risk, region_fav, panel_n, per_wave, north, adm1_key, adm3_key,
    commune     = adm3_name,
    urban       = as.integer(stats::runif(dplyr::n()) < p_urban),
    commune_hot = stats::rnorm(dplyr::n(), 0, 1.6),                 # persistent conflict risk
    u_commune   = stats::rnorm(dplyr::n(), 0, truth$sd_commune),
    n_villages  = sample(5:25, dplyr::n(), replace = TRUE)
  )

villages <- communes |>
  dplyr::select(adm3_key, adm1_key, n_villages, urban) |>
  dplyr::mutate(village_n = purrr::map(n_villages, seq_len)) |>
  tidyr::unnest(village_n) |>
  dplyr::mutate(
    village    = paste0("V", sprintf("%02d", village_n)),
    households = pmax(40, round(exp(stats::rnorm(dplyr::n(), 5 + 1.2 * urban, 0.6)))),
    u_village  = stats::rnorm(dplyr::n(), 0, truth$sd_village)
  )

communes <- communes |>
  dplyr::left_join(villages |> dplyr::summarise(households = sum(households),
                                                .by = c(adm1_key, adm3_key)),
                   by = c("adm1_key", "adm3_key"))

# ---- 2. Panel: communes drawn once, PPS on households, within region --------------------
panel <- communes |>
  dplyr::mutate(pi_panel = sampling::inclusionprobabilities(households, dplyr::first(panel_n)),
                .by = region) |>
  dplyr::mutate(pick = sampling::UPsystematic(pi_panel), .by = region) |>
  dplyr::filter(pick == 1)

# ---- 3. Events: mostly between-commune differences, small wave-to-wave change ---------------
events <- tidyr::expand_grid(panel |> dplyr::select(adm1_key, adm3_key, region_risk, commune_hot),
                             wave = waves) |>
  dplyr::mutate(events = stats::rnbinom(
    dplyr::n(), size = 10,
    mu = exp(-0.2 + region_risk + wave_shift_v[wave] + commune_hot))) |>
  dplyr::select(adm1_key, adm3_key, wave, events)

# ---- 4. Draw one wave ------------------------------------------------------------------
villages_per_commune   <- 3
households_per_village <- 6

draw_wave <- function(w) {
  # Stage 1: a simple random subsample of the panel within each region
  stage1 = panel |>
    dplyr::group_split(region) |>
    purrr::map(\(d) dplyr::slice_sample(d, n = d$per_wave[1])) |>
    purrr::list_rbind() |>
    dplyr::mutate(pi_commune = pi_panel * per_wave / panel_n)

  # Stage 2: villages, simple random sample within each selected commune
  stage2 = villages |>
    dplyr::semi_join(stage1, by = c("adm1_key", "adm3_key")) |>
    dplyr::mutate(pi_village = villages_per_commune / dplyr::n(), .by = c(adm1_key, adm3_key)) |>
    dplyr::slice_sample(n = villages_per_commune, by = c(adm1_key, adm3_key))

  # Stage 3: households, one adult each, about 80% respond
  stage2 |>
    dplyr::left_join(stage1 |> dplyr::select(-urban, -households, -n_villages),
                     by = c("adm1_key", "adm3_key")) |>
    dplyr::mutate(n_resp = pmax(2, stats::rbinom(dplyr::n(), households_per_village, 0.8))) |>
    dplyr::mutate(resp = purrr::map(n_resp, seq_len)) |>
    tidyr::unnest(resp) |>
    dplyr::mutate(
      wave     = w,
      adults   = sample(1:6, dplyr::n(), replace = TRUE),
      pi_house = households_per_village / households,
      weight   = (1 / (pi_commune * pi_village * pi_house)) * adults *
                 (households_per_village / n_resp)
    )
}

region_wave_shock <- tidyr::expand_grid(region = regions$region, wave = waves) |>
  dplyr::mutate(u_region_wave = stats::rnorm(dplyr::n(), 0, truth$sd_region_wave))

sample_all <- purrr::map(waves, draw_wave) |>
  purrr::list_rbind() |>
  dplyr::left_join(events, by = c("adm1_key", "adm3_key", "wave")) |>
  dplyr::left_join(region_wave_shock, by = c("region", "wave"))

# ---- 5. Respondents and outcome ---------------------------------------------------------
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
    v   = log1p(events),
    eta = 1.75 + wave_eff_fav[wave] + region_fav + truth$b_violence * v -
      0.25 * urban + 0.10 * female + 0.006 * (age - 36) +
      dplyr::case_when(ethnicity == "Tuareg" ~ -0.50, ethnicity == "Fulani" ~ -0.20, TRUE ~ 0) +
      u_commune + u_village + u_region_wave,
    fav = stats::rbinom(dplyr::n(), 1, stats::plogis(eta))
  )

# Light item nonresponse
survey <- survey |>
  dplyr::mutate(education = replace(education, stats::runif(dplyr::n()) < 0.01, NA))

# ---- 6. Write --------------------------------------------------------------------------
# The survey team duplicated region and commune as adm1_key and adm3_key
dir.create("data", showWarnings = FALSE)
survey_out <- survey |>
  dplyr::transmute(wave, region, commune, village,
                   adm1_key = region, adm3_key = commune,
                   weight, fav, events, female, age, ethnicity, education, urban)
attr(survey_out, "truth") <- truth
saveRDS(survey_out, "data/survey_sim.rds")

cw <- dplyr::distinct(survey_out, wave, region, commune)
print(glue::glue("Respondents: {nrow(survey_out)}; communes per wave: ",
                 "{toString(table(cw$wave))}; distinct communes: ",
                 "{nrow(dplyr::distinct(cw, region, commune))}; ",
                 "percent favorable: {round(100 * mean(survey_out$fav))}"))
