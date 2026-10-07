# R/prep_data.R
# Builds the analysis file `ad` from the prepared survey file.
#
# The file is named and coded upstream. It must have: wave, region, urban, commune,
# cercle, fav (1/0), weight, adm1_key, adm3_key, the violence count named in
# cfg$violence, and the covariates in cfg$covariates and cfg$covariates_m2b (factors,
# reference level first).
# The violence count is per cercle and wave, summed over the communes surveyed there.

if (!exists("cfg")) stop("Run the config chunk in analysis.qmd first.")

# Missing values are dropped on both covariate sets, so M2a and M2b use the same
# respondents and their AIC and BIC can be compared
ad <- readRDS(cfg$data_file) |>
  tidyr::drop_na(dplyr::all_of(c("fav", "wave", "region", "urban", "commune", "cercle",
                                 "weight", cfg$violence,
                                 union(cfg$covariates, cfg$covariates_m2b)))) |>
  dplyr::mutate(
    # Names repeat across regions, so IDs include the region
    commune_id = paste(region, commune, sep = " | "),
    cercle_id = paste(region, cercle, sep = " | "),
    cercle_wave = paste(cercle_id, wave, sep = " | ")
  ) |>
  # The count is a sum over surveyed communes, so a cercle with more surveyed
  # communes would look more violent. Divide by the number surveyed that wave.
  dplyr::mutate(events_per_commune = .data[[cfg$violence]] / dplyr::n_distinct(commune_id),
                .by = cercle_wave) |>
  dplyr::mutate(
    # Violence: log(1 + events per surveyed commune). 0 -> 0; +0.69 doubles (1 + events)
    v = log1p(events_per_commune),
    # Sensitivity split: the cercle's usual level (mean over the waves it was surveyed)
    # and that wave's departure from it
    v_cercle = mean(v[!duplicated(wave)]),
    v_change = v - v_cercle,
    .by = cercle_id
  ) |>
  # Each wave is its own sample: design codes carry the wave, and weights are
  # rescaled to sum to the wave's sample size so no wave dominates
  dplyr::mutate(strata_w = paste(wave, region, urban),
                psu_w = paste(wave, commune_id),
                wt = weight * dplyr::n() / sum(weight),
                .by = wave) |>
  dplyr::mutate(log_wt_c = log(wt) - mean(log(wt)))
