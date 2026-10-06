# R/prep_data.R
# Builds the analysis file `ad` from the prepared survey file.
#
# The file is named and coded upstream. It must have: wave, region, urban, strata,
# commune, cercle, fav (1/0), weight, adm1_key, adm3_key, the violence count named in
# cfg$violence, and the covariates in cfg$covariates. Factors arrive with their
# reference level first; numeric covariates arrive centered.

if (!exists("cfg")) stop("Run the config chunk in analysis.qmd first.")

ad <- readRDS(cfg$data_file) |>
  tidyr::drop_na(dplyr::all_of(c("fav", "wave", "region", "urban", "strata", "commune", "cercle",
                                 "weight", cfg$violence, cfg$covariates))) |>
  dplyr::mutate(
    # Commune and cercle names repeat across regions, so IDs include the region
    commune_id = paste(region, commune, sep = " | "),
    cercle_id = paste(region, cercle, sep = " | "),
    # Violence: log(1 + events). 0 events gives 0; each +0.69 doubles (1 + events)
    v = log1p(.data[[cfg$violence]])
  ) |>
  # Cercle split (sensitivity model): the average across the surveyed communes of
  # the same cercle and wave, each commune counted once, and the distance from it
  dplyr::mutate(v_cercle = mean(v[!duplicated(commune_id)]), .by = c(cercle_id, wave)) |>
  dplyr::mutate(v_within_cercle = v - v_cercle) |>
  # Each wave is its own sample: design codes carry the wave, and weights are
  # rescaled to sum to the wave's sample size so no wave dominates
  dplyr::mutate(strata_w = paste(wave, strata),
                psu_w = paste(wave, commune_id),
                wt = weight * dplyr::n() / sum(weight),
                .by = wave) |>
  dplyr::mutate(log_wt_c = log(wt) - mean(log(wt)))
