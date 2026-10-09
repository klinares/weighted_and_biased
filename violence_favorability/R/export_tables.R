# R/export_tables.R
# Writes the M3 results and predicted probabilities to output/tables/ as CSV.
# Run after R/models.R and R/poststratify.R (analysis.qmd does this).

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_tbl <- function(x, name) readr::write_csv(x, file.path("output/tables", paste0(name, ".csv")), na = "")

# M3 fixed effects: log-odds with Wald 95% CI, and odds ratios
broom.mixed::tidy(m3, effects = "fixed", conf.int = TRUE) |>
  dplyr::transmute(term, estimate, std.error, z = statistic, p.value, conf.low, conf.high,
                   odds_ratio = exp(estimate), or_low = exp(conf.low), or_high = exp(conf.high)) |>
  write_tbl("m3_fixed_effects")

# M3 random effects (standard deviation and variance of each intercept)
as.data.frame(lme4::VarCorr(m3)) |>
  tibble::as_tibble() |>
  dplyr::transmute(group = grp, sd = sdcor, variance = vcov) |>
  write_tbl("m3_random_effects")

# Violence coefficient across models; glmer rows use z, svyglm rows use t with df
violence_rows |>
  dplyr::rename(z_or_t = stat) |>
  write_tbl("violence_by_model")

# Predicted favorability at set numbers of events per surveyed commune
predicted |>
  dplyr::transmute(events_per_commune = events, estimate, conf.low, conf.high) |>
  write_tbl("m3_predicted_by_events")

# Change from 0 to 5 events per commune, with the minimum detectable effect
tibble::tibble(comparison = "5 vs 0 events per surveyed commune",
               change = diff_0_5$estimate, conf.low = diff_0_5$conf.low, conf.high = diff_0_5$conf.high,
               min_detectable_change = -abs(mde_0_5)) |>
  write_tbl("m3_change_0_to_5")

# Predicted favorability for each level of each factor
write_tbl(predicted_by_var, "m3_predicted_by_variable")

# Poststratified estimates
write_tbl(ps_cells, "poststratified_by_cell")
write_tbl(ps_national, "poststratified_national")
