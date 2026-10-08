# R/poststratify.R
# Poststratified percent favorable from M3.
#
# Idea: M3 gives every respondent a predicted probability of a favorable view. We average
# those predictions within each region x urban cell (separately by wave), then combine the
# cells using the cell's share of the adult population. That corrects for the sample having
# too many or too few people in a cell compared with the population.
#
# Population counts come from cfg$pop_file, with columns region, urban, population.
# The counts only cover region x urban, so inside a cell the mix of age, sex, and
# education still follows the sample, not the population.

if (!exists("m3")) source("R/models.R")


# ---- 1. Read the population counts and check they line up with the sample -----------------

pop <- readr::read_csv(cfg$pop_file, show_col_types = FALSE) |>
  dplyr::filter(population > 0) |>
  dplyr::mutate(region = as.character(region), urban = as.character(urban))

sample_cells <- ad |>
  dplyr::distinct(wave, region, urban) |>
  dplyr::mutate(wave = as.character(wave), region = as.character(region), urban = as.character(urban))

# A cell with people in it but no respondents in some wave cannot be estimated
expected_cells <- tidyr::expand_grid(wave = levels(ad$wave), dplyr::distinct(pop, region, urban))
missing_respondents <- dplyr::anti_join(expected_cells, sample_cells, by = c("wave", "region", "urban"))
if (nrow(missing_respondents) > 0) {
  stop("These populated cells have no respondents: ",
       paste(missing_respondents$wave, missing_respondents$region, missing_respondents$urban, collapse = "; "))
}

# A sampled cell with no population count would be silently dropped, so stop instead
missing_counts <- dplyr::anti_join(dplyr::distinct(sample_cells, region, urban), pop, by = c("region", "urban"))
if (nrow(missing_counts) > 0) {
  stop("These sampled cells are missing from ", cfg$pop_file, ": ",
       paste(missing_counts$region, missing_counts$urban, collapse = "; "))
}


# ---- 2. Simulate M3 predictions to carry the uncertainty through ---------------------------
# A point estimate alone is easy (average predict(m3)), but we also want an interval.
# We draw 1,000 plausible versions of the model:
#   - fixed effects from their estimated sampling distribution, and
#   - each cercle and cercle-wave effect from its estimate and conditional SD.
# For each draw we recompute every respondent's probability. The spread across draws
# gives the interval. It leaves out uncertainty in the variance parameters themselves,
# so the intervals are somewhat too narrow.

set.seed(2026)
n_draws <- 1000

# Fixed effects: one row per draw, one column per coefficient
beta_draws <- MASS::mvrnorm(n_draws, mu = lme4::fixef(m3), Sigma = as.matrix(stats::vcov(m3)))

# The fixed-effects design matrix M3 was fit with: one row per respondent
X <- lme4::getME(m3, "X")

# Random effects: draw each group's effect, then line the draws up with the respondents
draw_group_effects <- function(group, respondent_ids) {
  effects = as.data.frame(lme4::ranef(m3, condVar = TRUE)) |>
    dplyr::filter(grpvar == group)
  draws = matrix(stats::rnorm(nrow(effects) * n_draws, mean = effects$condval, sd = effects$condsd),
                 nrow = nrow(effects))
  draws[match(respondent_ids, as.character(effects$grp)), , drop = FALSE]
}
cercle_draws <- draw_group_effects("cercle_id", ad$cercle_id)
cercle_wave_draws <- draw_group_effects("cercle_wave", ad$cercle_wave)

# Linear predictor and probability: one row per respondent, one column per draw
linear_predictor <- X %*% t(beta_draws) + cercle_draws + cercle_wave_draws
prob_draws <- stats::plogis(linear_predictor)


# ---- 3. Average within cells, then weight the cells by population ---------------------------

# Number the cells, then find each respondent's cell with a join (no pasted text keys,
# so region names with accents such as Segou match in any locale)
cells <- ad |>
  dplyr::distinct(wave, region, urban) |>
  dplyr::arrange(wave, region, urban) |>
  dplyr::mutate(cell = dplyr::row_number())
respondent_cell <- dplyr::left_join(dplyr::select(ad, wave, region, urban), cells,
                                    by = c("wave", "region", "urban"))$cell

# Cell mean for every draw: sum the probabilities within each cell and divide by its size.
# rowsum() returns cells in increasing order of `cell`, matching the `cells` table.
cell_sums <- rowsum(prob_draws, group = respondent_cell)
cells <- cells |>
  dplyr::mutate(n = as.vector(table(respondent_cell)),
                wave = as.character(wave), region = as.character(region), urban = as.character(urban)) |>
  dplyr::left_join(pop, by = c("region", "urban"))
cell_means <- cell_sums / cells$n
stopifnot("every cell needs a population count" = !anyNA(cells$population))

# Summarise a set of draws (one row per estimate) as a mean and a 95% interval
summarise_draws <- function(draws) {
  tibble::tibble(
    estimate = rowMeans(draws),
    conf.low = apply(draws, 1, stats::quantile, probs = 0.025),
    conf.high = apply(draws, 1, stats::quantile, probs = 0.975)
  )
}

ps_cells <- dplyr::bind_cols(cells, summarise_draws(cell_means)) |>
  dplyr::select(wave, region, urban, population, n, estimate, conf.low, conf.high) |>
  dplyr::arrange(wave, region, urban)

# National estimate for one wave: population-weighted average of that wave's cell means
national_draws_for_wave <- function(w) {
  in_wave = cells$wave == w
  weights = cells$population[in_wave] / sum(cells$population[in_wave])
  colSums(cell_means[in_wave, , drop = FALSE] * weights)
}
waves <- sort(unique(cells$wave))
national_draws <- do.call(rbind, purrr::map(waves, national_draws_for_wave))   # one row per wave

# Unweighted sample percent, to show how much poststratification moves the estimate
sample_percent <- ad |>
  dplyr::summarise(sample = mean(fav), .by = wave) |>
  dplyr::mutate(wave = as.character(wave))

ps_national <- tibble::tibble(wave = waves) |>
  dplyr::bind_cols(summarise_draws(national_draws)) |>
  dplyr::left_join(sample_percent, by = "wave")


# ---- 4. Figure: the population counts and the poststratified estimates -------------------------

# Regions ordered by total population, largest at the top
region_order <- pop |>
  dplyr::summarise(total = sum(population), .by = region) |>
  dplyr::arrange(total) |>
  dplyr::pull(region)
area_colors <- c(rural = "#1b7837", urban = "#762a83")

plot_population <- pop |>
  dplyr::mutate(region = factor(region, levels = region_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = population / 1e6, y = region, fill = urban)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_fill_manual(values = area_colors, name = NULL) +
  ggplot2::labs(title = "A. Population", x = "Adults (millions)", y = NULL)

national_lines <- dplyr::mutate(ps_national, wave = paste("Wave", wave))

plot_estimates <- ps_cells |>
  dplyr::mutate(region = factor(region, levels = region_order), wave = paste("Wave", wave)) |>
  ggplot2::ggplot(ggplot2::aes(x = estimate, y = region, color = urban)) +
  ggplot2::geom_vline(data = national_lines, ggplot2::aes(xintercept = estimate),
                      linetype = "dashed", color = "gray40") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                           position = ggplot2::position_dodge(width = 0.5), size = 0.2) +
  ggplot2::scale_color_manual(values = area_colors, name = NULL) +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::facet_wrap(~wave, nrow = 1) +
  ggplot2::labs(title = "B. Percent favorable by region and area (dashed: national)", x = NULL, y = NULL)

ps_plot <- patchwork::wrap_plots(plot_population, plot_estimates, widths = c(1, 3)) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")
