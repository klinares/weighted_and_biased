# R/poststratify.R
# Percent favorable in the adult population, poststratified to population counts by
# region and urban/rural (cfg$pop_file: columns region, urban, population).
#
# Classical poststratification of the survey design: within each wave, the survey weights
# are scaled so that each region x area cell sums to its population count. Estimates and
# standard errors then come from the design (Lumley, 2010, ch. 7). Weights still vary within
# a cell, so household selection and nonresponse adjustments are kept.
#
# Why not M3's predictions (MRP)? With counts for region x area only, a model cannot adjust
# for anything the design does not already cover, and M3's predictions include each cercle's
# own effects, so its cell estimates nearly reproduce the observed cell means. The design-based
# version gives the same answer with honest standard errors and much less code.

if (!exists("des_sample")) source("R/models.R")


# ---- 1. Population counts, one row per wave x region x area cell --------------------------

# A CSV saved from Excel on Windows is usually Windows-1252, not UTF-8. If the names are
# not valid UTF-8, read the file again in that encoding so accented names still match.
pop <- readr::read_csv(cfg$pop_file, show_col_types = FALSE)
if (!all(validUTF8(c(pop$region, pop$urban)))) {
  pop <- readr::read_csv(cfg$pop_file, show_col_types = FALSE, locale = readr::locale(encoding = "windows-1252"))
}
pop <- dplyr::filter(pop, population > 0)

cells <- ad |>
  dplyr::distinct(wave, region, urban) |>
  dplyr::arrange(wave, region, urban) |>
  dplyr::mutate(ps_cell = dplyr::row_number()) |>
  dplyr::left_join(pop, by = c("region", "urban"))

# Every sampled cell needs a count, and every populated cell needs respondents in every wave
if (anyNA(cells$population)) {
  missing = dplyr::filter(cells, is.na(population)) |> dplyr::distinct(region, urban)
  stop("No population count for: ", paste(missing$region, missing$urban, collapse = "; "))
}
if (nrow(cells) != nrow(pop) * dplyr::n_distinct(ad$wave)) {
  stop("Some populated region x area cells have no respondents in some wave; ",
       "they cannot be poststratified. Check ", cfg$pop_file, ".")
}


# ---- 2. Poststratify the sampling design -----------------------------------------------------

# Each cell belongs to one wave, so each wave's weights are scaled to the full population
des_ps <- ad |>
  dplyr::left_join(dplyr::select(cells, wave, region, urban, ps_cell), by = c("wave", "region", "urban")) |>
  srvyr::as_survey_design(ids = psu_w, strata = strata_w, weights = wt, nest = TRUE) |>
  survey::postStratify(strata = ~ps_cell,
                       population = data.frame(ps_cell = cells$ps_cell, Freq = cells$population))

# 95% interval on the logit scale from a proportion and its design-based SE. Near 80%
# favorable a symmetric interval can run past 100% in small cells; this one cannot.
# A proportion of exactly 0 or 1 has no usable SE, so its interval is left empty.
logit_ci <- function(p, se, side) {
  half = stats::qnorm(0.975) * se / (p * (1 - p))
  bound = stats::plogis(stats::qlogis(p) + side * half)
  dplyr::if_else(p > 0 & p < 1, bound, NA_real_)
}

ps_national <- des_ps |>
  dplyr::group_by(wave) |>
  dplyr::summarise(estimate = srvyr::survey_mean(fav)) |>
  dplyr::mutate(conf.low = logit_ci(estimate, estimate_se, -1), conf.high = logit_ci(estimate, estimate_se, 1)) |>
  dplyr::left_join(dplyr::summarise(ad, sample = mean(fav), .by = wave), by = "wave")

# A cell with one commune has no design-based variance (its interval would have zero width),
# so its interval is left empty rather than shown as exact.
ps_cells <- des_ps |>
  dplyr::group_by(wave, region, urban) |>
  dplyr::summarise(n = srvyr::unweighted(dplyr::n()),
                   communes = srvyr::unweighted(dplyr::n_distinct(psu_w)),
                   estimate = srvyr::survey_mean(fav)) |>
  dplyr::mutate(conf.low = dplyr::if_else(communes < 2, NA_real_, logit_ci(estimate, estimate_se, -1)),
                conf.high = dplyr::if_else(communes < 2, NA_real_, logit_ci(estimate, estimate_se, 1))) |>
  dplyr::left_join(dplyr::select(cells, wave, region, urban, population), by = c("wave", "region", "urban"))


# ---- 3. Do the survey weights agree with the counts? ------------------------------------------
# If the weights were calibrated to these counts, each cell's share of the weights would match
# its share of the population. A large gap means the counts and the weights describe different
# populations (or the counts are wrong), and the poststratified estimate leans on that choice.

share_check <- ad |>
  dplyr::summarise(weight = sum(wt), .by = c(wave, region, urban)) |>
  dplyr::left_join(dplyr::select(cells, wave, region, urban, population), by = c("wave", "region", "urban")) |>
  dplyr::mutate(weight_share = weight / sum(weight), population_share = population / sum(population),
                ratio = weight_share / population_share, .by = wave)

ps_cells <- dplyr::left_join(ps_cells, dplyr::select(share_check, wave, region, urban, weight_share, population_share),
                             by = c("wave", "region", "urban"))
worst_share <- dplyr::slice_max(share_check, abs(log(ratio)), n = 1, with_ties = FALSE)
pop_is_example <- grepl("example", basename(cfg$pop_file))


# ---- 4. Figure: the population counts and the poststratified estimates -------------------------

# Regions ordered by total population, largest at the top. Names come from the joined
# cells table, so they are spelled exactly as in the survey data in both panels.
cell_population <- dplyr::distinct(cells, region, urban, population)
region_order <- cell_population |>
  dplyr::summarise(total = sum(population), .by = region) |>
  dplyr::arrange(total) |>
  dplyr::pull(region) |>
  as.character()
area_colors <- c(rural = "#1b7837", urban = "#762a83")

plot_population <- cell_population |>
  dplyr::mutate(region = factor(region, levels = region_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = population / 1e6, y = region, fill = urban)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_fill_manual(values = area_colors, guide = "none") +   # same colors as panel B's legend
  ggplot2::labs(title = "A. Population", x = "Adults (millions)", y = NULL)

plot_estimates <- ps_cells |>
  dplyr::mutate(region = factor(region, levels = region_order), wave = paste("Wave", wave)) |>
  ggplot2::ggplot(ggplot2::aes(x = estimate, y = region, color = urban)) +
  ggplot2::geom_vline(data = dplyr::mutate(ps_national, wave = paste("Wave", wave)),
                      ggplot2::aes(xintercept = estimate), linetype = "dashed", color = "gray40") +
  ggplot2::geom_linerange(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                          position = ggplot2::position_dodge(width = 0.5), na.rm = TRUE) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 1.2) +
  ggplot2::scale_color_manual(values = area_colors, name = NULL) +
  ggplot2::scale_x_continuous(labels = scales::percent, breaks = c(0.5, 0.75, 1)) +
  ggplot2::facet_wrap(~wave, nrow = 1) +
  ggplot2::labs(title = "B. Percent favorable (dashed: national)", x = NULL, y = NULL) +
  ggplot2::theme(panel.spacing.x = ggplot2::unit(1.2, "lines"))

ps_plot <- patchwork::wrap_plots(plot_population, plot_estimates, widths = c(1, 3)) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")
