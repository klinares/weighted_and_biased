# =============================================================================
# 01_benchmark_mrp.R
# Benchmark MRP on real LAPOP Colombia data. This is the estimator silicon
# must beat. No silicon here -- real respondents only.
#
# MODEL (classic Gelman MRP form; all categorical predictors as random
# intercepts for partial pooling, P-spline RW1-equivalent on year):
#   satisfied ~ (1|age_group) + (1|ethnicity) + (1|urbanicity)
#               + s(year, bs='ps', m=c(2,1)) + (1|department)
# Fit UNWEIGHTED (poststratification to population cell counts does the
# representativeness correction; weighting the model too would double-correct).
#
# WEIGHTS: enter only via the design-based OBSERVED estimates used for
# validation (Hajek weighted means). lapop_col$weight is weight1500, pulled
# wave-by-wave in the recode, so each year carries its own normalized weight.
# A per-year sum check confirms we grabbed the right weight.
#
# VALIDATION: leave-one-wave-out. Hold out each year, fit on the other 8,
# poststratify to the held-out year, compare predicted national and department
# estimates to the observed (design-based) values for that year. 2021 is the
# hardest hold-out (departments sampled only that wave).
#
# REPORTING GRAIN (marginal domains, not the full cell): national trend,
# department x year, and the three demographic margins over time.
#
# PARALLELISM: leave-one-wave-out fits run in parallel via furrr, each model
# 4 chains, no within-chain threading. 6 models x 4 chains = 24 (physical cores).
#
# CODING STANDARDS: set.seed(721); no for loops (purrr map/pmap); tidyverse
# loaded last; viridis for visuals; outputs persisted via save_result.
# =============================================================================

library(brms)
library(furrr)
library(future)
library(viridis)
library(gridExtra)
library(tidyverse)

set.seed(721)
source("00_config.R")

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
n_chains        <- 4L        # chains per model; no within-chain threading
models_parallel <- 6L        # 6 models x 4 chains = 24 physical cores
n_iter          <- 1500L
n_warmup        <- 500L

fits_dir <- file.path(results_dir, "fits")
if (!dir.exists(fits_dir)) dir.create(fits_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Inputs
# -----------------------------------------------------------------------------
load_if_missing("lapop_col")
load_if_missing("postrat_frame")

# =============================================================================
# 0. HELPERS + MODEL DATA
# =============================================================================
std_factors <- function(df) {
  df %>%
    mutate(
      age_group  = factor(age_group,  levels = age_levels),
      ethnicity  = factor(ethnicity,  levels = eth_levels),
      urbanicity = factor(urbanicity, levels = urb_levels),
      department = factor(department, levels = departments)
    )
}

# lapop_col carries department under the name `region`; rename so it matches the
# postrat frame. Drop rows with missing outcome or missing model covariates.
model_data <- lapop_col %>%
  filter(!is.na(satisfied), !is.na(age_group), !is.na(ethnicity),
         !is.na(urbanicity), !is.na(region), !is.na(weight)) %>%
  mutate(department = as.character(region)) %>%
  std_factors()

# Postrat frame factors must share the model's factor levels.
postrat_frame <- postrat_frame %>% std_factors()

cat("Model data rows: ", nrow(model_data), "\n", sep = "")

# WEIGHT CHECK: per-year sum should be ~1500 (minus dropped NA-covariate rows),
# confirming weight1500 (the wave-normalized analysis weight) was grabbed.
cat("\n--- Per-year weight sum (should be near 1500 each = weight1500 grabbed) ---\n")
model_data %>%
  group_by(year) %>%
  summarise(n = n(), weight_sum = round(sum(weight), 1), .groups = "drop") %>%
  print()

# =============================================================================
# 1. MODEL + FITTER
# =============================================================================
mrp_formula <- bf(
  satisfied ~ (1 | age_group) + (1 | ethnicity) + (1 | urbanicity) +
    s(year, bs = "ps", m = c(2, 1)) + (1 | department)
)

mrp_priors <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(exponential(1), class = "sd"),    # random-effect SDs
  prior(exponential(1), class = "sds")    # smooth SD
)

fit_mrp <- function(dat, label) {
  brm(
    mrp_formula, data = dat, family = bernoulli(), prior = mrp_priors,
    chains = n_chains, iter = n_iter, warmup = n_warmup, cores = n_chains,
    backend = "cmdstanr", seed = 721, refresh = 0,
    file = file.path(fits_dir, paste0("mrp_", label))
  )
}

# =============================================================================
# 2. POSTSTRATIFICATION (stream by year; aggregate to a within-year margin)
# =============================================================================
# within_vars = character(0) -> national; "department" -> dept; "age_group" etc.
postrat_margin <- function(fit, within_vars, years = study_years) {
  map(years, function(yr) {
    nd <- postrat_frame %>% filter(year == yr) %>% mutate(.row = row_number())
    ep <- posterior_epred(fit, newdata = nd, allow_new_levels = TRUE,
                          sample_new_levels = "gaussian")
    if (length(within_vars) == 0) {
      groups <- tibble(.g = 1L) %>%
        mutate(rows = list(nd$.row), w = list(nd$N_cell))
      keys <- tibble(year = yr)
    } else {
      groups <- nd %>%
        group_by(across(all_of(within_vars))) %>%
        summarise(rows = list(.row), w = list(N_cell), .groups = "drop")
      keys <- groups %>% select(all_of(within_vars)) %>% mutate(year = yr, .before = 1)
    }
    est <- pmap(list(groups$rows, groups$w), function(rows, w) {
      d <- as.vector(ep[, rows, drop = FALSE] %*% w) / sum(w)
      tibble(estimate = mean(d),
             lo = as.numeric(quantile(d, 0.05)),
             hi = as.numeric(quantile(d, 0.95)))
    }) %>% list_rbind()
    bind_cols(keys, est)
  }) %>% list_rbind()
}

# =============================================================================
# 3. OBSERVED (design-based Hajek weighted means) FOR VALIDATION
# =============================================================================
obs_national <- model_data %>%
  group_by(year) %>%
  summarise(obs = weighted.mean(satisfied, weight), .groups = "drop") %>%
  mutate(level = "national", group = "(national)")

obs_dept <- model_data %>%
  group_by(year, department) %>%
  summarise(obs = weighted.mean(satisfied, weight),
            n_obs = n(), .groups = "drop") %>%
  mutate(level = "department", group = as.character(department))

# =============================================================================
# 4. FULL-DATA FIT (the benchmark estimates)
# =============================================================================
fit_full <- fit_mrp(model_data, "full")

mrp_national    <- postrat_margin(fit_full, character(0))
mrp_dept_year   <- postrat_margin(fit_full, "department")
mrp_age_year    <- postrat_margin(fit_full, "age_group")
mrp_eth_year    <- postrat_margin(fit_full, "ethnicity")
mrp_urb_year    <- postrat_margin(fit_full, "urbanicity")

cat("\n--- MRP national trend vs observed (design-based) ---\n")
mrp_national %>%
  left_join(obs_national %>% select(year, obs), by = "year") %>%
  mutate(across(c(estimate, lo, hi, obs), function(x) round(x, 3))) %>%
  print(n = Inf)

# =============================================================================
# 5. LEAVE-ONE-WAVE-OUT VALIDATION (parallel across the 9 held-out years)
# =============================================================================
run_lowo <- function(h) {
  dat_h <- model_data %>% filter(year != h)
  fit_h <- fit_mrp(dat_h, paste0("lowo_", h))
  nat <- postrat_margin(fit_h, character(0), years = h) %>%
    transmute(year, level = "national", group = "(national)",
              pred = estimate, lo, hi)
  dep <- postrat_margin(fit_h, "department", years = h) %>%
    transmute(year, level = "department", group = as.character(department),
              pred = estimate, lo, hi)
  bind_rows(nat, dep)
}

plan(multisession, workers = models_parallel)
lowo_pred <- future_map(
  study_years, run_lowo,
  .options = furrr_options(seed = 721L,
                           packages = c("brms", "cmdstanr", "tidyverse"))
) %>%
  list_rbind()
plan(sequential)

# Attach observed and score.
obs_all <- bind_rows(
  obs_national %>% transmute(year, level, group, obs),
  obs_dept     %>% transmute(year, level, group, obs, n_obs)
)

lowo_eval <- lowo_pred %>%
  left_join(obs_all, by = c("year", "level", "group")) %>%
  mutate(err = pred - obs, in_ci = obs >= lo & obs <= hi)

cat("\n--- LOWO national: predicted (held-out) vs observed ---\n")
lowo_eval %>%
  filter(level == "national") %>%
  transmute(year, pred = round(pred, 3), obs = round(obs, 3),
            err = round(err, 3), in_ci) %>%
  arrange(year) %>%
  print(n = Inf)

cat("\n--- LOWO error summary by level (dept restricted to n_obs >= 5) ---\n")
lowo_eval %>%
  mutate(keep = level == "national" | (level == "department" & n_obs >= 5)) %>%
  filter(keep) %>%
  group_by(level) %>%
  summarise(
    n        = n(),
    rmse     = sqrt(mean(err^2, na.rm = TRUE)),
    bias     = mean(err, na.rm = TRUE),
    cover_90 = mean(in_ci, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- LOWO national error by held-out year (which years predict worst) ---\n")
lowo_eval %>%
  filter(level == "national") %>%
  transmute(year, abs_err = round(abs(err), 3)) %>%
  arrange(desc(abs_err)) %>%
  print(n = Inf)

# =============================================================================
# 6. VISUAL DIAGNOSTICS (viridis; analytical titles; not saved to disk)
# =============================================================================
viridis_col <- function(...) scale_color_viridis(option = "D", begin = 0.3,
                                                 end = 0.85, discrete = TRUE, ...)

p_trend <- mrp_national %>%
  mutate(source = "MRP (poststratified)") %>%
  bind_rows(obs_national %>% transmute(year, estimate = obs,
                                       source = "Observed (design-based)")) %>%
  ggplot(aes(year, estimate, color = source)) +
  geom_ribbon(data = mrp_national,
              aes(year, ymin = lo, ymax = hi),
              inherit.aes = FALSE, alpha = 0.15, fill = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  viridis_col(name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "MRP recovers the national satisfaction trend the survey already measures well",
    subtitle = "Poststratified MRP estimate (90% interval) vs design-based weighted estimate, LAPOP Colombia 2004-2023",
    x = NULL, y = "Satisfied with democracy"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

p_lowo <- lowo_eval %>%
  filter(level == "national") %>%
  ggplot(aes(obs, pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  geom_point(size = 2.5, color = viridis(1, begin = 0.4)) +
  geom_text(aes(label = year), vjust = -0.8, size = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Leave-one-wave-out: how well MRP predicts a year it never saw",
    subtitle = "Held-out-year national prediction vs observed; points on the dashed line are perfect",
    x = "Observed (design-based)", y = "MRP predicted (held-out)"
  ) +
  theme_minimal(base_size = 11)

grid.arrange(p_trend, p_lowo, ncol = 1, heights = c(1, 1))

# =============================================================================
# 7. PERSIST OUTPUTS
# =============================================================================
save_result(mrp_national,  "mrp_national")
save_result(mrp_dept_year, "mrp_dept_year")
save_result(mrp_age_year,  "mrp_age_year")
save_result(mrp_eth_year,  "mrp_eth_year")
save_result(mrp_urb_year,  "mrp_urb_year")
save_result(lowo_eval,     "mrp_lowo_eval")
cat("\nSaved MRP estimates and LOWO evaluation to", results_dir, "\n")

# =============================================================================
# OBJECTS PRODUCED:
#   mrp_national / mrp_dept_year / mrp_{age,eth,urb}_year - poststratified
#       estimates (mean + 90% interval) at each reporting margin
#   mrp_lowo_eval - held-out-year predictions vs observed, with error + coverage
#   cached fits   - results/fits/ as mrp_full and mrp_lowo_<year>
#
# READS:
#   * National trend should track the observed series closely (MRP shrinks
#     slightly). The value of MRP is at the dept x year and demographic margins
#     where the design-based estimates are noisy or unavailable.
#   * LOWO is the honest test: low national RMSE = good temporal borrowing;
#     2021 likely predicts worst (unique departments, half-missing outcome).
#
# NEXT: the silicon experiment re-anchored on MRP -- MRP cell prediction becomes
#   the LLM anchor, design-based calibration after, scored by LOWO against the
#   observed estimates. This is the only setting where silicon can legitimately
#   beat MRP (the LLM knows real Colombia).
# =============================================================================
