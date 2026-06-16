# =============================================================================
# 03_silicon_evaluation.R
# Design-based evaluation of MRP-anchored silicon sampling.
#
# WHAT THIS IS (and is NOT): the estimator AFTER the LLM is DESIGN-BASED
#   (Horvitz-Thompson / Taylor linearization via the survey package), calibrated
#   to one-way census MARGINS by raking/GREG. It is deliberately NOT another MRP
#   poststratification -- MRP is the "before" anchor, this is the "after", so the
#   two stages never relaunder each other.
#
# THE "WHAT IF 4,000" FRAME: we simulate a survey of n_target respondents per
#   year (4,000) in place of LAPOP's ~1,200. n_target sets the real:silicon
#   weight ratio. Real units keep their design weights (scaled to n_real);
#   silicon fills the remainder, weighted by POPULATION STRUCTURE (N_cell),
#   outcome-blind. The combined weights are then raked to census margins.
#
# THREE-LAYER UNCERTAINTY (all defensible, kept separate):
#   * within-draw design (sampling) variance  -> Taylor linearization (survey),
#       with silicon as a CERTAINTY (census) stratum => zero sampling variance,
#   * between-draw imputation variance         -> Rubin's rules over m draws,
#   * (MRP posterior enters only via the anchor used at generation time).
#   In LOWO mode the held-out year has no real units, so within-draw design
#   variance is 0 and ALL uncertainty is the between-draw (imputation) term --
#   the honest statement that a fully reconstructed year is imputation-limited.
#
# MODES:
#   "lowo"    (headline): for each held-out year, silicon (anchored on that
#             year's LOWO MRP fit) reconstructs the whole year; compared to the
#             observed Hajek mean and to the MRP LOWO benchmark (RMSE 0.128,
#             interval coverage 0.507). Apples-to-apples with 01.
#   "augment" (deliverable): real + silicon (denied/sparse cells) per surveyed
#             year; reported small-area estimates with widening intervals.
#
# INPUT CONTRACT: silicon_respondents with columns
#   year, department, age_group, ethnicity, urbanicity, draw, response (0/1).
#   For "lowo" this must be the LOWO-anchored, all-cells set (separate
#   generation pass). For "augment" it is the fit_full-anchored denied/sparse set.
#
# CODING STANDARDS: set.seed(721); no for loops (purrr; explicit function(x));
#   tidyverse loaded last; viridis (option D, begin .3 / end .85 / alpha .7);
#   gridExtra panels; analytical titles; nothing written unless requested.
# =============================================================================

library(survey)
library(gridExtra)
library(viridis)
library(tidyverse)

set.seed(721)
source("00_config.R")

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
mode      <- "lowo"        # "lowo" (headline) or "augment" (deliverable)
n_target  <- 4000          # simulated respondents/year (sets real:silicon ratio)
n_h       <- 5L            # candidates per cell per draw (matches 02)
min_n_obs <- 5L            # dept x year cells scored (matches 01's headline)
keys      <- c("year", "department", "age_group", "ethnicity", "urbanicity")

options(survey.lonely.psu = "adjust")

# -----------------------------------------------------------------------------
# Inputs
# -----------------------------------------------------------------------------
load_if_missing("silicon_respondents")   # the LOWO (or augment) silicon set
load_if_missing("postrat_frame")         # supplies N_cell + population margins
load_if_missing("lapop_col")             # real respondents (augment mode)
load_if_missing("mrp_lowo_eval")         # observed truth + MRP benchmark

# lapop_col carries the region variable as `region`; every other object uses
# `department` (01 renamed it when building model_data). Harmonize here so the
# key helpers and the real-respondent branch find the column.
if ("region" %in% names(lapop_col) && !("department" %in% names(lapop_col)))
  lapop_col <- lapop_col %>% rename(department = region)

key_chr <- function(df) df %>%
  mutate(across(c(department, age_group, ethnicity, urbanicity), as.character))

postrat_frame      <- key_chr(postrat_frame)
silicon_respondents <- key_chr(silicon_respondents)

# Population cell sizes (population-structure base weights + raking targets).
pop_cells <- postrat_frame %>% select(all_of(keys), N_cell)

# Observed Hajek truth + MRP benchmark coverage, department level, scored cells.
truth <- mrp_lowo_eval %>%
  filter(level == "department", n_obs >= min_n_obs) %>%
  transmute(year, department = as.character(group),
            obs, n_obs, mrp_in_ci = in_ci, mrp_pred = pred)

# =============================================================================
# 1. POPULATION MARGINS PER YEAR (raking targets; scaled to n_target)
# =============================================================================
# One-way margin on the n_target scale: population proportion x n_target,
# restricted to the levels actually present in this draw's sample (you can only
# calibrate to strata you have -- absent ones would error in rake/postStratify).
make_margin <- function(yr, var, present) {
  pop_cells %>%
    filter(year == yr, .data[[var]] %in% present) %>%
    group_by(level = .data[[var]]) %>%
    summarise(Freq = sum(N_cell), .groups = "drop") %>%
    mutate(Freq = Freq / sum(Freq) * n_target) %>%
    rename(!!var := level)
}

# =============================================================================
# 2. ONE (year, draw) -> design-based department estimates + within-draw SE
# =============================================================================
# Silicon base weight = N_cell / n_h (population structure); scaled within year
# to the silicon share of n_target. Silicon sits in a single CERTAINTY stratum
# per year (fpc = its own size => sampling fraction 1 => zero design variance).
# Real (augment mode) keeps weight1500, scaled to n_real, with a huge fpc so its
# variance is the with-replacement form LAPOP uses.
estimate_one <- function(yr, d) {
  
  sil_yd <- silicon_respondents %>%
    filter(year == yr, draw == d) %>%
    left_join(pop_cells, by = keys) %>%
    mutate(base_raw = N_cell / n_h)
  
  real_y <- if (mode == "augment") {
    lapop_col %>%
      key_chr() %>%
      filter(year == yr, !is.na(satisfied), !is.na(weight))
  } else {
    lapop_col[0, ] %>% key_chr()
  }
  n_real <- nrow(real_y)
  sil_target <- if (mode == "augment") max(n_target - n_real, 0) else n_target
  
  sil_part <- sil_yd %>%
    mutate(
      satisfied = response,
      w0        = base_raw / sum(base_raw) * sil_target,   # silicon share
      strata    = paste0("sil_", yr),
      psu       = str_c("s", row_number()),
      fpc       = n()                                       # certainty => 0 var
    ) %>%
    select(satisfied, w0, strata, psu, fpc, all_of(setdiff(keys, "year")))
  
  combined <- if (n_real > 0) {
    real_part <- real_y %>%
      mutate(
        w0     = weight / sum(weight) * n_real,             # real share
        strata = as.character(strata),
        psu    = as.character(psu),
        fpc    = 1e7                                         # ~with-replacement
      ) %>%
      select(satisfied, w0, strata, psu, fpc,
             department, age_group, ethnicity, urbanicity)
    bind_rows(real_part, sil_part)
  } else {
    sil_part
  }
  
  des <- svydesign(ids = ~ psu, strata = ~ strata, weights = ~ w0,
                   fpc = ~ fpc, data = combined, nest = TRUE)
  
  des_c <- rake(
    des,
    sample.margins     = list(~ department, ~ age_group, ~ ethnicity, ~ urbanicity),
    population.margins = list(
      make_margin(yr, "department", unique(combined$department)),
      make_margin(yr, "age_group",  unique(combined$age_group)),
      make_margin(yr, "ethnicity",  unique(combined$ethnicity)),
      make_margin(yr, "urbanicity", unique(combined$urbanicity))
    ),
    control = list(maxit = 50)
  )
  
  est <- svyby(~ satisfied, ~ department, des_c, svymean,
               na.rm = TRUE, vartype = "se")
  
  # Silicon sits in a certainty stratum, so it contributes zero sampling
  # variance. A fully-silicon (LOWO) domain therefore has no design variance and
  # svyby returns NA there -- that NA means zero, not missing. The uncertainty
  # for such domains is carried entirely by the between-draw (imputation) term
  # under Rubin's rules downstream.
  se_vec <- est$se
  se_vec[is.na(se_vec)] <- 0
  
  tibble(year = yr, draw = d,
         department = as.character(est$department),
         theta = est$satisfied, se = se_vec)
}

# =============================================================================
# 3. RUN over (year, draw); then COMBINE draws by Rubin's rules
# =============================================================================
grid <- silicon_respondents %>% distinct(year, draw)

draw_est <- pmap(grid, function(year, draw) estimate_one(year, draw)) %>%
  list_rbind()

# Rubin: point = mean of per-draw; total var = Wbar + (1 + 1/m) B; finite-m df.
rubin <- draw_est %>%
  group_by(year, department) %>%
  summarise(m         = n(),
            theta_bar = mean(theta),
            W_bar     = mean(se^2),            # within-draw (design) variance
            B         = var(theta),            # between-draw (imputation) var
            .groups   = "drop") %>%
  mutate(
    B        = if_else(is.na(B), 0, B),
    T_var    = W_bar + (1 + 1 / m) * B,
    se_total = sqrt(T_var),
    r        = (1 + 1 / m) * B / pmax(W_bar, 1e-12),
    df       = if_else(B > 0, (m - 1) * (1 + 1 / r)^2, Inf),
    tcrit    = qt(0.975, df),
    lo       = pmax(theta_bar - tcrit * se_total, 0),
    hi       = pmin(theta_bar + tcrit * se_total, 1)
  )

save_result(draw_est, paste0("silicon_draw_est_", mode))
save_result(rubin,    paste0("silicon_rubin_", mode))

# =============================================================================
# 4. HEADLINE: department x year RMSE + interval calibration vs MRP benchmark
# =============================================================================
scored <- rubin %>%
  inner_join(truth, by = c("year", "department")) %>%
  mutate(err      = theta_bar - obs,
         covered  = obs >= lo & obs <= hi,
         width    = hi - lo)

# Guard: an empty join silently turns every headline number into NaN. Surface
# the keys instead so a name/level mismatch is obvious rather than mysterious.
cat("\nScored department x year cells: ", nrow(scored), "\n", sep = "")
if (nrow(scored) == 0) {
  cat("rubin departments: ",
      paste(head(sort(unique(rubin$department)), 6), collapse = ", "), "\n",
      "truth departments: ",
      paste(head(sort(unique(truth$department)), 6), collapse = ", "), "\n",
      "rubin years: ", paste(sort(unique(rubin$year)), collapse = ", "), "\n",
      "truth years: ", paste(sort(unique(truth$year)), collapse = ", "), "\n",
      sep = "")
  stop("No overlap between silicon estimates and scored truth cells -- ",
       "check the department names / years printed above.")
}

headline <- scored %>%
  summarise(
    n             = n(),
    silicon_rmse  = sqrt(mean(err^2, na.rm = TRUE)),
    silicon_bias  = mean(err, na.rm = TRUE),
    silicon_cover = mean(covered, na.rm = TRUE),
    mrp_cover     = mean(mrp_in_ci, na.rm = TRUE),
    mrp_rmse      = sqrt(mean((mrp_pred - obs)^2, na.rm = TRUE)),
    mean_width    = mean(width, na.rm = TRUE)
  )

cat("\n=== HEADLINE: silicon (", mode, ") vs MRP benchmark, dept x year, n_obs >= ",
    min_n_obs, " ===\n", sep = "")
headline %>%
  mutate(across(where(is.numeric), function(x) round(x, 3))) %>%
  print(width = Inf)
cat("\nBenchmark targets from 01:  MRP dept RMSE 0.128  |  MRP coverage 0.507\n")
cat("Silicon wins on accuracy if silicon_rmse < mrp_rmse; on calibration if\n",
    "silicon_cover is closer to 0.90 than mrp_cover.\n", sep = "")

# =============================================================================
# 5. FIGURES (viridis; analytical titles; gridExtra)
# =============================================================================
vir_fill <- function(...) scale_fill_viridis_d(option = "D", begin = 0.3,
                                               end = 0.85, alpha = 0.7, ...)
vir_col  <- function(...) scale_color_viridis_d(option = "D", begin = 0.3,
                                                end = 0.85, ...)

# (a) Calibration: silicon vs MRP coverage against the 0.90 target.
cov_df <- tibble(
  method   = c("Silicon", "MRP benchmark"),
  coverage = c(headline$silicon_cover, headline$mrp_cover)
)
p_cov <- ggplot(cov_df, aes(method, coverage, fill = method)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0.90, linetype = "dashed") +
  vir_fill() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title    = if (isTRUE(headline$silicon_cover > headline$mrp_cover))
      "Silicon intervals recover the calibration MRP loses"
    else
      "Silicon does not yet fix MRP's interval under-coverage",
    subtitle = "Share of held-out department x year observed values inside the 90% interval (dashed = target)",
    x = NULL, y = "Interval coverage"
  ) +
  theme_minimal() + theme(legend.position = "none")

# (b) Interval width by coverage tier -- the honest "width grows as data thins".
width_tier <- rubin %>%
  left_join(
    postrat_frame %>%               # tier is per cell; collapse to dept x year
      distinct(year, department) %>%
      mutate(department = as.character(department)),
    by = c("year", "department")
  ) %>%
  left_join(truth %>% select(year, department, n_obs), by = c("year", "department")) %>%
  mutate(tier = case_when(is.na(n_obs)        ~ "denied (dept-yr)",
                          n_obs >= min_n_obs  ~ "covered",
                          TRUE                ~ "sparse")) %>%
  mutate(tier = factor(tier, levels = c("covered", "sparse", "denied (dept-yr)")))

p_width <- ggplot(width_tier, aes(tier, hi - lo, fill = tier)) +
  geom_boxplot(outlier.size = 0.5) +
  vir_fill() +
  labs(
    title    = "Uncertainty widens as real data thins, as it should",
    subtitle = "90% interval width by department x year data availability (silicon-augmented estimate)",
    x = NULL, y = "Interval width"
  ) +
  theme_minimal() + theme(legend.position = "none")

# (c) Predicted vs observed (scored cells); points near the line are accurate.
p_fit <- ggplot(scored, aes(obs, theta_bar, color = covered)) +
  geom_abline(linetype = "dashed") +
  geom_point(alpha = 0.6) +
  vir_col() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title    = paste0("Silicon recovers held-out department estimates (RMSE ",
                      round(headline$silicon_rmse, 3), ")"),
    subtitle = "Silicon-augmented vs observed Hajek mean; color = observed inside 90% interval",
    x = "Observed (design-based)", y = "Silicon-augmented estimate"
  ) +
  theme_minimal()

eval_panels <- arrangeGrob(p_cov, p_width, p_fit, ncol = 1,
                           heights = c(1, 1, 1.2))

cat("\nFigures assembled (p_cov, p_width, p_fit) in 'eval_panels'.\n")
cat("Saved silicon_draw_est_", mode, ", silicon_rubin_", mode, " to ",
    results_dir, "\n", sep = "")

# =============================================================================
# OBJECTS PRODUCED:
#   silicon_draw_est_<mode> - per (year, draw, department): theta + within-draw SE
#   silicon_rubin_<mode>    - per (year, department): point, total SE, 90% interval
#   headline (printed)      - RMSE + calibration vs the MRP benchmark
#   eval_panels             - calibration / width-by-tier / fit (call grid.arrange)
#
# VERIFY ON A REAL RUN (cannot be checked without R here):
#   * for a SILICON-ONLY (lowo) department, svyby SE should be ~0 -- confirms
#     silicon is treated as a certainty stratum (all uncertainty is the Rubin B
#     term). If SE is large there, the fpc/stratum setup needs fixing.
#   * rake() should converge within maxit for every (year, draw); a convergence
#     warning means a margin level had no respondents in that draw.
# =============================================================================