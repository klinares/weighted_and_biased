# R/models.R
# Fits the models and computes the numbers the report uses.
#
#   M1a  commune random intercept
#   M1b  + cercle random intercept
#   M2a  violence, wave, region, urban, covariates
#   M2b  M2a with the strata in place of region and urban
#   M3   final model: the chosen structure (cfg$cercle, cfg$strata) + log weight
#
# All are logistic multilevel models, unweighted, fit by maximum likelihood on the
# same respondents. cfg$cercle and cfg$strata are set by reading the M1 and M2 tests.

source("R/prep_data.R")
options(survey.lonely.psu = "adjust")   # a stratum left with one commune

# nloptwrap with tight tolerances: about 10x faster than bobyqa here, same likelihood
ctrl <- lme4::glmerControl(optimizer = "nloptwrap", optCtrl = list(xtol_abs = 1e-10, ftol_abs = 1e-10, maxeval = 1e5))

# Load a saved model, or fit and save it.
# Set cfg$refit = TRUE after changing the data, covariates, or switches.
dir.create(cfg$model_dir, showWarnings = FALSE)
fit_glmer <- function(name, formula) {
  path = file.path(cfg$model_dir, paste0(name, ".rds"))
  if (file.exists(path) && !cfg$refit) return(readRDS(path))
  fit = lme4::glmer(formula, data = ad, family = binomial(), control = ctrl)
  saveRDS(fit, path)
  fit
}

# Builds "fav ~ a + b + ..." from pieces
f <- function(...) stats::as.formula(paste("fav ~", paste(c(...), collapse = " + ")))
covs <- cfg$covariates
re <- if (cfg$cercle) c("(1 | commune_id)", "(1 | cercle_id)") else "(1 | commune_id)"
place <- if (cfg$strata) "strata" else c("region", "urban")

# ---- 1. Models ---------------------------------------------------------------------
m1a <- fit_glmer("m1a", f("1", "(1 | commune_id)"))
m1b <- fit_glmer("m1b", f("1", "(1 | commune_id)", "(1 | cercle_id)"))
m2a <- fit_glmer("m2a", f("v", "wave", "region", "urban", covs, re))
m2b <- fit_glmer("m2b", f("v", "wave", "strata", covs, re))
m3 <- fit_glmer("m3", f("v", "wave", place, covs, "log_wt_c", re))

# Sensitivity: violence split into the cercle average and the commune's distance from it
m_cercle <- fit_glmer("m_cercle", f("v_cercle", "v_within_cercle", "wave", place, covs, "log_wt_c", re))

# Design-based check: same fixed effects, survey weights, strata, communes as PSUs.
# Its coefficients are population-averaged, so compare it by z, not by size.
des <- survey::svydesign(ids = ~psu_w, strata = ~strata_w, weights = ~wt, nest = TRUE, data = ad)
m_svy <- survey::svyglm(f("v", "wave", place, covs), design = des, family = quasibinomial())

# ---- 2. Tests and fit --------------------------------------------------------------
# Adding the cercle variance: anova() gives a chi-square(1) p-value; a variance
# cannot be negative, so the correct p-value is half of it.
test_m1 <- stats::anova(m1a, m1b)
p_m1 <- test_m1$`Pr(>Chisq)`[2] / 2

# M2a is nested in M2b (the strata contain region and urban): ordinary anova() test
test_m2 <- stats::anova(m2a, m2b)
p_m2 <- test_m2$`Pr(>Chisq)`[2]

models <- list(M1a = m1a, M1b = m1b, M2a = m2a, M2b = m2b, M3 = m3)
comparison <- tibble::tibble(
  model = names(models),
  log_lik = purrr::map_dbl(models, \(m) as.numeric(stats::logLik(m))),
  n_par = purrr::map_dbl(models, \(m) attr(stats::logLik(m), "df")),
  aic = purrr::map_dbl(models, stats::AIC),
  bic = purrr::map_dbl(models, stats::BIC),
  singular = purrr::map_lgl(models, lme4::isSingular)
)

# Latent-scale ICCs; pi^2/3 stands in for the individual-level variance of a logit model
vc <- as.data.frame(lme4::VarCorr(m1b))
tau_commune <- vc$vcov[vc$grp == "commune_id"]
tau_cercle <- vc$vcov[vc$grp == "cercle_id"]
total <- tau_commune + tau_cercle + pi^2 / 3
icc <- tibble::tibble(
  model = c("M1a", "M1b", "M1b"),
  level = c("Commune", "Cercle (same cercle, different commune)", "Commune (same commune, includes cercle)"),
  icc = c(performance::icc(m1a)$ICC_adjusted, tau_cercle / total, (tau_cercle + tau_commune) / total)
)

# ---- 3. The violence effect ----------------------------------------------------------
coef_v <- function(m, label) {
  s = summary(m)$coefficients
  tibble::tibble(model = label, estimate = s["v", 1], std.error = s["v", 2], z = s["v", 3])
}
violence_rows <- dplyr::bind_rows(coef_v(m2a, "M2a"), coef_v(m2b, "M2b"), coef_v(m3, "M3 (final)"),
                                  coef_v(m_svy, "Design-based (svyglm)"))
b3 <- violence_rows[3, ]
p_v <- 2 * stats::pnorm(-abs(b3$z))

# Predicted favorability if every commune had 0, 1, 5, or 20 events (typical commune and cercle)
predicted <- marginaleffects::avg_predictions(
  m3, variables = list(v = log1p(cfg$event_levels)), newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::mutate(events = expm1(v))

# Change from 0 to 5 events, with its 95% CI
diff_0_5 <- marginaleffects::avg_comparisons(
  m3, variables = list(v = c(0, log1p(5))), newdata = ad, re.form = NA) |>
  tibble::as_tibble()

# Minimum detectable effect (80% power, alpha .05): about 2.8 standard errors,
# converted to percentage points with the average slope of the logistic curve,
# mean(p * (1 - p)), and scaled to 0 vs 5 events (log 6 units of v)
p_hat <- stats::predict(m3, type = "response", re.form = NA)
mde_0_5 <- 2.8 * b3$std.error * mean(p_hat * (1 - p_hat)) * log1p(5)

# ---- 4. Predicted favorability by variable ---------------------------------------------
# Factor variables: everyone set to each level, everything else as observed.
# Strata: average prediction for the people in each stratum, so no prediction is
# made for a stratum that does not exist (such as rural Bamako).
by_level <- function(var) {
  marginaleffects::avg_predictions(m3, variables = var, newdata = ad, re.form = NA) |>
    tibble::as_tibble() |>
    dplyr::transmute(variable = var, level = as.character(.data[[var]]), estimate, conf.low, conf.high)
}
factor_vars <- c("wave", purrr::keep(covs, \(x) is.factor(ad[[x]])))
by_strata <- marginaleffects::avg_predictions(m3, by = "strata", newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::transmute(variable = "strata", level = as.character(strata), estimate, conf.low, conf.high)
predicted_by_var <- dplyr::bind_rows(purrr::map(factor_vars, by_level), by_strata)

# ---- 5. Calibration ---------------------------------------------------------------------
# With 8 people per commune, residuals built on fitted random effects show a false
# slope. Instead, simulate 200 data sets from M3 with new commune and cercle effects
# and check that observed favorability falls inside the simulated range.
sims <- stats::simulate(m3, nsim = 200, re.form = NA, seed = 1)
calibration <- ad |>
  dplyr::mutate(expected = rowMeans(sims), bin = dplyr::ntile(expected, 20)) |>
  dplyr::bind_cols(sims) |>
  dplyr::summarise(expected = mean(expected), observed = mean(fav),
                   dplyr::across(dplyr::starts_with("sim_"), mean), .by = bin) |>
  tidyr::pivot_longer(dplyr::starts_with("sim_"), values_to = "sim_mean") |>
  dplyr::summarise(sim_low = stats::quantile(sim_mean, 0.025), sim_high = stats::quantile(sim_mean, 0.975),
                   .by = c(bin, expected, observed))
