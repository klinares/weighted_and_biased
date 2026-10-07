# R/models.R
# Fits the models and computes the numbers the report uses.
#
#   M1a  cercle random intercept                         how much favorability clusters
#   M1b  + cercle-by-wave random intercept               do cercles shift from wave to wave?
#   M2a  M1 winner + violence, wave, region, urban, cfg$covariates
#   M2b  the same with cfg$covariates_m2b
#   M3   final model: the covariates of cfg$final ("M2a" or "M2b") + log weight
#
# Violence is measured per cercle and wave, so the cercle is the grouping level.
# Communes stay in the design-based check as the sampling units (PSUs).
# All models are logistic, unweighted, fit by maximum likelihood on the same respondents.

source("R/prep_data.R")
options(survey.lonely.psu = "adjust")   # a stratum left with one commune

# nloptwrap with tight tolerances: fast, same likelihood as bobyqa
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
stopifnot("cfg$final must be \"M2a\" or \"M2b\"" = cfg$final %in% c("M2a", "M2b"))
design_fixed <- c("wave", "region", "urban")
covs_final <- if (cfg$final == "M2b") cfg$covariates_m2b else cfg$covariates
fixed <- c(design_fixed, covs_final)   # used by M3, the sensitivity model, and svyglm
re <- if (cfg$cercle_wave) c("(1 | cercle_id)", "(1 | cercle_wave)") else "(1 | cercle_id)"

# ---- 1. Models ---------------------------------------------------------------------
m_commune <- fit_glmer("m_commune", f("1", "(1 | commune_id)"))   # reference only: commune ICC
m1a <- fit_glmer("m1a", f("1", "(1 | cercle_id)"))
m1b <- fit_glmer("m1b", f("1", "(1 | cercle_id)", "(1 | cercle_wave)"))
m2a <- fit_glmer("m2a", f("v", design_fixed, cfg$covariates, re))
m2b <- fit_glmer("m2b", f("v", design_fixed, cfg$covariates_m2b, re))
m3 <- fit_glmer("m3", f("v", fixed, "log_wt_c", re))

# Sensitivity: violence split into the cercle's usual level and its wave-to-wave change
m_split <- fit_glmer("m_split", f("v_cercle", "v_change", fixed, "log_wt_c", re))

# Design-based check: same fixed effects, survey weights, strata, communes as PSUs.
# Its coefficients are population-averaged, so compare it by z, not by size.
des <- survey::svydesign(ids = ~psu_w, strata = ~strata_w, weights = ~wt, nest = TRUE, data = ad)
m_svy <- survey::svyglm(f("v", fixed), design = des, family = quasibinomial())

# ---- 2. Test, fit, and ICCs --------------------------------------------------------
# Adding the cercle-by-wave variance: anova() gives a chi-square(1) p-value; a variance
# cannot be negative, so the correct p-value is half of it.
test_m1 <- stats::anova(m1a, m1b)
p_m1 <- test_m1$`Pr(>Chisq)`[2] / 2

# M2a vs M2b: a likelihood ratio test only when one covariate set contains the other;
# otherwise compare AIC and BIC (same respondents either way)
nested_m2 <- all(cfg$covariates %in% cfg$covariates_m2b) || all(cfg$covariates_m2b %in% cfg$covariates)
same_m2 <- setequal(cfg$covariates, cfg$covariates_m2b)
test_m2 <- if (nested_m2 && !same_m2) stats::anova(m2a, m2b) else NULL
p_m2 <- if (is.null(test_m2)) NA_real_ else test_m2$`Pr(>Chisq)`[2]

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
var_of <- function(m, group) {
  v = as.data.frame(lme4::VarCorr(m))
  v$vcov[v$grp == group]
}
tau_cercle <- var_of(m1b, "cercle_id")
tau_cw <- var_of(m1b, "cercle_wave")
total <- tau_cercle + tau_cw + pi^2 / 3
icc <- tibble::tibble(
  model = c("Commune only (reference)", "M1a", "M1b", "M1b"),
  level = c("Commune", "Cercle", "Cercle, across waves", "Cercle and wave (same cercle, same wave)"),
  icc = c(performance::icc(m_commune)$ICC_adjusted, performance::icc(m1a)$ICC_adjusted,
          tau_cercle / total, (tau_cercle + tau_cw) / total)
)

# ---- 3. The violence effect ----------------------------------------------------------
coef_row <- function(m, label) {
  s = summary(m)$coefficients
  tibble::tibble(model = label, estimate = s["v", 1], std.error = s["v", 2], z = s["v", 3])
}
violence_rows <- dplyr::bind_rows(coef_row(m2a, "M2a"), coef_row(m2b, "M2b"), coef_row(m3, "M3 (final)"),
                                  coef_row(m_svy, "Design-based (svyglm)"))
b3 <- dplyr::filter(violence_rows, model == "M3 (final)")
z_svy <- dplyr::filter(violence_rows, model == "Design-based (svyglm)")$z
p_v <- 2 * stats::pnorm(-abs(b3$z))

# Predicted favorability at set numbers of events per surveyed commune (typical cercle)
predicted <- marginaleffects::avg_predictions(
  m3, variables = list(v = log1p(cfg$event_levels)), newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::mutate(events = expm1(v))

# Change from 0 to 5 events per commune, with its 95% CI
diff_0_5 <- marginaleffects::avg_comparisons(
  m3, variables = list(v = c(0, log1p(5))), newdata = ad, re.form = NA) |>
  tibble::as_tibble()

# Minimum detectable effect (80% power, alpha .05): about 2.8 standard errors,
# converted to percentage points with the average slope of the logistic curve,
# mean(p * (1 - p)), and scaled to 0 vs 5 events (log 6 units of v)
p_hat <- stats::predict(m3, type = "response", re.form = NA)
mde_0_5 <- 2.8 * b3$std.error * mean(p_hat * (1 - p_hat)) * log1p(5)

# ---- 4. Predicted favorability by variable ---------------------------------------------
# Everyone set to each level of the variable, everything else as observed.
by_level <- function(var) {
  marginaleffects::avg_predictions(m3, variables = var, newdata = ad, re.form = NA) |>
    tibble::as_tibble() |>
    dplyr::transmute(variable = var, level = as.character(.data[[var]]), estimate, conf.low, conf.high)
}
predicted_by_var <- purrr::keep(fixed, \(x) is.factor(ad[[x]])) |>
  purrr::map(by_level) |>
  purrr::list_rbind()

# ---- 5. Calibration ---------------------------------------------------------------------
# Simulate 200 data sets from M3 with new cercle effects and check that observed
# favorability, in bins of expected probability, falls inside the simulated range.
sims <- stats::simulate(m3, nsim = 200, re.form = NA, seed = 1)
calibration <- ad |>
  dplyr::mutate(expected = rowMeans(sims), bin = dplyr::ntile(expected, 20)) |>
  dplyr::bind_cols(sims) |>
  dplyr::summarise(expected = mean(expected), observed = mean(fav),
                   dplyr::across(dplyr::starts_with("sim_"), mean), .by = bin) |>
  tidyr::pivot_longer(dplyr::starts_with("sim_"), values_to = "sim_mean") |>
  dplyr::summarise(sim_low = stats::quantile(sim_mean, 0.025), sim_high = stats::quantile(sim_mean, 0.975),
                   .by = c(bin, expected, observed))
