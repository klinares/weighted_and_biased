# R/models.R
# Fits the models and computes the numbers the report uses.
#
#   M1   empty model: cercle and cercle-by-wave random intercepts
#   M2a  M1 + violence, wave, region, urban, cfg$covariates
#   M2b  the same with cfg$covariates_m2b
#   M3   final model: the covariates of cfg$final ("M2a" or "M2b") + log weight
#
# Violence takes one value per cercle and wave, so both random intercepts are part
# of the design of every model, not something to test and drop. The cercle-by-wave
# intercept is what keeps the standard error of a cercle-wave exposure honest;
# dropping it understates that standard error.
# All glmer models are logistic, unweighted, fit by maximum likelihood on the same respondents.

source("R/prep_data.R")
options(survey.lonely.psu = "adjust")   # a stratum left with one PSU

stopifnot("cfg$final must be \"M2a\" or \"M2b\"" = cfg$final %in% c("M2a", "M2b"))

# nloptwrap with tight tolerances: fast, same likelihood as bobyqa
ctrl <- lme4::glmerControl(optimizer = "nloptwrap", optCtrl = list(xtol_abs = 1e-10, ftol_abs = 1e-10, maxeval = 1e5))

# Load a saved model, or fit and save it.
# Set cfg$refit = TRUE after changing the data, covariates, or model settings.
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
re <- c("(1 | cercle_id)", "(1 | cercle_wave)")
design_fixed <- c("wave", "region", "urban")
covs_final <- if (cfg$final == "M2b") cfg$covariates_m2b else cfg$covariates
fixed <- c(design_fixed, covs_final)   # used by M3, the sensitivity model, and svyglm

# ---- 1. Models ---------------------------------------------------------------------
m1 <- fit_glmer("m1", f("1", re))
m2a <- fit_glmer("m2a", f("v", design_fixed, cfg$covariates, re))
m2b <- fit_glmer("m2b", f("v", design_fixed, cfg$covariates_m2b, re))
m3 <- fit_glmer("m3", f("v", fixed, "log_wt_c", re))

# Sensitivity: violence split into the cercle's usual level and its wave-to-wave change
m_split <- fit_glmer("m_split", f("v_cercle", "v_change", fixed, "log_wt_c", re))

# Sensitivity: M3 plus a commune intercept. Communes are the sampled clusters, so the
# model-based convention would include them; M3 leaves them out because violence does
# not vary within a cercle-wave and, on the real data, the commune variance failed to converge.
m_commune <- fit_glmer("m_commune", f("v", fixed, "log_wt_c", re, "(1 | commune_id)"))

# Sensitivity: the full design strata (region x area, by wave) in place of region, area,
# and wave main effects. Selection depends on the stratum, so conditioning on all stratum
# indicators is what makes commune selection ignorable in the model.
m_strata <- fit_glmer("m_strata", f("v", "strata_w", covs_final, "log_wt_c", re))

# Does the violence slope differ by survey weight? A clearly nonzero interaction would mean
# the weights carry information about the slope itself (DuMouchel & Duncan, 1983).
m_wt_slope <- fit_glmer("m_wt_slope", f("v * log_wt_c", fixed, re))

# ---- 2. Design-based checks ----------------------------------------------------------
# Same fixed effects as M3 (without the log weight), quasibinomial. These are marginal
# (population-averaged) models, so their coefficients answer a different question from M3's.
#
# Main check: cercles as the clusters, regions as strata. This is a choice about the
# estimand, not a description of the sampling: communes were sampled, cercles were not.
# Violence is assigned per cercle and wave, and treating the association as one that would
# hold for other cercle shocks calls for clustering where the exposure is assigned
# (Moulton, 1990; Abadie et al., 2023). Cercles nest within regions; a cercle can contain
# urban and rural communes, so urban cannot be a stratum here.
# Bamako is a single cercle in its region, so it is a lonely PSU: survey.lonely.psu = "adjust".
des_cercle <- ad |>
  srvyr::as_survey_design(ids = cercle_id, strata = region, weights = wt, nest = TRUE)
m_svy <- survey::svyglm(f("v", fixed), design = des_cercle, family = quasibinomial())

# Same clustering without the weights. Comparing it with m_svy shows how much of the gap
# between M3 and the weighted marginal model comes from the weights rather than from the
# difference between conditional and marginal coefficients.
des_cercle_unweighted <- ad |>
  srvyr::as_survey_design(ids = cercle_id, strata = region, nest = TRUE)
m_svy_unweighted <- survey::svyglm(f("v", fixed), design = des_cercle_unweighted, family = quasibinomial())

# Comparison only: the sampling design as drawn (communes within region x urban,
# separately by wave). This is the right design for descriptive estimates such as
# percent favorable by wave. For the violence coefficient it treats communes of the same
# cercle as independent information about violence; shown so the effect of the
# clustering choice is visible. (Gao urban has one commune per wave: a lonely PSU.)
des_sample <- ad |>
  srvyr::as_survey_design(ids = psu_w, strata = strata_w, weights = wt, nest = TRUE)
m_svy_commune <- survey::svyglm(f("v", fixed), design = des_sample, family = quasibinomial())

# ---- 3. Covariate comparison, fit, and ICCs ---------------------------------------------
# M2a vs M2b: a likelihood ratio test only when one covariate set contains the other;
# otherwise compare AIC and BIC (same respondents either way)
nested_m2 <- all(cfg$covariates %in% cfg$covariates_m2b) || all(cfg$covariates_m2b %in% cfg$covariates)
same_m2 <- setequal(cfg$covariates, cfg$covariates_m2b)
test_m2 <- if (nested_m2 && !same_m2) stats::anova(m2a, m2b) else NULL
p_m2 <- if (is.null(test_m2)) NA_real_ else test_m2$`Pr(>Chisq)`[2]

models <- list(M1 = m1, M2a = m2a, M2b = m2b, M3 = m3)
comparison <- tibble::tibble(
  model = names(models),
  log_lik = purrr::map_dbl(models, \(m) as.numeric(stats::logLik(m))),
  n_par = purrr::map_dbl(models, \(m) attr(stats::logLik(m), "df")),
  aic = purrr::map_dbl(models, stats::AIC),
  bic = purrr::map_dbl(models, stats::BIC),
  singular = purrr::map_lgl(models, lme4::isSingular)
)

# Latent-scale ICCs from M1; pi^2/3 stands in for the individual-level variance of a logit model
var_of <- function(m, group) {
  v = as.data.frame(lme4::VarCorr(m))
  v$vcov[v$grp == group]
}
tau_cercle <- var_of(m1, "cercle_id")
tau_cw <- var_of(m1, "cercle_wave")
total <- tau_cercle + tau_cw + pi^2 / 3
icc <- tibble::tibble(
  level = c("Same cercle, any wave", "Same cercle, same wave"),
  variance = c(tau_cercle, tau_cercle + tau_cw),
  icc = c(tau_cercle / total, (tau_cercle + tau_cw) / total)
)

# ---- 4. The violence effect ----------------------------------------------------------
# glmer: Wald z with normal p-values. svyglm: t with the design's residual degrees of
# freedom (clusters minus strata, plus 1, minus coefficients), which is what summary() uses.
coef_row <- function(m, label) {
  s = summary(m)$coefficients
  is_svy = inherits(m, "svyglm")
  tibble::tibble(model = label, estimate = s["v", 1], std.error = s["v", 2], stat = s["v", 3],
                 df = if (is_svy) m$df.residual else NA_real_, p.value = s["v", 4])
}
violence_rows <- dplyr::bind_rows(
  coef_row(m2a, "M2a"),
  coef_row(m2b, "M2b"),
  coef_row(m3, "M3 (final)"),
  coef_row(m_commune, "M3 + commune intercept"),
  coef_row(m_strata, "M3 with stratum-by-wave indicators"),
  coef_row(m_svy, "Marginal, weighted, cercle clusters (design-based check)"),
  coef_row(m_svy_unweighted, "Marginal, unweighted, cercle clusters"),
  coef_row(m_svy_commune, "Marginal, weighted, commune clusters")
)
b3 <- dplyr::filter(violence_rows, model == "M3 (final)")
ci3 <- b3$estimate + c(-1.96, 1.96) * b3$std.error

# Violence x log weight interaction from m_wt_slope
wt_slope <- summary(m_wt_slope)$coefficients["v:log_wt_c", ]

# M3's convergence check: lme4 flags an absolute gradient above 0.002, which large samples
# trip without a real problem. The relative gradient (the step the optimizer would still
# take) is the better check; below about 0.001 the fit has converged.
rel_gradient <- with(m3@optinfo$derivs, max(abs(solve(Hessian, gradient))))

# Predicted favorability at set numbers of events per surveyed commune (typical cercle).
# Levels above the largest value in the data would be extrapolation, so they are dropped.
max_events <- max(ad$events_per_commune)
event_levels <- cfg$event_levels[cfg$event_levels <= max_events]
predicted <- marginaleffects::avg_predictions(
  m3, variables = list(v = log1p(event_levels)), newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::mutate(events = expm1(v))

# Change from 0 to 5 events per commune, with its 95% CI
diff_0_5 <- marginaleffects::avg_comparisons(
  m3, variables = list(v = c(0, log1p(5))), newdata = ad, re.form = NA) |>
  tibble::as_tibble()

# Minimum detectable effect (80% power, two-sided alpha .05): about 2.8 standard errors,
# converted to percentage points with the average slope of the logistic curve,
# mean(p * (1 - p)), and scaled to 0 vs 5 events (log 6 units of v)
p_hat <- stats::predict(m3, type = "response", re.form = NA)
mde_0_5 <- 2.8 * b3$std.error * mean(p_hat * (1 - p_hat)) * log1p(5)

# ---- 5. Predicted favorability by variable ---------------------------------------------
# Everyone set to each level of the variable, everything else as observed (typical cercle).
by_level <- function(var) {
  marginaleffects::avg_predictions(m3, variables = var, newdata = ad, re.form = NA) |>
    tibble::as_tibble() |>
    dplyr::transmute(variable = var, level = as.character(.data[[var]]), estimate, conf.low, conf.high)
}
predicted_by_var <- purrr::keep(fixed, \(x) is.factor(ad[[x]])) |>
  purrr::map(by_level) |>
  purrr::list_rbind()

# ---- 6. Usual level vs change -----------------------------------------------------------
# Are the two parts of the split model different? Wald test of their difference.
split_coef <- lme4::fixef(m_split)[c("v_cercle", "v_change")]
split_vcov <- as.matrix(stats::vcov(m_split))[c("v_cercle", "v_change"), c("v_cercle", "v_change")]
split_diff <- unname(split_coef["v_change"] - split_coef["v_cercle"])
split_diff_se <- sqrt(split_vcov[1, 1] + split_vcov[2, 2] - 2 * split_vcov[1, 2])
split_diff_p <- 2 * stats::pnorm(-abs(split_diff / split_diff_se))

# ---- 7. Calibration ---------------------------------------------------------------------
# Simulate data sets from M3 with new cercle effects and check that observed favorability,
# in bins of expected probability, falls inside the simulated range. The bins and the
# simulated range use separate simulations, so simulation noise in the binning cannot
# line up with the range it is compared against.
expected_sims <- stats::simulate(m3, nsim = 200, re.form = NA, seed = 2)
sims <- stats::simulate(m3, nsim = 200, re.form = NA, seed = 1)
calibration <- ad |>
  dplyr::mutate(expected = rowMeans(expected_sims), bin = dplyr::ntile(expected, 20)) |>
  dplyr::bind_cols(sims) |>
  dplyr::summarise(expected = mean(expected), observed = mean(fav),
                   dplyr::across(dplyr::starts_with("sim_"), mean), .by = bin) |>
  tidyr::pivot_longer(dplyr::starts_with("sim_"), values_to = "sim_mean") |>
  dplyr::summarise(sim_low = stats::quantile(sim_mean, 0.025), sim_high = stats::quantile(sim_mean, 0.975),
                   .by = c(bin, expected, observed))
