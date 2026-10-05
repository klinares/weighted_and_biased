# =============================================================================
# R/models.R
# Fits the models and computes every number the report uses.
# Plain top-to-bottom script: step through it in the console to debug.
#
#   M1   commune random intercept, no predictors              ICC
#   M2a  + violence, random intercept                         \ LRT: keep the
#   M2b  + violence, random intercept and violence slope      / random slope?
#   M3a  M2 winner + region, wave dummies, covariates         \ LRT: add a
#   M3b  M3a + region-by-wave random intercept                / region-wave effect?
#   M4   M3 winner + design: village random intercept and     final model
#        log weight (informative-sampling check)
#   M3-MW  M3 winner with violence split within/between       sensitivity
#   svyglm design-based check with the same fixed effects     comparison only
#
# All multilevel models are unweighted and fit by maximum likelihood (Laplace
# approximation) on the same respondents, so their likelihoods can be compared.
# =============================================================================

source("R/prep_data.R")
options(survey.lonely.psu = "adjust")   # a stratum left with one commune after subsetting

ctrl <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ---- Likelihood ratio test for variance components (helper from SURV617) ------
# Testing a variance at zero puts the null on the boundary of the parameter
# space, so the p-value is a 50:50 mixture of chi-square distributions.
#   adding a random slope + its covariance:   df = c(1, 2)
#   adding one random intercept:              df = c(0, 1)  (chi-square with 0 df = 0)
log_like_pluck_fun <- function(mod_fit) {
  mod_fit |> stats::logLik() |> as.numeric()
}

calculate_lrt_pvalue_fun <- function(full_model, reduced_model, df) {
  lrt_statistic = -2 * (log_like_pluck_fun(reduced_model) - log_like_pluck_fun(full_model))
  mixed_p_value = 0.5 * stats::pchisq(lrt_statistic, df = df[1], lower.tail = FALSE) +
                  0.5 * stats::pchisq(lrt_statistic, df = df[2], lower.tail = FALSE)
  tibble::tibble(lrt = lrt_statistic, df = paste(df, collapse = " & "), p_value = mixed_p_value)
}

# ---- 1. M1: how much favorability differs between communes -------------------------
m1 <- lme4::glmer(fav ~ 1 + (1 | commune_id),
                  data = ad, family = binomial(), control = ctrl)

# Latent-scale ICC for a logit model: tau00 / (tau00 + pi^2 / 3)
tau00_m1 <- as.numeric(lme4::VarCorr(m1)$commune_id)
icc_m1   <- tau00_m1 / (tau00_m1 + pi^2 / 3)

# ---- 2. M2: violence, with and without a random slope -----------------------------
m2a <- lme4::glmer(fav ~ v_commune + (1 | commune_id),
                   data = ad, family = binomial(), control = ctrl)

m2b <- lme4::glmer(fav ~ v_commune + (v_commune | commune_id),
                   data = ad, family = binomial(), control = ctrl)

lrt_slope    <- calculate_lrt_pvalue_fun(full_model = m2b, reduced_model = m2a, df = c(1, 2))
m2b_singular <- lme4::isSingular(m2b)

# Rule fixed in advance: keep the slope only if p < .05 AND M2b is not singular.
keep_slope <- lrt_slope$p_value < 0.05 && !m2b_singular

# The commune term carried into M3 and M4
commune_re <- if (keep_slope) "(v_commune | commune_id)" else "(1 | commune_id)"

# ---- 3. M3: region and wave dummies, covariates; then a region-wave effect --------
fixed_m3 <- "fav ~ v_commune + region + wave + female + age10 + ethnicity + education + urban"

f_m3a <- stats::as.formula(paste(fixed_m3, "+", commune_re))
f_m3b <- stats::as.formula(paste(fixed_m3, "+", commune_re, "+ (1 | region_wave)"))

m3a <- lme4::glmer(f_m3a, data = ad, family = binomial(), control = ctrl)
m3b <- lme4::glmer(f_m3b, data = ad, family = binomial(), control = ctrl)

# M3b adds one variance (region-wave shocks): mixture of chi-square 0 and 1
lrt_region_wave <- calculate_lrt_pvalue_fun(full_model = m3b, reduced_model = m3a, df = c(0, 1))
m3b_singular    <- lme4::isSingular(m3b)

# Rule fixed in advance: M3b proceeds only if p < .05, AIC is lower, and it is not singular.
keep_region_wave <- lrt_region_wave$p_value < 0.05 &&
  stats::AIC(m3b) < stats::AIC(m3a) && !m3b_singular

m3     <- if (keep_region_wave) m3b else m3a
f_m3   <- if (keep_region_wave) f_m3b else f_m3a
m3_re  <- paste(commune_re, if (keep_region_wave) "+ (1 | region_wave)" else "")

tau00_m3 <- lme4::VarCorr(m3)$commune_id["(Intercept)", "(Intercept)"]
icc_m3   <- tau00_m3 / (tau00_m3 + pi^2 / 3)    # residual commune ICC after the predictors

# ---- 4. M4: M3 winner + the survey design --------------------------------------------
# Strata are already the region dummies; communes (first stage) are already a
# random intercept. M4 adds the second stage (villages within communes) and the
# log of the weight. A clearly nonzero weight coefficient means selection is
# related to favorability beyond the predictors (informative sampling), so the
# unweighted estimates should be read with caution.
f_m4 <- stats::update(f_m3, . ~ . + log_wt_c + (1 | commune_id:village))
m4   <- lme4::glmer(f_m4, data = ad, family = binomial(), control = ctrl)

tau_village <- as.numeric(lme4::VarCorr(m4)[["commune_id:village"]])
b_weight    <- summary(m4)$coefficients["log_wt_c", ]   # estimate, SE, z, p

# ---- 5. Sensitivity: usual level of violence vs change (same random part as M3) ----------
f_mw  <- stats::as.formula(paste(
  "fav ~ v_within + v_between + region + wave + female + age10 + ethnicity + education + urban +",
  "(1 | commune_id)", if (keep_region_wave) "+ (1 | region_wave)" else ""))
m3_mw <- lme4::glmer(f_mw, data = ad, family = binomial(), control = ctrl)

# ---- 6. Design-based check (comparison only) --------------------------------------------
# Survey-weighted, population-averaged, Taylor-linearized SEs at the commune level.
des <- survey::svydesign(ids = ~psu_w + village_w, strata = ~strata_w,
                         weights = ~wt_scaled, nest = TRUE, data = ad)
m_svy <- survey::svyglm(stats::as.formula(fixed_m3), design = des, family = quasibinomial())

# ---- 7. Violence coefficient across models ------------------------------------------------
coef_row <- function(m, label) {
  tibble::tibble(model     = label,
                 estimate  = unname(lme4::fixef(m)["v_commune"]),
                 std.error = unname(sqrt(diag(as.matrix(stats::vcov(m))))["v_commune"]))
}

violence_rows <- dplyr::bind_rows(
  coef_row(m2a, "M2a"),
  coef_row(m3,  if (keep_region_wave) "M3b" else "M3a"),
  coef_row(m4,  "M4 (final)"),
  tibble::tibble(model     = "Design-based (svyglm)",
                 estimate  = unname(stats::coef(m_svy)["v_commune"]),
                 std.error = unname(sqrt(diag(stats::vcov(m_svy)))["v_commune"]))
) |>
  dplyr::mutate(z = estimate / std.error)

# Variance cost of the design terms: M4 vs M3 (both conditional, same scale)
design_cost <- tibble::tibble(
  se_ratio   = violence_rows$std.error[3] / violence_rows$std.error[2],
  coef_ratio = violence_rows$estimate[3] / violence_rows$estimate[2]
)

# ---- 8. Effects in percentage points (final model M4) ------------------------------------
# re.form = NA: for a typical commune, village, and region-wave (random effects at 0).
ame <- marginaleffects::avg_slopes(m4, variables = "v_commune",
                                   newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::select(estimate, std.error, conf.low, conf.high)

event_levels <- c(0, 1, 5, 20)
predicted <- marginaleffects::avg_predictions(
  m4, variables = list(v_commune = log1p(event_levels)),
  newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::mutate(events = expm1(v_commune)) |>
  dplyr::select(events, estimate, conf.low, conf.high)

predicted_curve <- marginaleffects::avg_predictions(
  m4, variables = list(v_commune = log1p(0:30)),
  newdata = ad, re.form = NA) |>
  tibble::as_tibble() |>
  dplyr::mutate(events = expm1(v_commune))

# ---- 9. Commune-level predicted favorability (for the map) ---------------------------
# M4 prediction for each respondent, including all of its random effects,
# averaged with the survey weights within commune and wave. Communes with few
# respondents are pulled toward the overall level (shrinkage), as intended.
commune_pred <- ad |>
  dplyr::mutate(pred = stats::predict(m4, type = "response")) |>
  dplyr::summarise(
    pred_fav = stats::weighted.mean(pred, weight),
    obs_fav  = stats::weighted.mean(fav, weight),
    events   = dplyr::first(events),
    n        = dplyr::n(),
    .by = c(adm1_key, adm3_key, commune_id, wave)
  )

# ---- 10. Quick look when run in the console ----------------------------------------------
print(tibble::tibble(icc_m1, slope_p = lrt_slope$p_value, m2b_singular, keep_slope,
                     rw_p = lrt_region_wave$p_value, m3b_singular, keep_region_wave))
print(violence_rows)
print(predicted)
