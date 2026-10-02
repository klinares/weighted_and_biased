# =============================================================================
# models.R
# Fits the models and computes every number the report uses.
# Plain top-to-bottom script: step through it in the console to debug.
#
#   M1  region-wave violence   survey design                  original specification
#   M2  commune violence       no weights, no clustering      naive comparison
#   M3  commune violence       commune random intercept       multilevel alternative
#   M4  commune violence       survey design                  MAIN MODEL
#
# Every model has the same region dummies, wave dummies, and covariates.
# =============================================================================

source("diagnostics.R")   # runs R/prep_data.R; creates `ad`, `dat`, `cells`, diagnostics
pacman::p_load(survey, lme4, marginaleffects, tidyverse)
options(survey.lonely.psu = "adjust")   # a stratum left with one commune after subsetting

# ---- 1. Survey design -------------------------------------------------------------
# Two-stage design within wave-specific strata: communes (PSUs), then villages.
# Standard errors come from the commune level (Taylor linearization), which also
# covers the correlation among villages and respondents within a commune.
des <- survey::svydesign(ids     = ~commune_w + village_w,
                         strata  = ~strata_w,
                         weights = ~wt_scaled,
                         nest    = TRUE,
                         data    = ad)

# ---- 2. Formulas ----------------------------------------------------------------------
f_region  <- fav ~ v_region  + region + wave + female + age10 + ethnicity + education + urban
f_commune <- fav ~ v_commune + region + wave + female + age10 + ethnicity + education + urban

# ---- 3. The four models ----------------------------------------------------------------
m1 <- survey::svyglm(f_region, design = des, family = quasibinomial())

m2 <- glm(f_commune, data = ad, family = binomial())

m3 <- lme4::glmer(fav ~ v_commune + region + wave + female + age10 + ethnicity +
                    education + urban + (1 | commune_id),
                  data = ad, family = binomial(),
                  control = lme4::glmerControl(optimizer = "bobyqa"))

m4 <- survey::svyglm(f_commune, design = des, family = quasibinomial())

# ---- 4. Supporting models (both use the survey design, like M4) ----------------------
# Attitudes added. Violence may change these attitudes, so this is descriptive.
m4_attitudes <- survey::svyglm(fav ~ v_commune + region + wave + female + age10 + ethnicity +
                                 education + urban + local_gov + democracy_c +
                                 perceived_c + stress_c,
                               design = des, family = quasibinomial())

# Within/between split (appendix): change inside a commune vs. differences between communes
m4_within <- survey::svyglm(fav ~ v_within + v_between + region + wave + female + age10 +
                              ethnicity + education + urban,
                            design = des, family = quasibinomial())

# ---- 5. Average marginal effects (AME) ---------------------------------------------------
# Change in Pr(favorable) for a one-unit increase in log(1 + events).
# Survey models need newdata = ad and wts = "wt_scaled" (design-weighted average).
# M3 uses re.form = NA: the effect for a typical commune (random intercept = 0).
ame_m1 <- marginaleffects::avg_slopes(m1, variables = "v_region",  newdata = ad, wts = "wt_scaled")
ame_m2 <- marginaleffects::avg_slopes(m2, variables = "v_commune", newdata = ad)
ame_m3 <- marginaleffects::avg_slopes(m3, variables = "v_commune", newdata = ad, re.form = NA)
ame_m4 <- marginaleffects::avg_slopes(m4, variables = "v_commune", newdata = ad, wts = "wt_scaled")
ame_m4_attitudes <- marginaleffects::avg_slopes(m4_attitudes, variables = "v_commune",
                                                newdata = ad, wts = "wt_scaled")
ame_m4_within    <- marginaleffects::avg_slopes(m4_within, variables = c("v_within", "v_between"),
                                                newdata = ad, wts = "wt_scaled")

results <- dplyr::bind_rows(
  "M1 region-wave"    = tibble::as_tibble(ame_m1),
  "M2 naive"          = tibble::as_tibble(ame_m2),
  "M3 multilevel"     = tibble::as_tibble(ame_m3),
  "M4 survey (main)"  = tibble::as_tibble(ame_m4),
  "M4 + attitudes"    = tibble::as_tibble(ame_m4_attitudes),
  .id = "model"
) |>
  dplyr::select(model, estimate, std.error, conf.low, conf.high) |>
  dplyr::mutate(model = factor(model, unique(model)))

within_between <- tibble::as_tibble(ame_m4_within) |>
  dplyr::select(term, estimate, std.error, conf.low, conf.high)

main     <- results |> dplyr::filter(model == "M4 survey (main)")
m1_row   <- results |> dplyr::filter(model == "M1 region-wave")
se_ratio <- main$std.error / results$std.error[results$model == "M2 naive"]

# ---- 6. Headline: predicted favorability at set event counts (M4) ------------------------
# "If every respondent's commune had X events", other characteristics as observed,
# averaged with the survey weights.
event_levels <- c(0, 1, 5, 20)
predicted <- marginaleffects::avg_predictions(
  m4, variables = list(v_commune = log1p(event_levels)),
  newdata = ad, wts = "wt_scaled") |>
  tibble::as_tibble() |>
  dplyr::mutate(events = event_levels) |>
  dplyr::select(events, estimate, conf.low, conf.high)

# ---- 7. Leave one region out (M4) ---------------------------------------------------------
# With 8 regions, check that no single region drives the result.
fit_without_region <- function(r) {
  d = ad |> dplyr::filter(region != r) |> droplevels()
  des_r = survey::svydesign(ids = ~commune_w + village_w, strata = ~strata_w,
                            weights = ~wt_scaled, nest = TRUE, data = d)
  m = survey::svyglm(f_commune, design = des_r, family = quasibinomial())
  marginaleffects::avg_slopes(m, variables = "v_commune", newdata = d, wts = "wt_scaled") |>
    tibble::as_tibble() |>
    dplyr::mutate(dropped = r)
}

loo <- levels(ad$region) |>
  purrr::map(fit_without_region) |>
  purrr::list_rbind()

# ---- 8. Model 3 checks -----------------------------------------------------------------------
m3_singular <- lme4::isSingular(m3)
m3_messages <- m3@optinfo$conv$lme4$messages        # NULL = converged without warnings
tau2        <- as.numeric(lme4::VarCorr(m3)$commune_id)
icc         <- tau2 / (tau2 + pi^2 / 3)              # latent-scale ICC for a logit model
u_hat       <- lme4::ranef(m3)$commune_id |>
  tibble::as_tibble(rownames = "commune_id") |>
  dplyr::rename(u = `(Intercept)`)

# ---- 9. Quick look when run in the console ----------------------------------------------------
print(results, width = Inf)
print(predicted)
