# =============================================================================
# models.R
# Fits the four models and computes the violence effects.
# Plain top-to-bottom script: run it line by line in the console to debug.
# analysis.qmd sources this file and only presents the results.
#
#   M1  region-wave violence,  survey design clustered on PSU      (original spec)
#   M2  commune violence,      no weights, no clustering           (naive)
#   M3  commune violence,      commune random intercept, no weights (multilevel)
#   M4  commune violence,      survey design clustered on commune  (MAIN MODEL)
# =============================================================================

source("diagnostics.R")   # runs R/prep_data.R; creates `ad`, `dat`, diagnostics
pacman::p_load(survey, lme4, marginaleffects, tidyverse)
options(survey.lonely.psu = "adjust")

# ---- 1. Formulas -----------------------------------------------------------------
# Same region and wave dummies and covariates in every model.
f_region <- fav ~ v_region + region + wave +
  female + age10 + ethnicity + education + urban

f_commune <- fav ~ v_within + v_between + region + wave +
  female + age10 + ethnicity + education + urban

# ---- 2. Survey designs -------------------------------------------------------------
# Same data, two clustering levels. Codes are wave-specific (see R/prep_data.R).
des_psu <- survey::svydesign(ids = ~psu_w, strata = ~strata_w,
                             weights = ~wt_scaled, nest = TRUE, data = ad)

des_commune <- survey::svydesign(ids = ~commune_w, strata = ~strata_w,
                                 weights = ~wt_scaled, nest = TRUE, data = ad)

# ---- 3. The four models ---------------------------------------------------------------
m1 <- survey::svyglm(f_region, design = des_psu, family = quasibinomial())

m2 <- glm(f_commune, data = ad, family = binomial())

m3 <- lme4::glmer(fav ~ v_within + v_between + region + wave +
                    female + age10 + ethnicity + education + urban +
                    (1 | commune_key),
                  data = ad, family = binomial(),
                  control = lme4::glmerControl(optimizer = "bobyqa"))

m4 <- survey::svyglm(f_commune, design = des_commune, family = quasibinomial())

# ---- 4. Sensitivity versions of the main model ----------------------------------------
# Attitudes added (violence may change these, so this is descriptive only)
m4_attitudes <- survey::svyglm(fav ~ v_within + v_between + region + wave +
                                 female + age10 + ethnicity + education + urban +
                                 local_gov + democracy_c + perceived_c + stress_c,
                               design = des_commune, family = quasibinomial())

# Binary exposure: any event versus none
m4_binary <- survey::svyglm(fav ~ any_within + any_between + region + wave +
                              female + age10 + ethnicity + education + urban,
                            design = des_commune, family = quasibinomial())

# ---- 5. Violence effects: average marginal effects ------------------------------------
# Change in Pr(favorable) per one-unit increase in log(1 + events).
# Weighted models (svyglm) need newdata = ad and wts = "wt_scaled".
# M3 uses re.form = NA: effects for a typical commune (random intercept = 0).
ame_m1 <- marginaleffects::avg_slopes(m1, variables = "v_region",
                                      newdata = ad, wts = "wt_scaled")
ame_m2 <- marginaleffects::avg_slopes(m2, variables = c("v_within", "v_between"))
ame_m3 <- marginaleffects::avg_slopes(m3, variables = c("v_within", "v_between"),
                                      re.form = NA)
ame_m4 <- marginaleffects::avg_slopes(m4, variables = c("v_within", "v_between"),
                                      newdata = ad, wts = "wt_scaled")
ame_m4_attitudes <- marginaleffects::avg_slopes(m4_attitudes,
                                                variables = c("v_within", "v_between"),
                                                newdata = ad, wts = "wt_scaled")
ame_m4_binary <- marginaleffects::avg_slopes(m4_binary,
                                             variables = c("any_within", "any_between"),
                                             newdata = ad, wts = "wt_scaled")

# ---- 6. One results table ------------------------------------------------------------
results <- dplyr::bind_rows(
  "M1"                     = tibble::as_tibble(ame_m1),
  "M2"                     = tibble::as_tibble(ame_m2),
  "M3"                     = tibble::as_tibble(ame_m3),
  "M4"                     = tibble::as_tibble(ame_m4),
  "M4 + attitudes"         = tibble::as_tibble(ame_m4_attitudes),
  "M4, any event (binary)" = tibble::as_tibble(ame_m4_binary),
  .id = "model"
) |>
  dplyr::select(model, term, estimate, std.error, conf.low, conf.high) |>
  dplyr::mutate(
    part = dplyr::case_when(
      term == "v_region"                      ~ "Region-wave",
      term %in% c("v_within", "any_within")   ~ "Within commune",
      term %in% c("v_between", "any_between") ~ "Between communes"),
    part  = factor(part, c("Region-wave", "Within commune", "Between communes")),
    model = factor(model, unique(model))
  ) |>
  dplyr::arrange(model, part)

# Rows used in the text
main_within  <- results |> dplyr::filter(model == "M4", part == "Within commune")
main_between <- results |> dplyr::filter(model == "M4", part == "Between communes")
m1_effect    <- results |> dplyr::filter(model == "M1")
se_ratio     <- results$std.error[results$model == "M4" & results$term == "v_within"] /
                results$std.error[results$model == "M2" & results$term == "v_within"]

# ---- 7. Leave one region out (M4 within effect) ---------------------------------------
# With 8 regions, one region could drive the result. Refit M4 without each one.
fit_without_region <- function(r) {
  d = ad |> dplyr::filter(region != r) |> droplevels()
  des = survey::svydesign(ids = ~commune_w, strata = ~strata_w,
                          weights = ~wt_scaled, nest = TRUE, data = d)
  m = survey::svyglm(f_commune, design = des, family = quasibinomial())
  marginaleffects::avg_slopes(m, variables = "v_within", newdata = d, wts = "wt_scaled") |>
    tibble::as_tibble() |>
    dplyr::mutate(dropped = r)
}

loo <- levels(ad$region) |>
  purrr::map(fit_without_region) |>
  purrr::list_rbind()

# ---- 8. Model 3 checks ----------------------------------------------------------------
m3_singular <- lme4::isSingular(m3)
m3_messages <- m3@optinfo$conv$lme4$messages     # NULL = converged cleanly
tau2        <- as.numeric(lme4::VarCorr(m3)$commune_key)
icc         <- tau2 / (tau2 + pi^2 / 3)
u_hat       <- lme4::ranef(m3)$commune_key |>
  tibble::as_tibble(rownames = "commune_key") |>
  dplyr::rename(u = `(Intercept)`)

# ---- 9. Quick look when run in the console -----------------------------------------------
print(results, width = Inf)
