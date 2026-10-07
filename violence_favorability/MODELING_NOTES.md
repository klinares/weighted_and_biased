# Modeling Notes: Violence and Leader Favorability (Mali)

Reference for this project's models: what was decided, why, how to read the output, and how to work on the code. Give this file to an AI assistant as context (section 9 tells it how to behave).

---

## 1. Study and data

**Question.** Is violence against civilians in a respondent's cercle associated with favorable views of the national leader, comparing people in the same region, urban or rural area, and survey wave?

**Data.**
- Three survey waves, each an independent sample. Strata: region x urban (15; Bamako is urban only). Communes are the first and only sampling stage; every commune has 8 respondents. Weights exist only at the respondent level (no commune selection weights).
- Outcome: favorable view (1/0). No item nonresponse in the file; about 80% favorable.
- Exposure: ACLED event counts per **cercle** and wave over the survey window, `violence_civilian` (main) and `violence_all`. Each count is a sum over the communes surveyed in that cercle and wave, so it grows with the number of surveyed communes; the pipeline divides by that number. Only surveyed cercles have counts. No perpetrator field.
- Geography: region, cercle, commune. About 140 communes per wave, many repeated across waves; about 136 cercle-waves in total.

**Status on the real data (as reported by Kevin, commune-level version).** ICC about 17% with commune and cercle intercepts; violence coefficient about -0.10, p = 0.60; models with a commune intercept had convergence problems. After learning that violence is measured per cercle, the models were rebuilt at the cercle level (below). Real-data results for the cercle version are not yet in these notes.

---

## 2. Files and workflow

| File | Role |
|---|---|
| `analysis.qmd` | **Config list `cfg`** (data file, violence count, covariate sets for M2a and M2b, `final`, `cercle_wave` switch, event levels, model folder, `refit`), loads packages, sources the scripts, presents results |
| `R/prep_data.R` | Builds `ad`: IDs, events per surveyed commune, `v = log1p(events per commune)`, usual-level/change split, wave-specific design codes, rescaled and log weights |
| `R/models.R` | M1a, M1b, M2a, M2b, M3, svyglm check, sensitivity split, tests, predictions, calibration; saves models to `models/` |
| `R/maps.R` | Violence per surveyed commune by cercle (one map); M3 predicted favorability by cercle and wave; saved to `output/maps/` |
| `R/boundaries.R` | Name cleaning and boundary readers |
| `boundaries/` | OCHA layers and admin dictionary. **Never edit the dictionary** (used for matching at work) |

**Data contract (prepared upstream).** Columns named exactly: `wave`, `region`, `urban`, `commune`, `cercle`, `fav` (1/0), `weight`, `adm1_key`, `adm3_key`, the cercle-wave violence count named in `cfg$violence` (the same value for everyone in a cercle and wave), and the covariates in `cfg$covariates` and `cfg$covariates_m2b`. Factors arrive with their reference level first (region: Bamako; wave: 1); age is grouped. Cercle names must match the OCHA cercle names (accents and case do not matter). Rows missing any variable in either covariate set are dropped, so all models use the same respondents.

**Choosing the structure.** Render once and read the model comparison table. (1) M1: keep the cercle-by-wave intercept (`cfg$cercle_wave = TRUE`) if the boundary-corrected p < .05, AIC is lower, and the fit is not singular; otherwise set it to FALSE and keep only the cercle intercept. (2) M2: compare M2a and M2b by AIC and BIC (and the likelihood ratio test when one covariate set contains the other) and set `cfg$final` to the chosen one. Render again with `refit = TRUE`. Choose the covariate set before looking at the violence coefficient, and report both rows.

**Model caching.** `fit_glmer()` loads `models/<name>.rds` if it exists, otherwise fits and saves it. It does **not** notice changed data or formulas: set `cfg$refit = TRUE` (or delete `models/`) after changing the data, covariates, or switches, otherwise the report will use stale models.

**Packages.** `lme4`, `survey`, `marginaleffects`, `performance`, `broom.mixed`, `modelsummary`, `tinytable`, `sf`, `viridis`, `patchwork`, `knitr`, `tidyverse`. Main-text tables use `knitr::kable`; the appendix uses `modelsummary` + `tinytable` (native Typst). Typst tables cannot split across pages, so long tables are kept to one row per term.

## 3. Measures

- $V_{kw} = \log(1 + E_{kw} / n_{kw})$, where $E_{kw}$ is the cercle-wave count and $n_{kw}$ the number of communes surveyed there that wave. Without the division a cercle with more surveyed communes would look more violent. 0 events -> 0; +0.69 doubles $(1 + E/n)$. Report predicted favorability at 0, 1, 5, 20 events per surveyed commune.
- **Usual level and change (sensitivity, Mundlak).** $\bar V_k$ = mean of $V_{kw}$ over the waves cercle $k$ was surveyed; change = $V_{kw} - \bar V_k$. The change coefficient compares a cercle with itself over time.
- **Population distributions** (region x urban): not used to scale violence; useful later for poststratified summaries of predicted favorability.
- **Civilian vs all violence.** Swap `cfg$violence` between `violence_civilian` and `violence_all` and refit. A ratio (civilian / all) was rejected: undefined at 0 events and unstable with few events.
- **Centering** (client question): region centering is what the region dummies already do; centering within cercle is what the change term of the sensitivity model does. Centering is not an alternative to the log; it changes which comparison the coefficient makes.

---

## 4. Models

All `lme4::glmer`, logit, ML (Laplace), `nloptwrap` with tight tolerances (about 10x faster than `bobyqa`, same log-likelihood), unweighted, same complete-case respondents.

| Model | Specification | Test |
|---|---|---|
| (reference) | $\gamma_0 + c_j$, commune intercept only | ICC for comparison with the old version |
| M1a | $\gamma_0 + u_k$ (cercle) | |
| M1b | $\gamma_0 + u_k + v_{kw}$ (cercle-by-wave) | `anova(m1a, m1b)`, p halved (boundary), AIC, BIC; keep if p < .05, lower AIC, not singular |
| M2a | M1 winner + $\beta V$ + wave + region + urban + `cfg$covariates` | |
| M2b | the same with `cfg$covariates_m2b` | AIC, BIC; `anova()` only if the sets are nested |
| M3 | the `cfg$final` model + centered log weight | final model |
| svyglm | M3's fixed effects without the log weight; strata = wave x region x urban, PSU = wave x commune, weights rescaled within wave | design-based check, compare by z |
| Split | M3 with $\bar V_k$ and $V_{kw} - \bar V_k$ | sensitivity |

**ICCs (latent scale, $\pi^2/3$ for the individual level).**
$\rho_{\text{cercle}} = \tau_k / (\tau_k + \tau_{kw} + \pi^2/3)$: same cercle, any wave.
$\rho_{\text{cercle-wave}} = (\tau_k + \tau_{kw}) / (\tau_k + \tau_{kw} + \pi^2/3)$: same cercle, same wave.

**Why these choices.**
- Cercle is the grouping level because violence is measured per cercle and wave. The cercle-by-wave intercept matters because everyone in a cercle-wave shares one violence value; without it the SE of $\beta$ would be too small.
- No commune intercept: violence does not vary within a cercle-wave, and with 8 respondents per commune the commune variance caused convergence problems. Communes remain the PSUs in the svyglm check, which carries the commune clustering into the design-based SE. On simulated data, adding the commune intercept back changed the violence estimate by less than 0.01.
- Region, urban, wave stay fixed: 3 waves cannot support a variance; region and urban are the design strata; a random region intercept assumes no correlation with violence, which fails (north: more violence, lower favorability) (Bell & Jones, 2015). The 15 strata are not used in the model; region + urban carry them.
- No random slope on violence (decided; it would be identified from only about 136 cercle-waves).
- Weights: `glmer` cannot take sampling weights (its `weights` argument means binomial trials). `WeMix` needs cluster selection weights, which are not available. So M3 includes the centered log weight as a covariate (Pfeffermann et al., 1998; Gelman, 2007), and `svyglm` is the design-based check.
- `svyglm` coefficients are population-averaged, smaller than conditional ones by about $\sqrt{1 + 0.346\,\tau}$ (Zeger, Liang & Albert, 1988): compare by z. Never compare it with `glmer` by AIC or likelihood ratio tests.
- M2a vs M2b is a covariate choice, not a test of violence. Fix the choice before reading $\beta$; report $\beta$ from both rows.

---

## 5. Reported quantities

- Violence coefficient, 95% CI, p.
- Predicted favorability at `cfg$event_levels`: `avg_predictions(..., variables = list(v = log1p(levels)))`.
- **0 vs 5 events** with 95% CI: `avg_comparisons(m3, variables = list(v = c(0, log1p(5))))`. This is the plain-language range.
- **Minimum detectable effect** (80% power, alpha .05): $2.8 \times SE(\beta)$, converted to percentage points with mean $\hat p(1-\hat p)$, times $\log 6$ for 0 vs 5 events. Approximate; tells clients what size of effect the study could have found.
- Predicted favorability by variable: every factor in M3 (wave, region, urban, factor covariates) by counterfactual (everyone set to each level, everything else as observed).
- Maps: (1) violence per surveyed commune averaged over the waves each cercle was surveyed, binned at 0 and the quartiles of the positive values (bins follow the data, so they change with `cfg$violence`); (2) M3 predicted favorability (`predict(m3, type = "response")`, with cercle and cercle-wave effects), weighted mean per cercle and wave. Predictions describe the surveyed communes of a cercle, not the whole cercle. Gray = not surveyed.

---

## 6. Reading a null result (the current real-data situation)

- p = 0.60 means the data cannot distinguish the association from zero. It does not show there is no effect.
- Lead with the 0 vs 5 events CI in percentage points ("effects larger than X points are unlikely") and the minimum detectable effect ("the study could reliably detect effects of about Y points or more").
- Near 80% favorable, the logistic slope is $p(1-p) \approx 0.16$ (vs 0.25 at 50%), so the same log-odds effect moves the percentage less.
- Likely reasons the signal is weak: violence varies mostly between places (few comparisons within a place over time); no perpetrator split (state vs armed-group violence may push in opposite directions); sensitive outcome under a military government (over-reporting of favorability compresses differences); ACLED undercounting in remote areas (attenuation); coverage of insecure areas.
- **Do not** search over exposure forms, windows, subsets, or interactions until something is significant. Any added specification must be pre-stated and labeled exploratory, and all versions reported.

**Enhancements still open (ask before adding):** neighboring-cercle violence from OCHA adjacency; poststratified summaries with the region x urban population distributions; one or two pre-specified interactions (for example violence x ethnicity, violence x north/center); a lagged event window if dates are available.

---

## 7. Diagnostics

| Check | Good | Warning |
|---|---|---|
| ICC | clearly above 0 | near 0 |
| Cercle-by-wave test | follows the rule | singular M1b: set `cercle_wave = FALSE` |
| Log-weight coefficient | |z| < 1.96 | clearly nonzero: informative sampling |
| svyglm z vs M3 z | similar | much weaker: weights or clustering matter |
| Usual level / change split | similar in sign | opposite signs |
| VIF | below about 2 | high |
| Calibration (simulation-based) | observed inside simulated range | many bins outside |
| Convergence | `m3@optinfo$conv$lme4$messages` is NULL | gradient warnings: check estimates are stable; compare with `optimizer = "bobyqa"` |

Binned residuals from `performance::binned_residuals()` were replaced: residuals built on shrunken random effects show a false slope even when the model is correct (verified on simulated data). The calibration check simulates 200 data sets from M3 with new cercle and cercle-wave effects.

---

## 8. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `Run the config chunk` error | Script sourced without the config | Run the config chunk or copy `cfg` into the console |
| Model refits every render | `refit = TRUE` left on | Set `refit = FALSE` after one refit |
| Survey cercles not placed on the map (caption count above 0) | Region or cercle name does not match OCHA | Check the cercle spelling upstream |
| `cfg$final must be "M2a" or "M2b"` | Typo in the config | Use one of the two |
| Report uses old results | Saved models reused | Set `cfg$refit = TRUE` |
| `boundary (singular) fit` | A variance is at zero | Report; the decision rule handles it |
| Gradient convergence warning | Small max|grad| above tolerance | Usually harmless if estimates match a refit with `optimizer = "Nelder_Mead"` |
| `performance::check_convergence()` FALSE with gradient NA | lme4 2.0 does not store derivatives | Use lme4 messages (as the report does) |
| Region shows as `S..gou` | Non-UTF-8 locale | Render on Windows/RStudio (UTF-8) |
| `could not find function "%||%"` | R older than 4.4 | Update R |
| Quarto "juice ... deno.lock" messages | Quarto cannot write a lock file in RStudio's folder | Harmless; the PDF is created |

---

## 9. Instructions for AI assistants

You are helping a survey methodologist (Total Survey Error tradition). On this project:

1. **Keep the estimand**: the M3 violence coefficient (typical cercle and wave), with the svyglm check. Do not change exposure, model sequence, decision rules, or fixed/random structure without stating the consequence.
2. **Never pass sampling weights to `glmer`**, never add random effects to `svyglm`, never compare them by AIC or likelihood ratio tests.
3. **Variance tests are boundary tests**: halve the `anova()` p-value when adding one variance.
4. **Report percentage points and predicted percentages** for clients; treat a non-significant result as a CI and a minimum detectable effect, not as "no effect".
5. **No specification searching.** New specifications are exploratory and reported in full.
6. **Protect the maps**: gray is "not surveyed", never zero; the violence count is always divided by the number of surveyed communes.
7. **Do not edit `boundaries/mali_admin_dictionary.csv`.**
8. **Code style**: tidyverse, native pipe, `package::function()`, no `for` loops (use `purrr`), `=` inside function bodies, single space after `<-`, comments that explain why in plain words, settings only in `cfg`.
9. **Be honest about uncertainty**: separate established practice, defensible choices, and guesses. Never invent function arguments, results, or citations; verify by running code.

---

## 10. Validation (simulated data)

The cercle-level pipeline was run on simulated data with the same layout (8 regions, 15 strata, 8 respondents per commune, about 136 cercle-waves). The cercle count is a sum of commune counts, and the truth acts on log(1 + count / communes surveyed). Truth: violence -0.25; cercle variance 0.20; commune variance 0.20 (not modeled); cercle-wave variance 0.06; uninformative weights.

Recovered: cercle-by-wave intercept kept (boundary chi2 = 24.9, p < .001); log weight near zero (0.025, SE 0.068); calibration inside the simulated range; M2a and M2b (without ethnicity) gave nearly identical violence estimates.

Violence: M3 estimate -0.405 (SE 0.116, 95% CI -0.632 to -0.178), so the truth -0.25 is inside the CI; the miss is about 1.3 SE. Also checked: the scaling in prep_data reproduces the simulation's exposure exactly, and adding a commune intercept changes the estimate by less than 0.01 (-0.408). This is one simulated data set, so it shows the pipeline runs and recovers the truth within sampling error; it is not a test of bias or CI coverage, which would need many simulation seeds. The design-based check gave -0.297 (population-averaged, so smaller by design).

`testing/simulate_data.R` was used for this and is not part of the delivered pipeline; `data/survey_sim.rds` is an example of the expected file layout.

## References

Bell & Jones (2015) *PSRM* 3(1):133-153. Binder (1983) *ISR* 51(3):279-292. Gelman (2007) *Statistical Science* 22(2):153-164. Mundlak (1978) *Econometrica* 46(1):69-85. OCHA COD-AB Mali, DNCT 2021, HDX. Pfeffermann, Skinner, Holmes, Goldstein & Rasbash (1998) *JRSS-B* 60(1):23-40. Raleigh, Linke, Hegre & Karlsen (2010) *JPR* 47(5):651-660. Zeger, Liang & Albert (1988) *Biometrics* 44(4):1049-1060.
