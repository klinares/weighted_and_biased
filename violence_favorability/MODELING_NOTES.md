# Modeling Notes: Violence Against Civilians and Leader Favorability (Mali)

**Purpose of this file.** A complete reference for this project: the study, the design, every modeling decision and why it was made, how to read the results, how the code is organized, and how to troubleshoot it. Give it to an AI assistant as context before asking for help. Section 13 tells the assistant how to behave.

---

## 1. The study

**Question.** Is violence against civilians in a respondent's commune associated with how favorably they view the national leader, comparing people in the same region and survey wave?

**Audience.** The client explains results to policymakers. Results are reported as percentages and percentage points, and maps must be correct and unambiguous.

**Data**
- Three survey waves, each an independent sample, about 140 communes per wave with many communes repeated across waves.
- Outcome `fav`: 1 = favorable view of the leader, 0 = not (a native yes/no item). Favorability is high, around 80% in every wave, so the violence association is expected to be modest.
- Exposure: ACLED events of **violence against civilians** (events, not fatalities) in the commune during the same window as each wave's fieldwork. Already merged into the survey; only surveyed communes have counts.
- Geography: region (stratum), commune, village. `adm1_key` and `adm3_key` are copies of region and commune that match the OCHA boundary keys; they are used for the design and the maps. The original `region` and `commune` columns are used in the models.
- No attitude items are used. No external data (population, coordinates) can be added.

---

## 2. Survey design

**Working assumption: a two-stage design within each wave.** Strata: regions. Stage 1: communes. Stage 2: villages. Respondents within villages.

**How the code implements it** (`R/prep_data.R`, `R/models.R`):
1. Wave-specific codes, because each wave is its own design: `strata_w = paste(wave, adm1_key)`, `psu_w = paste(wave, adm1_key, adm3_key)`, `village_w = paste(wave, adm1_key, adm3_key, village)`.
2. Weights rescaled within wave: `wt_scaled = weight * n_w / sum(weight)`.
3. `svydesign(ids = ~psu_w + village_w, strata = ~strata_w, weights = ~wt_scaled, nest = TRUE)`.
4. `options(survey.lonely.psu = "adjust")`.

Standard errors come from Taylor linearization at the first stage (communes), which also covers clustering of villages and respondents within communes. This matters because violence is measured at the commune.

**Never add a random effect for PSUs to a design-based model**; the design already handles that clustering. The multilevel models (M1 to M3) and the design-based model (M4) are separate, parallel approaches to the same clustering.

---

## 3. Files and run order

| File | Role | Edit? |
|---|---|---|
| `R/prep_data.R` | Reads the survey, maps column names, builds violence and design variables | **Yes: file path and column names** |
| `R/boundaries.R` | Name-cleaning helpers and boundary readers | Rarely (`region_alias`) |
| `R/models.R` | M1 to M4, the LRT, design cost, effects, predictions | To change models |
| `R/maps.R` | Violence map and M3 predicted-favorability map, saved to `output/maps/` | Rarely |
| `analysis.qmd` | Loads packages, sources the scripts, presents results (Typst PDF) | Wording and figures |
| `boundaries/` | OCHA map layers (gpkg) and admin dictionary | **Never** (the dictionary is used for matching at work) |
| `testing/` | Simulation with known answers and a full test run | Testing only; delete when satisfied |

Run order: edit `R/prep_data.R`, then render `analysis.qmd` (it sources `R/models.R` and `R/maps.R`). To debug, run `source("R/models.R")` in the console; every number in the report is an object it creates (`m1`, `m2a`, `m2b`, `lrt_slope`, `keep_slope`, `m3`, `m3_mw`, `m4`, `violence_rows`, `design_cost`, `ame`, `predicted`, `commune_pred`).

**Packages** (loaded in `analysis.qmd`): `lme4`, `survey`, `marginaleffects`, `performance`, `jtools`, `huxtable`, `broom.mixed`, `sf`, `viridis`, `patchwork`, `knitr`, `glue`, `tidyverse`. The scripts use `package::function()` calls, so they also run on their own. `sampling` is used only by the simulation. Nothing connects to the internet.

---

## 4. Data contract (`R/prep_data.R`, section 2)

| Standard name | Meaning |
|---|---|
| `wave` | 1, 2, 3 (wave 1 = reference) |
| `region`, `commune` | names as in the survey (model dummies and random effect) |
| `village` | village name or code (second-stage design unit) |
| `adm1_key`, `adm3_key` | region and commune keys matching the OCHA boundaries (design and maps) |
| `weight` | final weight for that wave |
| `fav` | outcome, 0/1 |
| `events` | ACLED violence-against-civilians events, commune-wave, survey window |
| `female`, `age`, `ethnicity`, `education`, `urban` | respondent covariates |

`prep_data.R` cleans the keys the same way as the boundary file (idempotent if already clean), builds `commune_id = region | commune` (commune names repeat across regions), and warns if the number of communes by name and by key differ (one commune spelled two ways would become two random effects).

---

## 5. Violence measure

$V_{cw} = \log(1 + E_{cw})$. Zero events gives 0. A one-unit increase multiplies $(1 + E)$ by $e \approx 2.72$; $+0.69$ doubles it. For large counts, doubling $(1 + E)$ is close to doubling $E$; not for small counts (0 to 1 event is a doubling of $1 + E$). Results are therefore also shown as predicted favorability at 0, 1, 5, and 20 events.

**Baseline versus change.** Differencing violence from wave 1 was rejected: it drops every commune not surveyed in wave 1, uses only the small within-commune variation (about 6% in the real data), and the outcome (different respondents each wave) is not differenced. Instead, the Mundlak split (sensitivity model M3-MW): $\bar V_c$ = mean of $V_{cw}$ over the waves in which commune $c$ was surveyed (each wave once); within = $V_{cw} - \bar V_c$ (0 for communes surveyed once).

---

## 6. Models (`R/models.R`)

Notation: respondent $i$, commune $c$, wave $w$; $\mathbf{x}_i$ = female, age per decade centered, ethnicity (most common = reference), education, urban.

| Model | $\operatorname{logit}\Pr(y_{icw}=1)$ | Role |
|---|---|---|
| M1 | $\gamma_{00} + u_{0c}$ | ICC $= \tau_{00}/(\tau_{00} + \pi^2/3)$ |
| M2a | $\gamma_{00} + \gamma_{10}V_{cw} + u_{0c}$ | Reduced model for the slope test |
| M2b | M2a $+ u_{1c}V_{cw}$, $(u_{0c}, u_{1c}) \sim N(\mathbf{0}, \mathbf{T})$ | Random violence slope |
| M3a | M2 winner $+ \sum_{r \ne \text{Bamako}}\alpha_r \text{Region}_r + \gamma_2 \text{Wave}_2 + \gamma_3 \text{Wave}_3 + \mathbf{x}_i'\boldsymbol\beta$ | Region and wave dummies, covariates |
| M3b | M3a $+ v_{rw}$, $v_{rw} \sim N(0, \tau_{rw})$ | Region-by-wave shocks, partially pooled |
| **M4** | M3 winner $+ \delta \log\tilde w_i + w_{vc}$, $w_{vc} \sim N(0, \tau_v)$ | **Final model**: design in the model |
| M3-MW | M3 random part with $\gamma_W(V_{cw} - \bar V_c) + \gamma_B \bar V_c$ | Sensitivity: usual level vs change |
| Design-based check | M3 fixed effects, `svyglm`, weights, strata, PSUs | Comparison only (population-averaged) |

All multilevel models: `glmer`, ML (Laplace), `bobyqa`, unweighted, same respondents.

**Two pre-specified tests** (helper `calculate_lrt_pvalue_fun(full, reduced, df)`, boundary mixture $0.5\chi^2_{df_1} + 0.5\chi^2_{df_2}$):
- Random slope, M2b vs M2a, `df = c(1, 2)` (one variance and one covariance). Keep only if $p < .05$ and M2b is not singular. If kept, the slope is carried into M3a, M3b, and M4. Expect low power: a commune-level exposure's slope is identified only by within-commune change across waves.
- Region-by-wave intercept, M3b vs M3a, `df = c(0, 1)` (one variance). M3b proceeds only if $p < .05$, its AIC is lower, and it is not singular.

**Why region and wave stay as dummies.** Three waves are too few to estimate a variance; regions are the strata (all included, not sampled); and a region random intercept assumes region effects are unrelated to violence, which fails here (northern regions have more violence and lower favorability), so between-region confounding would leak into the violence coefficient (Bell & Jones, 2015). The region-by-wave random intercept (24 cells) gives partial pooling where it helps, on top of the dummies.

**Why M4 puts the design in the model.** `svyglm` cannot hold random effects, and `glmer` cannot use sampling weights (its `weights` argument means binomial trials or precision). So the design enters the multilevel model: strata are the region dummies, the first stage (communes) is a random intercept, M4 adds the second stage (villages within communes) and the centered log weight. A clearly nonzero log-weight coefficient signals informative sampling (Pfeffermann et al., 1998; Gelman, 2007). The `svyglm` fit is kept as a design-based check, compared by $z$ because its coefficient is population-averaged (smaller by about $\sqrt{1 + 0.346\,\tau_{00}}$; Zeger, Liang & Albert, 1988).

**Reported effects.**
- AME: `avg_slopes(m4, variables = "v_commune", newdata = ad, re.form = NA)` (typical commune, village, region-wave).
- Predicted favorability at 0, 1, 5, 20 events: `avg_predictions(..., variables = list(v_commune = log1p(c(0, 1, 5, 20))))`, from M4 with `re.form = NA`.
- Map values: `predict(m4, type = "response")` per respondent (includes all of its random effects), weighted mean within commune-wave. Shrinkage pulls communes with few respondents toward the overall level.
- Odds ratios: $\exp(\gamma_{10})$ per unit of $V$; $\exp(\gamma_{10}\log 2)$ per doubling of $(1 + E)$.

---

## 7. Reading the results

| Output | What it says |
|---|---|
| M1 ICC | Share of latent variance between communes; justifies the commune level |
| LRT table | Whether the violence effect varies across communes (low power, see above) |
| M3 table | Violence, region (vs Bamako), wave (vs wave 1), covariates, log-odds with 95% CI |
| Residual ICC | Between-commune variance left after the predictors |
| M3 vs M4 table | Design-based estimate and the variance cost of the design |
| Predicted table and curve | **Headline**: percent favorable at 0, 1, 5, 20 events |
| M3-MW | Usual level (between) vs change (within); similar values support the single coefficient |
| compare_performance | AIC, BIC, ICC for M1 to M3 |
| VIF table | Collinearity among M3 predictors |
| Binned residuals | Points outside the bounds or a curve suggest a missing nonlinearity |
| QQ of commune effects | Heavy tails suggest outlying communes |
| Maps | Violence by commune-wave; M3 predicted favorability by commune-wave; gray = not surveyed |

**Plain-language template**

> Among people surveyed in the same region and survey round, those living in communes with more violence against civilians were less likely to view the leader favorably. If every commune had experienced no such events, about X% would be expected to hold a favorable view; with 5 events, about Y%. This mostly compares different communes, so it shows an association, not proof that violence caused the change.

**Do not**
- call the result causal;
- show odds ratios to non-technical customers (they are read as probabilities);
- compare M3 and M4 by AIC or likelihood ratio tests;
- interpret one commune on the map;
- read a non-significant random slope test as "the effect is the same everywhere".

---

## 8. Pre-specification (the subtle-effect problem)

With favorability near 80%, the association is small on the probability scale ($p(1-p) \approx 0.16$). The temptation is to try exposure forms, windows, subsets, and interactions until something is significant. That invalidates the p-values. Fixed in advance: exposure $\log(1 + E)$ in the survey window; model ladder M1 to M4; the slope decision rule; sensitivity M3-MW. Report all of them regardless of results. Any added specification must be labeled exploratory.

---

## 9. Maps (`R/maps.R`)

- Boundaries: OCHA COD-AB for Mali, DNCT release of 10 November 2021 (701 communes, 53 cercles, 10 regions). Full citation in `boundaries/SOURCE.md`; credit line on every map.
- Join: survey `adm1_key + adm3_key` to the boundary keys (Ménaka is filed under Gao in the boundary keys). No matching step.
- Shared names: Benkadi (Koulikoro, 3 communes), Somo (Ségou, 2), Kapala (Sikasso, 2) cannot be placed by key and are left uncolored; the caption counts every survey commune not placed.
- Classes: violence 0, 1-2, 3-9, 10-29, 30 or more; predicted favorability under 70%, 70-75, 75-80, 80-85, 85-90, 90% or more (bins narrowed because favorability is high). Gray = not surveyed (missing, not zero).
- Saved as PDF (vector) and PNG (300 dpi) in `output/maps/`.

---

## 10. Decisions log

| Considered | Decision | Reason |
|---|---|---|
| Region-wave violence | Dropped | Region and wave dummies absorbed most of its variation |
| Name matching and diagnostics scripts | Removed | Keys matched deterministically at work; diagnostics confirmed |
| Random slope without a test | No | LRT with boundary mixture, rule fixed in advance |
| Weights in `glmer` | No | `lme4` weights are not sampling weights for binomial models |
| `WeMix` weighted multilevel model | Not used | Needs stage-specific conditional weights not available |
| GEE | Dropped | Ignores strata; `svyglm` already gives the design-based population-averaged estimate |
| Parallel fitting (`furrr`) | Dropped | Models take seconds; M3 depends on the M2 test |
| Region and wave as random intercepts (no dummies) | Rejected | 3 waves too few; regions are strata; region effects correlate with violence |
| Region-by-wave random intercept | M3b, kept if the test supports it | Partial pooling of region-wave shocks on top of the dummies |
| `svyglm` as the final model | Design-based check only | Cannot hold random effects; design enters M4 as village random intercept + log weight |
| Differencing violence from wave 1 | Rejected | Loses communes, uses only within variation, outcome not differenced |
| Mundlak within/between | Sensitivity model | Separates usual level from change without dropping communes |
| Attitude covariates | Not used | None in the data; possible mediators |
| Ordinal model | Not needed | Outcome is natively binary |
| Bayesian models, machine learning | Rejected earlier | No design-based inference; question is an effect, not prediction |
| Admin dictionary edits (Excel export) | Not done | Dictionary is used for matching at work; left untouched |

---

## 11. Validation performed (simulated data, `testing/`)

Simulation: real Mali communes; a PPS panel of 174 communes, 137 interviewed per wave; 3 villages per commune; about 5,900 respondents; 76% favorable; within-commune share of violence variance 10%. Truth: violence -0.25, commune variance 0.25, no random slope, village variance 0.04, region-wave variance 0.09, weights unrelated to favorability.

| Check | Result (identical in the workspace and Kevin's RStudio) |
|---|---|
| Random slope | M2b singular, LRT 0.02, p = 0.93, not kept (correct) |
| Region-by-wave | LRT 10.06, p < .001, lower AIC, M3b kept (correct); variance 0.03 (true 0.09: underestimated, 24 cells) |
| M4 violence | -0.303 (SE 0.047); truth inside 95% CI |
| M4 village variance | 0.046 (true 0.04) |
| Log-weight coefficient | -0.001, p = 0.97 (correct: no informative sampling) |
| Design-based check | -0.254 (SE 0.053), z = -4.8 vs -6.5 for M4 |
| Predicted favorability (M4) | 82.3%, 79.1%, 73.2%, 65.4% at 0, 1, 5, 20 events |

Rerun `testing/run_full_test.R` after any package upgrade; results go to `testing/test_results.txt`.

---

## 12. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| Warning "Communes by name ... by key" | A commune spelled two ways across waves | Fix the spelling in the survey or use `adm3_key` in `commune_id` |
| `boundary (singular) fit` for M2b | Slope variance at zero | Expected; the rule keeps the random intercept |
| Convergence warning in `glmer` | Optimizer stopped early | Read `m3@optinfo$conv$lme4$messages` (NULL = fine); try `optimizer = "Nelder_Mead"` |
| `performance::check_convergence()` returns FALSE with gradient NA | lme4 2.0 no longer stores the derivatives it needs; not a real warning | Use lme4's own messages (as `analysis.qmd` does) |
| `summ()` error "incorrect number of dimensions" for M1 | jtools 2.3 cannot compute intervals for an intercept-only `glmer` | `confint = FALSE` for M1 (as `analysis.qmd` does) |
| Render log shows "Running juice failed ... deno.lock" | Quarto cannot write a lock file in the RStudio install folder | Harmless; the PDF is still created |
| `object 'd' not found` in `avg_slopes()` | Arguments forwarded through `...` | Call `marginaleffects` directly with named arguments |
| Warning "training data could not be extracted reliably" | `marginaleffects` cannot find the `svyglm` data; results unaffected because `newdata` is supplied | Ignore |
| `could not find function "%||%"` | R older than 4.4 with a recent package | Update R |
| Error about a stratum with one PSU | Subsetting left one commune in a stratum | `options(survey.lonely.psu = "adjust")` (set in `models.R`) |
| Map communes all gray | Keys do not match the boundary keys | Compare `adm1_key`, `adm3_key` with `read_mali_boundaries("adm3")` |
| jtools table renders as raw text | Output format not detected | Tables go through `huxtable::to_md()` in `md_summs()` |

---

## 13. Instructions for AI assistants

You are assisting a survey methodologist trained in the Total Survey Error and Total Error Framework traditions. When working on this project:

1. **Keep the estimand.** Main quantity: the M4 violence coefficient (typical commune, design in the model), with the `svyglm` check alongside. Do not change the exposure, the model ladder, the slope rule, the clustering level, or the dummies without saying so and explaining the consequence.
2. **Never pass sampling weights to `glmer`** and never add random effects to `svyglm`. The design enters M4 as region dummies (strata), commune and village random intercepts (stages), and the log weight.
3. **Each wave is its own design**: wave-specific codes and within-wave weight rescaling stay.
4. **Never compare `svyglm` with `glmer` by AIC, BIC, or likelihood ratio tests.** Compare multilevel models with each other only when fit by ML on the same respondents.
5. **Report in percentage points and predicted percentages** for non-technical audiences.
6. **Protect the maps**: never place a commune whose key is shared within a region; keep "not surveyed" distinct from zero.
7. **Do not touch `boundaries/mali_admin_dictionary.csv`.**
8. **Keep the code simple and readable**: explicit model calls, `package::function()`, comments that explain why, no `for` loops (use `purrr`), native pipe, `=` inside function bodies, `pacman::p_load()` with `tidyverse` last, viridis palettes, no em-dashes in prose.
9. **Be honest about uncertainty.** Separate settled practice, defensible choices, and your own guesses. Never invent function arguments, package behavior, citations, or results. Check numbers by running code.
10. **Think in total error**: coverage (insecure areas), sampling (two-stage design, unweighted multilevel models), measurement (ACLED undercounting, sensitive outcome), processing (key joins, merges).
11. **Respect the pre-specification** in section 8.

---

## 14. References

- Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects modeling of time-series cross-sectional and panel data. *Political Science Research and Methods, 3*(1), 133-153.
- Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys. *International Statistical Review, 51*(3), 279-292.
- Lumley, T. (2010). *Complex surveys: A guide to analysis using R*. Wiley.
- Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica, 46*(1), 69-85.
- OCHA. Mali: Subnational administrative boundaries (COD-AB), levels 0 to 3. Source: Direction Nationale des Collectivités Territoriales (DNCT), 2021. Humanitarian Data Exchange.
- Raleigh, C., Linke, A., Hegre, H., & Karlsen, J. (2010). Introducing ACLED: An armed conflict location and event dataset. *Journal of Peace Research, 47*(5), 651-660.
- Gelman, A. (2007). Struggles with survey weighting and regression modeling. *Statistical Science, 22*(2), 153-164.
- Pfeffermann, D., Skinner, C. J., Holmes, D. J., Goldstein, H., & Rasbash, J. (1998). Weighting for unequal selection probabilities in multilevel models. *JRSS B, 60*(1), 23-40.
- Zeger, S. L., Liang, K.-Y., & Albert, P. S. (1988). Models for longitudinal data: A generalized estimating equation approach. *Biometrics, 44*(4), 1049-1060.
