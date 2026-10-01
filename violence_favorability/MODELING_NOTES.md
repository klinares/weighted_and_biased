# Modeling Notes: Local Violence and Leader Favorability (Mali)

Reference for analysts and AI coding assistants working on this project. It records what the code does, why each modeling decision was made, how to read the results, and how to troubleshoot. Give this file to an AI tool as context before asking it to modify or debug the code.

---

## 1. Research question

Is violence in a respondent's commune associated with how favorably they view the national leader, after accounting for region, survey wave, and respondent characteristics?

- **Outcome:** `fav`, binary (1 = favorable view of the leader).
- **Exposure:** ACLED violent events per commune (admin-3) per survey wave.
- **Data:** three survey waves, each an independent stratified, clustered sample with its own weights.
- **Audience:** results must be explainable to non-technical customers, so effects are reported in percentage points.

---

## 2. Files and run order

| File | Role | Edit it? |
|---|---|---|
| `R/prep_data.R` | Reads both files, maps column names, matches communes by name, builds exposures and design columns | **Yes: the only file to edit for new data** |
| `diagnostics.R` | Name-matching checks and the K, A, P, W diagnostics | Thresholds only |
| `models.R` | Fits Models 1 to 4 and the sensitivity models, computes every AME, leave-one-region-out, Model 3 checks. Plain top-to-bottom script for line-by-line debugging | To change models |
| `maps.R` | Builds the map data and the `draw_map()` helper | Rarely |
| `analysis.qmd` | Presents results only: tables, figures, maps, text; renders to PDF (Typst) | Wording and figures |
| `R/boundaries.R` | Map helpers: `clean_name()`, `region_key()`, `region_alias`, readers for the boundary files | Add region spellings to `region_alias` |
| `make_boundaries.R` | One-time build of the boundary files from raw geoBoundaries GeoJSON | No |
| `boundaries/mali_boundaries.gpkg` | Map layers adm0 to adm3 | No |
| `boundaries/mali_admin_dictionary.csv` | One row per commune with its cercle and region | Look up spellings here |
| `simulate_data.R` | Simulated test data with a known violence effect | Only for testing |

**Run order:**
1. `source("diagnostics.R")` and read both printed tables.
2. Fix any name problems in `R/prep_data.R` and repeat step 1.
3. `source("models.R")` (or step through it line by line) and check the printed `results` table.
4. Render `analysis.qmd`.

Dependency chain: `analysis.qmd` sources `models.R` and `maps.R`; `models.R` sources `diagnostics.R`, which sources `R/prep_data.R`, which sources `R/boundaries.R`.

**Debugging tip:** every number in the report is an object created in `models.R` or `maps.R` (`results`, `main_within`, `loo`, `u_hat`, `map_fav`, ...). If the report fails, run those scripts in the console first; the error will point to a specific line instead of a chunk.

**Packages:** `survey`, `lme4`, `marginaleffects`, `sf` (maps), `patchwork`, `viridis`, `knitr`, `glue`, `pacman`, `tidyverse`. No internet access is needed at run time.

---

## 3. Data contract

### Survey file (one row per respondent)

| Standard name | Meaning |
|---|---|
| `wave` | 1, 2, 3 |
| `strata` | design stratum for that wave (region) |
| `psu` | PSU code for that wave |
| `wt` | final respondent weight for that wave |
| `region`, `commune` | names; `cercle` optional (see section 6) |
| `fav` | outcome, 0/1 |
| `female`, `age`, `ethnicity`, `education`, `urban` | respondent covariates |
| `local_gov`, `democracy` | attitudes (sensitivity model only) |
| `perceived`, `stress` | possible mediators (sensitivity model only) |

### Violence file (one row per commune and wave, or per event)

| Standard name | Meaning |
|---|---|
| `wave`, `region`, `commune` | names; `cercle` optional |
| `events` | event count (set to 1 if the file has one row per event) |

Column mapping happens in `dplyr::transmute()` blocks in `R/prep_data.R`. The left side is the standard name used everywhere else; the right side is the real column name.

---

## 4. Survey design

**Each wave is its own design.** Strata and PSUs are drawn separately in each wave, and weights sum to each wave's own population total. The code therefore:

1. Makes design codes wave-specific: `strata_w = paste(wave, strata)`, `psu_w = paste(wave, psu)`, `commune_w = paste(wave, commune_key)`. Codes never repeat across waves.
2. Rescales weights within wave so each sums to that wave's sample size: `wt_scaled = wt * n_w / sum(wt)`. Without this, the wave with the largest population total dominates.
3. Builds one pooled `svydesign(..., nest = TRUE)`, which is the three designs stacked side by side.
4. Sets `options(survey.lonely.psu = "adjust")` for strata left with a single cluster after subsetting.

**Two clustering levels, same data:**

| Design object | `ids =` | Used by | Why |
|---|---|---|---|
| `des_psu` | `~psu_w` | Model 1 | The original specification |
| `des_commune` | `~commune_w` | Model 4 | Violence is assigned at the commune, so everyone in a commune-wave shares one value. Cluster at the level where exposure is assigned. |

Clustering at the commune is conservative relative to the PSU: if a commune contains several PSUs, commune clustering treats them as one cluster. Diagnostic P (section 7) reports how often this happens.

**Do not add a PSU random effect to a model that already uses the survey design.** The design already accounts for PSU clustering through the standard errors; adding a random effect counts that clustering twice.

---

## 5. Exposure construction

| Variable | Formula | Used by |
|---|---|---|
| `events` | sum of commune events in that wave; **0 if the commune has no row in the violence file** | all |
| `v_region` | `log1p(sum of events over ALL communes in the region, that wave)` | Model 1 |
| `v_commune` | `log1p(events)` | building blocks |
| `v_between` | mean of `v_commune` over the waves in which the commune was **surveyed**, each wave counted once | Models 2 to 4 |
| `v_within` | `v_commune - v_between` | Models 2 to 4 (main quantity) |
| `any_event`, `any_between`, `any_within` | same construction for "any event versus none" | binary sensitivity |

**Why `log1p`:** event counts are heavily right-skewed with many zeros. The log compresses large counts; `+1` keeps zeros defined. One unit of `log1p` is about a 2.7-fold change in events; about 0.69 units is a doubling.

**Why within/between (Mundlak):** violence was requested to be mean-centered at the commune level. Centering alone would discard the between-commune comparison, so both parts are kept:
- `v_within` asks: when a commune is more violent than its own usual level, is favorability lower? Stable commune features cannot drive this.
- `v_between` asks: are people in usually more violent communes less favorable? This compares different communes and is more exposed to confounding.

**A commune surveyed in only one wave has `v_within = 0`** and contributes nothing to the within estimate. Diagnostic W reports how much within variation exists.

The commune mean uses only waves in which the commune was surveyed, not all three waves of violence data. Using unsurveyed waves would make `v_within` nonzero for single-wave communes without any repeated favorability measurement, which is not a within-commune comparison.

---

## 6. Commune name matching

Communes are identified by **name**, not ID. The key is built by `make_key()` in `R/prep_data.R`:

- Each part is normalized with `clean_name()`: accents removed, lower case, spaces and punctuation stripped. "Ségou" and "SEGOU" both become `segou`.
- Region names first pass through `region_alias` in `R/boundaries.R` (for example, the boundary file spells "Koulikouro"; it is mapped to "Koulikoro").
- Parts are joined with `|`, for example `segou|markala`.
- `key_cols` sets which parts are used. Default: `c("region", "commune")`.

**Why region is included:** commune names repeat across Mali (13 distinct names are shared by two or more communes in the official list). Within a region, four still repeat (Benkadi three times in Koulikoro; Diedougou in Koulikoro; Somo in Segou; Kapala in Sikasso). All names are unique within region plus cercle.

**If diagnostics report ambiguous names** and both files have a cercle column, set `key_cols <- c("region", "cercle", "commune")`. In the simulation this resolved all four, and two communes that had been picking up their namesakes' events were corrected.

**Silent failure to watch for:** a sampled commune with no match in the violence file is assigned 0 events. A misspelling therefore looks like a peaceful commune. `diagnostics.R` compares both files against the official commune list (`boundaries/mali_admin_dictionary.csv`) to separate spelling problems from true zeros.

**Dictionary caveat:** the commune-cercle-region links in the dictionary are not an official code list. `make_boundaries.R` assigned each unit to the parent polygon it overlaps most. Counts match Mali's known structure (701 communes, 50 cercles including Bamako, 9 regions under the older regional structure). Newer regions (Ménaka, Taoudénit, and the later reorganization) are not represented.

---

## 7. Diagnostics: K, A, P, W

Computed in `diagnostics.R` before any model is fit, so the specification is chosen from the design rather than from the results.

| | Definition | Threshold (editable) | If it fails |
|---|---|---|---|
| **K** | Commune-wave cells in the analysis sample (share with at least one event) | at least 100 | Little gain over region-wave exposure |
| **A, region** | R-squared of `v_region ~ region + wave` across region-wave cells | above 90% = problem | Expected to fail: this is the evidence that Model 1 cannot answer the question |
| **A, commune** | R-squared of `v_commune ~ region + wave` across commune-wave cells | above 90% = problem | Commune exposure is still collinear with the dummies; report descriptively only |
| **P** | Median PSUs per commune-wave cell (share of cells with more than one) | above 1 | Confirms commune-level clustering in Model 4 is needed |
| **W** | `var(v_within) / var(v_commune)` across cells; plus communes surveyed 2+ waves and their share of respondents | at least 10% | Within estimate will be very wide; lead with the between estimate and describe it as cross-sectional |

**Why A matters:** whatever share of exposure variation the region and wave dummies explain cannot inform the violence coefficient. With A = 98%, Model 1 estimates the violence effect from 2% of the variation spread over 24 values (about 14 residual degrees of freedom).

**Simulation results** (for reference): K = 687 cells (41% with an event); A, region = 98%; A, commune = 9%; P = 1 (10% of cells have more than one PSU); W = 27% (205 communes surveyed more than once, 74% of respondents).

---

## 8. The four models

Each step changes **one** thing, so any difference between neighbouring models has a single explanation.

| Model | Exposure | Clustering | Weights | Estimator | Purpose |
|---|---|---|---|---|---|
| M1 | `v_region` | PSU (design) | Yes | `survey::svyglm` | Original specification; shows the identification problem |
| M2 | `v_within` + `v_between` | None | No | `glm` | Gain from finer exposure, before handling clustering |
| M3 | `v_within` + `v_between` | `(1 \| commune_key)` | No | `lme4::glmer` | Multilevel alternative |
| M4 | `v_within` + `v_between` | Commune (design) | Yes | `survey::svyglm` | **Main model** |

Sensitivity versions of M4: `M4 + attitudes` (adds `local_gov`, `democracy_c`, `perceived_c`, `stress_c`) and `M4, any event` (binary exposure).

### Equations

Shared base (region and wave dummies plus respondent covariates; Bamako and wave 1 are references):

$$
\eta_i = \beta_0 + \sum_{r \neq \text{Bamako}} \alpha_r \text{Region}_r + \sum_{w=2}^{3} \gamma_w \text{Wave}_w + \mathbf{x}_i'\boldsymbol\beta
$$

- M1: $\operatorname{logit}\Pr(y_i=1) = \eta_i + \beta_R V_{rw}$, with $V_{rw} = \log(1 + \sum_{c \in r} E_{cw})$
- M2, M4: $\operatorname{logit}\Pr(y_i=1) = \eta_i + \beta_W V^{\text{within}}_{cw} + \beta_B \bar V_c$
- M3: same as M2 plus $u_c$, with $u_c \sim N(0, \tau^2)$

with $V_{cw} = \log(1 + E_{cw})$, $\bar V_c = \frac{1}{T_c}\sum_{w \in S_c} V_{cw}$ ($S_c$ = waves in which commune $c$ was surveyed), and $V^{\text{within}}_{cw} = V_{cw} - \bar V_c$.

### Estimation details

- `svyglm` uses `family = quasibinomial()` (avoids warnings about non-integer weighted counts; estimates are identical to binomial). Variance by Taylor linearization.
- `glmer` uses the Laplace approximation and `optimizer = "bobyqa"`. It is unweighted.
- AIC, BIC, and likelihood ratio tests **cannot** compare `svyglm` with `glm` or `glmer`, because `svyglm` maximizes a weighted pseudo-likelihood. Compare models by their AMEs and standard errors only.

---

## 9. Reading the results

### Average marginal effect (AME)

The AME is the average change in the predicted probability of a favorable view, in **percentage points**, per one-unit increase in the exposure. Computed with `marginaleffects::avg_slopes()`:

| Model | Call |
|---|---|
| M1, M4 | `avg_slopes(m, variables = ..., newdata = ad, wts = "wt_scaled")` |
| M2 | `avg_slopes(m, variables = ...)` |
| M3 | `avg_slopes(m, variables = ..., re.form = NA)` |

"Per doubling" = AME times log(2), about 0.69. This is approximate because `log1p` is not exactly `log` at small counts.

### Conditional versus marginal (M3 versus M4)

`glmer` coefficients are commune-specific (conditional); `svyglm` coefficients are population-averaged (marginal). Under a logit link the marginal coefficient is attenuated relative to the conditional, roughly by a factor of $\sqrt{1 + 0.346\,\tau^2}$ (Zeger, Liang & Albert, 1988; approximate). The M3 AME with `re.form = NA` is evaluated at $u_c = 0$, a typical commune, not averaged over communes. With small $\tau^2$ (0.07 in the simulation) the difference is negligible, but **do not interpret small differences between M3 and M4 as substantive**.

### What each table and figure shows

| Item | Read it as |
|---|---|
| Name-matching table | Fix any nonzero "not in official list" rows before interpreting anything |
| Diagnostics table | A, region near 100% means Model 1 is uninformative; A, commune low means Models 2 to 4 have variation to work with |
| Spread figure | Communes within a region-wave differ widely; Model 1 gives them all one value |
| AME table and figure | Main result is M4 "Within commune". M2 vs M4 standard errors show the clustering correction |
| Prediction curve | Predicted favorability as a commune's violence moves above or below its own usual level |
| Violence map | Exposure by commune and wave (darker red = more events) |
| Model 4 map | Design-weighted percent favorable per commune-wave; few respondents each (median about 7), so read broad patterns only |
| Model 3 map | Commune random intercepts: green more favorable than predicted, purple less. Large same-colour clusters suggest a missing geographic factor |
| Leave one region out | If dropping one region moves the estimate a lot, that region drives the result |
| Binned residuals, calibration | Points outside bounds or a curve suggest a missing nonlinearity |

### Plain-language template for customers

> We compared the same communes across survey rounds. When a commune saw more violent events than it usually does, residents were about X percentage points less likely to view the leader favorably for each doubling of events, after accounting for region, survey round, and who was interviewed. The range of plausible values is L to U points.

### What not to say

- Do not call the result causal. Say "associated with."
- Do not present odds ratios to customers; they are routinely misread as probabilities.
- Do not interpret a single commune's map value; each rests on a handful of respondents.
- Do not present the M4 + attitudes row as the main estimate; violence may change those attitudes.
- Do not describe Model 1 as "wrong" on real data; describe it as **unable to distinguish an effect from no effect**, which is shown by diagnostic A before any outcome is examined.

### Simulation check

The simulated data set a true within effect of -0.30 on the logit scale. M2, M3, and M4 recover about -8 to -9 percentage points per unit of log events; M1 returns +6.0 (95% CI -4.0 to 16.0), the wrong sign with an interval spanning zero. M4's within-effect standard error is about 1.4 times M2's.

---

## 10. Decisions log

| Considered | Decision | Reason |
|---|---|---|
| Random effects for region or wave | Rejected | 8 regions and 3 waves are too few to estimate a variance; regions are the full set of strata, not a sample |
| Region-wave violence only (client's first specification) | Kept as M1 for comparison | Region and wave dummies absorb about 98% of its variation |
| PSU random effect plus survey design | Rejected | Counts PSU clustering twice |
| Bayesian model (`brms`) | Dropped | No design-based inference; adds complexity without fixing identification |
| Random forest, XGBoost, neural nets | Rejected as main analysis | Question is an effect, not prediction; no calibrated uncertainty; weights and clustering awkward; poor explainability. Optional bounded use: partial dependence of violence from one boosted model, as a functional-form check only |
| Double machine learning | Rejected | Defensible estimate but no clean survey-weight handling and hard to explain |
| Events per capita | Not used | Needs population denominators (external data); census figures are dated and displacement has shifted populations |
| Multilevel model with weights (`WeMix`) | Not used | Needs level-specific weights, which are not available |
| Binary versus continuous exposure | Both reported | Continuous is primary; binary is easier to explain and robust to skew |
| Centering violence at the commune | Done as within/between (Mundlak) | Keeps both the within and between comparisons |

---

## 11. Sensitivity checks and one-line changes

**Serial correlation across waves.** M4 clusters on commune within wave, so correlation of the same commune across waves is not reflected. Conservative check: cluster on commune across waves with region as the stratum.

```r
des_serial <- survey::svydesign(ids = ~commune_key, strata = ~region,
                                weights = ~wt_scaled, nest = TRUE, data = ad)
```

In the simulation the within-effect standard error barely changed (0.0247 versus 0.0244).

**Use cercle in the match key:** in `R/prep_data.R`, `key_cols <- c("region", "cercle", "commune")`.

**Change the reference region:** in `R/prep_data.R`, `forcats::fct_relevel(factor(region), "Bamako")`.

**Add a covariate:** add it to the survey `transmute()` in `R/prep_data.R`, to `tidyr::drop_na()` there, and to every model formula in `models.R` (`f_region`, `f_commune`, and the written-out formulas for `m3`, `m4_attitudes`, `m4_binary`).

**Change the event window:** done upstream when the violence file is built; the code takes counts as given. The window must end before each wave's fieldwork.

**Weighted versus unweighted:** compare M2 (unweighted) with M4 (weighted). If they agree, weighting is not driving the result (DuMouchel & Duncan, 1983).

---

## 12. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `Can't rename variables in this context` in `summarise()` | Renaming inside `.by = c(new = old)` | Create the column with `mutate()` first, then group by it |
| Many "survey communes with no violence record" | Spelling mismatch, or different region spellings | Check the diagnostics examples; add region spellings to `region_alias`; compare with the dictionary |
| Ambiguous names reported | Repeated commune names within a region | Add `"cercle"` to `key_cols` (both files need it) |
| Garbled accents in names | File read with the wrong encoding | `readr::read_csv(path, locale = readr::locale(encoding = "UTF-8"))` |
| `wts` error from `avg_slopes()` on `svyglm` | No `newdata` supplied | Pass `newdata = ad, wts = "wt_scaled"` |
| `object 'd' not found` inside `avg_slopes()` (seen with `marginaleffects` 1.0) | `avg_slopes()` re-evaluates its arguments in the calling function's environment; a `newdata` passed through a wrapper's `...` is not visible there | Call `avg_slopes()` directly with named arguments in the function that holds the data (as in `fit_without_region()` in `models.R`). Do not wrap it in a helper that forwards `...` |
| `object 'record_print' not found` when loading `marginaleffects` | `knitr` older than `marginaleffects` expects | Update `knitr` (and `xfun`) |
| Design effect of 1,000+ or `NA` from `svymean(deff = TRUE)` | Rescaled weights make the finite population correction collapse | Use `deff = "replace"` |
| Error about a stratum with one PSU | Subsetting left a lonely PSU | `options(survey.lonely.psu = "adjust")` |
| `glmer` singular fit | Commune variance near zero | Report it; M4 is the main model and is unaffected |
| `glmer` convergence warning | Scaling or sparse categories | Check rare ethnicity or education levels; covariates are already centered and scaled |
| Map communes all gray | Name key mismatch between data and boundaries, or name is ambiguous | Same fixes as name matching; ambiguous names are deliberately left gray |
| Real strata are not regions (for example region by urban) | A commune can span strata | `nest = TRUE` splits it into separate clusters; acceptable |

---

## 13. R coding conventions

- Native pipe `|>`, never `%>%`.
- No `for` or `while` loops: use `purrr::map()`, `map2()`, `imap()`, `reduce()`, `walk()`.
- Namespace-qualify functions: `dplyr::filter()`, `survey::svyglm()`.
- `=` for assignment inside function bodies, `<-` outside.
- Load packages with `pacman::p_load()`, with `tidyverse` last.
- `viridis` palettes; the violence map uses a white-to-dark-red scale.
- No em-dashes in prose or output.
- Reports render to PDF via Quarto with Typst.

---

## 14. Instructions for AI assistants

When modifying or debugging this project:

1. **Keep the estimand.** The main quantity is the M4 within-commune AME. Do not change the exposure, centering, or clustering level without saying so explicitly.
2. **Keep the design in the survey object.** Strata, PSUs or communes, and weights go in `svydesign()`. Do not replace the design with random effects in M4, and do not add a PSU random effect to a design-based model.
3. **Each wave is its own design.** Design codes must stay wave-specific and weights rescaled within wave.
4. **Report AMEs in percentage points.** Do not compare `svyglm` with `glmer` using AIC, BIC, or likelihood ratio tests.
5. **Check names before models.** Any change to data inputs must be followed by `source("diagnostics.R")`.
6. **Keep the code readable.** Prefer explicit model calls over generated formulas; one step per line; comment the why, not the what. Keep computation in `models.R` and `maps.R`, and presentation in `analysis.qmd`.
7. **Call `marginaleffects` functions directly** with named `newdata` and `wts` arguments. Never forward them through `...` in a wrapper function (see troubleshooting).
8. **State uncertainty.** Distinguish settled practice, defensible choices, and guesses. Do not invent function arguments or citations; flag anything unverified.
9. **Follow the conventions in section 13.**

---

## 15. References

- Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects modeling of time-series cross-sectional and panel data. *Political Science Research and Methods, 3*(1), 133-153.
- DuMouchel, W. H., & Duncan, G. J. (1983). Using sample survey weights in multiple regression analyses of stratified samples. *Journal of the American Statistical Association, 78*(383), 535-543.
- Lumley, T. (2010). *Complex surveys: A guide to analysis using R*. Wiley.
- Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica, 46*(1), 69-85.
- Raleigh, C., Linke, A., Hegre, H., & Karlsen, J. (2010). Introducing ACLED: An armed conflict location and event dataset. *Journal of Peace Research, 47*(5), 651-660.
- Runfola, D., et al. (2020). geoBoundaries: A global database of political administrative boundaries. *PLoS ONE, 15*(4), e0231866. https://doi.org/10.1371/journal.pone.0231866 (Mali files: gbOpen ADM1 to ADM3, CC BY 4.0, from https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/gbOpen/MLI)
- Zeger, S. L., Liang, K.-Y., & Albert, P. S. (1988). Models for longitudinal data: A generalized estimating equation approach. *Biometrics, 44*(4), 1049-1060.
