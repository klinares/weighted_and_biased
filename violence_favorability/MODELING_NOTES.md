# Modeling Notes: Violence and Leader Favorability (Mali)

Reference for this project's models: what was decided, why, how to read the output, and how to work on the code. Give this file to an AI assistant as context (section 9 tells it how to behave).

---

## 1. Study and data

**Question.** Is violence against civilians in a respondent's cercle associated with favorable views of the national leader, comparing people in the same region, urban or rural area, and survey wave?

**Data.**
- Three survey waves, each an independent cross-section of people (communes partly repeat across waves). Strata: region x urban (15; Bamako is urban only), sample allocated across strata in proportion to population. Communes are the first and only sampling stage, selected with equal probability within strata; every commune has 8 respondents. Weights exist only at the respondent level: no commune selection weights, no commune frame counts (communes per stratum), no household size.
- Single-cluster strata: Bamako is one cercle (lonely PSU in the cercle-clustered design); Gao urban has one commune per wave (lonely PSU in the sampling design). Handled with `survey.lonely.psu = "adjust"`.
- Outcome: favorable view (1/0). No item nonresponse in the file; about 80% favorable.
- Exposure: ACLED event counts per **cercle** and wave over the survey window, `violence_civilian` (main) and `violence_all`. Each count is a sum over the communes surveyed in that cercle and wave, so it grows with the number of surveyed communes; the pipeline divides by that number. Only surveyed cercles have counts. No perpetrator field.
- Geography: region, cercle, commune. About 140 communes per wave, many repeated across waves; about 136 cercle-waves in total.

**Status on the real data (as reported by Kevin, commune-level version).** ICC about 17% with commune and cercle intercepts; violence coefficient about -0.10, p = 0.60; models with a commune intercept had convergence problems. After learning that violence is measured per cercle, the models were rebuilt at the cercle level (below). A first cercle-level version kept or dropped the cercle-by-wave intercept by a test and clustered the design-based check at the commune; both understated the violence standard error and were corrected (section 4). Real-data results for the corrected version are not yet in these notes.

---

## 2. Files and workflow

| File | Role |
|---|---|
| `analysis.qmd` | **Config list `cfg`** (data file, population file, violence count, covariate sets for M2a and M2b, `final`, event levels, model folder, `refit`), loads packages, sources the scripts, presents results |
| `R/prep_data.R` | Builds `ad`: IDs, events per surveyed commune, `v = log1p(events per commune)`, usual-level/change split, wave-specific design codes, rescaled and log weights |
| `R/models.R` | M1, M2a, M2b, M3, sensitivity models, design-based checks, predictions, calibration; saves models to `models/` |
| `R/poststratify.R` | Poststratifies the survey design to region x area population counts; national and cell estimates; weight-vs-count check; figure |
| `R/maps.R` | Violence per surveyed commune by cercle; M3 predicted favorability by cercle and wave; saved to `output/maps/` |
| `R/export_tables.R` | Writes M3 results, predictions, and poststratified estimates to `output/tables/*.csv` |
| `R/boundaries.R` | Name cleaning and boundary readers |
| `boundaries/` | OCHA layers and admin dictionary. **Never edit the dictionary** (used for matching at work) |
| `data/population_example.csv` | **Placeholder** region x area counts for testing; replace with real counts (columns `region`, `urban`, `population`) |

**Data contract (prepared upstream).** Columns named exactly: `wave`, `region`, `urban`, `commune`, `cercle`, `fav` (1/0), `weight`, `adm1_key`, `adm3_key`, the cercle-wave violence count named in `cfg$violence` (the same value for everyone in a cercle and wave), and the covariates in `cfg$covariates` and `cfg$covariates_m2b`. Factors arrive with their reference level first (region: Bamako; wave: 1); age is grouped. Cercle names must match the OCHA cercle names (accents and case do not matter). Rows missing any variable in either covariate set are dropped, so all models use the same respondents. Population file: one row per region x area with `region` and `urban` spelled as in the survey file.

**Choosing the covariates.** Fix `cfg$final` on substantive grounds (potential confounders of violence and favorability) before looking at the violence coefficient. M2b is a check that the violence estimate does not depend on that choice, not a contest decided by AIC. The random-effect structure is fixed: there is nothing to choose.

**Model caching.** `fit_glmer()` loads `models/<name>.rds` if it exists, otherwise fits and saves it. It does **not** notice changed data or formulas: set `cfg$refit = TRUE` (or delete `models/`) after changing the data, covariates, or switches, otherwise the report will use stale models.

**Packages.** `lme4`, `survey`, `srvyr`, `PracTools`, `marginaleffects`, `performance`, `broom.mixed`, `sf`, `viridis`, `patchwork`, `knitr`, `tidyverse`. Tables in the report use `knitr::kable`; full M3 results go to CSV in `output/tables/` (no appendix).

## 3. Measures

- $V_{kw} = \log(1 + E_{kw} / n_{kw})$, where $E_{kw}$ is the cercle-wave count and $n_{kw}$ the number of communes surveyed there that wave. Without the division a cercle with more surveyed communes would look more violent. 0 events -> 0; +0.69 doubles $(1 + E/n)$. Report predicted favorability at 0, 1, 5, 20 events per surveyed commune.
- **Usual level and change (sensitivity, Mundlak).** $\bar V_k$ = mean of $V_{kw}$ over the waves cercle $k$ was surveyed; change = $V_{kw} - \bar V_k$. The change coefficient compares a cercle with itself over time.
- **Population counts** (region x urban): not used to scale violence; used to poststratify the survey design for population estimates of percent favorable (`R/poststratify.R`).
- **Civilian vs all violence.** Swap `cfg$violence` between `violence_civilian` and `violence_all` and refit. A ratio (civilian / all) was rejected: undefined at 0 events and unstable with few events.
- **Centering** (client question): region centering is what the region dummies already do; centering within cercle is what the change term of the sensitivity model does. Centering is not an alternative to the log; it changes which comparison the coefficient makes.

---

## 4. Models

All `lme4::glmer`, logit, ML (Laplace), `nloptwrap` with tight tolerances (about 10x faster than `bobyqa`, same log-likelihood), unweighted, same complete-case respondents.

| Model | Specification | Role |
|---|---|---|
| M1 | $\gamma_0 + u_k + v_{kw}$ (cercle, cercle-by-wave) | ICCs |
| M2a | M1 + $\beta V$ + wave + region + urban + `cfg$covariates` | |
| M2b | the same with `cfg$covariates_m2b` | covariate check; AIC, BIC, `anova()` if nested |
| M3 | the `cfg$final` model + centered log weight | **final model**: estimates, predictions, maps |
| Commune intercept | M3 + $(1 \mid \text{commune})$ | sensitivity |
| Stratum-by-wave | M3 with `strata_w` (region x urban x wave) in place of wave, region, urban | sensitivity: full design strata |
| Weight slope | M3 + violence x log weight | informative sampling for the slope (DuMouchel & Duncan, 1983) |
| Split | M3 with $\bar V_k$ and $V_{kw} - \bar V_k$, plus a Wald test of their difference | sensitivity |
| Marginal, weighted, cercle clusters | `svyglm`, ids = cercle, strata = region, weights | design-based check |
| Marginal, unweighted, cercle clusters | the same without weights | separates weighting from conditional-vs-marginal |
| Marginal, weighted, commune clusters | the sampling design (ids = wave x commune, strata = wave x region x urban) | shows the effect of the clustering choice |

**ICCs (latent scale, $\pi^2/3$ for the individual level).**
$\rho_{\text{cercle}} = \tau_k / (\tau_k + \tau_{kw} + \pi^2/3)$: same cercle, any wave.
$\rho_{\text{cercle-wave}} = (\tau_k + \tau_{kw}) / (\tau_k + \tau_{kw} + \pi^2/3)$: same cercle, same wave.

**Why these choices.**
- **Both random intercepts are fixed by design, not tested.** Violence takes one value per cercle and wave; the cercle-by-wave intercept accounts for respondents sharing that value. Testing whether to keep it (and dropping it when the test is not significant) is a pre-test that biases toward finding an effect, because the term is dropped exactly when it is imprecisely estimated.
- **Design-based check clustered at the cercle.** This is a choice about the estimand, not a description of the sampling (communes were sampled, cercles were not). For an association meant to hold beyond these particular cercle shocks, uncertainty belongs where the exposure is assigned (Moulton, 1990; Abadie et al., 2023). The commune-clustered version is the right design for descriptive estimates and gives a smaller violence SE; both are reported. Strata = region because cercles nest in regions but can contain urban and rural communes. Expect few design df (clusters minus strata, plus 1, minus coefficients; 17 on the simulated data).
- **No commune intercept in M3.** Violence does not vary within a cercle-wave, and on the real data the commune variance failed to converge. Reported as a sensitivity model.
- **Region, urban, wave fixed.** Strata are fixed by design; 3 waves cannot support a variance; a random region intercept would let north/south confounding into $\beta$ (Bell & Jones, 2015). The stratum-by-wave sensitivity conditions on all stratum indicators, which is what makes equal-probability commune selection ignorable in the model.
- **Weights.** A weighted multilevel model (pseudo-likelihood: Pfeffermann et al., 1998; Rabe-Hesketh & Skrondal, 2006; WeMix) needs a weight at each level. Only final respondent weights exist, and the commune frame needed to rebuild commune weights is not available. So weights enter as covariates (DuMouchel & Duncan, 1983; Little, 1991): the log weight (main effect) and its interaction with violence. Including the weight changes what other coefficients are conditional on, so M2 is shown beside M3. `svylme` would need stage weights too and fits linear models only.
- **Marginal vs conditional.** Marginal coefficients are smaller by about $\sqrt{1 + 0.346\,\tau}$ with $\tau$ the total random-intercept variance (Zeger, Liang & Albert, 1988); a larger gap is due to the weights, which the unweighted marginal row separates out. Compare marginal and conditional rows on direction and precision, not size. Never compare them by AIC or likelihood ratio tests.

---

## 5. Reported quantities

- Violence coefficient, 95% CI, p, in every model row.
- Predicted favorability at `cfg$event_levels` (levels above the data's maximum are dropped, so predictions never extrapolate).
- **0 vs 5 events** with 95% CI: `avg_comparisons(m3, variables = list(v = c(0, log1p(5))))`. This is the plain-language range.
- **Minimum detectable effect** (80% power, alpha .05): $2.8 \times SE(\beta)$, converted to percentage points with mean $\hat p(1-\hat p)$, times $\log 6$ for 0 vs 5 events. First-order approximation.
- Predicted favorability by variable: every factor in M3 by counterfactual (everyone set to each level, everything else as observed).
- **Poststratified percent favorable**: classical poststratification of the survey design (`survey::postStratify`) to region x area counts, by wave, with design-based CIs. Not MRP: with counts for region x area only, a model cannot adjust for anything the design does not already cover, and M3's predictions (which include each cercle's own effects) nearly reproduce observed cell means. The report shows the largest gap between a cell's share of the weights and its share of the population; large gaps mean the weights and the counts describe different populations.
- Maps: (1) violence per surveyed commune averaged over the waves each cercle was surveyed, binned at 0 and the quartiles of the positive values; (2) M3 predicted favorability with cercle and cercle-wave effects, weighted mean per cercle and wave (describes the surveyed communes of a cercle, not the whole cercle). Gray = not surveyed.
- **CSV files** in `output/tables/`: `m3_fixed_effects`, `m3_random_effects`, `violence_by_model`, `m3_predicted_by_events`, `m3_change_0_to_5`, `m3_predicted_by_variable`, `poststratified_by_cell`, `poststratified_national`.

---

## 6. Reading a null result (the current real-data situation)

- p = 0.60 means the data cannot distinguish the association from zero. It does not show there is no effect.
- Lead with the 0 vs 5 events CI in percentage points ("effects larger than X points are unlikely") and the minimum detectable effect ("the study could reliably detect effects of about Y points or more").
- Near 80% favorable, the logistic slope is $p(1-p) \approx 0.16$ (vs 0.25 at 50%), so the same log-odds effect moves the percentage less.
- Likely reasons the signal is weak: violence varies mostly between places (few comparisons within a place over time); no perpetrator split (state vs armed-group violence may push in opposite directions); sensitive outcome under a military government (over-reporting of favorability compresses differences); ACLED undercounting in remote areas (attenuation); coverage of insecure areas.
- **Do not** search over exposure forms, windows, subsets, or interactions until something is significant. Any added specification must be pre-stated and labeled exploratory, and all versions reported.

**Enhancements still open (ask before adding):** events before each respondent's interview date (if dates exist); commune-level ACLED with a cercle mean / local deviation split; neighboring-cercle violence from OCHA adjacency; one or two pre-specified interactions (for example violence x ethnicity, violence x north/center); a lagged event window if dates are available.

---

## 7. Diagnostics

| Check | Good | Warning |
|---|---|---|
| ICC | clearly above 0 | near 0 |
| Log weight and violence x log weight | both within about 2 SE of 0 | clearly nonzero: informative sampling |
| Marginal weighted vs unweighted | similar | very different: the weights matter |
| Commune and stratum-by-wave sensitivity rows | close to M3 | far from M3 |
| Usual level vs change | difference not clearly nonzero | clearly different (and see the composition caution) |
| VIF | below about 2 | high |
| Calibration (simulation-based) | observed inside simulated range | many bins outside |
| Convergence | relative gradient below about 0.001 | above: refit with `optimizer = "bobyqa"` and compare |
| Weight shares vs population shares | similar | large gaps: weights and counts disagree |

Binned residuals from `performance::binned_residuals()` were replaced: residuals built on shrunken random effects show a false slope even when the model is correct (verified on simulated data). The calibration check simulates data sets from M3 with new cercle and cercle-wave effects; the binning and the simulated range use separate simulations.

---

## 8. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `Run the config chunk` error | Script sourced without the config | Run the config chunk or copy `cfg` into the console |
| Model refits every render | `refit = TRUE` left on | Set `refit = FALSE` after one refit |
| Survey cercles not placed on the map (caption count above 0) | Region or cercle name does not match OCHA | Check the cercle spelling upstream |
| `cfg$final must be "M2a" or "M2b"` | Typo in the config | Use one of the two |
| Report uses old results | Saved models reused | Set `cfg$refit = TRUE` |
| `boundary (singular) fit` | A variance is at zero | Report it; the structure stays fixed |
| Gradient convergence warning | Absolute gradient above 0.002 | Check the relative gradient in the report; below about 0.001 it is a false alarm |
| `No population count for: ...` | Region or area spelled differently in the population file | Match the survey file's spelling |
| `Some populated region x area cells have no respondents` | A cell with people but no sample in some wave | Merge the cell with a neighbor in the population file, or report it as not covered |
| `object 'Hessian' not found`, or no convergence messages at all | lme4 2.0 skips its convergence checks (and stores no derivatives) for models with more than 20 parameters | `R/models.R` sets `check.conv.nparmax = Inf` when the installed lme4 has that setting; tested on lme4 1.1.35 and 2.0.6 |
| Region shows as `S..gou` | Non-UTF-8 locale | Render on Windows/RStudio (UTF-8) |
| `could not find function "%||%"` | R older than 4.4 | Update R |
| Quarto "juice ... deno.lock" messages | Quarto cannot write a lock file in RStudio's folder | Harmless; the PDF is created |

---

## 9. Instructions for AI assistants

You are helping a survey methodologist (Total Survey Error tradition). On this project:

1. **Keep the estimand**: the M3 violence coefficient (typical cercle and wave), with the design-based check. Do not change exposure, model sequence, or fixed/random structure without stating the consequence.
2. **Never drop the cercle-by-wave intercept**, and never cluster the violence SE below the level where violence is assigned without saying so.
3. **Never pass sampling weights to `glmer`**, never add random effects to `svyglm`, never compare them by AIC or likelihood ratio tests.
4. **Report percentage points and predicted percentages** for clients; treat a non-significant result as a CI and a minimum detectable effect, not as "no effect".
5. **No specification searching.** New specifications are exploratory and reported in full.
6. **Protect the maps**: gray is "not surveyed", never zero; the violence count is always divided by the number of surveyed communes.
7. **Do not edit `boundaries/mali_admin_dictionary.csv`.**
8. **Code style**: tidyverse, native pipe, `package::function()`, no `for` loops (use `purrr`), `=` inside function bodies, single space after `<-`, comments that explain why in plain words, settings only in `cfg`.
9. **Be honest about uncertainty**: separate established practice, defensible choices, and guesses. Never invent function arguments, results, or citations; verify by running code.

---

## 10. Validation (simulated data)

The pipeline was run on simulated data with the same layout (8 regions, 15 strata, 8 respondents per commune, 136 cercle-waves in 48 cercles). Truth: violence -0.25; cercle variance 0.20; commune variance 0.20 (not in M3); cercle-wave variance 0.06; uninformative weights.

- M3 violence -0.405 (SE 0.116, 95% CI -0.632 to -0.179): the truth is inside the CI, about 1.3 SE away. One simulated data set shows the pipeline runs and recovers the truth within sampling error; it is not a test of bias or coverage.
- Sensitivity rows: commune intercept -0.408 (SE 0.116); stratum-by-wave -0.437.
- Marginal: weighted, cercle clusters -0.297 (t on 17 df, p = .016); unweighted, cercle clusters -0.355; weighted, commune clusters SE 0.089 vs 0.111 at the cercle.
- Log weight 0.025 (SE 0.068); violence x log weight 0.110 (SE 0.094).
- Relative gradient 8e-4 (the lme4 absolute-gradient warning is a false alarm); a bobyqa refit gives the same estimates.
- An independent review (no access to the build conversation) re-derived every number in the report from the fitted models and CSVs and found no mismatches.

`data/survey_sim.rds` shows the expected file layout. `data/population_example.csv` holds placeholder counts that do not match the simulated weights (Bamako: 0.7% of weights, 14.5% of the counts); the report says so whenever the example file is in use.

## References

Abadie, Athey, Imbens & Wooldridge (2023) *QJE* 138(1):1-35. Bell & Jones (2015) *PSRM* 3(1):133-153. Binder (1983) *ISR* 51(3):279-292. DuMouchel & Duncan (1983) *JASA* 78(383):535-543. Gelman (2007) *Statistical Science* 22(2):153-164. Little (1991) *JOS* 7:405-424. Lumley (2010) *Complex Surveys*, Wiley. Moulton (1990) *REStat* 72(2):334-338. Mundlak (1978) *Econometrica* 46(1):69-85. OCHA COD-AB Mali, DNCT 2021, HDX. Pfeffermann, Skinner, Holmes, Goldstein & Rasbash (1998) *JRSS-B* 60(1):23-40. Rabe-Hesketh & Skrondal (2006) *JRSS-A* 169(4):805-827. Raleigh, Linke, Hegre & Karlsen (2010) *JPR* 47(5):651-660. Zeger, Liang & Albert (1988) *Biometrics* 44(4):1049-1060.
