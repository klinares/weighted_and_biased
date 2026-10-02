# Modeling Notes: Violence Against Civilians and Leader Favorability (Mali)

**Purpose of this file.** A complete reference for this project: the study, the design, every modeling decision and why it was made, how to read the results, how the code is organized, and how to troubleshoot it. Give it to an AI assistant as context before asking for help. Section 15 tells the assistant how to behave.

------------------------------------------------------------------------

## 1. The study

**Question.** Is violence against civilians in a respondent's commune associated with how favorably they view the national leader, after accounting for region, survey wave, and who was interviewed?

**Audience.** The client must explain results to policymakers and other non-technical customers. Results are therefore reported as percentages and percentage points, and maps must be correct and unambiguous.

**Data** - Three survey waves, a few months apart, each an independent sample. - Outcome `fav`: 1 = favorable view of the leader, 0 = not. - Exposure: ACLED events of **violence against civilians** (events, not fatalities) counted for each surveyed commune and wave on fixed dates. The counts were attached to the survey file by the project team; only surveyed communes have counts. - Geography: region (stratum), commune, village. There is no cercle column in the survey. - No external data (population, coordinates, or other sources) can be added.

**What "explainable" means here.** One main model, one sentence of interpretation, and predicted percentages at a few event counts.

------------------------------------------------------------------------

## 2. Survey design

**Working assumption: a two-stage design within each wave.** - Strata: region (8 regions, the pre-2016 structure). - Stage 1 (PSU): communes. - Stage 2 (SSU): villages within communes. - Respondents within villages.

This has not yet been confirmed with the survey team. To check from the data: if the number of communes per region is similar across waves and each commune contains several villages, communes were very likely the PSUs.

**How the code implements it** (`R/prep_data.R`, `models.R`): 1. Codes are made wave-specific, because each wave is its own design: `strata_w = paste(wave, region)`, `commune_w = paste(wave, commune_id)`, `village_w = paste(wave, commune_id, village)`. 2. Weights are rescaled within wave, `wt_scaled = weight * n_w / sum(weight)`, so the wave with the largest population total does not dominate the pooled models. 3. One design object: `svydesign(ids = ~commune_w + village_w, strata = ~strata_w, weights = ~wt_scaled, nest = TRUE)`. 4. `options(survey.lonely.psu = "adjust")` handles strata left with one commune after subsetting.

**Why standard errors come from communes.** Taylor linearization (without finite population corrections) computes variance from the first-stage units, the communes. Commune totals include all villages and respondents in the commune, so village-level clustering is covered too. This matters here because violence is measured at the commune: everyone in a commune-wave shares one value.

**If villages turn out to be the PSUs** (single-stage design): keep `ids = ~commune_w + village_w` anyway, or use `ids = ~commune_w`. Clustering at the commune, the level where violence is assigned, is the conservative and correct choice for a commune-level exposure. Using villages as clusters would understate the standard error of the violence effect.

**Never add a random effect for PSUs to a model that uses the survey design.** The design already handles that clustering through the standard errors; a random effect would count it twice.

------------------------------------------------------------------------

## 3. Files and run order

| File | Role | Edit? |
|------------------------|------------------------|------------------------|
| `R/prep_data.R` | Reads the survey, maps column names, builds violence variables and design columns | **Yes: the only file to edit for new data** |
| `R/boundaries.R` | Name-cleaning helpers, region spelling table, boundary readers | Add region spellings to `region_alias` |
| `match_communes.R` | Proposes matches from survey commune names to the official list | No |
| `matching/crosswalk.csv` | Reviewed matches (made by a person) | Yes, by hand |
| `diagnostics.R` | Data checks and K, A, P, W | Thresholds only |
| `models.R` | Models M1 to M4, effects, predictions, leave-one-region-out | To change models |
| `maps.R` | Builds maps and saves PDF and PNG copies to `output/maps/` | Rarely |
| `analysis.qmd` | Report (Typst PDF); presentation only | Wording and figures |
| `testing/` | Simulation and a stand-in for manual review | Testing only |
| `setup_done_do_not_rerun/` | One-time boundary build (already run) | No |

Run order: edit `R/prep_data.R`, then `match_communes.R`, review the draft, `diagnostics.R`, `models.R`, then render `analysis.qmd`. Each script sources the one before it: `analysis.qmd` sources `models.R` and `maps.R`; `models.R` sources `diagnostics.R`; `diagnostics.R` sources `R/prep_data.R`; which sources `R/boundaries.R`.

**Debugging:** every number in the report is an object created in `models.R` or `maps.R` (`results`, `main`, `predicted`, `loo`, `u_hat`, `map_data`). If rendering fails, run those scripts in the console first; errors will point to a line rather than a report chunk.

**Packages:** `survey`, `lme4`, `marginaleffects`, `sf`, `ggplot2` (via `tidyverse`), `viridis`, `patchwork`, `knitr`, `glue`, `stringi`, `pacman`. Nothing connects to the internet. `sampling` is used only by the simulation.

------------------------------------------------------------------------

## 4. Data contract (`R/prep_data.R`, section 2)

Standard names on the left are used everywhere; the right-hand side is the real column name.

| Standard name | Meaning |
|------------------------------------|------------------------------------|
| `wave` | 1, 2, 3 |
| `region` | stratum; one of the 8 survey regions |
| `commune` | commune name as typed in the survey |
| `village` | village name or code |
| `weight` | final weight for that wave |
| `fav` | outcome, 0/1 |
| `events` | ACLED violence-against-civilians events in the commune that wave |
| `female`, `age`, `ethnicity`, `education`, `urban` | respondent covariates (main models) |
| `local_gov`, `democracy`, `perceived`, `stress` | attitudes (sensitivity model only) |

`prep_data.R` also creates `commune_id`: the official P-code when the reviewed crosswalk exists (so a commune spelled differently across waves is one commune), otherwise the cleaned `region|commune` name.

------------------------------------------------------------------------

## 5. Commune name matching (maps, and a stable commune ID)

The models only need commune names to group respondents. Matching to the official list is needed for the maps and to unify spellings.

**Workflow** 1. Export distinct `region, commune` from the merged survey to `matching/communes_from_survey.csv`. 2. `source("match_communes.R")` writes `matching/crosswalk_draft.csv`: - `exact`: cleaned name matches exactly one official commune in that region. Done. - `ambiguous`: the name exists more than once in that region; all options are listed with their cercle. - `review`: no exact match; the 3 closest names in the region are listed, with codes and a distance (0 = identical; about 0.2 = one letter in five differs). 3. A person fills `adm3_code` for every non-exact row (or leaves it blank if the commune cannot be placed) and saves the file as `matching/crosswalk.csv`. The script never overwrites this file.

**Rules** - Never accept a suggested match without review. A wrong commune on a map given to a policymaker is a real harm. - Names are cleaned before comparing: accents, case, spaces and punctuation removed ("Ségou" and "SEGOU" both become `segou`). - Ménaka is searched under Gao, because the OCHA 2021 boundaries separate Ménaka while the survey's 8 regions do not. - **One name, one commune.** Some names repeat within a region (Benkadi in Koulikoro; Kapala in Sikasso; Somo in Ségou). If two same-named communes were both surveyed, the survey name alone cannot separate them. `prep_data.R` stops if the crosswalk has two rows for one `region + commune`, because joining would duplicate respondents. Resolve such names with the survey team (for example from village lists) before analysis. - Unplaced communes are never dropped silently: each map caption states how many survey communes could not be placed.

**Tested on simulated data** (275 communes, 50 deliberately misspelled): all 221 exact matches were correct; for every name needing review the correct commune was among the suggestions, and it was the first suggestion in 52 of 54 rows.

------------------------------------------------------------------------

## 6. Violence measure

For commune $c$ in wave $w$, with $E_{cw}$ events:

$$V_{cw} = \log(1 + E_{cw})$$

- Many communes have zero events and a few have many; the log compresses the long tail, and adding 1 keeps zero defined.
- **Exact meaning of a one-unit change:** $V$ rising by 1 means $(1 + E)$ is multiplied by $e \approx 2.72$. $V$ rising by $\log 2 \approx 0.69$ means $(1 + E)$ doubles. For counts well above zero, doubling $(1 + E)$ is close to doubling $E$; for small counts it is not (0 to 1 event is a doubling of $1 + E$).
- Because these steps are hard to picture, the headline is **predicted favorability at 0, 1, 5, and 20 events**.

**Region-wave measure (Model 1 only):** $V_{rw} = \log(1 + \sum_{c \in S_{rw}} E_{cw})$, summing over the communes **surveyed** in region $r$ and wave $w$. Only surveyed communes have counts, so this is not a complete regional total.

**Within/between split (appendix only):** $\bar V_c$ = mean of $V_{cw}$ over the waves in which commune $c$ was surveyed (each wave counted once); within = $V_{cw} - \bar V_c$. A commune surveyed once has within = 0.

------------------------------------------------------------------------

## 7. Diagnostics: K, A, P, W (`diagnostics.R`)

Computed before any model, so the specification follows from the data structure rather than from results.

|   | Definition | Flag |
|------------------------|------------------------|------------------------|
| Check | Commune-waves with more than one event count | Must be 0 (one commune-wave shares one count) |
| **K** | Commune-wave cells (share with at least one event) |  |
| **A, region** | $R^2$ of $V_{rw}$ on region + wave dummies, across region-wave cells | Above 90%: Model 1 cannot inform the violence effect |
| **A, commune** | $R^2$ of $V_{cw}$ on region + wave dummies, across commune-wave cells | Above 90%: commune exposure is also collinear with the dummies |
| **P** | Median villages per commune-wave | Shows the second stage |
| **W** | $\operatorname{var}(V^{\text{within}}) / \operatorname{var}(V_{cw})$, and communes surveyed in 2+ waves | Below 10%: within estimate is appendix only |

**Why A matters.** Variation in the exposure that the dummies explain cannot inform its coefficient. An earlier region-level version of the real data had A, region of about 98%.

**Real data so far.** The within-commune share was about 6% (stated by Kevin), with ample between-commune variation. The main estimate is therefore a comparison between communes in the same region, adjusted for wave.

------------------------------------------------------------------------

## 8. Models (`models.R`)

Shared base (separate region and wave dummies, kept separate at Kevin's request so other coefficients stay interpretable; references Bamako and wave 1):

$$\eta_i = \beta_0 + \sum_{r \neq \text{Bamako}} \alpha_r \,\text{Region}_r + \sum_{w=2}^{3} \gamma_w \,\text{Wave}_w + \mathbf{x}_i'\boldsymbol{\beta}$$

with $\mathbf{x}_i$ = female, age per decade (centered), ethnicity (most common group as reference), education, urban.

| Model | $\operatorname{logit}\Pr(y_i=1)$ | Clustering | Weights | Role |
|---------------|---------------|---------------|---------------|---------------|
| M1 | $\eta_i + \beta_R V_{rw}$ | Survey design | Yes | Original specification; shows the region-level problem |
| M2 | $\eta_i + \beta_V V_{cw}$ | None | No | Naive comparison |
| M3 | $\eta_i + \beta_V V_{cw} + u_c$, $u_c \sim N(0,\tau^2)$ | Commune random intercept | No | Multilevel alternative |
| **M4** | $\eta_i + \beta_V V_{cw}$ | Survey design | Yes | **Main model** |
| M4 + attitudes | M4 + `local_gov`, `democracy_c`, `perceived_c`, `stress_c` | Survey design | Yes | Descriptive: attitudes may themselves respond to violence |
| M4 within/between | $\eta_i + \beta_W (V_{cw} - \bar V_c) + \beta_B \bar V_c$ | Survey design | Yes | Appendix |

Each step changes one thing: M1 to M2 the exposure, M2 to M3 clustering by random effect, M3 to M4 survey design instead of random effect.

**Estimation** - M1 and M4: `survey::svyglm(..., family = quasibinomial())`. Pseudo-maximum likelihood solving $\sum_i w^*_i \mathbf{x}_i (y_i - \mu_i) = 0$. `quasibinomial` gives the same estimates as `binomial` and avoids warnings about non-integer weighted counts. - Variance (Binder, 1983), strata $h$, communes $j$, $n_h$ communes in stratum $h$: $$\widehat{\operatorname{Var}}(\hat{\boldsymbol\beta}) = \hat{\mathbf J}^{-1}\left[\sum_h \frac{n_h}{n_h-1}\sum_{j}(\mathbf z_{hj}-\bar{\mathbf z}_h)(\mathbf z_{hj}-\bar{\mathbf z}_h)'\right]\hat{\mathbf J}^{-1},\quad \mathbf z_{hj}=\sum_{i\in(h,j)} w^*_i\mathbf x_i(y_i-\hat\mu_i)$$ - M2: `glm`, model-based standard errors (too small; shown for contrast). - M3: `lme4::glmer`, Laplace approximation, `optimizer = "bobyqa"`, unweighted. - AIC, BIC, and likelihood ratio tests **cannot** compare `svyglm` with `glm` or `glmer` (pseudo-likelihood). Compare AMEs and standard errors only.

**Reported effects** - AME: design-weighted average of $\partial\hat\mu_i/\partial V$, in percentage points per one-unit change in $V$. Calls: survey models `avg_slopes(m, variables = "v_commune", newdata = ad, wts = "wt_scaled")`; M2 `avg_slopes(m2, variables = "v_commune", newdata = ad)`; M3 adds `re.form = NA`. - Predicted favorability: `avg_predictions(m4, variables = list(v_commune = log1p(c(0, 1, 5, 20))), newdata = ad, wts = "wt_scaled")`. Meaning: if every respondent's commune had that many events, other characteristics as observed, design-weighted. - M3's effect is for a typical commune ($u_c = 0$), a conditional effect. M1, M2 and M4 give population-averaged effects. Under a logit link these differ slightly, by more when $\tau^2$ is larger (approximate attenuation factor $\sqrt{1 + 0.346\,\tau^2}$; Zeger, Liang & Albert, 1988). Do not read small M3 versus M4 differences as substantive. - Leave one region out: M4 refit 8 times, each without one region.

------------------------------------------------------------------------

## 9. Reading the results

| Output | What it says |
|------------------------------------|------------------------------------|
| Predicted favorability table | **Headline.** Percent favorable if every commune had 0, 1, 5, or 20 events |
| AME table | M1 wide and uninformative; M2 to M4 similar estimates; M4's wider interval is the honest one |
| Diagnostics table | A, region high and A, commune low is the argument for commune exposure |
| Spread figure | Communes within one region-wave differ widely; a region measure erases this |
| Violence map | Events per surveyed commune and wave; gray = not surveyed (missing, not zero) |
| Favorability map | Design-weighted percent favorable per commune-wave; few respondents each, read broad patterns only |
| Model 3 map | Commune effects; large same-colored areas suggest a missing geographic factor |
| Leave one region out | If one region moves the estimate a lot, that region drives the result |
| Binned residuals, calibration | Points outside bounds or a curve suggest a missing nonlinearity |
| Appendix within/between | Within uses only repeat communes with changing violence; imprecise when W is small |

**Plain-language template**

> Among people surveyed in the same region and survey round, those living in communes with more violence against civilians were less likely to view the leader favorably. If every commune had experienced no such events, about X% would be expected to hold a favorable view; with 5 events, about Y%. This compares different communes, so it shows an association, not proof that violence caused the change.

**Do not** - call the result causal; - show odds ratios to customers (they are read as probabilities); - interpret a single commune on the favorability map; - present M4 + attitudes as the main estimate; - describe Model 1 as "wrong" on real data: it is **unable to distinguish an effect from no effect**, which diagnostic A shows before any outcome is examined.

------------------------------------------------------------------------

## 10. Maps (`maps.R`)

- Boundaries: OCHA COD-AB for Mali, DNCT release of 10 November 2021 (701 communes, 53 cercles, 10 regions, official P-codes). Full citation in `boundaries/SOURCE.md`; the credit line is printed on every map.
- Communes are placed **only** through the reviewed crosswalk.
- Classed colors keep three states distinct: **Not surveyed** (gray), **zero events** (palest red), **fewer than `min_n` respondents** (white, outlined). `min_n` is 5.
- Violence classes: 0, 1-2, 3-9, 10-29, 30 or more. Favorability classes: under 40%, 40-50%, 50-60%, 60-70%, 70% or more.
- Publication copies: `output/maps/*.pdf` (vector) and `*.png` (300 dpi).
- Region labels use OCHA's own names, accents included; overlapping labels are skipped automatically.

------------------------------------------------------------------------

## 11. Decisions log

| Considered | Decision | Reason |
|------------------------|------------------------|------------------------|
| Region-wave violence only (client's first idea) | Kept as M1 for contrast | Region and wave dummies absorb most of its variation |
| Random effects for region or wave | Rejected | 8 regions and 3 waves are too few to estimate a variance; regions are the strata, not a sample of regions |
| PSU random effect plus survey design | Rejected | Counts clustering twice |
| Region by wave dummies (24 cells) | Not used | Kevin prefers separate dummies for interpretability; noted as a limitation (region-specific shocks not absorbed) |
| Within/between (Mundlak) as main | Appendix | Within-commune share about 6% in the real data |
| Commune fixed effects (dummies) in a logit | Rejected | Few respondents per commune: incidental-parameters bias |
| Bayesian model (`brms`) | Dropped | No design-based inference; adds complexity without fixing identification |
| Random forest, XGBoost, neural nets | Rejected | Question is an effect, not prediction; no calibrated uncertainty; weights and clustering awkward; poor explainability |
| Double machine learning | Rejected | No clean handling of survey weights; hard to explain |
| Gaussian process or spline over interview month | Dropped | Three waves a few months apart: too few time points |
| Accumulating violence over waves | Not possible | Counts exist only for surveyed commune-waves, and most communes were surveyed once |
| Interactions (violence by wave, region, urban, ethnicity) | Deferred | Kevin's choice; if added later, pre-specify 2 or 3 and report group-specific effects |
| Random slopes | Rejected | Violence barely varies within commune; 8 regions too few |
| Events per capita | Not possible | Needs external population data |
| Event categories or binary exposure | Not used | `log1p` chosen, with predictions at set counts for interpretation |
| Aggregating to cercles | Rejected | Only 30 to 40% of communes surveyed: cercle totals would undercount violence and look authoritative |
| Fuzzy matching at run time | Rejected | Matches proposed once, reviewed by a person, stored in a crosswalk |
| Boundaries as CSV | Rejected | Polygons do not fit in rows; the GeoPackage is one file read in one line |
| OCHA versus geoBoundaries boundaries | OCHA 2021 DNCT | Official P-codes and hierarchy; same 701 communes as 2017 with current spellings; OCHA's 2025 release has no commune level |
| Scale bar (`ggspatial`) | Not used | Its dependencies include tile downloading; not needed for national maps |

------------------------------------------------------------------------

## 12. Validation performed

Simulated data (`testing/simulate_data.R`) mimic the assumed design: real Mali communes, two-stage PPS sample, events attached per commune-wave, misspelled names. True violence coefficient: -0.30 on the logit scale.

|                | Estimate (logit) | SE   |
|----------------|------------------|------|
| M1 region-wave | -0.08            | 0.12 |
| M2             | -0.33            | 0.04 |
| M3             | -0.33            | 0.04 |
| M4             | -0.34            | 0.06 |

M2 to M4 recover the true value within about one standard error; M1 does not. M4 AME: -7.9 points (95% CI -10.4 to -5.5); predicted favorability 58% at 0 events and 44% at 5 events. The M3 commune variance was underestimated in this draw (0.03 against a true 0.12); variance components in logistic multilevel models with few respondents per commune are imprecise, and this does not affect the main estimates.

------------------------------------------------------------------------

## 13. Troubleshooting

| Symptom | Cause | Fix |
|------------------------|------------------------|------------------------|
| `crosswalk.csv has more than one row for ...` | One survey name points to two communes | Keep one row per region + commune; resolve the name with the survey team |
| `Detected an unexpected many-to-many relationship` | Same as above (older code) | Same as above |
| `Commune-waves with conflicting event counts` above 0 | Two communes share one name, or a merge error | Check those communes in the source file |
| Many `review` rows in the draft | Spelling differences or region names | Review candidates; add region spellings to `region_alias` |
| `object 'd' not found` inside `avg_slopes()` | Arguments forwarded through `...` in a wrapper | Call `marginaleffects` functions directly with named arguments |
| Warning "training data could not be extracted reliably" | `marginaleffects` 1.0 cannot find the survey model's data; results unaffected because `newdata` is supplied | Ignore, or in version 1.0+ attach data with `marginaleffects::set_modeldata(m, ad)` |
| `object 'record_print' not found` loading `marginaleffects` | Old `knitr` | Update `knitr` and `xfun` |
| `Can't rename variables in this context` | Renaming inside `.by =` | Create the column with `mutate()` first |
| Design effect of 1,000+ or `NA` | Rescaled weights collapse the finite population correction | Use `deff = "replace"` |
| Error about a stratum with one PSU | Subsetting left one commune in a stratum | `options(survey.lonely.psu = "adjust")` |
| `glmer` singular fit | Commune variance near zero | Report it; M4 is unaffected |
| Theme error about `legend.title.position` | Older `ggplot2` | Use `guide_legend(title.position = "top")` (as the code does) |
| Map communes all gray | Crosswalk missing or codes not matching the boundaries | Rerun matching; codes must be OCHA P-codes from the dictionary |

------------------------------------------------------------------------

## 14. R conventions

Native pipe `|>`; no `for` or `while` loops (use `purrr`); namespace-qualified calls (`dplyr::filter()`); `=` inside function bodies, `<-` outside; `pacman::p_load()` with `tidyverse` last; `viridis` palettes (red scale for violence maps); no em-dashes in prose; reports render to PDF with Quarto and Typst.

------------------------------------------------------------------------

## 15. Instructions for AI assistants

You are assisting a survey methodologist trained in the Total Survey Error and Total Error Framework traditions. When working on this project:

1.  **Keep the estimand.** The main quantity is the M4 violence effect (commune exposure, survey design). Do not change the exposure, the design, the clustering level, or the dummies without saying so explicitly and explaining the consequence.
2.  **Keep the design in the survey object.** Strata, communes, villages and weights go in `svydesign()`. Do not add random effects to design-based models.
3.  **Each wave is its own design:** wave-specific codes and within-wave weight rescaling stay.
4.  **Report in percentage points and predicted percentages.** Never compare `svyglm` with `glm`/`glmer` by AIC, BIC or likelihood ratio tests.
5.  **Protect the maps.** Never auto-accept fuzzy matches; never let unmatched communes disappear silently; keep "not surveyed" visually distinct from zero.
6.  **Keep the code simple and readable:** explicit model calls, one step per line, comments that explain why. Computation in `models.R` and `maps.R`; presentation in `analysis.qmd`.
7.  **Call `marginaleffects` directly** with named `newdata` and `wts`; never forward them through `...`.
8.  **Be honest about uncertainty.** Separate settled practice, defensible choices, and your own guesses. Never invent function arguments, package behavior, citations, or results; say when something is unverified. Check numbers by running code, not by assumption.
9.  **Think in total error.** For any change, consider coverage (insecure areas), sampling (two-stage design), nonresponse, measurement (ACLED undercounting, sensitive outcome), and processing (name matching, merges).
10. **Follow section 14.**

------------------------------------------------------------------------

## 16. References

- Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects modeling of time-series cross-sectional and panel data. *Political Science Research and Methods, 3*(1), 133-153.
- Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys. *International Statistical Review, 51*(3), 279-292.
- Lumley, T. (2010). *Complex surveys: A guide to analysis using R*. Wiley.
- Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica, 46*(1), 69-85.
- OCHA. Mali: Subnational administrative boundaries (COD-AB), levels 0 to 3. Source: Direction Nationale des Collectivités Territoriales (DNCT), 2021. Humanitarian Data Exchange.
- Raleigh, C., Linke, A., Hegre, H., & Karlsen, J. (2010). Introducing ACLED: An armed conflict location and event dataset. *Journal of Peace Research, 47*(5), 651-660.
- Zeger, S. L., Liang, K.-Y., & Albert, P. S. (1988). Models for longitudinal data: A generalized estimating equation approach. *Biometrics, 44*(4), 1049-1060.
