# Modeling Notes: Violence and Leader Favorability (Mali)

Reference for this project's models: what was decided, why, how to read the output, and how to work on the code. Give this file to an AI assistant as context (section 9 tells it how to behave).

---

## 1. Study and data

**Question.** Is violence against civilians in a respondent's commune associated with favorable views of the national leader, comparing people in the same region, urban or rural area, and survey wave?

**Data.**
- Three survey waves, each an independent sample. Strata: region x urban (15; Bamako is urban only). Communes are the first and only sampling stage; every commune has 8 respondents. Weights exist only at the respondent level (no commune selection weights).
- Outcome: favorable view (1/0). No item nonresponse in the file; about 80% favorable.
- Exposure: ACLED event counts per commune and wave over the survey window, `violence_civilian` (main) and `violence_all`. Only surveyed communes have counts. No perpetrator field.
- Geography: region, cercle (from the OCHA dictionary via the commune key), commune. About 140 communes per wave, many repeated across waves.

**Status on the real data (as reported by Kevin).** M1 ICC about 17%; random violence slope dropped (only about 7% of violence varies within communes); M3a beat M3b in the earlier sequence; violence coefficient about -0.10, p = 0.60.

---

## 2. Files and workflow

| File | Role |
|---|---|
| `analysis.qmd` | **Config list `cfg`** (data file, violence count, covariates, `cercle` and `strata` switches, event levels, model folder, `refit`), loads packages, sources the scripts, presents results |
| `R/prep_data.R` | Builds `ad` (about 30 lines): IDs, `v = log1p(violence)`, cercle split, wave-specific design codes, rescaled and log weights |
| `R/models.R` | M1a to M3, svyglm check, cercle-split sensitivity, tests, predictions, calibration; saves models to `models/` |
| `R/maps.R` | Violence map by commune; M3 predicted favorability by cercle and by commune; saved to `output/maps/` |
| `R/boundaries.R` | Name cleaning and boundary readers |
| `boundaries/` | OCHA layers and admin dictionary. **Never edit the dictionary** (used for matching at work) |

**Data contract (prepared upstream).** Columns named exactly: `wave`, `region`, `urban`, `strata`, `commune`, `cercle`, `fav` (1/0), `weight`, `adm1_key`, `adm3_key`, the violence count named in `cfg$violence`, and the covariates in `cfg$covariates`. Factors arrive with their reference level first (region: Bamako; strata: Bamako urban; wave: 1); numeric covariates arrive centered. Cercle names must match the OCHA cercle names (accents and case do not matter).

**Choosing the structure.** Render once, read the M1 (cercle) and M2 (strata) tests in the model comparison table, then set `cfg$cercle` and `cfg$strata` and render again with `refit = TRUE`. The rules: keep the cercle if the boundary-corrected p < .05, AIC is lower, and the fit is not singular; use strata if p < .05 and AIC is lower.

**Model caching.** `fit_glmer()` loads `models/<name>.rds` if it exists, otherwise fits and saves it. It does **not** notice changed data or formulas: set `cfg$refit = TRUE` (or delete `models/`) after changing the data, covariates, or switches, otherwise the report will use stale models.

**Packages.** `lme4`, `survey`, `marginaleffects`, `performance`, `broom.mixed`, `modelsummary`, `tinytable`, `sf`, `viridis`, `patchwork`, `knitr`, `tidyverse`. Main-text tables use `knitr::kable`; the appendix uses `modelsummary` + `tinytable` (native Typst). Typst tables cannot split across pages, so long tables are kept to one row per term.

## 3. Measures

- $V_{cw} = \log(1 + E_{cw})$. 0 events -> 0; +1 multiplies $(1 + E)$ by 2.72; +0.69 doubles it. Report predicted favorability at 0, 1, 5, 20 events.
- **Cercle split (sensitivity).** $\bar V_{kw}$ = mean of $V$ across surveyed communes of cercle $k$ in wave $w$; within = $V_{cw} - \bar V_{kw}$. A cercle mean is used, not a sum: with 30-40% of communes surveyed, a sum would mostly reflect how many communes were sampled.
- **Civilian vs all violence.** Swap `cfg$violence` between `violence_civilian` and `violence_all` and refit. A ratio (civilian / all) was rejected: undefined at 0 events and unstable with few events.
- **Centering on region or commune** (client question): does not change significance in a useful way. Region centering is what the region dummies already do; commune centering uses only the ~7% within-commune variation and loses precision. Centering is not an alternative to the log; it changes which comparison the coefficient makes.

---

## 4. Models

All `lme4::glmer`, logit, ML (Laplace), `nloptwrap` with tight tolerances (about 10x faster than `bobyqa`, same log-likelihood), unweighted, same complete-case respondents.

| Model | Specification | Test |
|---|---|---|
| M1a | $\gamma_0 + u_c$ | |
| M1b | $\gamma_0 + u_c + r_k$ (communes in cercles) | `anova(m1a, m1b)`, p halved (boundary), AIC, BIC; keep if p < .05, lower AIC, not singular |
| M2a | M1 winner + $\beta V$ + wave + region + urban + covariates | |
| M2b | M2a with region + urban replaced by the 15 strata | `anova(m2a, m2b)` (nested), AIC, BIC; strata if p < .05 and lower AIC |
| M3 | M2 winner + centered log weight | final model |
| svyglm | M2 winner's fixed effects; strata x wave, communes as PSUs, weights | design-based check, compare by z |
| Cercle split | M3 with $\bar V_{kw}$ and $V - \bar V_{kw}$ | sensitivity |

**ICCs (latent scale, $\pi^2/3$ for the individual level).**
$\rho_{\text{cercle}} = \tau_k / (\tau_k + \tau_c + \pi^2/3)$: same cercle, different commune.
$\rho_{\text{commune}} = (\tau_k + \tau_c) / (\tau_k + \tau_c + \pi^2/3)$: same commune.
`performance::icc()` gives only the combined value; the code computes both.

**Why these choices.**
- Region, urban, wave stay fixed: 3 waves cannot support a variance; strata are design strata; a random region or strata intercept assumes no correlation with violence, which fails (north: more violence, lower favorability) and lets between-region confounding into $\beta$ (Bell & Jones, 2015).
- Cercle is added, not substituted: violence and sampling are at the commune, so dropping the commune intercept understates the SE of a commune-level exposure.
- No village level: villages are not part of the design.
- No random slope: identified only from within-commune change (~7%); removed for simplicity.
- Weights: `glmer` cannot take sampling weights (its `weights` argument means binomial trials). `WeMix` (weighted multilevel) needs commune selection weights, which are not available. So M3 includes the centered log weight as a covariate (Pfeffermann et al., 1998; Gelman, 2007), and `svyglm` is the design-based check.
- `svyglm` coefficients are population-averaged, smaller than conditional ones by about $\sqrt{1 + 0.346\,\tau}$ (Zeger, Liang & Albert, 1988): compare by z. Never compare it with `glmer` by AIC or likelihood ratio tests.

---

## 5. Reported quantities

- Violence coefficient, 95% CI, p.
- Predicted favorability at `cfg$event_levels`: `avg_predictions(..., variables = list(v = log1p(levels)))`.
- **0 vs 5 events** with 95% CI: `avg_comparisons(m3, variables = list(v = c(0, log1p(5))))`. This is the plain-language range.
- **Minimum detectable effect** (80% power, alpha .05): $2.8 \times SE(\beta)$, converted to percentage points with mean $\hat p(1-\hat p)$, times $\log 6$ for 0 vs 5 events. Approximate; tells clients what size of effect the study could have found.
- Predicted favorability by variable: wave and factor covariates by counterfactual (everyone set to each level); strata by the average prediction for people in each stratum (no prediction for strata that do not exist).
- Maps: commune map uses `predict(m3, type = "response")` (commune and cercle effects), weighted mean per commune-wave. Cercle map uses `re.form = ~(1 | cercle_id)` (cercle effect only), weighted mean over the cercle's respondents; it describes the surveyed communes of the cercle, not the whole cercle. For policymakers, lead with the cercle map; single communes rest on 8 respondents.

---

## 6. Reading a null result (the current real-data situation)

- p = 0.60 means the data cannot distinguish the association from zero. It does not show there is no effect.
- Lead with the 0 vs 5 events CI in percentage points ("effects larger than X points are unlikely") and the minimum detectable effect ("the study could reliably detect effects of about Y points or more").
- Near 80% favorable, the logistic slope is $p(1-p) \approx 0.16$ (vs 0.25 at 50%), so the same log-odds effect moves the percentage less.
- Likely reasons the signal is weak: violence varies mostly between communes (few comparisons within a place over time); no perpetrator split (state vs armed-group violence may push in opposite directions); sensitive outcome under a military government (over-reporting of favorability compresses differences); ACLED undercounting in remote areas (attenuation); coverage of insecure areas.
- **Do not** search over exposure forms, windows, subsets, or interactions until something is significant. Any added specification must be pre-stated and labeled exploratory, and all versions reported.

**Enhancements still open (ask before adding):** neighboring-commune violence from OCHA adjacency; one or two pre-specified interactions (for example violence x ethnicity, violence x north/center); a lagged event window if dates are available.

---

## 7. Diagnostics

| Check | Good | Warning |
|---|---|---|
| ICC | clearly above 0 | near 0 |
| Cercle test | follows the rule | singular M1b |
| Log-weight coefficient | |z| < 1.96 | clearly nonzero: informative sampling |
| svyglm z vs M3 z | similar | much weaker: weights or clustering matter |
| Cercle split | within and between similar in sign | opposite signs |
| VIF | below about 2 | high |
| Calibration (simulation-based) | observed inside simulated range | many bins outside |
| Convergence | `m3@optinfo$conv$lme4$messages` is NULL | gradient warnings: check estimates are stable; compare with `optimizer = "bobyqa"` |

Binned residuals from `performance::binned_residuals()` were replaced: with 8 people per commune, residuals built on shrunken random effects show a false slope even when the model is correct (verified on simulated data). The calibration check simulates 200 data sets from M3 with new commune and cercle effects.

---

## 8. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `Run the config chunk` error | Script sourced without the config | Run the config chunk or copy `cfg` into the console |
| Model refits every render | `refit = TRUE` left on | Set `refit = FALSE` after one refit |
| Survey communes not placed on the map | Region, cercle, or commune key does not match OCHA | Check spelling of the cercle and commune upstream |
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

1. **Keep the estimand**: the M3 violence coefficient (typical commune and cercle), with the svyglm check. Do not change exposure, model sequence, decision rules, or fixed/random structure without stating the consequence.
2. **Never pass sampling weights to `glmer`**, never add random effects to `svyglm`, never compare them by AIC or likelihood ratio tests.
3. **Variance tests are boundary tests**: halve the `anova()` p-value when adding one variance.
4. **Report percentage points and predicted percentages** for clients; treat a non-significant result as a CI and a minimum detectable effect, not as "no effect".
5. **No specification searching.** New specifications are exploratory and reported in full.
6. **Protect the maps**: never place communes whose key is shared within a region; gray is "not surveyed", never zero.
7. **Do not edit `boundaries/mali_admin_dictionary.csv`.**
8. **Code style**: tidyverse, native pipe, `package::function()`, no `for` loops (use `purrr`), `=` inside function bodies, single space after `<-`, comments that explain why in plain words, settings only in `cfg`.
9. **Be honest about uncertainty**: separate established practice, defensible choices, and guesses. Never invent function arguments, results, or citations; verify by running code.

---

## 10. Validation (simulated data)

The pipeline was checked on simulated data with the same layout and design (15 strata, 8 respondents per commune, about 136 communes per wave). Truth: violence -0.25, cercle variance 0.20, commune variance 0.45, no region-specific urban gap, uninformative weights. Recovered: violence -0.248 (truth inside the 95% CI); cercle level kept (boundary p = 0.006); strata not needed (p = 0.07); log weight near zero; calibration inside the simulated range. The simulation scripts were removed after testing; `data/survey_sim.rds` remains as an example of the expected file layout.

## References

Bell & Jones (2015) *PSRM* 3(1):133-153. Binder (1983) *ISR* 51(3):279-292. Gelman (2007) *Statistical Science* 22(2):153-164. Mundlak (1978) *Econometrica* 46(1):69-85. OCHA COD-AB Mali, DNCT 2021, HDX. Pfeffermann, Skinner, Holmes, Goldstein & Rasbash (1998) *JRSS-B* 60(1):23-40. Raleigh, Linke, Hegre & Karlsen (2010) *JPR* 47(5):651-660. Zeger, Liang & Albert (1988) *Biometrics* 44(4):1049-1060.
