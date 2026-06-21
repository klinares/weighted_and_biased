# weighted_and_biased

![Design-weighted latent class analysis](images/banner.svg)

A single, adaptable R/Quarto script for **latent class analysis of complex survey data** that respects the sampling design while estimating the classes and reports **design-based uncertainty**, not the simple-random-sample kind.

> The weights decide what the population looks like. The design decides how sure you are allowed to feel. You need both, and most tooling hands you one.

## Why this exists

Latent class analysis recovers hidden respondent types from categorical items. Doing it correctly on survey data is two separate jobs: weighting the estimation so the classes describe the population rather than the achieved sample, and computing standard errors that account for stratification and clustering instead of pretending the sample was independent. These jobs are independent. The weights can leave the point estimates almost unchanged while the honest standard errors are still inflated by the design effect.

No R package does both. `poLCA` cannot take sampling weights at all. The survey-aware mixture tools that exist (Mplus, Latent GOLD, Stata `gsem ..., lclass()`) are not free, not R, or not easily dropped into a reproducible pipeline. This repository fills the gap with a hand-rolled weighted EM and design-based replicate variance, written to be read and adapted rather than trusted blindly.

The bundled worked example classifies respondents by their openness to government negotiations, but the topic is incidental. Everything specific to a survey lives in one configuration block; swap that block and the method applies to any categorical survey instrument.

## Four methodological commitments

1.  **The design is respected during estimation, not bolted on afterward.** Prevalences and item-response probabilities are estimated by weighted pseudo-maximum-likelihood EM, the analogue of `svyset psu [pweight = w], strata(stratum)` in Stata.
2.  **`poLCA` is the unweighted benchmark.** Because it cannot weight, the gap between it and the weighted fit measures how informative the design is for this typology, and a unit-weight equivalence check confirms the hand-rolled EM reproduces `poLCA` exactly when every weight is 1.
3.  **Uncertainty is design-based.** Confidence intervals come from the stratified jackknife: the design is declared, converted to replicate weights, and the entire weighted EM is refit on each replicate. Intervals use a t critical value on the design degrees of freedom (PSUs minus strata), not 1.96.
4.  **"Don't know" is a substantive response category,** not missing data. Every respondent is classified, and each class reports its DK propensity separately.

## What the pipeline does

- **Weighted EM estimator.** A from-scratch pseudo-ML EM with a numerically stable log-sum-exp E-step and a weighted M-step, written as a `purrr` fold with no `for` or `while` loops.
- **Label alignment.** Every refit (the benchmark and each replicate) is aligned to a reference labeling by the Hungarian algorithm (`clue::solve_LSAP`), so "Class 1" means the same thing everywhere and the jackknife does not label-switch.
- **Model selection.** BIC-based enumeration over a candidate range, with a manual override knob for when parsimony or interpretability should win over the BIC minimum.
- **Design-based variance.** `survey::withReplicates` over JKn replicates, with `nest = TRUE` and the lonely-PSU adjustment.
- **Local-independence check.** Design-weighted bivariate residuals (BVR) for every item pair, the diagnostic for whether the conditional-independence assumption actually holds at the chosen number of classes.
- **Three-step profiling.** A design-based multinomial regression of class membership on demographics, refit on each replicate so the coefficient standard errors are honest about the design.

## Honest limitations

This is a reference implementation, and it states where it is approximate rather than hiding it.

- **Profiling is the naive three-step.** It regresses the modal class, which ignores classification error and biases the covariate associations toward zero with standard errors that are too small (Bolck, Croon, and Hagenaars 2004). The posteriors are written back to the data, so a BCH or ML bias correction (Vermunt 2010; Bakk, Tekle, and Vermunt 2013) is the recommended upgrade and is not yet implemented.
- **The BVR reference is approximate under clustering.** The chi-square reference ignores the covariance among expected cell counts and the design effect, so treat it as a screen, not a calibrated test.
- **Information criteria are not weight-scale-invariant.** If your weights sum to the population rather than the sample, BIC will over-extract; normalize the weights to sum to `n` for the IC computation. Point estimates and confidence intervals are unaffected.
- **Use shipped replicate weights when you have them.** If your survey provides BRR, JKn, or bootstrap replicate weights, route them through `svrepdesign()` rather than rebuilding JKn from strata and PSU. Public-use files usually mask the true design, and the provided weights also carry any calibration the producer applied.

## Requirements

- R (4.2 or newer for the native pipe) and Quarto.
- Packages: `tidyverse`, `survey`, `poLCA`, `clue`, `nnet`, `matrixStats`, `rlang`.
- For PDF output, a LaTeX engine (TinyTeX is sufficient). To build without TeX, switch the format block to Typst, which Quarto bundles.

## Usage

1.  Open `survey_lca_analysis.qmd`.
2.  Edit the single `cfg` block: your item columns, your design columns (`strata`, `psu`, `weight`), the demographics for profiling, and the candidate number of classes. Point `cfg$data` at your data frame.
3.  Confirm `cfg$weight` is the final analysis weight (base weight times nonresponse and calibration adjustments), not the raw inclusion weight alone.
4.  Render. For real data, delete the worked-example simulation and the known-truth comparison sections; both are marked in the script.

``` r
quarto::quarto_render("survey_lca_analysis.qmd")
```

## Selected references

- Pfeffermann, D. (1993). The role of sampling weights when modeling survey data. *International Statistical Review*, 61(2), 317-337.
- Asparouhov, T. (2005). Sampling weights in latent variable modeling. *Structural Equation Modeling*, 12(3), 411-434.
- Linzer, D. A., and Lewis, J. B. (2011). poLCA: An R package for polytomous variable latent class analysis. *Journal of Statistical Software*, 42(10), 1-29.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R.* Wiley.
- Nylund, K. L., Asparouhov, T., and Muthen, B. O. (2007). Deciding on the number of classes in latent class analysis and growth mixture modeling. *Structural Equation Modeling*, 14(4), 535-569.
- Bolck, A., Croon, M., and Hagenaars, J. (2004). Estimating latent structure models with categorical variables. *Political Analysis*, 12(1), 3-27.
- Vermunt, J. K. (2010). Latent class modeling with covariates: Two improved three-step approaches. *Political Analysis*, 18(4), 450-469.
- Visser, M., and Depaoli, S. (2022). A guide to detecting and modeling local dependence in latent class analysis models. *Structural Equation Modeling*, 29(6), 971-982.
