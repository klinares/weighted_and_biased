# weighted_and_biased

Design-based variance estimation for latent class analysis (LCA) of complex
survey data. One stratified jackknife (JKn) replicate design generates every
standard error in the analysis: the measurement model parameters, the segment
prevalences within demographic domains, the bias-adjusted three-step
correction, and the contrasts between domain levels. Parameters are estimated
by design-weighted pseudo-maximum likelihood via EM, the number of classes is
always the analyst's decision, and the output is a labelled data product for
downstream analysts.

Throughout, a latent class is called a **segment**: a word-choice preference,
the statistical object is unchanged.

Full methodology and operating documentation, with every estimator stated as
implemented and citations verified, is in `survey_lca_methods.qmd`.

## Why this exists

Fitting a latent class model to survey data is not the hard part. Reporting
honest uncertainty is. Point estimates from a design-weighted pseudo-likelihood
are available in several packages; standard errors that respect stratification
and clustering, carried through from the measurement model to domain estimates
and covariate contrasts, are not available jointly in R.

Nothing here is computed under an independence assumption at any stage. The
same replicate weights that produce the interval on a conditional response
probability produce the standard error of a contrast between two demographic
levels.

The closest work is `baysc` (Wu, Williams, Savitsky, and Stephenson 2024,
*Biometrics* 80(4) ujae122; package version 0.1.0 at
https://github.com/smwu/baysc), which fits a weighted pseudo-likelihood in a
Bayesian pseudo-posterior with a post-hoc variance adjustment. It selects the
number of classes automatically through an overfitted mixture, obtains
uncertainty from an adjusted posterior rather than replicate weights, and needs
no BCH correction because Bayesian estimation propagates classification
uncertainty through the posterior draws. It is currently distributed through
GitHub and requires a C++ and Fortran toolchain, which places it outside what
can be installed in the deployment environment this pipeline serves. CRAN
availability is anticipated; when it arrives it becomes the natural Bayesian
comparator for this work.

Mplus (`TYPE = MIXTURE COMPLEX`) and Stata (`gsem` under `svy`) maximize the
same objective with the same weight convention, so point estimates and
information criteria match up to optimizer tolerance and mode selection; their
standard errors are linearization-based where these are replication-based, both
design-consistent. The unweighted special case is validated at runtime against
poLCA.

## Repository layout (four files, run in this order)

1. **`survey_lca_source.R`** - the engine, never edited per dataset. Weighted
   EM with a log-sum-exp E-step and weights in every sufficient statistic,
   Hungarian label alignment, parallel JKn replicate variance, the BCH
   correction, NA-tolerant segment scoring with an information floor, item
   discrimination and bivariate residuals on a bounded scale, plotting and
   table helpers, and the LLM labeling instrument.

2. **`survey_data_config.R`** - the ONLY per-dataset file. Reads the source
   file with `user_na = TRUE`, recodes nonresponse, recodes items to
   consecutive integers on every row, recodes demographics with exhaustive
   `case_match` and an assertion, builds the dictionary and the recode audit,
   and defines `cfg`. Design columns are renamed to fixed names (`id`,
   `strata`, `psu`, `wt`) so nothing downstream varies by dataset. Sourced by
   both Quarto documents, so both always see identical data.

3. **`survey_lca_modeling.qmd`** - the heavy half. Missingness plot, recode
   audit, item dictionary, weight diagnostics, enumeration over `cfg$K_range`
   (AIC, BIC, BIC on the effective sample size, entropy; NOTHING
   auto-selected), and once `cfg$K_force` is set: the chosen model, item
   discrimination, the indicator screen, bivariate residuals with heatmap, JKn
   confidence intervals for all parameters, the poLCA benchmark with a printed
   verdict, and `model_fit.rds`.

4. **`survey_lca_segments.qmd`** - the fast half. Reads the fit, prints the
   labeling instrument verbatim, attaches labels under one rule, presents the
   measurement model, scores every respondent above the floor and rebuilds the
   design on that larger frame, reports design effects against SRS and
   clustering-only benchmarks, produces per-demographic domain estimates
   (design-based and BCH-corrected, with intervals) and contrasts, and writes
   the handoff.

Companion tools: `llm_label_obedience_experiment.R` (instrument
certification) and `audit_object_flow.R` (walks the Quarto documents in chunk
order and flags any symbol used before assignment).

## The workflow

Render the modeling document with `cfg$K_force = NULL`: it stops after
enumeration. Read AIC, BIC, BIC on the effective sample size, and entropy
together; set a candidate K; re-render; judge that model's discrimination,
bivariate residuals, and profiles; iterate until the choice holds. K is the
analyst's decision, always. Then render the segments document; its first run
drafts and freezes the labels and every later run reuses them.

Re-render the modeling document before the segments document after any config
change. `model_fit.rds` carries no staleness check.

## Reproducibility

Starting values are generated in the main session from a deterministic seed
sequence and passed to the workers as data. No worker touches the random
number generator, so results are identical under sequential and parallel plans
and under any number of workers. The pipeline asserts this at runtime by
refitting the chosen model sequentially and comparing log-likelihoods.

For segment labels the reproducibility mechanism is a freeze file, not a seed.
When `segment_labels.csv` exists in `cfg$out_dir` it is used and validated
against K; otherwise the model drafts once and writes it. Editing that file IS
taking over naming; deleting it triggers a redraft. A seed would be
reproducible only for a fixed model, endpoint, and package version.

## Segment labels (one rule)

One isolated LLM call per segment (joint prompts empirically confused
near-neighbor segments), an optional survey-context paragraph fenced to
referent resolution only, and a collision-gated harmonizer that may edit
labels, never descriptions. The instrument is certified by the obedience
experiment; any edit obliges a re-run, and each deployment endpoint gets one
certification pass before production use. Labels never re-enter estimation:
they are display strings attached after every number is computed, so a wrong
label is a presentation error and not a statistical one. Verify each against
its response-profile panel before quoting it.

## Keys

No secret appears in any file. At home, `OPENROUTER_API_KEY` in `.Renviron` and
`cfg$compass_base_url = NULL`. At work, set `cfg$compass_base_url` and
`cfg$llm_model`; `COMPASS_API_KEY` is bridged to the variable name ellmer
expects.

## Statistical decisions (each argued where used)

- **Variance is the point.** JKn replicates drive every standard error, and the
  same replicate covariance supplies the contrasts. Replication is a choice,
  not a necessity: it propagates to new statistics without rederivation, it
  avoids inverting a several-hundred-dimensional information matrix with
  parameters at the boundary, and it makes the design specification auditable.
- **Design effects are a specification check, not decoration.** A covariate
  constant within a cluster must have a design effect equal to the mean cluster
  size. If it does, the cluster identifier and the variance formula are jointly
  correct. A design effect below one indicates quota control at the final
  selection stage; those variables' precision is a fieldwork artifact.
- **Singleton strata are a hard stop.** `survey.lonely.psu` governs
  linearization, not replicate construction, so a singleton would silently
  contribute zero variance. The check runs on the analysis frame, since case
  exclusions can create singletons that the source file does not have.
- Weights enter every EM step; estimates describe the population.
- Information criteria rescale the log-likelihood to the sum-to-n scale;
  without it BIC leans toward too many segments. A second criterion using the
  Kish effective sample size is reported beside it so the disagreement is
  visible rather than hidden by a default.
- BLRT omitted: its parametric bootstrap presumes independent observations.
- The measurement model is fit on item-complete cases; scoring and all domain
  estimation run on the larger frame of everyone above `cfg$min_items`, because
  item nonresponse is not random and excluding those respondents biases the
  composition being estimated.
- Parameter and domain intervals are delta-method on the logit scale with the
  design t, falling back to truncated Wald at the boundary.
- Bivariate residuals are total variation distance between the observed and
  model-implied two-way table: bounded in [0, 1], zero under exact local
  independence, no division by a possibly-empty expected cell. No reference
  distribution is valid under a design-weighted pseudo-likelihood, so this
  ranks rather than tests.
- BCH is reported alongside the uncorrected design-based estimates rather than
  instead of them. Report the uncorrected numbers when the shift is small
  relative to its standard error; the correction protects null findings, not
  positive ones, because attenuation always understates group differences.
- The `.sav` carries posteriors and BCH weights, not just the modal
  assignment. Cross-tabulating the assignment alone reintroduces the
  attenuation the correction removes, and the variable label says so.

## Data acknowledgment

The demonstration application uses the 2023 AmericasBarometer for Mexico by the
LAPOP Lab at Vanderbilt University; obtain the data from LAPOP under their
terms. Nothing here redistributes data. Note that this file is self-weighting,
so the weighted and unweighted fits coincide and the demonstration exercises
the variance machinery rather than the weighting machinery.
