# weighted_and_biased

Design-based latent class analysis for complex survey data, with integrated
replicate variance estimation and LLM-assisted (analyst-controlled) class
labeling. Current application: institutional trust in Mexico, 13 items from the
2023 LAPOP AmericasBarometer.

## What this pipeline is for

Given categorical survey items from a stratified, clustered, weighted sample,
the pipeline estimates a population-level latent class measurement model and
carries the design all the way through: enumeration, parameter confidence
intervals, class assignment for item-partial respondents, and design-based
demographic profiling, all under one replicate-weight system. The measurement
model is estimated by a design-weighted pseudo-maximum-likelihood EM; every
downstream uncertainty statement comes from stratified jackknife (JKn)
replicate weights.

## Positioning and related work

The closest existing software is `baysc` (Wu, Williams, Savitsky and
Stephenson), which fits the same weighted pseudo-likelihood in a Bayesian
pseudo-posterior and corrects its uncertainty with a sandwich-based rescaling
of the draws (Wu et al. 2024, Biometrics 80(4), ujae122; JOSS 2026, 11(119),
doi:10.21105/joss.08382). We verified against their released code that the
likelihood kernel and the weight normalization are identical to this
pipeline's, and benchmarked both on simulation and on their NHANES example:
point estimates agree closely, with residual differences attributable to
posterior-median-versus-mode asymmetry and to near-tied local optima (see
Known properties below). This pipeline's niche relative to baysc is the
frequentist route: replicate-based variance that is carried through to
design-based domain estimation on auxiliary variables.

Relative to mainstream software: Mplus (TYPE = MIXTURE COMPLEX) and Stata
(gsem with svy) maximize the same weighted pseudo-likelihood with the same
weight-to-n normalization convention, so point estimates and information
criteria should match up to optimizer tolerance and local-optimum selection;
their standard errors are linearization-based where ours are replication-based
(both design-consistent, typically close). Mplus disables the bootstrapped LRT
under complex designs, matching this pipeline's explicit BLRT omission.
Unweighted estimation is validated internally against poLCA (Linzer and Lewis
2011, JSS 42(10)) at unit weights. Information criteria under complex sampling
follow the weight-normalization logic discussed by Lumley and Scott (2015,
Journal of Survey Statistics and Methodology 3(1)).

## Repository layout (two files)

- `survey_lca_source.R`: the dataset-agnostic engine. Item preparation
  (`prepare_items()`), the weighted EM (logsumexp E-step, multi-start with
  deterministic seeding), label alignment (Hungarian matching), the
  bivariate-residual diagnostic, and the LLM class-labeling functions. Dense
  by design and never edited per dataset.
- `survey_lca_analysis.qmd`: the analysis document (Quarto, LaTeX PDF). Data
  cleaning is FRONT-LOADED and transparent: two ordinary dplyr chunks the
  analyst owns read the .sav, recode nonresponse to NA, recode demographics,
  flag the complete-case analysis sample, strip nonresponse value labels, and
  build the dictionary; everything after them is machinery. K is the
  analyst's decision (`cfg$K_force`), made by iterating over the enumeration
  and per-K diagnostics; nothing runs past enumeration until it is set. Class
  labels follow one rule: `outputs/llm_labels.csv` is used when it exists,
  otherwise the LLM drafts it once and writes it (editing the file is taking
  over the naming). Once K is set and labels exist, membership is attached to
  the preserved original frame and written as a labelled SPSS file
  (`survey_with_classes.sav`) for analysts; complete cases are assigned,
  everyone else is honestly `NA`.

## Methods summary

- Estimation: weighted pseudo-ML via EM; weights enter every E- and M-step.
  Point estimates are invariant to weight scale; likelihood printouts and
  information criteria rescale weights to sum to n.
- Enumeration: weighted BIC across a K range, read with entropy and an
  unweighted poLCA cross-check. The BLRT is deliberately omitted: its
  parametric bootstrap presumes independent observations, which fails under a
  design-weighted pseudo-likelihood on a stratified clustered sample.
- Local independence: a design-weighted bivariate residual (BVR) per item
  pair, reported as a descriptive ranking index (no chi-square reference is
  valid here); guidance in the document covers drop/merge/K+1 responses.
- Variance: JKn replicate weights built from the design (strata, PSUs);
  parameter CIs on the logit scale; the same replicates drive all domain
  estimation.
- Missingness: complete cases only, flagged in the cleaning chunk; a
  posterior class requires all items, so incomplete cases are never assigned
  (NA in the SPSS handoff) rather than imputed silently.
- Profiling: design-based class prevalence within demographic levels using
  soft posteriors (the design-weighted mean posterior in the subpopulation),
  with JKn intervals on the logit scale. This is a domain-estimation design
  choice, not a three-step regression (R3STEP/BCH users: expect agreement in
  direction, not magnitude; different estimand).

## Class labeling (LLM-assisted, analyst-controlled)

Class numbers are replaced by drafted labels used in every downstream table
and figure. Design points, each validated empirically:

- One model call per class (joint prompts confuse near-neighbor classes; a
  tested global-context stage changed nothing).
- Prompts carry each item's wording (via the dictionary) and its response
  labels built per item from its own observed values, a value's label when one
  exists and its number otherwise, so items with different scales,
  anchors-only labeling, and unlabeled items all work without configuration;
  an optional one-sentence `survey_context` is fenced to referent resolution
  only.
- A harmonization pass runs only when a mechanical collision check fires,
  edits labels only, never descriptions, and records pre-edit drafts in the
  freeze file for audit.
- Precedence: an analyst CSV (`K, Label, Description`, requires a pinned K)
  always wins and the LLM never runs; otherwise a frozen
  `class_labels_llm.csv` in `out_dir` is reloaded; only failing both does the
  LLM run once, then freeze. The freeze file, not the seed, is the
  reproducibility mechanism (cloud sampling is not bit-reproducible even at
  temperature 0 with a fixed seed; class ORDER is pinned by the seed).
- Certification: the obedience experiment plants a known-meaning model
  (near-neighbor classes, a diffuse class, nonresponse-labelled items, a
  fictional survey context) and checks JSON compliance, grounding, hedging on
  diffuse profiles, persona integrity, context-leak fencing, and the
  harmonizer's properties. Certified to date: gemma4-31b and llama4 on the
  pre-context instrument; the current instrument requires a re-run, and each
  deployment endpoint should be certified once before first production use.
  Any edit to the persona, rules, or prompt structure obliges a re-run.
- Labels are drafts: the analyst verifies each against its response profile
  before quoting, and takes over by editing the frozen CSV.

## Known statistical properties (read before interpreting)

The weighted pseudo-likelihood for LCA can be multimodal with near-tied optima
at moderate n, especially with diffuse classes: on an 8-item simulation the
top modes differed by ~1 log unit with prevalence differences up to 0.23, and
on NHANES (28 items) 30 starts found ~28 distinct optima whose reportables
were nevertheless ridge-stable. Consequences adopted here: many random starts
with deterministic seeding; the honest claim is "best mode found", not "the
MLE"; and JKn replicates, being warm-started, express within-mode variance
only. The domain estimates treat the fitted measurement model as given; the
replicate intervals carry design variance, not the uncertainty of pi and rho
themselves.

## Configuration and keys

Everything an analyst sets lives in one `cfg` block: items, design variables,
K range or a forced K, output directory, the LLM endpoint
(`compass_base_url`, `llm_model`) and the environment variable naming the API
key (`llm_key_env`; set NULL at work to use the .Rprofile-supplied default).
No secret appears in any file. A fail-fast check stops the render in seconds
if labeling will need the LLM and the key variable is empty.

## Reproducibility

One master seed (2026) drives the EM starts, the poLCA starts, and the LLM
calls; the label freeze pins the class-label binding across renders; all
exports (enumeration, measurement model with CIs, class labels, case-level
assignments) are written to `cfg$out_dir`.

## Data acknowledgment

The current application uses the 2023 AmericasBarometer for Mexico by the
LAPOP Lab at Vanderbilt University; obtain the data from LAPOP under their
terms. The adapter never redistributes data.
