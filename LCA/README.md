# weighted_and_biased

Design-weighted latent class analysis (LCA) for complex survey data, with
stratified jackknife (JKn) replicate variance, analyst-driven model selection,
LLM-drafted (analyst-controlled) segment labels, and a labelled SPSS handoff.
Throughout, a latent class is called a **segment**: a word-choice preference;
the statistical object is unchanged. Current application: institutional trust
in Mexico, 13 items from the 2023 LAPOP AmericasBarometer.

Full methodology, with every estimator stated as implemented and citations,
is in `survey_lca_methods.qmd`.

## Why this exists

No R package jointly provides design-weighted LCA point estimation, replicate
variance carried through to domain estimation, honest assignment for
item-partial respondents, and a labelled data product. The closest work is
`baysc` (Wu et al. 2024, Biometrics 80(4) ujae122; JOSS 2026 11(119)), which
fits the same weighted pseudo-likelihood in a Bayesian pseudo-posterior; this
pipeline is its frequentist counterpart. Mplus (TYPE = MIXTURE COMPLEX) and
Stata (gsem under svy) maximize the same objective with the same weight
convention, so point estimates and information criteria match up to optimizer
tolerance and mode selection; their standard errors are linearization-based
where ours are replication-based, both design-consistent. Mplus disables the
bootstrapped LRT under complex designs, matching this pipeline's explicit
omission. The unweighted special case is validated at runtime against poLCA.

## Repository layout (four scripts, run in this order)

1. `survey_lca_source.R` - the engine, never edited per dataset: weighted EM
   (logsumexp E-step, weights in every sufficient statistic, multi-start with
   deterministic seeding), Hungarian label alignment, item discrimination and
   bivariate residuals, JKn machinery hooks, NA-tolerant segment prediction
   with a minimum-items floor, and the certified LLM labeling instrument.
2. `survey_data_config.R` - the ONLY per-dataset file: transparent dplyr
   cleaning (read the .sav, recode nonresponse to NA, recode demographics,
   flag complete cases, strip nonresponse value labels so every remaining
   label is a response), the dictionary (built from haven's base attributes,
   robust to accessor behavior), and `cfg` (items, design variables, K_force,
   starts, seed, prediction floor, output folder, LLM endpoint and model,
   survey context). Sourced by both quartos, so both always see identical
   data.
3. `survey_lca_modeling.qmd` - the heavy half: raw-data missingness plot,
   two-page dictionary, enumeration over `cfg$K_range` (BIC and AIC on the
   sum-to-n scale, entropy, inclusion probabilities; NOTHING auto-selected),
   and, once the analyst sets `cfg$K_force`, the chosen model with its
   report, item-discrimination table and plot, BVR table and heatmap, JKn
   confidence intervals, the poLCA benchmark with a printed validation
   verdict, and a save step: model objects to .rds, tables to .parquet
   (arrow), in `cfg$out_dir`.
4. `survey_lca_segments.qmd` - the fast half, requiring `cfg$K_force`:
   reads the saved objects, prints the labeling instrument verbatim (system
   prompt plus a real segment-1 prompt, annotated), attaches segment labels
   under one rule, presents inclusion probabilities with JKn intervals
   alongside poLCA and the match verdict, the conditional-probability
   heatmap, segment prediction back to the full frame with a one-line
   summary (complete and incomplete cases predicted, floor, mean maximum
   posterior), per-demographic domain tables and plots (one page each), and
   the handoff: `survey_with_segments.sav` (numeric segment with value
   labels, NA below the floor), plus CSVs of the measurement model and the
   demographic results.

Companion tools: `llm_label_obedience_experiment.R` (instrument
certification; see Labeling) and `audit_object_flow.R` (walks the quartos in
chunk order and flags any symbol used before it is assigned; run it after
any edit, noting that package exports can mask a missing object, which is
why the segment_* naming convention is kept strictly).

## The workflow

Render the modeling script with `K_force = NULL`: it stops after enumeration
with instructions. Read BIC, AIC, entropy, and inclusion probabilities; set
a candidate K; re-render; judge that model's discrimination, BVR, and
profiles; iterate until the choice holds. K is the analyst's decision,
always. Then render the segments script; its first run drafts and freezes
the labels, and every later run reuses them.

## Segment labels (one rule)

When `segment_labels.csv` exists in `cfg$out_dir` it is used, validated
against K; otherwise the LLM drafts once, one isolated call per segment
(joint prompts empirically confused near-neighbor segments), an optional
survey-context paragraph is fenced to referent resolution only, and a
collision-gated harmonizer may edit labels, never descriptions, recording
drafts for audit. Editing the file IS taking over naming; deleting it
triggers a redraft. The freeze file, not the seed, is the reproducibility
mechanism. The instrument (persona, rules, prompts, including the segment
terminology) is certified by the obedience experiment, most recently 14 of
15 checks on gemma with the sole miss a benign unexercised-harmonizer case;
any edit to the instrument obliges a re-run, and each deployment endpoint
gets one certification pass before production use. Labels are drafts:
verify each against its response-profile panel before quoting it.

## Keys

No secret appears in any file. `lca_chat()` bridges variable names only:
ellmer resolves credentials via `OPENAI_API_KEY`; if only
`OPENROUTER_API_KEY` is set (home), it is mirrored for the session; if
neither exists the run stops with instructions. Work deployments set
`cfg$compass_base_url` and `cfg$llm_model`.

## Statistical decisions (each argued where used)

- Weights enter every EM step; estimates describe the population.
- Information criteria rescale the log-likelihood to the sum-to-n scale;
  without it BIC leans toward too many segments (Lumley and Scott 2015).
- BLRT omitted: its bootstrap presumes independent observations.
- Complete-case fit; assignment (not imputation) extends to item-partial
  respondents above `cfg$min_items_predict`; below it, NA, honestly.
- Parameter CIs are Wald on the probability scale truncated to [0, 1];
  domain estimates use logit-scale intervals with delta-method SEs; the
  same JKn replicates drive both.
- BVR is reported as a descriptive ranking; no chi-square reference is
  valid under a weighted pseudo-likelihood (cf. Oberski, van Kollenburg,
  and Vermunt 2013 for the unweighted case).
- The pseudo-likelihood can be multimodal with near-tied optima: many
  seeded starts, the claim is "best mode found", and JKn replicates
  (warm-started) express within-mode variance. The validation verdict
  separates flat-ridge drift from genuine mode disagreement via the
  log-likelihood comparison.

## Data acknowledgment

The current application uses the 2023 AmericasBarometer for Mexico by the
LAPOP Lab at Vanderbilt University; obtain the data from LAPOP under their
terms. Nothing here redistributes data.
