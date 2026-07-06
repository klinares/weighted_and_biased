# weighted_and_biased

![](images/clipboard-2943011998.png)

Design-weighted latent class analysis (LCA) with integrated jackknife replicate variance estimation, and an LLM-assisted tool for drafting class labels from a fitted measurement model.

The repository fills a gap in the R ecosystem. Existing packages either estimate latent class models or compute replicate variances, but none combine survey-design-weighted LCA estimation with replicate-based variance estimation for the quantities that result: class prevalences, item-response probabilities, and design-based domain estimates.

The project has two parts, an estimation pipeline (R and Quarto) and a labeling tool (R).

## 1. Design-weighted LCA estimation

Files:

- `survey_lca_source.R` is the reusable engine: data-preparation and plotting helpers, the design-weighted EM estimator, and a configurable data simulator for testing.
- `survey_lca_analysis.qmd` is the analysis narrative; it sources the engine and renders to PDF.

A single configuration list (`cfg`) is the only intended edit point.

### Methods

Weighted EM. The latent class model is fit by an expectation-maximization algorithm in which each case contributes its survey weight to the sufficient statistics, so prevalences and item-response probabilities are design-weighted. Each random start is re-seeded deterministically, so a run reproduces exactly.

Jackknife replicate variance. Standard errors and confidence intervals come from jackknife (JKn) replicate weights constructed from the design's strata, primary sampling units, and weights, applied through `survey::withReplicates`. This propagates the complex-design variance into every reported quantity rather than assuming simple random sampling.

Design-based domain estimation. Class composition within a demographic domain is estimated from the soft posterior class probabilities, averaged with the replicate weights through a controlled `withReplicates` statistic. The result is logit-bounded confidence intervals that are coherent with the overall prevalences, since the weighted domain mean of the posteriors reproduces the prevalence at the EM fixed point. This replaced an earlier odds-ratio and multinomial approach that suffered quasi-separation when a stratum was sparse within a class and replication emptied the cell.

Model selection. The number of classes is chosen by an information criterion on the weighted pseudo-log-likelihood. Because base weights need not sum to the sample size, the pseudo-log-likelihood is rescaled by n / sum(w) before forming AIC and BIC, so the fit term and the ln(n) penalty are on the same footing; this leaves point estimates unchanged. A `cfg$K_force` override pins the class count when a substantive or diagnostic reason calls for it. The unweighted poLCA BIC-selected K is retained as a diagnostic comparator. The design-based criterion of Lumley and Scott (2015), which inflates the parameter penalty by the trace of a generalized design-effect matrix and so selects fewer classes when the design effect exceeds one, is the rigorous reference point.

Variable selection. Where indicator screening is used, the framework follows Fop, Smart and Murphy (2017), whose generalization allows a dropped variable to depend on the retained ones, alongside the stepwise BIC search of Dean and Raftery (2010) with its conditional-independence restricted comparison model.

Parallelism. The three repeated-fit loops (class-number enumeration, the indicator screen, and the poLCA comparison sweep) are parallelized with `furrr` using reproducible parallel seeding.

## 2. LLM-assisted class labeling

File: `lca_class_labeling.R`, with `gemma_label_experiment.R` as the ground-truth validation harness used to design it.

The tool reads a fitted measurement model and asks a language model to draft, for each class, a short label, a factual description anchored to that class's high-probability answers, and the items that define it. The output is a draft for an analyst to review and refine; it never re-enters estimation.

### Input

`read_lca_outputs()` reads the measurement-model export, a long CSV with a `kind` column where `pi` rows carry class prevalences and `rho` rows carry response probabilities by item, category, and class. It returns one category-by-class probability matrix per item plus the prevalence vector. Question wording and response-category labels are supplied alongside, since they are not part of the numeric fit.

### Method and its justification

The tool issues one model call per class. This was chosen empirically with the validation harness, which plants a known measurement model, runs the labeler blind, and scores recovery against the planted truth:

- Showing the model all classes at once and asking for a joint labeling confuses the structurally closest classes. The failure is similarity-driven, concentrated on near-neighbor pairs, and a forced one-to-one assignment lets a single confusion fail two classes.
- Reading one class in isolation removes that interference and gives a clean, independently scored result per class.
- A two-stage variant, a global comparison pass feeding each per-class call, was tested and changed no class on the near-neighbor cells, so the extra call was dropped.

The model is therefore used for what it does reliably, reading a profile and reporting facts, while the interpretive naming is left to the analyst. The output prints the items the model named beside the items that objectively most distinguish each class (total-variation distance), so the review is grounded. Labels are least reliable for classes with very similar profiles and for near-neutral response patterns, and these are flagged for closer review.

### Providers

The model call is provider-agnostic: OpenRouter, a local Ollama server, or an internal OpenAI-compatible endpoint, selected in the configuration block. API keys are read from environment variables and are never stored in the file.

## Reproducibility and conventions

- R style: native pipe only, no explicit loops (map, reduce, and matrix algebra), `tidyverse` loaded last, viridis palettes for figures, and explicit conflict resolution.
- Estimation is seeded per replicate fit, so weighted EM runs and replicate variances reproduce exactly.
- Quarto documents render to PDF; a stale-cache render is cleared by removing the `_freeze` and `.quarto` directories.

## References

- Dean, N. and Raftery, A. E. (2010). Latent class analysis variable selection. *Annals of the Institute of Statistical Mathematics*, 62(1), 11-35.
- Fop, M., Smart, K. M. and Murphy, T. B. (2017). Variable selection for latent class analysis with application to low back pain diagnosis. *Annals of Applied Statistics*, 11(4), 2080-2110.
- Lumley, T. and Scott, A. J. (2015). AIC and BIC for modeling with complex survey data. *Journal of Survey Statistics and Methodology*, 3(1), 1-18.
