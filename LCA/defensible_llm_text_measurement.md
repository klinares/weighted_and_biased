# Defensible LLM-based measurement of sentiment, stance, and topic

The methodology note for the LLM measurement in the **hot_topic** pipeline
(`llm_stance.qmd`, Script 2). It addresses one situation: you need to classify
text by sentiment, stance, or topic, and you cannot use a fine-tuned encoder
(RoBERTa, DeBERTa, a domain classifier) because the model weights or the
GPU/hosting are not available at work. The tools on hand are chat LLMs: one
local Ollama model (`gemma4:31b`) plus any hosted model through OpenRouter's
unified API (one key, many providers), with a one-line swap to the work
OpenAI-compatible endpoint -- all called from `ellmer` through one routing
function. The question is how to use them so the results survive review.

The machinery described here originated in a standalone toolkit
(`llm_classify.R`, now superseded) and lives in **`llm_stance_source.R`**,
sourced by `llm_stance.qmd` from the same folder; the function names below
(`stance_rater`, `build_stance_prompt`, `classify_with_retry`,
`model_agreement`, `consensus_label`, `triage_for_review`,
`validate_against_gold`) are that file's functions. The pipeline currently
implements **stance only**; sentiment and topic raters follow the same
constrained-output pattern and are one `type_enum` schema away when needed.

The short answer: a chat LLM used this way is a **zero/few-shot annotator**, not
a calibrated classifier. So you validate it exactly as you would validate a new
human coder, and you report the same things you would report for one.

## First, measure the right construct

Topic, sentiment, and stance are three different things, and conflating them is
the most common way these analyses go wrong.

- **Topic** is *aboutness*: which subject the text concerns.
- **Sentiment** is *affective valence*: how positive or negative the author's
  expressed feeling is.
- **Stance** is *position toward a specific target proposition*: favor, oppose,
  or neutral with respect to a named claim (e.g. "reducing the size of the
  federal workforce"). Stance is target-relative; the same text has different
  stances toward different targets. This favor/against/neither framing, and the
  distinction from sentiment, follow the stance-detection literature (Mohammad
  et al. 2016).

Sentiment and stance routinely diverge, which is why they must be named
precisely. A federal-worker comment like *"Good riddance, half these offices did
nothing"* is **negative in sentiment** (hostile, contemptuous) but **favors**
the cuts. *"I'm so proud of my colleagues fighting back against this"* is
**positive in sentiment** (pride) but **opposes** them. This is the mechanism
behind the empirical finding in the DOGE study that "oppose" was recoverable
while "favor" was not: favorable comments often arrive wrapped in anger, so any
method keying on tone will misread them. State which construct you are measuring,
and if it is stance, state the target in the prompt. The pipeline's
`stance_rater()` bakes this distinction into its codebook on purpose, and a
sentiment rater, when needed, states the converse in its own.

## Treat the LLM as a fallible annotator, and validate it like one

Eight practices, each implemented in the toolkit.

### 1. Validate against a human gold standard (non-negotiable)

This is the difference between a defensible measurement and a guess. Hold out a
human-labeled set and report per-class precision, recall, and F1, plus macro-F1,
overall accuracy, and Cohen's kappa against the human labels. `validate_against_gold()` returns all of these and the confusion matrix. If you have no labels, you
have no defensible claim about accuracy -- get some, even a few hundred, as the
DOGE study did with 400. Report **macro-F1**, not accuracy, when classes are
imbalanced: at 54% "oppose", a model that only ever says "oppose" scores 54%
accuracy and is useless.

### 2. Constrain the output

Ask for free text and you inherit a parsing problem: hallucinated categories,
verbose hedging, and -- with reasoning models such as deepseek-r1 -- a wall of
chain-of-thought you have to dig the answer out of with `str_sub()` and regex.
Structured output removes this. `ellmer::type_enum(labels, ...)` forces the model
to return one of your categories and nothing else, so there is no cleanup step
and no silent extraction failures. This alone is the main reason to move from a
free-text `rollama` call to a structured `ellmer` one.

### 3. Use multiple models and report agreement

Run more than one model and measure inter-model agreement: Cohen's kappa for two
raters (Cohen 1960), Fleiss' kappa for three or more (Fleiss 1971), plus the
confusion table so you can read agreement **per class**. `model_agreement()`
does this. Disagreement is information, not
noise: in the DOGE study the models agreed on "oppose" (.80) but almost never on
"favor" (.01) or "neutral" (.03), which tells you precisely which estimates to
distrust. Use `consensus_label()` for a majority-vote point estimate, and treat
non-unanimous items as low-confidence rather than hiding the split.

A caution on the agreement number itself: **Cohen's kappa has no validated
interpretive thresholds.** The familiar "fair / moderate / substantial" labels
are arbitrary and prevalence-sensitive, so report kappa alongside raw agreement
and the per-class confusion rather than leaning on the adjective.

### 4. Report per-class metrics and the confusion matrix

Never report only an overall number. The whole story in this kind of task lives
in the minority classes, and an aggregate hides it. Always show per-class recall
and precision and the confusion matrix so a reader can see *where* the errors go.

### 5. Pin the model and the settings

LLM outputs drift across model versions, so an unpinned model is unreproducible.
Record the exact model tag (the OpenRouter slug, or your API endpoint's full `provider/model` string,
not "Claude" or "Gemma"), `temperature = 0`, the verbatim prompt, and the run
date. Set a fixed `seed` where the provider supports one (OpenAI, Ollama); the
Anthropic API does not accept a seed, so for API models the pinned tag +
temperature 0 + recorded date is the reproducibility record. The toolkit fixes
temperature by default and keeps the prompt in the rater object.

### 6. Probe prompt sensitivity

A measurement that flips when you rephrase the instruction is fragile. Build two
or three raters with differently worded codebooks for the same construct, run
them on the validation set, and report whether the metrics move. If they do, say
so; if they are stable, that is evidence for robustness.

### 7. Be honest about calibration

The model's self-reported `confidence` and any verbalized probabilities are
**not calibrated** -- do not treat them as true probabilities. They are useful
only for triage (review the low-confidence items). If you genuinely need
calibrated scores, you need labeled data to calibrate against (e.g. isotonic or
Platt scaling on a held-out split), which again points back to practice 1. A more
trustworthy, label-free uncertainty signal is self-consistency (Wang et al.
2023): resample each item at temperature > 0 and see how often the label
recurs; unstable items are the ones the model is actually unsure about. The
production pipeline does not run it, because every resample spends the same
300-calls-per-window budget as a real classification; the deployed label-free
triage signal is model-to-model disagreement (practice 3), and
self-consistency remains an option for a small diagnostic subsample if a
dispute needs settling.

### 8. Few-shot from a held-out codebook, kept blind

If you add few-shot examples, draw them from a split **disjoint** from the
validation set (otherwise you leak the test answers and inflate your metrics),
and give the model the same written codebook your human coders used. The gain
is real: in a stance-annotation benchmark, few-shot prompting improved the
average F1 over zero-shot for every model tested (Li & Conrad 2024, Table 1;
the improvement holds per model averaged across tasks, though one or two
individual class-level cells tick down). Keep the model blind to
anything it should not use: metadata, the outcome, and -- if you are measuring
change over time -- the date, so it cannot infer the trend it is supposed to
help measure.

### 9. Expect failure on implicit texts; route them to humans

LLM-human disagreement is not random. Li and Conrad (2024) show it concentrates
on texts that express stance *implicitly* -- the same texts human coders
disagree on (they index explicitness by the spread of ten crowd judgments per
tweet, and lower explicitness predicts lower LLM-human agreement). Two design
consequences, both implemented in the toolkit:

- **Gate for relevance.** Keyword-collected corpora contain posts that never
  discuss the target; forcing a favor/oppose/neutral choice on them
  manufactures noise. Their prompts add an "irrelevant" option;
  the pipeline's `stance_rater()` carries the gate in its label set and
  codebook by default, since the corpus is topic-assigned rather than
  hand-screened and off-target paragraphs are guaranteed to occur.
- **Triage instead of trusting.** Model-to-model disagreement (and low
  self-consistency) is a label-free proxy for the implicitness that predicts
  failure. `triage_for_review()` accepts unanimous items from the LLMs at
  scale and routes split or unstable ones to a human coder -- the hybrid
  human-LLM pipeline the paper recommends. Their further advice folds into
  practice 6: pilot on a small sample first and read the model's rationales
  (the schema's `rationale` field) to refine the codebook before the full run.

## What this looks like in code

The shape of the measurement in `llm_stance.qmd` (Script 2), reading Script 1's
topic outputs; everything below is defined inline in that script.

```r
# --- Raters: one proposition per topic stratum; >= 2 pinned models ------------
# Routing off the model string: "openrouter/<slug>" -> chat_openrouter,
# anything else -> the work OpenAI-compatible endpoint (base URL and key
# from .Renviron). Temperature 0, structured output.
# "openrouter/<slug>" now; any other string routes to the work
# OpenAI-compatible endpoint (base URL and key from .Renviron).
st$models <- c(maverick = "openrouter/meta-llama/llama-4-maverick",
               second   = "openrouter/CHANGE-ME")
rater <- stance_rater(proposition = topics_meta$proposition[[1]],
                      model = st$models[["maverick"]], persona = st$persona)

# --- Census classification, sequential and checkpointed -----------------------
# run_rater() is the single seam between simulated and real runs. The loop
# classifies topic by topic (each prompt carries that topic's proposition,
# looked up from the analyst's topics CSV), checkpoints per (topic, model),
# and appends to the incremental label store, which never re-sends a
# paragraph already labeled.
stance_raw <- ...   # see the classify chunk in llm_stance.qmd

# --- Reliability, ensemble, triage, validity ----------------------------------
model_agreement(stance_raw)        # kappa + raw agreement + per-class confusion
consensus <- consensus_label(stance_raw)   # majority vote, deterministic ties
triage    <- ...                   # non-unanimous items -> human coder, CSV
validate_against_gold(stance_raw, gold, "maverick")  # per-class P/R/F1, macro-F1
```

The same four-step pattern -- build raters, classify, check agreement, validate
against gold -- transfers to any construct and project; only the label set and
the codebook text change. Model strings are one-string swaps and are pinned in
the script, since catalogs and defaults move.

## Scaling: sequential calls, checkpoints, and quota windows

Classification is deliberately **sequential**: one request at a time through
`chat_structured()`, matching how the work API is called, with each request on
a `clone()`d chat so every paragraph is judged from a fresh conversation
(without the clone, ellmer appends turns and later paragraphs would be judged
with earlier ones in context). A failed call yields an NA row instead of
killing the run.

**Quota-limited APIs.** The work allowance is 300 calls per 4-hour window per
model. The pipeline classifies topic by topic with one crash checkpoint per
(topic, model), and re-sends only failed texts after `Sys.sleep(retry_wait)`,
up to `retries` rounds, so successes never re-spend budget. With production
settings (`retries = 16`, `retry_wait = 30 * 60`) a multi-window census run
rides out full quota resets unattended. The label store is **incremental**:
paragraphs already labeled are never re-sent, so interruptions, re-renders,
and later-added topics cost only what is new; a temperature-0 label is a
fixed measurement of a fixed paragraph. The interactive defaults fail fast
(`retries = 1`, `retry_wait = 5`) so a misconfiguration surfaces in seconds.

**Provider batch APIs** (about half the per-token price, up to 24-hour
latency) work only with direct `openai/...` or `anthropic/...` chats, not
through OpenRouter, so the pipeline does not use them; on this stack the
checkpoint-plus-retry loop is the scaling mechanism. If a direct provider key
ever becomes available, the batch endpoint is a cost lever worth revisiting.

API models introduce one defensibility cost local models do not have: the text
leaves your machine. Confirm that sending your corpus to a commercial API is
permitted for your data (Reddit text is public, but agency data may not be)
before choosing the API path.

## A defensibility checklist

Before reporting an LLM-based measurement, confirm you can answer yes to each:

1. Is the construct named precisely (topic vs sentiment vs stance), and for
   stance, is the target stated?
2. Are there human labels, and do you report per-class precision/recall/F1,
   macro-F1, and kappa vs. human?
3. Is the output constrained to the valid label set (no free-text parsing)?
4. Did you run >= 2 models and report inter-model agreement and the confusion?
5. Is the model tag, temperature, seed, prompt, and date recorded?
6. Did you check that rephrasing the prompt does not move the results much?
7. Are you treating model confidence as triage only, not as calibrated
   probability?
8. If few-shot, are the examples disjoint from the validation set?

## What this fixes relative to the legacy pipeline

- Free-text extraction hacks (the old `str_sub(deepseek, -12)` pattern)
  disappear: structured `type_enum` output cannot return anything but a valid
  label.
- Multi-model agreement and gold-standard scoring are first-class functions,
  not one-off blocks, so every analysis reports the same defensible evidence.
- Construct precision is enforced in the prompts, separating the "favor with
  hostile tone" cases that a sentiment-flavored prompt mislabels.
- The measurement lives inline in one self-contained script with a single
  simulated/real seam, instead of a `source()`d toolkit whose version could
  drift from the analysis that cites it.

## Honest limits

Zero-shot chat LLMs are good for exploration and for the easy pole of a scale
(clear opposition), and weak on nuanced or minority classes -- exactly the
pattern the DOGE results show. The benchmark evidence matches: LLMs can rival
crowd annotators on labeling tasks (Gilardi et al. 2023), but on classification
benchmarks they do **not** outperform the best fine-tuned models, achieving fair
agreement with humans rather than parity (Ziems et al. 2024). They can encode
training biases, they are sensitive to prompt wording, and they are not a
substitute for a validated, fine-tuned classifier when labels exist and stakes
are high. Used with the practices above they are a defensible measurement
instrument with known error; used without them they are unverified output. The
toolkit is built to keep you in the first case.

## References

All entries verified against primary sources (ACL Anthology, MIT Press, PNAS,
the SemEval proceedings, dblp, and the original journals) while preparing this
document.

- Cohen, J. (1960). A Coefficient of Agreement for Nominal Scales. *Educational
  and Psychological Measurement*, 20(1), 37-46. doi:10.1177/001316446002000104
- Fleiss, J. L. (1971). Measuring Nominal Scale Agreement Among Many Raters.
  *Psychological Bulletin*, 76(5), 378-382.
- Gilardi, F., Alizadeh, M., & Kubli, M. (2023). ChatGPT Outperforms Crowd
  Workers for Text-Annotation Tasks. *PNAS*, 120(30), e2305016120.
  doi:10.1073/pnas.2305016120
- Landis, J. R., & Koch, G. G. (1977). The Measurement of Observer Agreement
  for Categorical Data. *Biometrics*, 33(1), 159-174. (Cited here only as the
  source of the widely used but arbitrary kappa threshold labels.)
- Li, M., & Conrad, F. (2024). Advancing Annotation of Stance in Social Media
  Posts: A Comparative Analysis of Large Language Models and Crowd Sourcing.
  arXiv preprint arXiv:2406.07483.
- Mohammad, S. M., Kiritchenko, S., Sobhani, P., Zhu, X., & Cherry, C. (2016).
  SemEval-2016 Task 6: Detecting Stance in Tweets. *Proceedings of the 10th
  International Workshop on Semantic Evaluation (SemEval-2016)*.
- Wang, X., Wei, J., Schuurmans, D., Le, Q. V., Chi, E. H., Narang, S.,
  Chowdhery, A., & Zhou, D. (2023). Self-Consistency Improves Chain of Thought
  Reasoning in Language Models. *International Conference on Learning
  Representations (ICLR)*. arXiv:2203.11171
- Ziems, C., Held, W., Shaikh, O., Chen, J., Zhang, Z., & Yang, D. (2024). Can
  Large Language Models Transform Computational Social Science? *Computational
  Linguistics*, 50(1), 237-291. doi:10.1162/coli_a_00502
