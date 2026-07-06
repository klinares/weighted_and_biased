# Defensible LLM-based measurement of sentiment, stance, and topic

A short methodology note and usage guide for `llm_classify.R`. It addresses one
situation: you need to classify text by sentiment, stance, or topic, and you
cannot use a fine-tuned encoder (RoBERTa, DeBERTa, a domain classifier) because
the model weights or the GPU/hosting are not available at work. The tools on
hand are chat LLMs: one local Ollama model (`gemma4:31b`) plus any hosted model
through OpenRouter's unified API (one key, many providers) -- all called from
`ellmer` through one interface. The question is how to use them
so the results survive review.

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
and if it is stance, state the target in the prompt. `stance_rater()` and
`sentiment_rater()` bake this distinction into their instructions on purpose.

## Treat the LLM as a fallible annotator, and validate it like one

Eight practices, each implemented in the toolkit.

### 1. Validate against a human gold standard (non-negotiable)

This is the difference between a defensible measurement and a guess. Hold out a
human-labeled set and report per-class precision, recall, and F1, plus macro-F1,
overall accuracy, and Cohen's kappa against the human labels. `validate_against_
gold()` returns all of these and the confusion matrix. If you have no labels, you
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
Record the exact model tag (`ollama/gemma4:31b`, or your API endpoint's full `provider/model` string,
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
Platt scaling on a held-out split), which again points back to practice 1. The
`self_consistency()` helper gives a more trustworthy, label-free uncertainty
signal, adapting the self-consistency idea of Wang et al. (2023): resample each
item at temperature > 0 and see how often the label recurs; unstable items are
the ones the model is actually unsure about.

### 8. Few-shot from a held-out codebook, kept blind

If you add few-shot examples, draw them from a split **disjoint** from the
validation set (otherwise you leak the test answers and inflate your metrics),
and give the model the same written codebook your human coders used. The gain
is real: in a stance-annotation benchmark, few-shot prompting improved F1 over
zero-shot for every model tested (Li & Conrad 2024). Keep the model blind to
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
  `stance_rater(target, include_irrelevant = TRUE)` does the same.
- **Triage instead of trusting.** Model-to-model disagreement (and low
  self-consistency) is a label-free proxy for the implicitness that predicts
  failure. `triage_for_review()` accepts unanimous items from the LLMs at
  scale and routes split or unstable ones to a human coder -- the hybrid
  human-LLM pipeline the paper recommends. Their further advice folds into
  practice 6: pilot on a small sample first and read the model's rationales
  (the schema's `rationale` field) to refine the codebook before the full run.

## What this looks like in code

```r
source("llm_classify.R")

# --- Stance (target-specific): gemma4 local, everything else via OpenRouter ---
target <- "reducing the size of the federal workforce"
raters <- list(   # slugs at openrouter.ai/models; OPENROUTER_API_KEY in .Renviron
  gemma4   = stance_rater(target, model = "ollama/gemma4:31b",
                          include_irrelevant = TRUE),
  maverick = stance_rater(target,
                          model = "openrouter/meta-llama/llama-4-maverick",
                          include_irrelevant = TRUE))

stance <- classify_texts(comments, comment, raters, id = comment_id,
                         max_active = 3, rpm = 60,
                         retries = 16, retry_wait = 30 * 60,  # 8-h quota reset
                         checkpoint = "stance_run.rds")       # resumable
model_agreement(stance)                                    # reliability
review <- triage_for_review(stance)                        # humans get the rest
validate_against_gold(stance, gold, model_name = "gemma4") # validity (per class)

# --- Sentiment (same toolkit, different construct) ----------------------------
sent <- classify_texts(comments, comment,
                       list(gemma4 = sentiment_rater("ollama/gemma4:31b")),
                       id = comment_id)

# --- Topic (supply your category set) ------------------------------------------
areas <- c("pay & benefits", "job security", "agency mission", "management")
topic <- classify_texts(comments, comment,
                        list(gemma4 = topic_rater(areas, "ollama/gemma4:31b")),
                        id = comment_id)
```

The same four-step pattern -- build raters, classify, check agreement, validate
against gold -- works for all three constructs and any new project; only the
label set and the codebook text change. Model strings are one-string swaps:
`"ollama/gemma4:31b"` for the local model, `"openrouter/<slug>"` for any hosted
model (the toolkit routes the latter to `ellmer::chat_openrouter()`, since
OpenRouter slugs themselves contain a slash), and direct `"provider/model"`
strings if you hold that provider's key. Pin exact tags and slugs in scripts,
since catalogs and defaults move.

## Scaling: parallel and batch execution

`classify_texts()` runs each model's texts through
`ellmer::parallel_chat_structured()`: concurrent requests with `max_active` and
`rpm` throttles, and `on_error = "continue"` so one failed request yields an NA
row instead of killing an hours-long run. Downstream helpers drop failed rows
and say how many. Set `max_active` low (2-3) for a local Ollama server and
respect your API tier's rate limits for hosted models.

**Quota-limited APIs.** When an API's token allowance resets on a clock (e.g.
every 8 hours), a long run will hit the wall mid-corpus. `classify_texts()`
handles this without babysitting: after each pass it collects the texts whose
requests failed, `Sys.sleep(retry_wait)`, and re-sends **only those texts**, up
to `retries` rounds -- so successes never re-spend tokens, and
`retry_wait = 30 * 60` with `retries = 16` rides out a full 8-hour reset
unattended. Submission-level failures (auth or quota rejection before any text
is processed) are caught and given the same sleep-and-retry treatment. Add
`checkpoint = "run.rds"` and each model's finished results are saved as they
complete, so an interrupted or crashed run resumes from disk instead of from
the API.

For large corpora, `classify_texts_batch()` uses the provider batch API via
`ellmer::batch_chat_structured()`: roughly half the per-token price in exchange
for up to 24-hour latency, and resumable -- the `path` state file means an
interrupted run picks up where it left off instead of resubmitting. One hard
constraint: the batch API works only with **direct** `openai/...` or
`anthropic/...` chats, not through OpenRouter. On an OpenRouter stack, scale
with `classify_texts()` itself -- checkpointing plus sleep-and-retry make long
runs resumable and quota-safe without the batch endpoint.

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

## What this fixes relative to the current pipeline

- The `rollama` script's `str_sub(deepseek, -12)` extraction disappears:
  structured `type_enum` output cannot return anything but a valid label.
- One toolkit now covers sentiment, stance, and topic, so the same validated
  machinery is reused across projects instead of re-written per task.
- Multi-model agreement and gold-standard scoring are first-class functions, not
  one-off blocks, so every analysis reports the same defensible evidence.
- Construct precision is enforced in the prompts, separating the "favor with
  hostile tone" cases that a sentiment-flavored prompt mislabels.

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
