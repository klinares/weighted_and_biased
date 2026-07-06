# ==============================================================================
# llm_classify.R
# Reusable LLM text classification with ellmer (API or local models).
#
# Purpose: measure SENTIMENT, STANCE, or TOPIC from text with chat LLMs when
# fine-tuned encoders (RoBERTa and the like) are unavailable. Works with API
# providers (Anthropic, OpenAI, Google Gemini) and local/HPC Ollama models
# (gemma4:31b, llama4:maverick) through one interface: ellmer::chat(), which
# takes a "provider/model" string. The design treats an LLM as a fallible
# zero/few-shot annotator, so every helper points toward VALIDATION: constrained
# outputs, multiple models, inter-model agreement, scoring against a
# human-labeled gold standard, and routing hard items to human review.
#
# Method citations (verified; full references in
# defensible_llm_text_measurement.md):
#   stance vs. sentiment, favor/against/none ... Mohammad et al. (2016)
#   LLM zero-shot annotation performance ....... Ziems et al. (2024),
#                                                Gilardi et al. (2023)
#   explicitness / human-LLM triage, few-shot
#   gains, relevance gate ...................... Li & Conrad (2024)
#   agreement statistics ....................... Cohen (1960), Fleiss (1971)
#   self-consistency ........................... Wang et al. (2023)
#
# Requires: ellmer (>= 0.2, for parallel_chat_structured / batch_chat_structured),
# dplyr, tidyr, purrr, tibble, glue, rlang, irr.
#   install.packages(c("ellmer","dplyr","tidyr","purrr","tibble","glue",
#                      "rlang","irr"))
#
# Providers:
#   Local  : model = "ollama/gemma4:31b" (server via `ollama serve`; pull first).
#   Hosted : all other models go through OpenRouter -- one key, many providers.
#            model = "openrouter/<slug>", where <slug> is the OpenRouter model id
#            in its own provider/model form (browse openrouter.ai/models), e.g.
#            "openrouter/meta-llama/llama-4-maverick". Key in .Renviron as
#            OPENROUTER_API_KEY (usethis::edit_r_environ()).
#            Direct APIs also work if you hold their keys: "openai/<model>",
#            "anthropic/<model>", "google_gemini/<model>".
# Pin exact tags/slugs in scripts; catalogs and defaults change over time.
# NOTE: ellmer's batch API (classify_texts_batch) works only with DIRECT
# openai/... or anthropic/... chats, not through OpenRouter -- on an
# OpenRouter stack, run large corpora with classify_texts() + checkpoint.
#
# Robustness for quota-limited APIs (e.g. tokens reset every 8 hours):
#   classify_texts() retries failed texts after Sys.sleep(retry_wait), up to
#   `retries` times, and can checkpoint per-model results to an .rds file so an
#   interrupted run resumes instead of re-spending tokens. See section 2.
#
# Conventions: native pipe, purrr (no loops; retries via recursion), fully
# namespace-qualified so the file can be source()d into any project.
# ==============================================================================


# ---- 1. Constrained rater for a single construct -----------------------------

#' Build a rater: a configured chat + schema that maps one text to exactly one
#' of a fixed set of labels.
#'
#' @param labels      Character vector of allowed categories. Structured output
#'                    (type_enum) means the model cannot return anything else.
#' @param task        The codebook: construct definition + coding instructions.
#' @param model       "provider/model" string for ellmer::chat(), e.g.
#'                    "ollama/gemma4:31b" (local) or "openrouter/<slug>" (hosted).
#' @param target      Stance only: the proposition stance is measured toward.
#' @param examples    Optional tibble(text, label) of few-shot exemplars, drawn
#'                    from a split DISJOINT from any validation set. Worth
#'                    doing: few-shot beat zero-shot for every model tested by
#'                    Li & Conrad (2024, Table 1).
#' @param params      ellmer::params(); temperature 0 recommended for coding.
#'                    Add seed = <int> where the provider supports it (OpenAI,
#'                    Ollama); the Anthropic API does not take a seed.
#' @return An object of class "llm_rater": list(chat, schema, model).
make_rater <- function(labels, task, model = "ollama/gemma4:31b", target = NULL,
                       examples = NULL, params = ellmer::params(temperature = 0)) {

  chat <- .make_chat(
    model,
    system_prompt = .build_codebook_prompt(task, labels, target, examples),
    params        = params)

  schema <- ellmer::type_object(
    label      = ellmer::type_enum(labels,
                   "The single best-fitting category."),
    confidence = ellmer::type_enum(c("low", "medium", "high"),
                   "How clearly the text fits the chosen label."),
    rationale  = ellmer::type_string(
                   "One short clause (<= 15 words) naming the deciding cue."))

  structure(list(chat = chat, schema = schema, model = model),
            class = "llm_rater")
}

#' Build the chat object from one model string. OpenRouter slugs are themselves
#' "provider/model" strings (they contain a slash), so "openrouter/<slug>" is
#' routed explicitly to ellmer::chat_openrouter() with the prefix stripped,
#' rather than trusting the generic chat() parser with a double slash. Every
#' other string goes to ellmer::chat() unchanged.
.make_chat <- function(model, system_prompt, params) {
  if (startsWith(model, "openrouter/")) {
    ellmer::chat_openrouter(
      model         = sub("^openrouter/", "", model),
      system_prompt = system_prompt,
      params        = params,
      echo          = "none")
  } else {
    ellmer::chat(model, system_prompt = system_prompt, params = params,
                 echo = "none")
  }
}

#' Assemble the system prompt from a codebook, the label set, and examples.
.build_codebook_prompt <- function(task, labels, target, examples) {
  parts <- c(
    task,
    if (!is.null(target))
      glue::glue("Stance target (the proposition): {target}"),
    glue::glue("Return exactly one label from: {paste(labels, collapse = ', ')}."),
    "Judge only the text shown; ignore metadata and outside knowledge.",
    "If the text is genuinely ambiguous, pick the closest label and set",
    "confidence to low rather than inventing certainty.")
  if (!is.null(examples)) {
    ex_lines <- examples |>
      dplyr::mutate(line = glue::glue('Example -- "{text}" => {label}')) |>
      dplyr::pull(line) |>
      paste(collapse = "\n")
    parts <- c(parts, "Worked examples (apply the same reasoning):", ex_lines)
  }
  paste(parts, collapse = "\n")
}


# ---- 2. Classify a text column (parallel, retry-with-sleep, checkpointed) ----

#' Classify a text column with one or more raters. Each rater's texts are sent
#' through ellmer::parallel_chat_structured(): concurrent, rate-limited, and
#' failure-tolerant (a failed request yields an NA label rather than aborting).
#'
#' Robustness: texts whose requests failed (quota exhausted, network drop,
#' provider hiccup) are retried after Sys.sleep(retry_wait), up to `retries`
#' times; only the failed texts are re-sent, so no tokens are re-spent on
#' successes. For an API whose token quota resets on a clock (e.g. every 8
#' hours), set retry_wait to a fraction of the reset window and retries high
#' enough to span it -- e.g. retry_wait = 30 * 60 with retries = 16 rides out
#' a full 8-hour reset unattended.
#'
#' Checkpointing: give `checkpoint` an .rds path and each model's finished
#' results are saved there as they complete; re-running the same call skips
#' models already on disk. Delete the file to force a fresh run.
#'
#' @param data       A data frame.
#' @param text       Unquoted column holding the text to classify.
#' @param raters     Named list of llm_rater objects, e.g.
#'                   list(gemma4 = ..., llama4 = ...). Names become `model`.
#' @param id         Optional unquoted id column; defaults to row number.
#' @param max_active Max simultaneous requests (lower for local Ollama, e.g. 2).
#' @param rpm        Max requests per minute (respect your API tier).
#' @param retries    How many sleep-and-retry rounds for failed texts.
#' @param retry_wait Seconds to Sys.sleep() before each retry round.
#' @param checkpoint Optional .rds path for resumable runs.
#' @return tibble(id, model, text, label, confidence, rationale):
#'         one row per (text, model); label is NA where every retry failed.
classify_texts <- function(data, text, raters, id = NULL,
                           max_active = 5, rpm = 100,
                           retries = 3, retry_wait = 60,
                           checkpoint = NULL) {
  txt    <- dplyr::pull(data, {{ text }})
  id_quo <- rlang::enquo(id)
  ids    <- if (rlang::quo_is_null(id_quo)) seq_along(txt)
            else dplyr::pull(data, !!id_quo)

  done <- if (!is.null(checkpoint) && file.exists(checkpoint))
            readRDS(checkpoint) else list()

  results <- purrr::imap(raters, function(rater, model_name) {
    if (model_name %in% names(done)) {
      message("[", model_name, "] restored from checkpoint; skipping.")
      return(done[[model_name]])
    }
    res <- .classify_with_retry(txt, ids, model_name, rater,
                                max_active, rpm, retries, retry_wait)
    if (!is.null(checkpoint)) {
      done[[model_name]] <<- res
      saveRDS(done, checkpoint)
    }
    res
  })
  purrr::list_rbind(results)
}

#' One parallel pass, then recurse on the failures after a sleep. Recursion
#' bottoms out when nothing failed or the retry budget is spent. A submission-
#' level error (e.g. auth or quota rejection before any text is processed) is
#' caught and treated as "all texts failed", so it too gets the sleep-and-retry
#' treatment instead of crashing the run.
.classify_with_retry <- function(txt, ids, model_name, rater,
                                 max_active, rpm, retries, retry_wait) {
  res <- .classify_pass(txt, ids, model_name, rater, max_active, rpm)

  failed <- dplyr::filter(res, is.na(label))
  if (nrow(failed) == 0 || retries == 0) {
    if (nrow(failed) > 0)
      warning("[", model_name, "] ", nrow(failed),
              " texts still unclassified after all retries (label = NA).")
    return(res)
  }

  message("[", model_name, "] ", nrow(failed), " of ", length(txt),
          " requests failed; sleeping ", retry_wait,
          "s then retrying (", retries, " retries left).")
  Sys.sleep(retry_wait)

  retried <- .classify_with_retry(failed$text, failed$id, model_name, rater,
                                  max_active, rpm, retries - 1, retry_wait)
  res |>
    dplyr::filter(!is.na(label)) |>
    dplyr::bind_rows(retried) |>
    dplyr::arrange(match(id, ids))
}

#' Single parallel_chat_structured() pass, normalized to a stable schema:
#' always the same columns, failed rows carried as NA labels.
.classify_pass <- function(txt, ids, model_name, rater, max_active, rpm) {
  base <- tibble::tibble(id = ids, model = model_name, text = txt)

  out <- tryCatch(
    ellmer::parallel_chat_structured(
      chat       = rater$chat,
      prompts    = as.list(txt),
      type       = rater$schema,
      max_active = max_active,
      rpm        = rpm,
      on_error   = "continue"),
    error = function(e) {
      message("[", model_name, "] submission failed: ", conditionMessage(e))
      NULL
    })

  if (is.null(out))
    return(dplyr::mutate(base, label = NA_character_,
                         confidence = NA_character_,
                         rationale  = NA_character_))
  out |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::any_of(c("label", "confidence", "rationale"))) |>
    dplyr::bind_cols(base) |>
    dplyr::select(id, model, text, label, confidence, rationale)
}

#' Large-corpus variant using the provider batch API (Anthropic and OpenAI
#' only): about half the per-token price, up to 24 h latency, and resumable --
#' `path` stores batch state, so re-running the same call retrieves results
#' instead of resubmitting.
#' @inheritParams classify_texts
#' @param rater A single llm_rater built on an anthropic/... or openai/... model.
#' @param path  .json state file for this batch (one file per batch).
classify_texts_batch <- function(data, text, rater, path, id = NULL,
                                 model_name = rater$model, wait = TRUE) {
  txt    <- dplyr::pull(data, {{ text }})
  id_quo <- rlang::enquo(id)
  ids    <- if (rlang::quo_is_null(id_quo)) seq_along(txt)
            else dplyr::pull(data, !!id_quo)

  res <- ellmer::batch_chat_structured(
    chat    = rater$chat,
    prompts = as.list(txt),
    path    = path,
    type    = rater$schema,
    wait    = wait)
  if (is.null(res)) return(invisible(NULL))   # wait = FALSE and not done yet
  dplyr::bind_cols(
    tibble::tibble(id = ids, model = model_name, text = txt),
    tibble::as_tibble(res))
}


# ---- 3. Ensemble: majority vote across models, with a disagreement flag ------

#' Collapse multi-model labels to a single consensus label per text. Rows with
#' failed requests (NA label) are dropped with a message.
#' @return tibble(id, consensus, n_models, n_agree, unanimous).
consensus_label <- function(classified) {
  ok <- dplyr::filter(classified, !is.na(label))
  if (nrow(ok) < nrow(classified))
    message(nrow(classified) - nrow(ok), " failed classifications dropped.")
  ok |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      consensus = .mode(label),
      n_models  = dplyr::n(),
      n_agree   = max(table(label)),
      unanimous = max(table(label)) == dplyr::n(),
      .groups   = "drop")
}

#' Plurality label; ties broken alphabetically so the result is deterministic.
.mode <- function(x) {
  tab <- table(x)
  names(tab)[order(-as.integer(tab), names(tab))][1]
}


# ---- 4. Human-LLM triage: route the implicit texts to people -----------------

#' Flag texts for human review. Li & Conrad (2024) show LLM-human disagreement
#' concentrates where texts express stance implicitly -- the same texts human
#' coders disagree on. Model-to-model disagreement is the analogous, label-free
#' signal here: unanimous items can be accepted from the LLMs at scale, while
#' split items (and, if supplied, low self-consistency items) go to a person.
#'
#' @param classified Output of classify_texts() with >= 2 models.
#' @param stability  Optional output of self_consistency() to also flag items
#'                   a single model labels unstably across resamples.
#' @param min_stability Items below this stability are flagged (default 1, i.e.
#'                   any instability flags the item; loosen to taste).
#' @return tibble(id, text, one column per model's label, consensus, n_agree,
#'         n_models, flag) sorted so the most-disputed items come first.
triage_for_review <- function(classified, stability = NULL,
                              min_stability = 1) {
  labels_wide <- classified |>
    dplyr::filter(!is.na(label)) |>
    dplyr::select(id, text, model, label) |>
    tidyr::pivot_wider(names_from = model, values_from = label)

  out <- labels_wide |>
    dplyr::inner_join(consensus_label(classified), by = "id") |>
    dplyr::mutate(flag = !unanimous)

  if (!is.null(stability))
    out <- out |>
      dplyr::left_join(dplyr::select(stability, id, stability), by = "id") |>
      dplyr::mutate(flag = flag | (!is.na(stability) &
                                     stability < min_stability))

  dplyr::arrange(out, dplyr::desc(flag), n_agree)
}


# ---- 5. Inter-rater reliability across models --------------------------------

#' Agreement among models: kappa (Cohen 1960 for 2 raters; Fleiss 1971 for 3+),
#' raw proportion of items all models agree on, and the 2-model confusion
#' table. Read agreement PER CLASS off the confusion table: a class the models
#' rarely agree on is a class the measure cannot be trusted for. Kappa has no
#' validated interpretive thresholds; report it with raw agreement and the
#' confusion, not with an adjective.
#' @return list(kappa, raw_agreement, n_items, confusion).
model_agreement <- function(classified) {
  wide <- classified |>
    dplyr::filter(!is.na(label)) |>
    dplyr::select(id, model, label) |>
    tidyr::pivot_wider(names_from = model, values_from = label) |>
    tidyr::drop_na()
  ratings <- as.data.frame(dplyr::select(wide, -id))

  kappa <- if (ncol(ratings) == 2) irr::kappa2(ratings)$value
           else irr::kappam.fleiss(ratings)$value
  raw <- mean(apply(ratings, 1, function(r) length(unique(r)) == 1L))

  list(kappa         = kappa,
       raw_agreement = raw,
       n_items       = nrow(ratings),
       confusion     = if (ncol(ratings) == 2)
         table(ratings[[1]], ratings[[2]], dnn = names(ratings)))
}


# ---- 6. Validation against a human-labeled gold standard (the core step) -----

#' Score predictions against human labels: per-class precision/recall/F1,
#' macro-F1, accuracy, kappa vs. human, and the confusion matrix. This is what
#' makes LLM labels defensible; without it they are unvalidated guesses.
#'
#' @param classified Output of classify_texts() (optionally several models).
#' @param gold       tibble(id, gold_label) of human annotations.
#' @param model_name If `classified` holds several models, the one to score.
#' @return list(per_class, macro_f1, accuracy, kappa_vs_human, confusion).
validate_against_gold <- function(classified, gold, model_name = NULL) {
  preds <- if (!is.null(model_name))
             dplyr::filter(classified, model == model_name) else classified
  joined  <- preds |>
    dplyr::filter(!is.na(label)) |>
    dplyr::inner_join(gold, by = "id")
  classes <- sort(union(joined$label, joined$gold_label))

  per_class <- purrr::map_dfr(classes, function(cl) {
    tp <- sum(joined$label == cl & joined$gold_label == cl)
    fp <- sum(joined$label == cl & joined$gold_label != cl)
    fn <- sum(joined$label != cl & joined$gold_label == cl)
    precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    recall    <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    f1 <- if (!is.na(precision) && !is.na(recall) && precision + recall > 0)
            2 * precision * recall / (precision + recall) else NA_real_
    tibble::tibble(class = cl, precision = precision, recall = recall,
                   f1 = f1, support = sum(joined$gold_label == cl))
  })

  list(
    per_class      = per_class,
    macro_f1       = mean(per_class$f1, na.rm = TRUE),
    accuracy       = mean(joined$label == joined$gold_label),
    kappa_vs_human = irr::kappa2(dplyr::select(joined, gold_label, label))$value,
    confusion      = table(gold = joined$gold_label, pred = joined$label))
}


# ---- 7. Self-consistency: stability of one model under resampling ------------

#' Re-classify each text `times` times and report how often the modal label
#' recurs (Wang et al. 2023). Low stability flags items the model itself is
#' unsure about -- a label-free uncertainty signal that does not rely on the
#' model's self-reported confidence, and a usable proxy for the implicitness
#' that drives LLM-human disagreement (Li & Conrad 2024). Build the rater with
#' temperature > 0; at temperature 0 output is deterministic and stability is
#' trivially 1.
#' @return tibble(id, modal_label, stability) with stability in (0, 1].
self_consistency <- function(data, text, rater, id = NULL, times = 5,
                             max_active = 5, rpm = 100) {
  purrr::map_dfr(seq_len(times), function(r)
    classify_texts(data, {{ text }}, list(rep = rater), id = {{ id }},
                   max_active = max_active, rpm = rpm, retries = 1)) |>
    dplyr::filter(!is.na(label)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(modal_label = .mode(label),
                     stability   = max(table(label)) / dplyr::n(),
                     .groups     = "drop")
}


# ---- 8. Ready-made constructs (edit the codebook text for your domain) -------

#' Affective sentiment: how positive or negative the expressed feeling is.
sentiment_rater <- function(model = "ollama/gemma4:31b", examples = NULL, ...)
  make_rater(
    labels = c("negative", "neutral", "positive"),
    task = paste(
      "You are a careful annotator measuring AFFECTIVE SENTIMENT: how positive",
      "or negative the author's expressed feeling is, independent of the topic",
      "or any policy position. Praise and gratitude are positive; anger, fear,",
      "and disgust are negative; factual or mixed text is neutral."),
    model = model, examples = examples, ...)

#' Stance: the author's position TOWARD a specific proposition (the target),
#' distinct from sentiment (Mohammad et al. 2016) -- hostile tone can still
#' favor the target.
#'
#' @param include_irrelevant Adds an "irrelevant" label for texts that do not
#'   discuss the target at all, per the relevance gate in Li & Conrad's (2024)
#'   prompts. Use it whenever the corpus was keyword-collected, where
#'   off-target texts are common; without it the model is forced to invent a
#'   stance for posts that have none.
stance_rater <- function(target, model = "ollama/gemma4:31b", examples = NULL,
                         include_irrelevant = FALSE, ...) {
  labels <- c("favor", "neutral", "oppose",
              if (include_irrelevant) "irrelevant")
  task <- paste(
    "You are a careful annotator measuring STANCE: the author's position",
    "TOWARD THE STATED PROPOSITION. Stance is distinct from sentiment -- an",
    "angry or negative tone can still FAVOR the proposition, and warm praise",
    "can OPPOSE it. Decide the position toward the target, not the mood.",
    if (include_irrelevant) paste(
      "First decide whether the text discusses the target at all; if it does",
      "not, the label is irrelevant."))
  make_rater(labels = labels, task = task, target = target,
             model = model, examples = examples, ...)
}

#' Topic: which single category the text is primarily ABOUT.
topic_rater <- function(labels, model = "ollama/gemma4:31b", examples = NULL, ...)
  make_rater(
    labels = labels,
    task = paste(
      "You are a careful annotator measuring TOPIC: the single category the",
      "text is primarily ABOUT (its subject matter), independent of sentiment",
      "or stance."),
    model = model, examples = examples, ...)


# ==============================================================================
# EXAMPLE -- stance on Reddit comments about the federal workforce reduction,
# mixing one API model and one local model, on a quota-limited work API.
# `if (FALSE)` keeps this inert when the file is source()d.
# ==============================================================================
if (FALSE) {

  target <- "reducing the size of the federal workforce"

  # gemma4 runs locally; every other model goes through OpenRouter
  # (OPENROUTER_API_KEY in .Renviron; confirm slugs at openrouter.ai/models).
  raters <- list(
    gemma4   = stance_rater(target, model = "ollama/gemma4:31b",
                            include_irrelevant = TRUE),
    maverick = stance_rater(target,
                            model = "openrouter/meta-llama/llama-4-maverick",
                            include_irrelevant = TRUE))

  # comments: tibble(comment_id, comment).  gold: tibble(id, gold_label).
  # Work API resets tokens every 8 h: retry every 30 min, enough rounds to
  # span a full reset, and checkpoint so an interrupted run resumes free.
  classified <- classify_texts(
    comments, comment, raters, id = comment_id,
    max_active = 3, rpm = 60,
    retries = 16, retry_wait = 30 * 60,
    checkpoint = "stance_run.rds")

  # 1. Reliability -- do the models agree? (kappa + confusion, per class)
  model_agreement(classified)

  # 2. Ensemble point estimate + human-LLM triage: accept unanimous items,
  #    send disputed (implicit) ones to a human coder (Li & Conrad 2024).
  ensemble <- consensus_label(classified)
  review   <- triage_for_review(classified)

  # 3. Validity -- score each model against the human labels (decisive step)
  validate_against_gold(classified, gold, model_name = "gemma4")
  validate_against_gold(classified, gold, model_name = "maverick")

  # 4. Large corpora: ellmer's batch API (classify_texts_batch) needs a DIRECT
  #    openai/... or anthropic/... chat and does not work through OpenRouter.
  #    On this stack, scale with classify_texts() itself -- the checkpoint file
  #    plus sleep-and-retry make long runs resumable and quota-safe.

  # 5. Optional -- stability under resampling (temperature > 0; Wang et al.
  #    2023), usable as a second triage signal in triage_for_review().
  stab <- self_consistency(
    comments, comment,
    stance_rater(target, model = "ollama/gemma4:31b",
                 params = ellmer::params(temperature = 0.7)),
    id = comment_id, times = 5)
  triage_for_review(classified, stability = stab, min_stability = 0.8)
}
