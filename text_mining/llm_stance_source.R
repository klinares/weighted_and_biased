# llm_stance_source.R ---------------------------------------------------------
# Functions for llm_stance.qmd. Three labeled sections:
#   (1) LLM chat and rater construction (prompt builder is the transparency
#       point: the qmd prints its output verbatim)
#   (2) Sequential classification with checkpoint and sleep-and-retry
#   (3) Measurement QC: agreement, consensus, triage, gold validation
# Conventions: native |>, no loops (purrr), dplyr verbs namespaced, no ellmer
# attach (namespaced), nothing here reads or writes global state except via
# arguments and return values.

# ---- (1) LLM chat and rater construction ------------------------------------

# Provider routing off the model string. "openrouter/<slug>" (the slug itself
# contains a "/") routes to OpenRouter now; ANY other string is the WORK SWAP:
# an OpenAI-compatible endpoint with base URL and key from .Renviron
# (WORK_LLM_BASE_URL, and ellmer reads the key per its own convention).
# Temperature 0 and a pinned model tag are part of the measurement record.
make_chat <- function(model, system_prompt) {
  if (startsWith(model, "openrouter/")) {
    ellmer::chat_openrouter(
      model         = sub("^openrouter/", "", model),
      system_prompt = system_prompt,
      params        = ellmer::params(temperature = 0),
      echo          = "none")
  } else {
    ellmer::chat_openai(
      model         = model,
      base_url      = Sys.getenv("WORK_LLM_BASE_URL"),
      system_prompt = system_prompt,
      params        = ellmer::params(temperature = 0),
      echo          = "none")
  }
}

stance_labels <- c("favor", "neutral", "oppose", "irrelevant")

# The full system prompt, assembled from named components so the qmd can
# explain each one and print the result verbatim. `persona` is the
# analyst-supplied framing (may be ""); everything else is fixed by the
# methodology (construct definition, verbatim target, relevance gate,
# closed label set, ambiguity rule, blindness rule).
build_stance_prompt <- function(proposition, persona = "") {
  str_c(
    if (nzchar(persona)) str_c(persona, "\n") else
      "You are a careful, neutral annotator.\n",
    "TASK: measure STANCE, the author's position TOWARD THE STATED ",
    "PROPOSITION. Stance is not sentiment: an angry tone can still FAVOR ",
    "the proposition, and warm praise can OPPOSE it. Judge the position ",
    "toward the target, not the mood.\n",
    "PROPOSITION (the stance target): ", proposition, "\n",
    "RELEVANCE GATE: first decide whether the text discusses the target at ",
    "all; if it does not, the label is irrelevant.\n",
    "LABELS: exactly one of ", str_c(stance_labels, collapse = ", "), ".\n",
    "AMBIGUITY: if the text is genuinely ambiguous, pick the closest label ",
    "and set confidence to low rather than inventing certainty.\n",
    "BLINDNESS: judge only the text shown; ignore metadata and outside ",
    "knowledge.")
}

stance_schema <- function() {
  ellmer::type_object(
    label      = ellmer::type_enum(stance_labels,
                   "The single best-fitting category."),
    confidence = ellmer::type_enum(c("low", "medium", "high"),
                   "How clearly the text fits the chosen label."),
    rationale  = ellmer::type_string(
                   "One short clause (<= 15 words) naming the deciding cue."))
}

stance_rater <- function(proposition, model, persona = "") {
  list(chat   = make_chat(model, build_stance_prompt(proposition, persona)),
       schema = stance_schema(),
       model  = model)
}

# ---- (2) Sequential classification ------------------------------------------

# One paragraph, one call. The chat is clone()d per call so every
# classification starts from a fresh conversation: without the clone, ellmer
# appends each exchange to the same chat and later paragraphs would be judged
# with earlier ones in context, a contamination no reviewer should have to
# ask about. A failed call returns NA fields instead of aborting the run.
classify_one <- function(text, rater) {
  out <- tryCatch(
    rater$chat$clone()$chat_structured(text, type = rater$schema),
    error = function(e) NULL)
  if (is.null(out))
    return(tibble::tibble(label = NA_character_, confidence = NA_character_,
                          rationale = NA_character_))
  tibble::as_tibble(out[c("label", "confidence", "rationale")])
}

# One sequential pass over a text vector for one rater. No parallelism by
# design: the work API is called one request at a time.
classify_pass <- function(txt, ids, rater, model_name) {
  purrr::map(txt, classify_one, rater = rater) |>
    purrr::list_rbind() |>
    dplyr::bind_cols(tibble::tibble(id = ids, model = model_name, text = txt)) |>
    dplyr::select(id, model, text, label, confidence, rationale)
}

# Sleep-and-retry by recursion: after a pass, only failed texts are re-sent,
# so successes never re-spend budget. Production settings (retries = 16,
# retry_wait = 30 * 60) ride out full quota resets unattended; interactive
# defaults fail fast so a misconfiguration surfaces in seconds.
classify_with_retry <- function(txt, ids, rater, model_name,
                                retries, retry_wait) {
  res    <- classify_pass(txt, ids, rater, model_name)
  failed <- dplyr::filter(res, is.na(label))
  if (nrow(failed) == 0 || retries == 0) {
    if (nrow(failed) > 0)
      warning("[", model_name, "] ", nrow(failed),
              " texts still unclassified after all retries (label = NA).")
    return(res)
  }
  message("[", model_name, "] ", nrow(failed), " of ", length(txt),
          " calls failed; sleeping ", retry_wait, "s then retrying (",
          retries, " retries left).")
  Sys.sleep(retry_wait)
  retried <- classify_with_retry(failed$text, failed$id, rater, model_name,
                                 retries - 1, retry_wait)
  res |>
    dplyr::filter(!is.na(label)) |>
    dplyr::bind_rows(retried) |>
    dplyr::arrange(match(id, ids))
}

# THE SEAM between simulated and real runs: the only function the
# classification loop calls per (stratum, model). This is the REAL version;
# the qmd's fenced SIMULATED MODE section overrides it with a fake of
# identical signature, and nothing downstream branches on the mode.
run_rater <- function(paras, proposition, model_string, model_name,
                      persona, retries, retry_wait) {
  rater <- stance_rater(proposition, model_string, persona)
  classify_with_retry(paras$text, paras$para_uid, rater, model_name,
                      retries, retry_wait)
}

# ---- (3) Measurement QC ------------------------------------------------------

model_agreement <- function(classified) {
  wide <- classified |>
    dplyr::filter(!is.na(label)) |>
    dplyr::select(id, model, label) |>
    tidyr::pivot_wider(names_from = model, values_from = label) |>
    tidyr::drop_na()
  ratings <- as.data.frame(dplyr::select(wide, -id))
  kappa <- if (ncol(ratings) == 2) irr::kappa2(ratings)$value
           else irr::kappam.fleiss(ratings)$value
  list(kappa         = kappa,
       raw_agreement = mean(apply(ratings, 1,
                                  function(r) length(unique(r)) == 1L)),
       n_items       = nrow(ratings),
       confusion     = if (ncol(ratings) == 2)
         table(ratings[[1]], ratings[[2]], dnn = names(ratings)))
}

mode_label <- function(x) {
  tab <- table(x)
  names(tab)[order(-as.integer(tab), names(tab))][1]   # deterministic ties
}

consensus_label <- function(classified) {
  ok <- dplyr::filter(classified, !is.na(label))
  if (nrow(ok) < nrow(classified))
    message(nrow(classified) - nrow(ok), " failed classifications dropped.")
  ok |>
    dplyr::group_by(id) |>
    dplyr::summarise(consensus = mode_label(label),
                     n_models  = dplyr::n(),
                     n_agree   = max(table(label)),
                     unanimous = max(table(label)) == dplyr::n(),
                     .groups   = "drop")
}

triage_for_review <- function(classified, consensus) {
  classified |>
    dplyr::filter(!is.na(label)) |>
    dplyr::select(id, text, model, label) |>
    tidyr::pivot_wider(names_from = model, values_from = label) |>
    dplyr::inner_join(consensus, by = "id") |>
    dplyr::filter(!unanimous) |>
    dplyr::arrange(n_agree)
}

validate_against_gold <- function(classified, gold, model_name) {
  joined <- classified |>
    dplyr::filter(model == model_name, !is.na(label)) |>
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
  list(per_class = per_class,
       macro_f1  = mean(per_class$f1, na.rm = TRUE),
       accuracy  = mean(joined$label == joined$gold_label),
       kappa     = irr::kappa2(dplyr::select(joined, gold_label, label))$value,
       confusion = table(gold = joined$gold_label, pred = joined$label))
}
