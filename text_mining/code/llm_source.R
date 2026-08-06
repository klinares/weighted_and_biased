# llm_source.R ----------------------------------------------------------------
# Functions for llm_stance.qmd. Three sections:
#   (1) Prompts and chats: ONE four-part prompt builder (ROLE / TASK / RULES /
#       OUTPUT) and ONE provider router, instantiated per task.
#   (2) Calls: one sequential wrapper with sleep-and-retry. Batched where the
#       task is a group task (labeling, screening); one item per call where
#       shared context would contaminate the measurement (stance).
#   (3) Design: stratified two-stage sample (year strata, comment clusters,
#       paragraph elements) with a sample-size formula, plus agreement.
# Conventions: native |>, no loops, dplyr namespaced, ellmer namespaced.

# ---- (1) Prompts and chats ---------------------------------------------------

# Every prompt in this pipeline has the same four parts, in this order:
#   role   expertise framing, so the model reasons as the right kind of reader
#   task   what to do and how to self-check before answering
#   rules  guardrails against invention and drift (the anti-hallucination slot)
#   output what to return; paired with a structured schema that enforces it
build_prompt <- function(role, task, rules, output) {
  str_c("ROLE\n", role,
        "\n\nTASK\n", task,
        "\n\nRULES\n", str_c("- ", rules, collapse = "\n"),
        "\n\nOUTPUT\n", output)
}

# Provider routing off the model string. "openrouter/<slug>" at home; any
# other string is the work swap, an OpenAI-compatible endpoint whose base URL
# and key come from .Renviron and never from code.
make_chat <- function(model, system_prompt) {
  if (startsWith(model, "openrouter/")) {
    ellmer::chat_openrouter(
      model = sub("^openrouter/", "", model), system_prompt = system_prompt,
      params = ellmer::params(temperature = 0), echo = "none")
  } else {
    ellmer::chat_openai_compatible(
      model = model, base_url = Sys.getenv("COMPASS_API_URL"),
      api_key = Sys.getenv("COMPASS_API_KEY"), system_prompt = system_prompt,
      params = ellmer::params(temperature = 0), echo = "none")
  }
}

# ---- (2) Calls ---------------------------------------------------------------

# One structured call, returning NULL on failure instead of aborting a run.
# The chat is clone()d so each call starts from a fresh conversation: without
# it ellmer appends turns and later items are judged with earlier ones in
# context, a contamination no reviewer should have to ask about.
call_one <- function(chat, prompt, schema) {
  tryCatch(chat$clone()$chat_structured(prompt, type = schema),
           error = function(e) NULL)
}

# Sequential over prompts, with sleep-and-retry on failures only, so
# successes never re-spend budget. Recursion, not a loop. Production settings
# (retries 16, wait 30*60) ride out quota resets unattended; the interactive
# defaults fail fast so a misconfiguration surfaces in seconds.
call_many <- function(chat, prompts, schema, ids, retries, retry_wait) {
  res <- purrr::map(prompts, call_one, chat = chat, schema = schema)
  ok  <- !purrr::map_lgl(res, is.null)
  out <- tibble::tibble(id = ids, ok = ok,
                        result = purrr::map(res, ~ .x %||% list()))
  if (all(ok) || retries == 0) {
    if (!all(ok)) warning(sum(!ok), " calls failed after all retries.")
    return(out)
  }
  message(sum(!ok), " of ", length(prompts), " calls failed; sleeping ",
          retry_wait, "s (", retries, " retries left).")
  Sys.sleep(retry_wait)
  redo <- call_many(chat, prompts[!ok], schema, ids[!ok],
                    retries - 1, retry_wait)
  dplyr::bind_rows(out[ok, ], redo) |> dplyr::arrange(match(id, ids))
}

# ---- (3) Design and agreement ------------------------------------------------

# Sample size for a proportion, with a clustering design effect and the
# finite population correction. n0 = deff * z^2 p(1-p) / e^2, then
# n = n0 / (1 + n0/N). p = 0.5 is the conservative maximum-variance choice.
n_for_precision <- function(N, e, icc, b, p = 0.5, z = 1.96) {
  deff <- 1 + (b - 1) * icc
  n0   <- deff * z^2 * p * (1 - p) / e^2
  ceiling(min(N, n0 / (1 + n0 / N)))
}

# One topic's sample. Strata are years; the PSU is the comment; b paragraphs
# per sampled comment are taken by SRS. Allocation is proportionate to each
# year's comment count with a floor, so thin years stay estimable and the
# weights w = 1/pi undo the disproportionality. pi = (n_h/N_h) * (b/m_i).
draw_topic_sample <- function(pool, n_target, b, floor_comments, seed) {
  set.seed(seed)
  by_comment <- pool |>
    dplyr::group_by(Year, atom_id) |>
    dplyr::summarise(m_i = dplyr::n(), .groups = "drop")
  # Comments needed = target paragraphs / realized yield per comment. Yield is
  # mean(min(b, m_i)), NOT b: within one topic's frame a comment usually
  # contributes a single paragraph, so the cluster stage is often inert and
  # dividing by b would under-allocate several-fold.
  yield <- mean(pmin(b, by_comment$m_i))
  alloc <- by_comment |>
    dplyr::count(Year, name = "N_h") |>
    dplyr::mutate(
      target = pmax(floor_comments,
                    round(n_target / yield * N_h / sum(N_h))),
      n_h    = pmin(N_h, target))
  chosen <- by_comment |>
    dplyr::left_join(alloc, by = "Year") |>
    dplyr::group_by(Year) |>
    dplyr::mutate(.r = sample(dplyr::n())) |>
    dplyr::filter(.r <= n_h) |>
    dplyr::ungroup() |>
    dplyr::mutate(pi1 = n_h / N_h)
  pool |>
    dplyr::inner_join(chosen |> dplyr::select(atom_id, m_i, N_h, n_h, pi1),
                      by = "atom_id") |>
    dplyr::group_by(atom_id) |>
    dplyr::mutate(.r = sample(dplyr::n())) |>
    dplyr::filter(.r <= b) |>
    dplyr::ungroup() |>
    dplyr::mutate(pi2 = pmin(1, b / m_i), pi = pi1 * pi2, w = 1 / pi) |>
    dplyr::select(-.r)
}

# Cohen's kappa from two label vectors, with raw agreement and the confusion
# table. Kappa has no validated interpretive thresholds, so all three are
# reported together and none is translated into an adjective.
agreement <- function(a, b, dnn = c("a", "b")) {
  keep <- !is.na(a) & !is.na(b)
  a <- as.character(a[keep]); b <- as.character(b[keep])
  lev <- sort(union(a, b))
  po  <- mean(a == b)
  pe  <- sum(purrr::map_dbl(lev, ~ mean(a == .x) * mean(b == .x)))
  list(n = length(a), raw = po, kappa = (po - pe) / (1 - pe),
       confusion = table(a, b, dnn = dnn))
}

# Per-class precision, recall, F1 against a reference vector, plus macro-F1.
# Macro-F1 is the headline under class imbalance; accuracy is not.
class_metrics <- function(pred, ref) {
  keep <- !is.na(pred) & !is.na(ref)
  pred <- pred[keep]; ref <- ref[keep]
  per <- purrr::map(sort(union(pred, ref)), function(cl) {
    tp <- sum(pred == cl & ref == cl); fp <- sum(pred == cl & ref != cl)
    fn <- sum(pred != cl & ref == cl)
    pr <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    rc <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    tibble::tibble(class = cl, precision = pr, recall = rc,
                   f1 = if (!is.na(pr) && !is.na(rc) && pr + rc > 0)
                          2 * pr * rc / (pr + rc) else NA_real_,
                   support = sum(ref == cl))
  }) |> purrr::list_rbind()
  list(per_class = per, macro_f1 = mean(per$f1, na.rm = TRUE),
       accuracy = mean(pred == ref))
}

# ---- (4) The single LLM seam and the data dictionary --------------------------

# ONE seam for every LLM task in the pipeline. Real mode builds the chat and
# calls sequentially; simulated mode returns the caller's `fake` responses, so
# every downstream path runs with no key and no tokens. One branch, one place.
llm_task <- function(prompts, ids, schema, model, system_prompt,
                     retries, retry_wait, simulated = FALSE, fake = NULL) {
  if (simulated && !is.null(fake)) return(fake(ids))
  chat <- make_chat(model, system_prompt)
  call_many(chat, prompts, schema, ids, retries, retry_wait) |>
    dplyr::mutate(purrr::map_dfr(result, ~ tibble::as_tibble(.x))) |>
    dplyr::select(-result)
}

# Emit a data-dict.yaml describing the tables this pipeline writes, following
# the data-dict.yaml specification (Posit). Generated from the objects
# themselves so it cannot drift from what was written. NOTE: the spec assumes
# parquet or database tables; these outputs are CSV/RDS by project convention,
# so the document is spec-shaped but the data-dict CLI validator may not run
# against it directly.
emit_data_dict <- function(path, name, description, tables, glossary,
                           relationships = character()) {
  q <- function(x) str_c('"', str_replace_all(x, '"', "'"), '"')
  var_block <- function(df, descs) {
    purrr::map_chr(names(df), function(v) str_c(
      "      - name: ", v,
      "\n        type: ", dplyr::case_when(
        is.numeric(df[[v]]) && all(df[[v]] == round(df[[v]]), na.rm = TRUE) ~ "integer",
        is.numeric(df[[v]]) ~ "double",
        is.logical(df[[v]]) ~ "boolean",
        TRUE ~ "string"),
      "\n        description: ", q(descs[[v]] %||% "undocumented"))) |>
      str_c(collapse = "\n")
  }
  tbl_block <- purrr::map_chr(tables, function(t) str_c(
    "  - name: ", t$name,
    "\n    path: ", t$path,
    "\n    description: ", q(t$description),
    "\n    rows: ", nrow(t$data),
    "\n    variables:\n", var_block(t$data, t$vars))) |>
    str_c(collapse = "\n")
  glo_block <- purrr::map_chr(names(glossary), function(g) str_c(
    "  - term: ", q(g), "\n    definition: ", q(glossary[[g]]))) |>
    str_c(collapse = "\n")
  writeLines(str_c(
    "# data-dict.yaml (spec: https://data-dict.tidyverse.org/)\n",
    "# Generated by llm_stance.qmd; edit the script, not this file.\n",
    "name: ", name, "\n",
    "description: ", q(description), "\n",
    "version: ", format(Sys.Date()), "\n",
    "tables:\n", tbl_block, "\n",
    if (length(relationships))
      str_c("relationships:\n",
            str_c("  - ", relationships, collapse = "\n"), "\n") else "",
    "glossary:\n", glo_block, "\n"), path)
  invisible(path)
}
