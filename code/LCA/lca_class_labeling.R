# lca_class_labeling.R
# ============================================================================
# LLM-assisted labeling of latent classes from an LCA measurement model.
#
# WHAT THIS DOES
#   Reads a fitted LCA measurement model (for every item, the probability that a
#   member of each class gives each answer), together with the question wording
#   and the response-category labels, and asks an LLM to draft, for ONE class at a
#   time, a short label, a factual description anchored to that class's high-
#   probability answers, and the items that define it. The output is a draft for
#   an analyst to review and refine; it never feeds back into estimation.
#
# THE DESIGN
#   One LLM call per class. Showing a single class in isolation makes each call
#   one-to-one and avoids the cross-class interference that an "all classes at
#   once" prompt suffers on near-neighbor classes. The model reads the profile and
#   reports facts; the analyst makes the naming decision.
#
# READ THE DRAFTS WITH CARE
#   The drafts are least reliable for classes whose profiles are very similar.
#   Always check a label against its profile, and check near-neighbor classes
#   hardest. The output prints the items the model named beside the items that
#   objectively distinguish each class (total-variation distance) to support that.
#
# HOW TO USE
#   1. Set lca_dir and the measurement-model file name (DATA section). The reader
#      pulls prevalences (kind == "pi" rows) and response probabilities (kind ==
#      "rho" rows) from that one file.
#   2. Fill in the questions and response categories in the same section. Made-up
#      placeholders are there now; delete and replace them with your real wording.
#   3. Set gx$provider and gx$model, and put the matching key in ~/.Renviron.
#   4. Run the whole file (prints the labels), or source it and call
#      label_classes(model, gx).
#
# PROVIDERS
#   "openrouter" (cloud), "ollama" (local), or "work" (your organization's own
#   OpenAI-compatible endpoint). API keys are read from environment variables set
#   in ~/.Renviron; they are never written in this file.
#
# CONVENTIONS
#   Native pipe |> only, no for/while loops, dplyr::select namespace-qualified,
#   tidyverse loaded last. Edit the gx block and the DATA section only.
#
# Requires: ellmer, jsonlite, and the tidyverse pieces loaded below.
# ============================================================================

suppressPackageStartupMessages({
  library(readr)     # read_csv
  library(dplyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(tidyr)
})

# ============================================================================
# CONFIGURATION  (the only block you edit)
# ============================================================================
gx <- list(
  # ---- provider selection ------------------------------------------------
  provider    = "openrouter",            # "openrouter", "ollama", or "work"
  model       = "google/gemma-4-31b-it", # provider's model id; for ollama e.g. "gemma4:12b-it-qat"
  temperature = 0,                       # 0 minimizes randomness (cloud models are not bit-reproducible even at 0)
  seed        = 2026L,                   # seeds synthetic-data generation during validation

  # ---- provider endpoints and credentials --------------------------------
  # Keys are read from these environment variables. Set them in ~/.Renviron
  # (e.g. OPENROUTER_API_KEY=sk-...) and restart R. Never hard-code a key here.
  openrouter_key_env = "OPENROUTER_API_KEY",
  work_base_url      = "https://llm.internal.example/v1",  # your org's OpenAI-compatible endpoint
  work_key_env       = "WORK_LLM_API_KEY",
  ollama_url         = "http://localhost:11434"
  # For ollama, pre-load the model once (e.g. `ollama run <model>`) so the first
  # call does not time out on a cold start.
)

# ============================================================================
# 1. PROVIDER LAYER  (the LLM call, one place to add or change a provider)
# ============================================================================
# NOTE ON ellmer VERSIONS. The argument for temperature/seed (params() here) and
# the exact chat_*() signatures vary across ellmer versions. If params() errors,
# the documented alternative is api_args = list(temperature = ..., seed = ...).
# If chat_openrouter is missing, chat_openai(base_url = "https://openrouter.ai/
# api/v1", api_key = Sys.getenv(gx$openrouter_key_env), model = gx$model) is the
# equivalent. Confirm the model id with the provider; a wrong id returns a 404.

# Build a chat handle for the configured provider. This is the single switch to
# touch when adding a provider or changing how one is reached.
make_chat <- function(system_prompt, gx) {
  prm <- ellmer::params(temperature = gx$temperature, seed = gx$seed)
  switch(
    gx$provider,
    openrouter = ellmer::chat_openrouter(
      model = gx$model, system_prompt = system_prompt, params = prm
    ),
    ollama = ellmer::chat_ollama(
      model = gx$model, system_prompt = system_prompt,
      base_url = gx$ollama_url, params = prm
    ),
    work = ellmer::chat_openai(
      base_url = gx$work_base_url, api_key = Sys.getenv(gx$work_key_env),
      model = gx$model, system_prompt = system_prompt, params = prm
    ),
    stop('Unknown gx$provider: ', gx$provider, '. Use "openrouter", "ollama", or "work".')
  )
}

# One request: system prompt in, model's text reply out.
call_llm <- function(system_prompt, user_prompt, gx) {
  if (!requireNamespace("ellmer", quietly = TRUE))
    stop("Install ellmer first: install.packages('ellmer')")
  make_chat(system_prompt, gx)$chat(user_prompt, echo = FALSE)
}

# Pull the single JSON object out of a reply. Each task here returns one object
# per call, so we keep nested fields as lists (simplifyVector = FALSE) and, if the
# whole reply does not parse, grab the outermost {...} block (handles stray prose
# or markdown fences the model may add despite the rules).
parse_json <- function(txt) {
  grab <- function(s) jsonlite::fromJSON(s, simplifyVector = FALSE)
  out  <- tryCatch(grab(txt), error = function(e) NULL)
  if (!is.null(out)) return(out)
  m <- regmatches(txt, regexpr("(?s)\\{.*\\}", txt, perl = TRUE))
  if (length(m) == 0) stop("No JSON object found in the model reply:\n", txt)
  grab(m)
}

# Confirm the provider is reachable (and the key is set) with one trivial call.
# Returns TRUE/FALSE so the entry points can stop cleanly with a message.
ensure_ready <- function(gx) {
  key_env <- switch(gx$provider,
                    openrouter = gx$openrouter_key_env,
                    work       = gx$work_key_env,
                    ollama     = NA_character_)
  if (!is.na(key_env) && !nzchar(Sys.getenv(key_env))) {
    message("Environment variable ", key_env, " is not set. Add it to ~/.Renviron ",
            "and restart R.")
    return(FALSE)
  }
  tryCatch({
    call_llm("You reply with one word.", "Reply with the single word: ready.", gx)
    TRUE
  }, error = function(e) {
    message("Provider check failed (", gx$provider, "): ", conditionMessage(e)); FALSE
  })
}

# ============================================================================
# 2. THE MEASUREMENT MODEL  (the input both tools read)
# ============================================================================
# A "model" is a plain list:
#   items      tibble with columns `item` (short code) and `text` (question
#              wording); an optional `theme` column is shown if present.
#   probs      named list keyed by item code; each element a matrix with one ROW
#              per response category (rownames = the category labels shown to the
#              model, e.g. "Strongly agree") and one COLUMN per class.
#   prevalence optional numeric vector of class shares (length = number of classes).
#   context    optional one-line description of the survey topic.
# The number of classes is ncol() of any probs matrix.

# Generic constructor. Use this with any fitted model once you have its per-class
# response probabilities in categories-by-class matrices.
model_from_parts <- function(items, probs, prevalence = NULL, context = NULL) {
  stopifnot(all(c("item", "text") %in% names(items)))
  stopifnot(all(items$item %in% names(probs)))
  list(items = items, probs = probs[items$item], prevalence = prevalence, context = context)
}

# Read the measurement model from your LCA export. It is one long file with a
# `kind` column: kind == "pi" rows give class prevalences (in `estimate`, keyed by
# `class`); kind == "rho" rows give response probabilities (in `estimate`, keyed by
# `item`, `category`, `class`). Columns are kind, item, category, class, estimate
# (se/lo/hi are the confidence bounds, not needed for labeling). Set the *_col
# arguments only if your column names differ; the function errors and lists the
# columns it found. Returns list(probs = <named category-by-class matrices>,
# prevalence). Rows of each matrix are ordered by category ascending (low to high)
# and columns by class id ascending, so prevalence[k] matches the k-th column.
read_lca_outputs <- function(mm_path,
                             kind_col = "kind", item_col = "item",
                             category_col = "category", class_col = "class",
                             estimate_col = "estimate") {
  mm <- read_csv(mm_path, show_col_types = FALSE)
  miss <- setdiff(c(kind_col, item_col, category_col, class_col, estimate_col), names(mm))
  if (length(miss))
    stop("measurement-model CSV is missing column(s): ", paste(miss, collapse = ", "),
         ".\nColumns found: ", paste(names(mm), collapse = ", "),
         ".\nSet the *_col arguments of read_lca_outputs() to your column names.")

  # class prevalences: the kind == "pi" rows, ordered by class id
  pi_df <- mm |>
    dplyr::filter(.data[[kind_col]] == "pi") |>
    dplyr::arrange(.data[[class_col]])
  prevalence <- as.numeric(pi_df[[estimate_col]])

  # one category-by-class matrix per item from the kind == "rho" rows
  rho <- mm |> dplyr::filter(.data[[kind_col]] == "rho")
  build_item <- function(it) {
    w <- rho |>
      dplyr::filter(.data[[item_col]] == it) |>
      dplyr::arrange(.data[[category_col]]) |>
      dplyr::select(all_of(c(category_col, class_col, estimate_col))) |>
      pivot_wider(names_from = all_of(class_col), values_from = all_of(estimate_col))
    m <- as.matrix(w[, -1, drop = FALSE])
    rownames(m) <- as.character(w[[category_col]])
    m[, order(as.numeric(colnames(m))), drop = FALSE]
  }
  items <- unique(rho[[item_col]])
  probs <- set_names(map(items, build_item), items)

  list(probs = probs, prevalence = prevalence)
}

# Number of classes in a model.
n_classes <- function(model) ncol(model$probs[[1]])

# Per-class, per-item discrimination: total-variation distance between class k's
# response distribution on an item and the average of the other classes. The top_d
# items per class are the ones that objectively most set the class apart; printed
# next to the model's named items so the analyst can sanity-check the draft.
distinguishing_items <- function(model, top_d = 3L) {
  K <- n_classes(model)
  map_dfr(names(model$probs), function(code) {
    m <- model$probs[[code]]                                    # categories x classes
    tibble(item = code, class = seq_len(K),
           discrim = map_dbl(seq_len(K), function(k) {
             others <- rowMeans(m[, -k, drop = FALSE])
             0.5 * sum(abs(m[, k] - others))
           }))
  }) |>
    group_by(class) |>
    slice_max(discrim, n = top_d, with_ties = FALSE) |>
    summarise(distinguishing = list(item), .groups = "drop")
}

# ============================================================================
# 3. PROMPTS  (edit these to test wording variants)
# ============================================================================
# The persona and rules are shared; each task adds its own instruction and schema.

persona <- function() {
  paste(
    "You are a senior survey methodologist who reads latent class measurement",
    "models. Each class is described only by its item-response probabilities: for",
    "every item, the probability that a member gives each answer. A class leans",
    "toward the answers with high probability. You interpret a class strictly from",
    "these probabilities and the item wording, never from outside assumptions."
  )
}

rules_block <- paste(
  "RULES:",
  "1. Use only the response probabilities and item wording shown.",
  "2. Anchor every statement to the high-probability answers of this class.",
  "3. If the profile is diffuse (no clear high-probability answers), say so.",
  "4. Return only valid JSON: no prose before or after, no markdown fences.",
  sep = "\n"
)

# Optional survey-topic line, plus a one-class profile block.
survey_context <- function(model) {
  if (!is.null(model$context) && nzchar(model$context))
    str_glue("SURVEY CONTEXT\n  {model$context}\n\n")
  else ""
}

# Render ONE class: each item with its (optional) theme tag, wording, and the
# probability of every labeled response category for that class.
format_class_block <- function(model, k) {
  has_theme <- "theme" %in% names(model$items)
  lines <- map_chr(seq_len(nrow(model$items)), function(i) {
    code  <- model$items$item[i]
    tag   <- if (has_theme) paste0(" [", model$items$theme[i], "]") else ""
    pr    <- model$probs[[code]][, k]
    labs  <- rownames(model$probs[[code]])
    probs <- paste(sprintf("P(%s)=%.2f", labs, pr), collapse = ", ")
    str_glue('  {code}{tag} "{model$items$text[i]}"\n      {probs}')
  })
  prev <- if (!is.null(model$prevalence))
    str_glue(" (estimated prevalence {round(100 * model$prevalence[k])}%)") else ""
  str_glue("CLASS {k}{prev}:\n", paste(lines, collapse = "\n"))
}

# ANALYST task: draft label + factual description + defining items, one class shown.
prompt_label <- function(model, k) {
  str_glue(
    "{survey_context(model)}",
    "ONE CLASS FROM THE MODEL\n{format_class_block(model, k)}\n\n",
    "TASK\n",
    "Read this single class and return: a short DRAFT label (2 to 5 words) for an ",
    "analyst to refine, a one or two sentence factual description anchored to its ",
    "high-probability answers, and the AT MOST 3 item codes whose answers most ",
    "define this class. Name fewer than 3 if fewer stand out; never more than 3.\n\n",
    "{rules_block}\n",
    'JSON (one object): {{"draft_label": "...", "description": "...", "defining_items": ["i1", "i2"]}}'
  )
}

# ============================================================================
# 4. ANALYST TOOL: draft labels for a real model
# ============================================================================
# One call per class. Returns a tidy tibble (class, draft_label, description,
# named_items, distinguishing, prevalence) and prints a review-oriented view: the
# drafts, then the items the model named beside the items that objectively
# distinguish each class. Review every label against its profile before using it.
label_classes <- function(model, gx) {
  if (!ensure_ready(gx)) return(invisible(NULL))
  sys <- persona()

  drafts <- map_dfr(seq_len(n_classes(model)), function(k) {
    obj <- parse_json(call_llm(sys, prompt_label(model, k), gx))
    tibble(
      class       = k,
      draft_label = pluck(obj, "draft_label", .default = NA_character_),
      description = pluck(obj, "description", .default = NA_character_),
      named_items = list(str_trim(unlist(pluck(obj, "defining_items", .default = list()))))
    )
  })

  out <- drafts |>
    left_join(distinguishing_items(model), by = "class") |>
    mutate(prevalence = if (!is.null(model$prevalence)) model$prevalence[class] else NA_real_) |>
    dplyr::select(class, prevalence, draft_label, description, named_items, distinguishing)

  cat("Draft class labels (review each against its profile before using):\n")
  print(out |> dplyr::select(class, prevalence, draft_label, description),
        n = Inf, width = Inf)
  cat("\nItems the model named as defining  vs  items that objectively distinguish (TV distance):\n")
  print(out |> transmute(class,
                         model_named   = map_chr(named_items,    ~ paste(.x, collapse = ", ")),
                         distinguishes = map_chr(distinguishing, ~ paste(.x, collapse = ", "))),
        n = Inf, width = Inf)

  invisible(out)
}

# ============================================================================
# 5a. READ LCA OUTPUTS (CSV)  (set the folder and file name; this part stays)
# ============================================================================
# THE ONLY PLACE TO EDIT THE LOCATION. Point lca_dir at the folder and name the
# measurement-model file (forward slashes; R accepts them on Windows). Everything
# the labeler needs is in this one file: the kind == "pi" rows hold prevalences and
# the kind == "rho" rows hold the response probabilities.
lca_dir <- "D:/repos/weighted_and_biased/code/LCA"
mm_csv  <- file.path(lca_dir, "lca_measurement_model_with_CIs.csv")

lca <- read_lca_outputs(mm_csv)   # columns: kind, item, category, class, estimate
# -> list(probs = <named list of category-by-class matrices>, prevalence)
# Item codes are names(lca$probs); key the questions below to them.

# ============================================================================
# 5b. QUESTIONS AND RESPONSE CATEGORIES  (DELETE and replace with your real text)
# ============================================================================
# These are derived from your fit so they ALWAYS match it and the script runs as
# is: each item gets a placeholder question and its categories are numbered. The
# labels stay generic until you replace them with the real wording, which is what
# makes the output useful.
question_text   <- set_names(paste("Survey item", names(lca$probs)), names(lca$probs))
response_labels <- map(lca$probs, ~ paste0("Category ", seq_len(nrow(.x))))
context         <- "REPLACE with a one-line description of your survey topic."

# To put in the real text, write one entry per item code (your codes are
# names(lca$probs)), response labels ordered low to high, one per category. E.g.:
#   question_text <- c(
#     q1 = "I trust the government to negotiate a treaty in the public interest.",
#     q2 = "A treaty would open valuable trade opportunities."
#   )
#   response_labels <- list(
#     q1 = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
#     q2 = c("No", "Yes")
#   )

# ============================================================================
# 5c. ASSEMBLE  (no need to edit)
# ============================================================================
# Check the wording and labels line up with the fit, attach the response labels to
# the probability matrices, and build the model object the labeler reads.
if (!setequal(names(lca$probs), names(question_text)))
  stop("question_text codes do not match the fitted items (names(lca$probs)).")
if (!setequal(names(lca$probs), names(response_labels)))
  stop("response_labels codes do not match the fitted items (names(lca$probs)).")
walk(names(lca$probs), function(code) {
  if (length(response_labels[[code]]) != nrow(lca$probs[[code]]))
    stop("response_labels[['", code, "']] has ", length(response_labels[[code]]),
         " labels but item '", code, "' has ", nrow(lca$probs[[code]]), " categories.")
})

probs <- imap(lca$probs, function(m, code) { rownames(m) <- response_labels[[code]]; m })
items <- tibble(item = names(question_text), text = unname(question_text))
model <- model_from_parts(items, probs, prevalence = lca$prevalence, context = context)

# ============================================================================
# 6. RUN
# ============================================================================
# Running this file builds the model above and prints the draft labels. If you
# source the file instead, call label_classes(model, gx) yourself.
if (sys.nframe() == 0L) {
  labels <- label_classes(model, gx)
}
