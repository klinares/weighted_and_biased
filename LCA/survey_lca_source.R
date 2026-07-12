# survey_lca_source.R
# ============================================================================
# Reusable source code for design-weighted latent class analysis.
# Sourced by survey_lca_analysis.qmd; keep it in the same folder when rendering.
#
# Contents (three sections):
#   1. Plotting and data-preparation helpers
#   2. The weighted EM engine and label alignment
#   3. LLM class labeling (per-class calls; analyst CSV takes precedence)
#
# These functions take their inputs as arguments and hold no analysis-specific
# state, with one documented exception kept from the original script: fit_lca()
# seeds its random starts from a global `cfg$seed`, so a `cfg` object carrying a
# `seed` element must exist when fit_lca() is called. The .qmd defines it before
# any call.
#
# Dependencies (loaded by the .qmd): tidyverse, matrixStats, clue. Section 3
# additionally uses ellmer, jsonlite, sjlabelled, and readr, all namespace-
# qualified so nothing extra is attached. There are no for/while loops anywhere;
# iteration is done with purrr and matrix algebra.
# ============================================================================

`%||%` <- rlang::`%||%`   # null-coalescing helper used in the EM fold


# ============================================================================
# 1. PLOTTING AND DATA-PREPARATION HELPERS
# ============================================================================

# A clean, colorblind-safe plotting theme reused by every figure.
theme_lca <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold", size = rel(0.85)),
          plot.title = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = rel(0.85)),
          plot.caption = element_text(hjust = 0, size = rel(0.78), color = "grey30"))
}

# Map any configured missing/DK codes to NA, leaving every other value intact.
to_na <- function(x, codes) { if (!is.null(codes)) x[x %in% codes] <- NA; x }

# Recode a vector to consecutive integers 1..C over its substantive (non-NA)
# values, preserving NA. Items of different lengths are handled the same way.
recode_consecutive <- function(x) as.integer(factor(x, levels = sort(unique(x[!is.na(x)]))))


# ============================================================================
# 2. THE WEIGHTED EM ENGINE AND LABEL ALIGNMENT
# ============================================================================

# Random starting values: a random class distribution and random response-prob matrices.
rand_init <- function(cats, K) {
  list(pi  = { x <- runif(K); x / sum(x) },
       rho = map(cats, function(Cj) {
         m <- matrix(runif(Cj * K) + 0.1, Cj, K); sweep(m, 2, colSums(m), "/")
       }))
}

# One full EM run, written as a fold (no loops). Y is a list of integer item vectors;
# OH is a list of one-hot indicator matrices (one per item).
em_run <- function(Y, OH, cats, w, K, init = NULL, maxit = 800L, tol = 1e-8) {
  nn <- length(Y[[1]])
  st0 <- c(init %||% rand_init(cats, K),
           list(post = NULL, ll = -Inf, iter = 0L, done = FALSE))

  step <- function(state, .iter) {
    if (isTRUE(state$done)) return(state)
    # E-step on the log scale. log(rho_j)[y, ] picks each respondent's answered-category
    # log-probability for item j; a missing (NA) answer contributes 0 (drops out of the
    # product). Summed across items and added to the log class prior.
    log_terms <- map2(state$rho, Y, function(rho_j, y) {
      lp <- log(rho_j)[y, , drop = FALSE]; lp[is.na(lp)] <- 0; lp
    })
    logdens <- reduce(log_terms, `+`) + matrix(log(state$pi), nn, K, byrow = TRUE)
    lse  <- matrixStats::rowLogSumExps(logdens)
    post <- exp(logdens - lse)
    ll   <- sum(w * lse)
    # M-step: weighted proportions. crossprod(OH_j, w*post) sums weighted responsibilities
    # over respondents who gave each category; columns renormalized to valid probabilities.
    wp  <- w * post
    den <- colSums(wp)
    rho_n <- map(OH, function(oh) {
      num <- pmax(crossprod(oh, wp), 1e-12)
      sweep(num, 2, colSums(num), "/")
    })
    list(pi = den / sum(den), rho = rho_n, post = post, ll = ll,
         iter = state$iter + 1L,
         done = abs(ll - state$ll) < tol * (abs(state$ll) + 1))
  }
  out <- reduce(seq_len(maxit), step, .init = st0)
  out$converged <- out$done
  out
}

# Build integer item vectors and one-hot category-indicator matrices (NA rows zeroed,
# so a missing answer contributes nothing to that item's response-probability update).
make_inputs <- function(df, items, cats) {
  Y  <- map(items, ~ as.integer(df[[.x]]))
  OH <- map2(items, cats, function(it, Cj) {
    oh <- (outer(as.integer(df[[it]]), seq_len(Cj), `==`)) + 0
    oh[is.na(oh)] <- 0
    oh
  })
  list(Y = Y, OH = OH)
}

# Posterior class probabilities for any responses under FIXED parameters (the E-step).
# Out-of-range or missing item codes contribute nothing, so this scores complete or
# partial response patterns alike.
posterior_of <- function(pi, rho, Y) {
  nn <- length(Y[[1]]); K <- length(pi)
  log_terms <- map2(rho, Y, function(rho_j, y) {
    y[y < 1 | y > nrow(rho_j)] <- NA
    lp <- log(rho_j)[y, , drop = FALSE]; lp[is.na(lp)] <- 0; lp
  })
  logdens <- reduce(log_terms, `+`) + matrix(log(pi), nn, K, byrow = TRUE)
  exp(logdens - matrixStats::rowLogSumExps(logdens))
}

# Assign class membership to ANY respondents from a fitted model: returns the data with
# posterior columns, modal class, and the maximum posterior. The same call scores an
# external data frame of new respondents that carries the item columns.
score_lca <- function(newdata, fit, items) {
  Y    <- map(items, ~ as.integer(newdata[[.x]]))
  post <- posterior_of(fit$pi, fit$rho, Y)
  colnames(post) <- paste0("post_class", seq_along(fit$pi))
  mi   <- max.col(post, ties.method = "first")
  newdata |>
    bind_cols(as_tibble(post)) |>
    mutate(modal_class = mi, max_posterior = post[cbind(seq_len(n()), mi)])
}

# Class labels are arbitrary; align any fit to a reference by matching response profiles
# with the Hungarian algorithm, so classes are comparable across starts, fits, and replicates.
profiles_of <- function(rho) do.call(cbind, map(rho, t))   # K x sum(Cj)
align_to <- function(fit, ref) {
  K <- length(fit$pi)
  Pf <- profiles_of(fit$rho); Pr <- profiles_of(ref$rho)
  cost <- outer(seq_len(K), seq_len(K),
                Vectorize(function(a, b) sum((Pf[a, ] - Pr[b, ])^2)))
  asg <- clue::solve_LSAP(cost)
  inv <- integer(K); inv[as.integer(asg)] <- seq_len(K)
  list(pi = fit$pi[inv],
       rho = map(fit$rho, ~ .x[, inv, drop = FALSE]),
       post = if (!is.null(fit$post)) fit$post[, inv, drop = FALSE] else NULL,
       ll = fit$ll, converged = fit$converged %||% NA)
}

# Fit at a given K from many random starts; keep the best weighted log-likelihood.
# Bivariate residuals (BVR): a local-independence diagnostic for each item PAIR.
# For items a and b, compare the design-weighted observed two-way table with the
# table the fitted model implies, p_exp(r, s) = sum_k pi_k rho_a[r, k] rho_b[s, k],
# as a Pearson X2 on proportions scaled by n, divided by (Ca-1)(Cb-1). Under a
# weighted pseudo-likelihood and a complex design the chi-square reference does not
# apply, so the value is a descriptive index for RANKING pairs, not a test.
# rho in the fit is positional in `items` order, so items must be passed in the
# same order used to fit.
bvr_pairs <- function(df, w, items, fit) {
  n  <- length(w)
  W  <- sum(w)
  pr <- t(utils::combn(seq_along(items), 2L))
  map_dfr(seq_len(nrow(pr)), function(i) {
    a <- pr[i, 1]; b <- pr[i, 2]
    Ca <- nrow(fit$rho[[a]]); Cb <- nrow(fit$rho[[b]])
    obs <- as.matrix(stats::xtabs(w ~ factor(df[[items[a]]], seq_len(Ca)) +
                                      factor(df[[items[b]]], seq_len(Cb)))) / W
    exp_p <- fit$rho[[a]] %*% (fit$pi * t(fit$rho[[b]]))   # Ca x Cb model-implied
    x2 <- n * sum((obs - exp_p)^2 / exp_p)
    tibble(item_a = items[a], item_b = items[b],
           df  = (Ca - 1L) * (Cb - 1L),
           bvr = x2 / ((Ca - 1L) * (Cb - 1L)))
  }) |>
    arrange(desc(bvr))
}

fit_lca <- function(df, w, cats, items, K, starts, ref = NULL) {
  inp <- make_inputs(df, items, cats)
  cands <- map(seq_len(starts), function(s) {
    set.seed(cfg$seed + s + 17L * K)
    em_run(inp$Y, inp$OH, cats, w, K)
  })
  best <- cands[[which.max(map_dbl(cands, "ll"))]]
  if (!is.null(ref)) best <- align_to(best, ref)
  best
}

# Parameter counts and relative entropy, used by the model-selection criteria.
df_k <- function(K, cats) (K - 1) + K * sum(cats - 1)

entropy_R2 <- function(post, K) {
  if (K == 1) return(NA_real_)
  1 - (-sum(post * log(pmax(post, 1e-12)))) / (nrow(post) * log(K))
}


# ============================================================================
# 3. LLM CLASS LABELING
# ============================================================================
# One model call PER CLASS, a design chosen empirically: a joint all-classes
# prompt systematically confuses the closest class pairs (a forced one-to-one
# assignment lets one confusion corrupt two classes), and a tested two-stage
# variant (a global comparison pass feeding each per-class call) changed no
# stance cell, so isolation costs no accuracy and buys clean, independently
# frozen per-class results. Labels are DRAFTS for the analyst to verify against
# the response profiles; they never feed back into estimation.
#
# Precedence (get_class_labels):
#   1. cfg$class_labels_csv set  -> read the analyst's CSV (columns K, Label,
#      Description). Requires cfg$K_force, because analyst labels are only
#      meaningful for a pinned K; the LLM never runs.
#   2. A frozen LLM output file exists in cfg$out_dir -> reload it (no repeat calls; edit that
#      file and point cfg$class_labels_csv at it to take over permanently).
#   3. Otherwise call the LLM once per class and freeze the result to CSV.
#
# Optional context (cfg$survey_context): ONE free-text sentence (country, year,
# topic) rendered as a SURVEY CONTEXT line to resolve what the items refer to;
# rule 1 then fences it to referent-resolution only. NULL (the default) omits
# the line and reproduces the certified context-free prompt exactly.
#
# Provider (lca_chat): one OpenAI-compatible branch. At work,
# cfg$compass_base_url is your endpoint and cfg$llm_model its model. At home,
# point it at OpenRouter (base_url "https://openrouter.ai/api/v1", model e.g.
# "google/gemma-4-31b-it" or "meta-llama/llama-4-maverick") and set
# OPENAI_API_KEY to the OpenRouter key for the session. Keys are never stored
# here; .Rprofile / .Renviron supply them.

# Question wording and response-category labels for the prompt, from sjlabelled
# attributes when present (labelled real data); item codes and "Category 1..C"
# otherwise (the simulator). Labels attached to na_codes values (98/99-style
# "Don't know"/"Refused") are dropped FIRST, because those codes become NA in
# preparation and their labels would otherwise break the count guard below,
# silently costing the substantive wording on exactly the items that have it.
# After the drop, attribute labels are used only when their count matches the
# fitted category count, since recoding can compress sparse codes.
item_meta <- function(df, items, cats, na_codes = NULL) {
  map(set_names(items), function(it) {
    q    <- sjlabelled::get_label(df[[it]])
    labs <- sjlabelled::get_labels(df[[it]])
    vals <- sjlabelled::get_values(df[[it]])
    if (!is.null(na_codes) && length(labs) == length(vals))
      labs <- labs[!vals %in% na_codes]
    C    <- cats[[it]]
    list(question  = if (length(q) == 1 && nzchar(q)) q else it,
         responses = if (length(labs) == C) labs else paste("Category", seq_len(C)))
  })
}

# The persona and rules are the measurement instrument; edit them only with the
# obedience experiment (llm_label_obedience_experiment.R) re-run afterwards.
lca_persona <- function() {
  paste(
    "You are a senior survey methodologist who reads latent class measurement",
    "models. Each class is described only by its item-response probabilities:",
    "for every survey item, the probability that a member of that class gives",
    "each answer. A class leans toward the answers with high probability. You",
    "interpret a class strictly from these probabilities and the item wording,",
    "never from outside assumptions."
  )
}
# Rule 1 gains a fence when survey context is supplied: context resolves what
# the items refer to and licenses nothing else. With context = FALSE the text is
# byte-identical to the certified context-free prompt.
lca_rules <- function(context = FALSE) {
  r1 <- if (context)
    paste("1. Use only the response probabilities and item wording shown. The survey",
          "   context only clarifies what the items refer to; attribute nothing to",
          "   the class that the probabilities do not show.", sep = "\n")
  else "1. Use only the response probabilities and item wording shown."
  paste("RULES:", r1,
        "2. Anchor every statement to the high-probability answers of this class.",
        "3. If the profile is diffuse (no clear high-probability answers), say so.",
        "4. Return only valid JSON: no prose before or after, no markdown fences.",
        sep = "\n")
}

# Render ONE class: every item with its wording and the probability of each
# labeled response category for that class.
format_class_block <- function(fit, k, meta, items) {
  lines <- map_chr(seq_along(items), function(j) {
    m  <- meta[[items[j]]]
    pr <- fit$rho[[j]][, k]
    probs <- paste(sprintf("P(%s)=%.2f", m$responses, pr), collapse = ", ")
    stringr::str_glue('  {items[j]} "{m$question}"\n      {probs}')
  })
  stringr::str_glue(
    "CLASS {k} (estimated prevalence {round(100 * fit$pi[k])}%):\n",
    paste(lines, collapse = "\n"))
}

# `context` is one optional free-text sentence (cfg$survey_context): country,
# year, topic, mode, whatever resolves the items' referents. NULL omits the
# line entirely and reproduces the certified context-free prompt byte for byte.
prompt_class_label <- function(fit, k, meta, items, context = NULL) {
  has_ctx <- !is.null(context) && nzchar(context)
  ctx <- if (has_ctx) stringr::str_glue("SURVEY CONTEXT\n{context}\n\n") else ""
  rules_txt <- lca_rules(context = has_ctx)
  stringr::str_glue(
    "{ctx}",
    "ONE CLASS FROM A LATENT CLASS MEASUREMENT MODEL\n",
    "{format_class_block(fit, k, meta, items)}\n\n",
    "TASK\n",
    "Read this single class and return: a short DRAFT label (2 to 5 words) for ",
    "an analyst to refine, and a one or two sentence factual description ",
    "anchored to its high-probability answers.\n\n",
    "{rules_txt}\n",
    'JSON (one object): {{"label": "...", "description": "..."}}')
}

# Chat handle for the configured provider (a FRESH handle per class, so no
# cross-class context bleeds between calls). Temperature 0 and the master seed
# make the calls as deterministic as the provider allows; cloud sampling is not
# bit-reproducible even so, which is why labels are FROZEN to CSV after the
# first run (the freeze, not the seed, is the reproducibility mechanism). If
# your ellmer version rejects params(), the documented fallback is
# api_args = list(temperature = 0, seed = cfg$seed).
lca_chat <- function(cfg) {
  prm <- ellmer::params(temperature = 0, seed = cfg$seed)
  if (is.null(cfg$llm_key_env)) {
    # key resolved by ellmer's default (OPENAI_API_KEY), supplied by .Rprofile
    ellmer::chat_openai(base_url = cfg$compass_base_url, model = cfg$llm_model,
                        system_prompt = lca_persona(), params = prm)
  } else {
    # key read from the NAMED env var (e.g. OPENROUTER_API_KEY in .Renviron);
    # nothing is ever stored in code
    ellmer::chat_openai(base_url = cfg$compass_base_url, model = cfg$llm_model,
                        api_key = Sys.getenv(cfg$llm_key_env),
                        system_prompt = lca_persona(), params = prm)
  }
}

# Pull the single JSON object out of a reply. Markdown fences are stripped
# FIRST (gemma in particular fences valid JSON despite rule 4; observed in the
# obedience experiment), then stray surrounding text is tolerated as a fallback.
parse_label_json <- function(txt) {
  txt  <- stringr::str_remove_all(txt, stringr::regex("```(json)?", ignore_case = TRUE))
  grab <- function(s) jsonlite::fromJSON(s, simplifyVector = FALSE)
  out  <- tryCatch(grab(txt), error = function(e) NULL)
  if (!is.null(out)) return(out)
  m <- regmatches(txt, regexpr("(?s)\\{.*\\}", txt, perl = TRUE))
  if (length(m) == 0) stop("No JSON object in the model reply:\n", txt)
  grab(m)
}

# One call per class; returns tibble(K, Label, Description) in class order.
label_classes_llm <- function(fit, df, items, cats, cfg) {
  meta <- item_meta(df, items, cats, cfg$na_codes)
  map_dfr(seq_along(fit$pi), function(k) {
    obj <- parse_label_json(
      lca_chat(cfg)$chat(prompt_class_label(fit, k, meta, items, cfg$survey_context), echo = FALSE))
    tibble(K = k,
           Label       = purrr::pluck(obj, "label",       .default = NA_character_),
           Description = purrr::pluck(obj, "description", .default = NA_character_))
  })
}

# ---- Label harmonization (one bounded EDITING call, gated on collision) ----
# Per-class isolation has one blind spot: two near-neighbor classes can draft
# the same label, since neither call saw the other. The fix is NOT joint
# profile-reading (tested; it changed nothing and risks anchoring): it is one
# closing call that sees only the finished (label, description) pairs, edits
# labels ONLY where they collide, minimally, anchored to each class's own
# description, and never touches descriptions. It runs only when a mechanical
# collision check fires, so most runs make no extra call. Draft labels are kept
# in Label_draft so every harmonizer edit is auditable in the frozen CSV.

# TRUE when any two labels are duplicates or share most of their words.
labels_collide <- function(labels) {
  ws <- map(stringr::str_squish(tolower(labels)), ~ unique(strsplit(.x, " ")[[1]]))
  pr <- t(utils::combn(length(labels), 2L))
  jac <- map_dbl(seq_len(nrow(pr)), function(i) {
    a <- ws[[pr[i, 1]]]; b <- ws[[pr[i, 2]]]
    length(intersect(a, b)) / length(union(a, b))
  })
  any(jac >= 0.5)
}

prompt_harmonize <- function(lab) {
  rows <- stringr::str_glue_data(lab, "CLASS {K}: LABEL \"{Label}\" | DESCRIPTION: {Description}")
  stringr::str_glue(
    "DRAFT LABELS FOR THE CLASSES OF ONE LATENT CLASS MODEL\n",
    "{paste(rows, collapse = '\n')}\n\n",
    "TASK\n",
    "Some labels are too similar to tell apart. Edit ONLY the labels that ",
    "overlap, as little as possible, so every label is distinct; anchor each ",
    "edit to that class's own description. Keep every non-overlapping label ",
    "verbatim. Do not change any description. Labels stay 2 to 5 words.\n\n",
    "{lca_rules()}\n",
    'JSON (one array, all classes): [{{"class": 1, "label": "..."}}, ...]')
}

harmonize_labels <- function(lab, cfg) {
  if (!labels_collide(lab$Label)) return(lab)
  reply <- lca_chat(cfg)$chat(prompt_harmonize(lab), echo = FALSE)
  arr <- tryCatch(jsonlite::fromJSON(reply, simplifyVector = FALSE),
                  error = function(e) {
                    m <- regmatches(reply, regexpr("(?s)\\[.*\\]", reply, perl = TRUE))
                    if (length(m) == 0) stop("No JSON array in the harmonizer reply:\n", reply)
                    jsonlite::fromJSON(m, simplifyVector = FALSE)
                  })
  new_lab <- map_dfr(arr, ~ tibble(K = as.integer(.x$class), new = as.character(.x$label)))
  lab |>
    left_join(new_lab, by = "K") |>
    mutate(Label = coalesce(new, Label)) |>
    dplyr::select(-new)
}

# The orchestrator the .qmd calls; implements the precedence documented above.
get_class_labels <- function(fit, df, items, cats, cfg,
                             cache = file.path(cfg$out_dir %||% ".",
                                               "class_labels_llm.csv")) {
  K    <- length(fit$pi)
  need <- c("K", "Label", "Description")
  check <- function(lab, src) {
    miss <- setdiff(need, names(lab))
    if (length(miss)) stop(src, " is missing column(s): ", paste(miss, collapse = ", "))
    if (nrow(lab) != K) stop(src, " has ", nrow(lab), " rows but the model has K = ", K, " classes.")
    lab |> arrange(K) |> dplyr::select(all_of(need))
  }
  if (!is.null(cfg$class_labels_csv)) {
    if (is.null(cfg$K_force))
      stop("cfg$class_labels_csv requires cfg$K_force: analyst labels are only ",
           "meaningful for a pinned number of classes.")
    return(check(readr::read_csv(cfg$class_labels_csv, show_col_types = FALSE),
                 cfg$class_labels_csv))
  }
  if (file.exists(cache))
    return(check(readr::read_csv(cache, show_col_types = FALSE), cache))
  lab <- label_classes_llm(fit, df, items, cats, cfg) |>
    mutate(Label_draft = Label) |>
    harmonize_labels(cfg)
  readr::write_csv(lab, cache)   # freeze: Label_draft records any harmonizer edit
  lab |> dplyr::select(all_of(need))
}
