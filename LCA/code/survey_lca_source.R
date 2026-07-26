# survey_lca_source.R
# Engine for design-weighted latent class analysis. Holds no analysis-specific
# state: everything arrives as an argument. Loaded before survey_data_config.R.
#
#   1. Plotting and tables
#   2. Weighted EM, alignment, and diagnostics
#   3. Design, replicate variance, and BCH
#   4. Prediction
#   5. LLM segment labeling
#
# Iteration is purrr and matrix algebra throughout; there are no loops.
# Data cleaning lives in survey_data_config.R, not here.

`%||%` <- function(x, y) if (is.null(x)) y else x


# =============================================================================
# 1. PLOTTING AND TABLES
# =============================================================================

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

# Wrap long lines before printing. Verbatim output does not wrap on its own and
# the overflow is clipped; fixing that in the preamble would be LaTeX-only, so it
# is done here instead and holds for LaTeX, Typst, HTML, and docx alike. Existing
# newlines and leading indentation are preserved, so structured text keeps its
# shape and only overlong lines are broken.
wrap_text <- function(x, width = 88L) {
  strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE)[[1]] |>
    map_chr(function(line) {
      if (nchar(line) <= width) return(line)
      pad <- str_extract(line, "^[ ]*")
      strwrap(str_squish(line), width = width,
              prefix = paste0(pad, "  "), initial = pad) |>
        paste(collapse = "\n")
    }) |>
    paste(collapse = "\n")
}

# Every table goes through here, so pagination and styling are set in one place.
# Under LaTeX it emits a longtable so the table can break across pages, and widths
# (a character vector, one entry per column, "" to leave a column alone) converts
# those columns to wrapping p{} types; total should stay under about 45em for a
# 1in-margin page. Captions are escaped there because knitr does not escape them:
# an unescaped $ or _ aborts the render and an unescaped % comments out the rest
# of the line.
# Under any other format, including Typst, it emits a markdown pipe table, which
# Quarto renders natively and paginates on its own. widths is ignored in that path
# because column sizing is the renderer's job there, so it is a LaTeX hint rather
# than a requirement and nothing breaks if it is absent.
lca_table <- function(df, ..., caption = NULL, widths = NULL, font_size = 8) {
  if (!knitr::is_latex_output())
    return(knitr::kable(df, format = "pipe", caption = caption, ...))
  if (!is.null(caption))
    caption <- str_replace_all(caption, "([#$%&_{}])", "\\\\\\1")
  out <- knitr::kable(df, format = "latex", longtable = TRUE, booktabs = TRUE,
                      linesep = "", caption = caption, ...) |>
    kableExtra::kable_styling(latex_options = c("repeat_header", "hold_position"),
                              font_size = font_size)
  if (is.null(widths)) return(out)
  reduce(seq_along(widths), function(tbl, i) {
    if (nzchar(widths[i])) kableExtra::column_spec(tbl, i, width = widths[i]) else tbl
  }, .init = out)
}

init_parallel <- function(cfg) {
  if (isTRUE(cfg$parallel)) {
    future::plan(future::multisession,
                 workers = cfg$workers %||% max(1L, future::availableCores() - 1L))
  } else {
    future::plan(future::sequential)
  }
  invisible(NULL)
}

plot_item_stack <- function(df, items, title, show_missing = TRUE) {
  long <- df |>
    select(all_of(items)) |>
    mutate(across(everything(), as.numeric)) |>
    pivot_longer(everything(), names_to = "item", values_to = "value")
  if (!show_missing) long <- filter(long, !is.na(value))
  lev <- as.character(sort(unique(long$value[!is.na(long$value)])))
  long |>
    mutate(value = factor(if_else(is.na(value), "Missing", as.character(value)),
                          levels = c(lev, if (show_missing) "Missing"))) |>
    count(item, value) |>
    ggplot(aes(item, n, fill = value)) +
    geom_col(position = "fill") +
    scale_fill_manual(name = "Response",
                      values = c(set_names(viridisLite::viridis(length(lev)), lev),
                                 Missing = "grey75")) +
    labs(x = NULL, y = "Proportion", title = title) +
    theme_lca() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# =============================================================================
# 2. WEIGHTED EM, ALIGNMENT, AND DIAGNOSTICS
# =============================================================================

rand_init <- function(cats, K) {
  list(pi = {x <- runif(K); x / sum(x)},
       rho = map(cats, function(Cj) {
         m <- matrix(runif(Cj * K) + 0.1, Cj, K)
         sweep(m, 2, colSums(m), "/")
       }))
}

# One EM run as a fold. Y is a list of integer item vectors, OH a list of one-hot
# category matrices. A missing answer contributes 0 on the log scale, so it drops
# out of the within-segment product.
em_run <- function(Y, OH, cats, w, K, init = NULL, maxit = 800L, tol = 1e-8) {
  nn <- length(Y[[1]])
  st0 <- c(init %||% rand_init(cats, K),
           list(post = NULL, ll = -Inf, iter = 0L, done = FALSE))

  step <- function(state, .iter) {
    if (isTRUE(state$done)) return(state)
    log_terms <- map2(state$rho, Y, function(rho_j, y) {
      lp <- log(rho_j)[y, , drop = FALSE]
      lp[is.na(lp)] <- 0
      lp
    })
    logdens <- reduce(log_terms, `+`) + matrix(log(state$pi), nn, K, byrow = TRUE)
    lse <- rowLogSumExps(logdens)
    post <- exp(logdens - lse)
    ll <- sum(w * lse)
    wp <- w * post
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

make_inputs <- function(df, items, cats) {
  Y <- map(items, function(it) as.integer(df[[it]]))
  OH <- map2(items, cats, function(it, Cj) {
    oh <- outer(as.integer(df[[it]]), seq_len(Cj), `==`) + 0
    oh[is.na(oh)] <- 0
    oh
  })
  list(Y = Y, OH = OH)
}

# E-step under fixed parameters.
posterior_of <- function(pi, rho, Y) {
  nn <- length(Y[[1]])
  K <- length(pi)
  log_terms <- map2(rho, Y, function(rho_j, y) {
    lp <- log(rho_j)[y, , drop = FALSE]
    lp[is.na(lp)] <- 0
    lp
  })
  logdens <- reduce(log_terms, `+`) + matrix(log(pi), nn, K, byrow = TRUE)
  exp(logdens - rowLogSumExps(logdens))
}

# Segment labels are arbitrary. Match any fit to a reference by response profile
# so segments are comparable across starts, fits, and replicates.
profiles_of <- function(rho) do.call(cbind, map(rho, t))

align_to <- function(fit, ref) {
  K <- length(fit$pi)
  Pf <- profiles_of(fit$rho)
  Pr <- profiles_of(ref$rho)
  cost <- outer(seq_len(K), seq_len(K),
                Vectorize(function(a, b) sum((Pf[a, ] - Pr[b, ])^2)))
  inv <- integer(K)
  inv[as.integer(solve_LSAP(cost))] <- seq_len(K)
  list(pi = fit$pi[inv],
       rho = map(fit$rho, function(m) m[, inv, drop = FALSE]),
       post = if (!is.null(fit$post)) fit$post[, inv, drop = FALSE] else NULL,
       ll = fit$ll,
       converged = fit$converged %||% NA)
}

# Seeds are passed as data rather than drawn inside the worker, so sequential and
# parallel plans return identical results.
start_seeds <- function(cfg, K) as.integer(cfg$seed + 1000L * K + seq_len(cfg$n_starts))

fit_lca <- function(df, w, cats, items, K, seeds, ref = NULL,
                    maxit = 800L, tol = 1e-8) {
  inp <- make_inputs(df, items, cats)
  cands <- map(seeds, function(s) {
    set.seed(s)
    em_run(inp$Y, inp$OH, cats, w, K, maxit = maxit, tol = tol)
  })
  best <- cands[[which.max(map_dbl(cands, "ll"))]]
  if (is.null(ref)) best else align_to(best, ref)
}

df_k <- function(K, cats) (K - 1) + K * sum(cats - 1)

# Relative entropy on the weighted scale, so it describes the population model
# rather than the achieved sample.
entropy_R2 <- function(post, w, K) {
  if (K == 1) return(NA_real_)
  1 + sum(w * rowSums(post * log(pmax(post, 1e-12)))) / (sum(w) * log(K))
}

# Item discrimination: mean over segment pairs of the total variation distance
# between their response distributions. Bounded in [0, 1].
item_discrimination <- function(fit, items) {
  pairs <- combn(length(fit$pi), 2, simplify = FALSE)
  tibble(item = items,
         discrimination = map_dbl(fit$rho, function(rho_j) {
           mean(map_dbl(pairs,
                        function(p) 0.5 * sum(abs(rho_j[, p[1]] - rho_j[, p[2]]))))
         })) |>
    arrange(desc(discrimination))
}

# Bivariate residual: total variation distance between the weighted observed
# two-way table and the model-implied one. Bounded in [0, 1], zero under exact
# local independence. No reference distribution applies under a design-weighted
# pseudo-likelihood, so this ranks rather than tests.
bvr_pairs <- function(df, w, items, fit) {
  W <- sum(w)
  pr <- t(combn(seq_along(items), 2L))
  map(seq_len(nrow(pr)), function(i) {
    a <- pr[i, 1]
    b <- pr[i, 2]
    obs <- as.matrix(xtabs(w ~ factor(df[[items[a]]], seq_len(nrow(fit$rho[[a]]))) +
                             factor(df[[items[b]]], seq_len(nrow(fit$rho[[b]]))))) / W
    exp_p <- fit$rho[[a]] %*% (fit$pi * t(fit$rho[[b]]))
    tibble(item_a = items[a], item_b = items[b],
           bvr = 0.5 * sum(abs(obs - exp_p)))
  }) |>
    list_rbind() |>
    arrange(desc(bvr))
}


# =============================================================================
# 3. DESIGN, REPLICATE VARIANCE, AND BCH
# =============================================================================

# Stratified jackknife design. Singleton strata are a hard stop: they cannot take
# the n_h / (n_h - 1) replicate scaling, and survey.lonely.psu governs
# linearization rather than replicate construction, so continuing would
# understate variance in the strata with least information.
build_rep_design <- function(dat, cfg) {
  lonely <- dat |>
    distinct(.data[[cfg$strata]], .data[[cfg$psu]]) |>
    count(.data[[cfg$strata]], name = "n_psu") |>
    filter(n_psu < 2)

  if (nrow(lonely) > 0) {
    print(lonely)
    stop(nrow(lonely), " stratum/strata contain a single PSU in the analysis ",
         "frame. Collapse them in survey_data_config.R before continuing.")
  }

  des <- svydesign(ids = reformulate(cfg$psu), strata = reformulate(cfg$strata),
                   weights = reformulate(cfg$weight), data = dat, nest = TRUE)
  list(des = des, rep_des = as.svrepdesign(des, type = "JKn"))
}

# Same estimator survey::withReplicates uses,
# V = scale * sum_r rscale_r (theta_r - theta_hat)(theta_r - theta_hat)',
# but the expensive part (one refit per replicate) is mapped rather than looped.
replicate_variance <- function(rep_des, theta_fun, theta_hat) {
  Wm <- weights(rep_des, type = "analysis")
  Theta <- do.call(rbind, future_map(seq_len(ncol(Wm)),
                                     function(r) theta_fun(Wm[, r]),
                                     .options = furrr_options(seed = NULL)))
  d <- sweep(Theta, 2, theta_hat, "-")
  rep_des$scale * crossprod(d * sqrt(rep_des$rscales))
}

# BCH: replace each hard assignment with row W_i of the inverse of the
# design-weighted classification error matrix D[k, s] = P(W = s | X = k).
# Entries can be negative; rows sum to one because D's rows do.
bch_weights <- function(post, modal, w) {
  K <- ncol(post)
  num <- crossprod(w * post, outer(modal, seq_len(K), `==`) + 0)
  D <- sweep(num, 1, rowSums(num), "/")
  solve(D)[modal, , drop = FALSE]
}


# =============================================================================
# 4. PREDICTION
# =============================================================================

# Posterior segment membership for any respondents carrying the item columns.
# Items arrive already recoded by survey_data_config.R, so the fitted and the
# predicted frames are on the same coding by construction.
predict_segments <- function(df, fit, items, min_items) {
  K <- length(fit$pi)
  Y <- map(items, function(it) as.integer(df[[it]]))
  post <- posterior_of(fit$pi, fit$rho, Y)
  answered <- reduce(Y, function(a, y) a + as.integer(!is.na(y)),
                     .init = integer(nrow(df)))

  seg <- max.col(post, ties.method = "first")
  seg[answered < min_items] <- NA_integer_

  bind_cols(
    tibble(segment = seg,
           max_posterior = if_else(is.na(seg), NA_real_, rowMaxs(post)),
           n_items_answered = answered),
    as_tibble(post) |> set_names(paste0("post_segment", seq_len(K))))
}


# =============================================================================
# 5. LLM SEGMENT LABELING
# =============================================================================
# One call per segment. A joint prompt confuses near-neighbor segments, because a
# forced one-to-one assignment lets one confusion corrupt two labels. Labels are
# drafts for the analyst to verify against the response profiles; they never feed
# back into estimation. The JSON keys stay label/description/class for stability.

lca_persona <- function() {
  paste(
    "You are a senior survey methodologist who reads latent class analysis",
    "(LCA) measurement models. In this work each latent class is called a",
    "SEGMENT; that is a word-choice preference and the statistical object is",
    "unchanged. Each segment is described only by its item-response",
    "probabilities: for every survey item, the probability that a member of",
    "that segment gives each answer. A segment leans toward the answers with",
    "high probability. You interpret a segment strictly from these",
    "probabilities and the item wording, never from outside assumptions.")
}

lca_rules <- function() {
  paste("RULES:",
        "1. Use only the response probabilities and item wording shown. Survey",
        "   context only clarifies what the items refer to; attribute nothing to",
        "   the segment that the probabilities do not show.",
        "2. Anchor every statement to the high-probability answers of this segment.",
        "3. If the profile is diffuse (no clear high-probability answers), say so.",
        "4. Return only valid JSON: no prose before or after, no markdown fences.",
        sep = "\n")
}

# dictionary supplies the question wording and the response labels, in the same
# order as the fitted category indices.
format_segment_block <- function(fit, k, dictionary, items) {
  lines <- map_chr(seq_along(items), function(j) {
    d <- filter(dictionary, item == items[j])
    probs <- paste(sprintf("P(%s)=%.2f", d$responses[[1]], fit$rho[[j]][, k]),
                   collapse = ", ")
    str_glue('  {items[j]} "{d$question}"\n      {probs}')
  })
  str_glue("SEGMENT {k} (estimated prevalence {round(100 * fit$pi[k])}%):\n",
           paste(lines, collapse = "\n"))
}

prompt_segment_label <- function(fit, k, dictionary, items, context = NULL) {
  ctx <- if (!is.null(context) && nzchar(context))
    str_glue("SURVEY CONTEXT\n{context}\n\n") else ""
  str_glue(
    "{ctx}",
    "ONE SEGMENT FROM A LATENT CLASS ANALYSIS (LCA) MEASUREMENT MODEL\n",
    "{format_segment_block(fit, k, dictionary, items)}\n\n",
    "TASK\n",
    "Read this single segment and return: a short DRAFT label (2 to 5 words) ",
    "for an analyst to refine, and a one or two sentence factual description ",
    "anchored to its high-probability answers.\n\n",
    "{lca_rules()}\n",
    'JSON (one object): {{"label": "...", "description": "..."}}')
}

# OpenRouter at home, an OpenAI-compatible endpoint at work. Keys are read from
# .Renviron by ellmer.
lca_chat <- function(cfg) {
  p <- ellmer::params(temperature = 0, seed = cfg$seed)
  if (is.null(cfg$compass_base_url)) {
    ellmer::chat_openrouter(model = cfg$llm_model,
                            system_prompt = lca_persona(), params = p)
  } else {
    if (!nzchar(Sys.getenv("OPENAI_API_KEY")))
      Sys.setenv(OPENAI_API_KEY = Sys.getenv("COMPASS_API_KEY"))
    ellmer::chat_openai(base_url = cfg$compass_base_url, model = cfg$llm_model,
                        system_prompt = lca_persona(), params = p)
  }
}

# Some models wrap valid JSON despite rule 4, so pull the object out by pattern.
parse_json_block <- function(txt, pattern = "(?s)\\{.*\\}") {
  m <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(m) == 0) stop("No JSON found in the model reply:\n", txt)
  jsonlite::fromJSON(m, simplifyVector = FALSE)
}

label_segments_llm <- function(fit, dictionary, items, cfg) {
  map(seq_along(fit$pi), function(k) {
    obj <- parse_json_block(
      lca_chat(cfg)$chat(prompt_segment_label(fit, k, dictionary, items,
                                              cfg$survey_context), echo = FALSE))
    tibble(K = k,
           Label = pluck(obj, "label", .default = NA_character_),
           Description = pluck(obj, "description", .default = NA_character_))
  }) |>
    list_rbind()
}

# Per-segment isolation has one blind spot: two neighbors can draft the same
# label, since neither call saw the other. One closing call edits only the labels
# that collide, and runs only when this mechanical check fires.
labels_collide <- function(labels) {
  ws <- map(str_squish(tolower(labels)), function(s) unique(strsplit(s, " ")[[1]]))
  pr <- t(combn(length(labels), 2L))
  any(map_dbl(seq_len(nrow(pr)), function(i) {
    a <- ws[[pr[i, 1]]]
    b <- ws[[pr[i, 2]]]
    length(intersect(a, b)) / length(union(a, b))
  }) >= 0.5)
}

prompt_harmonize <- function(lab) {
  rows <- str_glue_data(lab, "SEGMENT {K}: LABEL \"{Label}\" | DESCRIPTION: {Description}")
  str_glue(
    "DRAFT LABELS FOR THE SEGMENTS OF ONE LATENT CLASS ANALYSIS (LCA) MODEL\n",
    "{paste(rows, collapse = '\n')}\n\n",
    "TASK\n",
    "Some labels are too similar to tell apart. Edit ONLY the labels that ",
    "overlap, as little as possible, so every label is distinct; anchor each ",
    "edit to that segment's own description. Keep every non-overlapping label ",
    "verbatim. Do not change any description. Labels stay 2 to 5 words.\n\n",
    "{lca_rules()}\n",
    'JSON (one array, all segments): [{{"class": 1, "label": "..."}}, ...]')
}

harmonize_labels <- function(lab, cfg) {
  if (!labels_collide(lab$Label)) return(lab)
  arr <- parse_json_block(lca_chat(cfg)$chat(prompt_harmonize(lab), echo = FALSE),
                          "(?s)\\[.*\\]")
  new_lab <- map(arr, function(x) tibble(K = as.integer(x$class),
                                         new = as.character(x$label))) |>
    list_rbind()
  lab |>
    left_join(new_lab, by = "K") |>
    mutate(Label = coalesce(new, Label)) |>
    select(-new)
}

# out_dir/segment_labels.csv is used when it exists, otherwise the model drafts
# once and writes it. Editing that file is taking over the naming.
get_segment_labels <- function(fit, dictionary, items, cfg,
                               cache = file.path(cfg$out_dir, "segment_labels.csv")) {
  need <- c("K", "Label", "Description")

  if (file.exists(cache)) {
    lab <- read_csv(cache, show_col_types = FALSE)
    if (!all(need %in% names(lab)) || nrow(lab) != length(fit$pi))
      stop(cache, " does not match this model (needs ", length(fit$pi),
           " rows and columns K, Label, Description). Delete or fix it.")
    return(lab |> arrange(K) |> select(all_of(need)))
  }

  lab <- label_segments_llm(fit, dictionary, items, cfg) |>
    mutate(Label_draft = Label) |>
    harmonize_labels(cfg)
  write_csv(lab, cache)
  select(lab, all_of(need))
}
