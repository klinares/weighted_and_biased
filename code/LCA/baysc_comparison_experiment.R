# baysc_comparison_experiment.R
# ============================================================================
# EXPERIMENT: our design-weighted EM vs baysc's WOLCA, on our simulated data.
#
# PURPOSE
#   External validation of the pipeline's weighted EM against an independent,
#   published implementation that shares none of its code: the Weighted
#   Overfitted Latent Class Analysis (WOLCA) from the baysc package, called
#   directly via baysc::wolca(). Both maximize/sample THE SAME weighted
#   pseudo-likelihood (verified against their source: their per-case kernel
#   log(pi_k) + sum_j log theta is our E-step, and both normalize weights to sum
#   to n), so estimates should agree up to Monte Carlo error, posterior-median-
#   vs-MLE asymmetry in small cells, and the fixed-R padding noted below. A
#   large gap means a bug somewhere; a small one is strong evidence the custom
#   EM is right.
#
#   This script does NOT touch the pipeline. It sources the engine read-only.
#   It requires baysc installed locally (devtools::install_github("smwu/baysc")),
#   so it is a HOME run; the pipeline itself never depends on baysc.
#
# ATTRIBUTION
#   If results from this comparison are reported, cite:
#     Wu, Williams, Savitsky & Stephenson (2026). baysc: An R package for
#       Bayesian survey clustering. J. Open Source Software, 11(119),
#       doi:10.21105/joss.08382.
#     Wu, Williams, Savitsky & Stephenson (2024). Biometrics, 80(4), ujae122,
#       doi:10.1093/biomtc/ujae122.
#
# WHAT IT DOES
#   1. Simulates the pipeline's worked example (known truth, informative design).
#   2. Fits our weighted EM at the true K on the complete cases.
#   3. Runs baysc::wolca() on the same cases: their Gibbs sampler, fixed at the
#      same K (run_sampler = "fixed"), with the design's strata, PSUs, and raw
#      weights (wolca() normalizes the weights to sum to n itself).
#   4. Aligns all class labels to the generative truth, then compares:
#      class prevalences (pi), item-response probabilities (rho/theta),
#      the weighted log-likelihood of BOTH solutions under OUR objective,
#      and both solutions against the known truth.
#
# HONEST LIMITS
#   - baysc pads all items to a common number of categories R = max over items
#     (their code: R <- max(R_j)); categories an item does not have receive only
#     prior mass. Their theta_med is therefore RENORMALIZED here over each
#     item's real categories (equivalent to conditioning on the observed
#     support); the leaked mass is reported so you can see it.
#   - Point estimates only. wolca_var_adjust() is not run: it corrects the
#     UNCERTAINTY of the pseudo-posterior for the design, and no baysc
#     intervals are used in this comparison (our intervals come from JKn).
#   - Priors are baysc's defaults (flat Dirichlets), whose MAP equals the MLE,
#     so the prior cannot explain any gap.
#   - Their post-processing merges classes below class_cutoff and re-sorts
#     labels; alignment to truth below makes that harmless. If it returns fewer
#     than K classes, the script stops and says so.
#
# REQUIREMENTS
#   baysc (installed locally from GitHub), plus matrixStats, clue, and the
#   tidyverse pieces below.
#
# CONVENTIONS
#   Native pipe |>, no R loops (purrr/matrix algebra; the embedded Stan code
#   keeps its own loops, verbatim), namespace-qualified dplyr verbs,
#   tidyverse loaded last. Edit only the bx block.
# ============================================================================

suppressPackageStartupMessages({
  library(matrixStats)
  library(clue)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(stringr)
})

# ----------------------------------------------------------------------------
# CONFIGURATION (the only block you edit)
# ----------------------------------------------------------------------------
bx <- list(
  source_file   = "D://repos/weighted_and_biased/code/LCA/survey_lca_source.R",  # the pipeline engine (read-only)
  seed          = 2026L,   # drives the simulation and our EM starts (via cfg)
  K             = 3L,      # the simulator's true K; both methods fit this K
  n_starts      = 10L,     # random starts for our EM
  n_runs        = 20000L,  # baysc sampler iterations (their default)
  burn          = 10000L,  # baysc burn-in (their default)
  thin          = 5L,      # baysc thinning (their default)
  baysc_seed    = 2026L    # seed for baysc's fixed sampler
)

# The engine reads a global cfg for its per-start seeding; provide the minimal one.
cfg <- list(seed = bx$seed)
source(bx$source_file)

# ============================================================================
# RUN: the whole comparison is ONE function, so running the file executes it in a
# single call. This removes the partial-run trap: every intermediate (dat,
# fit_ours, ours, bay, ...) is LOCAL to this function, so it can never be stale or
# left over from a prior attempt, and any error aborts the entire call at once
# instead of continuing into downstream code that assumes earlier objects exist.
# ============================================================================
run_comparison <- function(bx) {
  
  # ============================================================================
  # 1. SIMULATED DATA WITH KNOWN TRUTH
  # ============================================================================
  sim   <- simulate_survey_data(cfg)
  items <- paste0("q", 1:8)
  
  # Complete cases for BOTH methods: baysc's WOLCA model takes an integer matrix
  # with no NA, and using the same cases keeps the comparison apples-to-apples.
  dat  <- sim$data |> dplyr::filter(complete.cases(pick(all_of(items))))
  w    <- dat$w
  n    <- nrow(dat)
  cats <- map_int(items, ~ max(dat[[.x]], na.rm = TRUE)) |> set_names(items)
  
  # Generative truth, kept in sync with simulate_survey_data() in the source file
  # (same constants; if the simulator's profiles change, change these too).
  prof_by_len <- list(
    `4` = list(open = c(0.08, 0.17, 0.35, 0.40),
               ambivalent = c(0.20, 0.32, 0.30, 0.18),
               closed = c(0.45, 0.30, 0.17, 0.08)),
    `3` = list(open = c(0.15, 0.35, 0.50),
               ambivalent = c(0.33, 0.34, 0.33),
               closed = c(0.50, 0.35, 0.15)),
    `2` = list(open = c(0.30, 0.70),
               ambivalent = c(0.50, 0.50),
               closed = c(0.70, 0.30))
  )
  item_len <- c(4L, 4L, 4L, 3L, 3L, 3L, 2L, 2L) |> set_names(items)
  rho_true <- map(items, function(it) {
    pr <- prof_by_len[[as.character(item_len[[it]])]]
    cbind(open = pr$open, ambivalent = pr$ambivalent, closed = pr$closed)
  }) |> set_names(items)
  pi_true_pop <- sim$pop_pi_true   # weighted population prevalences (open, ambivalent, closed)
  
  # ============================================================================
  # 2. OUR WEIGHTED EM (the pipeline engine, untouched)
  # ============================================================================
  fit_ours <- fit_lca(dat, w, cats, items, bx$K, bx$n_starts)
  
  # ============================================================================
  # 3. baysc's WOLCA, CALLED DIRECTLY
  # ============================================================================
  # Their Gibbs sampler at a fixed K: run_sampler = "fixed" bypasses the adaptive
  # (overfitted) stage entirely. Raw weights go in; wolca() normalizes them to sum
  # to n internally (their kappa step, our scale_ic). Strata and PSUs are passed
  # so the run is faithful to their intended usage, though they do not enter the
  # point estimates (they drive wolca_var_adjust, which is deliberately not run;
  # see HONEST LIMITS).
  if (!requireNamespace("baysc", quietly = TRUE))
    stop("baysc is required for this experiment: devtools::install_github(\"smwu/baysc\")")
  
  res_wolca <- baysc::wolca(
    x_mat       = as.matrix(dat[, items]),
    sampling_wt = as.numeric(w),
    cluster_id  = dat$psu,
    stratum_id  = dat$stratum,
    run_sampler = "fixed",
    K_fixed     = bx$K,
    fixed_seed  = bx$baysc_seed,
    n_runs      = bx$n_runs, burn = bx$burn, thin = bx$thin,
    update      = bx$n_runs %/% 10,
    save_res    = FALSE
  )
  
  # Point estimates: pi_med (length K_red, renormalized medians) and theta_med
  # (J x K_red x R, rows renormalized), after their post-processing, which can
  # merge classes below class_cutoff. The comparison needs all K classes back.
  pi_baysc  <- as.numeric(res_wolca$estimates$pi_med)
  theta_med <- res_wolca$estimates$theta_med
  if (length(pi_baysc) != bx$K)
    stop("baysc post-processing returned ", length(pi_baysc), " classes, not K = ",
         bx$K, ". Its class_cutoff merged a small class; inspect res_wolca before comparing.")
  
  # Rebuild per-item category-by-class matrices from theta_med; record the mass on
  # padded (impossible) categories, then RENORMALIZE over each item's real
  # categories.
  R_pad     <- dim(theta_med)[3]
  theta_raw <- map(seq_along(items), function(jj) t(theta_med[jj, , ])) |>   # R_pad x K
    set_names(items)
  leak <- map_dbl(items, function(it) {
    C <- cats[[it]]
    if (C == R_pad) return(0)
    max(colSums(theta_raw[[it]][(C + 1):R_pad, , drop = FALSE]))
  }) |> set_names(items)
  rho_baysc <- map(items, function(it) {
    m <- theta_raw[[it]][seq_len(cats[[it]]), , drop = FALSE]
    sweep(m, 2, colSums(m), "/")
  }) |> set_names(items)
  
  # ============================================================================
  # 4. ALIGN EVERYTHING TO THE GENERATIVE TRUTH
  # ============================================================================
  # Cost between an estimated class and a true class: total-variation distance
  # between their response profiles, summed over items. clue::solve_LSAP finds the
  # minimum-cost one-to-one matching. Both solutions are mapped into TRUTH order
  # (open, ambivalent, closed), so every table below reads in the same order.
  align_to_truth <- function(rho_est) {
    cost <- map(items, function(it) {
      e <- rho_est[[it]]; t0 <- rho_true[[it]]
      outer(seq_len(bx$K), seq_len(bx$K),
            Vectorize(function(a, b) 0.5 * sum(abs(e[, a] - t0[, b]))))
    }) |> reduce(`+`)
    as.integer(clue::solve_LSAP(cost))          # perm[a] = truth slot of estimated class a
  }
  reorder_fit <- function(pi_est, rho_est, perm) {
    inv <- order(perm)                           # estimated class sitting in each truth slot
    list(pi = pi_est[inv], rho = map(rho_est, ~ .x[, inv, drop = FALSE]))
  }
  ours  <- reorder_fit(fit_ours$pi, set_names(fit_ours$rho, items), align_to_truth(set_names(fit_ours$rho, items)))
  bay   <- reorder_fit(pi_baysc,   rho_baysc,                       align_to_truth(rho_baysc))
  
  # ============================================================================
  # 5. COMPARISONS
  # ============================================================================
  # (a) plug-in weighted log-likelihood under OUR objective, standalone so the
  # pipeline stays untouched: ll(pi, rho) = sum_i w_i log sum_k pi_k prod_j rho.
  loglik_wlca <- function(pi_est, rho_est) {
    logdens <- map(items, function(it) {
      lp <- log(rho_est[[it]])[as.integer(dat[[it]]), , drop = FALSE]
      lp
    }) |> reduce(`+`) +
      matrix(log(pi_est), n, bx$K, byrow = TRUE)
    sum(w * matrixStats::rowLogSumExps(logdens))
  }
  ll_ours  <- loglik_wlca(ours$pi, ours$rho)
  ll_baysc <- loglik_wlca(bay$pi,  bay$rho)
  
  cat("=========================================================\n")
  cat("OUR WEIGHTED EM  vs  baysc WOLCA (baysc::wolca), same data, K =", bx$K, "\n")
  cat("=========================================================\n\n")
  
  cat("Class prevalences (aligned to truth order: open, ambivalent, closed):\n")
  # NOTE: values are pulled out BEFORE tibble(). Inside tibble(), a column named
  # `ours` masks the environment object `ours` for all later arguments (data
  # masking), so referencing ours$pi after defining column `ours` errors with
  # "$ operator is invalid for atomic vectors".
  pi_ours <- ours$pi
  pi_bay  <- bay$pi
  pi_tab <- tibble(class = c("open", "ambivalent", "closed"),
                   truth_pop = round(pi_true_pop, 4),
                   ours      = round(pi_ours, 4),
                   baysc     = round(pi_bay, 4),
                   delta     = round(pi_bay - pi_ours, 4))
  print(pi_tab, n = Inf)
  
  cat("\nItem-response probabilities: agreement between the two solutions.\n")
  rho_delta <- map_dfr(items, function(it) {
    d <- abs(bay$rho[[it]] - ours$rho[[it]])
    tibble(item = it, max_abs_delta = max(d), mean_abs_delta = mean(d))
  })
  print(rho_delta |> mutate(across(where(is.numeric), ~ round(.x, 4))), n = Inf)
  
  cat("\nLargest single-cell disagreements (top 8):\n")
  cells <- map_dfr(items, function(it) {
    d <- bay$rho[[it]] - ours$rho[[it]]
    as_tibble(d, .name_repair = ~ c("open", "ambivalent", "closed")) |>
      mutate(item = it, category = row_number()) |>
      pivot_longer(c(open, ambivalent, closed), names_to = "class", values_to = "delta")
  })
  print(cells |> arrange(desc(abs(delta))) |> slice_head(n = 8) |>
          mutate(delta = round(delta, 4)), n = Inf)
  
  cat("\nMass leaked to padded (impossible) categories in baysc's theta, per item\n")
  cat("(0 for 4-category items; renormalized away before all comparisons above):\n")
  print(tibble(item = items, leaked = round(leak, 4)), n = Inf)
  
  cat("\nWeighted log-likelihood under OUR objective (same surface for both):\n")
  cat(sprintf("  ours (EM maximum): %.2f\n  baysc medians:     %.2f\n  gap:               %.2f\n",
              ll_ours, ll_baysc, ll_ours - ll_baysc))
  cat("The EM value should be the larger (it is the maximizer of this surface);\n")
  cat("a small gap says both implementations found the same optimum.\n")
  
  cat("\nBoth against the generative truth (mean abs error in rho, by method):\n")
  truth_err <- map_dfr(items, function(it) {
    tibble(item = it,
           ours  = mean(abs(ours$rho[[it]] - rho_true[[it]])),
           baysc = mean(abs(bay$rho[[it]]  - rho_true[[it]])))
  })
  print(truth_err |> mutate(across(where(is.numeric), ~ round(.x, 4))), n = Inf)
  cat(sprintf("\npi mean abs error vs population truth: ours %.4f | baysc %.4f\n",
              mean(abs(ours$pi - pi_true_pop)), mean(abs(bay$pi - pi_true_pop))))
  
  invisible(list(pi = pi_tab, rho_delta = rho_delta, cells = cells, leak = leak,
                 ll = c(ours = ll_ours, baysc = ll_baysc), truth_err = truth_err,
                 ours = ours, baysc = bay))
}

# ============================================================================
# ENTRY POINT: sourcing or running the whole file runs the comparison once.
# ============================================================================
results <- run_comparison(bx)