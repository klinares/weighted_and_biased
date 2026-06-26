# survey_lca_source.R
# ============================================================================
# Reusable source code for design-weighted latent class analysis.
# Sourced by survey_lca_analysis.qmd; keep it in the same folder when rendering.
#
# Contents (four sections):
#   1. Plotting and data-preparation helpers
#   2. The weighted EM engine and label alignment
#   3. The Bolck-Croon-Hagenaars (BCH) three-step correction
#   4. The worked-example data simulator
#
# These functions take their inputs as arguments and hold no analysis-specific
# state, with one documented exception kept from the original script: fit_lca()
# seeds its random starts from a global `cfg$seed`, so a `cfg` object carrying a
# `seed` element must exist when fit_lca() is called. The .qmd defines it before
# any call. simulate_survey_data() likewise reads `cfg$seed` from its argument.
#
# Dependencies (loaded by the .qmd): tidyverse, matrixStats, clue. Base R and
# stats supply the rest. There are no for/while loops anywhere; iteration is
# done with purrr and matrix algebra.
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
# 3. THE BCH (BOLCK-CROON-HAGENAARS) THREE-STEP CORRECTION
# ============================================================================
# Regresses latent class membership on external covariates without the
# attenuation that arises when a modal class assignment is treated as the true
# class. References: Bolck, Croon & Hagenaars (2004) Political Analysis
# 12(1):3-27; Vermunt (2010) Political Analysis 18(4):450-469; Bakk, Tekle &
# Vermunt (2013) Sociological Methodology 43(1):272-311.

# Classification (misclassification) matrix D, K x K with
# D[t, s] = P(assigned class = s | true class = t); rows sum to 1.
#   post  : n x K posterior class probabilities, columns in class order 1..K.
#   modal : length-n integer vector of assigned (modal) classes, values 1..K.
#   w     : length-n weights (survey weights, or replicate weights).
# A near-diagonal D means clean classification; off-diagonal mass is the error
# that attenuates a modal-class regression.
classification_matrix <- function(post, modal, w) {
  K <- ncol(post)
  modal_ind <- outer(modal, seq_len(K), `==`) + 0      # n x K, 1 where assigned = s
  # t(post * w) is K x n with row t equal to w_i * post[i, t] (w recycles down
  # each column because length(w) == nrow(post)); the matrix product then sums
  # the weighted (true t, assigned s) counts over respondents.
  N_ts <- t(post * w) %*% modal_ind                    # K x K: rows true, cols assigned
  N_ts / rowSums(N_ts)                                 # normalise each true-class row to 1
}

# Weighted multinomial logistic regression with BCH soft labels.
#   X   : n x p numeric design matrix (include the intercept column).
#   R   : n x K soft-label matrix; row i is row modal_i of solve(D), so each row
#         sums to 1 but entries may be NEGATIVE (this is the BCH correction).
#   w   : length-n numeric case weights (the survey or replicate weights).
#   ref : integer index (1..K) of the reference class (its coefficients are 0).
# Returns a p x (K-1) coefficient matrix: rows are the columns of X, columns are
# the non-reference classes (named by their integer label).
#
# Why negative soft labels do not destabilise the fit. The objective is the
# weighted log-likelihood
#   l(B) = sum_i w_i * sum_t R[i, t] * log P(class = t | X_i).
# Its curvature (the Hessian) depends only on the POSITIVE weights w_i and the
# fitted probabilities, not on the signed labels R, so l is concave with a
# single maximum; a quasi-Newton solver finds the unique BCH solution. The
# gradient for a non-reference class t is
#   sum_i w_i * X_i * (R[i, t] - P(class = t | X_i)),
# which the solver below supplies analytically.
weighted_multinom <- function(X, R, w, ref = 1L, maxit = 500L) {
  K <- ncol(R); p <- ncol(X)
  nonref <- setdiff(seq_len(K), ref)             # the K-1 classes that carry coefficients
  unpack <- function(par) matrix(par, nrow = p)  # p x (K-1); column j -> class nonref[j]
  # Linear predictors, n x K, with the reference column held at 0.
  linpred <- function(B) { eta <- matrix(0, nrow(X), K); eta[, nonref] <- X %*% B; eta }
  # Row-wise softmax, shifted by each row's maximum for numerical stability.
  probs <- function(eta) { e <- exp(eta - matrixStats::rowMaxs(eta)); e / rowSums(e) }
  negll <- function(par) {                       # negative weighted log-likelihood
    P <- probs(linpred(unpack(par)))
    -sum(w * rowSums(R * log(pmax(P, 1e-12))))   # floor probabilities off 0 before the log
  }
  neggr <- function(par) {                       # gradient of negll, length p*(K-1)
    P <- probs(linpred(unpack(par)))
    -as.vector(crossprod(X, w * (R[, nonref, drop = FALSE] - P[, nonref, drop = FALSE])))
  }
  opt <- optim(rep(0, p * (K - 1)), negll, neggr, method = "BFGS",
               control = list(maxit = maxit, reltol = 1e-11))
  B <- unpack(opt$par)
  dimnames(B) <- list(colnames(X), as.character(nonref))
  B
}

# End-to-end BCH fit from a data frame.
#   data      : data frame with the covariates, posteriors, and modal class.
#   aux       : character vector of covariate (predictor) column names.
#   post_cols : the K posterior column names, in class order 1..K.
#   modal     : name of the modal-class column (integer, or a factor whose
#               levels are the integer class labels).
#   w         : length-n weights (survey weights or, inside a replicate, the
#               replicate weights). Drives BOTH D and the regression.
#   ref       : integer reference class.
# The design matrix uses treatment contrasts (each factor's first level as its
# reference), matching nnet::multinom, so BCH and naive modal coefficients are
# comparable term by term. na.action = na.pass keeps rows aligned with the
# posteriors; missing covariates are caught explicitly rather than dropping rows.
bch_fit <- function(data, aux, post_cols, modal, w, ref = 1L) {
  mf <- model.frame(reformulate(aux), data, na.action = na.pass)
  X  <- model.matrix(attr(mf, "terms"), mf)                  # n x p, aligned with data
  if (anyNA(X))
    stop("Missing values in the BCH covariates (", paste(aux, collapse = ", "),
         "). BCH needs complete covariates; drop or impute them before profiling.")
  post      <- as.matrix(data[, post_cols, drop = FALSE])    # n x K posteriors, class order
  modal_int <- as.integer(as.character(data[[modal]]))       # recover integer class labels
  D <- classification_matrix(post, modal_int, w)
  R <- solve(D)[modal_int, , drop = FALSE]                   # soft labels: rows of D^{-1}
  weighted_multinom(X, R, w, ref = ref)
}

# Flatten a coefficient matrix to one named vector with names "<class>::<term>",
# laid out one class at a time, matching the naive multinom_theta() layout so
# survey::withReplicates can build a covariance matrix and the two coefficient
# sets join term by term.
bch_flatten <- function(B) {
  flat <- as.vector(B)                                       # column-major: class by class
  names(flat) <- as.vector(outer(rownames(B), colnames(B),
                                 function(tm, cl) paste(cl, tm, sep = "::")))
  flat
}


# ============================================================================
# 4. THE WORKED-EXAMPLE DATA SIMULATOR
# ============================================================================
# Generate the multistage worked example: 9 strata, ~20 PSUs in total, ~1,200
# respondents, stratum-varying base weights, and an INFORMATIVE design (the more
# heavily weighted strata are the ones least open to negotiation). Eight items
# measure openness on mixed scales (four-, three-, and two-point) to exercise the
# variable-length machinery, and a small fraction of responses (under 5% per
# item) are "Don't know", set to NA, to exercise the missing-data handling.
# Returns the data frame and the population class prevalences; the latter is used
# only by the truth comparison, which runs when cfg$simulated is TRUE. The whole
# routine is seeded from cfg$seed and avoids loops.
simulate_survey_data <- function(cfg) {
  # ---- generative truth -----------------------------------------------------
  set.seed(cfg$seed)                  # makes the simulated example itself reproducible
  K_true <- 3L
  J      <- 8L

  # Class-conditional response profiles at several item lengths, so the example has
  # items of DIFFERENT numbers of categories. In every length the "open" class leans
  # toward the high end and the "closed" class toward the low end.
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
  item_len <- c(4L, 4L, 4L, 3L, 3L, 3L, 2L, 2L)        # q1..q8: three lengths
  names(item_len) <- paste0("q", seq_len(J))

  # ---- multistage design ----------------------------------------------------
  # 9 strata; PSUs per stratum chosen to total ~20; 60 respondents per PSU.
  psu_per_stratum <- c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L)  # sums to 20 PSUs
  resp_per_psu    <- 60L
  H <- length(psu_per_stratum)

  # Stratum-level openness mix (alpha) and base weight, made to covary => informative.
  grade   <- seq(0, 1, length.out = H)
  alpha_h <- cbind(open       = 0.20 + 0.45 * grade,
                   ambivalent = 0.30,
                   closed     = 0.50 - 0.45 * grade)
  alpha_h <- alpha_h / rowSums(alpha_h)
  w_stratum <- round(seq(2.2, 0.6, length.out = H), 2)   # open strata sampled more (lower weight)

  # Build the respondent frame without loops.
  frame <- tibble(stratum = rep(seq_len(H), times = psu_per_stratum)) |>
    mutate(psu_in_stratum = sequence(psu_per_stratum)) |>
    mutate(psu = row_number()) |>                                   # unique PSU id
    tidyr::uncount(resp_per_psu, .id = "within") |>
    mutate(id = row_number(),
           w  = w_stratum[stratum])

  n <- nrow(frame)

  # ---- draw latent class for each respondent (vectorized inverse-CDF) --------
  draw_rows <- function(P) {
    cum <- matrixStats::rowCumsums(P)
    u   <- runif(nrow(P))
    pmin(rowSums(cum < u) + 1L, ncol(P))
  }
  true_class <- draw_rows(alpha_h[frame$stratum, , drop = FALSE])

  # ---- draw item responses given class, then set "Don't know" to NA ----------
  draw_one_item <- function(j) {
    set.seed(cfg$seed + j)
    pr <- prof_by_len[[as.character(item_len[j])]]
    by_class <- list(pr$open, pr$ambivalent, pr$closed)
    P <- do.call(rbind, by_class[true_class])       # n x L response probs per respondent
    resp <- draw_rows(P)
    # Don't-know overlay: under 5% per item, set to NA, mildly higher among the ambivalent.
    dk_p <- 0.03 + 0.02 * (true_class == 2)
    resp[rbinom(length(resp), 1, dk_p) == 1] <- NA_integer_
    resp
  }
  item_mat <- map(seq_len(J), draw_one_item) |> set_names(paste0("q", seq_len(J)))

  sim <- frame |>
    bind_cols(as_tibble(item_mat)) |>
    mutate(true_class = true_class,
           age_grp   = factor(sample(c("18-34","35-54","55+"), n, TRUE)),
           education = factor(sample(c("HS or less","Some college","BA+"), n, TRUE)),
           # region carries a mild signal so a profiling covariate is non-null
           region    = factor(if_else(runif(n) < plogis(-0.3 + 0.6*(true_class==1)),
                                      "Coastal", "Interior")))

  # Population truth for later comparison.
  pop_pi_true <- as.numeric(prop.table(xtabs(w ~ true_class, data = sim)))

  list(data = sim, pop_pi_true = pop_pi_true, K_true = K_true)
}
