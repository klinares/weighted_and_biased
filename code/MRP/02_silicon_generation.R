# =============================================================================
# 02_silicon_generation.R
# MRP-anchored TRUE silicon sampling, SURVEYED YEARS ONLY.
#
# DESIGN (per the agreed redesign):
#   * No pre-LLM sampling allocation. Every denied/sparse surveyed cell gets a
#     FLAT n_h candidate respondents per draw. All design work (inclusion
#     probabilities from population structure + GREG calibration) happens AFTER,
#     in the evaluation -- so no method is repeated before and after the LLM.
#   * The LLM SIMULATES n_h respondents per call (binary), not a rate.
#   * m DRAWS per cell at nonzero temperature; the pool stays STRATIFIED BY DRAW
#     so the between-draw spread is the imputation variance (Rubin downstream).
#
# WHY MRP HERE: only to build the per-cell anchor + marginal CONTEXT for the
#   prompt (prediction from the fitted model, not a re-fit).
#
# CONCURRENCY: parallel over (cell x draw) tasks via furrr (OpenRouter remote
#   API, not local Ollama). Per-(cell,draw) caching makes the run resumable.
#   Put OPENROUTER_API_KEY in .Renviron so workers inherit it.
#
# CODING STANDARDS: set.seed(721); no for loops (retry via recursion); tidyverse
#   loaded last; nothing written outside results_dir.
# =============================================================================

library(brms)
library(ellmer)
library(furrr)
library(future)
library(tidyverse)

set.seed(721)
source("00_config.R")
source("silicon_prompt_functions.R")

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
n_h         <- 5L                             # flat candidate respondents/cell/draw
m_draws     <- 10L                            # draws per cell (confirm via m-probe)
temperature <- 0.8                            # nonzero so draws genuinely vary
or_model    <- "google/gemma-4-31b-it"        # confirm exact OpenRouter slug
max_retries <- 3L
n_workers   <- 8L
target_tiers <- c("denied", "sparse")         # cells silicon augments
keys        <- c("year", "department", "age_group", "ethnicity", "urbanicity")

fits_dir  <- file.path(results_dir, "fits")
cache_dir <- file.path(results_dir, "silicon_cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Inputs
# -----------------------------------------------------------------------------
load_if_missing("postrat_coverage")
load_if_missing("postrat_frame")

mrp_path <- file.path(fits_dir, "mrp_full.rds")
if (!file.exists(mrp_path))
  stop("Fitted MRP not found at ", mrp_path, " -- run 01_benchmark_mrp.R first.")
fit_full <- readRDS(mrp_path)

std_factors <- function(df) {
  df %>%
    mutate(age_group  = factor(age_group,  levels = age_levels),
           ethnicity  = factor(ethnicity,  levels = eth_levels),
           urbanicity = factor(urbanicity, levels = urb_levels),
           department = factor(department, levels = departments))
}
postrat_frame <- std_factors(postrat_frame)

# Target = denied/sparse cells in surveyed years (flat n_h each; no allocation).
targets <- postrat_coverage %>%
  filter(year %in% study_years, tier %in% target_tiers) %>%
  select(all_of(keys))

cat("Surveyed-year target cells: ", nrow(targets),
    " | n_h: ", n_h, " | draws: ", m_draws,
    " | total calls: ", nrow(targets) * m_draws, "\n", sep = "")

# SMOKE TEST: a few departments x 2 recent years -- enough to see whether the
# posterior-propagation fix moves coverage, in well under an hour. Set FALSE for
# the full run. Names must match dept_lookup exactly (note "Bogota D.C.").
smoke <- TRUE
if (smoke) {
  targets <- targets %>%
    filter(year %in% c(2018L, 2023L),
           department %in% c("Bogota D.C.", "Antioquia", "Valle del Cauca",
                             "Atlantico", "Narino"))
  cat("SMOKE TEST active -> cells: ", nrow(targets),
      " | total calls: ", nrow(targets) * m_draws, "\n", sep = "")
}

# =============================================================================
# 1. MRP CELL ANCHORS (PER-DRAW POSTERIOR SAMPLES) + MARGINALS
# =============================================================================
# Each of the m draws is anchored on a DIFFERENT posterior sample of the cell
# rate, so MRP posterior uncertainty -- wide in sparse/denied cells -- flows
# into the between-draw spread (the imputation-variance term Rubin combines in
# 03). The prompt interval [mrp_lo, mrp_hi] stays the FULL posterior band
# (one per cell); only the point anchor mrp_rate varies by draw. This is the
# missing third uncertainty component: previously every draw used the posterior
# MEAN, so the anchor's own uncertainty never entered and intervals collapsed.
marg_point <- function(nd, ep, var) {
  nd %>%
    mutate(.row = row_number()) %>%
    group_by(level = .data[[var]]) %>%
    summarise(rows = list(.row), w = list(N_cell), .groups = "drop") %>%
    mutate(est = map2_dbl(rows, w, function(r, wt)
      mean(as.vector(ep[, r, drop = FALSE] %*% wt) / sum(wt)))) %>%
    select(level, est)
}

# m posterior-sample indices, evenly thinned across the chain (deterministic;
# the same indices are reused across years so draw d is a coherent posterior
# draw everywhere).
sel_draws <- unique(round(seq(1, brms::ndraws(fit_full), length.out = m_draws)))

mrp_context <- map(study_years, function(yr) {
  nd <- postrat_frame %>% filter(year == yr)
  ep <- posterior_epred(fit_full, newdata = nd, allow_new_levels = TRUE,
                        sample_new_levels = "gaussian")

  # Full-posterior interval + posterior-mean marginals (one row per cell).
  cell_ctx <- nd %>%
    mutate(mrp_lo = apply(ep, 2, quantile, 0.05),
           mrp_hi = apply(ep, 2, quantile, 0.95)) %>%
    select(all_of(keys), mrp_lo, mrp_hi)
  md <- marg_point(nd, ep, "department") %>% rename(department = level, marg_dept = est)
  ma <- marg_point(nd, ep, "age_group")  %>% rename(age_group  = level, marg_age  = est)
  me <- marg_point(nd, ep, "ethnicity")  %>% rename(ethnicity  = level, marg_eth  = est)
  mu <- marg_point(nd, ep, "urbanicity") %>% rename(urbanicity = level, marg_urb  = est)

  # Per-draw cell anchor: posterior sample sel_draws[d] for every cell.
  ep_sel <- ep[sel_draws, , drop = FALSE]            # m x n_cells
  map(seq_along(sel_draws), function(d) {
    nd %>%
      select(all_of(keys)) %>%
      mutate(draw = d, mrp_rate = ep_sel[d, ])
  }) %>%
    list_rbind() %>%
    left_join(cell_ctx, by = keys) %>%
    left_join(md, by = "department") %>%
    left_join(ma, by = "age_group") %>%
    left_join(me, by = "ethnicity") %>%
    left_join(mu, by = "urbanicity") %>%
    select(all_of(keys), draw, mrp_rate, mrp_lo, mrp_hi,
           marg_dept, marg_age, marg_eth, marg_urb)
}) %>%
  list_rbind()

save_result(mrp_context, "mrp_context")

# Context for target cells -- already one row per (cell, draw) because the
# per-draw anchors live in mrp_context (join on keys expands cells to draws).
target_ctx <- targets %>%
  mutate(across(c(department, age_group, ethnicity, urbanicity), as.character)) %>%
  left_join(
    mrp_context %>%
      mutate(across(c(department, age_group, ethnicity, urbanicity), as.character)),
    by = keys
  )

# =============================================================================
# 2. GENERATION HARNESS (one (cell,draw) task -> n_h respondents)
# =============================================================================
generate_draw <- function(year, department, age_group, ethnicity, urbanicity,
                          mrp_rate, mrp_lo, mrp_hi,
                          marg_dept, marg_age, marg_eth, marg_urb, draw) {

  cellid <- str_replace_all(
    str_c("pp", year, department, age_group, ethnicity, urbanicity, "d", draw,
          sep = "_"),
    "[^A-Za-z0-9]", "")
  cache_path <- file.path(cache_dir, str_c(cellid, ".rds"))
  if (file.exists(cache_path)) return(readRDS(cache_path))

  prompt <- build_silicon_prompt(
    department, age_group, ethnicity, urbanicity, year,
    mrp_rate, mrp_lo, mrp_hi, marg_dept, marg_age, marg_eth, marg_urb,
    is_survey_year = TRUE, n_h = n_h
  )

  attempt <- function(tries_left) {
    ch  <- ellmer::chat_openrouter(
      model = or_model,
      params = ellmer::params(temperature = temperature),
      system_prompt = "You return only a JSON array of integers."
    )
    out  <- tryCatch(ch$chat(prompt, echo = FALSE), error = function(e) NA_character_)
    resp <- parse_silicon_responses(out, n_h)
    if (!is.null(resp) || tries_left <= 1) return(resp)
    attempt(tries_left - 1)
  }
  resp   <- attempt(max_retries)
  status <- if (is.null(resp)) "fallback_anchor" else "ok"

  # Fallback: if no parse, simulate n_h from the MRP anchor (no adjustment).
  if (is.null(resp)) resp <- rbinom(n_h, 1, mrp_rate)
  # If the LLM returned fewer than n_h, top up from the returned share.
  if (length(resp) < n_h)
    resp <- c(resp, rbinom(n_h - length(resp), 1, mean(resp)))

  tibble(year = year, department = department, age_group = age_group,
         ethnicity = ethnicity, urbanicity = urbanicity, draw = draw,
         response = resp, mrp_rate = mrp_rate, status = status)
}

# =============================================================================
# 3. RUN (parallel over cell x draw; resumable). Each task returns n_h rows.
# =============================================================================
plan(multisession, workers = n_workers)
silicon_respondents <- future_pmap(
  target_ctx, generate_draw,
  .options  = furrr_options(seed = TRUE, packages = c("ellmer", "tidyverse")),
  .progress = TRUE
) %>%
  list_rbind() %>%
  mutate(is_silicon = TRUE)
plan(sequential)

save_result(silicon_respondents, "silicon_respondents")

# =============================================================================
# 4. DIAGNOSTIC: per-cell-draw rate, between-draw spread, movement off anchor
# =============================================================================
cat("\n--- Status counts (respondent rows) ---\n")
silicon_respondents %>% count(status) %>% print()

draw_rate <- silicon_respondents %>%
  group_by(across(all_of(keys)), draw) %>%
  summarise(rate = mean(response), anchor = first(mrp_rate), .groups = "drop")

cell_summary <- draw_rate %>%
  group_by(across(all_of(keys))) %>%
  summarise(mean_rate   = mean(rate),
            sd_rate     = sd(rate),       # between-draw spread of the silicon rate
            anchor_mean = mean(anchor),   # ~ MRP posterior mean for the cell
            sd_anchor   = sd(anchor),     # MRP posterior spread injected across draws
            .groups = "drop")

cat("\n--- Movement off anchor + imputation spread (now incl. MRP posterior) ---\n")
cell_summary %>%
  summarise(
    mean_abs_adj          = round(mean(abs(mean_rate - anchor_mean)), 3),
    mean_between_draw_sd  = round(mean(sd_rate), 3),     # total imputation spread
    mean_anchor_sd        = round(mean(sd_anchor), 3),   # MRP posterior contribution
    pct_cells_moved_gt_05 = round(mean(abs(mean_rate - anchor_mean) > 0.05) * 100, 1)
  ) %>%
  print()

save_result(cell_summary, "silicon_cell_summary")

cat("\n--- Silicon respondents ---\n")
cat("rows: ", nrow(silicon_respondents),
    " | cells: ", nrow(cell_summary),
    " | n_h: ", n_h, " | draws: ", m_draws, "\n", sep = "")

cat("\nSaved mrp_context, silicon_respondents, silicon_cell_summary to ",
    results_dir, "\n", sep = "")

# =============================================================================
# OBJECTS PRODUCED:
#   mrp_context          - per target cell: MRP anchor (+interval) + marginals
#   silicon_respondents  - one row per simulated respondent: cell, draw, response
#                          (n_h x m_draws per cell), is_silicon = TRUE
#   silicon_cell_summary - per cell: mean rate, between-draw SD, MRP anchor
#
# NEXT (evaluation -- design-based / TSL, NOT another MRP):
#   * inclusion probability per silicon respondent from POPULATION STRUCTURE
#     only (cell N_cell vs target contribution), outcome-blind, then GREG-
#     calibrate to census margins (design-independent),
#   * estimate department x year WITHIN each draw with Taylor-linearized
#     variance (survey pkg) on real + silicon,
#   * combine the m draws by Rubin's rules -> point + total variance + interval,
#   * headline: department-by-year interval calibration (target ~0.90 vs the
#     MRP benchmark's 0.507) and RMSE (vs 0.128); LOWO anchoring TBD.
# =============================================================================
