# =============================================================================
# silicon_cost_probe.R
# Estimate cost/time of the silicon generation under the FLAT-n_h design
# (no pre-LLM allocation): every denied/sparse surveyed cell gets n_h candidate
# respondents per draw, m draws. The LLM-call driver is cells x m_draws.
#
# DEPENDS ON: postrat_coverage from build_postrat_frame.R.
# COST/RUN NOTE: this probe itself makes a handful of paid calls.
#
# CODING STANDARDS: set.seed(721); no for loops (purrr); tidyverse last.
# =============================================================================

library(ellmer)
library(tidyverse)

set.seed(721)
source("00_config.R")
source("silicon_prompt_functions.R")

load_if_missing("postrat_coverage")

# -----------------------------------------------------------------------------
# Parameters (match 02; confirm OpenRouter values)
# -----------------------------------------------------------------------------
or_model        <- "google/gemma-4-31b-it"
n_h             <- 5L
m_draws         <- 10L
temperature     <- 0.8
n_probe_calls   <- 12L
n_workers_plan  <- 8L
target_tiers    <- c("denied", "sparse")
keys            <- c("year", "department", "age_group", "ethnicity", "urbanicity")

# OpenRouter price per 1,000,000 tokens (USD) -- fill from the model's page.
price_in_per_m  <- 0.05
price_out_per_m <- 0.10

# =============================================================================
# 1. TARGET CELLS (denied/sparse, surveyed years; flat n_h -- no SE allocation)
# =============================================================================
silicon_targets <- postrat_coverage %>%
  filter(year %in% study_years, tier %in% target_tiers) %>%
  select(all_of(keys), tier, N_cell)

n_target_cells <- nrow(silicon_targets)
total_calls    <- n_target_cells * m_draws

cat("--- Target cells (denied/sparse, surveyed years; flat n_h) ---\n")
cat("Cells: ", n_target_cells, "\n",
    "n_h per cell per draw: ", n_h, "\n",
    "Draws (m): ", m_draws, "\n",
    "LLM calls (cells x m): ", total_calls, "\n",
    "Candidate respondents (cells x n_h x m): ", n_target_cells * n_h * m_draws,
    "\n", sep = "")

cat("\n--- Target cells by tier ---\n")
silicon_targets %>% count(tier) %>% print()

cat("\n--- Target cells by year ---\n")
silicon_targets %>% count(year) %>% print(n = Inf)

# =============================================================================
# 2. REAL CALLS ON REPRESENTATIVE PROMPTS (n_h-respondent prompt)
# =============================================================================
probe_cells <- silicon_targets %>% slice_sample(n = min(n_probe_calls, n_target_cells))

one_probe <- function(year, department, age_group, ethnicity, urbanicity, ...) {
  prompt <- build_silicon_prompt(
    department, age_group, ethnicity, urbanicity, year,
    mrp_rate = 0.35, mrp_lo = 0.22, mrp_hi = 0.49,
    marg_dept = 0.33, marg_age = 0.34, marg_eth = 0.30, marg_urb = 0.38,
    is_survey_year = TRUE, n_h = n_h
  )
  ch <- ellmer::chat_openrouter(
    model = or_model,
    params = ellmer::params(temperature = temperature),
    system_prompt = "You return only a JSON array of integers."
  )
  t0  <- Sys.time()
  out <- tryCatch(ch$chat(prompt, echo = FALSE), error = function(e) NA_character_)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  toks <- tryCatch(ch$get_tokens(), error = function(e) NULL)
  in_tok  <- if (!is.null(toks) && "tokens" %in% names(toks))
    sum(toks$tokens[toks$role == "user"],      na.rm = TRUE) else NA_real_
  out_tok <- if (!is.null(toks) && "tokens" %in% names(toks))
    sum(toks$tokens[toks$role == "assistant"], na.rm = TRUE) else NA_real_

  resp <- parse_silicon_responses(out, n_h)
  tibble(prompt_chars = nchar(prompt),
         input_tokens = in_tok, output_tokens = out_tok,
         input_est = nchar(prompt) / 4, elapsed_s = elapsed,
         parsed_n = if (is.null(resp)) 0L else length(resp))
}

cat("\n--- Running ", nrow(probe_cells), " real probe calls to ", or_model,
    " ---\n", sep = "")
probe <- pmap(probe_cells, one_probe) %>% list_rbind()
print(probe)

# =============================================================================
# 3. PER-CALL AVERAGES + FULL-RUN PROJECTION
# =============================================================================
mean_in  <- mean(probe$input_tokens,  na.rm = TRUE)
mean_out <- mean(probe$output_tokens, na.rm = TRUE)
mean_lat <- mean(probe$elapsed_s,     na.rm = TRUE)
if (is.nan(mean_in))  mean_in  <- mean(probe$input_est, na.rm = TRUE)
if (is.nan(mean_out)) mean_out <- 30
parse_ok <- mean(probe$parsed_n >= 1)

cost_total <- total_calls * (mean_in / 1e6 * price_in_per_m +
                             mean_out / 1e6 * price_out_per_m)
time_serial_hr   <- total_calls * mean_lat / 3600
time_parallel_hr <- time_serial_hr / n_workers_plan

cat("\n=== PER-CALL AVERAGES ===\n")
cat("Input tokens/call : ", round(mean_in), "\n",
    "Output tokens/call: ", round(mean_out), "\n",
    "Latency/call (s)  : ", round(mean_lat, 2), "\n",
    "Parse success     : ", round(parse_ok * 100), "%\n", sep = "")

cat("\n=== FULL-RUN PROJECTION (", total_calls, " calls) ===\n", sep = "")
cat("Est. TOTAL cost : $", round(cost_total, 2),
    "  (at $", price_in_per_m, "/$", price_out_per_m, " per 1M in/out)\n",
    "Est. time serial: ", round(time_serial_hr, 1), " h\n",
    "Est. time @", n_workers_plan, " workers: ",
    round(time_parallel_hr, 1), " h\n", sep = "")

# =============================================================================
# 4. PERSIST
# =============================================================================
save_result(silicon_targets, "silicon_targets")
save_result(probe,           "silicon_cost_probe")
cat("\nSaved silicon_targets, silicon_cost_probe to", results_dir, "\n")

# =============================================================================
# READS:
#   * total_calls = cells x m_draws is the cost/time driver. Dropping the SE
#     allocation means ALL denied/sparse cells qualify (~17k), so this is much
#     larger than the prior 4,264-cell run.
#   * Levers if the wall-clock is too high: lower m_draws (set by the m-probe),
#     restrict target_tiers to "denied" only, or raise n_workers.
# =============================================================================
