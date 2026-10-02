# =============================================================================
# testing/run_full_test.R
# Runs the whole pipeline on SIMULATED data and checks the answers against the
# known truth. Open in RStudio and click "Source". Results are written to
# testing/test_results.txt. Not needed for the real analysis.
# =============================================================================

results_file <- "testing/test_results.txt"
log <- function(...) cat(..., "\n", file = results_file, append = TRUE)
if (file.exists(results_file)) invisible(file.remove(results_file))

log("Test run:", format(Sys.time()))
log("R:", R.version.string)

# ---- 1. Simulate, match, stand-in review -------------------------------------------
source("testing/simulate_data.R")
source("match_communes.R")
source("testing/simulate_review.R")

draft <- readr::read_csv("matching/crosswalk_draft.csv", show_col_types = FALSE)
truth_codes <- readr::read_csv("testing/true_commune_codes.csv", show_col_types = FALSE)
graded <- dplyr::inner_join(draft, truth_codes, by = c("region", "commune"))
exact_rows <- dplyr::filter(graded, status == "exact")
log("Matching: exact", nrow(exact_rows), "| exact but wrong", sum(exact_rows$adm3_code != exact_rows$true_adm3_code),
    "| needing review", sum(draft$status != "exact"))

# ---- 2. Diagnostics and models --------------------------------------------------------
source("models.R")
log("Rows: survey", nrow(survey), "| dat", nrow(dat), "| analysis", nrow(ad), "(survey and dat must match)")
log("Conflicting event counts:", nrow(events_conflict), "(must be 0)")
log("Diagnostics: A_region", round(A_region, 3), "| A_commune", round(A_commune, 3),
    "| P", P_median, "| W", round(W_share, 3))

coef_row <- function(m) summary(m)$coefficients["v_commune", 1:2]
log("TRUE violence coefficient: -0.30")
log("M1 region-wave coef:", round(summary(m1)$coefficients["v_region", 1], 3))
log("M2 coef, SE:", round(coef_row(m2), 3))
log("M3 coef:", round(lme4::fixef(m3)["v_commune"], 3), "| singular:", m3_singular)
log("M4 coef, SE:", round(coef_row(m4), 3))
log("AMEs (pp):", paste(results$model, sprintf("%.1f", 100 * results$estimate), collapse = "; "))
log("Predicted % favorable at 0,1,5,20 events:", paste(sprintf("%.1f", 100 * predicted$estimate), collapse = ", "))
log("Leave-one-region-out AME range (pp):", paste(sprintf("%.1f", 100 * range(loo$estimate)), collapse = " to "))

# ---- 3. Maps ------------------------------------------------------------------------
source("maps.R")
log("Maps: survey communes not placed", nrow(not_mapped), "| files:",
    paste(list.files("output/maps"), collapse = ", "))

# ---- 4. Package versions -----------------------------------------------------------
pk <- c("survey", "lme4", "marginaleffects", "sf", "ggplot2", "dplyr", "knitr", "sampling")
log("Packages:", paste(pk, purrr::map_chr(pk, \(p) as.character(utils::packageVersion(p))), collapse = "; "))
log("DONE")
message("Finished. Results in ", results_file)
