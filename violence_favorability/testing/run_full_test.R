# =============================================================================
# testing/run_full_test.R
# Runs the whole pipeline on SIMULATED data and checks the answers against the
# known truth. Open in RStudio and click "Source". Results are written to
# testing/test_results.txt. Not needed for the real analysis.
# =============================================================================

results_file <- "testing/test_results.txt"
log_line <- function(...) cat(..., "\n", file = results_file, append = TRUE)
if (file.exists(results_file)) invisible(file.remove(results_file))

log_line("Test run:", format(Sys.time()))
log_line("R:", R.version.string)

# ---- 1. Simulate, then run the models ----------------------------------------------
source("testing/simulate_data.R")
truth <- attr(readRDS("data/survey_sim.rds"), "truth")

source("R/models.R")
log_line("Rows: survey", nrow(survey), "| analysis", nrow(ad))
log_line("Communes per wave:", toString(table(dplyr::distinct(ad, wave, commune_id)$wave)),
    "| distinct:", dplyr::n_distinct(ad$commune_id), "| percent favorable:", round(100 * mean(ad$fav), 1))
log_line("Within-commune share of violence variance:", round(stats::var(ad$v_within) / stats::var(ad$v_commune), 3))

# ---- 2. Checks against the truth ---------------------------------------------------------
log_line("TRUE: violence", truth$b_violence, "| commune var", truth$sd_commune^2,
    "| slope SD", truth$sd_slope, "| village var", truth$sd_village^2,
    "| region-wave var", truth$sd_region_wave^2)
log_line("M1: tau00", round(tau00_m1, 3), "| ICC", round(icc_m1, 3))
log_line("M2 slope test: LRT", round(lrt_slope$lrt, 2), "| p", round(lrt_slope$p_value, 3),
    "| M2b singular", m2b_singular, "| keep slope", keep_slope)
log_line("M3 region-wave test: LRT", round(lrt_region_wave$lrt, 2), "| p", signif(lrt_region_wave$p_value, 3),
    "| AIC a/b", round(stats::AIC(m3a), 1), "/", round(stats::AIC(m3b), 1),
    "| M3b singular", m3b_singular, "| keep M3b", keep_region_wave)
log_line("M3 winner: residual commune ICC", round(icc_m3, 3))
log_line("M4: village var", round(tau_village, 3), "| log-weight coef", round(b_weight[1], 3),
    "p", round(b_weight[4], 3))
log_line("Violence:", paste(violence_rows$model, round(violence_rows$estimate, 3),
                       paste0("(", round(violence_rows$std.error, 3), ")"), collapse = "; "))
log_line("Design terms: SE ratio M4/M3", round(design_cost$se_ratio, 2),
    "| coefficient ratio", round(design_cost$coef_ratio, 2))
log_line("M3-MW: within", round(lme4::fixef(m3_mw)[["v_within"]], 3),
    "| between", round(lme4::fixef(m3_mw)[["v_between"]], 3))
log_line("AME M4 (pp):", sprintf("%.1f", 100 * ame$estimate))
log_line("Predicted % favorable (M4) at 0, 1, 5, 20 events:",
    paste(sprintf("%.1f", 100 * predicted$estimate), collapse = ", "))
m4_row <- dplyr::filter(violence_rows, model == "M4 (final)")
log_line("Truth inside M4 95% CI:", abs(m4_row$estimate - truth$b_violence) < 1.96 * m4_row$std.error)

# ---- 3. Diagnostics (performance package) ------------------------------------------------
log_line("performance::icc(m1) adjusted:", round(performance::icc(m1)$ICC_adjusted, 3),
    "(must equal the ICC above)")
log_line("Singular fits: M1", performance::check_singularity(m1), "| M3", performance::check_singularity(m3), "| M4", performance::check_singularity(m4))

# ---- 4. Maps ------------------------------------------------------------------------
source("R/maps.R")
log_line("Maps: survey communes not placed", nrow(not_placed), "| files:",
    paste(list.files("output/maps"), collapse = ", "))

# ---- 5. Package versions -----------------------------------------------------------
pk <- c("lme4", "survey", "marginaleffects", "performance", "sf", "ggplot2", "dplyr", "sampling")
log_line("Packages:", paste(pk, purrr::map_chr(pk, \(p) as.character(utils::packageVersion(p))), collapse = "; "))
log_line("DONE")
message("Finished. Results in ", results_file)
