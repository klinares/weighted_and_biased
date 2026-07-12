# =============================================================================
# 00_config.R
# Silicon-Sampling Small-Area-Estimation experiment: shared configuration.
#
# Sourced at the top of every pipeline script. Defines:
#   - the on-disk results location,
#   - the structural constants shared across scripts,
#   - save_result() / load_if_missing() helpers that make each script runnable
#     standalone: an object already in the session is used as-is; otherwise it
#     is read from disk; if it is in neither place, a clear error tells you which
#     upstream script to run.
#
# NOTE: scripts source this with a relative path, so run them with the working
# directory set to the folder that contains the scripts and this file.
# =============================================================================

# On-disk location for all persisted objects (.rds). Forward slashes work on
# Windows in R.
results_dir <- "D:/repos/weighted_and_biased/code/MRP/results"

# -----------------------------------------------------------------------------
# Shared structural constants (single source of truth for every script)
# -----------------------------------------------------------------------------
# REAL LAPOP AmericasBarometer Colombia waves (9 irregularly spaced rounds).
# The gaps (2,2,2,2,2,4,3,2 years) are handled by indexing the RW1 temporal
# term on the actual calendar year, not a wave counter.
study_years <- c(2004L, 2006L, 2008L, 2010L, 2012L, 2014L, 2018L, 2021L, 2023L)

# Full annual grid for the silicon EXTRAPOLATION product (model still trains
# only on study_years; in-between years have no survey and are produced, not scored).
all_years   <- 2004:2023

total_pop   <- 38e6                           # approx Colombian voting-age pop

age_levels  <- c("18-25", "26-35", "36-50", "51-65", "66+")          # 5
eth_levels  <- c("Mestizo/White", "Afro-Colombian", "Indigenous",
                 "Other")                                            # 4
urb_levels  <- c("City", "Suburb", "Rural")                          # 3

# Colombian departments + Bogota D.C. (33 geographic units). Names match the
# harmonized ASCII department names produced by read_lapop_colombia.R.
departments <- c(
  "Amazonas", "Antioquia", "Arauca", "Atlantico", "Bogota D.C.",
  "Bolivar", "Boyaca", "Caldas", "Caqueta", "Casanare", "Cauca", "Cesar",
  "Choco", "Cordoba", "Cundinamarca", "Guainia", "Guaviare", "Huila",
  "La Guajira", "Magdalena", "Meta", "Narino", "Norte de Santander",
  "Putumayo", "Quindio", "Risaralda", "San Andres", "Santander", "Sucre",
  "Tolima", "Valle del Cauca", "Vaupes", "Vichada"
)

# -----------------------------------------------------------------------------
# Persistence helpers
# -----------------------------------------------------------------------------

# Save an object to results_dir as <name>.rds (creating the directory if needed).
save_result <- function(obj, name) {
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  saveRDS(obj, file.path(results_dir, paste0(name, ".rds")))
  invisible(obj)
}

# Load <name>.rds into the global environment ONLY if the object is not already
# present in the session. Errors clearly if the object is in neither place.
load_if_missing <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) return(invisible())
  path <- file.path(results_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Required input '", name, "' is neither in the session nor on disk at\n  ",
         path, "\nRun the upstream script that produces it first.", call. = FALSE)
  }
  assign(name, readRDS(path), envir = .GlobalEnv)
  invisible()
}
