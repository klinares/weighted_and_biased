# =============================================================================
# R/boundaries.R
# Helpers for Mali administrative boundaries. Sourced by make_boundaries.R,
# simulate_data.R and analysis.qmd. No internet access needed: everything reads
# the local files written by make_boundaries.R.
#
#   boundaries/mali_boundaries.gpkg        layers adm0, adm1, adm2, adm3
#   boundaries/mali_admin_dictionary.csv   one row per commune with its cercle
#                                          and region (names, IDs, match keys)
#
# Source: geoBoundaries gbOpen, Mali ADM1-ADM3 (simplified), CC BY 4.0.
#   https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/gbOpen/MLI
#   Runfola et al. (2020) geoBoundaries: A global database of political
#   administrative boundaries. PLoS ONE 15(4): e0231866.
#   doi:10.1371/journal.pone.0231866
# The commune -> cercle -> region links in the dictionary are NOT an official
# code list: make_boundaries.R assigns each unit to the parent polygon it
# overlaps most.
#
# Packages used for maps: sf (read the .gpkg), ggplot2 (geom_sf), patchwork.
#
# Mali levels in these files (geoBoundaries gbOpen, source DNCT / DNP / OCHA):
#   adm1 = region (8 regions + Bamako district)   9 units
#   adm2 = cercle                                 50 units
#   adm3 = commune (urban or rural)               701 units
# =============================================================================

bnd_dir  <- "boundaries"
bnd_gpkg <- file.path(bnd_dir, "mali_boundaries.gpkg")
bnd_dict <- file.path(bnd_dir, "mali_admin_dictionary.csv")

# Region spellings that differ between sources. Left side: any spelling you
# meet (survey, ACLED, boundary file); right side: the name used in this
# project. Add rows as you find new variants.
region_alias <- c(
  "Koulikouro" = "Koulikoro",
  "Timbuktu"   = "Tombouctou",
  "Bamako District" = "Bamako",
  "District de Bamako" = "Bamako"
)

# Match key: accents, case, spaces and punctuation removed.
# "Ségou" -> "segou", "Kita Commune" -> "kitacommune"
clean_name <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_remove_all("[^a-z0-9]")
}

# Standardize a region name: apply the alias table, then the match key
region_key <- function(x) {
  alias_keys = stats::setNames(clean_name(region_alias), clean_name(names(region_alias)))
  k = clean_name(x)
  dplyr::coalesce(unname(alias_keys[k]), k)
}

# Read one level as an sf object. level: "adm0", "adm1", "adm2", "adm3".
# Every layer carries name, id and key columns for its own and higher levels.
read_mali_boundaries <- function(level = c("adm1", "adm2", "adm3", "adm0"),
                                 path = bnd_gpkg) {
  level = match.arg(level)
  if (!file.exists(path)) {
    stop(glue::glue("{path} not found. Run make_boundaries.R once, or copy the ",
                    "boundaries/ folder next to this script."))
  }
  sf::st_read(path, layer = level, quiet = TRUE)
}

# The commune -> cercle -> region dictionary
read_mali_dictionary <- function(path = bnd_dict) {
  readr::read_csv(path, show_col_types = FALSE)
}
