# =============================================================================
# R/boundaries.R
# Small helpers shared by every script: name cleaning and boundary readers.
# Nothing here downloads anything; it reads the local files in boundaries/.
#
#   boundaries/mali_boundaries.gpkg        map layers adm0, adm1, adm2, adm3
#   boundaries/mali_admin_dictionary.csv   one row per commune with its cercle
#                                          and region (names, codes, match keys)
#   boundaries/SOURCE.md                   where the boundaries came from
#
# Mali levels: adm1 = region, adm2 = cercle, adm3 = commune.
# =============================================================================

bnd_gpkg <- "boundaries/mali_boundaries.gpkg"
bnd_dict <- "boundaries/mali_admin_dictionary.csv"

# Region spellings that differ between sources. Left: a spelling you meet in
# the survey or a boundary file. Right: the spelling used in this project.
# Add rows as you find new variants.
# "Menaka" = "Gao": the OCHA 2021 boundaries list Menaka as its own region
# (split from Gao); the survey's 8 regions predate the split, so Menaka's
# communes are matched under Gao. Map labels still show OCHA's own names.
region_alias <- c(
  "Menaka"             = "Gao",
  "Koulikouro"         = "Koulikoro",
  "Timbuktu"           = "Tombouctou",
  "Bamako District"    = "Bamako",
  "District de Bamako" = "Bamako"
)

# Match key: accents, case, spaces, and punctuation removed.
#   "Ségou" -> "segou"     "Kita Commune" -> "kitacommune"
clean_name <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_remove_all("[^a-z0-9]")
}

# Region match key: apply region_alias first, then clean
region_key <- function(x) {
  alias_by_key = stats::setNames(clean_name(region_alias), clean_name(names(region_alias)))
  k = clean_name(x)
  dplyr::coalesce(unname(alias_by_key[k]), k)
}

# Read one map layer: "adm0", "adm1", "adm2", or "adm3"
read_mali_boundaries <- function(level) {
  if (!file.exists(bnd_gpkg)) {
    stop(bnd_gpkg, " not found. See boundaries/SOURCE.md.")
  }
  sf::st_read(bnd_gpkg, layer = level, quiet = TRUE)
}

# Read the commune dictionary (commune -> cercle -> region)
read_mali_dictionary <- function() {
  readr::read_csv(bnd_dict, show_col_types = FALSE)
}

# One-line boundary credit for map captions (written by make_boundaries.R)
read_boundary_credit <- function() readLines("boundaries/credit.txt", warn = FALSE)
