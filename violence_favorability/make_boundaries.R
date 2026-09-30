# =============================================================================
# make_boundaries.R
# Run ONCE to build the local boundary files. After that, nothing in this
# project needs internet access.
#
# Input: geoBoundaries gbOpen files for Mali, placed in boundaries/raw/:
#   MLI_ADM0.geojson  MLI_ADM1.geojson  MLI_ADM2.geojson  MLI_ADM3.geojson
# Source (CC BY 4.0; boundaries from DNCT / OCHA Mali), downloadable by hand:
#   https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/gbOpen/MLI
#   (use the *_simplified.geojson files; rename as above)
#
# Output:
#   boundaries/mali_boundaries.gpkg       layers adm0, adm1, adm2, adm3
#   boundaries/mali_admin_dictionary.csv  one row per commune
#
# If you already have the .gpkg and .csv, you do not need to run this.
# =============================================================================

pacman::p_load(sf, glue, tidyverse)
source("R/boundaries.R")

raw_dir <- file.path(bnd_dir, "raw")
sf::sf_use_s2(FALSE)  # planar overlap is fine for nesting polygons

read_raw <- function(level) {
  sf::st_read(file.path(raw_dir, glue("MLI_{toupper(level)}.geojson")), quiet = TRUE) |>
    sf::st_make_valid() |>
    dplyr::select(name = shapeName, id = shapeID)
}

adm0 <- read_raw("adm0")
adm1 <- read_raw("adm1")
adm2 <- read_raw("adm2")
adm3 <- read_raw("adm3")

# ---- Nest each level in its parent by largest area overlap -------------------
# The boundary files carry no parent codes, so each unit is assigned to the
# parent polygon it overlaps most. Edges that do not line up exactly between
# levels are harmless under this rule.
nest_in <- function(child, parent, prefix) {
  parent = parent |> dplyr::rename_with(\(x) paste0(prefix, "_", x), c(name, id))
  sf::st_join(child, parent, largest = TRUE)
}

adm2_nested <- adm2 |> nest_in(adm1, "adm1")
adm3_nested <- adm3 |>
  nest_in(adm2, "adm2") |>
  dplyr::left_join(adm2_nested |> sf::st_drop_geometry() |>
                     dplyr::select(adm2_id = id, adm1_name, adm1_id),
                   by = "adm2_id")

# ---- Dictionary: one row per commune -----------------------------------------
dictionary <- adm3_nested |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    adm1_name, adm1_id,
    adm2_name, adm2_id,
    adm3_name = name, adm3_id = id,
    adm1_key = region_key(adm1_name),
    adm2_key = clean_name(adm2_name),
    adm3_key = clean_name(adm3_name)
  ) |>
  dplyr::arrange(adm1_name, adm2_name, adm3_name)

# ---- Checks: the dictionary is only useful if keys are unique -----------------
dup_in_region <- dictionary |> dplyr::count(adm1_key, adm3_key) |> dplyr::filter(n > 1)
dup_in_cercle <- dictionary |> dplyr::count(adm1_key, adm2_key, adm3_key) |> dplyr::filter(n > 1)
dup_cercle    <- dictionary |> dplyr::distinct(adm1_key, adm2_key, adm2_id) |>
  dplyr::count(adm1_key, adm2_key) |> dplyr::filter(n > 1)

glue("Communes: {nrow(dictionary)}; cercles: {dplyr::n_distinct(dictionary$adm2_id)}; ",
     "regions: {dplyr::n_distinct(dictionary$adm1_id)}")
glue("Commune names repeated within a region: {nrow(dup_in_region)} ",
     "(within region + cercle: {nrow(dup_in_cercle)})")
glue("Cercle names repeated within a region: {nrow(dup_cercle)}")
if (nrow(dup_in_region) > 0) print(dup_in_region)

# ---- Write layers with keys for their own and higher levels -----------------
dir.create(bnd_dir, showWarnings = FALSE)
if (file.exists(bnd_gpkg)) file.remove(bnd_gpkg)

layers <- list(
  adm0 = adm0 |> dplyr::rename(adm0_name = name, adm0_id = id),
  adm1 = adm1 |> dplyr::transmute(adm1_name = name, adm1_id = id,
                                  adm1_key = region_key(name)),
  adm2 = adm2_nested |> dplyr::transmute(adm1_name, adm1_id, adm2_name = name, adm2_id = id,
                                         adm1_key = region_key(adm1_name),
                                         adm2_key = clean_name(name)),
  adm3 = adm3_nested |> dplyr::transmute(adm1_name, adm1_id, adm2_name, adm2_id,
                                         adm3_name = name, adm3_id = id,
                                         adm1_key = region_key(adm1_name),
                                         adm2_key = clean_name(adm2_name),
                                         adm3_key = clean_name(name))
)
purrr::iwalk(layers, \(d, nm) sf::st_write(d, bnd_gpkg, layer = nm, quiet = TRUE))
readr::write_csv(dictionary, bnd_dict)

glue("Wrote {bnd_gpkg} (layers: {toString(names(layers))}) and {bnd_dict}")
