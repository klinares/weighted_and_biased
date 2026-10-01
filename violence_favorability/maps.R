# =============================================================================
# maps.R
# Builds the data for the three maps. Run after models.R (needs m3's u_hat).
# Communes are placed on the map with the same name key as the data
# (key_cols in R/prep_data.R).
# =============================================================================

pacman::p_load(sf, tidyverse)

# ---- 1. Boundaries -------------------------------------------------------------
adm1 <- read_mali_boundaries("adm1")
adm3 <- read_mali_boundaries("adm3") |>
  dplyr::mutate(region = adm1_name, cercle = adm2_name, commune = adm3_name)
adm3$commune_key <- make_key(adm3)

# Names that are not unique under key_cols cannot be placed on a single polygon,
# so they are left out (they show as gray). Adding "cercle" to key_cols fixes this.
adm3 <- adm3 |> dplyr::filter(!commune_key %in% ambiguous_keys)

# ---- 2. Violence: every commune on the map, 0 where the file has no record ------
map_violence <- adm3 |>
  dplyr::select(commune_key) |>
  tidyr::expand_grid(wave = levels(ad$wave)) |>
  sf::st_as_sf() |>
  dplyr::left_join(events_cw |> dplyr::mutate(wave = as.character(wave)),
                   by = c("commune_key", "wave")) |>
  dplyr::mutate(value = log1p(tidyr::replace_na(events, 0)))

# ---- 3. Model 4 view: design-weighted percent favorable per commune and wave ----
fav_commune_wave <- dat |>
  dplyr::filter(!is.na(fav), !is.na(wt)) |>
  dplyr::summarise(value = stats::weighted.mean(fav, wt),
                   n     = dplyr::n(),
                   .by   = c(commune_key, wave)) |>
  dplyr::mutate(wave = as.character(wave))

map_fav <- dplyr::inner_join(adm3, fav_commune_wave, by = "commune_key")

# ---- 4. Model 3 view: commune random intercepts ---------------------------------
map_u <- dplyr::inner_join(adm3, u_hat |> dplyr::rename(value = u), by = "commune_key")

# ---- 5. One drawing function for all three maps -----------------------------------
# Gray communes underneath, coloured values on top, region outlines in black.
draw_map <- function(d) {
  ggplot(d) +
    geom_sf(data = adm3, fill = "gray92", color = NA, inherit.aes = FALSE) +
    geom_sf(aes(fill = value), color = NA) +
    geom_sf(data = adm1, fill = NA, color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    theme_void(base_size = 9)
}
