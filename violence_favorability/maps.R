# =============================================================================
# maps.R
# Builds and saves the maps. Run after models.R (Model 3 map needs u_hat).
# Communes are placed using ONLY the reviewed crosswalk (matching/crosswalk.csv).
#
# Each map separates three states that must never look alike:
#   Not surveyed      no data for that commune and wave (gray)
#   Surveyed, zero    surveyed and no events (palest color)
#   Too few people    surveyed, but fewer than `min_n` respondents (white)
# =============================================================================

pacman::p_load(sf, viridis, tidyverse)

min_n    <- 5                       # hide favorability based on fewer respondents
out_dir  <- "output/maps"           # PDF (vector) and PNG (300 dpi) copies
credit   <- read_boundary_credit()

# ---- 1. Boundaries and the reviewed crosswalk ---------------------------------------
if (!"adm3_code" %in% names(dat)) {
  stop("matching/crosswalk.csv not found. Run match_communes.R and review the draft first.")
}
sf::sf_use_s2(FALSE)   # quiet planar label placement
adm1 <- read_mali_boundaries("adm1")
adm3 <- read_mali_boundaries("adm3")

# Region labels placed inside each region
# Label positions computed once, as plain x/y columns.
# (suppressWarnings: sf notes that label points use flat geometry, which is fine here)
region_labels <- adm1 |>
  sf::st_point_on_surface() |>
  suppressWarnings() |>
  dplyr::mutate(label = adm1_name)   # OCHA's own region names
region_labels <- region_labels |>
  sf::st_drop_geometry() |>
  dplyr::bind_cols(tibble::as_tibble(sf::st_coordinates(region_labels)))

# Survey communes that cannot be placed (missing or blank in the crosswalk)
not_mapped <- dat |>
  dplyr::filter(is.na(adm3_code)) |>
  dplyr::distinct(region, commune)

# ---- 2. One value per commune and wave ------------------------------------------------
waves <- levels(dat$wave)

# R/prep_data.R already attached adm3_code from the crosswalk
commune_wave <- dat |>
  dplyr::filter(!is.na(fav), !is.na(weight), !is.na(adm3_code)) |>
  dplyr::summarise(
    events = dplyr::first(events),                      # same for everyone in the commune-wave
    fav    = stats::weighted.mean(fav, weight),         # design-weighted percent favorable
    n      = dplyr::n(),
    .by = c(adm3_code, wave)
  ) |>
  dplyr::mutate(wave = as.character(wave))

# Every commune polygon x every wave, so unsurveyed communes stay on the map
map_data <- adm3 |>
  dplyr::select(adm3_code) |>
  tidyr::expand_grid(wave = waves) |>
  sf::st_as_sf() |>
  dplyr::left_join(commune_wave, by = c("adm3_code", "wave")) |>
  dplyr::mutate(
    wave_label = paste("Wave", wave),
    violence_class = dplyr::case_when(
      is.na(events) ~ "Not surveyed",
      events == 0   ~ "0",
      events <= 2   ~ "1-2",
      events <= 9   ~ "3-9",
      events <= 29  ~ "10-29",
      TRUE          ~ "30 or more"),
    fav_class = dplyr::case_when(
      is.na(fav)  ~ "Not surveyed",
      n < min_n   ~ glue::glue("Fewer than {min_n} respondents"),
      fav < 0.40  ~ "Under 40%",
      fav < 0.50  ~ "40-50%",
      fav < 0.60  ~ "50-60%",
      fav < 0.70  ~ "60-70%",
      TRUE        ~ "70% or more")
  )

# ---- 3. Colors (fixed order so every wave uses the same legend) ------------------------
violence_colors <- c("0" = "#fee5d9", "1-2" = "#fcae91", "3-9" = "#fb6a4a",
                     "10-29" = "#de2d26", "30 or more" = "#a50f15",
                     "Not surveyed" = "#d9d9d9")

fav_levels <- c("Under 40%", "40-50%", "50-60%", "60-70%", "70% or more")
fav_colors <- c(stats::setNames(viridis::viridis(5, end = 0.9), fav_levels),
                stats::setNames("#ffffff", glue::glue("Fewer than {min_n} respondents")),
                "Not surveyed" = "#d9d9d9")

# ---- 4. One drawing function --------------------------------------------------------------
draw_map <- function(d, fill_var, colors, legend_title, title) {
  ggplot(d) +
    # show.legend = TRUE keeps every class in the legend, even classes with no
    # communes (needed in ggplot2 3.5 and later)
    geom_sf(aes(fill = factor(.data[[fill_var]], levels = names(colors))),
            color = "white", linewidth = 0.05, show.legend = TRUE) +
    geom_sf(data = adm1, fill = NA, color = "gray25", linewidth = 0.35, inherit.aes = FALSE) +
    # no region labels on the small three-wave panels: they would overlap
    scale_fill_manual(values = colors, drop = FALSE, name = legend_title) +
    labs(title = title,
         caption = glue::glue("{credit} Survey communes not placed on the map: {nrow(not_mapped)}.")) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.caption = element_text(size = 7, color = "gray30", hjust = 0),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}

map_violence <- draw_map(map_data, "violence_class", violence_colors,
                         "Events of violence against civilians (ACLED)",
                         "Violence against civilians by commune and survey wave") +
  facet_wrap(~ wave_label) +
  guides(fill = guide_legend(nrow = 1, title.position = "top"))

map_favorability <- draw_map(map_data, "fav_class", fav_colors,
                             "Weighted percent with a favorable view of the leader",
                             "Leader favorability by commune and survey wave") +
  facet_wrap(~ wave_label) +
  guides(fill = guide_legend(nrow = 2, title.position = "top",
                             override.aes = list(color = "gray60")))   # outline so white shows

# ---- 5. Model 3 view: commune random intercepts ----------------------------------------------
# u_hat is keyed by commune_id, which is the official code when the crosswalk exists.
map_u <- adm3 |>
  dplyr::select(adm3_code) |>
  dplyr::left_join(u_hat, by = c("adm3_code" = "commune_id"))

map_intercepts <- ggplot(map_u) +
  geom_sf(aes(fill = u), color = "white", linewidth = 0.05) +
  geom_sf(data = adm1, fill = NA, color = "gray25", linewidth = 0.35, inherit.aes = FALSE) +
  geom_text(data = region_labels, aes(X, Y, label = label), size = 2.3, color = "gray15",
            check_overlap = TRUE, inherit.aes = FALSE) +
  scale_fill_gradient2(low = "#440154", mid = "white", high = "#21918c", midpoint = 0,
                       na.value = "#d9d9d9", name = "Commune effect\n(log-odds)") +
  labs(title = "Communes more or less favorable than Model 3 predicts",
       caption = glue::glue("Gray: not surveyed. {credit}")) +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.caption = element_text(size = 7, color = "gray30", hjust = 0))

# ---- 6. Save publication copies ------------------------------------------------------------
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_map <- function(plot, name, width, height) {
  ggsave(file.path(out_dir, paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = width, height = height, dpi = 300,
         bg = "white")
}
save_map(map_violence,     "violence_by_commune_wave",     9, 4.2)
save_map(map_favorability, "favorability_by_commune_wave", 9, 4.4)
save_map(map_intercepts,   "model3_commune_effects",       6.5, 5)
