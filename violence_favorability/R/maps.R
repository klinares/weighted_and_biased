# R/maps.R
# Three maps by wave, saved to output/maps/ as PDF and PNG:
# violence events by commune, and predicted favorability from M3 by commune and by cercle.
# Communes join the OCHA boundaries by region + cercle + commune key, which is
# unique even for names that repeat within a region. Gray means "not surveyed".

source("R/boundaries.R")
if (!exists("m3")) source("R/models.R")
sf::sf_use_s2(FALSE)
adm1 <- read_mali_boundaries("adm1")
adm2 <- read_mali_boundaries("adm2")
adm3 <- read_mali_boundaries("adm3")

# Favorability classes, shared by the commune and cercle maps
fav_classes <- function(x) {
  cut(x, c(-Inf, 0.70, 0.75, 0.80, 0.85, 0.90, Inf), right = FALSE,
      labels = c("Under 70%", "70-75%", "75-80%", "80-85%", "85-90%", "90% or more"))
}

# One value per commune and wave. Predictions include the commune's and cercle's own
# effects, averaged over its respondents with the survey weights; with 8 respondents
# per commune they are pulled toward the cercle and overall level (shrinkage).
commune_values <- ad |>
  dplyr::mutate(pred = stats::predict(m3, type = "response"), adm2_key = clean_name(cercle)) |>
  dplyr::summarise(pred_fav = stats::weighted.mean(pred, wt),
                   events = dplyr::first(.data[[cfg$violence]]),
                   .by = c(adm1_key, adm2_key, adm3_key, wave))

keys <- c("adm1_key", "adm2_key", "adm3_key")
not_placed <- commune_values |>
  dplyr::distinct(adm1_key, adm2_key, adm3_key) |>
  dplyr::anti_join(sf::st_drop_geometry(adm3), by = keys)
caption <- paste0(read_boundary_credit(), " Survey communes not placed on the map: ", nrow(not_placed), ".")

# Every polygon x every wave, so unsurveyed communes still appear
map_data <- adm3 |>
  dplyr::select(dplyr::all_of(keys)) |>
  tidyr::expand_grid(wave = levels(ad$wave)) |>
  sf::st_as_sf() |>
  dplyr::left_join(dplyr::mutate(commune_values, wave = as.character(wave)), by = c(keys, "wave")) |>
  dplyr::mutate(
    wave = paste("Wave", wave),
    violence_class = cut(events, c(-Inf, 0, 2, 9, 29, Inf), labels = c("0", "1-2", "3-9", "10-29", "30 or more")),
    fav_class = fav_classes(pred_fav)
  )

# Shared look for all maps
map_theme <- list(
  ggplot2::geom_sf(data = adm1, fill = NA, color = "gray25", linewidth = 0.35, inherit.aes = FALSE),
  ggplot2::facet_wrap(~wave),
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top")),
  ggplot2::theme_void(base_size = 10),
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
                 plot.caption = ggplot2::element_text(size = 7, color = "gray30", hjust = 0),
                 strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
)

# drop = FALSE and show.legend = TRUE keep every class in the legend; NA is "not surveyed"
map_violence <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = violence_class), color = "white", linewidth = 0.05, show.legend = TRUE) +
  ggplot2::scale_fill_manual(values = c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"), drop = FALSE,
                             na.value = "#d9d9d9", na.translate = TRUE, labels = \(x) ifelse(is.na(x), "Not surveyed", x),
                             name = cfg$violence_label) +
  ggplot2::labs(title = paste(cfg$violence_label, "by commune and survey wave"), caption = caption) +
  map_theme

map_predicted <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = fav_class), color = "white", linewidth = 0.05, show.legend = TRUE) +
  ggplot2::scale_fill_manual(values = viridis::viridis(6, end = 0.9), drop = FALSE,
                             na.value = "#d9d9d9", na.translate = TRUE, labels = \(x) ifelse(is.na(x), "Not surveyed", x),
                             name = "Predicted percent favorable (M3)") +
  ggplot2::labs(title = "Predicted leader favorability by commune and survey wave", caption = caption) +
  map_theme

# Cercle map. Predictions keep only the cercle's own effect (not each commune's),
# averaged over the cercle's respondents with the survey weights. They describe the
# surveyed communes of the cercle, not the whole cercle: only 30-40% of communes are surveyed.
cercle_values <- ad |>
  dplyr::mutate(pred = stats::predict(m3, type = "response", re.form = if (cfg$cercle) ~(1 | cercle_id) else NA),
                adm2_key = clean_name(cercle)) |>
  dplyr::summarise(pred_fav = stats::weighted.mean(pred, wt), communes = dplyr::n_distinct(commune_id),
                   .by = c(adm1_key, adm2_key, wave))

cercle_map_data <- adm2 |>
  dplyr::select(adm1_key, adm2_key) |>
  tidyr::expand_grid(wave = levels(ad$wave)) |>
  sf::st_as_sf() |>
  dplyr::left_join(dplyr::mutate(cercle_values, wave = as.character(wave)), by = c("adm1_key", "adm2_key", "wave")) |>
  dplyr::mutate(wave = paste("Wave", wave), fav_class = fav_classes(pred_fav))

map_predicted_cercle <- ggplot2::ggplot(cercle_map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = fav_class), color = "white", linewidth = 0.1, show.legend = TRUE) +
  ggplot2::scale_fill_manual(values = viridis::viridis(6, end = 0.9), drop = FALSE,
                             na.value = "#d9d9d9", na.translate = TRUE, labels = \(x) ifelse(is.na(x), "Not surveyed", x),
                             name = "Predicted percent favorable (M3)") +
  ggplot2::labs(title = "Predicted leader favorability by cercle and survey wave",
                caption = paste(read_boundary_credit(), "Based on the surveyed communes of each cercle.")) +
  map_theme

dir.create("output/maps", recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave("output/maps/violence_by_commune_wave.png", map_violence, width = 9, height = 4.2, dpi = 300, bg = "white")
ggplot2::ggsave("output/maps/violence_by_commune_wave.pdf", map_violence, width = 9, height = 4.2)
ggplot2::ggsave("output/maps/predicted_favorability_commune_wave.png", map_predicted, width = 9, height = 4.4, dpi = 300, bg = "white")
ggplot2::ggsave("output/maps/predicted_favorability_commune_wave.pdf", map_predicted, width = 9, height = 4.4)
ggplot2::ggsave("output/maps/predicted_favorability_cercle_wave.png", map_predicted_cercle, width = 9, height = 4.4, dpi = 300, bg = "white")
ggplot2::ggsave("output/maps/predicted_favorability_cercle_wave.pdf", map_predicted_cercle, width = 9, height = 4.4)
