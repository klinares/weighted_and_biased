# R/maps.R
# Two cercle maps, saved to output/maps/ as PDF and PNG:
#   1. violence: events per surveyed commune, averaged over the waves the cercle was surveyed
#   2. predicted favorability from M3, by cercle and wave
# Gray means the cercle had no surveyed commune (no survey data and no ACLED count).

source("R/boundaries.R")
if (!exists("m3")) source("R/models.R")
sf::sf_use_s2(FALSE)
adm1 <- read_mali_boundaries("adm1")
adm2 <- read_mali_boundaries("adm2")
credit <- read_boundary_credit()

# ---- 1. One value per cercle (violence) and per cercle and wave (favorability) ----------
ad_map <- dplyr::mutate(ad, adm2_key = clean_name(cercle), pred = stats::predict(m3, type = "response"))

violence_values <- ad_map |>
  dplyr::distinct(adm1_key, adm2_key, wave, events_per_commune) |>
  dplyr::summarise(events = mean(events_per_commune), .by = c(adm1_key, adm2_key))

# Predictions include the cercle's and the cercle-wave's own effects, averaged over
# the cercle's respondents with the survey weights. They describe the surveyed
# communes of the cercle, not the whole cercle.
fav_values <- ad_map |>
  dplyr::summarise(pred_fav = stats::weighted.mean(pred, wt), .by = c(adm1_key, adm2_key, wave))

# Bins that follow the data: zero on its own, then quartiles of the positive values.
# Changing the violence variable changes the bins automatically.
violence_bins <- function(x) {
  cuts = unique(signif(stats::quantile(x[x > 0], c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 2))
  labels = paste(utils::head(cuts, -1), "to", utils::tail(cuts, -1))
  cut(x, c(-Inf, 0, cuts[-1]), labels = c("0", labels))
}

# ---- 2. Join to the cercle polygons ------------------------------------------------
violence_map_data <- adm2 |>
  dplyr::left_join(violence_values, by = c("adm1_key", "adm2_key")) |>
  dplyr::mutate(violence_class = violence_bins(events))

fav_map_data <- adm2 |>
  tidyr::expand_grid(wave = levels(ad$wave)) |>
  sf::st_as_sf() |>
  dplyr::left_join(dplyr::mutate(fav_values, wave = as.character(wave)), by = c("adm1_key", "adm2_key", "wave")) |>
  dplyr::mutate(wave = paste("Wave", wave),
                fav_class = cut(pred_fav, c(-Inf, 0.70, 0.75, 0.80, 0.85, 0.90, Inf), right = FALSE,
                                labels = c("Under 70%", "70-75%", "75-80%", "80-85%", "85-90%", "90% or more")))

# Survey cercles that did not match a polygon (should be 0)
n_not_placed <- nrow(dplyr::anti_join(violence_values, sf::st_drop_geometry(adm2), by = c("adm1_key", "adm2_key")))

# ---- 3. Draw -------------------------------------------------------------------------
map_theme <- list(
  ggplot2::geom_sf(data = adm1, fill = NA, color = "gray25", linewidth = 0.35, inherit.aes = FALSE),
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top")),
  ggplot2::theme_void(base_size = 10),
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
                 plot.caption = ggplot2::element_text(size = 7, color = "gray30", hjust = 0),
                 strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
)
not_surveyed <- \(x) ifelse(is.na(x), "Not surveyed", x)   # legend label for gray

map_violence <- ggplot2::ggplot(violence_map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = violence_class), color = "white", linewidth = 0.1, show.legend = TRUE) +
  ggplot2::scale_fill_brewer(palette = "Reds", drop = FALSE, na.value = "#d9d9d9", labels = not_surveyed,
                             name = "Events per surveyed commune (average across waves)") +
  ggplot2::labs(title = paste(cfg$violence_label, "by cercle"),
                caption = paste0(credit, " Cercles not placed on the map: ", n_not_placed, ".")) +
  map_theme

map_predicted <- ggplot2::ggplot(fav_map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = fav_class), color = "white", linewidth = 0.1, show.legend = TRUE) +
  ggplot2::scale_fill_manual(values = viridis::viridis(6, end = 0.9), drop = FALSE, na.value = "#d9d9d9",
                             labels = not_surveyed, name = "Predicted percent favorable (M3)") +
  ggplot2::facet_wrap(~wave) +
  ggplot2::labs(title = "Predicted leader favorability by cercle and survey wave",
                caption = paste(credit, "Based on the surveyed communes of each cercle.")) +
  map_theme

dir.create("output/maps", recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave("output/maps/violence_by_cercle.png", map_violence, width = 6.5, height = 5, dpi = 300, bg = "white")
ggplot2::ggsave("output/maps/violence_by_cercle.pdf", map_violence, width = 6.5, height = 5)
ggplot2::ggsave("output/maps/predicted_favorability_cercle_wave.png", map_predicted, width = 9, height = 4.4, dpi = 300, bg = "white")
ggplot2::ggsave("output/maps/predicted_favorability_cercle_wave.pdf", map_predicted, width = 9, height = 4.4)
