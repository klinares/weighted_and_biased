# =============================================================================
# R/maps.R
# Two maps by commune and wave, saved to output/maps/ (PDF and PNG):
#   1. ACLED events of violence against civilians
#   2. Predicted favorability from Model 4 (fixed effects + commune and village effects)
#
# Communes are joined to the OCHA boundaries by region key + commune key.
# Three states never look alike: Not surveyed (gray), surveyed with zero
# events (palest red), and communes that cannot be placed (counted in captions).
# =============================================================================

if (!exists("m4")) source("R/models.R")

out_dir <- "output/maps"
credit  <- read_boundary_credit()

# ---- 1. Boundaries ------------------------------------------------------------------
sf::sf_use_s2(FALSE)
adm1 <- read_mali_boundaries("adm1")
adm3 <- read_mali_boundaries("adm3")

# A few names repeat inside one region (Benkadi in Koulikoro, Somo in Segou,
# Kapala in Sikasso). The key alone cannot tell which polygon is meant, so those
# communes are left uncolored rather than risk shading the wrong one.
shared_keys <- adm3 |>
  sf::st_drop_geometry() |>
  dplyr::count(adm1_key, adm3_key) |>
  dplyr::filter(n > 1)

# Survey communes that cannot be placed: key not in the boundaries, or shared
survey_keys <- dplyr::distinct(commune_pred, adm1_key, adm3_key, commune_id)
not_placed <- survey_keys |>
  dplyr::anti_join(sf::st_drop_geometry(adm3), by = c("adm1_key", "adm3_key")) |>
  dplyr::bind_rows(dplyr::semi_join(survey_keys, shared_keys, by = c("adm1_key", "adm3_key")))

caption <- glue::glue("{credit} Survey communes not placed on the map: {nrow(not_placed)}.")

# ---- 2. Every commune polygon x every wave, so unsurveyed communes stay visible ---------
map_data <- adm3 |>
  dplyr::select(adm1_key, adm3_key) |>
  tidyr::expand_grid(wave = levels(ad$wave)) |>
  sf::st_as_sf() |>
  dplyr::left_join(
    commune_pred |>
      dplyr::anti_join(shared_keys, by = c("adm1_key", "adm3_key")) |>
      dplyr::mutate(wave = as.character(wave)),
    by = c("adm1_key", "adm3_key", "wave")) |>
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
      is.na(pred_fav)  ~ "Not surveyed",
      pred_fav < 0.70  ~ "Under 70%",
      pred_fav < 0.75  ~ "70-75%",
      pred_fav < 0.80  ~ "75-80%",
      pred_fav < 0.85  ~ "80-85%",
      pred_fav < 0.90  ~ "85-90%",
      TRUE             ~ "90% or more")
  )

# ---- 3. Colors (fixed order so every wave shares one legend) ----------------------------
violence_colors <- c("0" = "#fee5d9", "1-2" = "#fcae91", "3-9" = "#fb6a4a",
                     "10-29" = "#de2d26", "30 or more" = "#a50f15",
                     "Not surveyed" = "#d9d9d9")

fav_levels <- c("Under 70%", "70-75%", "75-80%", "80-85%", "85-90%", "90% or more")
fav_colors <- c(stats::setNames(viridis::viridis(6, end = 0.9), fav_levels),
                "Not surveyed" = "#d9d9d9")

# ---- 4. One drawing function ----------------------------------------------------------
draw_map <- function(fill_var, colors, legend_title, title, subtitle = NULL) {
  ggplot2::ggplot(map_data) +
    # show.legend = TRUE keeps every class in the legend, even empty ones
    ggplot2::geom_sf(ggplot2::aes(fill = factor(.data[[fill_var]], levels = names(colors))),
                     color = "white", linewidth = 0.05, show.legend = TRUE) +
    ggplot2::geom_sf(data = adm1, fill = NA, color = "gray25", linewidth = 0.35,
                     inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE, name = legend_title) +
    ggplot2::facet_wrap(~ wave_label) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top")) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 11),
                   plot.subtitle = ggplot2::element_text(size = 8, color = "gray30"),
                   plot.caption  = ggplot2::element_text(size = 7, color = "gray30", hjust = 0),
                   strip.text    = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}

map_violence <- draw_map("violence_class", violence_colors,
                         "Events of violence against civilians (ACLED)",
                         "Violence against civilians by commune and survey wave")

map_predicted <- draw_map("fav_class", fav_colors,
                          "Predicted percent with a favorable view of the leader (Model 4)",
                          "Predicted leader favorability by commune and survey wave",
                          paste0("Model 4 predictions including each commune's and village's own effects, averaged ",
                                 "over its respondents with survey weights.\nCommunes with few ",
                                 "respondents are pulled toward the overall level."))

# ---- 5. Save publication copies ---------------------------------------------------------
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_map <- function(plot, name, width, height) {
  ggplot2::ggsave(file.path(out_dir, paste0(name, ".pdf")), plot, width = width, height = height)
  ggplot2::ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = width, height = height,
                  dpi = 300, bg = "white")
}
save_map(map_violence,  "violence_by_commune_wave",           9, 4.2)
save_map(map_predicted, "predicted_favorability_commune_wave", 9, 4.4)
