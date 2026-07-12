# prepare_data_lapop.R
# ============================================================================
# DATASET ADAPTER: LAPOP Mexico 2023 (AmericasBarometer) -> pipeline-ready tibble
#
# This is the ONLY dataset-specific file. It is sourced by the analysis .qmd in
# a chunk BEFORE the config, and it produces four objects:
#   lapop             the prepared labelled tibble (design vars, items, demos)
#   lapop_dictionary  item code, English name, Spanish question (as extracted
#                     from the .sav), English question, and the response map
#   lapop_items       the 13 English item names, in order (goes into cfg$items)
#   lapop_before      the pre-cleaning item subset, for the before/after plot
# plus one helper the .qmd uses for both plots:
#   plot_item_stack(df, items, title)   stacked response-proportion bars
#
# ADAPTING TO ANOTHER DATASET (e.g. Latinobarometro): copy this file, then edit
# ONLY the pd block below: path, design-variable names, item codes and English
# names, the nonresponse label regex, the English question template/labels, and
# the demographic recodes. Nothing downstream changes.
#
# MISSINGNESS POLICY (deliberate, matches the analysis document):
#   - values whose LABEL matches pd$na_label_regex (No sabe / No responde /
#     No aplica, coded 98/99/888888-style) are recoded to NA, with an audit
#     table printed of exactly which labels were caught;
#   - listwise deletion applies to design variables, demographics, and
#     outcomes (they are covariates; a case without them profiles nothing);
#   - item-PARTIAL respondents are KEPT: the pipeline fits on complete cases
#     and assigns those respondents a class afterwards from their answered
#     items (the scoring section). Only rows missing EVERY item are dropped,
#     because there is nothing to score them from.
#
# TRANSLATION: the .sav labels are Spanish. English question text is authored
# here from the analyst's English item names using the B-series template
# ("To what extent do you trust ...?", anchors 1 = Not at all, 7 = A lot) and
# is SET ONTO the tibble with sjlabelled, so everything downstream (dictionary,
# prompts, plots) inherits English automatically. The dictionary keeps the
# extracted Spanish beside the English: CONFIRM the pairing there before
# trusting any LLM labels.
# ============================================================================

# ----------------------------------------------------------------------------
# pd: the only block you edit when adapting to a new dataset
# ----------------------------------------------------------------------------
pd <- list(
  sav_path = "D:/repos/weighted_and_biased/LCA/data/MEX_2023_LAPOP_AmericasBarometer_v1.0_w.sav",
  
  # design variables, as named in the file
  id = "idnum", strata = "strata", psu = "upm", weight = "wt",   # wt is constant 1 here
  
  # items: .sav code -> English name (order here is the analysis order)
  item_names = c(
    justice_system     = "b10a",
    electoral_tribunal = "b11",
    armed_forces       = "b12",
    legislature        = "b13",
    public_ministry    = "b15",
    police             = "b18",
    auditor            = "b19",
    political_parties  = "b21",
    president          = "b21a",
    supreme_court      = "b31",
    municipality       = "b32",
    media              = "b37",
    elections          = "b47a"
  ),
  
  # nonresponse detection: any value whose LABEL matches this regex becomes NA
  # (case-insensitive). Extend per dataset/language.
  na_label_regex = "no sabe|no responde|no contesta|no aplica|^dk$|^nr$|^n/a$",
  
  # English wording, authored: B-series template + anchor labels
  question_template = "To what extent do you trust {institution}?",
  response_en = c("Not at all", "2", "3", "4", "5", "6", "A lot"),   # values 1..7
  
  # demographics / outcomes used for profiling (source columns in the file)
  demo_source = c("q1tc_r", "q2", "edre", "ur", "ocup4a", "pn4", "m1")
)

# ----------------------------------------------------------------------------
# 1. Read, then keep only what the analysis touches
# ----------------------------------------------------------------------------
# haven::read_sav returns labelled NUMERICS unconditionally (observed:
# sjlabelled::read_spss factor-converted despite atomic.to.fac = FALSE, which
# breaks every code-based recode downstream). The sjlabelled accessors
# (get_label/get_labels/get_values) work on haven-labelled columns as-is.
lapop_raw <- haven::read_sav(pd$sav_path) |> as_tibble()

lapop_items <- names(pd$item_names)
design_vars <- c(pd$id, pd$strata, pd$psu, pd$weight)

lapop_work <- lapop_raw |>
  dplyr::select(all_of(c(design_vars, unname(pd$item_names), pd$demo_source))) |>
  rename(!!!pd$item_names)

# Snapshot BEFORE any missing-data handling, for the before/after plot.
lapop_before <- lapop_work |> dplyr::select(all_of(lapop_items))

# ----------------------------------------------------------------------------
# 2. Nonresponse -> NA, with a printed audit of exactly what was caught
# ----------------------------------------------------------------------------
# Evidence first: the full value-label inventory across all items, then the
# subset the regex flags as nonresponse. Both print; the inventory is what you
# tune pd$na_label_regex against, and an EMPTY audit is a legitimate outcome
# (LAPOP often declares nonresponse as user-missing, already NA at read).
label_inventory <- map_dfr(lapop_items, function(it) {
  labs <- sjlabelled::get_labels(lapop_work[[it]])
  vals <- sjlabelled::get_values(lapop_work[[it]])
  if (length(labs) == 0 || length(labs) != length(vals)) return(
    tibble(item = character(), value = numeric(), label = character()))
  tibble(item = it, value = as.numeric(vals), label = as.character(labs))
})
cat("Value-label inventory (distinct labels across the ", length(lapop_items),
    " items):\n", sep = "")
print(label_inventory |> dplyr::count(value, label, name = "n_items"), n = Inf)

na_audit <- label_inventory |>
  dplyr::filter(str_detect(tolower(label), pd$na_label_regex))
na_map <- na_audit |> distinct(value, label)

if (nrow(na_map) == 0) {
  cat("\nNo label-based nonresponse codes matched pd$na_label_regex ('",
      pd$na_label_regex, "').\n",
      "Either nonresponse is user-missing in the .sav and is ALREADY NA at read\n",
      "(then the before/after plots will match, correctly), or the labels use\n",
      "wording the regex misses: check the inventory above and extend the regex.\n",
      sep = "")
} else {
  cat("\nNonresponse labels recoded to NA:\n"); print(na_map, n = Inf)
  # The outcomes (pn4, m1) and age (q2) share the same nonresponse codes, so
  # the recode covers them too; their NAs then fall to listwise deletion below.
  na_recode_vars <- c(lapop_items, intersect(c("pn4", "m1", "q2"), names(lapop_work)))
  lapop_work <- lapop_work |>
    mutate(across(all_of(na_recode_vars),
                  ~ replace(.x, .x %in% na_map$value, NA)))
}

# ----------------------------------------------------------------------------
# 3. English wording set ONTO the items (single source of truth downstream)
# ----------------------------------------------------------------------------
english_question <- function(nm) {
  institution <- str_replace_all(nm, "_", " ")
  str_glue(pd$question_template, institution = str_c("the ", institution))
}
lapop_work <- lapop_work |>
  mutate(across(all_of(lapop_items), function(x) {
    nm <- cur_column()
    x  <- sjlabelled::set_label(x, label = as.character(english_question(nm)))
    sjlabelled::set_labels(x, labels = set_names(seq_along(pd$response_en),
                                                 pd$response_en),
                           force.labels = TRUE)
  }))

# ----------------------------------------------------------------------------
# 4. Demographics and outcomes (analyst-authored English recodes)
# ----------------------------------------------------------------------------
# pn4/m1 stay LABELLED NUMERICS (their factor recodes below match codes 1..4 /
# 1..5); only the demographics recoded by label STRING are converted. Converting
# the outcomes too was the bug that zeroed the sample.
lapop_mut <- lapop_work |>
  mutate(across(all_of(setdiff(pd$demo_source, c("q2", "pn4", "m1"))),
                sjlabelled::as_character)) |>
  mutate(
    age_cat = cut(q2, breaks = c(17, 29, 44, 59, Inf),
                  labels = c("18-29", "30-44", "45-59", "60+"), right = TRUE),
    male    = factor(ifelse(q1tc_r == "Mujer/femenino", "Female", "Male")),
    Edu = case_when(
      edre == "Ninguna" ~ "none",
      str_detect(edre, "^Primaria") ~ "primary",
      str_detect(edre, "^Secundaria") ~ "secondary",
      str_detect(edre, "^Universitaria") ~ "higher_ed",
      TRUE ~ NA_character_) |>
      factor(levels = c("none", "primary", "secondary", "higher_ed")),
    Urban = factor(ifelse(ur == "Urbano", "urban", "rural")),
    Employment = case_when(
      str_detect(ocup4a, "^Trabajando|pero tiene trabajo") ~ "Employed",
      str_detect(ocup4a, "quehaceres")                     ~ "house_keeper",
      str_detect(ocup4a, "estudiante")                     ~ "Student",
      str_detect(ocup4a, "jubilado|pensionado")            ~ "Retired",
      str_detect(ocup4a, "buscando trabajo|no est\u00e1 buscando") ~ "Unemployed",
      TRUE ~ NA_character_) |> factor(),
    # outcomes, English levels per the analyst's translation notes
    satis_demo = factor(pn4, levels = 1:4,
                        labels = c("Very satisfied", "Satisfied",
                                   "Dissatisfied", "Very dissatisfied")),
    prez_rating = factor(m1, levels = 1:5,
                         labels = c("Very good", "Good", "Neither good nor bad",
                                    "Bad", "Very bad"))
  )

# ---- Recode audit (the layer drop_na actually gates) -----------------------
# For each derived variable: its NA share, and its UNMATCHED share (source
# present but the recode produced NA), the fingerprint of a label-mismatch bug.
# Any derived variable with meaningful unmatched share also prints the source
# values it failed on, so the fix is evident from the output.
derived <- c("age_cat", "male", "Edu", "Urban", "Employment",
             "satis_demo", "prez_rating")
src_of  <- c(age_cat = "q2", male = "q1tc_r", Edu = "edre", Urban = "ur",
             Employment = "ocup4a", satis_demo = "pn4", prez_rating = "m1")
recode_audit <- map_dfr(derived, function(d) {
  s <- src_of[[d]]
  tibble(derived = d, source = s,
         na_share  = mean(is.na(lapop_mut[[d]])),
         unmatched = mean(is.na(lapop_mut[[d]]) & !is.na(lapop_mut[[s]])))
})
cat("\nRecode audit (derived variables; 'unmatched' = source present, recode NA):\n")
print(recode_audit |> mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf)
walk(derived, function(d) {
  s <- src_of[[d]]
  bad <- lapop_mut |> dplyr::filter(is.na(.data[[d]]), !is.na(.data[[s]]))
  if (nrow(bad) / nrow(lapop_mut) > 0.02) {
    cat("\nUnmatched source values for ", d, " (from ", s, "):\n", sep = "")
    print(bad |> dplyr::count(.data[[s]], sort = TRUE) |> slice_head(n = 8))
  }
})

lapop <- lapop_mut |>
  dplyr::select(all_of(design_vars), all_of(lapop_items),
                age_cat, male, Edu, Urban, Employment, satis_demo, prez_rating) |>
  # listwise deletion on design + demographics + outcomes ONLY; item-partial
  # respondents are KEPT for scoring, rows missing EVERY item are dropped.
  drop_na(all_of(design_vars), age_cat, male, Edu, Urban, Employment,
          satis_demo, prez_rating) |>
  dplyr::filter(!if_all(all_of(lapop_items), is.na))

# ----------------------------------------------------------------------------
# 5. Dictionary: code, name, Spanish (extracted), English (authored), responses
# ----------------------------------------------------------------------------
lapop_dictionary <- tibble(
  item     = lapop_items,
  sav_code = unname(pd$item_names),
  question_es = map_chr(unname(pd$item_names),
                        ~ sjlabelled::get_label(lapop_raw[[.x]]) %||% .x),
  question_en = map_chr(lapop_items, ~ as.character(english_question(.x)))
)
lapop_responses <- tibble(value = seq_along(pd$response_en),
                          response_en = pd$response_en)

# ----------------------------------------------------------------------------
# 6. Plot helper used by the .qmd for the before/after view
# ----------------------------------------------------------------------------
plot_item_stack <- function(df, items, title) {
  df |>
    dplyr::select(all_of(items)) |>
    mutate(across(everything(), as.numeric)) |>
    pivot_longer(everything(), names_to = "item", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::count(item, value) |>
    ggplot(aes(item, n, fill = factor(value))) +
    geom_col(position = "fill") +
    scale_fill_viridis_d(name = "Response") +
    labs(x = NULL, y = "Proportion", title = title) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Attrition accounting: where cases went, stage by stage. A zero here must be
# loud and diagnosable, never silent.
n_read <- nrow(lapop_work)
cat("\nAttrition: ", n_read, " read -> ", nrow(lapop), " prepared (",
    n_read - nrow(lapop), " dropped by the listwise rule on design/demographics/",
    "outcomes or by missing every item).\n", sep = "")
if (nrow(lapop) == 0) {
  stop("prepare_data_lapop.R produced 0 respondents. The recode audit above ",
       "identifies the derived variable responsible (high 'unmatched' share) ",
       "and prints the source values its recode failed to match.")
}
cat("Prepared: ", nrow(lapop), " respondents, ",
    length(lapop_items), " items (1..7), ",
    sum(!complete.cases(lapop[, lapop_items])),
    " item-partial cases kept for scoring.\n", sep = "")