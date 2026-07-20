# survey_data_config.R
# ============================================================================
# DATA CLEANING + CONFIGURATION for the design-weighted LCA pipeline.
# Sourced by BOTH quarto scripts (modeling and segments) AFTER
# survey_lca_source.R, so both always see identical data and settings.
# This is the ONLY file edited per dataset besides the cleaning code below,
# which is deliberately transparent dplyr the analyst owns.
#
# Produces: raw_survey_dat, process_survey_dat, survey_dat_original (full frame
# with the `keep` flag, preserved for the prediction/export step),
# survey_dat (complete-case analysis frame), items, dictionary, questions, cfg.
# ============================================================================

# ---- 1. Read and process the raw SPSS file ---------------------------------
# Items and design variables stay numeric; nonresponse codes become NA;
# demographics are recoded to character via their label text; the `keep` flag
# marks complete cases (items + demographics); nonresponse value labels are
# stripped so every label left on an item is a real response.
raw_survey_dat <- haven::read_sav(
  "D:/repos/Latent_to_Language/lapop/MEX_2023_LAPOP_AmericasBarometer_v1.0_w.sav")



items       <- c(justice_system = "b10a", electoral_tribunal = "b11",
                 armed_forces = "b12",  legislature = "b13",
                 public_ministry = "b15", police = "b18", auditor = "b19",
                 political_parties = "b21", president = "b21a",
                 supreme_court = "b31", municipality = "b32", media = "b37",
                 elections = "b47a")
design_vars <- c(id = "idnum", strata = "strata", psu = "upm", weight = "wt")
na_codes    <- c(888888, 988888, 999999)   # DK / NR / N-A in this file

process_survey_dat <- raw_survey_dat |>
  dplyr::select(all_of(unname(design_vars)), all_of(unname(items)),
                q1tc_r, q2, edre, ur, ocup4a, pn4, m1) |>
  rename(!!!items) |>
  # items and design variables stay numeric; nonresponse codes to NA
  # Items KEEP their SPSS attributes here: as.numeric() would drop the variable
  # label and the value labels, which the dictionary and the segment-labeling
  # prompts both read. prepare_items() coerces them safely at model time.
  mutate(across(all_of(names(items)), ~ replace(.x, .x %in% na_codes, NA)),
         across(all_of(unname(design_vars)), as.numeric),
         across(c(pn4, m1, q2), ~ replace(as.numeric(.x), .x %in% na_codes, NA))) |>
  # demographics recoded through their label text (as_character), kept character
  mutate(
    age_cat = as.character(cut(q2, breaks = c(17, 29, 44, 59, Inf),
                               labels = c("18-29", "30-44", "45-59", "60+"))),
    male    = ifelse(sjlabelled::as_character(q1tc_r) == "Mujer/femenino",
                     "Female", "Male"),
    Edu     = case_when(
      sjlabelled::as_character(edre) == "Ninguna" ~ "none",
      str_detect(sjlabelled::as_character(edre), "^Primaria") ~ "primary",
      str_detect(sjlabelled::as_character(edre), "^Secundaria") ~ "secondary",
      str_detect(sjlabelled::as_character(edre), "^Universitaria") ~ "higher_ed",
      TRUE ~ NA_character_),
    Urban   = ifelse(sjlabelled::as_character(ur) == "Urbano", "urban", "rural"),
    Employment = case_when(
      str_detect(sjlabelled::as_character(ocup4a),
                 "^Trabajando|pero tiene trabajo") ~ "Employed",
      str_detect(sjlabelled::as_character(ocup4a), "quehaceres") ~ "house_keeper",
      str_detect(sjlabelled::as_character(ocup4a), "estudiante")  ~ "Student",
      str_detect(sjlabelled::as_character(ocup4a), "jubilado|pensionado") ~ "Retired",
      str_detect(sjlabelled::as_character(ocup4a), "buscando") ~ "Unemployed",
      TRUE ~ NA_character_),
    satis_demo  = c("Very satisfied", "Satisfied", "Dissatisfied",
                    "Very dissatisfied")[pn4],
    prez_rating = c("Very good", "Good", "Neither good nor bad", "Bad",
                    "Very bad")[m1]
  ) |>
  dplyr::select(-q1tc_r, -q2, -edre, -ur, -ocup4a, -pn4, -m1) |>
  # strip nonresponse value labels so every label left on an item is a response
  mutate(across(all_of(names(items)),
                ~ sjlabelled::remove_labels(.x, labels = na_codes))) |>
  # the analysis flag: complete on items AND demographics (complete cases only)
  mutate(keep = stats::complete.cases(
    across(c(all_of(names(items)), age_cat, male, Edu, Urban, Employment,
             satis_demo, prez_rating))))

survey_dat_original <- process_survey_dat            # preserved for the export
survey_dat          <- process_survey_dat |> dplyr::filter(keep)
item_codes          <- items          # named vector: analysis name -> .sav variable
items               <- names(items)   # analysis names, used everywhere downstream

# ---- 2. Dictionary: what the model and the segment labeler will see --------
dictionary <- tibble(
  item     = items,
  variable = unname(item_codes[items]),
  # Wording and response labels are read from raw_survey_dat with BASE
  # attributes, not sjlabelled accessors: haven stores the question in
  # attr(x, "label") and the value labels in attr(x, "labels") (a named numeric
  # vector whose NAMES are the response texts). Reading them directly removes
  # any dependence on accessor behaviour or package version. Response labels are
  # per item and restricted to the values observed after the nonresponse recode,
  # so a labeled value prints its label, an unlabeled value prints its number,
  # and anchors-only, fully labeled, and unlabeled items all work unconfigured.
  question = map_chr(items, function(it) {
    lab <- attr(raw_survey_dat[[item_codes[[it]]]], "label", exact = TRUE)
    if (is.character(lab) && length(lab) == 1 && nzchar(lab)) lab else it
  }),
  responses = map_chr(items, function(it) {
    vl  <- attr(raw_survey_dat[[item_codes[[it]]]], "labels", exact = TRUE)
    lk  <- if (length(vl)) set_names(names(vl), as.character(unname(vl))) else character(0)
    obs <- setdiff(sort(unique(as.numeric(survey_dat[[it]]))), na_codes)
    paste(ifelse(as.character(obs) %in% names(lk), lk[as.character(obs)],
                 as.character(obs)), collapse = " | ")
  }))
questions <- set_names(dictionary$question, dictionary$item)

# ---- 3. Configuration -------------------------------------------------------
# K_force is THE analyst decision, made by iterating the modeling script:
# leave NULL, render, read the enumeration diagnostics, set a candidate,
# re-render, judge discrimination/BVR/profiles, adjust. Segment labels follow
# one rule: out_dir/segment_labels.csv is used when it exists, otherwise the
# LLM drafts once and writes it. API keys are never handled in code (ellmer
# reads OPENAI_API_KEY from .Renviron at home and at work).
cfg <- list(
  # --- estimation -------------------------------------------------------------
  items    = items,
  strata   = "strata", psu = "upm", weight = "wt",
  K_range  = 2:12,               # candidates shown in the enumeration table
  K_force  = 5,              # THE analyst decision; set after reviewing diagnostics
  n_starts = 20, seed = 2026, parallel = TRUE, workers = NULL,
  aux      = c("age_cat", "male", "Edu", "Urban", "Employment",
               "satis_demo", "prez_rating"),
  na_codes = NULL,              # items arrive clean from the processing chunk
  min_items_predict = 7L,       # evidence floor: a respondent needs at least this many
  # answered items for a segment prediction (NA below it)
  
  # --- outputs ----------------------------------------------------------------
  out_dir  = "../output/mexico",
  
  # --- LLM class labeling -------------------------------------------------------
  compass_base_url = "https://openrouter.ai/api/v1",  # work: your compass endpoint
  llm_model        = "google/gemma-4-31b-it",         # work: your model name
  survey_context   = paste(
    "These items come from the 2023 AmericasBarometer survey of Mexico,",
    "conducted by the LAPOP Lab at Vanderbilt University. The AmericasBarometer",
    "is a comparative public opinion study of democratic attitudes and",
    "governance across the Americas, fielded to national probability samples",
    "of voting-age adults.",
    "\n\nThe battery analyzed here measures trust in national institutions:",
    "respondents rate, for each institution, how much they trust it on a",
    "seven-point scale anchored at 1 (not at all) and 7 (a lot). The latent",
    "classes summarize patterns of institutional trust across these items."),
  
  data = survey_dat             # complete-case analysis frame from the chunks above
)
dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
