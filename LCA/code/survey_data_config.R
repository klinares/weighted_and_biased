# survey_data_config.R
# Reading, cleaning, and configuration. This is the only file edited per dataset.
# Sourced after survey_lca_source.R by both .qmd files.
#
# Produces: raw_survey_dat, survey_dat_full, item_levels, cats, dictionary,
# recode_audit, items, cfg.

# ---- 1. Settings ------------------------------------------------------------
item_codes <- c(justice_system = "b10a", electoral_tribunal = "b11",
                armed_forces = "b12", legislature = "b13",
                public_ministry = "b15", police = "b18", auditor = "b19",
                political_parties = "b21", president = "b21a",
                supreme_court = "b31", municipality = "b32", media = "b37",
                elections = "b47a")

demo_codes <- c(age_cat = "q2", sex = "q1tc_r", education = "edre",
                urban = "ur", employment = "ocup4a",
                satis_demo = "pn4", prez_rating = "m1")

items <- names(item_codes)
demos <- names(demo_codes)

na_codes <- c(888888, 988888, 999999)

# TRUE keeps "don't know" as a substantive category. The recode below sorts
# values, so the large DK code lands in category C+1 with no extra arithmetic.
dk_as_category <- FALSE
codes_to_drop <- if (dk_as_category) setdiff(na_codes, 888888) else na_codes

# TRUE fits on item-complete cases. FALSE fits on everyone with at least
# min_items answered, using the EM's own handling of missing items.
complete_cases <- TRUE
min_items <- 7L

# ---- 2. Read ----------------------------------------------------------------
# user_na = TRUE keeps the nonresponse codes as values instead of letting haven
# convert them to NA on read, so na_codes above does real work.

raw_survey_dat <- haven::read_sav(
  here::here("lca", "data", "MEX_2023_LAPOP_AmericasBarometer_v1.0_w.sav"),
  user_na = TRUE)

# ---- 3. Items ---------------------------------------------------------------
# select() with a named vector renames on the way through. Items are recoded to
# consecutive integers here, on every row, so the estimation frame and the
# prediction frame are always on the same coding. Levels come from the rows that
# will actually be fitted; a value seen only outside that set becomes NA and
# drops out of that respondent's product.

item_dat <- raw_survey_dat |>
  select(all_of(item_codes)) |>
  mutate(across(everything(), function(x) {
    v <- as.numeric(unclass(x))
    if_else(v %in% codes_to_drop, NA_real_, v)
  }))

n_answered <- rowSums(!is.na(item_dat))
in_analysis <- if (complete_cases) n_answered == length(items) else n_answered >= min_items

item_levels <- map(item_dat[in_analysis, ], function(x) sort(unique(x[!is.na(x)])))
cats <- map_int(item_levels, length)

item_dat <- item_dat |>
  mutate(across(everything(), function(x) match(x, item_levels[[cur_column()]])))

# ---- 4. Design --------------------------------------------------------------
design_dat <- raw_survey_dat |>
  transmute(id = as.numeric(unclass(idnum)),
            strata = as.numeric(unclass(strata)),
            psu = as.numeric(unclass(upm)),
            wt = as.numeric(unclass(wt)))

# ---- 5. Demographics --------------------------------------------------------
# One recode_values per variable, every observed label named. unmatched = "error"
# halts the render on any label not named here, which is the point: a
# pattern-matching scheme would silently misclassify or delete those respondents.
# The NA ~ NA arms are required by unmatched = "error"; zap_missing() turns the
# nonresponse codes into NA first, so "DK" and "NR" arrive here as NA.

demo_dat <- raw_survey_dat |>
  select(all_of(demo_codes)) |>
  haven::zap_missing() |>
  transmute(

    age_cat = cut(as.numeric(age_cat), breaks = c(17, 29, 44, 59, Inf),
                  labels = c("18-29", "30-44", "45-59", "60+")) |>
      as.character(),

    sex = as.character(haven::as_factor(sex)) |>
      recode_values("Hombre/masculino" ~ "Male",
                    "Mujer/femenino" ~ "Female",
                    NA ~ NA_character_,
                    unmatched = "error"),

    education = as.character(haven::as_factor(education)) |>
      recode_values(
        "Ninguna" ~ "None",
        c("Primaria incompleta", "Primaria completa") ~ "Primary",
        c("Secundaria o Educaci\u00f3n Media Superior/Bachillerato/Preparatoria/Profesional T\u00e9cnico incompleta",
          "Secundaria o Educaci\u00f3n Media Superior/Bachillerato/Preparatoria/Profesional T\u00e9cnico completa") ~ "Secondary",
        c("Universitaria, superior no universitaria o t\u00e9cnico universitario incompleta",
          "Universitaria, superior no universitaria o t\u00e9cnico universitario completa") ~ "Tertiary",
        NA ~ NA_character_,
        unmatched = "error"),

    urban = as.character(haven::as_factor(urban)) |>
      recode_values("Urbano" ~ "Urban",
                    "Rural" ~ "Rural",
                    NA ~ NA_character_,
                    unmatched = "error"),

    employment = as.character(haven::as_factor(employment)) |>
      recode_values(
        c("Trabajando?",
          "No est\u00e1 trabajando en este momento pero tiene trabajo?") ~ "Employed",
        "Est\u00e1 buscando trabajo activamente?" ~ "Unemployed",
        "No trabaja y no est\u00e1 buscando trabajo?" ~ "Not in labor force",
        "Es estudiante?" ~ "Student",
        "Se dedica a los quehaceres de su hogar?" ~ "Homemaker",
        "Est\u00e1 jubilado, pensionado o incapacitado permanentemente para trabajar?" ~ "Retired",
        NA ~ NA_character_,
        unmatched = "error"),

    satis_demo = as.character(haven::as_factor(satis_demo)) |>
      recode_values("Muy satisfecho(a)" ~ "Very satisfied",
                    "Satisfecho(a)" ~ "Satisfied",
                    "Insatisfecho(a)" ~ "Dissatisfied",
                    "Muy insatisfecho(a)" ~ "Very dissatisfied",
                    NA ~ NA_character_,
                    unmatched = "error"),

    prez_rating = as.character(haven::as_factor(prez_rating)) |>
      recode_values("Muy bueno" ~ "Very good",
                    "Bueno" ~ "Good",
                    "Ni bueno, ni malo (regular)" ~ "Neither",
                    "Malo" ~ "Bad",
                    "Muy malo (p\u00e9simo)" ~ "Very bad",
                    NA ~ NA_character_,
                    unmatched = "error"))

# First level of each is the contrast reference in the segments script.
demo_levels <- list(
  age_cat = c("18-29", "30-44", "45-59", "60+"),
  sex = c("Male", "Female"),
  education = c("Secondary", "None", "Primary", "Tertiary"),
  urban = c("Urban", "Rural"),
  employment = c("Employed", "Unemployed", "Not in labor force", "Student",
                 "Homemaker", "Retired"),
  satis_demo = c("Satisfied", "Very satisfied", "Dissatisfied", "Very dissatisfied"),
  prez_rating = c("Good", "Very good", "Neither", "Bad", "Very bad"))

demo_dat <- demo_dat |>
  mutate(across(all_of(demos),
                function(x) factor(x, levels = demo_levels[[cur_column()]])))

# ---- 6. Assemble ------------------------------------------------------------
# n_items_answered is not stored here: predict_segments() computes it from the
# same items, and two copies would collide in bind_cols() downstream.
survey_dat_full <- bind_cols(design_dat, item_dat, demo_dat) |>
  mutate(in_analysis = in_analysis)

# What each source label became. Read once per dataset.
recode_audit <- imap(demo_codes, function(src, tgt) {
  tibble(variable = tgt,
         source_label = as.character(haven::as_factor(raw_survey_dat[[src]])),
         recoded = as.character(survey_dat_full[[tgt]]))
}) |>
  list_rbind() |>
  count(variable, source_label, recoded, name = "n") |>
  arrange(variable, desc(n))

# ---- 7. Dictionary ----------------------------------------------------------
# Question wording and response labels in item_levels order, so the response text
# lines up with the fitted category indices. Read from base attributes.

dictionary <- tibble(item = items, variable = unname(item_codes)) |>
  mutate(
    question = map_chr(variable, function(v) {
      lab <- attr(raw_survey_dat[[v]], "label", exact = TRUE)
      if (is.character(lab) && length(lab) == 1 && nzchar(lab)) lab else v
    }),
    responses = map2(variable, item, function(v, it) {
      vl <- attr(raw_survey_dat[[v]], "labels", exact = TRUE)
      key <- if (length(vl)) set_names(names(vl), as.character(unname(vl))) else character(0)
      vals <- as.character(item_levels[[it]])
      unname(if_else(vals %in% names(key), key[vals], vals))
    }))

questions <- set_names(dictionary$question, dictionary$item)

# ---- 8. Configuration -------------------------------------------------------
# K_force is the analyst decision: leave NULL, render the modeling script, read
# the enumeration evidence, set a candidate, re-render.

# satis_demo and prez_rating are cleaned and exported but excluded from
# profiling: they are attitudes, not demographics, and both are correlated with
# institutional trust by construction, so profiling segments on them would partly
# explain the measurement model with itself.
profile_vars <- setdiff(demos, c("satis_demo", "prez_rating"))

cfg <- list(
  items = items,
  aux = profile_vars,
  strata = "strata", psu = "psu", weight = "wt", id = "id",
  cats = cats,
  min_items = min_items,

  K_range = 2:10,
  K_force = 5,
  n_starts = 200,
  seed = 2026,
  parallel = TRUE,
  workers = NULL,
  run_item_screen = TRUE,

  out_dir = here::here("output"),

  # Home: leave compass_base_url NULL and OpenRouter is used, reading
  # OPENROUTER_API_KEY from .Renviron. Work: set compass_base_url and llm_model,
  # and COMPASS_API_KEY is read instead.
  compass_base_url = NULL,
  llm_model = "google/gemma-3-27b-it",

  survey_context = paste(
    "These items come from the 2023 AmericasBarometer survey of Mexico,",
    "conducted by the LAPOP Lab at Vanderbilt University. The AmericasBarometer",
    "is a comparative public opinion study of democratic attitudes and",
    "governance across the Americas, fielded to national probability samples",
    "of voting-age adults.",
    "\n\nThe battery analyzed here measures trust in national institutions:",
    "respondents rate, for each institution, how much they trust it on a",
    "seven-point scale anchored at 1 (not at all) and 7 (a lot). The segments",
    "summarize patterns of institutional trust across these items.")
)

cfg$data <- filter(survey_dat_full, in_analysis)

dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
