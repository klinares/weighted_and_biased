# =============================================================================
# recode_lapop_colombia.R
# Full recode of the Colombia LAPOP subset into the analysis-ready dataset.
#
# DEPENDS ON: `colombia` produced by read_lapop_colombia.R (in session or
#   re-source that script first). `lapop` (the full raw object) is also needed
#   for the raw etid/tamano/ur1new distributions in the diagnostic prints.
#
# WHAT THIS SCRIPT DOES -------------------------------------------------------
#   1. Recodes age into 5 ordered groups.
#   2. Collapses etid into 4 ethnicity groups (prints raw codes first to verify).
#   3. Builds 3-level urbanicity wave-conditionally:
#        2004-2018: tamano (1-5 size of place) -> City / Suburb / Rural
#        2021:      ur1new (5-category)        -> City / Suburb / Rural
#        2023:      ur (binary): Rural direct; Urban split into City/Suburb
#                   probabilistically using each department's historical
#                   City:Suburb ratio from the other waves (seeded imputation).
#   4. Binarizes dem_sat: satisfied = {1,2} -> 1; not satisfied = {3,4} -> 0.
#   5. Assembles the analysis-ready dataset lapop_col with all design variables.
#   6. Builds the svydesign object using weight1500 (confirmed: sums to 1500/yr),
#      upm as PSU, strata throughout; 2021 has no cluster so upm is used there.
#   7. Prints national weighted satisfaction rates by year (validate vs DANE).
#   8. Persists lapop_col and svy_design to disk via save_result().
#
# CODING STANDARDS: no for loops (purrr map); tidyverse loaded last.
# =============================================================================

library(haven)
library(survey)
library(tidyverse)

source("00_config.R")

if (!exists("colombia")) stop("Run read_lapop_colombia.R first.")
if (!exists("lapop"))    stop("lapop (full raw object) must be in session.")

v_country <- "pais"
colombia_code <- 8L

# =============================================================================
# 1. AGE GROUPS (5 levels)
# =============================================================================
colombia <- colombia %>%
  mutate(
    age_group = cut(age,
                    breaks = c(17, 25, 35, 50, 65, Inf),
                    labels = c("18-25", "26-35", "36-50", "51-65", "66+"),
                    right  = TRUE, include.lowest = FALSE),
    age_group = factor(age_group,
                       levels = c("18-25", "26-35", "36-50", "51-65", "66+"))
  )

cat("\n--- Age group distribution ---\n")
colombia %>%
  count(age_group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# =============================================================================
# 2. ETHNICITY COLLAPSE (4 groups)
# =============================================================================
# Print raw etid codes FIRST -- verify the mapping matches this merged file.
# Standard LAPOP etid: 1=White/Blanco, 2=Mestizo, 3=Indigenous, 4=Black/Afro,
#   5=Mulato, 6=Other, 7=Zambo/Other Afro, 77/88/98/99=DK/NA.
cat("\n--- Raw etid codes (VERIFY before trusting the collapse below) ---\n")
lapop %>%
  filter(haven::zap_labels(.data[[v_country]]) == colombia_code) %>%
  count(etid_raw = haven::zap_labels(etid),
        etid_lab = as.character(haven::as_factor(etid))) %>%
  arrange(etid_raw) %>%
  print(n = 30)

# Pull etid numeric codes aligned to the colombia rows.
col_idx  <- which(haven::zap_labels(lapop[[v_country]]) == colombia_code)
eth_code <- as.numeric(haven::zap_labels(lapop[["etid"]][col_idx]))

colombia <- colombia %>%
  mutate(
    eth_code  = eth_code,
    ethnicity4 = case_when(
      eth_code %in% c(1, 2)          ~ "Mestizo/White",
      eth_code == 3                   ~ "Indigenous",
      eth_code %in% c(4, 5, 7)       ~ "Afro-Colombian",
      eth_code == 6                   ~ "Other",
      eth_code %in% c(77,88,98,99)   ~ NA_character_,
      is.na(eth_code)                ~ NA_character_,
      TRUE                           ~ "Other"
    ),
    ethnicity4 = factor(ethnicity4,
                        levels = c("Mestizo/White", "Afro-Colombian",
                                   "Indigenous", "Other"))
  ) %>%
  select(-eth_code)

cat("\n--- Ethnicity 4-group distribution ---\n")
colombia %>%
  count(ethnicity4) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# =============================================================================
# 3. URBANICITY (3 levels: City / Suburb / Rural) -- WAVE-CONDITIONAL
# =============================================================================
# Print raw distributions FIRST to confirm coding direction.
cat("\n--- tamano raw codes (2004-2018) ---\n")
lapop %>%
  filter(haven::zap_labels(.data[[v_country]]) == colombia_code,
         haven::zap_labels(year) %in% 2004:2018) %>%
  count(tam_raw = haven::zap_labels(tamano),
        tam_lab = as.character(haven::as_factor(tamano))) %>%
  arrange(tam_raw) %>%
  print()

cat("\n--- ur1new raw codes (2021) ---\n")
lapop %>%
  filter(haven::zap_labels(.data[[v_country]]) == colombia_code,
         haven::zap_labels(wave) == 2021) %>%
  count(ur1new_raw = haven::zap_labels(ur1new),
        ur1new_lab = as.character(haven::as_factor(ur1new))) %>%
  arrange(ur1new_raw) %>%
  print()

cat("\n--- ur raw codes (2023) ---\n")
lapop %>%
  filter(haven::zap_labels(.data[[v_country]]) == colombia_code,
         haven::zap_labels(year) == 2023) %>%
  count(ur_raw = haven::zap_labels(ur),
        ur_lab = as.character(haven::as_factor(ur))) %>%
  arrange(ur_raw) %>%
  print()

# Build the three numeric sources aligned to the colombia rows.
tamano_num <- as.numeric(haven::zap_labels(lapop[["tamano"]][col_idx]))
ur1new_num <- as.numeric(haven::zap_labels(lapop[["ur1new"]][col_idx]))
ur_num     <- as.numeric(haven::zap_labels(lapop[["ur"]][col_idx]))
yr_num     <- as.integer(haven::zap_labels(lapop[["year"]][col_idx]))

# Collapse helper: 5-category -> 3-level (City/Suburb/Rural).
collapse_5cat <- function(x) {
  case_when(
    x %in% c(1, 2) ~ "City",
    x %in% c(3, 4) ~ "Suburb",
    x == 5          ~ "Rural",
    TRUE            ~ NA_character_
  )
}

# 2023 has only binary ur (Urban/Rural), so its "Urban" lumps City + Suburb.
# We split 2023 Urban respondents into City vs Suburb probabilistically, using
# each department's historical City:Suburb composition from the OTHER waves.
# This is a hot-deck-style imputation (seeded); document it in the methods.

# Step 1: preliminary urbanicity for non-2023 waves only.
urb_pre <- case_when(
  yr_num %in% 2004:2018 ~ collapse_5cat(tamano_num),
  yr_num == 2021        ~ collapse_5cat(ur1new_num),
  TRUE                  ~ NA_character_
)

# Step 2: per-department P(Suburb | Urban) from non-2023 waves (unweighted).
dept_cs <- tibble(department = as.character(colombia$region), urb = urb_pre) %>%
  filter(urb %in% c("City", "Suburb")) %>%
  count(department, urb) %>%
  pivot_wider(names_from = urb, values_from = n, values_fill = 0L) %>%
  mutate(
    denom    = City + Suburb,
    p_suburb = if_else(denom > 0, Suburb / denom, 0)
  ) %>%
  select(department, p_suburb)

cat("\n--- P(Suburb | Urban) by department (from non-2023 waves) ---\n")
dept_cs %>% arrange(desc(p_suburb)) %>% print(n = Inf)

# Step 3: draw City/Suburb for every 2023 Urban respondent.
p_sub_vec <- dept_cs$p_suburb[match(as.character(colombia$region),
                                    dept_cs$department)]
p_sub_vec <- replace_na(p_sub_vec, 0)

set.seed(721)
draw_2023          <- runif(nrow(colombia))
assign_2023_urban  <- if_else(draw_2023 < p_sub_vec, "Suburb", "City")

urbanicity_vec <- case_when(
  yr_num %in% 2004:2018        ~ collapse_5cat(tamano_num),
  yr_num == 2021               ~ collapse_5cat(ur1new_num),
  yr_num == 2023 & ur_num == 1 ~ assign_2023_urban,   # split Urban -> City/Suburb
  yr_num == 2023 & ur_num == 2 ~ "Rural",
  TRUE                         ~ NA_character_
)

colombia <- colombia %>%
  mutate(urbanicity3 = factor(urbanicity_vec,
                               levels = c("City", "Suburb", "Rural")))

cat("\n--- Urbanicity by year (2023 now has Suburb via imputation) ---\n")
colombia %>%
  count(year, urbanicity3) %>%
  pivot_wider(names_from = urbanicity3, values_from = n, values_fill = 0L) %>%
  print(n = Inf)


# =============================================================================
# 4. BINARY OUTCOME: satisfied with democracy
# =============================================================================
# pn4: 1=very satisfied, 4=very dissatisfied. Satisfied = {1,2}.
colombia <- colombia %>%
  mutate(
    satisfied = case_when(
      dem_sat %in% c(1, 2) ~ 1L,
      dem_sat %in% c(3, 4) ~ 0L,
      TRUE                 ~ NA_integer_
    )
  )

cat("\n--- Outcome by year (pct satisfied + missing rate) ---\n")
colombia %>%
  group_by(year) %>%
  summarise(
    n            = n(),
    pct_sat      = round(mean(satisfied, na.rm = TRUE) * 100, 1),
    pct_missing  = round(mean(is.na(satisfied)) * 100, 1),
    .groups = "drop"
  ) %>%
  print()

# =============================================================================
# 5. DESIGN VARIABLES (wave1500 + upm + strata + wave-conditional cluster)
# =============================================================================
weight_vec  <- as.numeric(haven::zap_labels(lapop[["weight1500"]][col_idx]))
upm_vec     <- as.numeric(haven::zap_labels(lapop[["upm"]][col_idx]))
strata_vec  <- as.character(haven::zap_labels(lapop[["strata"]][col_idx]))
# cluster absent in 2021: fall back to upm for that wave.
cluster_raw <- haven::zap_labels(lapop[["cluster"]][col_idx])
cluster_vec <- as.numeric(
  if_else(yr_num == 2021, upm_vec, as.numeric(cluster_raw))
)

colombia <- colombia %>%
  mutate(
    weight  = weight_vec,
    psu     = upm_vec,
    strata  = strata_vec,
    cluster = cluster_vec
  )

# =============================================================================
# 6. ANALYSIS-READY DATASET
# =============================================================================
lapop_col <- colombia %>%
  transmute(
    year,
    region     = region,
    age_group,
    ethnicity  = ethnicity4,
    urbanicity = urbanicity3,
    sex,
    weight,
    psu,
    cluster,
    strata,
    satisfied,
    dem_sat,
    pres_app
  ) %>%
  filter(!is.na(region))    # drop the ~19 truly unknown 2021 respondents

cat("\n--- Final dataset: rows, years, region NA check ---\n")
cat("Rows: ", nrow(lapop_col),
    " | Years: ", n_distinct(lapop_col$year),
    " | Region NA: ", sum(is.na(lapop_col$region)), "\n", sep = "")

# =============================================================================
# 7. COVERAGE TABLE
# =============================================================================
cat("\n--- Year x region cell sizes (n with valid outcome) ---\n")
lapop_col %>%
  filter(!is.na(satisfied)) %>%
  count(year, region) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0L) %>%
  print(n = Inf)

cat("\n--- Full cell coverage (year x region x age x eth x urb) ---\n")
lapop_col %>%
  filter(!is.na(satisfied)) %>%
  count(year, region, age_group, ethnicity, urbanicity, name = "n_cell") %>%
  summarise(
    total_cells = n(),
    n_covered   = sum(n_cell >= 5),
    n_sparse    = sum(n_cell > 0 & n_cell < 5),
    n_denied    = sum(n_cell == 0),
    .groups = "drop"
  ) %>%
  print()

# =============================================================================
# 8. SURVEY DESIGN OBJECT
# =============================================================================
options(survey.lonely.psu = "adjust")

lapop_svy <- lapop_col %>% filter(!is.na(weight))

svy_design <- svydesign(
  ids     = ~ psu,
  strata  = ~ strata,
  weights = ~ weight,
  data    = lapop_svy,
  nest    = TRUE
)

cat("\n--- National weighted satisfaction rate by year ---\n")
map(sort(unique(lapop_svy$year)), function(yr) {
  sub_svy <- subset(svy_design, year == yr & !is.na(satisfied))
  est     <- tryCatch(svymean(~ satisfied, sub_svy, na.rm = TRUE),
                      error = function(e) NULL)
  if (is.null(est))
    return(tibble(year = yr, estimate = NA_real_, se = NA_real_))
  tibble(year     = yr,
         estimate = round(as.numeric(coef(est))[[1]], 3),
         se       = round(as.numeric(SE(est))[[1]],   3))
}) %>%
  list_rbind() %>%
  print()

# =============================================================================
# 9. PERSIST TO DISK
# =============================================================================
save_result(lapop_col,  "lapop_col")
save_result(svy_design, "svy_design")
cat("\nSaved lapop_col and svy_design to", results_dir, "\n")

# =============================================================================
# KEY VALIDATION CHECKS BEFORE PROCEEDING:
#   * Section 2: etid raw codes match the collapse mapping (check the printout).
#   * Section 3: tamano/ur1new coding direction confirmed (1=City, 5=Rural).
#   * Section 8: national satisfaction rates track the Colombian political
#     calendar -- high 2004-2012, drop at 2014, lower 2018, modest recovery 2023.
#   * 2021 should now appear in the year table if region was fixed correctly.
#   * 2023 will show no Suburb in urbanicity -- correct by design (ur is binary).
# =============================================================================
