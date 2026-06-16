# =============================================================================
# read_lapop_colombia.R
# Read the merged LAPOP SPSS file and produce a clean Colombia subset.
#
# WHAT THIS SCRIPT DOES -------------------------------------------------------
#   1. Reads the merged .sav file via haven.
#   2. Prints a variable dictionary and keyword searches for confirmation.
#   3. Builds TWO lookup tables:
#        dept_lookup : numeric prov/prov1t codes -> harmonized department name
#        dept_pop    : department name -> population counts (2005, 2018, 2020)
#      These are the poststratification frame for the MRP downstream.
#   4. Subsets to Colombia, unifying region across waves via the lookup:
#        prov   -> all waves except 2021 (numeric codes 805-897)
#        prov1t -> 2021 (numeric codes 800001-800031)
#   5. Applies consistent type conversions and produces `colombia`.
#
# CODING STANDARDS: no for loops (purrr map); tidyverse loaded last.
# =============================================================================

library(haven)
library(tidyverse)

source("00_config.R")

# =============================================================================
# 1. LOCATE THE FILE
# =============================================================================
data_dir  <- "D:/repos/weighted_and_biased/code/MRP/data"
list.files(data_dir)
data_path <- file.path(data_dir, "lapop_merged.sav")

# =============================================================================
# 2. READ
# =============================================================================
lapop <- read_sav(data_path)
cat("Rows x Cols: ", nrow(lapop), " x ", ncol(lapop), "\n", sep = "")

# =============================================================================
# 3. VARIABLE DICTIONARY
# =============================================================================
var_dict <- tibble(
  variable = names(lapop),
  label    = map_chr(lapop, function(x) {
    lab <- attr(x, "label")
    if (length(lab) == 0) NA_character_ else paste(format(lab), collapse = " ")
  })
)
print(var_dict, n = Inf)

# =============================================================================
# 4. KEYWORD SEARCHES
# =============================================================================
find_var <- function(pattern) {
  var_dict %>%
    filter(str_detect(str_to_lower(coalesce(label, "")), pattern) |
             str_detect(str_to_lower(variable), pattern))
}
find_var("countr|pais|naci")
find_var("year|wave|ola|anho|ano|round")
find_var("depart|provin|region|prov")
find_var("edad|^age|q2")
find_var("sexo|gender|^sex|q1")
find_var("etni|raza|color|ethni|etid")
find_var("urban|rural|tama|size|ur1new")
find_var("democ|pn4")
find_var("presiden|aprob|approv|desemp|trabajo|m1")
find_var("weight|ponder|peso|^wt|wt$|weight1500")
find_var("upm|psu|cluster|conglom|estrato|strata")

# =============================================================================
# 5. CONFIRMED VARIABLE NAMES
# =============================================================================
v_country <- "pais"
v_year    <- "year"
v_wave    <- "wave"
v_region  <- "prov"
v_reg21   <- "prov1t"
v_strata  <- "strata"
v_psu     <- "upm"
v_weight  <- "weight1500"
v_age     <- "q2"
v_sex     <- "q1"
v_eth     <- "etid"
v_urban   <- "ur"
v_ur1new  <- "ur1new"
v_size    <- "tamano"
v_demsat  <- "pn4"
v_presapp <- "m1"

colombia_code <- 8L

# =============================================================================
# 6. DEPARTMENT LOOKUP TABLES
# =============================================================================
# Two code series map to the same harmonized department names:
#   prov   (non-2021): 3-digit LAPOP numeric codes (805-897)
#   prov1t (2021):     6-digit numeric codes (800001-800031)
# Harmonized names use ASCII (no accents) for consistent joins downstream.

dept_lookup <- tribble(
  ~code,    ~department,
  # prov codes (non-2021 waves)
  805,      "Antioquia",
  808,      "Atlantico",
  811,      "Bogota D.C.",
  813,      "Bolivar",
  815,      "Boyaca",
  817,      "Caldas",
  818,      "Caqueta",
  819,      "Cauca",
  820,      "Cesar",
  823,      "Cordoba",
  825,      "Cundinamarca",
  841,      "Huila",
  844,      "La Guajira",
  847,      "Magdalena",
  850,      "Meta",
  852,      "Narino",
  854,      "Norte de Santander",
  863,      "Quindio",
  866,      "Risaralda",
  868,      "Santander",
  870,      "Sucre",
  873,      "Tolima",
  876,      "Valle del Cauca",
  885,      "Casanare",
  886,      "Putumayo",
  897,      "Vaupes",
  # prov1t codes (2021 wave)
  800001,   "Antioquia",
  800002,   "Atlantico",
  800003,   "Bogota D.C.",
  800004,   "Bolivar",
  800005,   "Boyaca",
  800006,   "Caldas",
  800007,   "Caqueta",
  800008,   "Cauca",
  800009,   "Cesar",
  800010,   "Cordoba",
  800011,   "Cundinamarca",
  800012,   "Choco",
  800013,   "Huila",
  800014,   "La Guajira",
  800015,   "Magdalena",
  800016,   "Meta",
  800017,   "Narino",
  800018,   "Norte de Santander",
  800019,   "Quindio",
  800020,   "Risaralda",
  800021,   "Santander",
  800022,   "Sucre",
  800023,   "Tolima",
  800024,   "Valle del Cauca",
  800025,   "Arauca",
  800026,   "Casanare",
  800027,   "Putumayo",
  800028,   "San Andres",
  800030,   "Guainia",
  800031,   "Guaviare",
  800032,   "Vaupes",
  800033,   "Vichada"
)

# Department population counts (DANE census / projections).
# 2020 used for main poststratification; 2005 and 2018 available for
# sensitivity or wave-specific interpolation.
dept_pop <- tribble(
  ~department,             ~pop_2005,  ~pop_2018,  ~pop_2020,
  "Bogota D.C.",           6778691,    7412566,    7743955,
  "Antioquia",             5601507,    6407102,    6677930,
  "Valle del Cauca",       4052535,    4475886,    4532152,
  "Cundinamarca",          2228682,    2919060,    3242999,
  "Atlantico",             2112001,    2535517,    2722128,
  "Santander",             1913444,    2184837,    2280908,
  "Bolivar",               1836640,    2070110,    2180976,
  "Cordoba",               1462909,    1784783,    1828947,
  "Cauca",                 1182022,    1464488,    1491937,
  "Narino",                1498234,    1630592,    1627589,
  "Norte de Santander",    1208336,    1491689,    1620318,
  "Cesar",                  878437,    1200574,    1295387,
  "Magdalena",             1136819,    1341746,    1427026,
  "Boyaca",                1255311,    1217376,    1242731,
  "Tolima",                1312304,    1330187,    1339998,
  "Meta",                   713772,    1039722,    1063454,
  "Huila",                 1001476,    1100386,    1122622,
  "Caldas",                 898490,     998255,    1018453,
  "La Guajira",             655943,     880560,     965718,
  "Risaralda",              859666,     943401,     961055,
  "Sucre",                  762263,     904863,     949252,
  "Quindio",                518691,     539904,     555401,
  "Choco",                  388476,     534826,     544764,
  "Caqueta",                337932,     401849,     410521,
  "Casanare",               281294,     420504,     435195,
  "Putumayo",               237197,     348182,     359127,
  "Arauca",                 153028,     262174,     294206,
  "Guaviare",                56758,      82767,      86657,
  "San Andres",              59573,      61280,      63692,
  "Amazonas",                46950,      76589,      79020,
  "Vichada",                 44592,     107808,     112958,
  "Vaupes",                  19943,      40797,      44712,
  "Guainia",                 18797,      48114,      50636
) %>%
  mutate(dept_share_2020 = pop_2020 / sum(pop_2020))

# =============================================================================
# 7. SUBSET TO COLOMBIA WITH LOOKUP-BASED REGION HARMONIZATION
# =============================================================================
var_map <- c(
  year      = v_year,
  region    = v_region,
  region_21 = v_reg21,
  strata    = v_strata,
  psu       = v_psu,
  weight    = v_weight,
  age       = v_age,
  sex       = v_sex,
  ethnicity = v_eth,
  urban2    = v_urban,
  ur1new    = v_ur1new,
  size5     = v_size,
  dem_sat   = v_demsat,
  pres_app  = v_presapp
)
present <- var_map[var_map %in% names(lapop)]
absent  <- setdiff(names(var_map), names(present))
if (length(absent) > 0)
  message("Variables not found: ", paste(absent, collapse = ", "))

colombia_raw <- lapop %>%
  filter(haven::zap_labels(.data[[v_country]]) == colombia_code) %>%
  select(all_of(present))

# Build region vector outside mutate: extract numeric codes from both sources,
# choose the right one by wave, then look up the harmonized department name.
yr_vec     <- as.integer(haven::zap_labels(colombia_raw[["year"]]))
prov_num   <- as.numeric(haven::zap_labels(colombia_raw[["region"]]))
prov1t_num <- if ("region_21" %in% names(colombia_raw)) {
  as.numeric(haven::zap_labels(colombia_raw[["region_21"]]))
} else rep(NA_real_, nrow(colombia_raw))

code_vec <- if_else(yr_vec == 2021, prov1t_num, prov_num)

region_unified <- dept_lookup$department[
  match(code_vec, dept_lookup$code)
]

# Report any codes not found in the lookup (should be NA only).
n_unmatched <- sum(is.na(region_unified) & !is.na(code_vec))
if (n_unmatched > 0)
  message(n_unmatched, " rows had a region code not in dept_lookup -- ",
          "add them to Section 6 if they are real departments.")

colombia <- colombia_raw %>%
  mutate(
    region = factor(region_unified, levels = sort(unique(dept_lookup$department))),
    across(any_of(c("strata", "sex", "ethnicity", "urban2", "ur1new", "size5")),
           function(x) haven::as_factor(x)),
    across(any_of("year"),
           function(x) as.integer(haven::zap_labels(x))),
    across(any_of(c("weight", "age", "psu", "dem_sat", "pres_app")),
           function(x) as.numeric(haven::zap_labels(x)))
  ) %>%
  select(-any_of("region_21"))

cat("Colombia rows: ", nrow(colombia), "\n", sep = "")

# =============================================================================
# 8. COVERAGE CHECK
# =============================================================================
colombia %>% count(year)

cat("\n--- Year x department (harmonized names) ---\n")
colombia %>%
  count(year, region) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0L) %>%
  print(n = Inf)

# =============================================================================
# 9. OUTCOME CODING CHECK
# =============================================================================
cat("\n--- dem_sat raw ---\n"); colombia %>% count(dem_sat)
cat("\n--- pres_app raw ---\n"); colombia %>% count(pres_app)
cat("\n--- Region NA check (should be <20, from the 19 genuine unknowns) ---\n")
colombia %>% filter(year == 2021) %>% count(is.na(region))

# =============================================================================
# 10. SAVE LOOKUP AND POPULATION FRAME
# =============================================================================
save_result(dept_lookup, "dept_lookup")
save_result(dept_pop,    "dept_pop")
cat("\nSaved dept_lookup and dept_pop to", results_dir, "\n")
