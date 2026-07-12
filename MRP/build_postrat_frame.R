# =============================================================================
# build_postrat_frame.R
# Constructs the MRP poststratification frame:
#   N(dept, age, ethnicity, urbanicity, year) for all cells.
#
# THREE-SOURCE DEMOGRAPHIC STRATEGY (stated explicitly for methods section):
#   Age:        National shares applied uniformly to all departments.
#   Ethnicity:  Department-specific shares from DANE 2018 census knowledge,
#               national defaults for non-distinctive departments.
#   Urbanicity: Department-level shares estimated from the pooled LAPOP sample
#               (all waves), weighted. Never-sampled departments fall back to a
#               uniform 1/3 split (flagged).
#
# INDEPENDENCE ASSUMPTION: within a department, age, ethnicity, and urbanicity
#   are assumed independent (standard MRP postrat approximation). The model's
#   interaction random effects partially correct departures from it.
#
# JOIN DESIGN: the FULL 5-way grid is built first, then each share table is
#   joined many-to-one on its proper keys (character, to avoid factor-level
#   mismatches). This is the fix for the earlier many-to-many cross-join.
#
# CODING STANDARDS: no for loops (purrr map); tidyverse loaded last.
# =============================================================================

library(tidyverse)

source("00_config.R")

load_if_missing("lapop_col")
load_if_missing("dept_pop")

all_depts <- dept_pop$department

# =============================================================================
# 1. AGE SHARES (national; character key for joining)
# =============================================================================
age_share_tbl <- tibble(
  age_group = age_levels,                         # character
  age_share = c(0.185, 0.230, 0.285, 0.190, 0.110)
) %>%
  mutate(age_share = age_share / sum(age_share))

cat("Age shares (national):\n"); print(age_share_tbl)

# =============================================================================
# 2. ETHNICITY SHARES BY DEPARTMENT (DANE 2018; character keys)
# =============================================================================
eth_national <- c(`Mestizo/White` = 0.870, `Afro-Colombian` = 0.068,
                  `Indigenous`    = 0.044, `Other`          = 0.018)

eth_overrides <- tribble(
  ~department,           ~`Mestizo/White`, ~`Afro-Colombian`, ~`Indigenous`, ~`Other`,
  "Choco",               0.060,            0.820,             0.110,         0.010,
  "La Guajira",          0.480,            0.080,             0.430,         0.010,
  "Cauca",               0.570,            0.220,             0.200,         0.010,
  "Narino",              0.640,            0.180,             0.170,         0.010,
  "Bolivar",             0.730,            0.250,             0.010,         0.010,
  "Cordoba",             0.870,            0.100,             0.020,         0.010,
  "Atlantico",           0.890,            0.100,             0.005,         0.005,
  "Valle del Cauca",     0.770,            0.220,             0.005,         0.005,
  "San Andres",          0.430,            0.560,             0.005,         0.005,
  "Vichada",             0.540,            0.020,             0.430,         0.010,
  "Guainia",             0.330,            0.020,             0.640,         0.010,
  "Vaupes",              0.320,            0.020,             0.650,         0.010,
  "Putumayo",            0.770,            0.020,             0.200,         0.010,
  "Caqueta",             0.880,            0.020,             0.090,         0.010,
  "Guaviare",            0.890,            0.020,             0.080,         0.010,
  "Arauca",              0.940,            0.020,             0.035,         0.005,
  "Sucre",               0.780,            0.190,             0.020,         0.010,
  "Magdalena",           0.820,            0.160,             0.015,         0.005
)

eth_shares <- map(all_depts, function(d) {
  if (d %in% eth_overrides$department) {
    row    <- eth_overrides %>% filter(department == d)
    shares <- c(row$`Mestizo/White`, row$`Afro-Colombian`,
                row$Indigenous, row$Other)
  } else {
    shares <- as.numeric(eth_national)
  }
  shares <- shares / sum(shares)
  tibble(department = d,
         ethnicity  = c("Mestizo/White", "Afro-Colombian",
                        "Indigenous", "Other"),       # character
         eth_share  = shares)
}) %>%
  list_rbind()

cat("\n--- Ethnicity shares (distinctive departments) ---\n")
eth_shares %>%
  filter(department %in% c("Choco", "La Guajira", "Bogota D.C.", "Narino")) %>%
  print(n = Inf)

# =============================================================================
# 3. URBANICITY SHARES BY DEPARTMENT (pooled LAPOP, weighted; character keys)
# =============================================================================
urb_from_lapop <- lapop_col %>%
  filter(!is.na(urbanicity), !is.na(weight), !is.na(region)) %>%
  mutate(department = as.character(region),
         urbanicity = as.character(urbanicity)) %>%
  group_by(department, urbanicity) %>%
  summarise(wt_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(department) %>%
  mutate(urb_share = wt_n / sum(wt_n)) %>%
  ungroup() %>%
  select(department, urbanicity, urb_share)

cat("\n--- urb_from_lapop: rows and sample (confirm it is NOT empty) ---\n")
cat("rows:", nrow(urb_from_lapop), "\n")
print(head(urb_from_lapop, 12))

urb_full <- expand_grid(
  department = all_depts,
  urbanicity = urb_levels                     # character
) %>%
  left_join(urb_from_lapop, by = c("department", "urbanicity")) %>%
  mutate(urb_share = replace_na(urb_share, 0.01)) %>%
  group_by(department) %>%
  mutate(urb_share = urb_share / sum(urb_share)) %>%
  ungroup()

# How many departments got REAL (non-floor) urbanicity shares?
n_real_urb <- urb_full %>%
  group_by(department) %>%
  summarise(is_real = any(abs(urb_share - 1/3) > 1e-6), .groups = "drop") %>%
  summarise(n = sum(is_real)) %>%
  pull(n)
cat("\nDepartments with real (non-floor) urbanicity shares:", n_real_urb,
    "of", length(all_depts), "\n")

cat("\n--- Urbanicity shares (sample) ---\n")
urb_full %>%
  filter(department %in% c("Bogota D.C.", "Choco", "Antioquia",
                            "La Guajira", "Amazonas")) %>%
  arrange(department, match(urbanicity, urb_levels)) %>%
  print(n = Inf)

# =============================================================================
# 4. DEPARTMENT POPULATION BY YEAR (linear interpolation between DANE anchors)
# =============================================================================
dept_pop_byyear <- dept_pop %>%
  select(department, pop_2005, pop_2018, pop_2020) %>%
  expand_grid(year = all_years) %>%
  mutate(
    pop_year = case_when(
      year <= 2018 ~ pop_2005 +
        (pop_2018 - pop_2005) / (2018 - 2005) * (year - 2005),
      year <= 2020 ~ pop_2018 +
        (pop_2020 - pop_2018) / (2020 - 2018) * (year - 2018),
      TRUE         ~ pop_2020 +
        (pop_2020 - pop_2018) / (2020 - 2018) * (year - 2020)
    ),
    pop_year = pmax(round(pop_year), 1L)
  ) %>%
  select(department, year, pop_year)

# =============================================================================
# 5. BUILD THE FULL POSTSTRATIFICATION GRID (full 5-way grid, then join shares)
# =============================================================================
postrat_frame <- expand_grid(
  department = all_depts,
  year       = all_years,
  age_group  = age_levels,
  ethnicity  = eth_levels,
  urbanicity = urb_levels
) %>%
  left_join(age_share_tbl,   by = "age_group") %>%
  left_join(eth_shares,      by = c("department", "ethnicity")) %>%
  left_join(urb_full,        by = c("department", "urbanicity")) %>%
  left_join(dept_pop_byyear, by = c("department", "year")) %>%
  mutate(
    cell_share = age_share * eth_share * urb_share,
    N_cell     = round(pop_year * cell_share),
    age_group  = factor(age_group,  levels = age_levels),
    ethnicity  = factor(ethnicity,  levels = eth_levels),
    urbanicity = factor(urbanicity, levels = urb_levels)
  ) %>%
  select(year, department, age_group, ethnicity, urbanicity,
         pop_year, cell_share, N_cell)

# =============================================================================
# 6. VALIDATION DIAGNOSTICS
# =============================================================================
cat("\n--- Postrat frame dimensions ---\n")
cat("Rows: ", nrow(postrat_frame), "\n",
    "Expected: ", length(all_depts), " depts x ",
    length(all_years), " years x 5 x 4 x 3 = ",
    length(all_depts) * length(all_years) * 5 * 4 * 3, "\n", sep = "")

cat("\n--- Cell shares sum to 1 within dept-year (max deviation) ---\n")
postrat_frame %>%
  group_by(year, department) %>%
  summarise(total_share = sum(cell_share), .groups = "drop") %>%
  summarise(max_dev = max(abs(total_share - 1))) %>%
  print()

cat("\n--- N_cell sums to dept population (max abs rounding error) ---\n")
postrat_frame %>%
  group_by(year, department) %>%
  summarise(n_sum = sum(N_cell), pop_year = first(pop_year), .groups = "drop") %>%
  summarise(max_abs_err = max(abs(n_sum - pop_year))) %>%
  print()

cat("\n--- National N_cell total by year (should track Colombian pop ~38-52M) ---\n")
postrat_frame %>%
  group_by(year) %>%
  summarise(total_pop = sum(N_cell), .groups = "drop") %>%
  print(n = Inf)

# Coverage: join LAPOP observation counts onto the grid (character keys).
lapop_coverage <- lapop_col %>%
  filter(!is.na(satisfied)) %>%
  transmute(
    year       = as.integer(year),
    department = as.character(region),
    age_group  = as.character(age_group),
    ethnicity  = as.character(ethnicity),
    urbanicity = as.character(urbanicity)
  ) %>%
  count(year, department, age_group, ethnicity, urbanicity, name = "n_obs")

postrat_coverage <- postrat_frame %>%
  mutate(
    year       = as.integer(year),
    age_group  = as.character(age_group),
    ethnicity  = as.character(ethnicity),
    urbanicity = as.character(urbanicity)
  ) %>%
  left_join(lapop_coverage,
            by = c("year", "department", "age_group", "ethnicity", "urbanicity")) %>%
  mutate(
    n_obs = replace_na(n_obs, 0L),
    tier  = case_when(n_obs >= 5 ~ "covered",
                      n_obs > 0  ~ "sparse",
                      TRUE       ~ "denied"),
    tier  = factor(tier, levels = c("covered", "sparse", "denied"))
  )

cat("\n--- Postrat cell coverage tiers (should NO LONGER be 100% denied) ---\n")
postrat_coverage %>%
  count(tier) %>%
  mutate(share = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n--- Coverage tiers by year ---\n")
postrat_coverage %>%
  count(year, tier) %>%
  pivot_wider(names_from = tier, values_from = n, values_fill = 0L) %>%
  print(n = Inf)

# =============================================================================
# 7. PERSIST TO DISK
# =============================================================================
save_result(postrat_frame,    "postrat_frame")
save_result(postrat_coverage, "postrat_coverage")
save_result(eth_shares,       "eth_shares")
save_result(urb_full,         "urb_shares")
cat("\nSaved postrat_frame, postrat_coverage, eth_shares, urb_shares to",
    results_dir, "\n")

# =============================================================================
# OBJECTS PRODUCED:
#   postrat_frame    - full year x dept x age x eth x urb grid with N_cell
#   postrat_coverage - same + n_obs from LAPOP + coverage tier
#   eth_shares       - dept-level ethnicity shares
#   urb_shares       - dept-level urbanicity shares (LAPOP-estimated)
#
# RE-RUN ORDER: read_lapop_colombia.R -> recode_lapop_colombia.R ->
#   build_postrat_frame.R  (so lapop_col reflects the current recode).
#
# NEXT: benchmark_mrp.R -- fit brms MRP on lapop_col, poststratify to
#   postrat_frame; leave-one-wave-out validation against observed estimates.
# =============================================================================
