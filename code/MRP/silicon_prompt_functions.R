# =============================================================================
# silicon_prompt_functions.R
# Production prompt for MRP-anchored TRUE silicon sampling: the LLM simulates
# n_h individual respondents per call (binary satisfied = 1 / not = 0), anchored
# on the MRP cell estimate and the model-based marginals. No rate is requested;
# the respondents themselves are the output (generalizes to ordinal later).
#
# Repeated across m draws, each cell accumulates n_h x m candidate respondents,
# stratified by draw. Design weights (population-structure inclusion + GREG
# calibration) are applied AFTER, in the evaluation -- not here.
#
# CODING STANDARDS: no for loops; pure string building.
# =============================================================================

build_silicon_prompt <- function(department, age_group, ethnicity, urbanicity, year,
                                 mrp_rate, mrp_lo, mrp_hi,
                                 marg_dept, marg_age, marg_eth, marg_urb,
                                 is_survey_year, n_h = 5L) {

  pct   <- function(x) paste0(round(100 * x), "%")
  width <- mrp_hi - mrp_lo

  conf <- if (width > 0.20) {
    paste0("This group-specific model estimate is highly uncertain (a wide ",
           "interval), so weigh the broader patterns below more heavily.")
  } else if (width > 0.10) {
    paste0("This group-specific model estimate carries moderate uncertainty; ",
           "treat it as a reasonable starting point alongside the broader patterns.")
  } else {
    "This group-specific model estimate is relatively precise."
  }

  year_note <- if (!is_survey_year) {
    paste0(" Note: no survey was fielded in ", year,
           ", so the model estimate is interpolated and should be treated with ",
           "extra caution.")
  } else {
    ""
  }

  paste0(
    "You are simulating public-opinion survey respondents in Colombia using your ",
    "knowledge of the country's political and social context.\n\n",

    "TARGET GROUP: ", age_group, " year-old ", ethnicity, " adults living in a ",
    tolower(urbanicity), " area of ", department, ", in ", year, ".\n",
    "QUESTION each respondent answers: are you satisfied with the way democracy ",
    "works in Colombia? (1 = satisfied, 0 = not satisfied)\n\n",

    "STATISTICAL MODEL ANCHOR:\n",
    "A multilevel regression and poststratification (MRP) model, fit to the ",
    "AmericasBarometer series, predicts ", pct(mrp_rate), " of this group are ",
    "satisfied (90% interval ", pct(mrp_lo), " to ", pct(mrp_hi), "). ", conf,
    year_note, "\n\n",

    "BROADER MODEL-BASED PATTERNS for context:\n",
    "- ", department, " overall, ", year, ": ", pct(marg_dept), "\n",
    "- ", ethnicity, " Colombians overall, ", year, ": ", pct(marg_eth), "\n",
    "- Age group ", age_group, " overall, ", year, ": ", pct(marg_age), "\n",
    "- ", urbanicity, " residents overall, ", year, ": ", pct(marg_urb), "\n\n",

    "TASK: Simulate ", n_h, " individual respondents drawn from this exact group, ",
    "consistent with the anchor and patterns above and with what you know about ",
    "Colombia in ", year, ". If you have no specific reason to depart from the ",
    "anchor, let the share of 1s stay close to it. Output 1 for a satisfied ",
    "respondent, 0 otherwise.\n\n",
    "Return ONLY a JSON array of ", n_h, " integers, each 0 or 1, for example ",
    "[1,0,1,1,0]. No other text."
  )
}

# Parse the LLM's JSON array into a 0/1 integer vector (length up to n_h), or
# NULL if nothing parseable is found.
parse_silicon_responses <- function(txt, n_h = 5L) {
  if (length(txt) == 0 || is.na(txt)) return(NULL)
  txt <- gsub("```json|```", "", txt)
  br  <- regmatches(txt, regexpr("\\[[^]]*\\]", txt))      # first [...] block
  if (length(br) == 0) return(NULL)
  toks <- unlist(strsplit(gsub("[^01,]", "", br), ","))
  vals <- suppressWarnings(as.integer(toks))
  vals <- vals[!is.na(vals) & vals %in% c(0L, 1L)]
  if (length(vals) == 0) return(NULL)
  vals[seq_len(min(length(vals), n_h))]
}
