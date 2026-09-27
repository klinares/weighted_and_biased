# =============================================================================
# eval_rag.R -- retrieval + answer accuracy for the coursework RAG
#
#   source("D:/repos/weighted_and_biased/rag_time/course_app/eval_rag.R")
#   ev <- run_eval()            # all stages, cached per stage in eval/
#
# Stages (each cached as .rds so a rerun only redoes what was deleted):
#   1. gold   : sample equation, concept and R-code chunks from the ORIGINAL
#               coursework (no summaries); an LLM writes one student-style
#               question + answer per item. Gold doc = the item's origin file.
#   2. retr   : rank of the gold origin in rag_retrieve() top-10 -> recall@k, MRR
#   3. answers: full ask_rag() for each model on a stratified subset
#   4. judge  : LLM grades correctness (0-2), equation fidelity, grounding
#   5. report : summary tables written to eval/summary_*.csv
# =============================================================================

source("D:/repos/weighted_and_biased/rag_time/course_app/rag_core.R")

EVAL_DIR   <- "D:/repos/weighted_and_biased/rag_time/course_app/eval"
GEN_MODEL  <- "openrouter:meta-llama/llama-4-maverick"   # writes questions
JUDGE_MODEL <- "openrouter:meta-llama/llama-4-maverick"  # grades answers
N_ITEMS    <- c(equation = 45L, concept = 45L, code = 30L)
N_ANSWER   <- c(equation = 16L, concept = 14L, code = 10L)
SEED       <- 752L

dir.create(EVAL_DIR, recursive = TRUE, showWarnings = FALSE)
cache <- function(name, f) {
  p <- file.path(EVAL_DIR, paste0(name, ".rds"))
  if (file.exists(p)) return(readRDS(p))
  x <- f(); saveRDS(x, p); x
}

# --- 1. gold set -------------------------------------------------------------
# Items come only from ORIGINAL sources (lectures, root notes, textbook,
# assignment/exam .qmd/.Rmd, labs, code). Three kinds:
#   equation: chunk with a real display equation ($$...$$ with =, \frac, \sum ...)
#   concept : prose-heavy lecture / notes / textbook chunk
#   code    : chunk with an R code block from assignments / labs / code
sample_items <- function(store) {
  set.seed(SEED)
  ch <- as_tibble(dbGetQuery(store@con, "SELECT origin, text FROM chunks")) |>
    left_join(load_meta(store), by = "origin") |>
    filter(nchar(text) > 600, coalesce(map_dbl(text, garble), 0) < 0.2)
  pick <- function(d, n) d |> group_by(origin) |> slice_sample(n = 1) |> ungroup() |>
    group_by(course_code) |> slice_sample(n = 4) |> ungroup() |>
    slice_sample(n = min(n, nrow(d)))
  eq_rx <- "(?s)\\$\\$[^$]*(=|\\\\frac|\\\\sum|\\\\hat)[^$]*\\$\\$"
  eq <- ch |> filter(str_detect(text, eq_rx),
                     doc_type %in% c("lecture", "notes", "textbook", "assignment_src", "exam_src")) |>
    pick(N_ITEMS[["equation"]]) |> mutate(kind = "equation")
  co <- ch |> filter(!str_detect(text, fixed("$$")), !str_detect(text, fixed("```")),
                     doc_type %in% c("lecture", "notes", "textbook"), nchar(text) > 800) |>
    anti_join(eq, by = "origin") |> pick(N_ITEMS[["concept"]]) |> mutate(kind = "concept")
  cd <- ch |> filter(str_detect(text, "```(\\{r|r\\b)"),
                     doc_type %in% c("assignment_src", "assignment_code", "lab", "code", "exam_src")) |>
    pick(N_ITEMS[["code"]]) |> mutate(kind = "code")
  bind_rows(eq, co, cd) |>
    mutate(item_id = sprintf("%s_%03d", kind, row_number()), .by = kind) |>
    mutate(gold_latex = if_else(kind == "equation",
             str_squish(str_remove_all(str_extract(text, eq_rx), "^\\$\\$|\\$\\$$")),
             NA_character_))
}

GEN_SYS <- str_squish("
  You write evaluation questions for a retrieval system over graduate
  survey-methodology course notes. Given one passage, write ONE question a
  student might ask that this passage answers, plus a short reference answer.
  Phrase the question naturally, as a student would; do NOT copy distinctive
  phrases or the heading verbatim and do not mention 'the passage' or a course
  number. If the passage centres on a formula, ask for that formula (e.g. 'what
  is the formula for ...') and put the formula in LaTeX in the answer. If it
  centres on R code, ask how to do that task in R (which function/arguments)
  and put the key code in the answer. Ask about substance, not about
  assignment logistics, file paths or seeds.
")

gen_question <- function(text, kind) {
  chat <- make_chat(GEN_MODEL, GEN_SYS, temperature = 0.3)
  tryCatch(chat$chat_structured(
    paste0("Passage type: ", kind, "\n\nPassage:\n", str_trunc(text, 3500)),
    type = type_object(question = type_string(), answer = type_string())),
    error = function(e) list(question = NA_character_, answer = NA_character_))
}

build_gold <- function(store) {
  items <- sample_items(store)
  message(nrow(items), " items; generating questions...")
  qa <- map2(items$text, items$kind, gen_question, .progress = TRUE)
  items |> mutate(question = map_chr(qa, \(x) x$question %||% NA_character_),
                  ref_answer = map_chr(qa, \(x) x$answer %||% NA_character_)) |>
    filter(!is.na(question), nzchar(question))
}

# --- 2. retrieval ------------------------------------------------------------
eval_retrieval <- function(store, gold, k = 10L) {
  meta <- load_meta(store)
  tic()
  r <- gold |> mutate(hits = map(question, \(q) rag_retrieve(store, q, top_k = k),
                                 .progress = "retrieval"))
  t <- toc(quiet = TRUE)
  r |> mutate(
    rank = map2_int(hits, origin, \(h, o) { i <- match(o, h$origin); if (is.na(i)) NA_integer_ else i }),
    course_hit5 = map2_lgl(hits, course_code, \(h, c)
      c %in% (meta$course_code[match(head(h$origin, 5), meta$origin)])),
    secs_per_q = unname(t$toc - t$tic) / nrow(gold)) |>
    select(-hits)
}

retrieval_summary <- function(r)
  r |> group_by(kind = coalesce(kind, "all")) |>
    summarise(n = n(),
              `recall@1`  = mean(!is.na(rank) & rank <= 1),
              `recall@3`  = mean(!is.na(rank) & rank <= 3),
              `recall@5`  = mean(!is.na(rank) & rank <= 5),
              `recall@10` = mean(!is.na(rank)),
              MRR         = mean(if_else(is.na(rank), 0, 1 / rank)),
              course_hit5 = mean(course_hit5), .groups = "drop") |>
    bind_rows(r |> summarise(kind = "ALL", n = n(),
              `recall@1` = mean(!is.na(rank) & rank <= 1), `recall@3` = mean(!is.na(rank) & rank <= 3),
              `recall@5` = mean(!is.na(rank) & rank <= 5), `recall@10` = mean(!is.na(rank)),
              MRR = mean(if_else(is.na(rank), 0, 1 / rank)), course_hit5 = mean(course_hit5)))

# --- 3. answers --------------------------------------------------------------
answer_subset <- function(gold) {
  set.seed(SEED)
  imap_dfr(N_ANSWER, \(n, kd) gold |> filter(kind == kd) |> slice_sample(n = n))
}

run_answers <- function(store, sub, model_id) {
  meta <- load_meta(store)
  message("answering with ", model_id)
  res <- map(sub$question, \(q) ask_rag(make_chat(model_id), store, q, meta = meta),
             .progress = model_id)
  sub |> select(item_id, kind, origin, question, ref_answer, gold_latex, text) |>
    mutate(model = model_id,
           answer = map_chr(res, "answer"),
           secs   = map_dbl(res, "secs"),
           error  = map_lgl(res, "error"),
           gold_in_ctx = map2_lgl(res, origin, \(x, o) o %in% x$hits$origin),
           gold_eq_in_panel = map2_lgl(res, gold_latex, \(x, g)
             !is.na(g) && str_squish(g) %in% card_equations(x$hits, 10L)$latex),
           n_cited  = map_int(answer, \(a) length(cited_labels(a))),
           math_ok  = map_dbl(answer, \(a) attr(render_answer(a), "math_ok")),
           n_math   = map_int(answer, \(a) attr(render_answer(a), "n_math")))
}

# --- 4. judge ----------------------------------------------------------------
JUDGE_SYS <- str_squish("
  You grade answers from a course-notes assistant. Compare the ANSWER to the
  REFERENCE passage and reference answer. Score `correct`: 2 = fully correct
  and complete for the question, 1 = partially correct or missing key parts,
  0 = wrong, off-topic, or says it cannot answer. If an EXPECTED FORMULA is
  given, score `equation_correct` 1 only if the answer states a formula that is
  mathematically equivalent (notation may differ), else 0; if no expected
  formula is given, set it to -1. Score `unsupported` 1 if the answer asserts
  substantive facts that the reference does not support, else 0.
")

judge_one <- function(question, text, ref_answer, gold_latex, answer) {
  chat <- make_chat(JUDGE_MODEL, JUDGE_SYS, temperature = 0)
  prompt <- paste0("QUESTION:\n", question,
                   "\n\nREFERENCE PASSAGE:\n", str_trunc(text, 3500),
                   "\n\nREFERENCE ANSWER:\n", ref_answer,
                   "\n\nEXPECTED FORMULA:\n", coalesce(gold_latex, "(none)"),
                   "\n\nANSWER TO GRADE:\n", str_trunc(answer, 4000))
  tryCatch(chat$chat_structured(prompt, type = type_object(
    correct = type_integer(), equation_correct = type_integer(),
    unsupported = type_integer(), note = type_string())),
    error = function(e) list(correct = NA_integer_, equation_correct = NA_integer_,
                             unsupported = NA_integer_, note = conditionMessage(e)))
}

judge_answers <- function(ans) {
  j <- pmap(select(ans, question, text, ref_answer, gold_latex, answer), judge_one,
            .progress = "judge")
  ans |> mutate(correct = map_int(j, \(x) as.integer(x$correct %||% NA)),
                equation_correct = map_int(j, \(x) as.integer(x$equation_correct %||% NA)),
                unsupported = map_int(j, \(x) as.integer(x$unsupported %||% NA)),
                judge_note = map_chr(j, \(x) as.character(x$note %||% "")),
                equation_correct = na_if(equation_correct, -1L))
}

answer_summary <- function(j)
  j |> group_by(model, kind) |>
    summarise(n = n(), score = mean(correct, na.rm = TRUE) / 2,
              fully_correct = mean(correct == 2, na.rm = TRUE),
              equation_correct = mean(equation_correct, na.rm = TRUE),
              eq_in_panel = mean(gold_eq_in_panel[!is.na(gold_latex)]),
              gold_in_context = mean(gold_in_ctx),
              unsupported = mean(unsupported, na.rm = TRUE),
              cites = mean(n_cited > 0), math_render_ok = mean(math_ok, na.rm = TRUE),
              median_secs = median(secs, na.rm = TRUE), errors = sum(error),
              .groups = "drop") |>
    mutate(across(where(is.double), \(x) round(x, 3)))

# --- driver ------------------------------------------------------------------
run_eval <- function(models = unname(MODELS)) {
  tic("eval total")
  store <- open_store()
  on.exit(tryCatch(dbDisconnect(store@con, shutdown = TRUE), error = function(e) NULL), add = TRUE)

  gold <- cache("gold", \() build_gold(store))
  retr <- cache("retrieval", \() eval_retrieval(store, gold))
  rs   <- retrieval_summary(retr)
  readr::write_csv(rs, file.path(EVAL_DIR, "summary_retrieval.csv"))
  message("\nRETRIEVAL"); print(rs)

  sub <- answer_subset(gold)
  ans <- map_dfr(models, \(m) cache(paste0("answers_", slug(m, 40)),
                                    \() run_answers(store, sub, m)))
  jd  <- cache(paste0("judged_", paste(map_chr(models, slug, 20), collapse = "_")),
               \() judge_answers(ans))
  as_ <- answer_summary(jd)
  overall <- jd |> group_by(model) |>
    summarise(kind = "ALL", n = n(), score = mean(correct, na.rm = TRUE) / 2,
              fully_correct = mean(correct == 2, na.rm = TRUE),
              equation_correct = mean(equation_correct, na.rm = TRUE),
              eq_in_panel = mean(gold_eq_in_panel[!is.na(gold_latex)]),
              gold_in_context = mean(gold_in_ctx), unsupported = mean(unsupported, na.rm = TRUE),
              cites = mean(n_cited > 0), math_render_ok = mean(math_ok, na.rm = TRUE),
              median_secs = median(secs, na.rm = TRUE), errors = sum(error)) |>
    mutate(across(where(is.double), \(x) round(x, 3)))
  as_ <- bind_rows(as_, overall)
  readr::write_csv(as_, file.path(EVAL_DIR, "summary_answers.csv"))
  readr::write_csv(select(jd, -text), file.path(EVAL_DIR, "judged_answers.csv"))
  message("\nANSWERS"); print(as_, n = Inf, width = Inf)
  toc()
  invisible(list(gold = gold, retrieval = retr, retrieval_summary = rs,
                 judged = jd, answer_summary = as_))
}
