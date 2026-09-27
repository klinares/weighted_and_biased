# eval_rag.R -- retrieval and answer accuracy, on the app's own code path.
#
#   source("eval/eval_rag.R"); ev <- run_eval()      # from the app folder
#
# LLM calls: ~120 to write questions + 2 x 40 answers + 80 grades = ~280,
#   all on the worker model except the chat-model answers. Every stage is
#   cached in eval/*.rds; delete a file to redo that stage.

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(stringr); library(tibble) })
options(svymeth.app_dir = normalizePath("."))
invisible(lapply(sort(list.files("R", "\\.R$", full.names = TRUE)), source))

EV <- list(dir = "eval", n_items = c(equation = 45L, concept = 45L, code = 30L),
           n_answer = c(equation = 16L, concept = 14L, code = 10L), seed = 752L)

cache <- function(name, f) {
  p <- file.path(EV$dir, paste0(name, ".rds"))
  if (file.exists(p)) return(readRDS(p))
  x <- f(); saveRDS(x, p); x
}

EQ_RX <- "(?s)\\$\\$[^$]*(=|\\\\frac|\\\\sum|\\\\hat)[^$]*\\$\\$"

sample_items <- function(st) {
  set.seed(EV$seed)
  ch <- DBI::dbGetQuery(st$con, "SELECT id, origin, doc_type, course, text FROM svy_chunks") |>
    as_tibble() |> filter(nchar(text) > 600)
  pick <- function(d, n) d |> slice_sample(prop = 1) |> distinct(origin, .keep_all = TRUE) |>
    group_by(course) |> slice_head(n = 4) |> ungroup() |> slice_sample(n = min(n, nrow(d)))
  eq <- ch |> filter(str_detect(text, EQ_RX), doc_type %in% c("lecture", "notes", "textbook", "assignment", "exam")) |>
    pick(EV$n_items[["equation"]]) |> mutate(kind = "equation")
  co <- ch |> filter(!str_detect(text, fixed("$$")), !str_detect(text, fixed("```")),
                     doc_type %in% c("lecture", "notes", "textbook"), nchar(text) > 800) |>
    anti_join(eq, by = "origin") |> pick(EV$n_items[["concept"]]) |> mutate(kind = "concept")
  cd <- ch |> filter(str_detect(text, "```\\s*(\\{r|r\\b)")) |>
    pick(EV$n_items[["code"]]) |> mutate(kind = "code")
  bind_rows(eq, co, cd) |>
    mutate(item_id = sprintf("%s_%03d", kind, row_number()), .by = kind) |>
    mutate(gold_latex = if_else(kind == "equation",
             str_squish(str_remove_all(str_extract(text, EQ_RX), "^\\$\\$|\\$\\$$")), NA_character_))
}

GEN_SYS <- paste(
  "You write evaluation questions for a retrieval system over graduate survey-methodology",
  "course materials. Given one passage, write ONE question a student might ask that this",
  "passage answers, and a short reference answer. Phrase it naturally; do not copy headings",
  "or distinctive phrases, and do not mention 'the passage' or a course number. If the passage",
  "centres on a formula, ask for it and give it in LaTeX. If it centres on R code, ask how to do",
  "that task in R. Ask about substance, never about file paths, seeds or assignment logistics.")

gen_one <- function(text, kind) {
  ch <- llm_chat("worker", GEN_SYS, temperature = 0.3)
  tryCatch(ch$chat_structured(paste0("Passage type: ", kind, "\n\nPassage:\n", str_trunc(text, 3500)),
    type = ellmer::type_object(question = ellmer::type_string(), answer = ellmer::type_string())),
    error = function(e) list(question = NA_character_, answer = NA_character_))
}

build_gold <- function(st) {
  it <- sample_items(st)
  qa <- map2(it$text, it$kind, gen_one, .progress = "questions")
  it |> mutate(question = map_chr(qa, function(x) x$question %||% NA_character_),
               ref_answer = map_chr(qa, function(x) x$answer %||% NA_character_)) |>
    filter(!is.na(question), nzchar(question))
}

eval_retrieval <- function(st, gold, k = 10L) {
  qv <- embed_texts(gold$question)
  gold |> mutate(
    hits = map(seq_len(n()), function(i) retrieve(st, question[i], k, qvec = qv[i, ]), .progress = "retrieval"),
    rank = map2_int(hits, origin, function(h, o) { i <- match(o, h$origin); if (is.na(i)) NA_integer_ else i }),
    course_hit5 = map2_lgl(hits, course, function(h, c) c %in% head(h$course, 5))) |>
    select(-hits)
}

retrieval_summary <- function(r) {
  s <- function(d) summarise(d, n = n(),
    `recall@1` = mean(!is.na(rank) & rank <= 1), `recall@3` = mean(!is.na(rank) & rank <= 3),
    `recall@5` = mean(!is.na(rank) & rank <= 5), `recall@10` = mean(!is.na(rank)),
    MRR = mean(if_else(is.na(rank), 0, 1 / rank)), course_hit5 = mean(course_hit5))
  bind_rows(r |> group_by(kind) |> s(), r |> s() |> mutate(kind = "ALL")) |>
    mutate(across(where(is.double), function(x) round(x, 3)))
}

run_answers <- function(st, sub, role) {
  res <- map(sub$question, function(q) {
    ch <- llm_chat(role, svy_system_prompt())
    tryCatch(ask_methodologist(ch, st, q),
             error = function(e) list(answer = paste("ERROR:", conditionMessage(e)), hits = tibble(), secs = NA_real_))
  }, .progress = role)
  sub |> select(item_id, kind, origin, question, ref_answer, gold_latex, text) |>
    mutate(role = !!role, model = !!svy_profile()[[role]],
           answer = map_chr(res, "answer"), secs = map_dbl(res, "secs"),
           gold_in_ctx = map2_lgl(res, origin, function(x, o) o %in% x$hits$origin),
           audit = map(res, function(x) if (nrow(x$hits)) audit_answer(x$answer, x$hits) else NULL),
           n_eq = map_int(audit, function(a) a$n_eq %||% 0L),
           eq_unsourced = map_int(audit, function(a) length(a$eq_unsourced)),
           bad_cite = map_int(audit, function(a) length(a$bad_cite)),
           cites = map_lgl(audit, function(a) length(a$cited) > 0),
           math_ok = map_dbl(answer, function(a) attr(render_markdown(a), "math_ok"))) |>
    select(-audit)
}

JUDGE_SYS <- paste(
  "You grade answers from a course-notes assistant against a reference passage and answer.",
  "correct: 2 = fully correct and complete, 1 = partly correct or missing key parts, 0 = wrong",
  "or declines. equation_correct: if an EXPECTED FORMULA is given, 1 if the answer states a",
  "mathematically equivalent formula (notation may differ), else 0; -1 if none is given.",
  "unsupported: 1 if the answer asserts substantive facts the reference does not support.")

judge_one <- function(question, text, ref_answer, gold_latex, answer) {
  ch <- llm_chat("worker", JUDGE_SYS, temperature = 0)
  tryCatch(ch$chat_structured(paste0(
    "QUESTION:\n", question, "\n\nREFERENCE PASSAGE:\n", str_trunc(text, 3500),
    "\n\nREFERENCE ANSWER:\n", ref_answer, "\n\nEXPECTED FORMULA:\n", coalesce(gold_latex, "(none)"),
    "\n\nANSWER TO GRADE:\n", str_trunc(answer, 5000)),
    type = ellmer::type_object(correct = ellmer::type_integer(), equation_correct = ellmer::type_integer(),
                               unsupported = ellmer::type_integer(), note = ellmer::type_string())),
    error = function(e) list(correct = NA, equation_correct = NA, unsupported = NA, note = conditionMessage(e)))
}

run_eval <- function(roles = c("chat", "worker")) {
  t0 <- Sys.time()
  st <- open_store(); on.exit(close_store(st), add = TRUE); check_store_matches(st)
  gold <- cache("gold", function() build_gold(st))
  retr <- cache("retrieval", function() eval_retrieval(st, gold))
  rs <- retrieval_summary(retr); readr::write_csv(rs, file.path(EV$dir, "summary_retrieval.csv"))
  message("\nRETRIEVAL"); print(rs)

  set.seed(EV$seed)
  sub <- imap(EV$n_answer, function(n, kd) gold |> filter(kind == kd) |> slice_sample(n = n)) |> list_rbind()
  ans <- map(roles, function(r) cache(paste0("answers_", r), function() run_answers(st, sub, r))) |> list_rbind()
  jd <- cache("judged", function() {
    j <- pmap(select(ans, question, text, ref_answer, gold_latex, answer), judge_one, .progress = "judge")
    ans |> mutate(correct = map_int(j, function(x) as.integer(x$correct %||% NA)),
                  equation_correct = na_if(map_int(j, function(x) as.integer(x$equation_correct %||% NA)), -1L),
                  unsupported = map_int(j, function(x) as.integer(x$unsupported %||% NA)),
                  judge_note = map_chr(j, function(x) as.character(x$note %||% "")))
  })
  sm <- function(d) summarise(d, n = n(), score = mean(correct, na.rm = TRUE) / 2,
    fully_correct = mean(correct == 2, na.rm = TRUE), equation_correct = mean(equation_correct, na.rm = TRUE),
    eq_flagged_by_audit = sum(eq_unsourced), bad_citations = sum(bad_cite),
    gold_in_context = mean(gold_in_ctx), unsupported = mean(unsupported, na.rm = TRUE),
    cites = mean(cites), math_render_ok = mean(math_ok, na.rm = TRUE), median_secs = median(secs, na.rm = TRUE))
  as_ <- bind_rows(jd |> group_by(model, kind) |> sm() |> ungroup(),
                   jd |> group_by(model) |> sm() |> ungroup() |> mutate(kind = "ALL")) |>
    mutate(across(where(is.double), function(x) round(x, 3)))
  readr::write_csv(as_, file.path(EV$dir, "summary_answers.csv"))
  readr::write_csv(select(jd, -text), file.path(EV$dir, "judged_answers.csv"))
  message("\nANSWERS"); print(as_, n = Inf, width = Inf)
  message(sprintf("eval done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(list(gold = gold, retrieval = retr, retrieval_summary = rs, judged = jd, answer_summary = as_))
}
