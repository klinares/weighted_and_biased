# =============================================================================
# rag_core.R -- shared by app.R and eval_rag.R, so the eval scores exactly the
# code path the app runs.
#
#   source("D:/repos/weighted_and_biased/rag_time/course_app/rag_core.R")
#
# Retrieval comes from rag.R (RRF-fused rag_retrieve); this file adds the model
# layer, prompt/context building, math rendering and source links.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(stringr); library(tibble)
  library(DBI); library(ragnar); library(ellmer); library(katex); library(tictoc)
})

RAG_R <- "D:/repos/rag_time/code/rag.R"
source(RAG_R)                      # STORE_PATH, embed_fn, rag_retrieve(), ...

REPO_URL  <- "https://github.com/klinares/UMD_JPSM_coursework"
TOP_K     <- 8L                    # chunks handed to the model
CTX_CHARS <- 1600L                 # per-chunk cap keeps prompts < ~4k tokens
NUM_CTX   <- 8192L                 # Ollama context; confirm with `ollama ps`

# Ollama's OpenAI-compatible endpoint (what ellmer uses) IGNORES num_ctx, so
# gemma loaded with its 262k default: 9.5 GB, 4.2 GB on GPU, ~140 s/answer.
# A derived model with num_ctx baked in fixes it (8.5 GB, 6 GB on GPU).
LOCAL_BASE <- "gemma4:12b-it-qat"
LOCAL_RAG  <- "gemma4-12b-rag"
ensure_local_model <- function(base = LOCAL_BASE, name = LOCAL_RAG, num_ctx = NUM_CTX) {
  have <- tryCatch(map_chr(httr2::resp_body_json(httr2::req_perform(
    httr2::request("http://localhost:11434/api/tags")))$models, "name"), error = function(e) character())
  if (!any(str_detect(have, paste0("^", name)))) {
    message("creating ", name, " (", base, ", num_ctx = ", num_ctx, ")")
    httr2::request("http://localhost:11434/api/create") |>
      httr2::req_body_json(list(model = name, from = base,
                                parameters = list(num_ctx = num_ctx), stream = FALSE)) |>
      httr2::req_perform()
  }
  invisible(name)
}
try(ensure_local_model(), silent = TRUE)

# Both fit the 8GB-VRAM rule (local) or run remotely (OpenRouter).
MODELS <- c(
  "Local: gemma4 12b (Ollama)"     = "ollama:gemma4-12b-rag",
  "OpenRouter: llama-4-scout"      = "openrouter:meta-llama/llama-4-scout"
)

SYSTEM_PROMPT <- str_squish("
  You are a survey-methodology study assistant for the UMD/Michigan JPSM
  program. Answer ONLY from the retrieved course materials in the user
  message; if they do not contain the answer, say so plainly instead of
  guessing. Cite the sources you use with their bracket labels, e.g. [S2].
  Prefer the textbook, notes and assignment/exam sources for formulas; lecture
  slide text can be fragmentary.
  Math: wrap EVERY symbol or expression in $...$ (inline) or $$...$$ (display).
  Copy formulas from the materials exactly, keeping the LaTeX commands
  (\\frac, \\sum, \\hat, subscripts) -- never Unicode math glyphs.
  Be concise: a direct answer first, then the formula, then what each symbol
  means.
")

# -----------------------------------------------------------------------------
# Models
# -----------------------------------------------------------------------------
make_chat <- function(model_id, system_prompt = SYSTEM_PROMPT, temperature = 0.2) {
  prov <- str_extract(model_id, "^[^:]+")
  mdl  <- str_remove(model_id, "^[^:]+:")
  switch(prov,
    ollama = chat_ollama(model = mdl, system_prompt = system_prompt, echo = "none",
                         params = params(temperature = temperature),
                         api_args = list(options = list(num_ctx = NUM_CTX))),
    openrouter = chat_openrouter(model = mdl, system_prompt = system_prompt, echo = "none",
                                 params = params(temperature = temperature)),
    stop("unknown provider: ", prov))
}

# After each turn swap the augmented user message (question + ~3k tokens of
# context) for the bare question, so history never overflows num_ctx.
trim_last_user_turn <- function(chat, question) {
  tryCatch({
    turns <- chat$get_turns(); n <- length(turns)
    if (n >= 2 && identical(turns[[n - 1]]@role, "user")) {
      turns[[n - 1]] <- if (exists("UserTurn", asNamespace("ellmer")))
        ellmer::UserTurn(list(ellmer::ContentText(question)))
      else ellmer::Turn("user", list(ellmer::ContentText(question)))
      chat$set_turns(turns)
    }
  }, error = function(e) message("(history trim skipped: ", conditionMessage(e), ")"))
  invisible(chat)
}

# -----------------------------------------------------------------------------
# Retrieval + context
# -----------------------------------------------------------------------------
open_store <- function() {
  s <- ragnar_store_connect(STORE_PATH, read_only = TRUE)
  try(dbExecute(s@con, "SET enable_progress_bar = false"), silent = TRUE)
  s
}

load_meta <- function(store)
  tryCatch(as_tibble(dbGetQuery(store@con,
    "SELECT origin, doc_type, course_code, card_type, tier FROM doc_meta")),
    error = function(e) tibble(origin = character()))

retrieve <- function(store, query, k = TOP_K, meta = NULL) {
  h <- rag_retrieve(store, query, top_k = k)
  if (!nrow(h)) return(h)
  h <- mutate(h, label = paste0("S", row_number()))
  if (!is.null(meta) && nrow(meta)) h <- left_join(h, meta, by = "origin")
  h
}

page_of <- function(txt) {
  m <- str_match(txt, "## Page (\\d+)")[, 2]
  suppressWarnings(as.integer(m))
}

build_context <- function(h)
  h |>
    mutate(block = sprintf("[%s] %s\n%s", label, origin,
                           str_trunc(str_squish(text), CTX_CHARS))) |>
    pull(block) |>
    paste(collapse = "\n\n---\n\n")

build_prompt <- function(h, question)
  paste0("Retrieved course materials:\n\n", build_context(h),
         "\n\n----\nQuestion: ", question)

# One question -> answer + hits + timing. A fresh `chat` gives a stateless call
# (eval); the app passes its running chat so follow-ups keep history.
ask_rag <- function(chat, store, question, k = TOP_K, meta = NULL) {
  tic()
  h <- retrieve(store, question, k, meta)
  if (!nrow(h)) {
    toc(quiet = TRUE)
    return(list(answer = "I couldn't find relevant course materials for that.",
                hits = h, secs = NA_real_, error = FALSE))
  }
  err <- FALSE
  ans <- tryCatch(as.character(chat$chat(build_prompt(h, question), echo = "none")),
                  error = function(e) { err <<- TRUE; paste0("**Error:** ", conditionMessage(e)) })
  trim_last_user_turn(chat, question)
  t <- toc(quiet = TRUE)
  list(answer = ans, hits = h, secs = unname(t$toc - t$tic), error = err)
}

# -----------------------------------------------------------------------------
# Deterministic equations: display math copied straight from the retrieved
# source chunks (qmd assignments, notes, textbook), so a model that mangles
# $$...$$ can't lose them.
# -----------------------------------------------------------------------------
card_equations <- function(h, max_eq = 4L) {
  if (!nrow(h)) return(tibble(label = character(), origin = character(), latex = character()))
  h |>
    filter(str_detect(text, fixed("$$"))) |>
    mutate(latex = str_extract_all(text, "(?s)\\$\\$(.+?)\\$\\$")) |>
    select(label, origin, latex) |>
    tidyr::unnest(latex) |>
    mutate(latex = str_squish(str_remove_all(latex, "^\\$\\$|\\$\\$$"))) |>
    filter(nchar(latex) > 4, str_detect(latex, "=|\\\\(frac|sum|hat|bar|sqrt)")) |>
    distinct(latex, .keep_all = TRUE) |>
    head(max_eq)
}

# -----------------------------------------------------------------------------
# Rendering: KaTeX server-side to MathML (the reliable output mode), markdown
# via commonmark. Math is swapped for alphanumeric tokens first so markdown
# can't mangle underscores/asterisks inside formulas.
# -----------------------------------------------------------------------------
kx <- function(expr, display = FALSE)
  tryCatch(as.character(katex_html(expr, displayMode = display, include_css = FALSE,
                                   preview = FALSE, output = "mathml")),
           error = function(e) NA_character_)

MATH_RX <- c(display = "(?s)\\$\\$(.+?)\\$\\$",
             inline  = "(?<![\\\\$])\\$(?!\\$)([^$\\n]+?)\\$",
             bare    = "\\\\[A-Za-z]+(?:\\^\\{[^}]*\\}|\\^[A-Za-z0-9]|_\\{[^}]*\\}|_[A-Za-z0-9]|\\{[^}]*\\})*")

# Returns list(text = tokenised text, math = tibble(token, html, ok))
protect_math <- function(text) {
  text <- gsub("\\\\_", "_", text %||% "")
  env <- new.env(); env$math <- tibble(token = character(), html = character(), ok = logical())
  sub_kind <- function(txt, kind) {
    rx <- MATH_RX[[kind]]
    str_replace_all(txt, regex(rx), function(m) map_chr(m, function(one) {
      body <- if (kind == "bare") one else str_match(one, regex(rx))[, 2]
      html <- kx(body, display = kind == "display")
      tok  <- sprintf("MATHTOKEN%04dZ", nrow(env$math) + 1L)
      env$math <- add_row(env$math, token = tok,
                          html = if (is.na(html)) as.character(htmltools::htmlEscape(one)) else html,
                          ok = !is.na(html))
      if (kind == "display") paste0("\n\n", tok, "\n\n") else tok
    }))
  }
  # code is not math: `df$a + df$b` must not become an inline formula
  code <- str_extract_all(text, "(?s)```.*?```|`[^`\\n]+`")[[1]]
  ctok <- sprintf("CODETOKEN%04dZ", seq_along(code))
  text <- reduce2(code, ctok, \(acc, cd, tk) sub(cd, tk, acc, fixed = TRUE), .init = text)
  text <- reduce(names(MATH_RX), sub_kind, .init = text)
  text <- reduce2(ctok, code, \(acc, tk, cd) sub(tk, cd, acc, fixed = TRUE), .init = text)
  list(text = text, math = env$math)
}

render_answer <- function(text) {
  p <- protect_math(text)
  html <- commonmark::markdown_html(p$text, extensions = TRUE)
  html <- reduce2(p$math$token, p$math$html,
                  \(acc, tok, h) str_replace_all(acc, fixed(tok), h), .init = html)
  structure(html, math_ok = if (nrow(p$math)) mean(p$math$ok) else NA_real_,
            n_math = nrow(p$math))
}

equations_html <- function(eqs) {
  if (!nrow(eqs)) return("")
  items <- pmap_chr(eqs, function(label, origin, latex) {
    m <- kx(latex, TRUE)
    if (is.na(m)) return("")
    sprintf("<div style='margin:.3em 0'>%s<div style='font-size:.75em;color:#999'>[%s] %s</div></div>",
            m, label, htmltools::htmlEscape(origin))
  })
  paste0("<details open style='margin-top:.6em'><summary style='font-size:.85em'>",
         "<strong>Equations as written in the retrieved sources</strong></summary>",
         paste(items, collapse = ""), "</details>")
}

source_link <- function(origin, page) {
  if (str_detect(origin, "^(reference|book)/")) return(htmltools::htmlEscape(origin))
  enc <- paste(map_chr(str_split(origin, "/")[[1]], \(s) URLencode(s, reserved = TRUE)),
               collapse = "/")
  url <- paste0(REPO_URL, "/blob/main/", enc,
                if (!is.na(page) && str_detect(origin, "(?i)\\.pdf$")) paste0("#page=", page) else "")
  sprintf("<a href='%s' target='_blank'>%s</a>", url, htmltools::htmlEscape(origin))
}

cited_labels <- function(answer)
  unique(str_extract_all(answer, "S\\d+")[[1]])

sources_html <- function(h, answer = "") {
  if (!nrow(h)) return("")
  cited <- cited_labels(answer)
  li <- h |>
    mutate(page = page_of(text),
           link = map2_chr(origin, page, source_link),
           pg   = if_else(is.na(page), "", sprintf(" <span style='color:#888'>(p.&nbsp;%d)</span>", page)),
           mark = if_else(label %in% cited, "<strong>", ""),
           unmark = if_else(label %in% cited, "</strong>", ""),
           li = sprintf("<li>%s[%s]%s %s%s</li>", mark, label, unmark, link, pg)) |>
    pull(li)
  paste0("<hr style='margin:.75em 0;border:none;border-top:1px solid #444'>",
         "<div style='font-size:.85em'><strong>Sources</strong> ",
         "<span style='color:#888'>(bold = cited)</span>",
         "<ul style='margin:.25em 0;padding-left:1.25em'>", paste(li, collapse = ""),
         "</ul></div>")
}

message("rag_core.R loaded.  make_chat()  ask_rag()  render_answer()")
