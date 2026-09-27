# render.R -- markdown + LaTeX to HTML, server side.
#
# KaTeX renders to MathML (output = "mathml"): the browser draws it natively
#   with no stylesheet or fonts, which is what failed with output = "html".
#   Math is swapped for placeholder tokens before markdown runs, so markdown
#   cannot mangle an underscore or asterisk inside a formula; code spans are
#   protected first so `df$a + df$b` is not mistaken for inline math.

kx <- function(expr, display = FALSE)
  tryCatch(as.character(katex::katex_html(expr, displayMode = display,
                                          include_css = FALSE, preview = FALSE,
                                          output = "mathml")),
           error = function(e) NA_character_)

MATH_RX <- c(display = "(?s)\\$\\$(.+?)\\$\\$",
             inline  = "(?<![\\\\$\\w])\\$(?!\\$|\\s)([^$\\n]+?)(?<!\\s)\\$(?!\\d)",
             paren   = "(?s)\\\\\\((.+?)\\\\\\)",
             bracket = "(?s)\\\\\\[(.+?)\\\\\\]")

protect_math <- function(text) {
  text <- gsub("\\\\_", "_", text %||% "")
  code <- stringr::str_extract_all(text, "(?s)```.*?```|`[^`\n]+`")[[1]]
  ctok <- sprintf("CODETOKEN%04dZ", seq_along(code))
  text <- purrr::reduce2(code, ctok, function(a, cd, tk) sub(cd, tk, a, fixed = TRUE), .init = text)

  env <- new.env(); env$m <- list()
  sub_kind <- function(txt, kind) {
    rx <- MATH_RX[[kind]]
    disp <- kind %in% c("display", "bracket")
    stringr::str_replace_all(txt, stringr::regex(rx), function(m) purrr::map_chr(m, function(one) {
      body <- stringr::str_match(one, stringr::regex(rx))[, 2]
      html <- kx(body, disp)
      tok  <- sprintf("MATHTOKEN%04dZ", length(env$m) + 1L)
      env$m[[tok]] <- list(html = if (is.na(html)) as.character(htmltools::htmlEscape(one)) else html,
                           ok = !is.na(html))
      if (disp) paste0("\n\n", tok, "\n\n") else tok
    }))
  }
  text <- purrr::reduce(names(MATH_RX), sub_kind, .init = text)
  text <- purrr::reduce2(ctok, code, function(a, tk, cd) sub(tk, cd, a, fixed = TRUE), .init = text)
  list(text = text, math = env$m)
}

render_markdown <- function(text) {
  p <- protect_math(text)
  html <- commonmark::markdown_html(p$text, extensions = TRUE)
  html <- purrr::reduce2(names(p$math), purrr::map_chr(p$math, "html"),
                         function(a, tk, h) gsub(tk, h, a, fixed = TRUE), .init = html)
  structure(html, math_ok = if (length(p$math)) mean(purrr::map_lgl(p$math, "ok")) else NA_real_)
}

source_link <- function(origin, page) {
  if (startsWith(origin, "book/")) return(htmltools::htmlEscape(origin))
  enc <- paste(purrr::map_chr(strsplit(origin, "/")[[1]],
                              function(s) utils::URLencode(s, reserved = TRUE)), collapse = "/")
  url <- paste0(SVY_REPO_URL, "/blob/main/", enc,
                if (!is.na(page) && grepl("\\.pdf$", origin, ignore.case = TRUE))
                  paste0("#page=", page) else "")
  sprintf("<a href='%s' target='_blank'>%s</a>", url, htmltools::htmlEscape(origin))
}

sources_html <- function(h, audit) {
  if (!nrow(h)) return("")
  li <- purrr::pmap_chr(list(h$label, h$origin, h$page), function(l, o, p) {
    b <- l %in% audit$cited
    sprintf("<li>%s[%s]%s %s%s</li>", if (b) "<strong>" else "", l,
            if (b) "</strong>" else "", source_link(o, p),
            if (is.na(p)) "" else sprintf(" <span class='text-muted'>(p.&nbsp;%d)</span>", p))
  })
  paste0("<details style='margin-top:.6em'><summary class='small'><strong>Sources</strong>",
         " <span class='text-muted'>(bold = cited)</span></summary>",
         "<ul class='small' style='margin:.25em 0'>", paste(li, collapse = ""), "</ul></details>")
}

audit_html <- function(audit) {
  msgs <- c(
    if (length(audit$bad_cite))
      paste0("Cites ", paste(audit$bad_cite, collapse = ", "),
             ", which was not among the retrieved sources."),
    if (length(audit$eq_unsourced))
      paste0(length(audit$eq_unsourced), " of ", audit$n_eq,
             " displayed formula(s) do not appear in the retrieved sources; check them against the course material before use."))
  if (!length(msgs)) {
    if (audit$n_eq) return(sprintf(
      "<div class='small text-success'>Audit: all %d displayed formula(s) found in the retrieved sources.</div>", audit$n_eq))
    return("")
  }
  paste0("<div class='small text-warning' style='border-left:3px solid;padding-left:.5em'>",
         paste(paste0("Audit: ", msgs), collapse = "<br>"), "</div>")
}
