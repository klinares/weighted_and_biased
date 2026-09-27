# extract.R -- turn original coursework into markdown text. Used only by
#   build_store.R (needs pdftools, which the deployed app does not).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- what goes in -----------------------------------------------------------
# Folders indexed, and extensions from each. Quarto/R sources carry the code
#   and the typed math; PDFs are used only where there is no source twin.
FOLDERS <- list(
  lectures = "pdf", assignments = c("qmd", "rmd", "r"),
  midterm = c("qmd", "rmd", "pdf"), final = c("qmd", "rmd", "pdf"),
  exams = c("qmd", "rmd", "pdf"),
  code = c("r", "qmd", "rmd", "py"), class_code = c("r", "qmd", "rmd", "py"),
  class_code_files = c("r", "qmd", "rmd", "py"),
  class_examples = c("r", "qmd", "rmd", "pdf", "py"),
  labs = c("r", "qmd", "rmd", "pdf"), lab = c("r", "qmd", "rmd", "pdf"),
  class_labs = c("r", "qmd", "rmd", "pdf"),
  llm_demo = c("r", "qmd", "rmd", "py"), record_linkage = c("r", "qmd", "rmd", "pdf"),
  project = c("qmd", "rmd", "r", "pdf"),
  notes = "pdf",          # PDFs at a course root: lecture/reading notes, reviews
  textbook = "md")        # TEXTBOOK_DIR: chapter markdown from parse_textbook.R

# Top-level corpus folder holding the textbook chapters (parse_textbook.R).
TEXTBOOK_DIR <- "practical_tools_textbook"

# Never indexed. Rendered output, data, binaries, scanned readings, admin.
SKIP <- c("\\.Rhistory$", "/\\.quarto/", "/_freeze/", "/_site/", "/renv/",
          "/\\.git/", "/node_modules/", "/images/", "/figures?/", "/libs/",
          "_files/", "/readings/", "/data/",
          "/assignments/[^/]*\\.pdf$",            # rendered; the .qmd is indexed
          "syllabus|pledge|waiver|mail -|peerrs",
          "\\.(rdata|rds|rdx|rdb|dat|dta|sav|sas7bdat|xlsx|xls|csv|tsv|png|jpe?g|gif|svg|mp4|zip|html|docx|odt|pptx)$")

eligible_files <- function(corpus) {
  all <- as.character(fs::dir_ls(corpus, recurse = TRUE, type = "file"))
  rel <- rel_of(all, corpus)
  drop <- purrr::reduce(purrr::map(SKIP, function(p)
    stringr::str_detect(paste0("/", rel), stringr::regex(p, ignore_case = TRUE))), `|`)
  all <- all[!drop]; rel <- rel[!drop]
  fold <- purrr::map_chr(rel, function(r) {
    s <- strsplit(r, "/")[[1]]
    if (identical(s[1], TEXTBOOK_DIR)) return("textbook")
    if (length(s) == 2) return("notes")
    h <- tolower(s) %in% setdiff(names(FOLDERS), c("notes", "textbook"))
    if (any(h)) tolower(s)[which(h)[1]] else NA_character_
  })
  ext <- tolower(tools::file_ext(all))
  keep <- !is.na(fold) & purrr::map2_lgl(ext, fold, function(e, f) !is.na(f) && e %in% FOLDERS[[f]])
  all <- all[keep]; ext <- ext[keep]
  # A PDF with a Quarto/R Markdown twin (same folder, same stem) is its render.
  stem <- tools::file_path_sans_ext(all)
  src_stems <- stem[ext %in% c("qmd", "rmd")]
  all[!(ext == "pdf" & tolower(stem) %in% tolower(src_stems))]
}

rel_of <- function(p, corpus) {
  n <- gsub("\\\\", "/", p); c0 <- sub("/+$", "", gsub("\\\\", "/", corpus))
  sub("^/+", "", ifelse(startsWith(tolower(n), tolower(c0)), substring(n, nchar(c0) + 1), basename(n)))
}

# --- text clean-up ------------------------------------------------------------
# LaTeX PDFs emit ligature codepoints (speci + U+FB01 + cation) invisible to a
#   keyword search spelled normally; smart quotes and minus signs likewise.
tidy_text <- function(x, dehyphen = FALSE) {
  if (!length(x)) return(x)
  x <- stringi::stri_replace_all_fixed(x,
    c("ﬀ","ﬁ","ﬂ","ﬃ","ﬄ","‘","’",
      "“","”"," ","­","−","‐","‑"),
    c("ff","fi","fl","ffi","ffl","'","'","\"","\""," ","","-","-","-"),
    vectorize_all = FALSE)
  if (dehyphen)
    x <- stringr::str_replace_all(x, "([[:alpha:]])-[ \t]*\r?\n[ \t]*([[:lower:]])", "\\1\\2")
  iconv(x, "UTF-8", "UTF-8", sub = "")
}

drop_furniture <- function(page) {
  ln <- strsplit(page, "\n", fixed = TRUE)[[1]]
  if (length(ln) < 3) return(page)
  nb <- which(nzchar(trimws(ln))); if (!length(nb)) return(page)
  cand <- unique(c(nb[1], nb[length(nb)])); s <- trimws(ln[cand])
  junk <- grepl("^\\d{1,4}$", s) | grepl("^\\d{1,4}[ \t]{2,}\\S", s) | grepl("\\S[ \t]{2,}\\d{1,4}$", s)
  if (!any(junk)) page else paste(ln[-cand[junk]], collapse = "\n")
}

# Corruption score for one page (broken font encodings). NA = too little text.
garble <- function(txt) {
  txt <- trimws(txt %||% "")
  if (nchar(txt) < 40) return(NA_real_)
  tk <- unlist(strsplit(txt, "[[:space:]]+")); tk <- tk[nchar(tk) >= 3]
  if (length(tk) < 8) return(NA_real_)
  al <- tk[grepl("^[A-Za-z]{3,}$", tk)]
  max(if (length(al) >= 5) mean(!grepl("[aeiouyAEIOUY]", al)) else 0,
      mean(grepl("[[:punct:]][[:punct:][:digit:]]{2,}", tk)))
}
GARBLE_MAX <- 0.35

slide_title <- function(page) {
  ln <- trimws(strsplit(page, "\n", fixed = TRUE)[[1]]); ln <- ln[nzchar(ln)]
  if (!length(ln)) return("")
  t1 <- stringr::str_squish(ln[1])
  if (nchar(t1) > 90 || nchar(t1) < 3 || grepl("^[0-9[:punct:][:space:]]+$", t1)) "" else t1
}

read_pdf <- function(path) {
  pg <- tryCatch(pdftools::pdf_text(path), error = function(e) character(0))
  if (!length(pg)) return("")
  g <- purrr::map_dbl(pg, garble); ok <- !is.na(g) & g <= GARBLE_MAX
  if (!any(ok)) return("")
  i <- which(ok)
  head <- purrr::map_chr(i, function(j) {
    t <- slide_title(pg[j])
    if (nzchar(t)) sprintf("\n\n## Page %d -- %s\n\n", j, t) else sprintf("\n\n## Page %d\n\n", j)
  })
  paste0(head, tidy_text(purrr::map_chr(pg[i], drop_furniture)), collapse = "")
}

# Cached by path and modification time, so a rerun after a failure is fast.
read_any <- function(path, rel, cache_dir) {
  cp <- file.path(cache_dir, "text", paste0(gsub("[/\\\\:]", "__", rel), ".md"))
  if (file.exists(cp) && file.info(cp)$mtime >= file.info(path)$mtime)
    return(paste(readLines(cp, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  ext <- tolower(tools::file_ext(path))
  rd <- function() paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- switch(ext, pdf = read_pdf(path), qmd = , rmd = , md = rd(),
                r  = paste0("```r\n", rd(), "\n```"),
                py = paste0("```python\n", rd(), "\n```"), "")
  txt <- tidy_text(txt %||% "")
  dir.create(dirname(cp), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, cp, useBytes = TRUE)
  txt
}

# --- textbook -------------------------------------------------------------------
# PracTools prints the section number in every running header, so chapter
#   membership is read from every page rather than from opener pages.
chapter_of <- function(page) {
  ln <- utils::head(stringr::str_subset(strsplit(page, "\n")[[1]], "\\S"), 2)
  if (!length(ln)) return(c(NA_character_, NA_character_))
  s <- ln |> trimws() |> sub("^\\d{1,4}[ \t]+", "", x = _) |> sub("[ \t]+\\d{1,4}$", "", x = _)
  m <- stringr::str_match(s, "^(\\d{1,2})((?:\\.\\d{1,2})*)[ \t]+([A-Za-z].{0,70})$")
  i <- which(!is.na(m[, 2]))[1]
  if (is.na(i)) return(c(NA_character_, NA_character_))
  c(m[i, 2], if (nzchar(m[i, 3])) NA_character_ else stringr::str_squish(m[i, 4]))
}

# Returns one markdown document per chapter, or NULL. Vision-converted pages
#   (cache/pages/BOOK__practical_tools/p0001.md, ...) are preferred when present.
book_documents <- function(path, cache_dir) {
  if (!nzchar(path) || !file.exists(path)) { message("no textbook at '", path, "' -- skipped"); return(NULL) }
  raw <- tryCatch(pdftools::pdf_text(path), error = function(e) character(0))
  if (!length(raw)) return(NULL)
  ch  <- purrr::map(raw, chapter_of)
  num <- purrr::map_int(ch, function(x) suppressWarnings(as.integer(x[1])))
  ttl <- purrr::map_chr(ch, function(x) x[2] %||% NA_character_)
  if (sum(!is.na(num)) < 0.4 * length(num)) { message("  no chapter headers found"); return(NULL) }
  r <- rle(dplyr::if_else(is.na(num), -1L, num))
  e <- cumsum(r$lengths); s <- e - r$lengths + 1L
  k <- r$values > 0 & r$lengths >= 2
  chs <- tibble::tibble(page = s[k], end = e[k], n = as.character(r$values[k]),
    title = purrr::map2_chr(s[k], e[k], function(a, b) {
      t <- ttl[seq(a, b)]; t <- t[!is.na(t)]
      if (!length(t)) "" else names(sort(table(t), decreasing = TRUE))[1] })) |>
    dplyr::group_by(n) |> dplyr::slice_min(page, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::arrange(page) |> dplyr::mutate(label = stringr::str_squish(paste("Chapter", n, title)),
                                          end = dplyr::lead(page, default = length(raw) + 1L) - 1L)
  pdir <- file.path(cache_dir, "pages", "BOOK__practical_tools")
  pages <- purrr::map_chr(seq_along(raw), function(i) {
    f <- file.path(pdir, sprintf("p%04d.md", i))
    if (file.exists(f) && file.info(f)$size > 0)
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    else drop_furniture(tidy_text(raw[i], dehyphen = TRUE))
  })
  ok <- purrr::map_lgl(pages, function(p) { g <- garble(p); !is.na(g) && g <= GARBLE_MAX })
  purrr::pmap(chs, function(page, end, n, title, label) {
    idx <- seq(page, end); idx <- idx[ok[idx]]
    if (!length(idx)) return(NULL)
    slug <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(label)))
    list(origin = paste0("book/practical-tools/", substr(slug, 1, 60)),
         md = paste0("# ", label, paste0("\n\n## Page ", idx, "\n\n", pages[idx], collapse = "")))
  }) |> purrr::compact()
}

# --- metadata from the origin path -----------------------------------------
doc_meta <- function(origin) {
  p1 <- stringr::str_split_i(origin, "/", 1); p2 <- tolower(stringr::str_split_i(origin, "/", 2))
  ext <- tolower(tools::file_ext(origin))
  n_parts <- stringr::str_count(origin, "/") + 1
  dt <- dplyr::case_when(
    p1 %in% c("book", TEXTBOOK_DIR) ~ "textbook",
    p2 == "lectures" ~ "lecture",
    n_parts == 2 & ext == "pdf" ~ "notes",
    p2 %in% c("midterm", "final", "exams") ~ "exam",
    p2 == "assignments" & ext == "r" ~ "assignment_code",
    p2 == "assignments" ~ "assignment",
    p2 %in% c("labs", "lab", "class_labs") ~ "lab",
    p2 == "project" ~ "project",
    TRUE ~ "code")
  tibble::tibble(
    origin = origin, doc_type = dt,
    course = dplyr::if_else(p1 %in% c("book", TEXTBOOK_DIR), "BOOK",
                            dplyr::coalesce(stringr::str_extract(p1, "SURV\\d{3}"), p1)),
    tier = dplyr::case_when(dt %in% c("lecture", "notes", "textbook", "assignment", "exam") ~ "core",
                            TRUE ~ "widened"))
}
