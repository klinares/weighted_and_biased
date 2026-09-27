# parse_textbook.R -- split the Practical Tools textbook PDF into one markdown
#   file per chapter, for the UMD_JPSM_coursework repo:
#
#     UMD_JPSM_coursework/practical_tools_textbook/
#       ch01-an-overview-of-sample-design-and-weighting.md
#       ch02-...
#
#   source("parse_textbook.R")          # from the rag_time folder
#
# Then build_store.R indexes those files like any other course file (no PDF
#   needed at build time), and sources link to them on GitHub.
#
# Chapters are found from the running headers (build/extract.R: book_documents),
#   and each page is kept under a "## Page n" heading so answers can cite pages.
#   Where a cleaner page transcript exists in <cache>/pages/BOOK__practical_tools/
#   pNNNN.md it is used instead of the raw PDF text (better formulas).
#
# COPYRIGHT: this is a published textbook. Keep the coursework repo PRIVATE.
#
# Paths come from the environment, or edit the defaults below:
#   SVYMETH_BOOK    the textbook PDF
#   SVYMETH_CORPUS  the UMD_JPSM_coursework folder
#   SVYMETH_CACHE   folder that may hold pages/BOOK__practical_tools/ transcripts

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(stringr) })
source("build/extract.R")

parse_textbook <- function(pdf    = Sys.getenv("SVYMETH_BOOK"),
                           corpus = Sys.getenv("SVYMETH_CORPUS"),
                           cache  = Sys.getenv("SVYMETH_CACHE", tempdir())) {
  stopifnot("Set SVYMETH_BOOK to the textbook PDF" = file.exists(pdf),
            "Set SVYMETH_CORPUS to the coursework folder" = dir.exists(corpus))
  t0 <- Sys.time()
  docs <- book_documents(pdf, cache)
  if (!length(docs)) stop("No chapters found in ", pdf)
  out <- file.path(corpus, TEXTBOOK_DIR)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  files <- imap_chr(docs, function(d, i) {
    slug <- basename(d$origin)                                 # chapter-1-an-overview...
    n    <- as.integer(str_match(slug, "^chapter-(\\d+)")[, 2]) %||% i
    f    <- file.path(out, sprintf("ch%02d-%s.md", n, sub("^chapter-\\d+-?", "", slug)))
    con  <- file(f, open = "w", encoding = "UTF-8")
    writeLines(c("---", sprintf('title: "%s"', sub("^# ", "", strsplit(d$md, "\n")[[1]][1])),
                 "source: Valliant, Dever & Kreuter, Practical Tools for Designing and Weighting Survey Samples",
                 "---", "", d$md), con)
    close(con)
    f
  })
  message(sprintf("%d chapter files written to %s in %.1f s", length(files), out,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(tibble::tibble(file = basename(files),
                           kb = round(file.size(files) / 1024)))
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) print(parse_textbook(), n = Inf)
