# build_store.R -- build store/jpsm.duckdb from the ORIGINAL coursework.
#
#   source("build_store.R")          # from the app folder, in RStudio
#
# Needs: the coursework folder, the key for the embedding endpoint in the
#   environment (OPENROUTER_API_KEY at home, OPENAI_API_KEY at work -- see
#   config.R), and pdftools. No chat model is called: the only API calls are
#   embeddings, about one per 100 chunks (~110 requests, ~3M tokens).
#
# Resumable: extracted text is cached per file and embeddings per chunk, under
#   BUILD$cache. If it stops (network, quota), run it again; finished work is
#   not repeated. Delete the cache folder only to force a full rebuild.
#
# At work, set SVYMETH_PROFILE=work and SVYMETH_CORPUS / SVYMETH_BOOK /
#   SVYMETH_CACHE in .Renviron, and run this. Then deploy (deploy.R).

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(stringr); library(tibble); library(DBI)
})
invisible(lapply(c("R/config.R", "R/llm.R", "R/retrieve.R", "build/extract.R"), source))

BUILD <- list(
  corpus = Sys.getenv("SVYMETH_CORPUS", "D:/repos/UMD_JPSM_coursework"),
  book   = Sys.getenv("SVYMETH_BOOK", "D:/UMD/books/practical_tools_textbook.pdf"),
  # cache/pages/BOOK__practical_tools/ holds vision-converted textbook pages, if any
  cache  = Sys.getenv("SVYMETH_CACHE", "D:/RAG_files/cache"),
  chunk_size = 1200L, overlap = 0.2, book_chunk = 1500L,
  batch  = 96L)

build_store <- function(out = svy_store_path()) {
  t0 <- Sys.time(); prof <- svy_profile()
  key <- Sys.getenv(prof$key_var)
  if (!nzchar(key)) stop(prof$key_var, " is not set; the build needs it for embeddings.")
  dir.create(BUILD$cache, recursive = TRUE, showWarnings = FALSE)

  # 1. originals -> markdown ---------------------------------------------------
  files <- eligible_files(BUILD$corpus)
  message(length(files), " original files eligible")
  docs <- imap(files, function(p, i) {
    if (i %% 50 == 0) message("  read ", i, "/", length(files))
    rel <- rel_of(p, BUILD$corpus)
    list(origin = rel, md = tryCatch(read_any(p, rel, BUILD$cache),
                                     error = function(e) { message("  unreadable: ", rel); "" }))
  })
  docs <- c(docs, book_documents(BUILD$book, BUILD$cache) %||% list())
  empty <- map_lgl(docs, function(d) nchar(str_trim(d$md)) < 60)
  if (any(empty)) message(sum(empty), " files had no usable text (scanned/image-only): ",
                          paste(basename(map_chr(docs[empty], "origin")), collapse = "; "))
  docs <- docs[!empty]

  # 2. chunk -------------------------------------------------------------------
  chunks <- map(docs, function(d) {
    size <- if (startsWith(d$origin, "book/")) BUILD$book_chunk else BUILD$chunk_size
    ch <- tryCatch(ragnar::markdown_chunk(ragnar::MarkdownDocument(d$md, origin = d$origin),
                                          target_size = size, target_overlap = BUILD$overlap),
                   error = function(e) NULL)
    if (is.null(ch) || !nrow(ch)) return(NULL)
    tibble(origin = d$origin, context = as.character(ch$context %||% ""), text = as.character(ch$text))
  }) |> list_rbind() |>
    filter(nchar(str_trim(text)) >= 40) |>
    mutate(id = row_number(),
           embed_in = str_trunc(paste(origin, context, text, sep = "\n"), 24000),
           hash = map_chr(embed_in, rlang::hash))
  message(nrow(chunks), " chunks from ", n_distinct(chunks$origin), " files")

  # 3. embed, cached by content hash ------------------------------------------
  cache_f <- file.path(BUILD$cache, sprintf("emb_%s_%d.rds",
                        gsub("[^a-z0-9]+", "-", embed_model_id(prof$embed)), SVY_SETTINGS$embed_dims))
  emb <- if (file.exists(cache_f)) readRDS(cache_f) else list()
  todo <- which(!chunks$hash %in% names(emb))
  batches <- split(todo, ceiling(seq_along(todo) / BUILD$batch))
  message(length(todo), " chunks to embed in ", length(batches), " requests (",
          nrow(chunks) - length(todo), " cached)")
  iwalk(batches, function(ix, b) {
    m <- embed_texts(chunks$embed_in[ix], key = key, prof = prof)
    emb[chunks$hash[ix]] <<- map(seq_len(nrow(m)), function(r) m[r, ])
    if (as.integer(b) %% 10 == 0 || as.integer(b) == length(batches)) {
      saveRDS(emb, cache_f); message("  embedded batch ", b, "/", length(batches))
    }
  })
  saveRDS(emb, cache_f)

  # 4. keyword index (same tokenizer the app uses for queries) ---------------
  terms <- tibble(id = chunks$id,
                  term = map(paste(chunks$context, chunks$text), svy_tokens)) |>
    tidyr::unnest(term)
  lens  <- count(terms, id, name = "len")
  terms <- count(terms, id, term, name = "tf")
  N <- nrow(chunks)
  idf <- terms |> count(term, name = "df") |> mutate(idf = log(1 + (N - df + 0.5) / (df + 0.5))) |> select(term, idf)

  # 5. write -------------------------------------------------------------------
  tmp <- paste0(out, ".tmp"); unlink(tmp)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  con <- dbConnect(duckdb::duckdb(), dbdir = tmp, config = list(home_directory = tempdir()))
  on.exit(try(dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  d <- SVY_SETTINGS$embed_dims
  meta <- doc_meta(chunks$origin)
  base <- bind_cols(select(chunks, id, origin, context, text), select(meta, -origin))
  dbWriteTable(con, "svy_base", as.data.frame(base))
  vecs <- tibble(id = chunks$id, embedding = unname(emb[chunks$hash]))
  dbWriteTable(con, "svy_vec", as.data.frame(tibble(id = rep(vecs$id, each = d),
                                                   pos = rep(seq_len(d), N),
                                                   v = unlist(vecs$embedding))))
  dbExecute(con, sprintf(
    "CREATE TABLE svy_chunks AS
       SELECT b.*, CAST(list(v.v ORDER BY v.pos) AS FLOAT[%d]) AS embedding
         FROM svy_base b JOIN svy_vec v USING (id)
        GROUP BY ALL ORDER BY b.id", d))
  walk(c("svy_base", "svy_vec"), function(t) dbExecute(con, paste("DROP TABLE", t)))
  dbWriteTable(con, "svy_terms", as.data.frame(terms))
  dbWriteTable(con, "svy_len", as.data.frame(lens))
  dbWriteTable(con, "svy_idf", as.data.frame(idf))
  dbWriteTable(con, "svy_meta", data.frame(
    key = c("embed_model", "embed_dims", "built_at", "profile", "n_docs", "n_chunks", "corpus"),
    value = c(prof$embed, d, format(Sys.time(), "%Y-%m-%d %H:%M"), prof$name,
              n_distinct(chunks$origin), N, BUILD$corpus)))
  dbExecute(con, "CHECKPOINT")
  dbDisconnect(con, shutdown = TRUE)
  unlink(out)
  if (!file.rename(tmp, out)) { file.copy(tmp, out, overwrite = TRUE); unlink(tmp) }
  message(sprintf("store written: %s (%.0f MB) in %.1f min", out, file.info(out)$size / 1e6,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  print(count(meta, doc_type, sort = TRUE))
  invisible(out)
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) build_store()
