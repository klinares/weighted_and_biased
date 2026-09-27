# retrieve.R -- the store and hybrid search. Plain DuckDB SQL only: no
#   ragnar, no fts/vss extensions, nothing to download on Connect.
#
# Tables written by build_store.R:
#   svy_chunks (id, origin, doc_type, course, tier, context, text, embedding FLOAT[dims])
#   svy_terms  (id, term, tf)      -- BM25 postings, same tokenizer as queries
#   svy_len    (id, len)           -- chunk length in tokens
#   svy_idf    (term, idf)
#   svy_meta   (key, value)        -- embed model, dims, build date, counts

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

SVY_STOPWORDS <- c(
  "a","an","and","are","as","at","be","by","for","from","has","have","in",
  "is","it","its","of","on","or","that","the","this","to","was","were","will",
  "with","what","which","how","why","when","where","who","do","does","can",
  "i","we","you","they","he","she","not","no","if","then","than","so","but",
  "into","about","between","also","each","these","those","there","their",
  "our","your","my","me","us","use","used","using","one","two","may")

# One tokenizer for the index and for queries. A light plural fold only:
#   "weights" and "weight" meet, "variance" and "variances" meet.
svy_tokens <- function(txt) {
  tk <- stringr::str_split(stringr::str_to_lower(txt %||% ""), "[^a-z0-9_]+")[[1]]
  tk <- tk[nchar(tk) >= 2 & !tk %in% SVY_STOPWORDS]
  pl <- nchar(tk) > 4 & grepl("[^s]s$", tk); tk[pl] <- sub("s$", "", tk[pl]); tk
}

# A connection per session, read-only. home_directory points DuckDB at a
#   writable temp folder: on Windows it otherwise tries to create ~/.duckdb
#   (and can raise a hidden dialog), and on Connect the home may be read-only.
open_store <- function(path = svy_store_path()) {
  if (!file.exists(path))
    stop("No store at ", path, ". Build it with build_store.R.", call. = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE,
                        config = list(home_directory = tempdir()))
  DBI::dbExecute(con, "SET enable_progress_bar = false")
  meta <- DBI::dbGetQuery(con, "SELECT key, value FROM svy_meta")
  meta <- stats::setNames(as.list(meta$value), meta$key)
  list(con = con, meta = meta,
       avglen = DBI::dbGetQuery(con, "SELECT avg(len) a FROM svy_len")$a)
}

close_store <- function(st)
  try(DBI::dbDisconnect(st$con, shutdown = TRUE), silent = TRUE)

# Refuses a mismatch rather than returning plausible, wrong sources.
check_store_matches <- function(st, prof = svy_profile()) {
  want <- embed_model_id(prof$embed); have <- embed_model_id(st$meta$embed_model)
  if (!identical(want, have) ||
      !identical(as.integer(st$meta$embed_dims), SVY_SETTINGS$embed_dims))
    stop("The store was built with ", have, " (", st$meta$embed_dims,
         " dims) but this profile embeds with ", want, " (",
         SVY_SETTINGS$embed_dims, " dims). Rebuild the store or fix config.R.",
         call. = FALSE)
  invisible(TRUE)
}

vec_literal <- function(v, dims)
  sprintf("[%s]::FLOAT[%d]", paste(format(v, digits = 7, scientific = FALSE,
                                          trim = TRUE), collapse = ","), dims)

search_vector <- function(st, qvec, n) {
  DBI::dbGetQuery(st$con, sprintf(
    "SELECT id, array_cosine_distance(embedding, %s) AS cos
       FROM svy_chunks ORDER BY cos LIMIT %d",
    vec_literal(qvec, as.integer(st$meta$embed_dims)), n))
}

search_bm25 <- function(st, query, n, k1 = 1.2, b = 0.75) {
  q <- unique(svy_tokens(query))
  if (!length(q)) return(data.frame(id = integer(), bm = numeric()))
  terms <- paste(sprintf("'%s'", gsub("'", "''", q)), collapse = ",")
  DBI::dbGetQuery(st$con, sprintf(
    "SELECT t.id, SUM(i.idf * (t.tf * (%1$f + 1)) /
            (t.tf + %1$f * (1 - %2$f + %2$f * l.len / %3$f))) AS bm
       FROM svy_terms t JOIN svy_idf i USING (term) JOIN svy_len l USING (id)
      WHERE t.term IN (%4$s)
      GROUP BY t.id ORDER BY bm DESC LIMIT %5$d",
    k1, b, st$avglen, terms, n))
}

# Reciprocal-rank fusion. Cosine (lower better, bounded) and BM25 (higher
#   better, unbounded) are not on one scale, and a keyword-only hit has no
#   cosine to blend, so ranks are fused rather than scores. One chunk per file
#   keeps the context from being five slices of the same deck.
retrieve <- function(st, query, k = SVY_SETTINGS$top_k, qvec = NULL) {
  n  <- SVY_SETTINGS$pool; rk <- SVY_SETTINGS$rrf_k
  qvec <- qvec %||% embed_texts(query)[1, ]
  v  <- search_vector(st, qvec, n); v$r_v <- seq_len(nrow(v))
  bm <- search_bm25(st, query, n);  bm$r_b <- seq_len(nrow(bm))
  f  <- dplyr::full_join(v, bm, by = "id") |>
    dplyr::mutate(score = dplyr::coalesce(1 / (rk + r_v), 0) +
                          dplyr::coalesce(1 / (rk + r_b), 0)) |>
    dplyr::arrange(dplyr::desc(score))
  if (!nrow(f)) return(tibble::tibble())
  rows <- DBI::dbGetQuery(st$con, sprintf(
    "SELECT id, origin, doc_type, course, tier, context, text
       FROM svy_chunks WHERE id IN (%s)", paste(f$id, collapse = ",")))
  dplyr::inner_join(f, rows, by = "id") |>
    dplyr::arrange(dplyr::desc(score)) |>
    dplyr::distinct(origin, .keep_all = TRUE) |>
    utils::head(k) |>
    dplyr::mutate(label = paste0("S", dplyr::row_number()),
                  page = suppressWarnings(as.integer(
                    stringr::str_match(text, "## Page (\\d+)")[, 2])))
}

build_context <- function(h)
  paste(sprintf("[%s] %s%s\n%s", h$label, h$origin,
                ifelse(is.na(h$page), "", paste0(" (p. ", h$page, ")")),
                stringr::str_trunc(stringr::str_squish(h$text),
                                   SVY_SETTINGS$ctx_chars)),
        collapse = "\n\n---\n\n")
