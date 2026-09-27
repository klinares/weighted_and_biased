# llm.R -- every model and embedding call. Keys are held per Shiny session and
#   never written to disk or to the process environment (a Sys.setenv() on a
#   shared Connect process would leak one analyst's key to every other).

.llm_state <- new.env(parent = emptyenv())
.llm_state$keys <- list()

llm_session_key <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s)) return(NULL)
  k <- .llm_state$keys[[s$token]]
  if (is.null(k) || !nzchar(k)) NULL else k
}
llm_set_session_key <- function(key) {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s)) stop("No session to attach a key to.", call. = FALSE)
  .llm_state$keys[[s$token]] <- key
  invisible(TRUE)
}
llm_clear_session_key <- function(token) {
  .llm_state$keys[[token]] <- NULL
  invisible(TRUE)
}

llm_api_key <- function() {
  prof <- svy_profile()
  env <- if (identical(SVY_SETTINGS$key_source, "server")) Sys.getenv(prof$key_var) else ""
  key <- llm_session_key() %||% env
  if (!nzchar(key))
    stop("No API key. Paste one on the Start here tab",
         if (identical(SVY_SETTINGS$key_source, "server"))
           paste0(", or set ", prof$key_var, " in .Renviron") else "",
         ".", call. = FALSE)
  key
}

# A fresh object per conversation. The key travels in the closure, so a key
#   change means a new chat object (the chat module does that).
llm_chat <- function(role = c("chat", "worker"), system_prompt = NULL,
                     temperature = 0.2, model = NULL) {
  role <- match.arg(role)
  prof <- svy_profile()
  key  <- llm_api_key()
  ellmer::chat_openai_compatible(
    base_url = prof$base_url,
    model = model %||% prof[[role]],
    credentials = function() list(Authorization = paste("Bearer", key)),
    system_prompt = system_prompt,
    params = ellmer::params(temperature = temperature,
                            max_tokens = SVY_SETTINGS$max_tokens),
    echo = "none")
}

# Query embeddings go straight to /embeddings, not through the store: the
#   store never holds a key or a function that needs one.
embed_texts <- function(x, key = llm_api_key(), prof = svy_profile(),
                        dims = SVY_SETTINGS$embed_dims) {
  resp <- httr2::request(paste0(sub("/+$", "", prof$base_url), "/embeddings")) |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_body_json(list(model = prof$embed, input = as.list(x),
                              dimensions = dims)) |>
    httr2::req_timeout(SVY_SETTINGS$timeout) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  if (httr2::resp_status(resp) >= 400 || !is.null(body$error))
    stop("Embedding request failed (HTTP ", httr2::resp_status(resp), "): ",
         substr(body$error$message %||% "", 1, 300), call. = FALSE)
  d <- body$data[order(purrr::map_int(body$data, "index"))]
  do.call(rbind, purrr::map(d, function(e) unlist(e$embedding)))
}

# Strip a provider prefix so openai/text-embedding-3-small (OpenRouter) and
#   text-embedding-3-small (OpenAI) are recognised as the same model.
embed_model_id <- function(m) sub("^openai/", "", m)
