# config.R -- endpoint, models, paths. Committed on purpose: these are
#   properties of the deployment, not of the analyst. The analyst supplies one
#   thing, the key.

# Two profiles, chosen by the SVYMETH_PROFILE environment variable (set it in
#   .Renviron locally, or in the Connect content's Vars pane). Everything is
#   OpenAI-compatible, so the app talks to both through the same code.
#
# THE EMBEDDING MODEL AND DIMENSIONS MUST MATCH THE STORE. The store records
#   which model built it; the app refuses to start a search with a different
#   one, because vectors from two models are not comparable and the failure
#   would otherwise be silent (plausible but wrong sources).
#   openai/text-embedding-3-small via OpenRouter and text-embedding-3-small via
#   OpenAI are the same model and produce the same vectors.
SVY_PROFILES <- list(
  home = list(
    base_url = "https://openrouter.ai/api/v1",
    key_var  = "OPENROUTER_API_KEY",
    chat     = "meta-llama/llama-4-maverick",
    worker   = "google/gemma-4-31b-it",
    embed    = "openai/text-embedding-3-small"),
  work = list(
    base_url = "https://api.openai.com/v1",
    key_var  = "OPENAI_API_KEY",
    # EDIT to the model names your OpenAI endpoint serves.
    chat     = "gpt-4.1",
    worker   = "gpt-4.1-mini",
    embed    = "text-embedding-3-small"))

svy_profile <- function() {
  p <- Sys.getenv("SVYMETH_PROFILE", "home")
  if (!p %in% names(SVY_PROFILES))
    stop("SVYMETH_PROFILE is '", p, "'; expected one of: ",
         paste(names(SVY_PROFILES), collapse = ", "), call. = FALSE)
  c(SVY_PROFILES[[p]], list(name = p))
}

SVY_SETTINGS <- list(
  embed_dims  = 512L,     # 512 keeps the store small enough for GitHub
  # "server": use the key in the environment if the analyst pastes none.
  # "analyst": ignore the environment; every analyst pastes their own.
  #   Use "analyst" on Connect unless the server key is meant to be shared.
  key_source  = Sys.getenv("SVYMETH_KEY_SOURCE", "server"),
  top_k       = 8L,       # chunks handed to the model
  pool        = 40L,      # candidates per retriever before fusion
  rrf_k       = 60L,
  ctx_chars   = 1800L,    # per-chunk cap in the prompt
  max_tokens  = 3000L,
  timeout     = 120,
  max_turns   = 20L)

# The store ships inside the app folder so Connect deploys it with the code.
svy_app_dir  <- function() getOption("svymeth.app_dir", getwd())
svy_store_path <- function() file.path(svy_app_dir(), "store", "jpsm.duckdb")

# Course materials repository; sources link here.
SVY_REPO_URL <- "https://github.com/klinares/UMD_JPSM_coursework"
