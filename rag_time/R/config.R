# config.R -- endpoint, models, paths. Committed on purpose: these are
#   properties of the deployment, not of the analyst. The analyst supplies one
#   thing, the key.
#
# Every model call goes through ellmer (R/llm.R) to an OpenAI endpoint.
#   EDIT chat / worker to the model names your endpoint serves. Environment
#   variables of the same name override them without editing this file.
#
# THE EMBEDDING MODEL AND DIMENSIONS MUST MATCH THE STORE. The store records
#   which model built it; the app refuses to search with a different one,
#   because vectors from two models are not comparable and the failure would
#   otherwise be silent (plausible but wrong sources).
SVY_LLM <- list(
  name     = "openai",
  base_url = Sys.getenv("SVYMETH_BASE_URL", "https://api.openai.com/v1"),
  key_var  = "OPENAI_API_KEY",
  chat     = Sys.getenv("SVYMETH_CHAT_MODEL",   "gpt-4.1"),
  worker   = Sys.getenv("SVYMETH_WORKER_MODEL", "gpt-4.1-mini"),
  embed    = "text-embedding-3-small")

# An optional R/llm_*.R file may define SVY_LLM_OVERRIDE (a list with the same
#   fields) to point the app at another OpenAI-compatible provider. Delete that
#   file and the app uses SVY_LLM above.
svy_profile <- function() {
  o <- if (exists("SVY_LLM_OVERRIDE", inherits = TRUE)) get("SVY_LLM_OVERRIDE", inherits = TRUE) else list()
  utils::modifyList(SVY_LLM, o)
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
