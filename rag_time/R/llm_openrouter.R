# llm_openrouter.R -- OPTIONAL. Points the app at OpenRouter instead of OpenAI.
#   Same ellmer code path (R/llm.R); only the endpoint, key and model names
#   change. Where OpenRouter is not available, DELETE THIS FILE: the app then
#   uses the OpenAI settings in R/config.R. Nothing else references it.
#
# openai/text-embedding-3-small on OpenRouter is the same model as OpenAI's
#   text-embedding-3-small, so a store built through either works with both.
SVY_LLM_OVERRIDE <- list(
  name     = "openrouter",
  base_url = "https://openrouter.ai/api/v1",
  key_var  = "OPENROUTER_API_KEY",
  chat     = "meta-llama/llama-4-maverick",
  worker   = "google/gemma-4-31b-it",
  embed    = "openai/text-embedding-3-small")
