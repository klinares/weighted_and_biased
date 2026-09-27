# deploy.R -- publish to Posit Connect.
#
#   source("deploy.R")
#
# Only what the running app needs is sent: shiny_app.R, R/, the store. The build
#   scripts, the extraction cache and the evaluation stay behind (they need
#   pdftools and the coursework, which Connect does not have).
#
# On Connect, set in the content's Vars pane:
#   SVYMETH_PROFILE    = work
#   SVYMETH_KEY_SOURCE = analyst   (each analyst pastes their own key)
# The embedding model and dims in config.R must match the store you deploy;
#   the app checks and refuses otherwise.

deploy_files <- function()
  c("shiny_app.R", list.files("R", "\\.R$", full.names = TRUE), "store/jpsm.duckdb")

if (sys.nframe() == 0L || identical(environment(), globalenv()))
  rsconnect::deployApp(appDir = ".", appFiles = deploy_files(),
                       appTitle = "RAG Time", appPrimaryDoc = "shiny_app.R", forceUpdate = TRUE)
