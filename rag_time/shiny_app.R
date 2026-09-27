# app.R -- AI Survey Methodologist.
#
#   shiny::runApp("shiny_app.R")                              # from this folder
#   source("deploy.R")                           # publish to Posit Connect
#
# Everything in R/ only defines things; this file and the scripts at the root
#   do things. R/ is sourced flat and alphabetically, which is safe only while
#   no two files define the same name.

suppressPackageStartupMessages({
  library(shiny); library(bslib)
})
options(svymeth.app_dir = normalizePath("."))
invisible(lapply(sort(list.files("R", "\\.R$", full.names = TRUE)), source, local = globalenv()))

ui <- page_navbar(
  title = "RAG Time \u00b7 AI Survey Methodologist",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id = "tabs",
  # Model answers use markdown headings; keep them at reading size in the chat.
  header = tags$style(HTML("shiny-chat-message h1, shiny-chat-message h2 { font-size: 1.3rem; margin-top: .8em; } shiny-chat-message h3, shiny-chat-message h4 { font-size: 1.1rem; } shiny-chat-message math[display=block] { font-size: 1.15em; margin: .5em 0; }")),
  nav_panel("1. Start here", mod_start_ui("start")),
  nav_panel("2. Methodologist", mod_chat_ui("meth")))

server <- function(input, output, session) {
  state <- reactiveValues(key_version = 0L, store = NULL, store_error = NULL)

  # One read-only connection per session, closed with it.
  st <- try({ s <- open_store(); check_store_matches(s); s }, silent = TRUE)
  if (inherits(st, "try-error")) state$store_error <- conditionMessage(attr(st, "condition"))
  else state$store <- st
  session$onSessionEnded(function() if (!inherits(st, "try-error")) close_store(st))

  mod_start_server("start", state)
  mod_chat_server("meth", state)
}

shinyApp(ui, server)
