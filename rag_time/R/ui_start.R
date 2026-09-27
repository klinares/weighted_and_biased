# ui_start.R -- tab 1: documentation, the key, and the store status.

mod_start_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(bslib::card_header("Start here"), help_html(),
                shiny::tags$details(shiny::tags$summary(shiny::strong("Reflexes")),
                                    reflex_table_html())),
    bslib::card(
      bslib::card_header("Your API key"),
      shiny::uiOutput(ns("key_status")),
      shiny::passwordInput(ns("key"), NULL, width = "100%",
                           placeholder = "Paste your key here"),
      shiny::div(shiny::actionButton(ns("save"), "Use this key", class = "btn-primary"),
                 shiny::actionButton(ns("clear"), "Forget it")),
      shiny::hr(),
      shiny::h6("Configuration"),
      shiny::uiOutput(ns("config"))))
}

mod_start_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    token <- session$token
    bump <- function() state$key_version <- (state$key_version %||% 0L) + 1L

    shiny::observeEvent(input$save, {
      k <- trimws(input$key %||% "")
      if (!nzchar(k)) return(shiny::showNotification("Nothing pasted.", type = "warning"))
      llm_set_session_key(k)
      shiny::updateTextInput(session, "key", value = "")
      # One embedding call proves the key works before the analyst relies on it.
      ok <- try(embed_texts("key check"), silent = TRUE)
      if (inherits(ok, "try-error")) {
        llm_clear_session_key(token)
        shiny::showNotification(paste("That key did not work:",
                                      conditionMessage(attr(ok, "condition"))),
                                type = "error", duration = NULL)
      } else shiny::showNotification("Key accepted for this session.", type = "message")
      bump()
    })
    shiny::observeEvent(input$clear, { llm_clear_session_key(token); bump() })
    session$onSessionEnded(function() llm_clear_session_key(token))

    output$key_status <- shiny::renderUI({
      state$key_version
      prof <- svy_profile()
      if (!is.null(llm_session_key()))
        shiny::p(class = "text-success", "A key you supplied is in use for this session.")
      else if (identical(SVY_SETTINGS$key_source, "server") && nzchar(Sys.getenv(prof$key_var)))
        shiny::p(class = "text-muted", "Using the key configured on the server. Paste your own to use it instead.")
      else shiny::p(class = "text-warning", "No key yet. The Methodologist tab needs one.")
    })

    output$config <- shiny::renderUI({
      prof <- svy_profile(); m <- state$store$meta %||% list()
      shiny::tags$table(class = "table table-sm small",
        shiny::tags$tbody(purrr::imap(list(
          Provider = prof$name, Endpoint = prof$base_url,
          `Chat model` = prof$chat, `Worker model` = prof$worker,
          `Embedding` = paste0(prof$embed, " (", SVY_SETTINGS$embed_dims, " dims)"),
          `Store built` = m$built_at %||% "--",
          `Store contents` = if (length(m)) paste0(m$n_docs, " files, ", m$n_chunks, " chunks") else "--",
          `Store status` = state$store_error %||% "ready"),
          function(v, k) shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(v)))))
    })
  })
}
