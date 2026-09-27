# =============================================================================
# app.R -- JPSM Course Assistant (coursework RAG)
#
#   shiny::runApp("D:/repos/weighted_and_biased/rag_time/course_app", launch.browser = TRUE)
#
# Needs: Ollama running (query embeddings, and the local model), the store at
# D:/RAG_files/jpsm.duckdb, OPENROUTER_API_KEY for the OpenRouter model.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(shinychat)
})
source("D:/repos/weighted_and_biased/rag_time/course_app/rag_core.R")

store <- open_store()
meta  <- load_meta(store)
onStop(function() tryCatch(dbDisconnect(store@con, shutdown = TRUE), error = function(e) NULL))

ui <- page_sidebar(
  title = "JPSM Course Assistant",
  theme = bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(
    width = 290,
    selectInput("model", "Model", choices = MODELS, selected = MODELS[[1]]),
    sliderInput("top_k", "Chunks retrieved", min = 4, max = 12, value = TOP_K, step = 1),
    checkboxInput("show_eq", "Show equations from retrieved cards", TRUE),
    actionButton("new_chat", "New conversation", class = "btn-sm btn-outline-light w-100"),
    hr(),
    helpText(sprintf("Store: %s indexed documents (coursework, textbook, reference cards).",
                     format(nrow(meta), big.mark = ","))),
    helpText("Switching models starts a fresh conversation. Sources in bold were cited.")
  ),
  chat_ui("chat", height = "100%", messages = list(paste(
    "**Ready.** Ask about a method, formula, or assignment; answers come from your",
    "JPSM materials with cited sources.")))
)

server <- function(input, output, session) {
  chat_obj <- reactiveVal(make_chat(MODELS[[1]]))
  reset    <- function() { chat_obj(make_chat(input$model)); chat_clear("chat") }
  observeEvent(input$model, reset(), ignoreInit = TRUE)
  observeEvent(input$new_chat, reset())

  observeEvent(input$chat_user_input, {
    q   <- input$chat_user_input
    nm  <- names(MODELS)[MODELS == input$model]
    res <- withProgress(message = paste0("Thinking (", nm, ")..."), value = 0.5,
                        ask_rag(chat_obj(), store, q, k = input$top_k, meta = meta))
    html <- paste0(
      render_answer(res$answer),
      if (isTRUE(input$show_eq)) equations_html(card_equations(res$hits)) else "",
      sources_html(res$hits, res$answer),
      if (!is.na(res$secs)) sprintf("<div style='font-size:.75em;color:#777'>%.1f s</div>", res$secs) else ""
    )
    chat_append("chat", HTML(html))
  })
}

shinyApp(ui, server)
