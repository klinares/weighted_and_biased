# ui_chat.R -- tab 2: the methodologist. One conversation per session.
#
# Only the bare question is kept in history (trim_last_user_turn): the ~3k
#   tokens of retrieved context sent with each question would otherwise pile
#   up turn after turn. On any failed call the chat object is rebuilt rather
#   than retried against a history that may now hold a partial turn.

trim_last_user_turn <- function(chat, question) {
  tryCatch({
    turns <- chat$get_turns(); n <- length(turns)
    if (n >= 2 && identical(turns[[n - 1]]@role, "user")) {
      turns[[n - 1]] <- if (exists("UserTurn", asNamespace("ellmer")))
        ellmer::UserTurn(list(ellmer::ContentText(question)))
      else ellmer::Turn("user", list(ellmer::ContentText(question)))
      chat$set_turns(turns)
    }
  }, error = function(e) NULL)
  invisible(chat)
}

# A short follow-up ("and how do I do that in R?") carries no search terms of
#   its own, so the previous question is searched with it.
search_query <- function(question, previous = NULL)
  if (!is.null(previous) && nchar(question) < 80) paste(previous, question) else question

ask_methodologist <- function(chat, st, question, k = SVY_SETTINGS$top_k, previous = NULL) {
  t0 <- Sys.time()
  h <- retrieve(st, search_query(question, previous), k)
  if (!nrow(h)) return(list(answer = "Nothing in the course materials matched that question.",
                            hits = h, secs = NA_real_))
  prompt <- paste0("Retrieved course materials:\n\n", build_context(h),
                   "\n\n----\nQuestion: ", question)
  ans <- as.character(chat$chat(prompt, echo = "none"))
  trim_last_user_turn(chat, question)
  list(answer = ans, hits = h,
       secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

answer_html <- function(res) {
  a <- audit_answer(res$answer, res$hits)
  paste0(render_markdown(res$answer), audit_html(a), sources_html(res$hits, a),
         if (!is.na(res$secs)) sprintf("<div class='small text-muted'>%.1f s</div>", res$secs) else "")
}

mod_chat_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 280,
      shiny::selectInput(ns("role"), "Model", choices = c("Chat model" = "chat", "Worker model" = "worker")),
      shiny::sliderInput(ns("k"), "Sources per answer", 4, 12, SVY_SETTINGS$top_k, 1),
      shiny::actionButton(ns("new"), "New conversation", class = "btn-sm w-100"),
      shiny::uiOutput(ns("meter"))),
    shinychat::chat_ui(ns("chat"), height = "calc(100vh - 140px)",
      messages = list("Ask about a method, a formula, or how to do something in R. Answers come from your course materials, with sources.")))
}

mod_chat_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    chat  <- shiny::reactiveVal(NULL)
    turns <- shiny::reactiveVal(0L)
    last_q <- shiny::reactiveVal(NULL)
    reset <- function() { chat(NULL); turns(0L); last_q(NULL); shinychat::chat_clear("chat") }

    shiny::observeEvent(input$role, reset(), ignoreInit = TRUE)
    shiny::observeEvent(input$new, reset())
    # A chat object holds the key it was built with; a new key means a new one.
    shiny::observeEvent(state$key_version, chat(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$chat_user_input, {
      q <- input$chat_user_input
      if (is.null(state$store)) return(shinychat::chat_append("chat",
        paste("The store is not available:", state$store_error)))
      res <- try(shiny::withProgress(message = "Searching your materials and answering...", value = 0.5, {
        if (is.null(chat())) chat(llm_chat(input$role, system_prompt = svy_system_prompt()))
        ask_methodologist(chat(), state$store, q, input$k, previous = last_q())
      }), silent = TRUE)
      if (inherits(res, "try-error")) {
        chat(NULL); turns(0L); last_q(NULL)
        return(shinychat::chat_append("chat", paste0(
          "**That did not work**, so the conversation was reset: ",
          conditionMessage(attr(res, "condition")))))
      }
      turns(turns() + 1L); last_q(q)
      shinychat::chat_append("chat", shiny::HTML(answer_html(res)))
    })

    output$meter <- shiny::renderUI({
      shiny::tagList(shiny::p(class = "small text-muted", turns(), " turn(s)"),
        if (turns() >= SVY_SETTINGS$max_turns)
          shiny::p(class = "small text-warning",
                   "Long conversation: every turn resends the history. Start a new one when this thread is done."))
    })
  })
}
