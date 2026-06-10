# app.R -- Survey Reports Assistant (basic v1)
# Two panels:
#   Chat: RAG over PDF reports + data dictionaries; generation via an
#         OpenAI-compatible API endpoint (BASE_URL in config.R) through ellmer.
#   Plot: deterministic design-based estimates (srvyr) with 95% CIs.
#         The LLM never computes estimates -- it helps you find the variable;
#         the plot panel does the statistics.
#
# Run: shiny::runApp("D:/RAG_reports/app.R", launch.browser = TRUE)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinychat)
  library(ellmer)
  library(ragnar)
  library(srvyr)
  library(haven)
  library(tidyverse)
})
source("D:/RAG_reports/config.R")

store <- ragnar_store_connect(STORE_PATH, read_only = TRUE)

SYSTEM_PROMPT <- str_squish("
  You are an assistant for published survey reports. Answer using ONLY the
  retrieved materials provided in each message. Report point estimates exactly
  as published, with the survey name and year. If the retrieved chunk is a
  data dictionary entry, give the variable name and dataset id and tell the
  user it can be plotted in the Plot panel. If the materials do not contain
  the answer, say so plainly.")

new_chat <- function() {
  chat_openai(base_url = BASE_URL, api_key = API_KEY, model = GEN_MODEL,
              system_prompt = SYSTEM_PROMPT, echo = "none")
}

# Margin-filtered retrieval (same idea as the coursework store).
retrieve_ctx <- function(q) {
  hits <- tryCatch(ragnar_retrieve(store, q, top_k = TOP_K), error = \(e) NULL)
  if (is.null(hits) || nrow(hits) == 0) return(NULL)
  cos <- map_dbl(hits$cosine_distance,
                 \(x) { x <- suppressWarnings(as.numeric(unlist(x)))
                        x <- x[is.finite(x)]; if (length(x)) min(x) else Inf })
  keep <- cos <= (min(cos) + COS_MARGIN)
  hits <- hits[keep, , drop = FALSE]
  list(
    ctx = paste(sprintf("[Source: %s]\n%s", basename(hits$origin %||% "unknown"),
                        hits$text), collapse = "\n\n---\n\n"),
    sources = unique(basename(hits$origin))
  )
}

# --- Survey-design helpers ----------------------------------------------------
load_design <- local({
  cache <- list()
  function(id) {
    if (!is.null(cache[[id]])) return(cache[[id]])
    row <- filter(DATASETS, id == !!id)
    df  <- read_rds(row$rds)
    des <- df |>
      as_survey_design(
        weights = !!sym(row$weight),
        strata  = if (!is.na(row$strata)) sym(row$strata) else NULL,
        ids     = if (!is.na(row$psu))    sym(row$psu)    else 1,
        nest    = TRUE
      )
    cache[[id]] <<- list(df = df, des = des)
    cache[[id]]
  }
})

var_choices <- function(df) {
  labs <- map_chr(df, \(x) attr(x, "label") %||% "")
  nms  <- names(df)
  setNames(nms, ifelse(nzchar(labs), paste0(nms, " -- ", labs), nms))
}

# --- UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = "Survey Reports Assistant",
  theme = bs_theme(bootswatch = "darkly"),

  nav_panel("Chat",
    chat_ui("chat", height = "85vh",
            messages = list(paste(
              "Ask about published estimates, e.g.",
              "*what are the estimates of favorability toward Putin?*")))
  ),

  nav_panel("Plot",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput("dataset", "Dataset", choices = DATASETS$id),
        selectInput("variable", "Variable", choices = NULL),
        selectInput("groupvar", "Group by (optional)", choices = NULL),
        actionButton("draw", "Estimate & plot", class = "btn-primary w-100"),
        helpText("Estimates use the registered complex design",
                 "(weights / strata / PSU) via srvyr, with 95% CIs.")
      ),
      plotOutput("est_plot", height = "70vh"),
      tableOutput("est_table")
    )
  )
)

# --- Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # Chat ------------------------------------------------------------------------
  chat <- new_chat()
  observeEvent(input$chat_user_input, {
    q <- input$chat_user_input
    r <- retrieve_ctx(q)
    if (is.null(r)) {
      chat_append("chat", "No relevant material found in the store.")
      return(invisible())
    }
    ans <- tryCatch(
      chat$chat(paste0("Retrieved materials:\n\n", r$ctx,
                       "\n\n----\n\nQuestion: ", q)),
      error = \(e) paste("API error:", conditionMessage(e))
    )
    chat_append("chat", paste0(
      ans, "\n\n*Sources: ", paste(r$sources, collapse = ", "), "*"))
  })

  # Plot ------------------------------------------------------------------------
  dat <- reactive(load_design(input$dataset))

  observeEvent(input$dataset, {
    ch  <- var_choices(dat()$df)
    row <- filter(DATASETS, id == input$dataset)
    drop <- na.omit(c(row$weight, row$strata, row$psu))
    ch  <- ch[!ch %in% drop]
    updateSelectInput(session, "variable", choices = ch)
    updateSelectInput(session, "groupvar", choices = c("(none)" = "", ch))
  })

  est <- eventReactive(input$draw, {
    des <- dat()$des
    v   <- input$variable
    g   <- if (nzchar(input$groupvar)) input$groupvar else NULL
    col <- dat()$df[[v]]
    categorical <- inherits(col, "haven_labelled") || is.factor(col) ||
                   is.character(col) || n_distinct(col, na.rm = TRUE) <= 10

    des <- des |> mutate(across(any_of(c(v, g)), \(x)
             if (inherits(x, "haven_labelled")) as_factor(x) else x))

    if (categorical) {
      des |>
        filter(!is.na(.data[[v]])) |>
        group_by(across(all_of(c(g, v)))) |>
        summarize(estimate = survey_prop(vartype = "ci"), .groups = "drop")
    } else {
      des |>
        { \(d) if (is.null(g)) d else group_by(d, .data[[g]]) }() |>
        summarize(estimate = survey_mean(.data[[v]], na.rm = TRUE,
                                         vartype = "ci"), .groups = "drop")
    }
  })

  output$est_table <- renderTable(est(), digits = 3)

  output$est_plot <- renderPlot({
    df <- est()
    v  <- input$variable
    g  <- if (nzchar(input$groupvar)) input$groupvar else NULL
    x  <- if (v %in% names(df)) v else (g %||% NULL)
    p  <- ggplot(df, aes(x = .data[[x]], y = estimate,
                         ymin = estimate_low, ymax = estimate_upp)) +
      geom_col(fill = "#4477AA", alpha = 0.85) +
      geom_errorbar(width = 0.2, color = "grey20") +
      labs(x = NULL, y = "Design-based estimate (95% CI)",
           title = paste0(v, " -- ", input$dataset)) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    if (!is.null(g) && g %in% names(df) && !identical(g, x))
      p <- p + facet_wrap(vars(.data[[g]]))
    p
  })
}

shinyApp(ui, server)
