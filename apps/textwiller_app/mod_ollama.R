# Ollama integration: simple RAG on corpus and custom prompts

mod_ollama_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Ollama (Local LLM)")),
      shiny::column(4, align = "right",
        shiny::actionButton(ns("calculate"), "Execute request", class = "btn-success")
      )
    ),
    shiny::wellPanel(
      shiny::p("Send prompts to the local Ollama server to run RAG on the corpus or analyze data produced by TextWiller."),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("mode"), "Mode", choices = c("RAG on corpus" = "rag", "Prompt with data" = "prompt"), selected = "rag")
        ),
      shiny::column(4,
        shiny::uiOutput(ns("model_picker"))
      ),
        shiny::column(4, shiny::numericInput(ns("temperature"), "Temperature", value = 0.2, min = 0, max = 1, step = 0.05))
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'rag'", ns("mode")),
        shiny::textInput(ns("rag_query"), "Question on the corpus", placeholder = "E.g. Summarize the main themes"),
        shiny::numericInput(ns("rag_docs"), "Number of documents to use", value = 10, min = 1, max = 200, step = 1)
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'prompt'", ns("mode")),
        shiny::textInput(ns("prompt_text"), "Prompt to send", placeholder = "E.g. Analyze the keywords and propose insights"),
        shiny::selectInput(ns("context_source"), "Data source", choices = c("Imported corpus" = "raw", "Preprocessed corpus" = "processed", "Custom text" = "custom"), selected = "raw"),
        shiny::numericInput(ns("context_docs"), "Documents to include (if corpus)", value = 5, min = 1, max = 200, step = 1),
        shiny::numericInput(ns("context_chars"), "Max context characters", value = 4000, min = 500, max = 20000, step = 500),
        shiny::textAreaInput(ns("custom_context"), "Custom context (optional)", rows = 4, placeholder = "Paste other statistics or outputs from other tabs")
      ),
      shiny::uiOutput(ns("history_selector")),
      shiny::actionButton(ns("run"), "Send to Ollama", class = "btn-primary btn-block")
    ),
    shiny::wellPanel(
      shiny::h4("Log"),
      shiny::verbatimTextOutput(ns("log"), placeholder = TRUE)
    ),
    shiny::wellPanel(
      shiny::h4("Model response"),
      shiny::verbatimTextOutput(ns("response"), placeholder = TRUE)
    )
  )
}

mod_ollama_server <- function(id, corpus, processed_corpus = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    log_lines <- shiny::reactiveVal("[init] Ollama ready.")
    response_text <- shiny::reactiveVal("")
    model_choices <- shiny::reactiveVal(c("llama3", "mistral", "phi3"))
    history_payloads <- shiny::reactiveVal(list())
    `%||%` <- function(x, y) if (!is.null(x)) x else y

    get_ollama_host <- function() {
      raw_host <- trimws(Sys.getenv("OLLAMA_HOST", unset = "http://localhost:11434"))
      if (!nzchar(raw_host)) raw_host <- "http://localhost:11434"
      if (!grepl("^https?://", raw_host, ignore.case = TRUE)) {
        raw_host <- paste0("http://", raw_host)
        append_log("OLLAMA_HOST missing scheme: added 'http://'.")
      }
      sub("/+$", "", raw_host)
    }

    append_log <- function(msg) {
      ts <- format(Sys.time(), "%H:%M:%S")
      log_lines(paste0(log_lines(), "\n[", ts, "] ", msg))
    }

    safe_log_action <- function(operation, parameters = list(), input_state = NULL, output_state = NULL) {
      log_fun <- NULL
      if (exists("log_reproducibility_action", envir = .GlobalEnv, mode = "function")) {
        log_fun <- get("log_reproducibility_action", envir = .GlobalEnv)
      } else if (requireNamespace("TextWiller3", quietly = TRUE) &&
                 exists("log_reproducibility_action", envir = asNamespace("TextWiller3"), mode = "function")) {
        log_fun <- get("log_reproducibility_action", envir = asNamespace("TextWiller3"))
      }
      if (is.null(log_fun)) return(invisible(NULL))
      try(
        log_fun(
          module = "ollama",
          operation = operation,
          parameters = parameters,
          input_state = input_state,
          output_state = output_state
        ),
        silent = TRUE
      )
    }

    has_ollama_client <- function() {
      if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
        shiny::showNotification("Packages 'httr' and 'jsonlite' are required to call Ollama.", type = "error")
        append_log("Missing httr/jsonlite packages.")
        return(FALSE)
      }
      TRUE
    }

    list_models <- function() {
      if (!has_ollama_client()) return(NULL)
      host <- get_ollama_host()
      url <- paste0(host, "/api/tags")
      req <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) e)
      if (inherits(req, "error")) {
        append_log(paste("Error fetching models:", conditionMessage(req)))
        return(NULL)
      }
      if (httr::http_error(req)) {
        append_log(paste("Unable to retrieve model list (HTTP", httr::status_code(req), ")."))
        return(NULL)
      }
      res <- tryCatch(jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8")), error = function(e) NULL)
      if (is.null(res) || is.null(res$models)) return(NULL)
      mods <- res$models$name
      mods
    }

    update_model_input <- function(choices) {
      current <- input$model
      choices <- if (!is.null(choices) && length(choices) > 0) choices else model_choices()
      # Keep any manually typed model in the list so it stays selectable
      if (!is.null(current) && nzchar(current) && !(current %in% choices)) {
        choices <- c(current, choices)
      }
      selected <- if (!is.null(current) && nzchar(current) && current %in% choices) current else head(choices, 1)
      shiny::updateSelectizeInput(session, "model", choices = choices, selected = selected, server = FALSE)
    }

    refresh_model_choices <- function() {
      mods <- list_models()
      if (!is.null(mods) && length(mods) > 0) {
        model_choices(mods)
        append_log(paste("Detected models:", paste(mods, collapse = ", ")))
      } else {
        append_log("Using default model list.")
      }
      update_model_input(model_choices())
    }

    get_history_steps <- function() {
      if (!requireNamespace("TextWiller3", quietly = TRUE)) return(list())
      hist <- tryCatch(TextWiller3::get_auto_history(), error = function(e) NULL)
      if (is.null(hist) || is.null(hist$steps)) return(list())
      hist$steps
    }

    format_history_step <- function(step) {
      ts <- format(step$timestamp, "%H:%M:%S")
      op <- paste0(step$module, "/", step$operation)
      label <- paste(ts, "-", op)
      payload <- tryCatch({
        jsonlite::toJSON(step$output_state, auto_unbox = TRUE, pretty = TRUE)
      }, error = function(e) {
        paste(capture.output(str(step$output_state)), collapse = "\n")
      })
      list(id = step$id, label = label, payload = payload)
    }

    refresh_history_choices <- function() {
      steps <- get_history_steps()
      if (length(steps) == 0) {
        history_payloads(list())
        shiny::updateCheckboxGroupInput(session, "history_items", choices = list(), selected = character(0))
        return()
      }
      formatted <- lapply(steps, format_history_step)
      choices <- setNames(vapply(formatted, `[[`, character(1), "id"), vapply(formatted, `[[`, character(1), "label"))
      payload_map <- setNames(lapply(formatted, `[[`, "payload"), vapply(formatted, `[[`, character(1), "id"))
      history_payloads(payload_map)
      shiny::updateCheckboxGroupInput(session, "history_items", choices = choices, selected = input$history_items %||% character(0))
    }

    output$model_picker <- shiny::renderUI({
      shiny::tagList(
        shiny::selectizeInput(
          ns("model"),
          "Ollama model",
          choices = model_choices(),
          selected = head(model_choices(), 1),
          options = list(create = TRUE, placeholder = "Select or type a model")
        ),
        shiny::actionButton(ns("refresh_models"), "Refresh model list", class = "btn-default btn-sm")
      )
    })

    output$history_selector <- shiny::renderUI({
      shiny::tagList(
        shiny::div(
          shiny::strong("Analysis outputs to include"),
          shiny::p("Select results from past analyses (automatic history).")
        ),
        shiny::checkboxGroupInput(
          ns("history_items"),
          label = NULL,
          choices = list(),
          selected = character(0)
        ),
        shiny::actionButton(ns("refresh_history"), "Refresh available outputs", class = "btn-default btn-sm")
      )
    })

    build_context <- function() {
      mode <- input$mode
      if (mode == "rag") {
        docs <- corpus()
        if (is.null(docs) || length(docs) == 0) {
          append_log("No corpus available for RAG.")
          return(NULL)
        }
        n <- min(length(docs), input$rag_docs)
        ctx <- paste(head(docs, n), collapse = "\n---\n")
        return(ctx)
      } else {
        src <- input$context_source
        ctx <- ""
        if (src %in% c("raw", "processed")) {
          docs <- if (src == "processed" && !is.null(processed_corpus) && !is.null(processed_corpus())) {
            processed_corpus()
          } else {
            corpus()
          }
          if (is.null(docs) || length(docs) == 0) {
            append_log("No text available for context.")
          } else {
            n <- min(length(docs), input$context_docs)
            ctx <- paste(head(docs, n), collapse = "\n---\n")
          }
        }
        if (nzchar(input$custom_context)) {
          ctx <- paste(ctx, input$custom_context, sep = "\n")
        }
        if (!is.null(input$history_items) && length(input$history_items) > 0) {
          payloads <- history_payloads()
          selected_payloads <- vapply(input$history_items, function(id) payloads[[id]] %||% "", character(1))
          selected_payloads <- selected_payloads[nzchar(selected_payloads)]
          if (length(selected_payloads) > 0) {
            ctx <- paste(ctx, paste(selected_payloads, collapse = "\n---\n"), sep = "\n")
          }
        }
        max_chars <- input$context_chars
        if (!is.null(max_chars) && nchar(ctx) > max_chars) {
          ctx <- substr(ctx, 1, max_chars)
          ctx <- paste0(ctx, "\n[truncated to ", max_chars, " characters]")
        }
        return(ctx)
      }
    }

    call_ollama <- function(prompt, model, temperature) {
      host <- get_ollama_host()
      url <- paste0(host, "/api/generate")
      body <- list(
        model = model,
        prompt = prompt,
        stream = FALSE,
        options = list(temperature = temperature)
      )
      append_log(paste("Calling Ollama at", url))
      req <- tryCatch(
        httr::POST(url, body = body, encode = "json", httr::timeout(120)),
        error = function(e) e
      )
      if (inherits(req, "error")) {
        stop("HTTP error: ", conditionMessage(req))
      }
      if (httr::http_error(req)) {
        stop("HTTP status: ", httr::status_code(req))
      }
      res <- tryCatch(
        jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8")),
        error = function(e) stop("Parse error: ", conditionMessage(e))
      )
      if (!is.null(res$error)) {
        stop(res$error)
      }
      res$response %||% ""
    }

    run_request <- function() {
      append_log("Run button clicked.")
      if (!has_ollama_client()) return()
      append_log("Preparing request to Ollama.")
      model <- input$model
      temp <- input$temperature
      ctx <- build_context()
      if (is.null(ctx)) {
        shiny::showNotification("Missing context: load a corpus or provide text.", type = "warning")
        append_log("Stopped: missing context.")
        return()
      }

      prompt <- if (input$mode == "rag") {
        qry <- input$rag_query
        if (!nzchar(qry)) {
          shiny::showNotification("Enter a question for RAG.", type = "warning")
          append_log("Stopped: missing RAG question.")
          return()
        }
        paste("Context:\n", ctx, "\n\nQuestion:\n", qry)
      } else {
        base_prompt <- if (nzchar(input$prompt_text)) input$prompt_text else "Analyze the following context and provide insights."
        paste(base_prompt, "\n\nContext:\n", ctx)
      }

      append_log(paste("Mode:", input$mode, "- model:", model))
      append_log(paste("Prompt length:", nchar(prompt), "characters"))
      shiny::showNotification("Calling Ollama...", type = "message", duration = NULL, id = ns("ollama_progress"))
      res <- tryCatch({
        call_ollama(prompt, model, temp)
      }, error = function(e) {
        append_log(paste("Ollama error:", conditionMessage(e)))
        shiny::showNotification(paste("Ollama error:", conditionMessage(e)), type = "error", id = ns("ollama_progress"))
        return(NULL)
      })
      shiny::removeNotification(id = ns("ollama_progress"))
      if (!is.null(res)) {
        response_text(res)
        append_log("Response received.")
        shiny::showNotification("Response received from Ollama.", type = "message", duration = 3)
        safe_log_action(
          operation = "ollama_request",
          parameters = list(mode = input$mode, model = model, temperature = temp),
          input_state = list(prompt_chars = nchar(prompt)),
          output_state = list(response_chars = nchar(res))
        )
      } else {
        append_log("No response from the Ollama server.")
      }
    }

    shiny::observeEvent(input$run, {
      append_log(paste("Run event trigger value:", input$run))
      tryCatch(run_request(), error = function(e) {
        append_log(paste("run_request error:", e$message))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$calculate, {
      append_log(paste("Calculate event trigger value:", input$calculate))
      tryCatch(run_request(), error = function(e) {
        append_log(paste("run_request error:", e$message))
      })
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$refresh_models, {
      append_log("Requested refresh of model list.")
      refresh_model_choices()
    })
    shiny::observeEvent(input$refresh_history, {
      append_log("Refreshing available analysis outputs.")
      refresh_history_choices()
    })
    shiny::observeEvent(session, {
      refresh_model_choices()
      refresh_history_choices()
    }, once = TRUE)

    output$log <- shiny::renderText(log_lines())
    output$response <- shiny::renderText(response_text())
  })
}
