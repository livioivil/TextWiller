#' Automatic Analysis History Tracking
#' 
#' Tracks all user interactions automatically for complete reproducibility
#' 

#' Analysis Step Class
#' @export
AnalysisStep <- R6::R6Class(
  "AnalysisStep",
  public = list(
    id = NULL,
    timestamp = NULL,
    module = NULL,
    operation = NULL,
    parameters = NULL,
    input_state = NULL,
    output_state = NULL,
    
    initialize = function(module, operation, parameters, input_state, id = NULL, timestamp = NULL) {
      self$id <- if (is.null(id)) paste0("step_", as.integer(Sys.time()), "_", sample(1000:9999, 1)) else id
      self$timestamp <- if (is.null(timestamp)) Sys.time() else timestamp
      self$module <- module
      self$operation <- operation
      self$parameters <- parameters
      self$input_state <- input_state
    },
    
    set_output = function(output_state) {
      self$output_state <- output_state
    },
    
    to_list = function() {
      list(
        id = self$id,
        timestamp = self$timestamp,
        module = self$module,
        operation = self$operation,
        parameters = self$parameters,
        input_state_summary = digest::digest(self$input_state),
        output_state_summary = digest::digest(self$output_state)
      )
    },
    
    to_serializable = function() {
      list(
        id = self$id,
        timestamp = self$timestamp,
        module = self$module,
        operation = self$operation,
        parameters = self$parameters,
        input_state = self$input_state,
        output_state = self$output_state
      )
    }
  )
)

#' Automatic History Manager
#' @export
AutoHistory <- R6::R6Class(
  "AutoHistory",
  public = list(
    steps = list(),
    current_state = NULL,
    
    initialize = function() {
      self$current_state <- list(
        corpus = NULL,
        preprocessing = NULL,
        analysis = NULL
      )
    },
    
    # Track any user action automatically
    track_action = function(module, operation, parameters, input_data, output_data) {
      step <- AnalysisStep$new(module, operation, parameters, input_data)
      step$set_output(output_data)
      self$steps <- c(self$steps, step)
      
      # Update current state
      if (module == "corpus_io") {
        self$current_state$corpus <- output_data
      } else if (module == "preprocessing") {
        self$current_state$preprocessing <- output_data
      } else if (module == "analysis") {
        self$current_state$analysis <- output_data
      }
      
      cat("📝 Auto-tracked:", module, "-", operation, "\n")
      return(step$id)
    },
    
    # Get step by ID
    get_step = function(step_id) {
      for (step in self$steps) {
        if (step$id == step_id) return(step)
      }
      return(NULL)
    },
    
    # Export reproducible script
    export_reproducible_script = function() {
      script_lines <- c(
        "# TextWiller3 Reproducible Analysis Script",
        "# Generated:", as.character(Sys.time()),
        "# Steps:", length(self$steps),
        "",
        "library(TextWiller3)",
        ""
      )
      
      # Group by module for better organization
      modules <- sapply(self$steps, function(x) x$module)
      unique_modules <- unique(modules)
      
      for (module in unique_modules) {
        module_steps <- self$steps[modules == module]
        
        script_lines <- c(script_lines, paste0("# ", toupper(module), " MODULE"))
        
        for (step in module_steps) {
          # Format parameters for R code
          param_str <- paste(names(step$parameters), "=", 
                            sapply(step$parameters, function(x) {
                              if(is.character(x)) paste0('"', x, '"') 
                              else if(is.logical(x)) as.character(x)
                              else x
                            }), collapse = ", ")
          
          script_lines <- c(script_lines,
            paste0("# ", step$operation, " - ", format(step$timestamp, "%H:%M:%S")),
            paste0("result_", step$id, " <- ", step$operation, "(", param_str, ")"),
            ""
          )
        }
        script_lines <- c(script_lines, "")
      }
      
      return(paste(script_lines, collapse = "\n"))
    },
    
    # Get history summary
    get_summary = function() {
      summary <- data.frame(
        Step = seq_along(self$steps),
        Time = sapply(self$steps, function(x) format(x$timestamp, "%H:%M:%S")),
        Module = sapply(self$steps, function(x) x$module),
        Operation = sapply(self$steps, function(x) x$operation),
        Parameters = sapply(self$steps, function(x) paste(names(x$parameters), collapse = ", "))
      )
      return(summary)
    },
    
    # Clear history
    clear = function() {
      self$steps <- list()
      self$current_state <- list(
        corpus = NULL,
        preprocessing = NULL, 
        analysis = NULL
      )
    },
    
    as_serializable = function() {
      lapply(self$steps, function(step) step$to_serializable())
    },
    
    load_serialized = function(serialized_steps, append = FALSE) {
      if (!is.list(serialized_steps)) {
        stop("Serialized history must be a list of steps")
      }
      if (!append) {
        self$clear()
      }
      for (entry in serialized_steps) {
        step <- AnalysisStep$new(
          module = entry$module,
          operation = entry$operation,
          parameters = entry$parameters %||% list(),
          input_state = entry$input_state,
          id = entry$id,
          timestamp = entry$timestamp
        )
        if (!is.null(entry$output_state)) {
          step$set_output(entry$output_state)
        }
        self$steps <- c(self$steps, step)
      }
    },
    
    save_to_file = function(path) {
      saveRDS(self$as_serializable(), path)
    },
    
    load_from_file = function(path, append = FALSE) {
      serialized <- readRDS(path)
      self$load_serialized(serialized, append = append)
    }
  )
)

#' Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Global Auto History Manager
#' @export
get_auto_history <- function() {
  if (!exists(".textwiller_auto_history", envir = .GlobalEnv)) {
    assign(".textwiller_auto_history", AutoHistory$new(), envir = .GlobalEnv)
  }
  get(".textwiller_auto_history", envir = .GlobalEnv)
}

save_analysis_history <- function(path) {
  get_auto_history()$save_to_file(path)
}

load_analysis_history <- function(path, append = FALSE) {
  get_auto_history()$load_from_file(path, append = append)
}

export_history_script <- function(path) {
  writeLines(get_auto_history()$export_reproducible_script(), path)
}

mod_reproducibility_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    shiny::h4("📜 Analysis History"),
    shiny::p("Esporta o ri-carica la storia delle analisi per garantire piena riproducibilità."),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("refresh_btn"), "Aggiorna", icon = shiny::icon("sync"), class = "btn-primary btn-sm btn-block")
      ),
      shiny::column(6,
        shiny::actionButton(ns("clear_btn"), "Svuota cronologia", icon = shiny::icon("trash"), class = "btn-warning btn-sm btn-block")
      )
    ),
    shiny::br(),
    shiny::tableOutput(ns("history_table")),
    shiny::br(),
    shiny::downloadButton(ns("download_script"), "Scarica script R", class = "btn-success btn-sm"),
    shiny::downloadButton(ns("download_history"), "Scarica history (.rds)", class = "btn-default btn-sm"),
    shiny::br(), shiny::br(),
    shiny::fileInput(ns("upload_history"), "Carica history (.rds)", accept = ".rds"),
    shiny::checkboxInput(ns("append_history"), "Aggiungi alla history esistente", value = TRUE),
    shiny::verbatimTextOutput(ns("script_preview"))
  )
}

mod_reproducibility_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    history <- get_auto_history()
    history_summary <- shiny::reactiveVal(NULL)
    
    refresh_summary <- function() {
      summary <- history$get_summary()
      if (is.null(summary) || nrow(summary) == 0) {
        summary <- data.frame(
          Step = character(0),
          Time = character(0),
          Module = character(0),
          Operation = character(0),
          Parameters = character(0),
          stringsAsFactors = FALSE
        )
      }
      history_summary(summary)
    }
    
    refresh_summary()
    
    shiny::observeEvent(input$refresh_btn, {
      refresh_summary()
      shiny::showNotification("Cronologia aggiornata", type = "message")
    })
    
    shiny::observeEvent(input$clear_btn, {
      history$clear()
      refresh_summary()
      shiny::showNotification("Cronologia azzerata", type = "warning")
    })
    
    shiny::observeEvent(input$upload_history, {
      shiny::req(input$upload_history$datapath)
      tryCatch({
        history$load_from_file(input$upload_history$datapath, append = isTRUE(input$append_history))
        refresh_summary()
        shiny::showNotification("Cronologia importata correttamente", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Errore caricamento history:", e$message), type = "error")
      })
    })
    
    output$history_table <- shiny::renderTable({
      data <- history_summary()
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame(Messaggio = "Nessuna operazione registrata finora"))
      }
      data
    }, bordered = TRUE, striped = TRUE, spacing = "xs")
    
    output$download_script <- shiny::downloadHandler(
      filename = function() paste0("textwiller_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
      content = function(file) {
        export_history_script(file)
      }
    )
    
    output$download_history <- shiny::downloadHandler(
      filename = function() paste0("textwiller_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
      content = function(file) {
        save_analysis_history(file)
      }
    )
    
    output$script_preview <- shiny::renderPrint({
      script <- history$export_reproducible_script()
      lines <- unlist(strsplit(script, "\n"))
      preview <- head(lines, 15)
      cat(paste(preview, collapse = "\n"))
      if (length(lines) > 15) {
        cat("\n...\n")
      }
    })
  })
}
