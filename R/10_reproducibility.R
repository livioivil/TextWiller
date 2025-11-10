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
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Global Auto History Manager
#' @export
get_auto_history <- function() {
  if (!exists(".textwiller_auto_history", envir = .GlobalEnv)) {
    assign(".textwiller_auto_history", AutoHistory$new(), envir = .GlobalEnv)
  }
  get(".textwiller_auto_history", envir = .GlobalEnv)
}

#' Backwards-compatible alias for global history access
#'
#' Retained for older modules that still call `get_global_history()`. Internally
#' delegates to `get_auto_history()`.
#' @export
get_global_history <- function() {
  .Deprecated("get_auto_history", package = "TextWiller3")
  get_auto_history()
}

#' Persist the current analysis history to disk
#'
#' @param path Destination `.rds` file
#' @return Invisibly returns the path
#' @export
save_analysis_history <- function(path) {
  history <- get_auto_history()
  history$save_to_file(path)
  invisible(path)
}

#' Load a previously saved history file
#'
#' @param path Path to `.rds` history file
#' @param append Whether to append to the current history (default `FALSE`)
#' @return Invisibly returns the number of steps loaded
#' @export
load_analysis_history <- function(path, append = FALSE) {
  history <- get_auto_history()
  before <- length(history$steps)
  history$load_from_file(path, append = append)
  after <- length(history$steps)
  invisible(after - before)
}

#' Export the analysis history as a runnable script file
#'
#' @param path Destination path for the `.R` script
#' @return Invisibly returns the path
#' @export
export_history_script <- function(path) {
  history <- get_auto_history()
  script <- history$export_reproducible_script()
  writeLines(script, path)
  invisible(path)
}

#' Safely log a reproducibility action
#'
#' Provides a lightweight wrapper around the global AutoHistory tracker so that
#' core package functions can log what happened without failing if history is
#' disabled or not yet initialised.
#' @param module Logical module name (e.g. "corpus_io", "preprocessing")
#' @param operation Function or action name
#' @param parameters Named list of parameter values (character summaries)
#' @param input_state Optional snapshot of inputs
#' @param output_state Optional snapshot of outputs
#' @noRd
log_reproducibility_action <- function(module, operation, parameters = list(),
                                       input_state = NULL, output_state = NULL) {
  if (!isTRUE(getOption("textwiller.reproducibility_enabled", TRUE))) {
    return(invisible(NULL))
  }
  
  history <- tryCatch(get_auto_history(), error = function(e) NULL)
  if (is.null(history)) return(invisible(NULL))
  
  safe_parameters <- parameters
  if (!is.list(safe_parameters)) {
    safe_parameters <- as.list(safe_parameters)
  }
  
  tryCatch({
    history$track_action(
      module = module,
      operation = operation,
      parameters = safe_parameters,
      input_data = input_state,
      output_data = output_state
    )
  }, error = function(e) {
    msg <- paste("Reproducibility logging failed for", operation, ":", e$message)
    message(msg)
  })
  
  invisible(NULL)
}

#' Extract function arguments for reproducibility logging
#' @param call_obj A call object from match.call()
#' @param exclude Optional character vector of argument names to skip
#' @noRd
capture_repro_args <- function(call_obj, exclude = NULL) {
  if (is.null(call_obj)) return(list())
  
  args <- as.list(call_obj)[-1]
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  
  keep_idx <- seq_along(args)
  if (!is.null(exclude) && length(exclude) > 0) {
    keep_idx <- which(!arg_names %in% exclude)
  }
  
  result <- list()
  for (idx in keep_idx) {
    name <- arg_names[idx]
    if (is.null(name) || name == "") {
      name <- paste0("arg", idx)
    }
    arg <- args[[idx]]
    value <- if (is.language(arg) || is.symbol(arg)) {
      paste(deparse(arg, width.cutoff = 80), collapse = "")
    } else {
      arg
    }
    result[[name]] <- value
  }
  
  result
}

#' Snapshot helpers used when logging reproducibility steps
#' @noRd
snapshot_text_vector <- function(text, max_docs = 3, max_chars = 120) {
  if (is.null(text)) {
    return(list(length = 0))
  }
  
  preview <- head(text, max_docs)
  preview <- substr(preview, 1, max_chars)
  
  list(
    length = length(text),
    preview = preview
  )
}

#' @noRd
snapshot_dataframe <- function(data, max_rows = 3, max_chars = 120) {
  if (is.null(data)) {
    return(list(rows = 0, cols = 0))
  }
  
  preview <- utils::head(data, max_rows)
  if (nrow(preview) > 0) {
    text_cols <- names(preview)[sapply(preview, is.character)]
    if (length(text_cols) > 0) {
      for (col in text_cols) {
        preview[[col]] <- substr(preview[[col]], 1, max_chars)
      }
    }
  }
  
  list(
    rows = nrow(data),
    cols = ncol(data),
    columns = names(data),
    preview = preview
  )
}

#' @noRd
snapshot_matrix <- function(mat, max_rows = 3, max_cols = 3) {
  if (is.null(mat)) {
    return(list(dim = c(0, 0)))
  }
  
  dims <- dim(mat)
  if (is.null(dims)) {
    # Handle vectors or dist objects that don't expose dim()
    length_val <- length(mat)
    preview <- utils::head(as.vector(mat), max_rows * max_cols)
    return(list(dim = c(length_val, 1), preview = preview))
  }
  
  preview <- mat[
    seq_len(min(max_rows, dims[1])),
    seq_len(min(max_cols, dims[2])),
    drop = FALSE
  ]
  
  list(
    dim = dims,
    preview = preview
  )
}
