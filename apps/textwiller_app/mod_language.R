mod_language_ui <- function(id) {
  ns <- shiny::NS(id)
  current_lang <- tryCatch({
    TextWiller3::get_analysis_language()
  }, error = function(e) "it")
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("🌍 Language & Resources")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Refresh",
          class = "btn-success"
        )
      )
    ),
    shiny::p("Load stopwords, dictionaries or multi-word lists for Italian and English to power your analyses."),
    
    shiny::fluidRow(
      shiny::column(
        4,
          shiny::selectInput(
            ns("language"),
            "Reference language",
            choices = c("Italian" = "it", "English" = "en"),
            selected = current_lang
          )
      ),
      shiny::column(
        4,
        shiny::textInput(ns("resource_name"), "Resource name", placeholder = "e.g., sentiment_ita_2024")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          ns("resource_type"),
          "Resource type",
          choices = c("Stopwords" = "stopwords", "Lexicon" = "lexicon", "Multiword" = "multiword", "Sentiment" = "sentiment", "UDPipe model" = "udpipe_model"),
          selected = "lexicon"
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::fileInput(
          ns("resource_file"),
          "Upload file (txt, csv, rds, rda, udpipe)",
          accept = c(".txt", ".csv", ".rds", ".rda", ".udpipe")
        )
      ),
      shiny::column(
        6,
        shiny::actionButton(
          ns("add_resource_btn"),
          "Register resource",
          class = "btn-primary",
          icon = shiny::icon("database")
        ),
        shiny::br(),
        shiny::actionButton(
          ns("download_stopwords_btn"),
          "Download stopwords-iso",
          class = "btn-success",
          icon = shiny::icon("cloud-download-alt")
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Cleanup & Maintenance"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::actionButton(
            ns("clean_cache_btn"),
            "🧹 Clear Cache",
            class = "btn-warning",
            icon = shiny::icon("broom")
          )
        ),
        shiny::column(4,
          shiny::actionButton(
            ns("clean_resources_btn"),
            "🗑️ Remove All Resources",
            class = "btn-danger",
            icon = shiny::icon("trash")
          )
        ),
        shiny::column(4,
          shiny::actionButton(
            ns("clean_temp_btn"),
            "🧽 Clean Temporary Files",
            class = "btn-info",
            icon = shiny::icon("recycle")
          )
        )
      ),
      shiny::helpText("Cache: stored models and embeddings | Resources: registered language resources | Temp: session files"),
      shiny::verbatimTextOutput(ns("cleanup_status"))
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Resource Preview"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::selectInput(ns("preview_resource"), "Select resource to preview", choices = character(0))
            ),
            shiny::column(6,
              shiny::numericInput(ns("preview_lines"), "Preview lines:", value = 20, min = 5, max = 100, step = 5)
            )
          ),
          shiny::verbatimTextOutput(ns("resource_preview"), placeholder = TRUE)
        )
      )
    ),
    
      shiny::hr(),
      shiny::h4("Registered resources"),
      DT::dataTableOutput(ns("resource_table")),
      shiny::hr(),
      shiny::h4("Useful links"),
      shiny::p("Ready-to-use UDPipe models:", 
        shiny::tags$a("https://github.com/jwijffels/udpipe.models.ud", 
                      href = "https://github.com/jwijffels/udpipe.models.ud", target = "_blank")
      )
  )
}

mod_language_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang_cfg <- tryCatch({
      TextWiller3::get_language_config()
    }, error = function(e) {
      # Create a simple config if TextWiller3 not available
      list(
        set_language = function(lang) {},
        get_version = function() 0,
        list_resources = function(...) data.frame(),
        get_resource_content = function(...) NULL
      )
    })
    
    storage_root <- file.path(tools::R_user_dir("TextWiller3", "data"), "language_resources")
    cache_root <- tools::R_user_dir("TextWiller3", "cache")
    
    ensure_storage_dir <- function(language) {
      path <- file.path(storage_root, language)
      if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
      path
    }
    
    # Reactive value for cleanup status
    cleanup_status <- shiny::reactiveVal("Ready for cleanup operations.")
    
    # Function to calculate disk usage
    get_disk_usage <- function() {
      usage <- list()
      
      # Check cache directory
      if (dir.exists(cache_root)) {
        cache_files <- list.files(cache_root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
        cache_size <- sum(file.size(cache_files), na.rm = TRUE) / (1024^2) # MB
        usage$cache <- list(files = length(cache_files), size_mb = round(cache_size, 2))
      } else {
        usage$cache <- list(files = 0, size_mb = 0)
      }
      
      # Check resources directory
      if (dir.exists(storage_root)) {
        resource_files <- list.files(storage_root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
        resource_size <- sum(file.size(resource_files), na.rm = TRUE) / (1024^2) # MB
        usage$resources <- list(files = length(resource_files), size_mb = round(resource_size, 2))
      } else {
        usage$resources <- list(files = 0, size_mb = 0)
      }
      
      # Check temporary files in session temp directory
      temp_patterns <- c(
        "^textwiller_", "^embeddings_", "^corpus_", 
        "^udpipe_temp", "^shinyapp", "Rtmp"
      )
      temp_dir <- tempdir()
      temp_files <- list.files(temp_dir, full.names = TRUE, pattern = paste(temp_patterns, collapse = "|"))
      temp_size <- sum(file.size(temp_files), na.rm = TRUE) / (1024^2) # MB
      usage$temp <- list(files = length(temp_files), size_mb = round(temp_size, 2))
      
      usage
    }
    
    # Update cleanup status display
    update_cleanup_status <- function() {
      usage <- get_disk_usage()
      status <- paste(
        "Current disk usage:\n",
        sprintf("📁 Cache: %d files (%.2f MB)\n", usage$cache$files, usage$cache$size_mb),
        sprintf("🗂️ Resources: %d files (%.2f MB)\n", usage$resources$files, usage$resources$size_mb),
        sprintf("🧹 Temp files: %d files (%.2f MB)", usage$temp$files, usage$temp$size_mb)
      )
      cleanup_status(status)
    }
    
    # Initialize status
    shiny::observe({
      update_cleanup_status()
    })
    
    # Clean cache directory
    shiny::observeEvent(input$clean_cache_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Clear Cache",
        "Are you sure you want to clear all cached files (embeddings, models, etc.)?",
        footer = tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_cache_clean"), "Yes, Clear Cache", class = "btn-danger")
        )
      ))
    })
    
    shiny::observeEvent(input$confirm_cache_clean, {
      shiny::removeModal()
      
      if (dir.exists(cache_root)) {
        tryCatch({
          # List all files in cache
          cache_files <- list.files(cache_root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
          cache_files <- cache_files[!grepl("/\\.$", cache_files)]  # Exclude . and ..
          
          if (length(cache_files) > 0) {
            deleted <- 0
            total_size <- 0
            
            for (file in cache_files) {
              if (file.exists(file)) {
                file_size <- file.size(file)
                unlink(file, recursive = TRUE, force = TRUE)
                deleted <- deleted + 1
                total_size <- total_size + ifelse(is.na(file_size), 0, file_size)
              }
            }
            
            total_size_mb <- round(total_size / (1024^2), 2)
            shiny::showNotification(
              sprintf("Cleared cache: %d files (%.2f MB) removed", deleted, total_size_mb),
              type = "success"
            )
            
            # Also clear any cached objects in memory
            if (exists(".textwiller_cache", envir = .GlobalEnv)) {
              rm(".textwiller_cache", envir = .GlobalEnv)
            }
            
          } else {
            shiny::showNotification("Cache directory is already empty", type = "info")
          }
        }, error = function(e) {
          shiny::showNotification(paste("Error clearing cache:", e$message), type = "error")
        })
      } else {
        shiny::showNotification("Cache directory does not exist", type = "info")
      }
      
      update_cleanup_status()
    })
    
    # Clean resources directory
    shiny::observeEvent(input$clean_resources_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Remove All Resources",
        "WARNING: This will delete all registered language resources (stopwords, lexicons, models). This action cannot be undone!",
        footer = tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_resources_clean"), "Yes, Delete All", class = "btn-danger")
        )
      ))
    })
    
    shiny::observeEvent(input$confirm_resources_clean, {
      shiny::removeModal()
      
      if (dir.exists(storage_root)) {
        tryCatch({
          # List all files in resources
          resource_files <- list.files(storage_root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
          resource_files <- resource_files[!grepl("/\\.$", resource_files)]  # Exclude . and ..
          
          if (length(resource_files) > 0) {
            deleted <- 0
            total_size <- 0
            
            for (file in resource_files) {
              if (file.exists(file)) {
                file_size <- file.size(file)
                unlink(file, recursive = TRUE, force = TRUE)
                deleted <- deleted + 1
                total_size <- total_size + ifelse(is.na(file_size), 0, file_size)
              }
            }
            
            total_size_mb <- round(total_size / (1024^2), 2)
            shiny::showNotification(
              sprintf("Removed resources: %d files (%.2f MB) deleted", deleted, total_size_mb),
              type = "success"
            )
            
            # Also clear the language configuration
            tryCatch({
              if (exists(".textwiller_language_config", envir = .GlobalEnv)) {
                config <- get(".textwiller_language_config", envir = .GlobalEnv)
                if (methods::is(config, "R6") && exists("clear", envir = config)) {
                  config$clear()
                }
              }
            }, error = function(e) {
              # Ignore errors here
            })
            
          } else {
            shiny::showNotification("Resources directory is already empty", type = "info")
          }
        }, error = function(e) {
          shiny::showNotification(paste("Error removing resources:", e$message), type = "error")
        })
      } else {
        shiny::showNotification("Resources directory does not exist", type = "info")
      }
      
      update_cleanup_status()
    })
    
    # Clean temporary files
    shiny::observeEvent(input$clean_temp_btn, {
      temp_dir <- tempdir()
      temp_patterns <- c(
        "^textwiller_", "^embeddings_", "^corpus_", 
        "^udpipe_temp", "^shinyapp", "Rtmp.*\\.rds$", "Rtmp.*\\.csv$"
      )
      
      tryCatch({
        temp_files <- list.files(temp_dir, full.names = TRUE, 
                               pattern = paste(temp_patterns, collapse = "|"))
        
        # Also look for recent temporary files (last 24 hours)
        all_files <- list.files(temp_dir, full.names = TRUE)
        file_info <- file.info(all_files)
        recent_files <- all_files[
          !is.na(file_info$mtime) & 
          difftime(Sys.time(), file_info$mtime, units = "hours") < 24
        ]
        
        temp_files <- unique(c(temp_files, recent_files))
        temp_files <- temp_files[file.exists(temp_files)]
        
        if (length(temp_files) > 0) {
          deleted <- 0
          total_size <- 0
          
          for (file in temp_files) {
            if (file.exists(file) && !dir.exists(file)) {
              file_size <- file.size(file)
              success <- file.remove(file)
              if (success) {
                deleted <- deleted + 1
                total_size <- total_size + ifelse(is.na(file_size), 0, file_size)
              }
            }
          }
          
          total_size_mb <- round(total_size / (1024^2), 2)
          shiny::showNotification(
            sprintf("Cleaned temp files: %d files (%.2f MB) removed", deleted, total_size_mb),
            type = "success"
          )
        } else {
          shiny::showNotification("No temporary files found to clean", type = "info")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error cleaning temp files:", e$message), type = "error")
      })
      
      update_cleanup_status()
    })
    
    # Display cleanup status
    output$cleanup_status <- shiny::renderPrint({
      cat(cleanup_status())
    })
    
    # ... [rest of the existing server code remains the same] ...
    resource_data <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(input$calculate, input$language, input$add_resource_btn),
      valueFunc = function() {
        tryCatch({
          TextWiller3::list_language_resources(language = input$language, include_content = FALSE)
        }, error = function(e) {
          data.frame(name = character(), type = character(), language = character(), 
                    source = character(), created = character(), stringsAsFactors = FALSE)
        })
      }
    )
    
    update_preview_choices <- function() {
      resources <- resource_data()
      choices <- c("None" = "")
      if (is.data.frame(resources) && nrow(resources) > 0) {
        label <- paste0(resources$name, " [", resources$type, "]")
        names(label) <- resources$name
        choices <- c("None" = "", label)
      }
      shiny::updateSelectInput(session, "preview_resource", choices = choices)
    }
    
    shiny::observe({
      resource_data()
      update_preview_choices()
    })
    
    shiny::observeEvent(input$language, {
      tryCatch({
        TextWiller3::set_analysis_language(input$language)
        shiny::showNotification(
          paste("Language set to", ifelse(input$language == "it", "Italian", "English")),
          type = "message"
        )
      }, error = function(e) {
        # Fallback if TextWiller3 not available
        shiny::showNotification(paste("Language set to", input$language), type = "message")
      })
    })
    
    shiny::observeEvent(input$add_resource_btn, {
      shiny::req(input$resource_file)
      resource_name <- if (nzchar(input$resource_name)) input$resource_name else tools::file_path_sans_ext(input$resource_file$name)
      ext <- tolower(tools::file_ext(input$resource_file$name))
      
      # For heavy resources like UDPipe models save to disk and register the path
      if (ext == "udpipe" || input$resource_type == "udpipe_model") {
        target_dir <- ensure_storage_dir(input$language)
        target_path <- file.path(target_dir, basename(input$resource_file$name))
        ok <- file.copy(input$resource_file$datapath, target_path, overwrite = TRUE)
        if (!isTRUE(ok)) {
          shiny::showNotification("Cannot save UDPipe model.", type = "error")
          return()
        }
        tryCatch({
          TextWiller3::register_language_resource_file(
            language = input$language,
            name = resource_name,
            path = target_path,
            type = "udpipe_model"
          )
          shiny::showNotification(paste("Registered UDPipe model:", basename(target_path)), type = "message")
          update_preview_choices()
        }, error = function(e) {
          shiny::showNotification(paste("Error registering model:", e$message), type = "error")
        })
        return()
      }
      
      content <- tryCatch({
        if (ext == "rds") {
          readRDS(input$resource_file$datapath)
        } else if (ext == "rda") {
          env <- new.env()
          load(input$resource_file$datapath, envir = env)
          as.list(env)
        } else if (ext %in% c("csv", "tsv")) {
          read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
        } else {
          # Text file
          lines <- readLines(input$resource_file$datapath, encoding = "UTF-8", warn = FALSE)
          lines[!is.na(lines) & nchar(lines) > 0]
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message), type = "error")
        NULL
      })
      shiny::req(!is.null(content))
      
      tryCatch({
        TextWiller3::register_language_resource(
          language = input$language,
          name = resource_name,
          content = content,
          type = input$resource_type
        )
        shiny::showNotification(paste("Registered resource:", resource_name), type = "message")
        update_preview_choices()
      }, error = function(e) {
        shiny::showNotification(paste("Error registering resource:", e$message), type = "error")
      })
    })
    
    shiny::observeEvent(input$download_stopwords_btn, {
      tryCatch({
        name <- paste0("stopwords_iso_", input$language, "_", format(Sys.time(), "%H%M%S"))
        words <- TextWiller3::download_stopwords_iso(language = input$language, auto_register = TRUE, resource_name = name)
        shiny::showNotification(paste("Downloaded", length(words), "stopwords from stopwords-iso"), type = "message")
        update_preview_choices()
      }, error = function(e) {
        shiny::showNotification(paste("Error downloading stopwords:", e$message), type = "error")
      })
    })
    
    output$resource_table <- DT::renderDataTable({
      data <- resource_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(
          name = character(),
          type = character(),
          language = character(),
          source = character(),
          created = character(),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        data,
        options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$resource_preview <- shiny::renderPrint({
      req(input$preview_resource)
      if (input$preview_resource == "" || input$preview_resource == "None") {
        cat("Select a resource to preview its content.")
        return()
      }
      
      # Get resource content safely
      content <- tryCatch({
        TextWiller3::get_language_resource(
          language = input$language,
          name = input$preview_resource
        )
      }, error = function(e) NULL)
      
      if (is.null(content)) {
        # Try to get from the resource data
        resources <- resource_data()
        if (is.data.frame(resources) && input$preview_resource %in% resources$name) {
          idx <- which(resources$name == input$preview_resource)
          path_val <- resources$path[idx]
          if (!is.na(path_val) && file.exists(path_val)) {
            ext <- tools::file_ext(path_val)
            if (ext == "rds") {
              content <- readRDS(path_val)
            } else if (ext == "rda") {
              env <- new.env()
              load(path_val, envir = env)
              content <- as.list(env)
            } else if (ext %in% c("csv", "tsv")) {
              content <- read.csv(path_val, stringsAsFactors = FALSE)
            } else {
              content <- readLines(path_val, encoding = "UTF-8", warn = FALSE)
            }
          }
        }
      }
      
      if (is.null(content)) {
        cat("No information available for the selected resource.")
      } else if (is.character(content) && length(content) == 1 && file.exists(content)) {
        cat("Resource on disk:", content, "\n\n")
        # Try to preview file content
        if (tools::file_ext(content) %in% c("txt", "csv", "tsv")) {
          preview <- tryCatch({
            if (tools::file_ext(content) == "csv") {
              read.csv(content, nrows = 10)
            } else {
              readLines(content, n = 20, encoding = "UTF-8", warn = FALSE)
            }
          }, error = function(e) "Cannot preview file content")
          cat("Preview:\n")
          print(preview)
        }
      } else if (is.character(content)) {
        max_lines <- input$preview_lines
        preview_lines <- head(content, max_lines)
        cat(sprintf("Showing %d of %d items:\n", length(preview_lines), length(content)))
        cat(paste(preview_lines, collapse = "\n"))
        if (length(content) > max_lines) cat("\n... (truncated)")
      } else if (is.data.frame(content)) {
        cat(sprintf("Data frame with %d rows and %d columns:\n", nrow(content), ncol(content)))
        print(utils::head(content, 10))
      } else if (is.list(content)) {
        cat("List structure:\n")
        str(content, max.level = 2, list.len = 10)
      } else {
        cat("Resource type:", class(content), "\n")
        print(utils::head(content, 10))
      }
    })
  })
}