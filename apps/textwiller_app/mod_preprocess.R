# Enhanced Preprocessing Module with Legacy Support
mod_preprocess_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Preprocessing Pipeline Configuration")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Calculate",
          class = "btn-success"
        )
      )
    ),
    fluidRow(
      column(6,
        shiny::wellPanel(
          shiny::h4("Basic Normalization"),
          shiny::checkboxGroupInput(ns("normalization_steps"), "Text Normalization:",
            choices = c(
              "Convert to lowercase" = "lowercase",
              "Remove punctuation" = "remove_punct", 
              "Remove numbers" = "remove_numbers",
              "Remove symbols" = "remove_symbols",
              "Remove extra whitespace" = "remove_whitespace",
              "Normalize URLs" = "normalize_urls",
              "Normalize emoticons" = "normalize_emoticons"
            ),
            selected = c("lowercase", "remove_punct", "remove_whitespace")
          )
        )
      ),
      column(6,
        shiny::wellPanel(
          shiny::h4("Advanced Processing"),
          shiny::checkboxGroupInput(ns("advanced_steps"), "Advanced Steps:",
            choices = c(
              "Remove stopwords" = "remove_stopwords",
              "Remove short words (<3 chars)" = "remove_short_words",
              "Apply stemming" = "apply_stemming"
            )
          ),
          shiny::uiOutput(ns("language_badge")),
          shiny::numericInput(ns("min_word_length"), "Minimum word length:", 
            value = 2, min = 1, max = 10
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        wellPanel(
          h4("Stopwords & Multi-word Tools"),
          fluidRow(
            column(6,
              selectInput(
                ns("stopword_source"),
                "Stopword resource",
                choices = c("Built-in (default)" = "builtin"),
                selected = "builtin"
              )
            ),
            column(6,
              actionButton(
                ns("load_stopwords_iso"),
                "Download stopwords-iso",
                class = "btn-info btn-block",
                icon = shiny::icon("cloud-download-alt")
              )
            )
          ),
          helpText("Custom stopwords can be loaded from the \"Language & Resources\" tab."),
          verbatimTextOutput(ns("stopword_status")),
          tags$hr(),
          checkboxInput(ns("enable_multiword"), "Enable multi-word creation (RAKE/Collocations)", value = FALSE),
          fileInput(ns("udpipe_model"), "UDPipe model (.udpipe)", accept = ".udpipe"),
          selectInput(ns("udpipe_resource"), "Or select a registered model", choices = c("None" = "")),
          fluidRow(
            column(4,
              selectInput(ns("multiword_method"), "Method",
                choices = c("RAKE" = "rake", "PMI" = "pmi", "MD" = "md", "LFMD" = "lfmd"),
                selected = "rake"
              )
            ),
            column(4,
              numericInput(ns("multiword_ngram_min"), "Ngram min", value = 2, min = 2, max = 5)
            ),
            column(4,
              numericInput(ns("multiword_ngram_max"), "Ngram max", value = 4, min = 2, max = 6)
            )
          ),
          fluidRow(
            column(6,
              numericInput(ns("multiword_freq_min"), "Minimum frequency", value = 2, min = 1, max = 100)
            ),
            column(6,
              selectInput(ns("multiword_term"), "Rebuild on", choices = c("lemma", "token"), selected = "lemma")
            )
          ),
          verbatimTextOutput(ns("multiword_status")),
          tableOutput(ns("multiword_table"))
        )
      )
    ),
    
    fluidRow(
      column(12,
        wellPanel(
          h4("Processing Actions"),
          fluidRow(
            column(4,
              actionButton(ns("preview_btn"), "Preview Processing", 
                         class = "btn-primary btn-block")
            ),
            column(4,
              actionButton(ns("apply_btn"), "Apply to Corpus", 
                         class = "btn-success btn-block")
            ),
            column(4,
              actionButton(ns("reset_btn"), "Reset Pipeline", 
                         class = "btn-warning btn-block")
            )
          )
        )
      )
    ),
    
    hr(),
    
    # Preview Section
    fluidRow(
      column(6,
        wellPanel(
          h4("Original Text Preview"),
          verbatimTextOutput(ns("original_preview"))
        )
      ),
      column(6,
        wellPanel(
          h4("Processed Text Preview"),
          verbatimTextOutput(ns("processed_preview"))
        )
      )
    ),
    
    # Statistics Section
    wellPanel(
      h4("Processing Statistics"),
      fluidRow(
        column(6,
          tableOutput(ns("before_stats"))
        ),
        column(6,
          tableOutput(ns("after_stats"))
        )
      )
    )
  )
}

mod_preprocess_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    processed_corpus <- shiny::reactiveVal()
    lang_cfg <- tryCatch({
      TextWiller3::get_language_config()
    }, error = function(e) {
      list(current_language = "it", get_version = function() 0)
    })
    
    analysis_language <- shiny::reactive({
      tryCatch({
        TextWiller3::get_analysis_language()
      }, error = function(e) {
        lang_cfg$current_language
      })
    })
    
    multiword_model <- shiny::reactiveVal(NULL)
    multiword_model_label <- shiny::reactiveVal("No UDPipe model loaded.")
    multiword_stats <- shiny::reactiveVal(NULL)
    multiword_message <- shiny::reactiveVal(NULL)
    udpipe_inventory <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(input$calculate, analysis_language()),
      valueFunc = function() {
        tryCatch({
          TextWiller3::list_language_resources(
            language = analysis_language(),
            type = "udpipe_model",
            include_content = TRUE
          )
        }, error = function(e) {
          data.frame()
        })
      }
    )
    
    stopword_inventory <- shiny::reactivePoll(
      2000, session,
      # Include load_stopwords_iso so newly downloaded lists refresh the catalog
      checkFunc = function() paste(input$calculate, input$load_stopwords_iso, analysis_language()),
      valueFunc = function() {
        tryCatch({
          TextWiller3::list_language_resources(
            language = analysis_language(),
            type = "stopwords",
            include_content = TRUE
          )
        }, error = function(e) {
          data.frame()
        })
      }
    )
    
    custom_stopwords <- shiny::reactive({
      resources <- stopword_inventory()
      selected <- input$stopword_source
      if (!is.data.frame(resources) || nrow(resources) == 0 || is.null(selected) || selected == "builtin") {
        return(NULL)
      }
      idx <- which(resources$name == selected)
      if (length(idx) == 0) {
        return(NULL)
      }
      resources$content[[idx]]
    })
    
    shiny::observe({
      resources <- udpipe_inventory()
      choices <- c("None" = "")
      if (is.data.frame(resources) && nrow(resources) > 0) {
        labels <- paste0(resources$name, " [", resources$type, "]")
        names(labels) <- resources$name
        choices <- c(choices, labels)
      }
      shiny::updateSelectInput(session, "udpipe_resource", choices = choices)
    })
    
    observeEvent(input$udpipe_resource, {
      res_name <- input$udpipe_resource
      if (is.null(res_name) || res_name == "") return()
      resources <- udpipe_inventory()
      idx <- which(resources$name == res_name)
      if (length(idx) == 0) return()
      path <- resources$path[idx]
      if (is.null(path) || is.na(path)) {
        path <- resources$content[[idx]]
      }
      if (is.null(path) || is.na(path) || !file.exists(path)) {
        shiny::showNotification("Model path not available.", type = "error")
        return()
      }
      tryCatch({
        if (requireNamespace("udpipe", quietly = TRUE)) {
          model <- udpipe::udpipe_load_model(file = path)
          multiword_model(model)
          multiword_model_label(paste("Model loaded:", basename(path)))
          multiword_message(NULL)
          shiny::showNotification("UDPipe model loaded from resources.", type = "message")
        } else {
          shiny::showNotification("Install 'udpipe' package to use UDPipe models.", type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error loading UDPipe model:", e$message), type = "error")
      })
    })
    
    shiny::observe({
      resources <- stopword_inventory()
      choices <- c("Built-in (default)" = "builtin")
      if (is.data.frame(resources) && nrow(resources) > 0) {
        labels <- paste0(resources$name, " [", resources$type, "]")
        names(labels) <- resources$name
        choices <- c(choices, labels)
      }
      shiny::updateSelectInput(session, "stopword_source", choices = choices)
    })
    
    output$language_badge <- shiny::renderUI({
      lang <- analysis_language()
      label <- ifelse(lang == "it", "Italian", "English")
      shiny::p(
        shiny::strong("Analysis language: "),
        label,
        shiny::span(" (change in Language & Resources tab)", style = "font-size:90%; color:#7f8c8d;")
      )
    })
    
    shiny::observeEvent(input$load_stopwords_iso, {
      tryCatch({
        name <- paste0("stopwords_iso_", analysis_language(), "_", format(Sys.time(), "%H%M%S"))
        words <- TextWiller3::download_stopwords_iso(
          language = analysis_language(),
          auto_register = TRUE,
          resource_name = name
        )
        shiny::showNotification(
          paste("Downloaded", length(words), "stopwords from stopwords-iso"),
          type = "message"
        )
        shiny::updateSelectInput(session, "stopword_source", selected = name)
      }, error = function(e) {
        shiny::showNotification(paste("Error downloading stopwords:", e$message), type = "error")
      })
    })
    
    output$stopword_status <- shiny::renderPrint({
      resources <- stopword_inventory()
      if (!is.data.frame(resources) || nrow(resources) == 0) {
        cat("Using built-in stopwords.")
      } else if (input$stopword_source == "builtin") {
        cat("Custom stopwords available:", nrow(resources))
      } else {
        selected <- custom_stopwords()
        if (is.null(selected)) {
          cat("No information for selected resource.")
        } else {
          cat("Active stopwords:", length(selected))
        }
      }
    })
    
    observeEvent(input$udpipe_model, {
      req(input$udpipe_model$datapath)
      tryCatch({
        if (requireNamespace("udpipe", quietly = TRUE)) {
          model <- udpipe::udpipe_load_model(file = input$udpipe_model$datapath)
          multiword_model(model)
          multiword_model_label(paste("Model loaded:", input$udpipe_model$name))
          multiword_message(NULL)
          shiny::showNotification("UDPipe model loaded successfully", type = "message")
        } else {
          shiny::showNotification("Install 'udpipe' package to use UDPipe models.", type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error loading UDPipe model:", e$message), type = "error")
      })
    })
    
    observeEvent(input$enable_multiword, {
      if (!isTRUE(input$enable_multiword)) {
        multiword_stats(NULL)
        multiword_message(NULL)
      }
    })
    
    output$multiword_status <- shiny::renderPrint({
      if (!isTRUE(input$enable_multiword)) {
        cat("Multi-word disabled.")
      } else if (is.null(multiword_model())) {
        cat("Load a UDPipe model (.udpipe) or select one from registered models.")
      } else if (!is.null(multiword_message())) {
        cat(multiword_model_label(), "\n", multiword_message())
      } else {
        cat(multiword_model_label())
      }
    })
    
    output$multiword_table <- shiny::renderTable({
      stats <- multiword_stats()
      if (is.null(stats) || nrow(stats) == 0) {
        return(data.frame(Message = "No multi-words detected yet"))
      }
      if ("Message" %in% names(stats)) {
        return(stats)
      }
      head(stats[, intersect(c("keyword", "freq", "ngram", "pmi", "md", "lfmd"), names(stats)), drop = FALSE], 10)
    }, striped = TRUE, bordered = TRUE)
    
    apply_multiword_pipeline <- function(text_vec) {
      if (!isTRUE(input$enable_multiword)) {
        multiword_stats(NULL)
        return(text_vec)
      }
      
      model <- multiword_model()
      if (is.null(model)) {
        multiword_message("No active UDPipe model: load one or select from catalog.")
        shiny::showNotification("Load a UDPipe model to use multi-words.", type = "warning")
        return(text_vec)
      }
      
      if (is.null(text_vec) || length(text_vec) == 0) {
        return(text_vec)
      }
      
      doc_ids <- paste0("doc_", seq_along(text_vec))
      annotations <- tryCatch({
        udpipe::udpipe_annotate(model, x = text_vec, doc_id = doc_ids)
      }, error = function(e) {
        shiny::showNotification(paste("UDPipe annotation error:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(annotations)) {
        multiword_message("UDPipe annotation failed.")
        return(text_vec)
      }
      
      tokens <- as.data.frame(annotations)
      if (nrow(tokens) == 0) {
        multiword_stats(NULL)
        multiword_message("Empty annotation: check text or model.")
        return(text_vec)
      }
      
      tokens$term_id <- tokens$token_id
      tokens$POSSelected <- TRUE
      tokens$lemma[is.na(tokens$lemma) | tokens$lemma == ""] <- tokens$token[is.na(tokens$lemma) | tokens$lemma == ""]
      
      token_subset <- tokens[, c("doc_id", "term_id", "token", "lemma", "upos", "POSSelected")]
      n_relevant <- sum(token_subset$upos %in% c("PROPN", "NOUN", "ADJ", "VERB"))
      if (n_relevant == 0) {
        multiword_stats(data.frame(Message = "No relevant POS (NOUN/VERB/ADJ/PROPN) found."))
        multiword_message("No relevant POS in text.")
        return(text_vec)
      }
      
      results <- tryCatch({
        TextWiller3::rake_multiword_candidates(
          token_subset,
          group = "doc_id",
          ngram_max = input$multiword_ngram_max,
          ngram_min = input$multiword_ngram_min,
          freq_min = input$multiword_freq_min,
          term = input$multiword_term,
          method = input$multiword_method
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error generating multi-words:", e$message), type = "error")
        multiword_message(paste("Generation error:", e$message))
        NULL
      })
      
      if (is.null(results)) {
        return(text_vec)
      }
      
      if (is.null(results$stats) || nrow(results$stats) == 0) {
        multiword_stats(data.frame(Message = "No multi-words found with current parameters"))
        multiword_message("No multi-words found: try lowering Minimum frequency or adjusting Ngram min/max.")
        return(text_vec)
      }
      
      multiword_stats(head(results$stats, 50))
      multiword_message(NULL)
      
      updated_tokens <- tryCatch({
        TextWiller3::apply_rake_multiwords(token_subset, results, term = input$multiword_term)
      }, error = function(e) {
        shiny::showNotification(paste("Error applying multi-words:", e$message), type = "error")
        multiword_message(paste("Application error:", e$message))
        NULL
      })
      
      if (is.null(updated_tokens)) {
        return(text_vec)
      }
      
      updated_tokens <- updated_tokens[order(match(updated_tokens$doc_id, doc_ids), updated_tokens$term_id), ]
      keep_rows <- updated_tokens$POSSelected != FALSE & updated_tokens$upos != "NGRAM_MERGED"
      updated_tokens <- updated_tokens[keep_rows, , drop = FALSE]
      
      field <- if (input$multiword_term == "lemma") updated_tokens$lemma else updated_tokens$token
      reconstructed <- vapply(split(field, updated_tokens$doc_id), paste, character(1), collapse = " ")
      
      new_text <- text_vec
      matched <- match(names(reconstructed), doc_ids)
      new_text[matched] <- unname(reconstructed)
      new_text
    }
    
    # Reset pipeline
    shiny::observeEvent(input$reset_btn, {
      shiny::updateCheckboxGroupInput(session, "normalization_steps", 
                              selected = c("lowercase", "remove_punct", "remove_whitespace"))
      shiny::updateCheckboxGroupInput(session, "advanced_steps", selected = character(0))
      shiny::updateNumericInput(session, "min_word_length", value = 2)
      processed_corpus(NULL)
      shiny::showNotification("Pipeline reset to defaults", type = "message")
    })
    
    # Preview original text
    output$original_preview <- shiny::renderPrint({
      shiny::req(corpus())
      if(length(corpus()) > 0) {
        cat("First 3 documents:\n\n")
        for(i in 1:min(3, length(corpus()))) {
          cat(sprintf("Doc %d: %s\n\n", i, substr(corpus()[i], 1, 200)))
        }
      } else {
        cat("No corpus available")
      }
    })
    
    # Process text with selected pipeline
    process_text <- function(text) {
      if(is.null(text) || length(text) == 0) return(text)
      
      # Apply basic normalization steps
      if("lowercase" %in% input$normalization_steps) {
        text <- tolower(text)
      }
      
      if("remove_punct" %in% input$normalization_steps) {
        text <- gsub("[[:punct:]]", " ", text)
      }
      
      if("remove_numbers" %in% input$normalization_steps) {
        text <- gsub("[[:digit:]]", " ", text)
      }

      if("remove_symbols" %in% input$normalization_steps) {
        text <- gsub("[^[:alnum:]\\s]", " ", text)
      }
      
      if("remove_whitespace" %in% input$normalization_steps) {
        text <- gsub("\\s+", " ", text)
        text <- trimws(text)
      }
      
      # Apply advanced steps
      if("remove_stopwords" %in% input$advanced_steps) {
        tryCatch({
          text <- TextWiller3::remove_stopwords_enhanced(
            text,
            language = analysis_language(),
            custom_stopwords = custom_stopwords()
          )
        }, error = function(e) {
          # Fallback: simple stopword removal
          cat("Stopword removal error, using fallback\n")
        })
      }
      
      if("remove_short_words" %in% input$advanced_steps) {
        words <- strsplit(text, "\\s+")
        text <- sapply(words, function(x) {
          keep_words <- x[nchar(x) >= input$min_word_length]
          paste(keep_words, collapse = " ")
        })
      }
      
       if("apply_stemming" %in% input$advanced_steps) {
        stem_lang <- ifelse(analysis_language() == "it", "italian", "english")
        text <- sapply(strsplit(text, "\\s+"), function(tokens) {
          tokens <- tokens[nchar(tokens) > 0]
          if (length(tokens) == 0) return("")
          if (requireNamespace("SnowballC", quietly = TRUE)) {
            stemmed <- SnowballC::wordStem(tokens, language = stem_lang)
          } else {
            stemmed <- tokens  # Fallback if SnowballC not available
          }
          paste(stemmed, collapse = " ")
        })
      }
      
      text <- apply_multiword_pipeline(text)
      
      return(text)
    }
    
    # Preview processing
    shiny::observeEvent(list(input$preview_btn, input$calculate), {
      shiny::req(corpus())
      
      preview_text <- head(corpus(), 3)
      processed_preview <- process_text(preview_text)
      
      output$processed_preview <- shiny::renderPrint({
        cat("Processed version:\n\n")
        for(i in 1:length(processed_preview)) {
          cat(sprintf("Doc %d: %s\n\n", i, substr(processed_preview[i], 1, 200)))
        }
      })
      
      # Show statistics with safe fallbacks
      tryCatch({
        original_stats <- TextWiller3::get_corpus_stats(preview_text)
        processed_stats <- TextWiller3::get_corpus_stats(processed_preview)
        
        output$before_stats <- shiny::renderTable({
          data.frame(
            Metric = c("Documents", "Total Words", "Avg Words/Doc"),
            Value = c(
              original_stats$n_docs,
              original_stats$total_words,
              original_stats$avg_words
            )
          )
        }, bordered = TRUE)
        
        output$after_stats <- shiny::renderTable({
          data.frame(
            Metric = c("Documents", "Total Words", "Avg Words/Doc"),
            Value = c(
              processed_stats$n_docs,
              processed_stats$total_words,
              processed_stats$avg_words
            )
          )
        }, bordered = TRUE)
      }, error = function(e) {
        # Fallback stats
        word_counts_orig <- sapply(strsplit(preview_text, "\\s+"), length)
        word_counts_proc <- sapply(strsplit(processed_preview, "\\s+"), length)
        
        output$before_stats <- shiny::renderTable({
          data.frame(
            Metric = c("Documents", "Total Words", "Avg Words/Doc"),
            Value = c(length(preview_text), sum(word_counts_orig), round(mean(word_counts_orig), 1))
          )
        }, bordered = TRUE)
        
        output$after_stats <- shiny::renderTable({
          data.frame(
            Metric = c("Documents", "Total Words", "Avg Words/Doc"),
            Value = c(length(processed_preview), sum(word_counts_proc), round(mean(word_counts_proc), 1))
          )
        }, bordered = TRUE)
      })
    })
    
    # Apply processing to full corpus
    shiny::observeEvent(input$apply_btn, {
      shiny::req(corpus())
      
      shiny::showModal(shiny::modalDialog(
        title = "Processing Corpus",
        "Applying preprocessing pipeline to all documents...",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Process the entire corpus
      full_processed <- process_text(corpus())
      
      # Update processed corpus
      processed_corpus(full_processed)
      
      shiny::removeModal()
      
      # Show summary with safe calculations
      word_counts_orig <- sapply(strsplit(corpus(), "\\s+"), length)
      word_counts_proc <- sapply(strsplit(full_processed, "\\s+"), length)
      
      shiny::showNotification(
        sprintf(
          "Processing complete! Reduced from %d to %d words (%.1f%%)",
          sum(word_counts_orig),
          sum(word_counts_proc),
          (1 - sum(word_counts_proc) / sum(word_counts_orig)) * 100
        ),
        type = "message",
        duration = 10
      )
    })
    
    # Return processed corpus
    return(processed_corpus)
  })
}
