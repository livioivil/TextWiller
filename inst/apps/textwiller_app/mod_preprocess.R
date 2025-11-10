# Enhanced Preprocessing Module
# Enhanced Preprocessing Module with Legacy Support
mod_preprocess_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Configurable Preprocessing Pipeline"),
    
    fluidRow(
      column(12,
        shiny::wellPanel(
          shiny::h4("TextWiller Legacy Features"),
          shiny::checkboxInput(ns("use_legacy"), "Use original TextWiller normalization", value = TRUE),
          shiny::helpText("Abilita le funzioni storiche di TextWiller (normalizzazione, slang, emoticon) quando disponibili.")
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
              "Remove extra whitespace" = "remove_whitespace",
              "Normalize URLs" = "normalize_urls",
              "Normalize emoticons" = "normalize_emoticons",
              "Normalize Italian slang" = "normalize_slang"
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
                "Scarica stopwords-iso",
                class = "btn-info btn-block",
                icon = shiny::icon("cloud-download-alt")
              )
            )
          ),
          helpText("Le stopwords personalizzate possono essere caricate dal tab \"Language & Resources\"."),
          verbatimTextOutput(ns("stopword_status")),
          tags$hr(),
          checkboxInput(ns("enable_multiword"), "Abilita creazione multi-word (RAKE/Collocations)", value = FALSE),
          fileInput(ns("udpipe_model"), "Modello UDPipe (.udpipe)", accept = ".udpipe"),
          fluidRow(
            column(4,
              selectInput(ns("multiword_method"), "Metodo",
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
              numericInput(ns("multiword_freq_min"), "Frequenza minima", value = 5, min = 1, max = 100)
            ),
            column(6,
              selectInput(ns("multiword_term"), "Ricostruisci su", choices = c("lemma", "token"), selected = "lemma")
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
    lang_cfg <- TextWiller3::get_language_config()
    analysis_language <- shiny::reactive({
      lang_cfg$get_version()
      lang_cfg$current_language
    })
    
    multiword_model <- shiny::reactiveVal(NULL)
    multiword_model_label <- shiny::reactiveVal("Nessun modello UDPipe caricato.")
    multiword_stats <- shiny::reactiveVal(NULL)
    
    stopword_inventory <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(lang_cfg$get_version(), analysis_language()),
      valueFunc = function() {
        TextWiller3::list_language_resources(
          language = analysis_language(),
          type = "stopwords",
          include_content = TRUE
        )
      }
    )
    
    custom_stopwords <- shiny::reactive({
      resources <- stopword_inventory()
      selected <- input$stopword_source
      if (is.null(resources) || nrow(resources) == 0 || is.null(selected) || selected == "builtin") {
        return(NULL)
      }
      idx <- which(resources$name == selected)
      if (length(idx) == 0) {
        return(NULL)
      }
      resources$content[[idx]]
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
      label <- ifelse(lang == "it", "Italiano", "English")
      shiny::p(
        shiny::strong("Analysis language: "),
        label,
        shiny::span(" (modificabile nel tab Language & Resources)", style = "font-size:90%; color:#7f8c8d;")
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
          paste("Scaricate", length(words), "stopwords dal repository stopwords-iso"),
          type = "message"
        )
        shiny::updateSelectInput(session, "stopword_source", selected = name)
      }, error = function(e) {
        shiny::showNotification(paste("Errore caricamento stopwords:", e$message), type = "error")
      })
    })
    
    output$stopword_status <- shiny::renderPrint({
      resources <- stopword_inventory()
      if (is.null(resources) || nrow(resources) == 0) {
        cat("Uso stopwords integrate.")
      } else if (input$stopword_source == "builtin") {
        cat("Stopwords personalizzate disponibili:", nrow(resources))
      } else {
        selected <- custom_stopwords()
        if (is.null(selected)) {
          cat("Nessuna informazione per la risorsa selezionata.")
        } else {
          cat("Stopwords attive:", length(selected))
        }
      }
    })
    
    observeEvent(input$udpipe_model, {
      req(input$udpipe_model$datapath)
      tryCatch({
        model <- udpipe::udpipe_load_model(file = input$udpipe_model$datapath)
        multiword_model(model)
        multiword_model_label(paste("Modello caricato:", input$udpipe_model$name))
        shiny::showNotification("Modello UDPipe caricato correttamente", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Errore caricamento modello UDPipe:", e$message), type = "error")
      })
    })
    
    observeEvent(input$enable_multiword, {
      if (!isTRUE(input$enable_multiword)) {
        multiword_stats(NULL)
      }
    })
    
    output$multiword_status <- shiny::renderPrint({
      if (!isTRUE(input$enable_multiword)) {
        cat("Multi-word disabilitato.")
      } else if (is.null(multiword_model())) {
        cat("Carica un modello UDPipe (.udpipe) per abilitare la creazione di multi-word.")
      } else {
        cat(multiword_model_label())
      }
    })
    
    output$multiword_table <- shiny::renderTable({
      stats <- multiword_stats()
      if (is.null(stats) || nrow(stats) == 0) {
        return(data.frame(Messaggio = "Nessuna multi-word rilevata finora"))
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
        shiny::showNotification("Caricare un modello UDPipe per usare i multi-word.", type = "warning")
        return(text_vec)
      }
      
      if (is.null(text_vec) || length(text_vec) == 0) {
        return(text_vec)
      }
      
      doc_ids <- paste0("doc_", seq_along(text_vec))
      annotations <- tryCatch({
        udpipe::udpipe_annotate(model, x = text_vec, doc_id = doc_ids)
      }, error = function(e) {
        shiny::showNotification(paste("Errore annotazione UDPipe:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(annotations)) {
        return(text_vec)
      }
      
      tokens <- as.data.frame(annotations)
      if (nrow(tokens) == 0) {
        multiword_stats(NULL)
        return(text_vec)
      }
      
      tokens$term_id <- tokens$token_id
      tokens$POSSelected <- TRUE
      tokens$lemma[is.na(tokens$lemma) | tokens$lemma == ""] <- tokens$token[is.na(tokens$lemma) | tokens$lemma == ""]
      
      token_subset <- tokens[, c("doc_id", "term_id", "token", "lemma", "upos", "POSSelected")]
      
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
        shiny::showNotification(paste("Errore generazione multi-word:", e$message), type = "error")
        NULL
      })
      
      if (is.null(results)) {
        return(text_vec)
      }
      
      if (is.null(results$stats) || nrow(results$stats) == 0) {
        multiword_stats(NULL)
        return(text_vec)
      }
      
      multiword_stats(head(results$stats, 50))
      
      updated_tokens <- tryCatch({
        TextWiller3::apply_rake_multiwords(token_subset, results, term = input$multiword_term)
      }, error = function(e) {
        shiny::showNotification(paste("Errore applicazione multi-word:", e$message), type = "error")
        NULL
      })
      
      if (is.null(updated_tokens)) {
        return(text_vec)
      }
      
      updated_tokens <- updated_tokens[order(match(updated_tokens$doc_id, doc_ids), updated_tokens$term_id), ]
      keep_rows <- updated_tokens$POSSelected != FALSE & updated_tokens$upos != "NGRAM_MERGED"
      updated_tokens <- updated_tokens[keep_rows, , drop = FALSE]
      
      field <- if (input$multiword_term == "lemma") updated_tokens$lemma else updated_tokens$token
      reconstructed <- tapply(field, updated_tokens$doc_id, function(words) paste(words, collapse = " "))
      
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
      shiny::updateSelectInput(session, "language", selected = "it")
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
      
      if("remove_whitespace" %in% input$normalization_steps) {
        text <- gsub("\\s+", " ", text)
        text <- trimws(text)
      }
      
      # Apply advanced steps
      if("remove_stopwords" %in% input$advanced_steps) {
        text <- TextWiller3::remove_stopwords_enhanced(
          text,
          language = analysis_language(),
          custom_stopwords = custom_stopwords()
        )
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
          stemmed <- SnowballC::wordStem(tokens, language = stem_lang)
          paste(stemmed, collapse = " ")
        })
      }
      
      text <- apply_multiword_pipeline(text)
      
      return(text)
    }
    
    # Preview processing
    shiny::observeEvent(input$preview_btn, {
      shiny::req(corpus())
      
      preview_text <- head(corpus(), 3)
      processed_preview <- process_text(preview_text)
      
      output$processed_preview <- shiny::renderPrint({
        cat("Processed version:\n\n")
        for(i in 1:length(processed_preview)) {
          cat(sprintf("Doc %d: %s\n\n", i, substr(processed_preview[i], 1, 200)))
        }
      })
      
      # Show statistics
      original_stats <- TextWiller3::get_corpus_stats(preview_text)
      processed_stats <- TextWiller3::get_corpus_stats(processed_preview)
      
      output$before_stats <- shiny::renderTable({
        data.frame(
          Metric = c("Documents", "Total Words", "Avg Words/Doc", "Vocabulary"),
          Value = c(
            original_stats$n_docs,
            original_stats$total_words,
            original_stats$avg_words,
            nrow(TextWiller3::calculate_word_frequencies_enhanced(preview_text, preprocess = FALSE))
          )
        )
      }, bordered = TRUE)
      
      output$after_stats <- shiny::renderTable({
        data.frame(
          Metric = c("Documents", "Total Words", "Avg Words/Doc", "Vocabulary"),
          Value = c(
            processed_stats$n_docs,
            processed_stats$total_words,
            processed_stats$avg_words,
            nrow(TextWiller3::calculate_word_frequencies_enhanced(processed_preview, preprocess = FALSE))
          )
        )
      }, bordered = TRUE)
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
      
      # Show summary
      original_stats <- TextWiller3::get_corpus_stats(corpus())
      processed_stats <- TextWiller3::get_corpus_stats(full_processed)
      
      shiny::showNotification(
        sprintf(
          "Processing complete! Reduced from %d to %d words (%.1f%%)",
          original_stats$total_words,
          processed_stats$total_words,
          (1 - processed_stats$total_words / original_stats$total_words) * 100
        ),
        type = "message",
        duration = 10
      )
    })
    
    # Return processed corpus
    return(processed_corpus)
  })
}
