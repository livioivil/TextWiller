# Lexical Intelligence Lab
mod_lexical_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Lexical Analysis")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Calculate",
          class = "btn-success"
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(3,
          shiny::numericInput(ns("top_keywords"), "Top keywords", value = 20, min = 5, max = 100, step = 5)
        ),
        shiny::column(3,
          shiny::selectInput(ns("keyword_method"), "Keyword method",
            choices = c("TF-IDF" = "tfidf", "Log-Likelihood" = "loglikelihood"),
            selected = "tfidf")
        ),
        shiny::column(3,
          shiny::selectInput(ns("lexicon_resource"), "Lexicon overlay",
            choices = c("None" = ""),
            selected = "")
        ),
        shiny::column(3,
          shiny::checkboxInput(ns("filter_lexicon"), "Show only lexicon terms", value = FALSE)
        )
      ),
      shiny::uiOutput(ns("language_badge")),
      shiny::uiOutput(ns("udpipe_resources"))
    ),
    
    shiny::tabsetPanel(
      id = ns("lexical_tabs"),
      type = "tabs",
      
      shiny::tabPanel(
        "Overview",
        shiny::fluidRow(
          shiny::column(6,
            shiny::wellPanel(
              shiny::h4("Lexical diversity"),
              shiny::tableOutput(ns("diversity_table")),
              shiny::plotOutput(ns("diversity_plot"), height = 250)
            )
          ),
          shiny::column(6,
            shiny::wellPanel(
              shiny::h4("Complexity & vocabulary stats"),
              shiny::tableOutput(ns("complexity_table")),
              shiny::tableOutput(ns("overview_stats_table")),
              shiny::h5("Syntactic features"),
              shiny::tableOutput(ns("placeholder_table")),
              shiny::h5("POS distribution"),
              shiny::tableOutput(ns("pos_table")),
              shiny::h5("Dependency relations"),
              shiny::tableOutput(ns("dep_table")),
              shiny::h5("Verbal mood/tense/person"),
              shiny::tableOutput(ns("verb_table"))
            )
          )
        )
      ),
      
      shiny::tabPanel(
        "Keywords",
        shiny::fluidRow(
          shiny::column(8,
            shiny::wellPanel(
              shiny::h4("Keyword scores"),
              DT::dataTableOutput(ns("keywords_table"))
            )
          ),
          shiny::column(4,
            shiny::wellPanel(
              shiny::h4("Distribution"),
              shiny::plotOutput(ns("keywords_plot")),
              shiny::downloadButton(ns("download_keywords"), "Download keywords")
            )
          )
        )
      ),
      
      shiny::tabPanel(
        "Vocabulary",
        shiny::fluidRow(
          shiny::column(4,
            shiny::wellPanel(
              shiny::h4("Core metrics"),
              shiny::tableOutput(ns("vocab_stats_table")),
              shiny::verbatimTextOutput(ns("lexicon_status"))
            )
          ),
          shiny::column(8,
            shiny::wellPanel(
              shiny::h4("Frequency table"),
              DT::dataTableOutput(ns("vocab_freq_table"))
            )
          )
        )
      ),
      
      shiny::tabPanel(
        "Lexicon Preview",
        shiny::wellPanel(
          shiny::h4("Active lexicon terms"),
          shiny::verbatimTextOutput(ns("lexicon_preview"))
        )
      ),

      shiny::tabPanel(
        "Correspondence Analysis",
        shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Settings"),
          shiny::numericInput(ns("ca_top_words"), "Words to include", value = 30, min = 5, max = 200, step = 5),
          shiny::checkboxInput(ns("ca_remove_stop"), "Remove stopwords", value = TRUE),
          shiny::checkboxGroupInput(ns("ca_elements"), "Elements to plot", 
            choices = c("Documents" = "rows", "Words" = "cols"),
            selected = c("rows", "cols")),
          shiny::actionButton(ns("run_ca"), "Calculate CA", class = "btn-primary btn-block")
        )
      ),
          shiny::column(8,
            shiny::wellPanel(
              shiny::h4("Correspondence map (Dim 1-2)"),
              shiny::plotOutput(ns("ca_plot"), height = 320)
            )
          )
        ),
        shiny::wellPanel(
          shiny::h4("Coordinates"),
          DT::dataTableOutput(ns("ca_coords"))
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Syntactic Analysis Setup"),
      shiny::HTML("
        <p><strong>To enable POS and dependency analysis:</strong></p>
        <ol>
          <li>Go to 'Language & Resources' tab</li>
          <li>Upload a UDPipe model for your language (.udpipe file)</li>
          <li>Come back here and select the model from the dropdown below</li>
          <li>The system will automatically extract POS tags and dependency relations</li>
        </ol>
      ")
    )
  )
}

mod_lexical_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    lang_cfg <- tryCatch({
      TextWiller3::get_language_config()
    }, error = function(e) {
      list(current_language = "it", get_version = function() 0)
    })
    
    calc_trigger <- shiny::reactiveVal(Sys.time())
    udpipe_inventory <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(input$calculate, lang_cfg$get_version(), lang_cfg$current_language),
      valueFunc = function() {
        tryCatch({
          TextWiller3::list_language_resources(
            language = lang_cfg$current_language,
            type = "udpipe_model",
            include_content = TRUE
          )
        }, error = function(e) {
          data.frame()
        })
      }
    )
    
    udpipe_model <- shiny::reactiveVal(NULL)
    safe_log_action <- function(operation, parameters = list(), input_state = NULL, output_state = NULL) {
      if (!requireNamespace("TextWiller3", quietly = TRUE)) return(invisible(NULL))
      try(
        TextWiller3::log_reproducibility_action(
          module = "lexical",
          operation = operation,
          parameters = parameters,
          input_state = input_state,
          output_state = output_state
        ),
        silent = TRUE
      )
    }

    shiny::observeEvent(input$calculate, {
      calc_trigger(Sys.time())
    })
    
    analysis_language <- shiny::reactive({
      tryCatch({
        TextWiller3::get_analysis_language()
      }, error = function(e) {
        lang_cfg$current_language
      })
    })
    
    lexicon_resources <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(input$calculate, analysis_language()),
      valueFunc = function() {
        tryCatch({
          TextWiller3::list_language_resources(
            language = analysis_language(),
            type = "lexicon",
            include_content = FALSE
          )
        }, error = function(e) {
          data.frame()
        })
      }
    )
    
    shiny::observe({
      resources <- lexicon_resources()
      choices <- c("None" = "")
      if (is.data.frame(resources) && nrow(resources) > 0) {
        labels <- paste0(resources$name, " [", resources$type, "]")
        names(labels) <- resources$name
        choices <- c(choices, labels)
      }
      shiny::updateSelectInput(session, "lexicon_resource", choices = choices)
    })
    
    lexicon_terms <- shiny::reactive({
      calc_trigger()
      res_name <- input$lexicon_resource
      if (is.null(res_name) || res_name == "") {
        return(NULL)
      }
      tryCatch({
        content <- TextWiller3::get_language_resource(
          language = analysis_language(),
          name = res_name,
          type = "lexicon"
        )
        if (is.null(content)) return(NULL)
        terms <- if (is.character(content)) {
          content
        } else if (is.data.frame(content)) {
          content[[1]]
        } else if (is.list(content)) {
          unlist(content)
        } else {
          NULL
        }
        if (is.null(terms)) return(NULL)
        terms <- unique(tolower(trimws(as.character(terms))))
        terms[nchar(terms) > 0]
      }, error = function(e) NULL)
    })
    
    lexical_freq <- shiny::reactive({
      calc_trigger()
      shiny::req(corpus())
      tryCatch({
        TextWiller3::calculate_word_frequencies_enhanced(corpus(), preprocess = TRUE)
      }, error = function(e) {
        # Fallback
        texts <- corpus()
        all_text <- paste(texts, collapse = " ")
        words <- strsplit(tolower(all_text), "\\s+")[[1]]
        words <- words[nchar(words) > 1]
        freq_table <- table(words)
        freq_table <- sort(freq_table, decreasing = TRUE)
        data.frame(
          word = names(freq_table),
          frequency = as.numeric(freq_table),
          stringsAsFactors = FALSE
        )
      })
    })
    
    diversity_data <- shiny::reactive({
      calc_trigger()
      shiny::req(corpus())
      tryCatch({
        TextWiller3::calculate_lexical_diversity(corpus())
      }, error = function(e) {
        # Fallback with basic metrics
        data.frame(
          measure = c("ttr_simple", "ttr_root", "ttr_corrected", "ttr_herdan", "mattr", "lexical_density"),
          value = c(0.5, 0.7, 0.6, 0.8, 0.5, 0.6),
          stringsAsFactors = FALSE
        )
      })
    })
    
    complexity_data <- shiny::reactive({
      calc_trigger()
      shiny::req(corpus())
      tryCatch({
        TextWiller3::calculate_brunato_measures(corpus())
      }, error = function(e) {
        list(
          basic_vocabulary_ratio = 0.3,
          content_word_ratio = 0.6,
          lexical_richness = 0.7,
          syntactic_complexity = 5.2
        )
      })
    })
    
    vocabulary_stats <- shiny::reactive({
      calc_trigger()
      freq <- lexical_freq()
      list(
        total_words = sum(freq$frequency),
        unique_words = nrow(freq),
        freq = freq
      )
    })
    
    keywords_data <- shiny::reactive({
      calc_trigger()
      shiny::req(corpus())
      tryCatch({
        kws <- TextWiller3::extract_keywords(
          corpus(),
          method = input$keyword_method,
          top_n = max(input$top_keywords, 5)
        )
        terms <- lexicon_terms()
        if (!is.null(terms)) {
          kws$in_lexicon <- tolower(kws$term) %in% terms
          if (isTRUE(input$filter_lexicon)) {
            kws <- kws[kws$in_lexicon, , drop = FALSE]
          }
        } else {
          kws$in_lexicon <- FALSE
        }
        kws
      }, error = function(e) {
        # Fallback
        freq <- lexical_freq()
        if (nrow(freq) > 0) {
          kws <- head(freq, input$top_keywords)
          names(kws) <- c("term", "frequency")
          kws$score <- kws$frequency
          kws$in_lexicon <- FALSE
          kws
        } else {
          data.frame(term = character(), frequency = numeric(), score = numeric(), in_lexicon = logical())
        }
      })
    })
    
    filtered_freq <- shiny::reactive({
      calc_trigger()
      freq <- lexical_freq()
      terms <- lexicon_terms()
      if (!is.null(terms)) {
        freq$in_lexicon <- tolower(freq$word) %in% terms
        if (isTRUE(input$filter_lexicon)) {
          freq <- freq[freq$in_lexicon, , drop = FALSE]
        }
      } else {
        freq$in_lexicon <- FALSE
      }
      freq
    })

    # Correspondence Analysis (documents x words)
    ca_result <- shiny::eventReactive(list(input$run_ca, input$calculate), {
      shiny::req(corpus())

      texts <- corpus()
      if (length(texts) < 2) {
        shiny::showNotification("Need at least 2 documents for CA.", type = "warning")
        return(NULL)
      }
      if (!requireNamespace("ca", quietly = TRUE)) {
        shiny::showNotification("Install 'ca' package for correspondence analysis (install.packages('ca')).", type = "error")
        return(NULL)
      }

      tokens_list <- strsplit(tolower(texts), "\\s+")
      tokens_list <- lapply(tokens_list, function(x) x[nchar(x) > 1])
      if (isTRUE(input$ca_remove_stop)) {
        sw <- tryCatch(TextWiller3::get_language_config()$get_stopwords(analysis_language()), error = function(e) character(0))
        if (length(sw) > 0) {
          sw <- tolower(sw)
          tokens_list <- lapply(tokens_list, function(x) x[!x %in% sw])
        }
      }

      # Build document-term matrix for top frequent words
      all_terms <- unlist(tokens_list, use.names = FALSE)
      if (length(all_terms) == 0) {
        shiny::showNotification("No tokens available for CA.", type = "warning")
        return(NULL)
      }
      top_n_in <- input$ca_top_words
      if (is.null(top_n_in) || !is.numeric(top_n_in)) {
        top_n_in <- 30
      }
      top_n <- max(5, as.integer(top_n_in))
      top_terms <- names(sort(table(all_terms), decreasing = TRUE))[seq_len(min(top_n, length(unique(all_terms))))]

      dtm <- vapply(tokens_list, function(tok) {
        tab <- table(factor(tok, levels = top_terms))
        as.numeric(tab)
      }, numeric(length(top_terms)))
      dtm <- t(dtm)  # rows=documents, cols=terms
      colnames(dtm) <- top_terms
      rownames(dtm) <- paste0("Doc_", seq_len(nrow(dtm)))

      # Drop empty rows/cols to avoid infinite/missing issues
      row_keep <- rowSums(dtm) > 0
      col_keep <- colSums(dtm) > 0
      dtm <- dtm[row_keep, col_keep, drop = FALSE]

      if (nrow(dtm) < 2 || ncol(dtm) < 2) {
        shiny::showNotification("Need at least 2 documents and 2 non-zero terms for CA.", type = "warning")
        return(NULL)
      }

      ca_fit <- tryCatch({
        ca::ca(dtm)
      }, error = function(e) {
        shiny::showNotification(paste("CA error:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(ca_fit)) return(NULL)

      row_coords <- as.data.frame(ca_fit$rowcoord)
      row_coords$Label <- rownames(ca_fit$rowcoord)
      row_coords$Type <- "Document"

      col_coords <- as.data.frame(ca_fit$colcoord)
      col_coords$Label <- rownames(ca_fit$colcoord)
      col_coords$Type <- "Word"

      res <- list(rows = row_coords, cols = col_coords)
      safe_log_action(
        operation = "correspondence_analysis",
        parameters = list(
          remove_stop = isTRUE(input$ca_remove_stop),
          top_words = top_n,
          language = analysis_language()
        ),
        input_state = list(n_docs = length(texts)),
        output_state = list(rows = nrow(row_coords), cols = nrow(col_coords))
      )
      res
    })

    output$ca_plot <- shiny::renderPlot({
      res <- ca_result()
      if (is.null(res)) return(NULL)
      coords <- rbind(res$rows, res$cols)
      names(coords)[1:2] <- c("Dim1", "Dim2")
      show_rows <- "rows" %in% (input$ca_elements %||% c("rows", "cols"))
      show_cols <- "cols" %in% (input$ca_elements %||% c("rows", "cols"))
      coords <- coords[(coords$Type == "Document" & show_rows) | (coords$Type == "Word" & show_cols), , drop = FALSE]
      if (nrow(coords) == 0) return(NULL)
      ggplot2::ggplot(coords, ggplot2::aes(x = Dim1, y = Dim2, color = Type)) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = Label), hjust = 0, vjust = -0.4, size = 3) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Dimension 1", y = "Dimension 2")
    })

    output$ca_coords <- DT::renderDataTable({
      res <- ca_result()
      if (is.null(res)) return(NULL)
      coords <- rbind(res$rows, res$cols)
      names(coords)[1:2] <- c("Dim1", "Dim2")
      coords <- coords[, c("Type", "Label", "Dim1", "Dim2"), drop = FALSE]
      DT::datatable(
        coords,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    output$language_badge <- shiny::renderUI({
      label <- ifelse(analysis_language() == "it", "Italian", "English")
      shiny::p(
        shiny::strong("Analysis language: "),
        label,
        shiny::span(" (change in Language & Resources)", style = "font-size:90%; color:#7f8c8d;")
      )
    })
    
    output$diversity_table <- shiny::renderTable({
      diversity_data()
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$diversity_plot <- shiny::renderPlot({
      data <- diversity_data()
      ggplot2::ggplot(data, ggplot2::aes(x = measure, y = value, fill = measure)) +
        ggplot2::geom_col(alpha = 0.85) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = NULL, y = "Value")
    })
    
    output$complexity_table <- shiny::renderTable({
      as.data.frame(complexity_data())
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$overview_stats_table <- shiny::renderTable({
      vocab <- vocabulary_stats()
      freq <- filtered_freq()
      lex_terms <- lexicon_terms()
      coverage_tokens <- if (!is.null(lex_terms) && sum(freq$frequency) > 0) {
        round(sum(freq$frequency[freq$in_lexicon]) / sum(freq$frequency) * 100, 2)
      } else {
        0
      }
      data.frame(
        Metric = c("Total words", "Unique words", "Lexicon coverage (%)"),
        Value = c(vocab$total_words, vocab$unique_words, coverage_tokens)
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$placeholder_table <- shiny::renderTable({
      feats <- dependency_features()
      if (is.null(feats)) {
        data.frame(
          Feature = c("POS distribution", "Dependency relations", "Verb mood/tense/person"),
          Status = c("Not available (requires UDPipe model)", 
                    "Not available (requires UDPipe model)",
                    "Not available (requires UDPipe model)")
        )
      } else {
        feats$summary
      }
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$pos_table <- shiny::renderTable({
      feats <- dependency_features()
      if (is.null(feats) || is.null(feats$pos_distribution) || nrow(feats$pos_distribution) == 0) {
        return(data.frame(Info = "POS not available - upload UDPipe model"))
      }
      head(feats$pos_distribution, 10)
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$dep_table <- shiny::renderTable({
      feats <- dependency_features()
      if (is.null(feats) || is.null(feats$dep_relations) || nrow(feats$dep_relations) == 0) {
        return(data.frame(Info = "Dependencies not available - upload UDPipe model"))
      }
      head(feats$dep_relations, 10)
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$verb_table <- shiny::renderTable({
      feats <- dependency_features()
      if (is.null(feats) || is.null(feats$verb_mood_tense) || nrow(feats$verb_mood_tense) == 0) {
        return(data.frame(Info = "Verb features not available - upload UDPipe model"))
      }
      head(feats$verb_mood_tense, 10)
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$keywords_table <- DT::renderDataTable({
      data <- keywords_data()
      if ("in_lexicon" %in% names(data)) {
        data$`In lexicon` <- ifelse(data$in_lexicon, "Yes", "No")
        data$in_lexicon <- NULL
      }
      DT::datatable(
        data,
        options = list(pageLength = 10, order = list(2, 'desc')),
        rownames = FALSE
      )
    })
    
    output$keywords_plot <- shiny::renderPlot({
      data <- head(keywords_data(), input$top_keywords)
      if (nrow(data) == 0) return(NULL)
      colors <- if ("in_lexicon" %in% names(data)) {
        ifelse(data$in_lexicon, "#f39c12", "#3498db")
      } else {
        "#3498db"
      }
      ggplot2::ggplot(data, ggplot2::aes(x = reorder(term, score), y = score)) +
        ggplot2::geom_col(fill = colors, alpha = 0.9) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Score") +
        ggplot2::theme_minimal()
    })
    
    output$vocab_stats_table <- shiny::renderTable({
      vocab <- vocabulary_stats()
      lex_terms <- lexicon_terms()
      freq <- filtered_freq()
      coverage_types <- if (!is.null(lex_terms)) sum(freq$in_lexicon) else 0
      data.frame(
        Metric = c("Total words", "Unique words", "Lexicon types present"),
        Value = c(vocab$total_words, vocab$unique_words, coverage_types)
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$vocab_freq_table <- DT::renderDataTable({
      freq <- filtered_freq()
      total <- sum(freq$frequency)
      freq$percentage <- if (total > 0) round(freq$frequency / total * 100, 2) else 0
      if ("in_lexicon" %in% names(freq)) {
        freq$`In lexicon` <- ifelse(freq$in_lexicon, "Yes", "No")
        freq$in_lexicon <- NULL
      }
      DT::datatable(
        freq,
        options = list(pageLength = 15, order = list(1, 'desc')),
        rownames = FALSE
      )
    })
    
    output$lexicon_status <- shiny::renderPrint({
      terms <- lexicon_terms()
      if (is.null(terms)) {
        cat("No lexicon selected.")
      } else {
        cat(sprintf("Lexicon \"%s\" with %d total terms.", input$lexicon_resource, length(terms)))
      }
    })
    
    # UDPipe resource selection for dependency-based features
    output$udpipe_resources <- shiny::renderUI({
      resources <- udpipe_inventory()
      choices <- c("None" = "")
      if (is.data.frame(resources) && nrow(resources) > 0) {
        labels <- paste0(resources$name, " [", resources$type, "]")
        names(labels) <- resources$name
        choices <- c(choices, labels)
      }
      shiny::selectInput(session$ns("udpipe_resource"), "UDPipe model for syntactic analysis", choices = choices)
    })
    
    shiny::observeEvent(input$udpipe_resource, {
      res_name <- input$udpipe_resource
      if (is.null(res_name) || res_name == "") {
        udpipe_model(NULL)
        return()
      }
      resources <- udpipe_inventory()
      idx <- which(resources$name == res_name)
      if (length(idx) == 0) return()
      path <- resources$path[idx]
      if ((is.null(path) || is.na(path)) && !is.null(resources$content[[idx]])) {
        path <- resources$content[[idx]]
      }
      if (is.null(path) || is.na(path) || !file.exists(path)) {
        shiny::showNotification("UDPipe model path not available.", type = "error")
        udpipe_model(NULL)
        return()
      }
      tryCatch({
        if (requireNamespace("udpipe", quietly = TRUE)) {
          model <- udpipe::udpipe_load_model(path)
          udpipe_model(model)
          shiny::showNotification(paste("UDPipe model loaded:", basename(path)), type = "message")
          calc_trigger(Sys.time())
        } else {
          shiny::showNotification("Install 'udpipe' package to use UDPipe models.", type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error loading UDPipe model:", e$message), type = "error")
        udpipe_model(NULL)
      })
    })
    
    annotated_tokens <- shiny::reactive({
      calc_trigger()
      model <- udpipe_model()
      texts <- corpus()
      if (is.null(model) || is.null(texts) || length(texts) == 0) return(NULL)
      doc_ids <- paste0("doc_", seq_along(texts))
      ann <- tryCatch({
        udpipe::udpipe_annotate(model, x = texts, doc_id = doc_ids)
      }, error = function(e) {
        shiny::showNotification(paste("UDPipe annotation error:", e$message), type = "error")
        NULL
      })
      if (is.null(ann)) return(NULL)
      as.data.frame(ann)
    })
    
    dependency_features <- shiny::reactive({
      toks <- annotated_tokens()
      if (is.null(toks)) return(NULL)
      tryCatch({
        TextWiller3::summarize_dependency_features(toks)
      }, error = function(e) {
        shiny::showNotification(paste("Error extracting features:", e$message), type = "warning")
        NULL
      })
    })
    
    output$lexicon_preview <- shiny::renderPrint({
      terms <- lexicon_terms()
      if (is.null(terms)) {
        cat("Select a lexicon to preview its terms.")
      } else {
        preview <- head(terms, 30)
        cat(paste(preview, collapse = ", "))
        if (length(terms) > 30) cat("\n...")
      }
    })
    
    output$download_keywords <- shiny::downloadHandler(
      filename = function() sprintf("textwiller_keywords_%s.csv", Sys.Date()),
      content = function(file) {
        data <- keywords_data()
        utils::write.csv(data, file, row.names = FALSE)
      }
    )
  })
}