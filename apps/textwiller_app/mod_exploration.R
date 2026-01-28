# Corpus Exploration Module
mod_exploration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Explore")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Calculate",
          class = "btn-success"
        )
      )
    ),
    
    # Controls
    shiny::wellPanel(
      shiny::h4("Analysis Settings"),
      shiny::fluidRow(
        shiny::column(3,
          shiny::numericInput(ns("top_n_words"), "Top N words:",
            value = 20, min = 5, max = 50, step = 5)
        ),
        shiny::column(3,
          shiny::numericInput(ns("wordcloud_max"), "Wordcloud max words:",
            value = 50, min = 10, max = 100, step = 10)
        ),
        shiny::column(3,
          shiny::checkboxInput(ns("remove_stopwords"), "Remove stopwords", value = TRUE)
        ),
        shiny::column(3,
          shiny::uiOutput(ns("language_badge"))
        )
      ),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(ns("lexicon_resource"), "Lexicon resource",
            choices = c("None" = ""),
            selected = "")
        ),
        shiny::column(6,
          shiny::checkboxInput(ns("filter_lexicon"), "Show only terms present in the lexicon", value = FALSE)
        )
      )
    ),
    
    # Summary Statistics
    shiny::fluidRow(
      shiny::column(12,
        shiny::wellPanel(
          shiny::h4("Corpus Summary"),
          shiny::tableOutput(ns("summary_table")),
          shiny::verbatimTextOutput(ns("lexicon_status"))
        )
      )
    ),

    # Corpus Snapshots (before/after preprocessing)
    shiny::wellPanel(
      shiny::h4("Corpus Snapshots"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(
            ns("corpus_view"),
            "Version to display",
            choices = c("Imported" = "raw", "Preprocessed (if available)" = "processed"),
            selected = "raw"
          )
        ),
        shiny::column(6,
          shiny::numericInput(
            ns("snapshot_rows"),
            "Rows to display",
            value = 10,
            min = 3,
            max = 50,
            step = 1
          )
        )
      ),
      DT::dataTableOutput(ns("corpus_snapshot"))
    ),

    # Concordance / KWIC
    shiny::wellPanel(
      shiny::h4("Concordances (KWIC)"),
      shiny::fluidRow(
        shiny::column(5,
          shiny::textInput(ns("kwic_term"), "Term/regex (case-insensitive):", value = "")
        ),
        shiny::column(3,
          shiny::selectInput(
            ns("kwic_source"),
            "Use corpus",
            choices = c("Imported" = "raw", "Preprocessed" = "processed"),
            selected = "raw"
          )
        ),
        shiny::column(2,
          shiny::numericInput(ns("kwic_window"), "Context words", value = 5, min = 1, max = 20, step = 1)
        ),
        shiny::column(2,
          shiny::actionButton(ns("run_kwic"), "Run KWIC", class = "btn-primary btn-block")
        )
      ),
      DT::dataTableOutput(ns("kwic_table"))
    ),

    # Word Analysis
    shiny::fluidRow(
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Most Frequent Words"),
          shiny::plotOutput(ns("freq_plot"))
        )
      ),
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Word Cloud"),
          shiny::plotOutput(ns("wordcloud_plot"))
        )
      )
    ),
    
    # Detailed Tables
    shiny::wellPanel(
      shiny::h4("Most Frequent Words"),
      DT::dataTableOutput(ns("freq_table"))
    )
  )
}

mod_exploration_server <- function(id, corpus, processed_corpus = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    lang_cfg <- TextWiller3::get_language_config()
    calc_trigger <- shiny::reactiveVal(Sys.time())

    shiny::observeEvent(input$calculate, {
      calc_trigger(Sys.time())
    })

    analysis_language <- shiny::reactive({
      lang_cfg$get_version()
      lang_cfg$current_language
    })
    
    output$language_badge <- shiny::renderUI({
      lang <- analysis_language()
      label <- ifelse(lang == "it", "Italian", "English")
      shiny::p(
        shiny::strong("Analysis language: "),
        label,
        shiny::span(" (change in Language & Resources)", style = "font-size:90%; color:#7f8c8d;")
      )
    })
    lang_cfg <- TextWiller3::get_language_config()
    
    lexicon_resources <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(lang_cfg$get_version(), analysis_language()),
      valueFunc = function() {
        TextWiller3::list_language_resources(
          language = analysis_language(),
          type = "lexicon",
          include_content = FALSE
        )
      }
    )
    
    shiny::observe({
      resources <- lexicon_resources()
      choices <- c("None" = "")
      if (!is.null(resources) && nrow(resources) > 0) {
        labels <- paste0(resources$name, " [", resources$type, "]")
        names(labels) <- resources$name
        choices <- c(choices, labels)
      }
      shiny::updateSelectInput(session, "lexicon_resource", choices = choices)
    })
    
    lexicon_terms <- shiny::reactive({
      res_name <- input$lexicon_resource
      if (is.null(res_name) || res_name == "") {
        return(NULL)
      }
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
    })
    
    # Calculate word frequencies reactively - VERSIONE SICURA
    word_frequencies <- shiny::reactive({
      calc_trigger()
      shiny::req(corpus())
      
      # Usa una funzione sicura che non dipende da TextWiller3
      calculate_word_frequencies_safe <- function(text, remove_stopwords = FALSE, language = "it") {
        if (is.null(text) || length(text) == 0) {
          return(data.frame(word = character(), frequency = numeric()))
        }
        
        # Combine all documents
        all_text <- paste(text, collapse = " ")
        
        # Split into words
        words <- strsplit(tolower(all_text), "\\s+")[[1]]
        words <- words[nchar(words) > 1]  # Remove single characters
        
        # Remove stopwords if requested
        if (remove_stopwords) {
          stopwords_list <- TextWiller3::get_language_config()$get_stopwords(language)
          words <- words[!tolower(words) %in% tolower(stopwords_list)]
        }
        
        # Calculate frequencies
        freq_table <- table(words)
        freq_table <- sort(freq_table, decreasing = TRUE)
        
        data.frame(
          word = names(freq_table),
          frequency = as.numeric(freq_table),
          stringsAsFactors = FALSE
        )
      }
      
      # Usa la funzione sicura
      calculate_word_frequencies_safe(
        corpus(), 
        remove_stopwords = input$remove_stopwords,
        language = analysis_language()
      )
    })
    
    display_frequencies <- shiny::reactive({
      calc_trigger()
      freq <- word_frequencies()
      terms <- lexicon_terms()
      if (isTRUE(input$filter_lexicon) && !is.null(terms) && length(terms) > 0) {
        freq <- freq[tolower(freq$word) %in% terms, , drop = FALSE]
      }
      freq
    })
    
    # Get corpus stats - VERSIONE SICURA
    get_corpus_stats_safe <- function(text) {
      if (is.null(text) || length(text) == 0) {
        return(list(
          n_docs = 0,
          total_words = 0,
          total_chars = 0,
          avg_words = 0,
          avg_chars = 0
        ))
      }
      
      word_counts <- sapply(strsplit(text, "\\s+"), length)
      char_counts <- nchar(text)
      
      list(
        n_docs = length(text),
        total_words = sum(word_counts),
        total_chars = sum(char_counts),
        avg_words = round(mean(word_counts), 1),
        avg_chars = round(mean(char_counts), 1),
        max_words = max(word_counts),
        min_words = min(word_counts)
      )
    }
    
    # Summary table
    output$summary_table <- shiny::renderTable({
      shiny::req(corpus())
      
      stats <- get_corpus_stats_safe(corpus())
      freq_data <- word_frequencies()
      
      tokens <- unlist(strsplit(tolower(paste(corpus(), collapse = " ")), "\\s+"))
      tokens <- tokens[nchar(tokens) > 0]
      types <- table(tokens)
      total_tokens <- length(tokens)
      total_types <- length(types)
      hapax <- sum(types == 1)
      hapax_pct <- if (total_tokens > 0) round(hapax / total_tokens * 100, 2) else 0
      ttr <- if (total_tokens > 0) round(total_types / total_tokens, 3) else 0
      lexical_stats <- TextWiller3::calculate_lexical_diversity(corpus())
      lexical_div <- lexical_stats$value[lexical_stats$measure == "lexical_density"]
      lexical_div <- ifelse(length(lexical_div) == 0, 0, lexical_div)
      
      summary_df <- data.frame(
        Statistic = c(
          "Total Documents",
          "Word Tokens (N)", 
          "Word Types (V)",
          "Average Words/Doc",
          "Hapax %",
          "Type Token Ratio (TTR)",
          "Lexical Diversity"
        ),
        Value = c(
          stats$n_docs,
          total_tokens,
          total_types,
          stats$avg_words,
          hapax_pct,
          ttr,
          lexical_div
        )
      )
      
      lex_terms <- lexicon_terms()
      if (!is.null(lex_terms) && length(lex_terms) > 0) {
        lex_token_cov <- if (total_tokens > 0) round(sum(tokens %in% lex_terms) / total_tokens * 100, 2) else 0
        lex_type_cov <- sum(names(types) %in% lex_terms)
        summary_df <- rbind(
          summary_df,
          data.frame(
            Statistic = c("Lexicon coverage (tokens)", "Lexicon terms in V"),
            Value = c(paste0(lex_token_cov, "%"), lex_type_cov)
          )
        )
      }
      
      summary_df
    }, bordered = TRUE, align = 'l', width = '100%')
    
    output$lexicon_status <- shiny::renderPrint({
      res_name <- input$lexicon_resource
      if (is.null(res_name) || res_name == "") {
        cat("No lexicon selected.")
        return()
      }
      terms <- lexicon_terms()
      if (is.null(terms)) {
        cat("Impossibile leggere il contenuto del lexicon selezionato.")
        return()
      }
      cat(sprintf("Lexicon \"%s\" con %d termini totali.", res_name, length(terms)))
    })

    # Snapshot of corpus (raw vs processed)
    selected_corpus_for_view <- shiny::reactive({
      calc_trigger()
      choice <- input$corpus_view
      if (choice == "processed" && !is.null(processed_corpus) && !is.null(processed_corpus())) {
        return(processed_corpus())
      }
      corpus()
    })

    escape_regex_local <- function(x) {
      gsub("([.\\^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", x)
    }

    output$corpus_snapshot <- DT::renderDataTable({
      texts_raw <- corpus()
      texts_proc <- if (!is.null(processed_corpus)) processed_corpus() else NULL
      view_choice <- input$corpus_view
      n <- input$snapshot_rows
      if (is.null(n) || !is.numeric(n)) n <- 10
      idx <- seq_len(min(n, length(texts_raw)))

      df <- data.frame(
        Document = idx,
        Importata = if (length(texts_raw) > 0) substr(texts_raw[idx], 1, 200) else character(length(idx)),
        stringsAsFactors = FALSE
      )
      if (!is.null(texts_proc) && length(texts_proc) >= max(idx)) {
        df$Preprocessata <- substr(texts_proc[idx], 1, 200)
      } else {
        df$Preprocessata <- NA_character_
      }

      if (view_choice == "raw") {
        df <- df[, c("Document", "Imported"), drop = FALSE]
      } else if (view_choice == "processed") {
        df <- df[, c("Document", "Preprocessed"), drop = FALSE]
      }

      DT::datatable(
        df,
        options = list(pageLength = n, dom = 'tip', scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Concordance / KWIC
    kwic_data <- shiny::eventReactive(input$run_kwic, {
      term <- trimws(input$kwic_term)
      if (!nzchar(term)) {
        shiny::showNotification("Enter a term for concordance.", type = "warning")
        return(NULL)
      }
      source_choice <- input$kwic_source
      texts <- if (source_choice == "processed" && !is.null(processed_corpus) && !is.null(processed_corpus())) {
        processed_corpus()
      } else {
        corpus()
      }
      if (is.null(texts) || length(texts) == 0) {
        shiny::showNotification("No text available for concordance.", type = "warning")
        return(NULL)
      }

      kwic_window <- input$kwic_window
      if (is.null(kwic_window) || !is.numeric(kwic_window)) kwic_window <- 5
      kwic_window <- max(1, kwic_window)
      pattern <- paste0("\\b", escape_regex_local(term), "\\b")
      if (is.null(pattern)) return(NULL)

      build_kwic <- function(text_vec, pat, window) {
        results <- list()
        for (i in seq_along(text_vec)) {
          tokens <- unlist(strsplit(text_vec[i], "\\s+"))
          if (length(tokens) == 0) next
          match_idx <- which(grepl(pat, tokens, ignore.case = TRUE, perl = TRUE))
          if (length(match_idx) == 0) next
          for (m in match_idx) {
            left_start <- max(1, m - window)
            right_end <- min(length(tokens), m + window)
            left_ctx <- paste(tokens[left_start:(m - 1)], collapse = " ")
            right_ctx <- paste(tokens[(m + 1):right_end], collapse = " ")
            results[[length(results) + 1]] <- data.frame(
              Documento = i,
              Left = left_ctx,
              Keyword = tokens[m],
              Right = right_ctx,
              stringsAsFactors = FALSE
            )
          }
        }
        if (length(results) == 0) {
          return(data.frame(Documento = integer(), Left = character(), Keyword = character(), Right = character()))
        }
        do.call(rbind, results)
      }

      build_kwic(texts, pattern, kwic_window)
    })

    output$kwic_table <- DT::renderDataTable({
      kwic <- kwic_data()
      if (is.null(kwic)) return(NULL)
      DT::datatable(
        kwic,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Word frequency plot
    output$freq_plot <- shiny::renderPlot({
      shiny::req(display_frequencies())
      
      freq_data <- display_frequencies()
      top_words <- head(freq_data, input$top_n_words)
      
      ggplot2::ggplot(top_words, ggplot2::aes(x = reorder(word, frequency), y = frequency)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Top", input$top_n_words, "Most Frequent Words"),
          x = "Words",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 10)
        )
    })
    
    # Word cloud
    output$wordcloud_plot <- shiny::renderPlot({
      shiny::req(display_frequencies())
      
      freq_data <- head(display_frequencies(), input$wordcloud_max)
      
      if (nrow(freq_data) > 0 && requireNamespace("wordcloud", quietly = TRUE)) {
        wordcloud::wordcloud(
          words = freq_data$word,
          freq = freq_data$frequency,
          max.words = input$wordcloud_max,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.8),
          random.order = FALSE,
          rot.per = 0.3
        )
      } else if (nrow(freq_data) > 0) {
        # Fallback: bar plot se wordcloud non disponibile
        top_words <- head(freq_data, 15)
        ggplot2::ggplot(top_words, ggplot2::aes(x = reorder(word, frequency), y = frequency)) +
          ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Top Words (Wordcloud not available)", x = "Words", y = "Frequency") +
          ggplot2::theme_minimal()
      }
    })
    
    # Frequency table
    output$freq_table <- DT::renderDataTable({
      shiny::req(display_frequencies())
      
      freq_data <- display_frequencies()
      if (nrow(freq_data) > 0) {
        freq_data$percentage <- round(freq_data$frequency / sum(freq_data$frequency) * 100, 2)
      } else {
        freq_data$percentage <- numeric(0)
      }
      
      DT::datatable(
        freq_data,
        options = list(
          pageLength = 10,
          order = list(1, 'desc'),
          dom = 'ltipr'
        ),
        colnames = c('Word', 'Frequency', 'Percentage (%)'),
        rownames = FALSE,
        caption = 'Word Frequency Distribution'
      ) %>% 
        DT::formatStyle('frequency', fontWeight = 'bold') %>%
        DT::formatStyle('percentage', color = 'steelblue')
    })
  })
}
