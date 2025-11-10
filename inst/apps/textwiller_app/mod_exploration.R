# Corpus Exploration Module
mod_exploration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Corpus Exploration & Visualization"),
    
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
            choices = c("Nessuna" = ""),
            selected = "")
        ),
        shiny::column(6,
          shiny::checkboxInput(ns("filter_lexicon"), "Mostra solo termini presenti nel lexicon", value = FALSE)
        )
      )
    ),
    
    # Summary Statistics
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Corpus Summary"),
          shiny::tableOutput(ns("summary_table")),
          shiny::verbatimTextOutput(ns("lexicon_status"))
        )
      ),
      shiny::column(8,
        shiny::wellPanel(
          shiny::h4("Document Length Distribution"),
          shiny::plotOutput(ns("length_histogram"))
        )
      )
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
      shiny::h4("Detailed Word Frequencies"),
      DT::dataTableOutput(ns("freq_table"))
    )
  )
}

mod_exploration_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    lang_cfg <- TextWiller3::get_language_config()
    analysis_language <- shiny::reactive({
      lang_cfg$get_version()
      lang_cfg$current_language
    })
    
    output$language_badge <- shiny::renderUI({
      lang <- analysis_language()
      label <- ifelse(lang == "it", "Italiano", "English")
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
      choices <- c("Nessuna" = "")
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
      freq <- word_frequencies()
      terms <- lexicon_terms()
      if (isTRUE(input$filter_lexicon) && !is.null(terms) && length(terms) > 0) {
        freq <- freq[tolower(freq$word) %in% terms, , drop = FALSE]
      }
      freq
    })
    
    # Get document lengths - VERSIONE SICURA
    doc_lengths <- shiny::reactive({
      shiny::req(corpus())
      
      # Funzione sicura per lunghezze documento
      get_document_lengths_safe <- function(text) {
        if (is.null(text) || length(text) == 0) {
          return(data.frame(
            document_id = integer(),
            word_count = integer(),
            char_count = integer(),
            stringsAsFactors = FALSE
          ))
        }
        
        word_counts <- sapply(strsplit(text, "\\s+"), length)
        char_counts <- nchar(text)
        
        data.frame(
          document_id = seq_along(text),
          word_count = word_counts,
          char_count = char_counts,
          stringsAsFactors = FALSE
        )
      }
      
      get_document_lengths_safe(corpus())
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
        cat("Nessun lexicon selezionato.")
        return()
      }
      terms <- lexicon_terms()
      if (is.null(terms)) {
        cat("Impossibile leggere il contenuto del lexicon selezionato.")
        return()
      }
      cat(sprintf("Lexicon \"%s\" con %d termini totali.", res_name, length(terms)))
    })
    
    # Document length histogram
    output$length_histogram <- shiny::renderPlot({
      shiny::req(doc_lengths())
      
      lengths <- doc_lengths()
      
      ggplot2::ggplot(lengths, ggplot2::aes(x = word_count)) +
        ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7, color = "white") +
        ggplot2::labs(
          title = "Distribution of Document Lengths (Words)",
          x = "Words per Document",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
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
