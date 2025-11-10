# Lexical Intelligence Lab
mod_lexical_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Lexical Analysis"),
    
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
            choices = c("Nessuna" = ""),
            selected = "")
        ),
        shiny::column(3,
          shiny::checkboxInput(ns("filter_lexicon"), "Show lexicon terms only", value = FALSE)
        )
      ),
      shiny::uiOutput(ns("language_badge"))
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
              shiny::h5("Advanced Brunato features"),
              shiny::tableOutput(ns("placeholder_table"))
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
        "References",
        shiny::wellPanel(
          shiny::h4("Bibliography"),
          shiny::HTML("
            <ul>
              <li>Brunato, D. et al. (2018). <em>A Two-level Approach to Measure Text Complexity in Italian.</em></li>
              <li>Gibson, E. (1998). <em>Linguistic complexity: locality of syntactic dependencies.</em></li>
              <li>Collins-Thompson, K. (2015). <em>Computational assessment of text readability.</em></li>
            </ul>
          ")
        )
      )
    )
  )
}

mod_lexical_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    lang_cfg <- TextWiller3::get_language_config()
    
    analysis_language <- shiny::reactive({
      lang_cfg$get_version()
      lang_cfg$current_language
    })
    
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
    
    lexical_freq <- shiny::reactive({
      shiny::req(corpus())
      TextWiller3::calculate_word_frequencies_enhanced(corpus(), preprocess = TRUE)
    })
    
    diversity_data <- shiny::reactive({
      shiny::req(corpus())
      TextWiller3::calculate_lexical_diversity(corpus())
    })
    
    complexity_data <- shiny::reactive({
      shiny::req(corpus())
      TextWiller3::calculate_brunato_measures(corpus())
    })
    
    vocabulary_stats <- shiny::reactive({
      freq <- lexical_freq()
      list(
        total_words = sum(freq$frequency),
        unique_words = nrow(freq),
        freq = freq
      )
    })
    
    keywords_data <- shiny::reactive({
      shiny::req(corpus())
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
    })
    
    filtered_freq <- shiny::reactive({
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
    
    output$language_badge <- shiny::renderUI({
      label <- ifelse(analysis_language() == "it", "Italiano", "English")
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
      basic_features <- TextWiller3::summarize_lexical_features(corpus())
      data.frame(
        Metric = c("Total words", "Unique words", "Lexicon coverage (%)"),
        Value = c(vocab$total_words, vocab$unique_words, coverage_tokens)
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$complexity_table <- shiny::renderTable({
      basic_features <- TextWiller3::summarize_lexical_features(corpus())
      basic_features$basic
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    output$placeholder_table <- shiny::renderTable({
      TextWiller3::summarize_lexical_features(corpus())$placeholders
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
        cat("Nessun lexicon selezionato.")
      } else {
        cat(sprintf("Lexicon \"%s\" con %d termini totali.", input$lexicon_resource, length(terms)))
      }
    })
    
    output$lexicon_preview <- shiny::renderPrint({
      terms <- lexicon_terms()
      if (is.null(terms)) {
        cat("Seleziona un lexicon per visualizzarne un'anteprima.")
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
