# Sentiment Analysis Module
mod_sentiment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Sentiment Analysis (Italian)")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Calculate",
          class = "btn-success"
        )
      )
    ),
    
    # Analysis Controls
    shiny::wellPanel(
      shiny::h4("Analysis Setup"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("sentiment_algorithm"), "Algorithm:",
            choices = c(
              "Mattivio (external vocabulary)" = "mattivio",
              "Maddalena (external vocabulary)" = "maddalena",
              "General (built-in dictionary)" = "general"
            ),
            selected = "mattivio")
        ),
        shiny::column(4,
          shiny::checkboxInput(ns("normalize_text"), "Normalize text", value = TRUE)
        )
      ),
      shiny::helpText("Mattivio/Maddalena use registered external vocabularies; General uses the package built-in dictionary."),
      shiny::actionButton(ns("run_sentiment"), "Run Sentiment Analysis", 
                         class = "btn-primary btn-block")
    ),
    
    # Results
    shiny::wellPanel(
      shiny::h4("Sentiment Results"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::plotOutput(ns("sentiment_plot"))
        ),
        shiny::column(6,
          shiny::tableOutput(ns("sentiment_summary"))
        )
      ),
      shiny::hr(),
      shiny::h4("Detailed Distribution"),
      DT::dataTableOutput(ns("sentiment_table"))
    ),
    
    # Dictionary Info
    shiny::wellPanel(
      shiny::h4("Sentiment Dictionaries"),
      shiny::verbatimTextOutput(ns("dictionary_info"))
    )
  )
}

mod_sentiment_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive for sentiment results
    sentiment_results <- shiny::reactiveVal()
    
    as_numeric_scores <- function(x) {
      if (is.null(x)) return(numeric(0))
      if (is.data.frame(x)) {
        candidate_cols <- intersect(c("sentiment", "score", "value", "sentiment_score"), names(x))
        if (length(candidate_cols) > 0) {
          x <- x[[candidate_cols[[1]]]]
        }
      }
      if (is.list(x)) {
        x <- unlist(x, use.names = FALSE)
      }
      x <- suppressWarnings(as.numeric(x))
      x <- x[!is.na(x)]
      x
    }
    
    # Run sentiment analysis
    shiny::observeEvent(list(input$run_sentiment, input$calculate), {
      shiny::req(corpus())
      
      shiny::showNotification("Running sentiment analysis...", type = "message")
      
      tryCatch({
        algo_input <- input$sentiment_algorithm
        algo_normalized <- if (identical(algo_input, "maddalena")) {
          "Maddalena"
        } else if (identical(algo_input, "mattivio")) {
          "Mattivio"
        } else {
          algo_input
        }
        use_legacy <- !identical(algo_input, "general")
        
        scores_raw <- TextWiller3::analyze_sentiment_it(
          corpus(),
          algorithm = algo_normalized,
          normalizzaTesti = isTRUE(input$normalize_text),
          use_legacy = use_legacy  # usa il legacy per Mattivio/Maddalena, dizionario generale altrimenti
        )
        
        scores <- as_numeric_scores(scores_raw)
        if (length(scores) == 0) {
          stop("No numeric sentiment score generated.")
        }
        # Allinea lunghezza ai documenti se necessario
        if (length(scores) != length(corpus())) {
          scores <- head(scores, length(corpus()))
        }
        
        # Create results object
        results <- list(
          scores = scores,
          summary = TextWiller3::summarize_sentiment(scores),
          data = data.frame(
            document = seq_along(corpus()),
            text = substr(corpus(), 1, 100),
            sentiment = scores,
            sentiment_label = ifelse(scores > 0, "Positive", 
                                   ifelse(scores < 0, "Negative", "Neutral")),
            stringsAsFactors = FALSE
          )
        )
        
        sentiment_results(results)
        shiny::showNotification("Sentiment analysis completed!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error in sentiment analysis:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)
    
    # Sentiment plot
    output$sentiment_plot <- shiny::renderPlot({
      results <- sentiment_results()
      if (!is.null(results)) {
        data <- results$data
        
        ggplot2::ggplot(data, ggplot2::aes(x = sentiment_label, fill = sentiment_label)) +
          ggplot2::geom_bar(alpha = 0.8) +
          ggplot2::scale_fill_manual(values = c("Negative" = "red", "Neutral" = "gray", "Positive" = "green")) +
          ggplot2::labs(
            title = "Sentiment Distribution",
            x = "Sentiment Category",
            y = "Document Count"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
      }
    })
    
    # Sentiment summary table
    output$sentiment_summary <- shiny::renderTable({
      results <- sentiment_results()
      if (!is.null(results)) {
        summary <- results$summary
        data.frame(
          Metric = c("Positive Documents", "Negative Documents", "Neutral Documents", "Mean Sentiment"),
          Value = c(
            summary$positive,
            summary$negative, 
            summary$neutral,
            round(summary$mean_sentiment, 3)
          )
        )
      }
    }, bordered = TRUE, align = 'l', width = '100%')
    
    # Detailed sentiment table
    output$sentiment_table <- DT::renderDataTable({
      results <- sentiment_results()
      if (!is.null(results)) {
        DT::datatable(
          results$data,
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE,
          caption = "Detailed Sentiment Analysis"
        ) %>%
          DT::formatStyle('sentiment', 
                         color = DT::styleInterval(c(-0.1, 0.1), c('red', 'gray', 'green')))
      }
    })
    
    # Dictionary info
    output$dictionary_info <- shiny::renderPrint({
      tryCatch({
        dicts <- TextWiller3::get_sentiment_dictionaries()
        cat("Available Sentiment Dictionaries:\n")
        cat("================================\n")
        for (dict_name in names(dicts)) {
          dict <- dicts[[dict_name]]
          if (is.data.frame(dict)) {
            cat(sprintf("- %s: %d terms\n", dict_name, nrow(dict)))
          } else {
            cat(sprintf("- %s: object of class %s\n", dict_name, class(dict)))
          }
        }
      }, error = function(e) {
        cat("Unable to load dictionary information:\n", e$message)
      })
    })
  })
}
