# Sentiment Analysis Module
mod_sentiment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Sentiment Analysis (Italiano)")),
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
      shiny::h4("Configurazione Analisi"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("sentiment_algorithm"), "Algoritmo:",
            choices = c("Mattivio" = "mattivio", "Maddalena" = "maddalena", "Generale" = "general"),
            selected = "mattivio")
        ),
        shiny::column(4,
          shiny::checkboxInput(ns("normalize_text"), "Normalizza testo", value = TRUE)
        )
      ),
      shiny::actionButton(ns("run_sentiment"), "Analizza Sentiment", 
                         class = "btn-primary btn-block")
    ),
    
    # Results
    shiny::wellPanel(
      shiny::h4("Risultati Sentiment"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::plotOutput(ns("sentiment_plot"))
        ),
        shiny::column(6,
          shiny::tableOutput(ns("sentiment_summary"))
        )
      ),
      shiny::hr(),
      shiny::h4("Distribuzione Dettagliata"),
      DT::dataTableOutput(ns("sentiment_table"))
    ),
    
    # Dictionary Info
    shiny::wellPanel(
      shiny::h4("Dizionari Sentiment"),
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
      
      shiny::showNotification("Analizzando sentiment...", type = "message")
      
      tryCatch({
        scores_raw <- TextWiller3::analyze_sentiment_it(
          corpus(),
          use_legacy = FALSE  # Usa sempre il dizionario del pacchetto
        )
        
        scores <- as_numeric_scores(scores_raw)
        if (length(scores) == 0) {
          stop("Nessun punteggio sentiment numerico generato.")
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
        shiny::showNotification("Analisi sentiment completata!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Errore nell'analisi sentiment:", e$message), type = "error")
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
            title = "Distribuzione Sentiment",
            x = "Categoria Sentiment",
            y = "Numero Documenti"
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
          Metrica = c("Documenti Positivi", "Documenti Negativi", "Documenti Neutrali", "Sentiment Medio"),
          Valore = c(
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
          caption = "Analisi Sentiment Dettagliata"
        ) %>%
          DT::formatStyle('sentiment', 
                         color = DT::styleInterval(c(-0.1, 0.1), c('red', 'gray', 'green')))
      }
    })
    
    # Dictionary info
    output$dictionary_info <- shiny::renderPrint({
      tryCatch({
        dicts <- TextWiller3::get_sentiment_dictionaries()
        cat("Dizionari Sentiment Disponibili:\n")
        cat("================================\n")
        for (dict_name in names(dicts)) {
          dict <- dicts[[dict_name]]
          if (is.data.frame(dict)) {
            cat(sprintf("- %s: %d termini\n", dict_name, nrow(dict)))
          } else {
            cat(sprintf("- %s: oggetto di tipo %s\n", dict_name, class(dict)))
          }
        }
      }, error = function(e) {
        cat("Impossibile caricare informazioni sui dizionari:\n", e$message)
      })
    })
  })
}
