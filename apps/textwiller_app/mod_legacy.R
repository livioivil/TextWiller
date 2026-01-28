### FILE: mod_legacy.R

# Modulo per le funzioni Legacy di TextWiller

#helper
get_example_text <- function() {
  return("Hi!!! How are you??? It's a great day!!! #beautiful 😊 
Visit the site: http://example.com and follow me @myhandle")
}

mod_legacy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("TextWiller Legacy Functions - Original Features"),
    shiny::p("Placeholder for the original TextWiller functions for Italian text analysis."),
    
    shiny::wellPanel(
      shiny::h4("📝 Text Normalization"),
      shiny::textAreaInput(ns("legacy_text"), "Text to normalize:",
        value = "Hi!!! How are you??? It's a great day!!! #beautiful",
        rows = 4,
        placeholder = "Enter text to normalize..."
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::actionButton(ns("normalizza_btn"), "Normalize Text", class = "btn-primary")),
        shiny::column(3, shiny::actionButton(ns("emoticon_btn"), "Normalize Emoticons", class = "btn-info")),
        shiny::column(3, shiny::actionButton(ns("html_btn"), "Normalize HTML", class = "btn-info")),
        shiny::column(3, shiny::actionButton(ns("slang_btn"), "Normalize Slang", class = "btn-info"))
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("😊 Sentiment Analysis"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(ns("sentiment_algo"), "Algorithm:",
            choices = c("Mattivio" = "Mattivio", "Maddalena" = "Maddalena"),
            selected = "Mattivio")
        ),
        shiny::column(6,
          shiny::checkboxInput(ns("sentiment_normalize"), "Normalize text", value = TRUE)
        )
      ),
      shiny::actionButton(ns("sentiment_btn"), "Run Sentiment", class = "btn-success")
    ),
    
    shiny::wellPanel(
      shiny::h4("👥 User Classification"),
      shiny::textInput(ns("nomi_input"), "Names to classify (comma-separated):",
        value = "marco,maria,alessandro,alessandra,luca"),
      shiny::actionButton(ns("classifica_btn"), "Classify Gender", class = "btn-warning")
    ),
    
    shiny::wellPanel(
      shiny::h4("🔗 Pattern Extraction"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("pattern_input"), "Regex pattern:", value = "@\\w+")
        ),
        shiny::column(6,
          shiny::selectInput(ns("pattern_type"), "Pattern type:",
            choices = c("Mentions @" = "@\\w+", "Hashtag #" = "#\\w+", "URL" = "http[^\\s]+"),
            selected = "@\\w+")
        )
      ),
      shiny::actionButton(ns("pattern_btn"), "Extract Pattern", class = "btn-info")
    ),
    
    shiny::hr(),
    
    # Risultati
    shiny::wellPanel(
      shiny::h4("📊 Results"),
      shiny::tabsetPanel(
        id = ns("results_tabs"),
        shiny::tabPanel("Normalized Text", shiny::verbatimTextOutput(ns("normalized_output"))),
        shiny::tabPanel("Sentiment", 
          shiny::tableOutput(ns("sentiment_table")),
          shiny::plotOutput(ns("sentiment_plot"))
        ),
        shiny::tabPanel("Classification", shiny::tableOutput(ns("classification_table"))),
        shiny::tabPanel("Pattern", shiny::tableOutput(ns("pattern_table"))),
        shiny::tabPanel("Function Details", shiny::verbatimTextOutput(ns("function_info")))
      )
    ),
    
    # Informazioni sulle funzioni legacy
    shiny::wellPanel(
      shiny::h4("ℹ️ Legacy Function Information"),
      shiny::HTML("
        <p><strong>TextWiller Legacy</strong> preserves all the original functions of the historic package for Italian text analysis:</p>
        <ul>
          <li><strong>Advanced normalization</strong> for Italian text (slang, emoticons, punctuation)</li>
          <li><strong>Sentiment analysis</strong> tailored for Italian</li>
          <li><strong>Italian name classification</strong> by gender</li>
          <li><strong>Pattern extraction</strong> (URLs, mentions, hashtags)</li>
          <li><strong>Specialized dictionaries</strong> for Italian</li>
          <li><strong>And more</strong> for Italian</li>
        </ul>
        <p><em>And more to come....</em></p>
      ")
    )
  )
}

# Server per il modulo Legacy
### FILE: mod_legacy.R - VERSIONE CORRETTA CON DIPENDENZE
mod_legacy_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Carica i pacchetti necessari
    ensure_packages <- function() {
      required_packages <- c("stringr", "SnowballC", "dplyr", "tibble", "tidytext")
      missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
      
      if(length(missing_packages) > 0) {
        shiny::showNotification(
          paste("Installing missing packages:", paste(missing_packages, collapse = ", ")),
          type = "message"
        )
        install.packages(missing_packages, quiet = TRUE)
      }
      
      # Carica i pacchetti
      suppressPackageStartupMessages({
        library(stringr, quietly = TRUE)
        library(SnowballC, quietly = TRUE)
        library(dplyr, quietly = TRUE)
        library(tibble, quietly = TRUE)
        library(tidytext, quietly = TRUE)
      })
    }
    
    # Carica i dizionari necessari
    load_dictionaries <- function() {
      # Crea dizionari di fallback se quelli originali non esistono
      if (!exists("vocabolario_nomi_propri")) {
        vocabolario_nomi_propri <<- data.frame(
          categoria = c("masc", "femm", "masc", "femm", "masc", "femm"),
          row.names = c("marco", "maria", "alessandro", "alessandra", "luca", "lucia")
        )
      }
      
      if (!exists("stopwords_ita")) {
        stopwords_ita <<- c("il", "lo", "la", "i", "gli", "le", "un", "uno", "una",
                          "di", "a", "da", "in", "con", "su", "per", "tra", "fra",
                          "è", "sono", "era", "erano", "essere", "avere", "ha", "hanno")
      }
    }
    
    # Inizializza all'avvio
    shiny::observe({
      ensure_packages()
      load_dictionaries()
    })
    
    # Normalizzazione testo 
    shiny::observeEvent(input$normalizza_btn, {
      shiny::req(input$legacy_text)
      
      tryCatch({
        ensure_packages()
        
        if (exists("normalizzaTesti")) {
          risultato <- normalizzaTesti(input$legacy_text)
          output$normalized_output <- shiny::renderPrint({
            cat("ORIGINAL TEXT:\n")
            cat(input$legacy_text, "\n\n")
            cat("NORMALIZED TEXT:\n")
            cat(risultato, "\n\n")
          })
        } else {
          # Fallback: normalizzazione basica
          testo <- input$legacy_text
          testo <- tolower(testo)
          testo <- gsub("[[:punct:]]", " ", testo)
          testo <- gsub("\\s+", " ", testo)
          testo <- trimws(testo)
          
          output$normalized_output <- shiny::renderPrint({
            cat("ORIGINAL TEXT:\n")
            cat(input$legacy_text, "\n\n")
            cat("NORMALIZED TEXT (FALLBACK):\n")
            cat(testo, "\n\n")
            cat("⚠️ Function normalizzaTesti not available - using fallback\n")
          })
        }
      }, error = function(e) {
        output$normalized_output <- shiny::renderPrint({
          cat("Normalization error:", e$message, "\n")
        })
      })
    })
    
    # Analisi Sentiment - VERSIONE SICURA
    shiny::observeEvent(input$sentiment_btn, {
      shiny::req(input$legacy_text)
      
      tryCatch({
        ensure_packages()
        
        if (exists("sentiment")) {
          scores <- sentiment(
            input$legacy_text,
            algorithm = input$sentiment_algo,
            normalizzaTesti = input$sentiment_normalize
          )
          
          # Tabella risultati
          output$sentiment_table <- shiny::renderTable({
            data.frame(
              Text = substr(input$legacy_text, 1, 50),
              Score = scores,
              Sentiment = ifelse(scores > 0, "POSITIVE", 
                               ifelse(scores < 0, "NEGATIVE", "NEUTRAL")),
              Algorithm = input$sentiment_algo
            )
          }, bordered = TRUE)
          
          # Plot sentiment semplificato
          output$sentiment_plot <- shiny::renderPlot({
            sentiment_val <- ifelse(scores > 0, "Positive", 
                                  ifelse(scores < 0, "Negative", "Neutral"))
            
            plot_data <- data.frame(
              Sentiment = factor(sentiment_val, levels = c("Negative", "Neutral", "Positive")),
              Value = 1
            )
            
            ggplot2::ggplot(plot_data, ggplot2::aes(x = Sentiment, fill = Sentiment)) +
              ggplot2::geom_bar(alpha = 0.8) +
              ggplot2::scale_fill_manual(values = c("Negative" = "red", "Neutral" = "gray", "Positive" = "green")) +
              ggplot2::labs(title = "Sentiment Analysis Result") +
              ggplot2::theme_minimal()
          })
          
        } else {
          # Fallback per sentiment
          output$sentiment_table <- shiny::renderTable({
            data.frame(
              Message = "Sentiment function not available - using fallback",
              Score = 0,
              Sentiment = "NEUTRAL"
            )
          }, bordered = TRUE)
        }
        
      }, error = function(e) {
        output$sentiment_table <- shiny::renderTable({
          data.frame(Error = paste("Sentiment analysis error:", e$message))
        })
      })
    })
    
    # Classificazione nomi - VERSIONE SICURA
    shiny::observeEvent(input$classifica_btn, {
      shiny::req(input$nomi_input)
      
      tryCatch({
        ensure_packages()
        load_dictionaries()
        
        if (exists("classificaUtenti")) {
          nomi <- strsplit(input$nomi_input, ",")[[1]]
          nomi <- trimws(nomi)
          
          classificazione <- classificaUtenti(nomi)
          
          output$classification_table <- shiny::renderTable({
            data.frame(
              Name = nomi,
              Gender = classificazione,
              stringsAsFactors = FALSE
            )
          }, bordered = TRUE)
        } else {
          # Fallback per classificazione
          nomi <- strsplit(input$nomi_input, ",")[[1]]
          nomi <- trimws(nomi)
          
          # Classificazione basica basata su desinenze italiane
          classifica_basica <- function(nomi) {
            sapply(nomi, function(nome) {
              nome_lower <- tolower(nome)
              if (grepl("a$", nome_lower)) "female"
              else if (grepl("o$|e$|i$", nome_lower)) "male"
              else "unknown"
            })
          }
          
          classificazione <- classifica_basica(nomi)
          
          output$classification_table <- shiny::renderTable({
            data.frame(
              Name = nomi,
              Gender = classificazione,
              Note = "⚠️ Basic classification (fallback)",
              stringsAsFactors = FALSE
            )
          }, bordered = TRUE)
        }
      }, error = function(e) {
        output$classification_table <- shiny::renderTable({
          data.frame(Error = paste("Classification error:", e$message))
        })
      })
    })
    
    # Estrazione pattern - VERSIONE SICURA
    shiny::observeEvent(input$pattern_btn, {
      shiny::req(input$legacy_text)
      
      tryCatch({
        ensure_packages()
        
        if (exists("patternExtract")) {
          pattern <- input$pattern_input
          if (input$pattern_type != "") {
            pattern <- input$pattern_type
          }
          
          risultato <- patternExtract(input$legacy_text, pattern = pattern)
          
          output$pattern_table <- shiny::renderTable({
            if (nrow(risultato) > 0) {
              head(risultato, 10)  # Mostra solo primi 10 risultati
            } else {
              data.frame(Result = "No pattern found")
            }
          }, bordered = TRUE)
        } else {
          # Fallback per estrazione pattern
          pattern <- input$pattern_input
          if (input$pattern_type != "") {
            pattern <- input$pattern_type
          }
          
          # Estrazione basica con stringr
          matches <- stringr::str_extract_all(input$legacy_text, pattern)[[1]]
          
          output$pattern_table <- shiny::renderTable({
            if (length(matches) > 0) {
              data.frame(
                Found_Patterns = matches,
                Note = "⚠️ Extraction with fallback (stringr)"
              )
            } else {
              data.frame(Result = "No pattern found")
            }
          }, bordered = TRUE)
        }
      }, error = function(e) {
        output$pattern_table <- shiny::renderTable({
          data.frame(Error = paste("Pattern extraction error:", e$message))
        })
      })
    })
    
    # Informazioni funzioni
    output$function_info <- shiny::renderPrint({
      ensure_packages()
      
      cat("FUNZIONI LEGACY DISPONIBILI:\n")
      cat("============================\n\n")
      
      funzioni <- c("normalizzaTesti", "sentiment", "classificaUtenti", 
                   "patternExtract", "urlExtract", "normalizza_emoticon",
                   "normalizzacaratteri", "normalizzahtml", "normalizzapunteggiatura",
                   "normalizzaslang")
      
      for(funz in funzioni) {
        if(exists(funz)) {
          cat("✅", funz, "- DISPONIBILE\n")
        } else {
          cat("❌", funz, "- NON DISPONIBILE (usa fallback)\n")
        }
      }
      
      cat("\nPACCHETTI CARICATI:\n")
      cat("===================\n")
      cat("stringr:", "stringr" %in% loadedNamespaces(), "\n")
      cat("SnowballC:", "SnowballC" %in% loadedNamespaces(), "\n")
      cat("dplyr:", "dplyr" %in% loadedNamespaces(), "\n")
    })
  })
}
