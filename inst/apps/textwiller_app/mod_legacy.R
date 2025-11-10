### FILE: mod_legacy.R

# Modulo per le funzioni Legacy di TextWiller

#helper
get_example_text <- function() {
  return("Ciao!!! Come stai??? È un'ottima giornata!!! #bellissimo 😊 
Visita il sito: http://example.com e seguimi @mionome")
}

mod_legacy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("TextWiller Legacy Functions - Funzioni Originali"),
    shiny::p("Placeholder per le funzioni originali di TextWiller per l'analisi del testo italiano"),
    
    shiny::wellPanel(
      shiny::h4("📝 Normalizzazione Testo"),
      shiny::textAreaInput(ns("legacy_text"), "Testo da normalizzare:",
        value = "Ciao!!! Come stai??? È un'ottima giornata!!! #bellissimo",
        rows = 4,
        placeholder = "Inserisci il testo da normalizzare..."
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::actionButton(ns("normalizza_btn"), "Normalizza Testo", class = "btn-primary")),
        shiny::column(3, shiny::actionButton(ns("emoticon_btn"), "Normalizza Emoticon", class = "btn-info")),
        shiny::column(3, shiny::actionButton(ns("html_btn"), "Normalizza HTML", class = "btn-info")),
        shiny::column(3, shiny::actionButton(ns("slang_btn"), "Normalizza Slang", class = "btn-info"))
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("😊 Analisi Sentiment"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(ns("sentiment_algo"), "Algoritmo:",
            choices = c("Mattivio" = "Mattivio", "Maddalena" = "Maddalena"),
            selected = "Mattivio")
        ),
        shiny::column(6,
          shiny::checkboxInput(ns("sentiment_normalize"), "Normalizza testo", value = TRUE)
        )
      ),
      shiny::actionButton(ns("sentiment_btn"), "Analizza Sentiment", class = "btn-success")
    ),
    
    shiny::wellPanel(
      shiny::h4("👥 Classificazione Utenti"),
      shiny::textInput(ns("nomi_input"), "Nomi da classificare (separati da virgola):",
        value = "marco,maria,alessandro,alessandra,luca"),
      shiny::actionButton(ns("classifica_btn"), "Classifica Genere", class = "btn-warning")
    ),
    
    shiny::wellPanel(
      shiny::h4("🔗 Estrazione Pattern"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("pattern_input"), "Pattern Regex:", value = "@\\w+")
        ),
        shiny::column(6,
          shiny::selectInput(ns("pattern_type"), "Tipo Pattern:",
            choices = c("Menzioni @" = "@\\w+", "Hashtag #" = "#\\w+", "URL" = "http[^\\s]+"),
            selected = "@\\w+")
        )
      ),
      shiny::actionButton(ns("pattern_btn"), "Estrai Pattern", class = "btn-info")
    ),
    
    shiny::hr(),
    
    # Risultati
    shiny::wellPanel(
      shiny::h4("📊 Risultati"),
      shiny::tabsetPanel(
        id = ns("results_tabs"),
        shiny::tabPanel("Testo Normalizzato", shiny::verbatimTextOutput(ns("normalized_output"))),
        shiny::tabPanel("Sentiment", 
          shiny::tableOutput(ns("sentiment_table")),
          shiny::plotOutput(ns("sentiment_plot"))
        ),
        shiny::tabPanel("Classificazione", shiny::tableOutput(ns("classification_table"))),
        shiny::tabPanel("Pattern", shiny::tableOutput(ns("pattern_table"))),
        shiny::tabPanel("Dettagli Funzioni", shiny::verbatimTextOutput(ns("function_info")))
      )
    ),
    
    # Informazioni sulle funzioni legacy
    shiny::wellPanel(
      shiny::h4("ℹ️ Informazioni Funzioni Legacy"),
      shiny::HTML("
        <p><strong>TextWiller Legacy</strong> preserva tutte le funzioni originali del pacchetto storico per l'analisi del testo italiano:</p>
        <ul>
          <li><strong>Normalizzazione avanzata</strong> per testo italiano (slang, emoticon, punteggiatura)</li>
          <li><strong>Sentiment analysis</strong> specifica per la lingua italiana</li>
          <li><strong>Classificazione nomi italiani</strong> per genere</li>
          <li><strong>Estrazione pattern</strong> (URL, menzioni, hashtag)</li>
          <li><strong>Dizionari specializzati</strong> per l'italiano</li>
          <li><strong>Ecc Ecc</strong> per l'italiano</li>
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
          paste("Installando pacchetti mancanti:", paste(missing_packages, collapse = ", ")),
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
            cat("TESTO ORIGINALE:\n")
            cat(input$legacy_text, "\n\n")
            cat("TESTO NORMALIZZATO:\n")
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
            cat("TESTO ORIGINALE:\n")
            cat(input$legacy_text, "\n\n")
            cat("TESTO NORMALIZZATO (FALLBACK):\n")
            cat(testo, "\n\n")
            cat("⚠️ Funzione normalizzaTesti non disponibile - usando fallback\n")
          })
        }
      }, error = function(e) {
        output$normalized_output <- shiny::renderPrint({
          cat("Errore nella normalizzazione:", e$message, "\n")
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
              Testo = substr(input$legacy_text, 1, 50),
              Punteggio = scores,
              Sentiment = ifelse(scores > 0, "POSITIVO", 
                               ifelse(scores < 0, "NEGATIVO", "NEUTRO")),
              Algoritmo = input$sentiment_algo
            )
          }, bordered = TRUE)
          
          # Plot sentiment semplificato
          output$sentiment_plot <- shiny::renderPlot({
            sentiment_val <- ifelse(scores > 0, "Positivo", 
                                  ifelse(scores < 0, "Negativo", "Neutro"))
            
            plot_data <- data.frame(
              Sentiment = factor(sentiment_val, levels = c("Negativo", "Neutro", "Positivo")),
              Value = 1
            )
            
            ggplot2::ggplot(plot_data, ggplot2::aes(x = Sentiment, fill = Sentiment)) +
              ggplot2::geom_bar(alpha = 0.8) +
              ggplot2::scale_fill_manual(values = c("Negativo" = "red", "Neutro" = "gray", "Positivo" = "green")) +
              ggplot2::labs(title = "Risultato Sentiment Analysis") +
              ggplot2::theme_minimal()
          })
          
        } else {
          # Fallback per sentiment
          output$sentiment_table <- shiny::renderTable({
            data.frame(
              Messaggio = "Funzione sentiment non disponibile - usando fallback",
              Punteggio = 0,
              Sentiment = "NEUTRO"
            )
          }, bordered = TRUE)
        }
        
      }, error = function(e) {
        output$sentiment_table <- shiny::renderTable({
          data.frame(Errore = paste("Errore sentiment analysis:", e$message))
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
              Nome = nomi,
              Genere = classificazione,
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
              if (grepl("a$", nome_lower)) "femm"
              else if (grepl("o$|e$|i$", nome_lower)) "masc"
              else "unknown"
            })
          }
          
          classificazione <- classifica_basica(nomi)
          
          output$classification_table <- shiny::renderTable({
            data.frame(
              Nome = nomi,
              Genere = classificazione,
              Note = "⚠️ Classificazione basica (fallback)",
              stringsAsFactors = FALSE
            )
          }, bordered = TRUE)
        }
      }, error = function(e) {
        output$classification_table <- shiny::renderTable({
          data.frame(Errore = paste("Errore classificazione:", e$message))
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
              data.frame(Risultato = "Nessun pattern trovato")
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
                Pattern_Trovati = matches,
                Note = "⚠️ Estrazione con fallback (stringr)"
              )
            } else {
              data.frame(Risultato = "Nessun pattern trovato")
            }
          }, bordered = TRUE)
        }
      }, error = function(e) {
        output$pattern_table <- shiny::renderTable({
          data.frame(Errore = paste("Errore estrazione pattern:", e$message))
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