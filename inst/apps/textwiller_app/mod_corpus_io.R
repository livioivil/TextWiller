# Corpus Import Module UI
mod_corpus_io_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Corpus Import & Management"),
    
    # File Import Section
    shiny::wellPanel(
      shiny::h4("Import Documents"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("import_type"), "Import Type:",
            choices = c(
              "Text Files (.txt)" = "text_files",
              "CSV Files" = "csv_files", 
              "Manual Input" = "manual_input",
              "Demo Data" = "demo_data"
            ),
            selected = "text_files"
          )
        ),
        shiny::column(8,
          shiny::uiOutput(ns("import_ui"))
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Document Granularity"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("segmentation_mode"), "Segment documents as:",
            choices = c(
              "Whole documents" = "document",
              "Sentences (sentencizer)" = "sentences",
              "Word n-gram chunks" = "word_chunks",
              "Character chunks" = "char_chunks"
            ),
            selected = "document"
          )
        ),
        shiny::column(4,
          shiny::selectInput(ns("sentence_language"), "Sentence language:",
            choices = c("Italiano" = "it", "English" = "en"),
            selected = "it")
        ),
        shiny::column(4,
          shiny::numericInput(ns("chunk_size"), "Chunk size:", value = 100, min = 10, max = 1000, step = 10)
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'word_chunks'", ns("segmentation_mode")),
        shiny::helpText("Crea nuovi documenti da blocchi di N parole consecutivamente.")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'char_chunks'", ns("segmentation_mode")),
        shiny::helpText("Crea nuovi documenti da blocchi di N caratteri consecutivi.")
      ),
      shiny::actionButton(ns("apply_segmentation"), "Apply granularity settings", class = "btn-primary"),
      shiny::helpText("La segmentazione viene applicata al corpus attualmente caricato.")
    ),
    
    # Corpus Management Section
    shiny::wellPanel(
      shiny::h4("Corpus Management"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::actionButton(ns("clear_btn"), "Clear Corpus", class = "btn-warning"),
          shiny::actionButton(ns("save_btn"), "Save Corpus", class = "btn-info")
        ),
        shiny::column(6,
          shiny::downloadButton(ns("download_btn"), "Export Corpus")
        )
      )
    ),
    
    # Corpus Preview Section
    shiny::wellPanel(
      shiny::h4("Corpus Preview"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::numericInput(ns("preview_rows"), "Preview rows:", 
            value = 10, min = 5, max = 50, step = 5)
        ),
        shiny::column(6,
          shiny::selectInput(ns("preview_type"), "Preview Type:",
            choices = c("Head" = "head", "Tail" = "tail", "Sample" = "sample"),
            selected = "head"
          )
        )
      ),
      DT::dataTableOutput(ns("preview_table")),
      shiny::verbatimTextOutput(ns("corpus_info"))
    )
  )
}

# Corpus Import Module Server
mod_corpus_io_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive value for corpus data
    corpus_data <- shiny::reactiveVal(data.frame(
      text = character(0),
      doc_id = character(0),
      source = character(0),
      stringsAsFactors = FALSE
    ))
    
    segment_corpus_data <- function(data, mode, language = "it", chunk_size = 100) {
      if (mode == "document" || nrow(data) == 0) {
        return(data)
      }
      
      chunk_size <- max(1, chunk_size)
      new_texts <- character()
      new_ids <- character()
      new_sources <- character()
      
      for (i in seq_len(nrow(data))) {
        doc_text <- data$text[i]
        base_id <- data$doc_id[i]
        base_source <- data$source[i]
        
        if (mode == "sentences") {
          sentences <- tokenizers::tokenize_sentences(doc_text, simplify = FALSE, lowercase = FALSE, language = language)[[1]]
          sentences <- sentences[nchar(trimws(sentences)) > 0]
          if (length(sentences) == 0) next
          ids <- paste0(base_id, "_s", seq_along(sentences))
          new_texts <- c(new_texts, sentences)
          new_ids <- c(new_ids, ids)
          new_sources <- c(new_sources, rep(paste0(base_source, "_sent"), length(sentences)))
        } else if (mode == "word_chunks") {
          words <- unlist(strsplit(doc_text, "\\s+"))
          words <- words[nchar(words) > 0]
          if (length(words) == 0) next
          groups <- split(words, ceiling(seq_along(words) / chunk_size))
          chunks <- vapply(groups, function(w) paste(w, collapse = " "), character(1))
          ids <- paste0(base_id, "_w", seq_along(chunks))
          new_texts <- c(new_texts, chunks)
          new_ids <- c(new_ids, ids)
          new_sources <- c(new_sources, rep(paste0(base_source, "_wchunk"), length(chunks)))
        } else if (mode == "char_chunks") {
          clean_text <- gsub("\\s+", " ", doc_text)
          if (nchar(clean_text) == 0) next
          starts <- seq(1, nchar(clean_text), by = chunk_size)
          chunks <- substring(clean_text, starts, pmin(starts + chunk_size - 1, nchar(clean_text)))
          ids <- paste0(base_id, "_c", seq_along(chunks))
          new_texts <- c(new_texts, chunks)
          new_ids <- c(new_ids, ids)
          new_sources <- c(new_sources, rep(paste0(base_source, "_cchunk"), length(chunks)))
        }
      }
      
      if (length(new_texts) == 0) {
        return(data)
      }
      
      data.frame(
        text = new_texts,
        doc_id = new_ids,
        source = new_sources,
        stringsAsFactors = FALSE
      )
    }
    
    shiny::observeEvent(input$apply_segmentation, {
      data <- corpus_data()
      if (nrow(data) == 0) {
        shiny::showNotification("Load a corpus before applying segmentation.", type = "warning")
        return()
      }
      mode <- input$segmentation_mode
      chunk_size <- input$chunk_size
      language <- input$sentence_language
      
      segmented <- tryCatch({
        segment_corpus_data(data, mode, language = language, chunk_size = chunk_size)
      }, error = function(e) {
        shiny::showNotification(paste("Segmentation error:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(segmented)) {
        corpus_data(segmented)
        if (mode == "document") {
          shiny::showNotification("Corpus left unchanged (whole documents).", type = "message")
        } else {
          shiny::showNotification(
            sprintf("Segmentation applied: %d fragments generated.", nrow(segmented)),
            type = "message"
          )
        }
      }
    })
    
    # Dynamic UI based on import type
    output$import_ui <- shiny::renderUI({
      ns <- session$ns
      
      switch(input$import_type,
        "text_files" = {
          shiny::tagList(
            shiny::fileInput(ns("text_files"), "Select text files:",
              multiple = TRUE,
              accept = c(".txt"),
              buttonLabel = "Browse...",
              placeholder = "No files selected"
            ),
            shiny::textInput(ns("file_encoding"), "File encoding:", value = "UTF-8")
          )
        },
        
        "csv_files" = {
          shiny::tagList(
            shiny::fileInput(ns("csv_files"), "Select CSV files:",
              multiple = TRUE,
              accept = c(".csv"),
              buttonLabel = "Browse..."
            ),
            shiny::textInput(ns("text_column"), "Text column name:", value = "text"),
            shiny::textInput(ns("id_column"), "ID column name (optional):", value = ""),
            shiny::numericInput(ns("skip_rows"), "Skip rows:", value = 0, min = 0)
          )
        },
        
        "manual_input" = {
          shiny::tagList(
            shiny::textAreaInput(ns("manual_text"), "Enter texts (one per line):",
              rows = 6,
              placeholder = "Enter your texts here, one per line..."
            ),
            shiny::textInput(ns("manual_source"), "Source name:", value = "manual_input"),
            shiny::actionButton(ns("add_manual_btn"), "Add to Corpus", class = "btn-primary")
          )
        },
        
        "demo_data" = {
          shiny::tagList(
            shiny::selectInput(ns("demo_dataset"), "Demo dataset:",
              choices = c(
                "Italian News Sample" = "ita_news",
                "Twitter Italian Sample" = "twitter_ita"
              )
            ),
            shiny::actionButton(ns("load_demo_btn"), "Load Demo Data", class = "btn-primary")
          )
        }
      )
    })
    
    # Handle text file imports
    shiny::observeEvent(input$text_files, {
      shiny::req(input$text_files)
      
      shiny::showNotification("Importing text files...", type = "message")
      
      imported_data <- tryCatch({
        texts <- character(0)
        doc_ids <- character(0)
        sources <- character(0)
        
        for(i in 1:nrow(input$text_files)) {
          file_info <- input$text_files[i, ]
          
          content <- readLines(file_info$datapath, 
                             encoding = input$file_encoding, 
                             warn = FALSE)
          
          text_content <- paste(content, collapse = "\n")
          
          texts <- c(texts, text_content)
          doc_ids <- c(doc_ids, tools::file_path_sans_ext(file_info$name))
          sources <- c(sources, "text_file")
        }
        
        new_data <- data.frame(
          text = texts,
          doc_id = doc_ids,
          source = sources,
          stringsAsFactors = FALSE
        )
        
        # Merge with existing corpus
        current_data <- corpus_data()
        updated_data <- if(nrow(current_data) == 0) {
          new_data
        } else {
          rbind(current_data, new_data)
        }
        
        corpus_data(updated_data)
        shiny::showNotification(sprintf("Successfully imported %d documents", nrow(new_data)), 
                        type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error importing files:", e$message), type = "error")
        NULL
      })
    })
    
    # Handle CSV file imports
    shiny::observeEvent(input$csv_files, {
      shiny::req(input$csv_files, input$text_column)
      
      shiny::showNotification("Importing CSV data...", type = "message")
      
      imported_data <- tryCatch({
        all_texts <- character(0)
        all_doc_ids <- character(0)
        all_sources <- character(0)
        
        for(i in 1:nrow(input$csv_files)) {
          file_info <- input$csv_files[i, ]
          
          data <- read.csv(file_info$datapath,
                         skip = input$skip_rows,
                         stringsAsFactors = FALSE)
          
          # Extract text column
          if(input$text_column %in% names(data)) {
            texts <- data[[input$text_column]]
            texts <- texts[!is.na(texts) & nchar(texts) > 0]
            
            # Extract IDs
            if(input$id_column != "" && input$id_column %in% names(data)) {
              doc_ids <- as.character(data[[input$id_column]][1:length(texts)])
            } else {
              doc_ids <- paste0(tools::file_path_sans_ext(file_info$name), 
                               "_", seq_along(texts))
            }
            
            all_texts <- c(all_texts, texts)
            all_doc_ids <- c(all_doc_ids, doc_ids)
            all_sources <- c(all_sources, rep("csv_file", length(texts)))
          }
        }
        
        new_data <- data.frame(
          text = all_texts,
          doc_id = all_doc_ids,
          source = all_sources,
          stringsAsFactors = FALSE
        )
        
        # Merge with existing corpus
        current_data <- corpus_data()
        updated_data <- if(nrow(current_data) == 0) {
          new_data
        } else {
          rbind(current_data, new_data)
        }
        
        corpus_data(updated_data)
        shiny::showNotification(sprintf("Successfully imported %d documents", nrow(new_data)), 
                        type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error importing CSV data:", e$message), type = "error")
        NULL
      })
    })
    
    # Handle manual input
    shiny::observeEvent(input$add_manual_btn, {
      shiny::req(input$manual_text)
      
      texts <- strsplit(input$manual_text, "\n")[[1]]
      texts <- texts[nchar(texts) > 0]  # Remove empty lines
      
      if(length(texts) > 0) {
        new_data <- data.frame(
          text = texts,
          doc_id = paste0(input$manual_source, "_", seq_along(texts)),
          source = "manual_input",
          stringsAsFactors = FALSE
        )
        
        # Merge with existing corpus
        current_data <- corpus_data()
        updated_data <- if(nrow(current_data) == 0) {
          new_data
        } else {
          rbind(current_data, new_data)
        }
        
        corpus_data(updated_data)
        
        # Clear manual input
        shiny::updateTextAreaInput(session, "manual_text", value = "")
        
        shiny::showNotification(sprintf("Added %d manual documents", length(texts)), 
                        type = "message")
      }
    })
    
    # Handle demo data
    shiny::observeEvent(input$load_demo_btn, {
      shiny::req(input$demo_dataset)
      
      demo_data <- switch(input$demo_dataset,
        "ita_news" = {
          data.frame(
            text = c(
              "Il governo italiano ha annunciato nuove misure economiche per sostenere le imprese.",
              "La squadra di calcio ha vinto il campionato nazionale dopo una stagione eccellente.",
              "La ricerca scientifica mostra importanti progressi nella medicina rigenerativa.",
              "Il festival del cinema attira migliaia di visitatori da tutto il mondo.",
              "Le nuove tecnologie stanno rivoluzionando il modo in cui lavoriamo e comunichiamo."
            ),
            doc_id = c("news_1", "news_2", "news_3", "news_4", "news_5"),
            source = "demo_ita_news",
            stringsAsFactors = FALSE
          )
        },
        "twitter_ita" = {
          data.frame(
            text = c(
              "Che bella giornata oggi! Il sole splende e l'umore è alle stelle! #sole #estate",
              "Non vedo l'ora delle vacanze! Finalmente un po' di riposo meritato 🌴",
              "Grande partita della squadra! Giocatori fantastici e risultato eccellente! #forza",
              "Pizza con gli amici, serata perfetta! 🍕 #amicizia #serata",
              "Studio intensivo per l'esame di domani. Incrociamo le dita! #università #studio"
            ),
            doc_id = c("tweet_1", "tweet_2", "tweet_3", "tweet_4", "tweet_5"),
            source = "demo_twitter_ita", 
            stringsAsFactors = FALSE
          )
        }
      )
      
      corpus_data(demo_data)
      shiny::showNotification("Demo data loaded successfully", type = "message")
    })
    
    # Clear corpus
    shiny::observeEvent(input$clear_btn, {
      corpus_data(data.frame(
        text = character(0),
        doc_id = character(0), 
        source = character(0),
        stringsAsFactors = FALSE
      ))
      shiny::showNotification("Corpus cleared", type = "message")
    })
    
    # Corpus preview
    output$preview_table <- DT::renderDataTable({
      shiny::req(corpus_data())
      
      preview_data <- switch(input$preview_type,
        "head" = head(corpus_data(), input$preview_rows),
        "tail" = tail(corpus_data(), input$preview_rows),
        "sample" = {
          data <- corpus_data()
          data[sample(min(nrow(data), input$preview_rows)), ]
        }
      )
      
      # Show only text preview (first 100 chars)
      preview_data$text_preview <- substr(preview_data$text, 1, 100)
      if(any(nchar(preview_data$text) > 100)) {
        preview_data$text_preview <- paste0(preview_data$text_preview, "...")
      }
      
      DT::datatable(
        preview_data[, c("doc_id", "source", "text_preview")],
        options = list(
          pageLength = input$preview_rows,
          scrollX = TRUE,
          dom = 't'
        ),
        colnames = c("Document ID", "Source", "Text Preview"),
        rownames = FALSE
      )
    })
    
    # Corpus info
    output$corpus_info <- shiny::renderPrint({
      data <- corpus_data()
      if(nrow(data) == 0) {
        cat("No corpus data loaded.\n")
      } else {
        stats <- TextWiller3::get_corpus_stats(data$text)
        cat("Corpus Summary:\n")
        cat("Total documents:", nrow(data), "\n")
        cat("Sources:", paste(unique(data$source), collapse = ", "), "\n")
        cat("Total words:", stats$total_words, "\n")
        vocab_size <- if(stats$total_words > 0) {
          nrow(TextWiller3::calculate_word_frequencies_enhanced(data$text))
        } else {
          0
        }
        cat("Vocabulary size:", vocab_size, "\n")
        cat("Average document length:", stats$avg_words, "words\n")
      }
    })
    
    # Export functionality
    output$download_btn <- shiny::downloadHandler(
      filename = function() {
        paste0("textwiller_corpus_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(corpus_data(), file, row.names = FALSE)
      }
    )
    
    # Return corpus data for other modules (just the text vector)
    return(shiny::reactive({
      data <- corpus_data()
      if(nrow(data) > 0) data$text else character(0)
    }))
  })
}
