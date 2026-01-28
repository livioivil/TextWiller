# Corpus Import Module UI
mod_corpus_io_ui <- function(id) {
  ns <- shiny::NS(id)
  current_language <- tryCatch({
    TextWiller3::get_analysis_language()
  }, error = function(e) "it")
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Corpus Import & Management")),
      shiny::column(4, align = "right",
        shiny::actionButton(
          ns("calculate"),
          "Calculate",
          class = "btn-success"
        )
      )
    ),
    
    # File Import Section
    shiny::wellPanel(
      shiny::h4("Import Documents"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(ns("import_type"), "Import Type:",
            choices = c(
              "Text Files (.txt)" = "text_files",
              "CSV Files" = "csv_files", 
              "CSV with POS tags (Lemma_CAT, Lemma, CAT)" = "csv_pos",
              "Manual Input" = "manual_input",
              "Demo Data" = "demo_data"
            ),
            selected = "text_files"
          )
        ),
        shiny::column(4,
          shiny::selectInput(
            ns("corpus_language"),
            "Corpus language:",
            choices = c("Italian" = "it", "English" = "en"),
            selected = current_language
          )
        ),
        shiny::column(4,
          shiny::helpText("Selected language guides import, segmentation and resources.")
        )
      ),
      shiny::fluidRow(
        shiny::column(12,
          shiny::uiOutput(ns("import_ui"))
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Document Granularity"),
      shiny::fluidRow(
        shiny::column(6,
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
        shiny::column(6,
          shiny::numericInput(ns("chunk_size"), "Chunk size:", value = 100, min = 10, max = 1000, step = 10)
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'word_chunks'", ns("segmentation_mode")),
        shiny::helpText("Create new documents from consecutive blocks of N words.")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'char_chunks'", ns("segmentation_mode")),
        shiny::helpText("Create new documents from consecutive blocks of N characters.")
      ),
      shiny::actionButton(ns("apply_segmentation"), "Apply granularity settings", class = "btn-primary"),
      shiny::helpText("Segmentation applies to currently loaded corpus and uses language selected above.")
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
    lang_cfg <- tryCatch({
      TextWiller3::get_language_config()
    }, error = function(e) NULL)
    
    calc_trigger <- shiny::reactiveVal(Sys.time())
    safe_log_action <- function(operation, parameters = list(), input_state = NULL, output_state = NULL) {
      if (!requireNamespace("TextWiller3", quietly = TRUE)) return(invisible(NULL))
      try(
        TextWiller3::log_reproducibility_action(
          module = "corpus_io",
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
    
    shiny::observeEvent(input$corpus_language, {
      shiny::req(input$corpus_language)
      if (!is.null(lang_cfg)) {
        tryCatch({
          TextWiller3::set_analysis_language(input$corpus_language)
          shiny::showNotification(
            paste("Corpus language set to", ifelse(input$corpus_language == "it", "Italian", "English")),
            type = "message"
          )
        }, error = function(e) {
          # Fallback if TextWiller3 not available
          if (!is.null(lang_cfg) && exists("set_language", envir = lang_cfg)) {
            lang_cfg$set_language(input$corpus_language)
          }
        })
      }
    }, ignoreNULL = TRUE)
    
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
          tryCatch({
            sentences <- tokenizers::tokenize_sentences(doc_text, simplify = FALSE, 
                                                       lowercase = FALSE, language = language)[[1]]
            sentences <- sentences[nchar(trimws(sentences)) > 0]
            if (length(sentences) == 0) next
            ids <- paste0(base_id, "_s", seq_along(sentences))
            new_texts <- c(new_texts, sentences)
            new_ids <- c(new_ids, ids)
            new_sources <- c(new_sources, rep(paste0(base_source, "_sent"), length(sentences)))
          }, error = function(e) {
            # Fallback: simple sentence splitting
            sentences <- unlist(strsplit(doc_text, "[.!?]+"))
            sentences <- sentences[nchar(trimws(sentences)) > 0]
            if (length(sentences) > 0) {
              ids <- paste0(base_id, "_s", seq_along(sentences))
              new_texts <- c(new_texts, sentences)
              new_ids <- c(new_ids, ids)
              new_sources <- c(new_sources, rep(paste0(base_source, "_sent"), length(sentences)))
            }
          })
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
      language <- input$corpus_language
      
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
        safe_log_action(
          operation = "apply_segmentation",
          parameters = list(mode = mode, chunk_size = chunk_size, language = language),
          input_state = list(original_docs = nrow(data)),
          output_state = list(segmented_docs = nrow(segmented))
        )
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
              accept = c(".csv", ".tsv"),
              buttonLabel = "Browse..."
            ),
            shiny::textInput(ns("text_column"), "Text column name:", value = "text"),
            shiny::textInput(ns("id_column"), "ID column name (optional):", value = ""),
            shiny::numericInput(ns("skip_rows"), "Skip rows:", value = 0, min = 0)
          )
        },
        
        "csv_pos" = {
          shiny::tagList(
            shiny::fileInput(ns("csv_pos_file"), "Select CSV file with POS tags:",
              multiple = FALSE,
              accept = c(".csv"),
              buttonLabel = "Browse..."
            ),
            shiny::helpText("Expected columns: Lemma_CAT, Lemma, CAT (or similar)"),
            shiny::checkboxInput(ns("register_as_lexicon"), "Register as lexicon resource", value = TRUE),
            shiny::textInput(ns("pos_resource_name"), "Resource name:", value = "pos_lexicon"),
            shiny::selectInput(ns("pos_language"), "Language for lexicon:", 
                             choices = c("Italian" = "it", "English" = "en"),
                             selected = input$corpus_language %||% "it")
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
                "Twitter Italian Sample" = "twitter_ita",
                "English News Sample" = "en_news"
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
        safe_log_action(
          operation = "import_text_files",
          parameters = list(n_files = nrow(input$text_files), encoding = input$file_encoding),
          input_state = NULL,
          output_state = list(imported_docs = nrow(new_data), total_docs = nrow(updated_data))
        )
        
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
        safe_log_action(
          operation = "import_csv",
          parameters = list(n_files = nrow(input$csv_files), text_column = input$text_column, id_column = input$id_column),
          input_state = list(skip_rows = input$skip_rows),
          output_state = list(imported_docs = nrow(new_data), total_docs = nrow(updated_data))
        )
        
      }, error = function(e) {
        shiny::showNotification(paste("Error importing CSV data:", e$message), type = "error")
        NULL
      })
    })
    
    # Handle POS-tagged CSV imports
    shiny::observeEvent(input$csv_pos_file, {
      shiny::req(input$csv_pos_file)
      
      shiny::showNotification("Importing POS-tagged lexicon...", type = "message")
      
      tryCatch({
        # Read the CSV
        data <- read.csv(input$csv_pos_file$datapath, stringsAsFactors = FALSE)
        
        # Check for required columns
        if (nrow(data) == 0) {
          shiny::showNotification("CSV file is empty", type = "error")
          return()
        }
        
        # Try to identify columns
        col_names <- tolower(names(data))
        lemma_cat_col <- which(grepl("lemma_cat|lemma.cat|lemma_cat", col_names))[1]
        lemma_col <- which(grepl("^lemma$|lemma_", col_names))[1]
        cat_col <- which(grepl("^cat$|pos|tag", col_names))[1]
        
        if (is.na(lemma_col)) {
          shiny::showNotification("Cannot find 'Lemma' column in CSV", type = "error")
          return()
        }
        
        # Extract lemmas
        lemmas <- data[[lemma_col]]
        lemmas <- lemmas[!is.na(lemmas) & nchar(lemmas) > 0]
        
        if (length(lemmas) == 0) {
          shiny::showNotification("No valid lemmas found in file", type = "error")
          return()
        }
        
        # Register as lexicon resource if requested
        if (isTRUE(input$register_as_lexicon)) {
          resource_name <- input$pos_resource_name
          if (nchar(resource_name) == 0) {
            resource_name <- tools::file_path_sans_ext(input$csv_pos_file$name)
          }
          
          language <- input$pos_language %||% input$corpus_language %||% "it"
          
          tryCatch({
            TextWiller3::register_language_resource(
              language = language,
              name = resource_name,
              content = unique(lemmas),
              type = "lexicon"
            )
            shiny::showNotification(
              sprintf("Registered %d lemmas as '%s' lexicon for %s", 
                     length(unique(lemmas)), resource_name, 
                     ifelse(language == "it", "Italian", "English")),
              type = "message"
            )
          }, error = function(e) {
            shiny::showNotification(paste("Error registering lexicon:", e$message), type = "warning")
          })
        }
        
        # Also add to corpus if there's text content
        text_cols <- names(data)[sapply(data, is.character)]
        text_cols <- text_cols[!tolower(text_cols) %in% c("lemma_cat", "lemma", "cat", "pos", "tag")]
        
        if (length(text_cols) > 0) {
          # Use first text column found
          text_col <- text_cols[1]
          texts <- data[[text_col]]
          texts <- texts[!is.na(texts) & nchar(texts) > 0]
          
          if (length(texts) > 0) {
            new_data <- data.frame(
              text = texts,
              doc_id = paste0("pos_lex_", seq_along(texts)),
              source = "pos_lexicon_csv",
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
            shiny::showNotification(sprintf("Added %d text entries from POS lexicon", length(texts)), 
                            type = "message")
          }
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Error importing POS-tagged CSV:", e$message), type = "error")
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
        safe_log_action(
          operation = "add_manual_documents",
          parameters = list(source = input$manual_source),
          input_state = list(lines = length(texts)),
          output_state = list(total_docs = nrow(updated_data))
        )
      }
    })
    
    # Handle demo data
    shiny::observeEvent(input$load_demo_btn, {
      shiny::req(input$demo_dataset)
      
      demo_data <- switch(input$demo_dataset,
        "ita_news" = {
          data.frame(
            text = c(
              "The Italian government announced new economic measures to support businesses.",
              "The soccer team won the national championship after an excellent season.",
              "Scientific research shows major advances in regenerative medicine.",
              "The film festival attracts thousands of visitors from around the world.",
              "New technologies are revolutionizing how we work and communicate."
            ),
            doc_id = c("news_1", "news_2", "news_3", "news_4", "news_5"),
            source = "demo_ita_news",
            stringsAsFactors = FALSE
          )
        },
        "twitter_ita" = {
          data.frame(
            text = c(
              "What a beautiful day! The sun is shining and the mood is sky-high! #sun #summer",
              "Can't wait for the holidays! Finally some well-deserved rest 🌴",
              "Great match from the team! Fantastic players and an excellent result! #go",
              "Pizza with friends, perfect evening! 🍕 #friendship #nightout",
              "Intensive study for tomorrow's exam. Fingers crossed! #university #study"
            ),
            doc_id = c("tweet_1", "tweet_2", "tweet_3", "tweet_4", "tweet_5"),
            source = "demo_twitter_ita", 
            stringsAsFactors = FALSE
          )
        },
        "en_news" = {
          data.frame(
            text = c(
              "The government announced new economic measures to support businesses.",
              "The football team won the national championship after an excellent season.",
              "Scientific research shows important progress in regenerative medicine.",
              "The film festival attracts thousands of visitors from around the world.",
              "New technologies are revolutionizing the way we work and communicate."
            ),
            doc_id = c("en_news_1", "en_news_2", "en_news_3", "en_news_4", "en_news_5"),
            source = "demo_en_news",
            stringsAsFactors = FALSE
          )
        }
      )
      
      corpus_data(demo_data)
      shiny::showNotification("Demo data loaded successfully", type = "message")
      safe_log_action(
        operation = "load_demo_data",
        parameters = list(dataset = input$demo_dataset),
        input_state = NULL,
        output_state = list(docs = nrow(demo_data))
      )
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
      safe_log_action(
        operation = "clear_corpus",
        parameters = list(trigger = "clear_btn"),
        input_state = NULL,
        output_state = list(docs = 0)
      )
    })
    
    # Corpus preview
    output$preview_table <- DT::renderDataTable({
      calc_trigger()
      shiny::req(corpus_data())
      
      preview_data <- switch(input$preview_type,
        "head" = head(corpus_data(), input$preview_rows),
        "tail" = tail(corpus_data(), input$preview_rows),
        "sample" = {
          data <- corpus_data()
          if (nrow(data) > 0) {
            data[sample(min(nrow(data), input$preview_rows)), ]
          } else {
            data
          }
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
      calc_trigger()
      data <- corpus_data()
      lang_label <- if (!is.null(input$corpus_language) && input$corpus_language == "en") "English" else "Italian"
      if(nrow(data) == 0) {
        cat("No corpus data loaded.\n")
      } else {
        # Safe stats calculation
        word_counts <- sapply(strsplit(data$text, "\\s+"), length)
        total_words <- sum(word_counts)
        avg_words <- if (length(word_counts) > 0) round(mean(word_counts), 1) else 0
        
        cat("Corpus Summary:\n")
        cat("Total documents:", nrow(data), "\n")
        cat("Sources:", paste(unique(data$source), collapse = ", "), "\n")
        cat("Corpus language:", lang_label, "\n")
        cat("Total words:", total_words, "\n")
        cat("Average document length:", avg_words, "words\n")
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
