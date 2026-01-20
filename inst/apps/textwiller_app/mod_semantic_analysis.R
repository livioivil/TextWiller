# Semantic Analysis - CBOW only, fully manual, with logging

mod_semantic_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::h3("Analisi semantica (CBOW / SBERT)")),
      shiny::column(4, align = "right",
        shiny::actionButton(ns("calculate"), "Calculate", class = "btn-success")
      )
    ),
    shiny::wellPanel(
      shiny::h4("Parametri CBOW"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::selectInput(
            ns("engine"),
            "Motore",
            choices = c("CBOW (word2vec)" = "cbow", "SBERT (Hugging Face via {text})" = "sbert"),
            selected = "cbow"
          )
        ),
        shiny::column(8,
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'sbert'", ns("engine")),
            shiny::textInput(
              ns("sbert_model"),
              "Modello SBERT (Hugging Face)",
              value = "sentence-transformers/all-MiniLM-L6-v2",
              placeholder = "es. sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'cbow'", ns("engine")),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("dim"), "Dimensione vettore", 50, min = 10, max = 300, step = 5)),
          shiny::column(3, shiny::numericInput(ns("window"), "Finestra", 5, min = 2, max = 15, step = 1)),
          shiny::column(3, shiny::numericInput(ns("epochs"), "Epochs", 5, min = 1, max = 50, step = 1)),
          shiny::column(3, shiny::numericInput(ns("min_count"), "Frequenza minima", 2, min = 1, max = 50, step = 1))
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'sbert'", ns("engine")),
        shiny::fluidRow(
          shiny::column(4,
            shiny::numericInput(ns("sbert_dim"), "Dimensione target (opzionale)", value = 0, min = 0, max = 768, step = 16)
          ),
          shiny::column(8,
            shiny::helpText("Lascia 0 per usare la dimensione nativa del modello; >0 per troncare alle prime N colonne.")
          )
        )
      ),
      shiny::tags$div(
        style = "font-size: 90%; color: #555; margin-top:8px;",
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("CBOW — Dimensione vettore:"), " aumenta il dettaglio ma richiede più dati; tipico 50-200."),
          shiny::tags$li(shiny::strong("CBOW — Finestra:"), " contesto a sinistra/destra; valori 4-8 per testi generali."),
          shiny::tags$li(shiny::strong("CBOW — Epochs:"), " cicli di training; 5-10 spesso bastano per corpus piccoli."),
          shiny::tags$li(shiny::strong("CBOW — Frequenza minima:"), " ignora parole rare; alza per ridurre rumore, abbassa per tenere termini rari."),
          shiny::tags$li(shiny::strong("SBERT — Modello:"), " nome su Hugging Face; es. all-MiniLM-L6-v2 o paraphrase-multilingual-MiniLM-L12-v2."),
          shiny::tags$li(shiny::strong("SBERT — Dim target:"), " 0 = dimensione nativa; >0 = tronca alle prime N dimensioni.")
        )
      ),
      shiny::actionButton(ns("run_cbow"), "Calcola embeddings", class = "btn-primary btn-block")
    ),
    shiny::wellPanel(
      shiny::h4("Stato"),
      shiny::uiOutput(ns("status"))
    ),
    shiny::wellPanel(
      shiny::h4("Log"),
      shiny::verbatimTextOutput(ns("log"), placeholder = TRUE)
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Proiezione PCA (Dim1-2)"),
          shiny::plotOutput(ns("pca_plot"), height = 320)
        )
      ),
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Snapshot embedding"),
          DT::dataTableOutput(ns("emb_snapshot"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Gestione embeddings"),
          shiny::downloadButton(ns("download_emb"), "Scarica embeddings (RDS)"),
          shiny::fileInput(ns("upload_emb"), "Carica embeddings (RDS)", accept = ".rds"),
          shiny::helpText("Salva/richiama la matrice di embedding (inclusi metadati motore/modello).")
        )
      ),
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Clustering / Distanze"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("k_clusters"), "K cluster (kmeans)", value = 3, min = 2, max = 12, step = 1)),
            shiny::column(6, shiny::actionButton(ns("run_cluster"), "Esegui clustering", class = "btn-info btn-block"))
          ),
          shiny::plotOutput(ns("cluster_plot"), height = 260),
          shiny::plotOutput(ns("distance_heatmap"), height = 260)
        )
      )
    ),
    shiny::wellPanel(
      shiny::h4("Dettaglio embedding documento"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(ns("doc_select"), "Seleziona documento", choices = character(0))
        ),
        shiny::column(6,
          shiny::numericInput(ns("detail_dims"), "Dimensioni da mostrare", value = 20, min = 5, max = 300, step = 5)
        )
      ),
      shiny::verbatimTextOutput(ns("doc_text_preview"), placeholder = TRUE),
      DT::dataTableOutput(ns("emb_detail"))
    )
  )
}

mod_semantic_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang_cfg <- TextWiller3::get_language_config()

    embeddings <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)
    log_lines <- shiny::reactiveVal("[init] Modulo pronto.")
    cluster_res <- shiny::reactiveVal(NULL)

    append_log <- function(msg) {
      ts <- format(Sys.time(), "%H:%M:%S")
      log_lines(paste0(log_lines(), "\n[", ts, "] ", msg))
    }

    safe_log_action <- function(operation, parameters = list(), input_state = NULL, output_state = NULL) {
      log_fun <- NULL
      if (exists("log_reproducibility_action", envir = .GlobalEnv, mode = "function")) {
        log_fun <- get("log_reproducibility_action", envir = .GlobalEnv)
      } else if (requireNamespace("TextWiller3", quietly = TRUE) &&
                 exists("log_reproducibility_action", envir = asNamespace("TextWiller3"), mode = "function")) {
        log_fun <- get("log_reproducibility_action", envir = asNamespace("TextWiller3"))
      }
      if (is.null(log_fun)) return(invisible(NULL))
      try(
        log_fun(
          module = "semantic",
          operation = operation,
          parameters = parameters,
          input_state = input_state,
          output_state = output_state
        ),
        silent = TRUE
      )
    }

    reset_state <- function(reason = NULL) {
      embeddings(NULL)
      cluster_res(NULL)
      running(FALSE)
      if (!is.null(reason)) append_log(reason)
    }

    preprocess_text <- function(text) {
      txt <- tolower(text)
      txt <- gsub("[^[:alnum:]\\s]", " ", txt)
      txt <- gsub("\\s+", " ", txt)
      txt <- trimws(txt)
      txt[nchar(txt) == 0] <- "(vuoto)"
      txt
    }

    ensure_word2vec <- function() {
      if (!requireNamespace("word2vec", quietly = TRUE)) {
        shiny::showNotification("Installa il pacchetto 'word2vec' per il CBOW (install.packages('word2vec')).", type = "error")
        append_log("Errore: pacchetto 'word2vec' non disponibile.")
        return(FALSE)
      }
      TRUE
    }

    ensure_text <- function() {
      if (!requireNamespace("text", quietly = TRUE)) {
        shiny::showNotification("Installa il pacchetto 'text' per SBERT (install.packages('text')).", type = "error")
        append_log("Errore: pacchetto 'text' non disponibile.")
        return(FALSE)
      }
      if (!requireNamespace("torch", quietly = TRUE) || !isTRUE(torch::torch_is_installed())) {
        shiny::showNotification("SBERT richiede torch: install.packages('torch') e poi torch::install_torch().", type = "error")
        append_log("Errore: runtime torch non disponibile per SBERT.")
        return(FALSE)
      }
      TRUE
    }

    extract_sbert_embeddings <- function(embed_res, n_docs) {
      normalize_matrix <- function(x) {
        if (inherits(x, "torch_tensor")) {
          x <- tryCatch(as.array(x), error = function(e) {
            if (requireNamespace("torch", quietly = TRUE)) {
              tryCatch(torch::as_array(x), error = function(e2) x)
            } else {
              x
            }
          })
        }
        if (inherits(x, "python.builtin.object")) {
          # try conversion if reticulate objects sneak in
          x <- tryCatch(reticulate::py_to_r(x), error = function(e) x)
        }
        if (is.matrix(x) && is.numeric(x)) return(x)
        if (is.array(x) && is.numeric(x) && length(dim(x)) >= 2) {
          return(matrix(as.numeric(x), nrow = dim(x)[1], ncol = dim(x)[2]))
        }
        if (is.vector(x) && is.numeric(x)) return(matrix(x, nrow = 1))
        if (is.list(x) && all(vapply(x, is.numeric, logical(1)))) {
          m <- do.call(rbind, lapply(x, as.numeric))
          if (is.matrix(m)) return(m)
        }
        NULL
      }

      try_extract_df_col <- function(df) {
        mats <- list()
        for (nm in names(df)) {
          col <- df[[nm]]
          if (is.list(col)) {
            mats_col <- lapply(col, normalize_matrix)
            mats_col <- mats_col[!vapply(mats_col, is.null, logical(1))]
            mats <- c(mats, mats_col)
          }
        }
        mats
      }

      gather_mats <- function(obj) {
        found <- list()
        if (is.null(obj)) return(found)
        cand <- normalize_matrix(obj)
        if (!is.null(cand) && nrow(cand) > 0) found <- c(found, list(cand))
        if (is.data.frame(obj)) {
          found <- c(found, try_extract_df_col(obj))
        }
        if (is.list(obj)) {
          for (item in obj) {
            found <- c(found, gather_mats(item))
          }
        }
        found
      }

      mats <- gather_mats(embed_res)
      append_log(paste("Slot SBERT disponibili:", paste(names(embed_res), collapse = ", ")))
      if (length(mats) == 0) stop("text::textEmbed non ha restituito embeddings utilizzabili.")

      pick_matrix <- function(ms, n) {
        if (length(ms) == 0) return(NULL)
        if (!is.null(n) && is.numeric(n) && n > 0) {
          exact <- Filter(function(m) nrow(m) == n, ms)
          if (length(exact) > 0) return(exact[[1]])
          trans <- Filter(function(m) ncol(m) == n, ms)
          if (length(trans) > 0) return(t(trans[[1]]))
        }
        ms[[1]]
      }

      emb <- pick_matrix(mats, n_docs)
      if (is.null(emb)) stop("text::textEmbed non ha restituito embeddings utilizzabili.")
      emb
    }

    run_sbert <- function(texts, model_name, target_dim = 0) {
      embed_safe <- function(call_fn) {
        withCallingHandlers(
          call_fn(),
          warning = function(w) {
            append_log(paste("Warning SBERT:", w$message))
            invokeRestart("muffleWarning")
          }
        )
      }

      embed_res <- tryCatch({
        embed_safe(function() text::textEmbed(texts = texts, model = model_name))
      }, error = function(e1) {
        tryCatch({
          embed_safe(function() text::textEmbed(x = texts, model = model_name))
        }, error = function(e2) {
          stop(paste("Impossibile ottenere embeddings SBERT. Tentativi falliti:", e1$message, "|", e2$message))
        })
      })
      emb <- extract_sbert_embeddings(embed_res, n_docs = length(texts))
      if (!is.null(target_dim) && is.numeric(target_dim) && target_dim > 0 && ncol(emb) > target_dim) {
        emb <- emb[, seq_len(target_dim), drop = FALSE]
      }
      emb
    }

    compute_embeddings <- function() {
      engine <- input$engine
      append_log(paste("Richiesta calcolo embeddings con motore:", engine))

      docs <- corpus()
      if (is.null(docs) || length(docs) == 0) {
        shiny::showNotification("Carica un corpus prima di calcolare gli embeddings.", type = "warning")
        append_log("Nessun corpus disponibile.")
        return(NULL)
      }

      if (engine == "cbow" && !ensure_word2vec()) return(NULL)
      if (engine == "sbert" && !ensure_text()) return(NULL)

      dim <- input$dim
      window <- input$window
      epochs <- input$epochs
      min_count <- input$min_count
      sbert_model <- input$sbert_model
      sbert_dim <- input$sbert_dim
      if (engine == "sbert") {
        append_log(paste("Modello SBERT selezionato:", sbert_model))
      }

      running(TRUE)
      shiny::showNotification("Calcolo embeddings...", type = "message", duration = NULL, id = ns("cbow_progress"))
      append_log("Preprocessing testi...")

      res <- tryCatch({
        shiny::withProgress(message = "Calcolo embeddings", value = 0, {
          shiny::incProgress(0.3, detail = "Pulizia testi")
          prep <- preprocess_text(docs)

          if (engine == "cbow") {
            shiny::incProgress(0.6, detail = "Training modello CBOW")
            model <- word2vec::word2vec(
              x = prep,
              dim = dim,
              window = window,
              iter = epochs,
              type = "cbow",
              min_count = min_count,
              threads = 1L
            )

            shiny::incProgress(0.8, detail = "Costruzione embedding doc")
            vocab_matrix <- as.matrix(model)
            doc_tokens <- strsplit(prep, " ")
            doc_mat <- matrix(0, nrow = length(doc_tokens), ncol = ncol(vocab_matrix))
            vocab_terms <- rownames(vocab_matrix)
            if (is.null(vocab_terms)) vocab_terms <- character(0)

            for (i in seq_along(doc_tokens)) {
              toks <- doc_tokens[[i]]
              toks <- toks[toks %in% vocab_terms]
              if (length(toks) > 0) {
                vecs <- vocab_matrix[toks, , drop = FALSE]
                if (is.null(dim(vecs))) {
                  doc_mat[i, ] <- vecs
                } else {
                  doc_mat[i, ] <- colMeans(vecs)
                }
              }
            }

            if (ncol(doc_mat) < dim) {
              doc_mat <- cbind(doc_mat, matrix(0, nrow(doc_mat), dim - ncol(doc_mat)))
            }
            doc_mat <- doc_mat[, seq_len(dim), drop = FALSE]
            embeddings(doc_mat)
          } else if (engine == "sbert") {
            shiny::incProgress(0.75, detail = "Download/caricamento modello SBERT")
            emb <- run_sbert(prep, sbert_model, target_dim = sbert_dim)
            embeddings(emb)
          } else {
            stop("Motore non supportato:", engine)
          }

          shiny::incProgress(0.95, detail = "Finalizzazione")
        })
        append_log("Embeddings calcolati con successo.")
        embeddings()
      }, error = function(e) {
        append_log(paste("Errore embeddings:", e$message))
        shiny::showNotification(paste("Errore embeddings:", e$message), type = "error", id = ns("cbow_progress"))
        reset_state()
        NULL
      })

      shiny::removeNotification(id = ns("cbow_progress"))
      running(FALSE)
      if (!is.null(res)) {
        shiny::showNotification("Embeddings pronti.", type = "message", duration = 4)
        append_log(paste0("Embeddings pronti. Dim matrix: ", paste(dim(embeddings()), collapse = " x ")))
        safe_log_action(
          operation = "compute_embeddings",
          parameters = list(
            engine = input$engine,
            sbert_model = input$sbert_model,
            sbert_dim = input$sbert_dim,
            cbow_dim = input$dim,
            cbow_window = input$window,
            cbow_epochs = input$epochs,
            cbow_min_count = input$min_count
          ),
          input_state = list(n_docs = length(corpus())),
          output_state = list(dim = dim(embeddings()))
        )
        cluster_res(NULL) # reset clustering after new embeddings
      }
      res
    }

    shiny::observeEvent(input$run_cbow, {
      append_log("Pulsante 'Calcola embeddings' premuto.")
      compute_embeddings()
    })

    shiny::observeEvent(input$calculate, {
      append_log("Pulsante 'Calculate' premuto.")
      compute_embeddings()
    })

    shiny::observeEvent(corpus(), {
      reset_state("Corpus cambiato: ricalcola il CBOW.")
    })

    output$status <- shiny::renderUI({
      emb <- embeddings()
      shiny::tagList(
        shiny::p(shiny::strong("Lingua:"), ifelse(lang_cfg$current_language == "it", "Italiano", "English")),
        shiny::p(shiny::strong("Documenti:"), ifelse(is.null(corpus()), 0, length(corpus()))),
        shiny::p(shiny::strong("Motore:"), ifelse(input$engine == "cbow", "CBOW (word2vec)", "SBERT")),
        shiny::p(shiny::strong("Embeddings pronti:"), ifelse(!is.null(emb), "Sì", "No")),
        if (isTRUE(running())) shiny::p(shiny::em("Elaborazione in corso..."))
      )
    })

    output$log <- shiny::renderText({
      log_lines()
    })

    output$download_emb <- shiny::downloadHandler(
      filename = function() {
        paste0("embeddings_", input$engine, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        emb <- embeddings()
        if (is.null(emb)) stop("Nessun embedding da salvare.")
        meta <- list(
          engine = input$engine,
          sbert_model = input$sbert_model,
          sbert_dim = input$sbert_dim,
          cbow_params = list(dim = input$dim, window = input$window, epochs = input$epochs, min_count = input$min_count)
        )
        saveRDS(list(embeddings = emb, meta = meta), file = file)
      }
    )

    shiny::observeEvent(input$upload_emb, {
      req(input$upload_emb$datapath)
      emb_obj <- tryCatch(readRDS(input$upload_emb$datapath), error = function(e) {
        shiny::showNotification(paste("Errore caricamento embeddings:", e$message), type = "error")
        NULL
      })
      if (is.null(emb_obj)) return()
      if (is.list(emb_obj) && "embeddings" %in% names(emb_obj)) {
        embeddings(emb_obj$embeddings)
        cluster_res(NULL)
        append_log(paste0("Embeddings caricati da file. Dim: ", paste(dim(emb_obj$embeddings), collapse = " x ")))
        safe_log_action(
          operation = "upload_embeddings",
          parameters = list(source = input$upload_emb$name %||% "upload_emb"),
          input_state = NULL,
          output_state = list(dim = dim(emb_obj$embeddings))
        )
        if (!is.null(emb_obj$meta$engine)) {
          updateSelectInput(session, "engine", selected = emb_obj$meta$engine)
        }
      } else if (is.matrix(emb_obj)) {
        embeddings(emb_obj)
        cluster_res(NULL)
        append_log(paste0("Embeddings caricati da file (matrice). Dim: ", paste(dim(emb_obj), collapse = " x ")))
        safe_log_action(
          operation = "upload_embeddings",
          parameters = list(source = input$upload_emb$name %||% "upload_emb_matrix"),
          input_state = NULL,
          output_state = list(dim = dim(emb_obj))
        )
      } else {
        shiny::showNotification("Formato embeddings non riconosciuto.", type = "error")
      }
    })

    projection_data <- shiny::reactive({
      emb <- embeddings()
      if (is.null(emb) || nrow(emb) < 2) return(NULL)
      tryCatch({
        stats::prcomp(emb, center = TRUE, scale. = TRUE)$x[, 1:2, drop = FALSE]
      }, error = function(e) NULL)
    })

    shiny::observe({
      emb <- embeddings()
      n_docs <- if (!is.null(emb)) nrow(emb) else 0
      choices <- if (n_docs > 0) {
        setNames(seq_len(n_docs), paste0("Doc ", seq_len(n_docs)))
      } else {
        character(0)
      }
      shiny::updateSelectInput(session, "doc_select", choices = choices, selected = if (n_docs > 0) 1 else character(0))
    })

    output$pca_plot <- shiny::renderPlot({
      proj <- projection_data()
      if (is.null(proj)) return(NULL)
      df <- data.frame(Document = seq_len(nrow(proj)), Dim1 = proj[, 1], Dim2 = proj[, 2])
      if (!is.null(cluster_res())) {
        df$Cluster <- factor(cluster_res())
      }
      ggplot2::ggplot(df, ggplot2::aes(x = Dim1, y = Dim2, color = Cluster)) +
        ggplot2::geom_point(size = 3, alpha = 0.85) +
        ggplot2::geom_text(ggplot2::aes(label = Document), vjust = -0.6, size = 3, color = "black") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Dim 1", y = "Dim 2", color = "Cluster") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)))
    })

    output$emb_snapshot <- DT::renderDataTable({
      emb <- embeddings()
      if (is.null(emb)) return(NULL)
      preview <- as.data.frame(emb[1:min(5, nrow(emb)), 1:min(6, ncol(emb)), drop = FALSE])
      DT::datatable(round(preview, 3), options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
    })

    output$emb_detail <- DT::renderDataTable({
      emb <- embeddings()
      if (is.null(emb)) return(NULL)
      doc_id <- input$doc_select
      if (is.null(doc_id) || !nzchar(doc_id)) return(NULL)
      doc_idx <- suppressWarnings(as.integer(doc_id))
      if (is.na(doc_idx) || doc_idx < 1 || doc_idx > nrow(emb)) return(NULL)
      dims <- input$detail_dims
      if (is.null(dims) || !is.numeric(dims)) dims <- 20
      dims <- max(1, min(ncol(emb), as.integer(dims)))
      df <- data.frame(
        Dimension = paste0("Dim", seq_len(dims)),
        Value = round(emb[doc_idx, seq_len(dims)], 4),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
    })

    output$doc_text_preview <- shiny::renderText({
      docs <- corpus()
      emb <- embeddings()
      if (is.null(docs) || is.null(emb)) return("Calcola il CBOW e seleziona un documento.")
      doc_id <- input$doc_select
      if (is.null(doc_id) || !nzchar(doc_id)) return("Seleziona un documento.")
      doc_idx <- suppressWarnings(as.integer(doc_id))
      if (is.na(doc_idx) || doc_idx < 1 || doc_idx > length(docs)) return("Documento non valido.")
      txt <- docs[doc_idx]
      paste0("Documento ", doc_idx, " (primi 240 caratteri):\n", substr(txt, 1, 240), ifelse(nchar(txt) > 240, "...", ""))
    })

    shiny::observeEvent(input$run_cluster, {
      emb <- embeddings()
      if (is.null(emb) || nrow(emb) < 2) {
        shiny::showNotification("Calcola prima gli embeddings (almeno 2 documenti).", type = "warning")
        return()
      }
      k <- input$k_clusters
      if (is.null(k) || !is.numeric(k) || k < 2) {
        shiny::showNotification("Imposta un numero di cluster valido (>=2).", type = "warning")
        return()
      }
      km <- stats::kmeans(emb, centers = as.integer(k))
      cluster_res(km$cluster)
      append_log(paste("Clustering kmeans completato con k =", k))
      safe_log_action(
        operation = "kmeans_cluster",
        parameters = list(k = k),
        input_state = list(n_docs = nrow(emb)),
        output_state = list(cluster = km$cluster)
      )
    })

    output$cluster_plot <- shiny::renderPlot({
      proj <- projection_data()
      if (is.null(proj)) return(NULL)
      df <- data.frame(Document = seq_len(nrow(proj)), Dim1 = proj[, 1], Dim2 = proj[, 2])
      if (!is.null(cluster_res())) df$Cluster <- factor(cluster_res())
      ggplot2::ggplot(df, ggplot2::aes(x = Dim1, y = Dim2, color = Cluster)) +
        ggplot2::geom_point(size = 3, alpha = 0.9) +
        ggplot2::geom_text(ggplot2::aes(label = Document), vjust = -0.6, size = 3, color = "black") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Dim 1", y = "Dim 2", color = "Cluster") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)))
    })

    output$distance_heatmap <- shiny::renderPlot({
      emb <- embeddings()
      if (is.null(emb) || nrow(emb) < 2) return(NULL)
      dmat <- as.matrix(stats::dist(emb))
      rownames(dmat) <- paste0("Doc_", seq_len(nrow(dmat)))
      colnames(dmat) <- rownames(dmat)
      stats::heatmap(dmat, symm = TRUE, Rowv = NA, Colv = NA, scale = "none", col = heat.colors(50), margins = c(6, 6))
    })
  })
}
