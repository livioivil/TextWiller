# modulo UI ----
similarity_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Semantic similarity between sentences"),
    p("This function calculates semantic similarity between texts using transformer embedding models."),
    
    textAreaInput(ns("texts"), "Enter texts (one per line):",
                  "The weather is beautiful today.\nIt's a sunny and warm day outside.\nDogs are loyal and friendly animals.\nI enjoy reading books in my free time.",
                  rows = 8),
    
    selectInput(ns("model"), "Embedding model:",
                choices = c("sentence-transformers/all-MiniLM-L6-v2" = "sentence-transformers/all-MiniLM-L6-v2",
                            "text-embedding-3-small" = "text-embedding-3-small")),
    
    actionButton(ns("compute"), "Calculate similarity"),
    hr(),
    h4("Similarity matrix:"),
    DT::dataTableOutput(ns("table")),
    br(),
    h4("Similarity heatmap:"),
    plotOutput(ns("heatmap"))
  )
}

# modulo server ----
similarity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$compute, {
      req(input$texts)
      
      texts <- unlist(strsplit(input$texts, "\\n"))
      texts <- trimws(texts)
      texts <- texts[nchar(texts) > 0]
      
      if (length(texts) < 2) {
        showNotification("Please enter at least two sentences!", type = "error")
        return(NULL)
      }
      
      # carica libreria text
      if (!requireNamespace("text", quietly = TRUE)) {
        showNotification("You need to install the 'text' package (install.packages('text'))", type = "error")
        return(NULL)
      }
      
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        showNotification("You need to install the 'ggplot2' package (install.packages('ggplot2'))", type = "error")
        return(NULL)
      }
      
      library(text)
      library(ggplot2)
      showNotification("Calculating embeddings...", type = "message", duration = NULL)
      
      tryCatch({
        # funzione per similarità coseno
        cosine_similarity <- function(mat) {
          # Normalizza i vettori
          norms <- sqrt(rowSums(mat^2))
          # Evita divisione per zero
          norms[norms == 0] <- 1
          mat_norm <- mat / norms
          sim <- mat_norm %*% t(mat_norm)
          return(sim)
        }
        
        # calcola gli embeddings
        embeddings <- text::textEmbed(texts = texts, model = input$model)
        
        # estrazione embeddings in modo robusto
        if ("texts" %in% names(embeddings)) {
          embs <- embeddings$texts$texts
          # Se è una lista con embeddings come elemento separato
          if ("embeddings" %in% names(embs)) {
            embs <- embs$embeddings
          }
        } else if ("word_type_embeddings" %in% names(embeddings)) {
          embs <- embeddings$word_type_embeddings
        } else {
          embs <- embeddings[[1]]
        }
        
        # converti in matrice numerica - gestisci diversi formati
        if (is.list(embs)) {
          embs <- as.matrix(as.data.frame(embs))
        } else if (is.data.frame(embs)) {
          embs <- as.matrix(embs)
        }
        
        # Verifica che la matrice abbia le dimensioni corrette
        if (nrow(embs) != length(texts)) {
          showNotification("Error: number of embeddings doesn't match number of texts", type = "error")
          return(NULL)
        }
        
        # calcola similarità coseno
        sim <- cosine_similarity(embs)
        
        # Assicurati che la matrice di similarità sia quadrata
        if (nrow(sim) != ncol(sim)) {
          showNotification("Error: similarity matrix is not square", type = "error")
          return(NULL)
        }
        
        # Crea nomi che corrispondono esattamente alle dimensioni
        text_names <- paste0("T", seq_len(nrow(sim)))
        
        # crea tabella leggibile
        tab <- round(sim, 3)
        colnames(tab) <- text_names
        rownames(tab) <- text_names
        
        output$table <- DT::renderDataTable({
          DT::datatable(tab, 
                       options = list(
                         pageLength = 10,
                         dom = 't',
                         scrollX = TRUE
                       ),
                       class = 'cell-border stripe')
        })
        
        # Crea heatmap
        output$heatmap <- renderPlot({
          # Prepara i dati per ggplot
          sim_df <- as.data.frame(as.table(sim))
          colnames(sim_df) <- c("Text1", "Text2", "Similarity")
          
          # Aggiungi le etichette complete dei testi
          sim_df$Text1_Label <- paste0("T", sim_df$Text1, ": ", substr(texts[as.numeric(sim_df$Text1)], 1, 30))
          sim_df$Text2_Label <- paste0("T", sim_df$Text2, ": ", substr(texts[as.numeric(sim_df$Text2)], 1, 30))
          
          ggplot(sim_df, aes(x = Text2_Label, y = Text1_Label, fill = Similarity)) +
            geom_tile(color = "white") +
            geom_text(aes(label = round(Similarity, 2)), color = "black", size = 4) +
            scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                               midpoint = 0.5, limits = c(0, 1),
                               name = "Similarity") +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold")
            ) +
            labs(title = "Semantic Similarity Heatmap") +
            coord_fixed()
        })
        
        showNotification("Calculation completed!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
}
