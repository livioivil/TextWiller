# Semantic Studio Module
mod_semantic_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Semantic Studio"),
    
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Embedding model setup"),
          shiny::selectInput(ns("model_type"), "Model family",
            choices = c("FastText" = "fasttext", "Word2Vec" = "word2vec", "BERT" = "bert"),
            selected = "fasttext"),
          shiny::selectInput(ns("model_language_mode"), "Model language",
            choices = c("Use analysis language" = "auto", "Italiano" = "it", "English" = "en", "Multilingual" = "multi"),
            selected = "auto"),
          shiny::selectInput(ns("aggregation"), "Text aggregation",
            choices = c("Mean" = "mean", "Sum" = "sum"),
            selected = "mean"),
          shiny::textInput(ns("custom_model_path"), "Custom model path (optional)", placeholder = "/path/to/model.bin"),
          shiny::actionButton(ns("load_model"), "Load / refresh model", class = "btn-primary btn-block")
        )
      ),
      shiny::column(8,
        shiny::wellPanel(
          shiny::h4("Status"),
          shiny::uiOutput(ns("semantic_status")),
          shiny::helpText("Una volta caricato il modello e calcolati gli embeddings puoi navigare nelle tab sottostanti.")
        )
      )
    ),
    
    shiny::tabsetPanel(
      id = ns("semantic_tabs"),
      type = "tabs",
      
      shiny::tabPanel(
        "Document Similarity",
        shiny::fluidRow(
          shiny::column(4,
            shiny::wellPanel(
              shiny::selectInput(ns("similarity_method"), "Similarity measure",
                choices = c("Cosine" = "cosine", "Euclidean" = "euclidean"),
                selected = "cosine"),
              shiny::numericInput(ns("top_similar"), "Top similar documents", value = 5, min = 1, max = 20),
              shiny::actionButton(ns("calc_similarity"), "Calculate", class = "btn-success btn-block")
            )
          ),
          shiny::column(8,
            shiny::plotOutput(ns("similarity_heatmap"), height = 300)
          )
        ),
        DT::dataTableOutput(ns("similarity_table"))
      ),
      
      shiny::tabPanel(
        "Word Explorer",
        shiny::fluidRow(
          shiny::column(6,
            shiny::wellPanel(
              shiny::textInput(ns("target_word"), "Target word", value = ""),
              shiny::numericInput(ns("top_words"), "Top neighbours", value = 10, min = 1, max = 30),
              shiny::actionButton(ns("find_similar"), "Search", class = "btn-success btn-block")
            )
          ),
          shiny::column(6,
            shiny::plotOutput(ns("word_similarity_plot"), height = 300)
          )
        ),
        DT::dataTableOutput(ns("word_similarity_table"))
      ),
      
      shiny::tabPanel(
        "Clusters & Projection",
        shiny::fluidRow(
          shiny::column(4,
            shiny::wellPanel(
              shiny::numericInput(ns("n_clusters"), "Number of clusters", value = 3, min = 2, max = 10),
              shiny::selectInput(ns("clustering_method"), "Clustering method",
                choices = c("K-means" = "kmeans", "Hierarchical" = "hierarchical"),
                selected = "kmeans"),
              shiny::selectInput(ns("reduction_method"), "Visualization",
                choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
                selected = "pca"),
              shiny::actionButton(ns("run_clustering"), "Run clustering", class = "btn-success btn-block")
            )
          ),
          shiny::column(8,
            shiny::plotOutput(ns("clustering_plot"), height = 320)
          )
        ),
        DT::dataTableOutput(ns("cluster_table"))
      ),
      
      shiny::tabPanel(
        "Embedding Diagnostics",
        shiny::fluidRow(
          shiny::column(6,
            shiny::wellPanel(
              shiny::h4("Norm distribution"),
              shiny::plotOutput(ns("embedding_norm_plot"), height = 250)
            )
          ),
          shiny::column(6,
            shiny::wellPanel(
              shiny::h4("Embedding snapshot"),
              DT::dataTableOutput(ns("embedding_preview"))
            )
          )
        )
      ),
      
      shiny::tabPanel(
        "References",
        shiny::wellPanel(
          shiny::h4("Bibliography"),
          shiny::HTML("
            <ul>
              <li>Mikolov, T. et al. (2013). <em>Distributed representations of words and phrases.</em></li>
              <li>Pennington, J. et al. (2014). <em>GloVe: Global Vectors for Word Representation.</em></li>
              <li>Reimers, N. & Gurevych, I. (2019). <em>Sentence-BERT: Sentence Embeddings using Siamese BERT-networks.</em></li>
            </ul>
          ")
        )
      )
    )
  )
}

mod_semantic_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    lang_cfg <- TextWiller3::get_language_config()
    
    analysis_language <- shiny::reactive({
      lang_cfg$get_version()
      lang_cfg$current_language
    })
    
    selected_model_language <- shiny::reactive({
      if (input$model_language_mode == "auto") {
        analysis_language()
      } else {
        input$model_language_mode
      }
    })
    
    embedding_model <- shiny::reactiveVal()
    document_embeddings <- shiny::reactiveVal()
    
    observe_status <- shiny::reactive({
      list(
        model_loaded = !is.null(embedding_model()),
        embeddings_ready = !is.null(document_embeddings()),
        docs = if (!is.null(corpus())) length(corpus()) else 0
      )
    })
    
    output$semantic_status <- shiny::renderUI({
      status <- observe_status()
      shiny::tagList(
        shiny::p(
          shiny::strong("Language: "),
          ifelse(analysis_language() == "it", "Italiano", "English"),
          shiny::span(" (model set to ", ifelse(selected_model_language() == "it", "Italiano",
                           ifelse(selected_model_language() == "en", "English",
                             ifelse(selected_model_language() == "multi", "Multilingual", "Auto"))),
                      ")", style = "font-size:90%; color:#7f8c8d;")
        ),
        shiny::p(shiny::strong("Documents available: "), status$docs),
        shiny::p(shiny::strong("Model loaded: "),
          ifelse(status$model_loaded, "Yes", "No")),
        shiny::p(shiny::strong("Embeddings ready: "),
          ifelse(status$embeddings_ready, "Yes", "No"))
      )
    })
    
    shiny::observeEvent(input$load_model, {
      shiny::req(corpus())
      shiny::showNotification("Loading embedding model...", type = "message")
      
      tryCatch({
        model <- TextWiller3::load_embedding_model(
          model_type = input$model_type,
          language = selected_model_language(),
          model_path = ifelse(input$custom_model_path == "", NULL, input$custom_model_path)
        )
        embedding_model(model)
        document_embeddings(NULL)
        shiny::showNotification("Model loaded successfully.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error loading model:", e$message), type = "error")
      })
    })
    
    shiny::observe({
      if (!is.null(embedding_model()) && !is.null(corpus())) {
        shiny::showNotification("Calculating document embeddings...", type = "message")
        tryCatch({
          embeddings <- TextWiller3::get_embeddings(
            corpus(),
            embedding_model(),
            method = input$aggregation
          )
          document_embeddings(embeddings)
          shiny::showNotification("Embeddings ready!", type = "message")
        }, error = function(e) {
          shiny::showNotification(paste("Error calculating embeddings:", e$message), type = "error")
        })
      }
    })
    
    shiny::observeEvent(input$calc_similarity, {
      shiny::req(document_embeddings(), corpus())
      
      tryCatch({
        similarity_matrix <- TextWiller3::calculate_semantic_similarity(
          document_embeddings(),
          method = input$similarity_method
        )
        
        similarity_data <- create_similarity_table(similarity_matrix, corpus(), input$top_similar)
        
        output$similarity_heatmap <- shiny::renderPlot({
          plot_similarity_heatmap(similarity_matrix)
        })
        
        output$similarity_table <- DT::renderDataTable({
          DT::datatable(
            similarity_data,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
          )
        })
        
      }, error = function(e) {
        shiny::showNotification(paste("Similarity calculation error:", e$message), type = "error")
      })
    })
    
    shiny::observeEvent(input$find_similar, {
      shiny::req(embedding_model(), input$target_word != "")
      
      tryCatch({
        similar_words <- TextWiller3::find_similar_words(
          input$target_word,
          embedding_model(),
          top_n = input$top_words
        )
        
        if (nrow(similar_words) > 0) {
          output$word_similarity_plot <- shiny::renderPlot({
            ggplot2::ggplot(similar_words, ggplot2::aes(x = reorder(word, similarity), y = similarity)) +
              ggplot2::geom_col(fill = "steelblue", alpha = 0.85) +
              ggplot2::coord_flip() +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = NULL, y = "Similarity")
          })
          
          output$word_similarity_table <- DT::renderDataTable({
            DT::datatable(similar_words, options = list(pageLength = 10), rownames = FALSE)
          })
        } else {
          output$word_similarity_plot <- shiny::renderPlot({ NULL })
          output$word_similarity_table <- DT::renderDataTable({ NULL })
          shiny::showNotification("Word not in vocabulary or no neighbours found.", type = "warning")
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Word similarity error:", e$message), type = "error")
      })
    })
    
    shiny::observeEvent(input$run_clustering, {
      shiny::req(document_embeddings(), corpus())
      
      tryCatch({
        clusters <- TextWiller3::cluster_documents(
          document_embeddings(),
          n_clusters = input$n_clusters,
          method = input$clustering_method
        )
        
        reduced_embeddings <- TextWiller3::reduce_dimensions(
          document_embeddings(),
          method = input$reduction_method,
          n_components = 2
        )
        
        output$clustering_plot <- shiny::renderPlot({
          plot_clustering(reduced_embeddings, clusters, corpus())
        })
        
        cluster_data <- data.frame(
          Document = seq_along(corpus()),
          Text = substr(corpus(), 1, 120),
          Cluster = clusters,
          stringsAsFactors = FALSE
        )
        
        output$cluster_table <- DT::renderDataTable({
          DT::datatable(cluster_data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        })
        
      }, error = function(e) {
        shiny::showNotification(paste("Clustering error:", e$message), type = "error")
      })
    })
    
    embedding_norms <- shiny::reactive({
      emb <- document_embeddings()
      if (is.null(emb)) return(NULL)
      data.frame(
        Document = seq_len(nrow(emb)),
        Norm = sqrt(rowSums(emb^2))
      )
    })
    
    output$embedding_norm_plot <- shiny::renderPlot({
      norms <- embedding_norms()
      if (is.null(norms)) return(NULL)
      ggplot2::ggplot(norms, ggplot2::aes(x = Norm)) +
        ggplot2::geom_histogram(fill = "#2ecc71", color = "white", bins = 20) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Vector norm", y = "Documents")
    })
    
    output$embedding_preview <- DT::renderDataTable({
      emb <- document_embeddings()
      if (is.null(emb)) return(NULL)
      preview <- as.data.frame(emb[1:min(5, nrow(emb)), 1:min(6, ncol(emb)), drop = FALSE])
      DT::datatable(round(preview, 3), options = list(dom = 't'), rownames = FALSE)
    })
  })
}

create_similarity_table <- function(similarity_matrix, corpus, top_similar) {
  top_indices <- apply(similarity_matrix, 1, function(x) head(order(-x), top_similar + 1)[-1])
  
  similarity_data <- data.frame()
  for (i in seq_len(nrow(similarity_matrix))) {
    for (j in seq_len(top_similar)) {
      target_idx <- top_indices[j, i]
      if (!is.na(target_idx)) {
        similarity_data <- rbind(similarity_data, data.frame(
          Document = i,
          Neighbor = target_idx,
          Text = substr(corpus[i], 1, 60),
          NeighborText = substr(corpus[target_idx], 1, 60),
          Similarity = round(similarity_matrix[i, target_idx], 3)
        ))
      }
    }
  }
  similarity_data
}

plot_similarity_heatmap <- function(similarity_matrix) {
  n_show <- min(20, nrow(similarity_matrix))
  show_matrix <- similarity_matrix[1:n_show, 1:n_show, drop = FALSE]
  melted_matrix <- reshape2::melt(show_matrix)
  colnames(melted_matrix) <- c("Document1", "Document2", "Similarity")
  
  ggplot2::ggplot(melted_matrix, ggplot2::aes(x = Document1, y = Document2, fill = Similarity)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0.5, limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())
}

plot_clustering <- function(reduced_embeddings, clusters, corpus) {
  plot_data <- data.frame(
    X = reduced_embeddings[, 1],
    Y = reduced_embeddings[, 2],
    Cluster = as.factor(clusters),
    Text = substr(corpus, 1, 40)
  )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = X, y = Y, color = Cluster)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Dim 1", y = "Dim 2")
}
