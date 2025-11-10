#' Semantic Analysis with Word Embeddings
#' 
#' Support for FastText, Word2Vec, and BERT embeddings for Italian text
#' 

#' Load pre-trained embedding models
#' 
#' @param model_type Type of model ("fasttext", "word2vec", "bert")
#' @param language Language of the model
#' @param model_path Optional path to custom model
#' @return Embedding model object
#' @export
load_embedding_model <- function(model_type = "fasttext", language = "it", model_path = NULL) {
  mc <- match.call()
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required for embedding models")
  }
  
  model_type <- tolower(model_type)
  
  model <- switch(model_type,
    "fasttext" = {
      load_fasttext_model(language, model_path)
    },
    "word2vec" = {
      load_word2vec_model(language, model_path)  
    },
    "bert" = {
      load_bert_model(language, model_path)
    },
    stop("Unsupported model type: ", model_type)
  )
  
  log_reproducibility_action(
    module = "analysis",
    operation = "load_embedding_model",
    parameters = capture_repro_args(mc),
    input_state = NULL,
    output_state = list(type = model$type, language = model$language)
  )
  
  model
}

#' Load FastText model
#' @noRd
load_fasttext_model <- function(language = "it", model_path = NULL) {
  if (is.null(model_path)) {
    # Default Italian FastText model
    model_path <- switch(language,
      "it" = "cc.it.300.bin",  # Would need to be downloaded
      "en" = "cc.en.300.bin",
      stop("No default model for language: ", language)
    )
  }
  
  if (!file.exists(model_path)) {
    stop("FastText model not found at: ", model_path, 
         "\nDownload from: https://fasttext.cc/docs/en/crawl-vectors.html")
  }
  
  # Load via reticulate
  ft <- reticulate::import("fasttext")
  model <- ft$load_model(model_path)
  
  return(list(
    type = "fasttext",
    model = model,
    language = language
  ))
}

#' Load Word2Vec model  
#' @noRd
load_word2vec_model <- function(language = "it", model_path = NULL) {
  if (is.null(model_path)) {
    # Default Italian Word2Vec model
    model_path <- switch(language,
      "it" = "italian_word2vec.bin",  # Would need custom model
      "en" = "GoogleNews-vectors-negative300.bin",
      stop("No default model for language: ", language)
    )
  }
  
  if (!file.exists(model_path)) {
    stop("Word2Vec model not found at: ", model_path)
  }
  
  # Load via gensim
  gensim <- reticulate::import("gensim.models")
  model <- gensim$KeyedVectors$load_word2vec_format(model_path, binary = TRUE)
  
  return(list(
    type = "word2vec", 
    model = model,
    language = language
  ))
}

#' Load BERT model
#' @noRd
load_bert_model <- function(language = "it", model_path = NULL) {
  if (is.null(model_path)) {
    # Default models for each language
    model_path <- switch(language,
      "it" = "dbmdz/bert-base-italian-xxl-cased",
      "en" = "sentence-transformers/all-MiniLM-L6-v2",
      "multi" = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
      stop("No default BERT model for language: ", language)
    )
  }
  
  # Load via sentence-transformers
  sentence_transformers <- reticulate::import("sentence_transformers")
  model <- sentence_transformers$SentenceTransformer(model_path)
  
  return(list(
    type = "bert",
    model = model,
    language = language
  ))
}

#' Get word embeddings
#' 
#' @param text Character vector of texts
#' @param model Embedding model from load_embedding_model()
#' @param method Method for text representation ("mean", "sum", "cls")
#' @return Matrix of embeddings
#' @export
get_embeddings <- function(text, model, method = "mean") {
  mc <- match.call()
  
  embeddings <- if (model$type == "fasttext") {
    get_fasttext_embeddings(text, model$model, method)
  } else if (model$type == "word2vec") {
    get_word2vec_embeddings(text, model$model, method)  
  } else if (model$type == "bert") {
    get_bert_embeddings(text, model$model, method)
  } else {
    stop("Unsupported model type: ", model$type)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "get_embeddings",
    parameters = capture_repro_args(mc),
    input_state = list(text_length = length(text), model_type = model$type, method = method),
    output_state = snapshot_matrix(embeddings)
  )
  
  embeddings
}

#' Get FastText embeddings
#' @noRd
get_fasttext_embeddings <- function(text, model, method = "mean") {
  embeddings <- lapply(text, function(txt) {
    words <- strsplit(txt, "\\s+")[[1]]
    words <- words[nchar(words) > 2]  # FastText needs min 3 chars
    
    if (length(words) == 0) {
      return(matrix(0, nrow = 1, ncol = 300))
    }
    
    word_vectors <- lapply(words, function(word) {
      tryCatch({
        model$get_word_vector(word)
      }, error = function(e) {
        rep(0, 300)
      })
    })
    
    word_matrix <- do.call(rbind, word_vectors)
    
    # Aggregate word vectors
    switch(method,
      "mean" = colMeans(word_matrix),
      "sum" = colSums(word_matrix),
      stop("Unsupported method: ", method)
    )
  })
  
  do.call(rbind, embeddings)
}

#' Get Word2Vec embeddings
#' @noRd  
get_word2vec_embeddings <- function(text, model, method = "mean") {
  embeddings <- lapply(text, function(txt) {
    words <- strsplit(tolower(txt), "\\s+")[[1]]
    words <- words[nchar(words) > 1]
    
    # Get vectors for words in vocabulary
    word_vectors <- lapply(words, function(word) {
      if (word %in% model$index_to_key) {
        model$get_vector(word)
      } else {
        NULL
      }
    })
    
    # Remove NULLs
    word_vectors <- word_vectors[!sapply(word_vectors, is.null)]
    
    if (length(word_vectors) == 0) {
      return(rep(0, model$vector_size))
    }
    
    word_matrix <- do.call(rbind, word_vectors)
    
    switch(method,
      "mean" = colMeans(word_matrix),
      "sum" = colSums(word_matrix), 
      stop("Unsupported method: ", method)
    )
  })
  
  do.call(rbind, embeddings)
}

#' Get BERT embeddings
#' @noRd
get_bert_embeddings <- function(text, model, method = "mean") {
  # BERT handles full sentences directly
  embeddings <- model$encode(text)
  return(embeddings)
}

#' Calculate semantic similarity between texts
#' 
#' @param embeddings Matrix of embeddings from get_embeddings()
#' @param method Similarity method ("cosine", "euclidean", "manhattan")
#' @return Similarity matrix
#' @export
calculate_semantic_similarity <- function(embeddings, method = "cosine") {
  mc <- match.call()
  result <- if (method == "cosine") {
    cosine_similarity(embeddings)
  } else if (method == "euclidean") {
    1 / (1 + stats::dist(embeddings, method = "euclidean"))
  } else if (method == "manhattan") {
    1 / (1 + stats::dist(embeddings, method = "manhattan"))  
  } else {
    stop("Unsupported similarity method: ", method)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_semantic_similarity",
    parameters = capture_repro_args(mc),
    input_state = snapshot_matrix(embeddings),
    output_state = snapshot_matrix(result)
  )
  
  result
}

#' Cosine similarity
#' @noRd
cosine_similarity <- function(x) {
  x <- as.matrix(x)
  sim <- x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2))))
  sim[is.na(sim)] <- 0
  return(sim)
}

#' Find most similar words
#' 
#' @param word Target word
#' @param model Embedding model
#' @param top_n Number of similar words to return
#' @return Data frame of similar words and similarities
#' @export
find_similar_words <- function(word, model, top_n = 10) {
  mc <- match.call()
  if (model$type == "fasttext") {
    similar <- model$model$get_nearest_neighbors(word, k = top_n)
    result <- data.frame(
      word = sapply(similar, function(x) x[[2]]),
      similarity = sapply(similar, function(x) x[[1]]),
      stringsAsFactors = FALSE
    )
  } else if (model$type == "word2vec") {
    if (word %in% model$model$index_to_key) {
      similar <- model$model$most_similar(word, topn = top_n)
      result <- data.frame(
        word = sapply(similar, function(x) x[[1]]),
        similarity = sapply(similar, function(x) x[[2]]),
        stringsAsFactors = FALSE
      )
    } else {
      warning("Word '", word, "' not in vocabulary")
      result <- data.frame(word = character(), similarity = numeric())
    }
  } else {
    stop("Similar word search not supported for BERT models")
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "find_similar_words",
    parameters = capture_repro_args(mc),
    input_state = list(word = word, model_type = model$type, top_n = top_n),
    output_state = snapshot_dataframe(result)
  )
  
  return(result)
}

#' Semantic clustering of documents
#' 
#' @param embeddings Document embeddings
#' @param n_clusters Number of clusters
#' @param method Clustering method ("kmeans", "hierarchical")
#' @return Cluster assignments
#' @export
cluster_documents <- function(embeddings, n_clusters = 3, method = "kmeans") {
  mc <- match.call()
  result <- if (method == "kmeans") {
    stats::kmeans(embeddings, centers = n_clusters)$cluster
  } else if (method == "hierarchical") {
    stats::cutree(stats::hclust(stats::dist(embeddings)), k = n_clusters)
  } else {
    stop("Unsupported clustering method: ", method)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "cluster_documents",
    parameters = capture_repro_args(mc),
    input_state = list(dim = dim(embeddings), n_clusters = n_clusters, method = method),
    output_state = list(cluster_sizes = table(result))
  )
  
  result
}

#' Reduce embedding dimensions for visualization
#' 
#' @param embeddings High-dimensional embeddings
#' @param method Dimensionality reduction method ("pca", "tsne", "umap")
#' @param n_components Number of dimensions for reduction
#' @return Reduced embeddings
#' @export
reduce_dimensions <- function(embeddings, method = "pca", n_components = 2) {
  mc <- match.call()
  result <- if (method == "pca") {
    stats::prcomp(embeddings, center = TRUE, scale. = TRUE)$x[, 1:n_components]
  } else if (method == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      stop("Package 'Rtsne' required for t-SNE")
    }
    Rtsne::Rtsne(embeddings, dims = n_components)$Y
  } else if (method == "umap") {
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop("Package 'umap' required for UMAP")
    }
    umap::umap(embeddings)$layout
  } else {
    stop("Unsupported reduction method: ", method)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "reduce_dimensions",
    parameters = capture_repro_args(mc),
    input_state = list(dim = dim(embeddings), method = method, n_components = n_components),
    output_state = snapshot_matrix(result)
  )
  
  result
}
