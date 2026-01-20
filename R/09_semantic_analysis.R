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
  
  model_type <- tolower(model_type)
  
  model <- if (identical(model_type, "bert")) {
    load_bert_model(language, model_path)
  } else {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("Package 'reticulate' required for FastText/Word2Vec models. ",
           "Install it or switch to the default BERT model.")
    }
    switch(model_type,
      "fasttext" = {
        load_fasttext_model(language, model_path)
      },
      "word2vec" = {
        load_word2vec_model(language, model_path)  
      },
      stop("Unsupported model type: ", model_type)
    )
  }
  
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
    model_path <- ensure_fasttext_model(language)
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
  if (!requireNamespace("text", quietly = TRUE)) {
    stop("Package 'text' is required for BERT embeddings. Install it via install.packages('text').")
  }

  model_name <- model_path
  if (is.null(model_name) || !nzchar(model_name)) {
    model_name <- switch(language,
      "it" = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
      "en" = "sentence-transformers/all-MiniLM-L6-v2",
      "multi" = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
      stop("No default BERT model for language: ", language)
    )
  }

  warmup_embed <- function() {
    # Prefer the "texts" argument, fall back to legacy "x" for older versions
    tryCatch({
      text::textEmbed(texts = "TextWiller warmup", model = model_name)
    }, error = function(e1) {
      tryCatch({
        text::textEmbed(x = "TextWiller warmup", model = model_name)
      }, error = function(e2) {
        stop("Unable to initialize BERT model via text::textEmbed. First attempt (texts=): ", e1$message,
             " | Fallback (x=) error: ", e2$message)
      })
    })
  }

  embedding_dim <- tryCatch({
    warmup <- warmup_embed()
    as.matrix(extract_text_embeddings(warmup))
  }, error = function(e) {
    stop("Unable to initialize BERT model via text::textEmbed: ", e$message)
  })

  return(list(
    type = "bert",
    model_name = model_name,
    language = language,
    embedding_dim = ncol(embedding_dim)
  ))
}

extract_text_embeddings <- function(embed_res) {
  embeddings <- NULL
  if (!is.null(embed_res$sentence_embeddings)) {
    embeddings <- embed_res$sentence_embeddings
  } else if (!is.null(embed_res$texts) && is.list(embed_res$texts)) {
    candidate <- embed_res$texts
    if (!is.null(candidate$texts) && is.list(candidate$texts)) {
      inner <- candidate$texts
      if (!is.null(inner$embeddings)) {
        embeddings <- inner$embeddings
      } else if (!is.null(inner$sentence_embeddings)) {
        embeddings <- inner$sentence_embeddings
      }
    }
  } else if (!is.null(embed_res$word_type_embeddings)) {
    embeddings <- embed_res$word_type_embeddings
  }
  if (is.null(embeddings)) {
    stop("text::textEmbed non ha restituito embeddings utilizzabili (struttura inattesa).")
  }
  embeddings
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
    get_bert_embeddings(text, model, method)
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
  if (!requireNamespace("text", quietly = TRUE)) {
    stop("Package 'text' is required for BERT embeddings.")
  }
  embed_res <- text::textEmbed(texts = text, model = model$model_name)
  embeddings <- as.matrix(extract_text_embeddings(embed_res))
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

cosine_to_target <- function(candidate_mat, target_vec) {
  if (is.null(candidate_mat) || nrow(candidate_mat) == 0) {
    return(numeric())
  }
  target_norm <- sqrt(sum(target_vec^2))
  candidate_norms <- sqrt(rowSums(candidate_mat^2))
  dot_products <- as.numeric(candidate_mat %*% t(target_vec))
  denom <- candidate_norms * target_norm
  denom[denom == 0] <- 1
  similarities <- dot_products / denom
  similarities[is.na(similarities)] <- 0
  similarities
}

build_candidate_vocabulary <- function(text, limit = 300, min_nchar = 3) {
  if (is.null(text) || length(text) == 0) {
    return(character())
  }
  freq <- calculate_word_frequencies_enhanced(text, preprocess = TRUE)
  if (!is.data.frame(freq) || nrow(freq) == 0 || !"word" %in% names(freq)) {
    return(character())
  }
  vocab <- freq$word[nchar(freq$word) >= min_nchar]
  vocab <- unique(tolower(vocab))
  head(vocab, limit)
}

#' Find most similar words
#' 
#' @param word Target word
#' @param model Embedding model
#' @param top_n Number of similar words to return
#' @param corpus_text Optional character vector used to derive candidate vocabulary
#' @return Data frame of similar words and similarities
#' @export
find_similar_words <- function(word, model, top_n = 10, corpus_text = NULL) {
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
  } else if (model$type == "bert") {
    if (!requireNamespace("text", quietly = TRUE)) {
      stop("Package 'text' is required for BERT word similarities.")
    }
    candidates <- build_candidate_vocabulary(corpus_text, limit = max(200, top_n * 6))
    candidates <- setdiff(candidates, tolower(word))
    if (length(candidates) == 0) {
      result <- data.frame(word = character(), similarity = numeric())
    } else {
      embed_res <- text::textEmbed(
        x = c(word, candidates),
        model = model$model_name
      )
      embedding_matrix <- as.matrix(embed_res$sentence_embeddings)
      target_vec <- embedding_matrix[1, , drop = FALSE]
      candidate_mat <- embedding_matrix[-1, , drop = FALSE]
      sims <- cosine_to_target(candidate_mat, target_vec)
      top_idx <- head(order(sims, decreasing = TRUE), top_n)
      result <- data.frame(
        word = candidates[top_idx],
        similarity = sims[top_idx],
        stringsAsFactors = FALSE
      )
    }
  } else {
    stop("Similar word search not supported for model type: ", model$type)
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
