

### FILE: 00_import_legacy.R
#' Import Legacy TextWiller Functions
#' 
#' This file imports and adapts the original TextWiller functions
#' to work within the new TextWiller3 structure.
#' 

#' Import original normalization functions
#' 
#' These are your core text normalization functions that we want to preserve
import_legacy_normalization <- function() {
  # Your original normalization functions will be loaded here
  # We'll create wrapper functions that maintain the original API
  # while integrating with the new system
}

#' Import original sentiment analysis
import_legacy_sentiment <- function() {
  # Your sentiment analysis functions
}

#' Import original classification functions  
import_legacy_classification <- function() {
  # classificaUtenti and related functions
}

#' Import original URL and pattern extraction
import_legacy_extraction <- function() {
  # urlExtract, patternExtract, etc.
}


### FILE: 01_corpus_io.R
#' Corpus Import and Export Functions
#' 
#' Functions for importing text data from various sources and exporting results.
#' 

#' Import text files from directory
#' 
#' @param directory Path to directory containing text files
#' @param pattern File pattern to match (default: "\\\\.txt$")
#' @param recursive Whether to search subdirectories
#' @param encoding File encoding
#' @return A data frame with documents and metadata
#' @export
import_text_files <- function(directory, pattern = "\\.txt$", recursive = FALSE, encoding = "UTF-8") {
  if (!dir.exists(directory)) {
    stop("Directory does not exist: ", directory)
  }
  
  files <- list.files(directory, pattern = pattern, full.names = TRUE, recursive = recursive)
  
  if (length(files) == 0) {
    stop("No files found matching pattern: ", pattern)
  }
  
  documents <- data.frame(
    text = character(length(files)),
    doc_id = character(length(files)),
    source = rep("text_file", length(files)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(files)) {
    tryCatch({
      content <- readLines(files[i], encoding = encoding, warn = FALSE)
      documents$text[i] <- paste(content, collapse = "\n")
      documents$doc_id[i] <- tools::file_path_sans_ext(basename(files[i]))
    }, error = function(e) {
      warning("Error reading file ", files[i], ": ", e$message)
      documents$text[i] <- ""
    })
  }
  
  # Remove empty documents
  documents <- documents[nchar(documents$text) > 0, ]
  
  return(documents)
}

#' Import CSV files with text data
#' 
#' @param file_path Path to CSV file
#' @param text_column Name of column containing text
#' @param id_column Name of column for document IDs (optional)
#' @param skip_rows Number of rows to skip
#' @param encoding File encoding
#' @return A data frame with documents and metadata
#' @export
import_csv_text <- function(file_path, text_column, id_column = NULL, skip_rows = 0, encoding = "UTF-8") {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  data <- read.csv(file_path, skip = skip_rows, encoding = encoding, stringsAsFactors = FALSE)
  
  if (!text_column %in% names(data)) {
    stop("Text column '", text_column, "' not found in file")
  }
  
  if (is.null(id_column)) {
    doc_ids <- paste0(tools::file_path_sans_ext(basename(file_path)), "_", seq_len(nrow(data)))
  } else {
    if (!id_column %in% names(data)) {
      stop("ID column '", id_column, "' not found in file")
    }
    doc_ids <- as.character(data[[id_column]])
  }
  
  documents <- data.frame(
    text = data[[text_column]],
    doc_id = doc_ids,
    source = rep("csv_file", nrow(data)),
    stringsAsFactors = FALSE
  )
  
  # Remove empty documents
  documents <- documents[!is.na(documents$text) & nchar(documents$text) > 0, ]
  
  return(documents)
}

#' Get corpus statistics
#' 
#' @param corpus Character vector of documents
#' @return A list with corpus statistics
#' @export
get_corpus_stats <- function(corpus) {
  if (is.null(corpus) || length(corpus) == 0) {
    return(list(
      n_docs = 0,
      total_words = 0,
      total_chars = 0,
      avg_words = 0,
      avg_chars = 0
    ))
  }
  
  word_counts <- sapply(strsplit(corpus, "\\s+"), length)
  char_counts <- nchar(corpus)
  
  list(
    n_docs = length(corpus),
    total_words = sum(word_counts),
    total_chars = sum(char_counts),
    avg_words = round(mean(word_counts), 1),
    avg_chars = round(mean(char_counts), 1),
    max_words = max(word_counts),
    min_words = min(word_counts)
  )
}


### FILE: 02_preprocessing.R
#' Text Preprocessing Functions
#' 
#' Enhanced preprocessing that integrates original TextWiller functionality
#' with new capabilities.
#' 

#' Enhanced text normalization using original TextWiller pipeline
#' 
#' This function wraps your original normalizzaTesti with additional options
#' 
#' @param text Character vector of texts
#' @param use_legacy Whether to use the original TextWiller normalization
#' @param ... Additional parameters passed to normalizzaTesti
#' @return Normalized text vector
#' @export
normalize_text <- function(text, use_legacy = TRUE, ...) {
  if (use_legacy && exists("normalizzaTesti")) {
    # Use your original function
    return(normalizzaTesti(text, ...))
  } else {
    # Use new simplified normalization
    return(clean_text_basic(text, ...))
  }
}

#' Basic text cleaning (fallback when legacy not available)
#' @noRd
clean_text_basic <- function(text, to_lower = TRUE, remove_punct = TRUE, 
                            remove_numbers = TRUE, remove_whitespace = TRUE) {
  
  if (to_lower) {
    text <- tolower(text)
  }
  
  if (remove_punct) {
    text <- gsub("[[:punct:]]", " ", text)
  }
  
  if (remove_numbers) {
    text <- gsub("[[:digit:]]", " ", text)
  }
  
  if (remove_whitespace) {
    text <- gsub("\\s+", " ", text)
    text <- trimws(text)
  }
  
  return(text)
}

#' Enhanced stopword removal with Italian support
#' 
#' Uses your original Italian stopwords when available
#' 
#' @param text Character vector
#' @param language Language for stopwords
#' @param use_legacy Whether to use original stopword lists
#' @return Text with stopwords removed
#' @export
remove_stopwords_enhanced <- function(text, language = "it", use_legacy = TRUE) {
  
  if (use_legacy && language == "it" && exists("stopwords_ita")) {
    # Use your original Italian stopwords
    stopwords_list <- stopwords_ita
  } else {
    # Use basic stopwords
    stopwords_list <- get_basic_stopwords(language)
  }
  
  # Remove stopwords
  pattern <- paste0("\\b(", paste(stopwords_list, collapse = "|"), ")\\b")
  text <- gsub(pattern, "", text, ignore.case = TRUE)
  
  # Clean up
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}

#' Get basic stopword lists
#' @noRd
get_basic_stopwords <- function(language = "it") {
  if (language == "it") {
    return(c("il", "lo", "la", "i", "gli", "le", "un", "uno", "una", 
            "di", "a", "da", "in", "con", "su", "per", "tra", "fra",
            "è", "sono", "era", "erano", "essere", "avere", "ha", "hanno",
            "questo", "questa", "quello", "quella", "che", "chi", "cui",
            "come", "dove", "quando", "perché"))
  } else {
    return(c("the", "a", "an", "and", "or", "but", "in", "on", "at", 
            "to", "for", "of", "with", "by", "as", "is", "are", "was",
            "were", "be", "been", "have", "has", "had"))
  }
}

#' Configurable preprocessing pipeline
#' 
#' @param text Character vector
#' @param pipeline List of processing steps
#' @param use_legacy Use original TextWiller functions when available
#' @return Processed text
#' @export
preprocess_pipeline <- function(text, pipeline = c("lowercase", "remove_punct", "remove_stopwords"), 
                               use_legacy = TRUE) {
  
  for (step in pipeline) {
    text <- switch(step,
      "lowercase" = tolower(text),
      "remove_punct" = if(use_legacy && exists("normalizzapunteggiatura")) {
        normalizzapunteggiatura(text)
      } else {
        gsub("[[:punct:]]", " ", text)
      },
      "remove_numbers" = gsub("[[:digit:]]", " ", text),
      "remove_stopwords" = remove_stopwords_enhanced(text, use_legacy = use_legacy),
      "normalize_urls" = if(use_legacy && exists("normalizzahtml")) {
        normalizzahtml(text)
      } else {
        gsub("https?://[^\\s]+", " URL ", text)
      },
      "normalize_emoticons" = if(use_legacy && exists("normalizza_emoticon")) {
        normalizza_emoticon(text)
      } else {
        text  # Fallback - no emoticon normalization
      },
      "normalize_slang" = if(use_legacy && exists("normalizzaslang")) {
        normalizzaslang(text)
      } else {
        text  # Fallback - no slang normalization
      },
      "trim_whitespace" = {
        text <- gsub("\\s+", " ", text)
        trimws(text)
      },
      text  # Default: no change
    )
  }
  
  return(text)
}


### FILE: 03_exploration.R
#' Enhanced Corpus Exploration Functions
#' 
#' Integrates original TextWiller analysis capabilities
#' 

#' Advanced word frequency analysis
#' 
#' Uses original TextWiller preprocessing when available
#' 
#' @param corpus Character vector
#' @param preprocess Whether to preprocess text
#' @param use_legacy Use original TextWiller preprocessing
#' @return Data frame with word frequencies
#' @export
calculate_word_frequencies_enhanced <- function(corpus, preprocess = TRUE, 
                                               use_legacy = TRUE) {
  
  if (preprocess) {
    corpus <- preprocess_pipeline(corpus, 
                                 pipeline = c("lowercase", "remove_punct", "remove_stopwords"),
                                 use_legacy = use_legacy)
  }
  
  # Combine all documents
  all_text <- paste(corpus, collapse = " ")
  
  # Split into words
  words <- strsplit(all_text, "\\s+")[[1]]
  words <- words[nchar(words) > 1]  # Remove single characters
  
  # Calculate frequencies
  freq_table <- table(words)
  freq_table <- sort(freq_table, decreasing = TRUE)
  
  data.frame(
    word = names(freq_table),
    frequency = as.numeric(freq_table),
    percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 2),
    stringsAsFactors = FALSE
  )
}

#' Sentiment analysis using original TextWiller function
#' 
#' @param text Character vector
#' @param use_legacy Whether to use original sentiment function
#' @return Sentiment scores
#' @export
analyze_sentiment <- function(text, use_legacy = TRUE) {
  if (use_legacy && exists("sentiment")) {
    return(sentiment(text))
  } else {
    warning("Original sentiment function not available. Using basic fallback.")
    return(rep(0, length(text)))  # Basic fallback
  }
}

#' User classification using original classificaUtenti
#' 
#' @param names Character vector of names
#' @param use_legacy Whether to use original classification
#' @return Classification results
#' @export
classify_users <- function(names, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti")) {
    return(classificaUtenti(names))
  } else {
    warning("Original classificaUtenti not available.")
    return(rep("unknown", length(names)))
  }
}

#' URL extraction using original function
#' 
#' @param text Character vector
#' @param use_legacy Whether to use original urlExtract
#' @return Extracted URLs
#' @export
extract_urls <- function(text, use_legacy = TRUE) {
  if (use_legacy && exists("urlExtract")) {
    return(urlExtract(text))
  } else {
    # Basic URL extraction fallback
    urls <- regmatches(text, gregexpr("https?://[^\\s]+", text))
    return(unlist(urls))
  }
}


### FILE: 04_lexical_analysis.R
#' Lexical Analysis Functions
#' 
#' Advanced lexical analysis including complexity measures, TTR, and Italian-specific metrics
#' 

#' Calculate Type-Token Ratio (TTR) and variants
#' 
#' @param text Character vector of texts
#' @param variant Type of TTR to calculate ("simple", "root", "corrected", "herdan", "guiraud")
#' @return TTR value
#' @export
calculate_ttr <- function(text, variant = "simple") {
  if (is.null(text) || length(text) == 0) return(NA)
  
  # Combine all texts
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]  # Remove empty strings
  
  if (length(words) == 0) return(NA)
  
  types <- unique(words)
  tokens <- length(words)
  
  switch(variant,
    "simple" = length(types) / tokens,
    "root" = length(types) / sqrt(tokens),
    "corrected" = length(types) / sqrt(2 * tokens),
    "herdan" = log(length(types)) / log(tokens),
    "guiraud" = length(types) / sqrt(tokens),
    length(types) / tokens  # default to simple
  )
}

#' Calculate Moving Average TTR (MATTR)
#' 
#' @param text Character vector
#' @param window_size Size of moving window
#' @return MATTR value
#' @export
calculate_mattr <- function(text, window_size = 100) {
  if (is.null(text) || length(text) == 0) return(NA)
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) <= window_size) {
    return(calculate_ttr(text))
  }
  
  ttr_values <- numeric(length(words) - window_size + 1)
  
  for (i in 1:(length(words) - window_size + 1)) {
    window_words <- words[i:(i + window_size - 1)]
    types <- unique(window_words)
    ttr_values[i] <- length(types) / window_size
  }
  
  mean(ttr_values, na.rm = TRUE)
}

#' Calculate lexical density (content word ratio)
#' 
#' @param text Character vector
#' @param language Language for content word identification
#' @return Lexical density value
#' @export
calculate_lexical_density <- function(text, language = "it") {
  if (is.null(text) || length(text) == 0) return(NA)
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) return(NA)
  
  # Basic content word identification (can be enhanced with POS tagging)
  if (language == "it") {
    # Italian function words
    function_words <- c("il", "lo", "la", "i", "gli", "le", "un", "uno", "una",
                       "di", "a", "da", "in", "con", "su", "per", "tra", "fra",
                       "è", "sono", "era", "essere", "avere", "ha", "hanno",
                       "questo", "questa", "quello", "quella", "che", "chi")
  } else {
    # English function words
    function_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at",
                       "to", "for", "of", "with", "by", "as", "is", "are", "was")
  }
  
  content_words <- words[!words %in% function_words]
  length(content_words) / length(words)
}

#' Calculate Brunato's complexity measures for Italian text
#' 
#' Based on Italian linguistic complexity measures
#' 
#' @param text Character vector
#' @return List of complexity measures
#' @export
calculate_brunato_measures <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(list(
      basic_vocabulary_ratio = NA,
      content_word_ratio = NA,
      lexical_richness = NA,
      syntactic_complexity = NA
    ))
  }
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) {
    return(list(
      basic_vocabulary_ratio = NA,
      content_word_ratio = NA,
      lexical_richness = NA,
      syntactic_complexity = NA
    ))
  }
  
  # Italian basic vocabulary (simplified - should be expanded)
  basic_vocab <- c("essere", "avere", "fare", "dire", "andare", "venire",
                  "potere", "volere", "dovere", "sapere", "vedere", "sentire",
                  "parlare", "pensare", "trovare", "prendere", "dare", "mettere")
  
  # Calculate measures
  basic_vocab_ratio <- sum(words %in% basic_vocab) / length(words)
  content_word_ratio <- calculate_lexical_density(text, "it")
  lexical_richness <- calculate_ttr(text, "herdan")
  
  # Simple syntactic complexity (average word length as proxy)
  avg_word_length <- mean(nchar(words), na.rm = TRUE)
  
  list(
    basic_vocabulary_ratio = round(basic_vocab_ratio, 3),
    content_word_ratio = round(content_word_ratio, 3),
    lexical_richness = round(lexical_richness, 3),
    syntactic_complexity = round(avg_word_length, 2)
  )
}

#' Calculate multiple lexical diversity measures
#' 
#' @param text Character vector
#' @return Data frame with multiple diversity measures
#' @export
calculate_lexical_diversity <- function(text) {
  measures <- list()
  
  measures$ttr_simple <- calculate_ttr(text, "simple")
  measures$ttr_root <- calculate_ttr(text, "root")
  measures$ttr_corrected <- calculate_ttr(text, "corrected")
  measures$ttr_herdan <- calculate_ttr(text, "herdan")
  measures$mattr <- calculate_mattr(text)
  measures$lexical_density <- calculate_lexical_density(text)
  
  # Convert to data frame
  result <- data.frame(
    measure = names(measures),
    value = unlist(measures),
    stringsAsFactors = FALSE
  )
  
  result$value <- round(result$value, 4)
  return(result)
}

#' Extract keywords based on TF-IDF or log-likelihood
#' 
#' @param corpus Character vector of documents
#' @param method Method for keyword extraction ("tfidf", "loglikelihood")
#' @param top_n Number of top keywords to return
#' @return Data frame with keywords and scores
#' @export
extract_keywords <- function(corpus, method = "tfidf", top_n = 20) {
  if (is.null(corpus) || length(corpus) == 0) {
    return(data.frame(term = character(), score = numeric()))
  }
  
  # Calculate term frequencies
  all_terms <- unlist(strsplit(tolower(paste(corpus, collapse = " ")), "\\s+"))
  all_terms <- all_terms[nchar(all_terms) > 1]
  
  if (length(all_terms) == 0) {
    return(data.frame(term = character(), score = numeric()))
  }
  
  term_freq <- table(all_terms)
  term_freq <- sort(term_freq, decreasing = TRUE)
  
  if (method == "tfidf") {
    # Simple TF-IDF implementation
    doc_freq <- sapply(names(term_freq), function(term) {
      sum(grepl(paste0("\\b", term, "\\b"), tolower(corpus)))
    })
    
    tf <- as.numeric(term_freq)
    idf <- log(length(corpus) / (doc_freq + 1))
    scores <- tf * idf
    
  } else if (method == "loglikelihood") {
    # Log-likelihood ratio (simplified)
    total_terms <- sum(term_freq)
    expected <- total_terms / length(term_freq)
    scores <- 2 * term_freq * log(term_freq / expected)
  } else {
    scores <- as.numeric(term_freq)  # Default to frequency
  }
  
  result <- data.frame(
    term = names(term_freq),
    frequency = as.numeric(term_freq),
    score = round(as.numeric(scores), 4),
    stringsAsFactors = FALSE
  )
  
  result <- result[order(-result$score), ]
  return(head(result, top_n))
}


### FILE: 05_sentiment_analysis.R
# R/05_sentiment_analysis.R
#' Enhanced Sentiment Analysis with Original TextWiller Functions
#' 

#' Italian Sentiment Analysis
#' 
#' Wrapper per la tua funzione sentiment originale
#' 
#' @param text Testo da analizzare
#' @param algorithm Algoritmo da usare
#' @param normalizzaTesti Normalizzare il testo prima dell'analisi
#' @return Punteggi sentiment
#' @export
analyze_sentiment_it <- function(text, algorithm = "Mattivio", normalizzaTesti = TRUE) {
  if (exists("sentiment")) {
    # Usa la funzione originale
    return(sentiment(text, algorithm = algorithm, normalizzaTesti = normalizzaTesti))
  } else {
    # Fallback basico
    warning("Funzione sentiment originale non disponibile")
    return(rep(0, length(text)))
  }
}

#' Carica i dizionari sentiment
#' @export
load_sentiment_dicts <- function() {
  if (exists("vocabolarioMattivio")) {
    return(vocabolarioMattivio)
  } else if (exists("vocabolariMadda")) {
    return(vocabolariMadda)
  } else {
    stop("Dizionari sentiment non disponibili")
  }
}


### FILE: 06_classification.R
# R/06_classification.R
#' User Classification Functions
#' 

#' Classifica genere per nomi italiani
#' 
#' @param names Vettore di nomi
#' @param use_legacy Usa la funzione originale
#' @return Classificazione di genere
#' @export
classify_gender_it <- function(names, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti")) {
    return(classificaUtenti(names))
  } else {
    # Fallback basico
    return(rep("unknown", length(names)))
  }
}

#' Classifica luoghi italiani
#' @export
classify_location_it <- function(places, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti") && exists("vocabolarioLuoghi")) {
    return(classificaUtenti(places, vocabolario = vocabolarioLuoghi))
  } else {
    return(rep("unknown", length(places)))
  }
}


### FILE: 07_text_extraction.R
# R/07_text_extraction.R
#' Text Extraction Utilities
#' 

#' Estrazione URL
#' @export
extract_urls_enhanced <- function(text, use_legacy = TRUE) {
  if (use_legacy && exists("urlExtract")) {
    return(urlExtract(text))
  } else {
    # Estrazione basica
    urls <- regmatches(text, gregexpr("https?://[^\\s]+", text))
    return(unlist(urls))
  }
}

#' Estrazione pattern (hashtag, mention, etc.)
#' @export
extract_patterns <- function(text, pattern = "@\\w+", use_legacy = TRUE) {
  if (use_legacy && exists("patternExtract")) {
    return(patternExtract(text, pattern = pattern))
  } else {
    # Estrazione basica
    matches <- regmatches(text, gregexpr(pattern, text))
    return(unlist(matches))
  }
}


### FILE: 09_semantic_analysis.R
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
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required for embedding models")
  }
  
  model_type <- tolower(model_type)
  
  switch(model_type,
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
  
  if (model$type == "fasttext") {
    get_fasttext_embeddings(text, model$model, method)
  } else if (model$type == "word2vec") {
    get_word2vec_embeddings(text, model$model, method)  
  } else if (model$type == "bert") {
    get_bert_embeddings(text, model$model, method)
  } else {
    stop("Unsupported model type: ", model$type)
  }
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
  if (method == "cosine") {
    cosine_similarity(embeddings)
  } else if (method == "euclidean") {
    1 / (1 + dist(embeddings, method = "euclidean"))
  } else if (method == "manhattan") {
    1 / (1 + dist(embeddings, method = "manhattan"))  
  } else {
    stop("Unsupported similarity method: ", method)
  }
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
  if (model$type == "fasttext") {
    similar <- model$model$get_nearest_neighbors(word, k = top_n)
    data.frame(
      word = sapply(similar, function(x) x[[2]]),
      similarity = sapply(similar, function(x) x[[1]]),
      stringsAsFactors = FALSE
    )
  } else if (model$type == "word2vec") {
    if (word %in% model$model$index_to_key) {
      similar <- model$model$most_similar(word, topn = top_n)
      data.frame(
        word = sapply(similar, function(x) x[[1]]),
        similarity = sapply(similar, function(x) x[[2]]),
        stringsAsFactors = FALSE
      )
    } else {
      warning("Word '", word, "' not in vocabulary")
      return(data.frame(word = character(), similarity = numeric()))
    }
  } else {
    stop("Similar word search not supported for BERT models")
  }
}

#' Semantic clustering of documents
#' 
#' @param embeddings Document embeddings
#' @param n_clusters Number of clusters
#' @param method Clustering method ("kmeans", "hierarchical")
#' @return Cluster assignments
#' @export
cluster_documents <- function(embeddings, n_clusters = 3, method = "kmeans") {
  if (method == "kmeans") {
    kmeans(embeddings, centers = n_clusters)$cluster
  } else if (method == "hierarchical") {
    cutree(hclust(dist(embeddings)), k = n_clusters)
  } else {
    stop("Unsupported clustering method: ", method)
  }
}

#' Reduce embedding dimensions for visualization
#' 
#' @param embeddings High-dimensional embeddings
#' @param method Dimensionality reduction method ("pca", "tsne", "umap")
#' @param n_components Number of dimensions for reduction
#' @return Reduced embeddings
#' @export
reduce_dimensions <- function(embeddings, method = "pca", n_components = 2) {
  if (method == "pca") {
    prcomp(embeddings, center = TRUE, scale. = TRUE)$x[, 1:n_components]
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
}


### FILE: classificaUtenti.R
#' User classification
#' 
#' The functions associates names in \code{names} to the items in a dictionary. E.g.,
#' \code{data(dizionario_nomi_propri)} classifies names based on gender and \code{data(dizionario_luoghi)}
#' classifies a city's geographic area (vedi esempio).
#' 
#' 
#' In \code{data(dizionario_luoghi)} the towns of Re (800
#' residents, north-west) and Lu (1200 residents, north-west) were excluded as 
#' they were conflicting with provinces' abbreviations.
#' 
#' (update 05-09-2016) For names detection, we intriduced 
#' three different classification options: the first two look for words divided by blanks, 
#' while the third also finds character strings within words using the function \code{grepl}. 
#' The parameters that define this part of the classification process are described below.
#' 
#' @aliases classificaUtenti dizionario_nomi_propri dizionario_luoghi
#' @param names Vector containing names.
#' @param vocabolario Single column \code{data.frame} used for the classification. 
#' \code{rownames(vocabolario)} must be unique (these are the values that will be used for classification). 
#' For names, the dictionary \code{data(dizionario_nomi_propri)} can be used. NOTE: In \code{vocabolario}
#' use lower case only and do not use "NA" ("na" is accepted).
#' @param scan_interno If \code{TRUE}, the function also performs the (\code{grepl} internal scan on the dictionaries.
#' @param vocab_interno Dictionary used for internal scan. By default, 
#' it is equal to all vocabulary names of length >= 5.
#' @param how_class Defines classification in ambiguous cases, i.e. when 
#' multiple matches are found for a single name. We have defined three possible cases:
#' \code{modeFirst} (default) uses a recognized modal category and, in case of multimodality,
#' the first match; \code{first} performs classification using the first match, 
#' and \code{last} performs classification using the last match.
#' @param cat_interna \code{NULL} (default) allows to identify one or more classification categories such that
#' all values of a dictionary are used in the internal scan (instead of just those with length >= 5).
#' @return A named vector with elements from the \code{categoria} columns of the 
#' \code{vocabolario} data.frame. For \code{vocabolario=dizionario_nomi_propri}, the levels are
#' \code{c('masc','femm','ente')}.
#' @author Mattia Uttini, Livio Finos, Andrea Mamprin, Dario Solari
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' \dontrun{data(dizionario_nomi_propri)}
#' \dontrun{str(dizionario_nomi_propri)}
#' classificaUtenti(c('livio','alessandra','alessandraRossi', 'mariobianchi'), scan_interno=TRUE)
#' \dontrun{data(dizionario_luoghi)}
#' classificaUtenti(c('Bosa','Pordenone, Italy'), dizionario_luoghi)
#' 
#' @export classificaUtenti
classificaUtenti <- function(names, vocabolario=NULL, scan_interno=FALSE, vocab_interno=NULL, how_class="modeFirst", cat_interna=NULL){
  
  if (is.factor(names)){
    names <- as.character(names)
  }
  
  
  if (is.null(vocabolario)) {
    data(vocabolario_nomi_propri)
    vocabolario = vocabolario_nomi_propri
  }
  
  if(scan_interno&is.null(vocab_interno)){
    vocab_interno <- vocabolario[which(nchar((rownames(vocabolario))>=5)|(substr(rownames(vocabolario),1,1)==".")|(vocabolario[,1]%in%cat_interna)),,drop=FALSE]
  }
  
  Encoding(names) <- "UTF-8"
  names <- iconv(names, "UTF-8", "UTF-8", sub='')
  nomi_originali <- .togliSpaziEsterni(names)
  
  names <- tolower(names)
  
  idspazi = grep("\\W", rownames(vocabolario))
  conspazi = rownames(vocabolario)[idspazi]
  conspazi = conspazi[order(sapply(conspazi, nchar), decreasing = TRUE)]
  for (i in conspazi) {
    names = gsub(i, gsub("\\W", "_", i), names)
  }
  rownames(vocabolario)[idspazi] = gsub("\\W", "_", rownames(vocabolario)[idspazi])
  

  classifica <- function(class,how_class){
    
    if(how_class=="modeFirst"){
      ct <- table(class)
      return(ifelse(sum(ct==max(ct))==1, names(ct)[which.max(ct)], class[1]))
    }
    
    if(how_class=="first") return(class[1])
    
    if(how_class=="last") return(class[length(class)])
    
  }
  
  
  ## prima classificazione
  txt1 <- names
  classificazione <- sapply(txt1, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, split="[[:punct:][:space:]]")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## seconda classificazione su NA della prima
  na1 <- is.na(classificazione)
  txt2 <- nomi_originali[na1]
  txt2 <- gsub('([[:upper:]])',  ' \\1', txt2)
  txt2 <- tolower(txt2)
  txt2 <- gsub("\\d", "", txt2)
  classificazione[which(na1)] <- sapply(txt2, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, "\\W")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## terza classificazione, se scan_interno==T
  if(scan_interno){
    na2 <- is.na(classificazione)
    txt3 <- nomi_originali[na2]
    txt3 <- tolower(txt3)
    classificazione[which(na2)] <- sapply(txt3, function(txt){
      cl <- as.character(vocab_interno$categoria[sapply(ifelse(substr(rownames(vocab_interno),1,1)==".",paste0("\\",rownames(vocab_interno)),rownames(vocab_interno)), function(x) grepl(x, txt))])
      return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
    }
    )
  }
  
  return(unlist(classificazione))
  
}


### FILE: normalizza_emoticon.R
#' Normalize emoticons
#' 
#' Using regexes, finds emoticons (e.g.,: :), :D) in a text and substitutes them with a keyword.
#' 
#' @param testo Character vector of texts.
#' @param perl See \code{\link[base:gsub]{base:gsub}}. Preferably do no use.
#' 
#' @examples
#' test_emoticon <- c(":)", ":(", ";)", "*_*", ":P", "O_o", "a   b")
#' normalizza_emoticon(test_emoticon, perl = T)

normalizza_emoticon <-
  function(testo, perl = TRUE){
    testo <- gsub("([:=8]([- '])?[])Dd>]+)|(\\^[-_o]?\\^)",
                  "emoticon_good", 
                  testo, 
                  perl = perl)
    testo <- gsub("([:=]([- '])?[(|/x*[])|([>Xx#][._][>Xx<#])|(\\):)",
                  "emoticon_bad", 
                  testo, 
                  perl = perl)
    testo <- gsub(";-?[])>Ddo]",
                  "emoticon_wink", 
                  testo)
    testo <- gsub("\\*[-._o]\\*",
                  "emoticon_amazed", 
                  testo)
    testo <- gsub("([:=]-?[pP]+)|(\\b[xX][dD]+\\b)|(\\bd:\\b)",
                  "emoticon_joke", 
                  testo, 
                  perl = perl)
    testo <- gsub("[0Oo]+[\\._-]+[0Oo]+",
                  "emoticon_shock", 
                  testo, 
                  perl = perl)
    testo <- gsub("[[:blank:]]+",
                  " ", 
                  testo, 
                  perl = perl) # substitute multiple blank spaces with a single blank space
    testo
  }



### FILE: normalizzacaratteri.R
#' normalizzacaratteri
#'
#' \code{normalizzacaratteri} replaces escape and punctation codes in \code{testo} with a blank.
#'  
#' @param testo a set of texts to be stripped of escape and punctation codes
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @return a set of processed texts
#' @author Livio Finos
#' @examples 
#'
#'  testo<-c("\t","\r") 
#'  normalizzacaratteri(testo)
#'  
#'  

normalizzacaratteri <- function(testo,fixed=TRUE){
  
 	testo <- gsub("\001" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\002" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\003" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\004" ," ", testo, fixed=fixed, useBytes=FALSE)			
	testo <- gsub("\005" ," ", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\006" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\007" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\008" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\009" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\010" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\011" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\012" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\013" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\014" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\015" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\016" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\017" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\018" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\019" ," ", testo, fixed=fixed, useBytes=FALSE)																
	testo <- gsub("\020" ," ", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\021" ,"!", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\022" ,'"', testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\023" ,' ', testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\027" ,"'", testo, fixed=fixed, useBytes=FALSE)		
	testo <- gsub("\030" ,"'", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\031" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\032" ,"'", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\033" ," ", testo, fixed=fixed, useBytes=FALSE)		
	testo <- gsub("\034" ,"'", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\035" ,"'", testo, fixed=fixed, useBytes=FALSE)
  testo <- gsub("“" ,"'", testo, fixed=fixed, useBytes=FALSE)
  testo <- gsub("”" ,"'", testo, fixed=fixed, useBytes=FALSE)

 	
 
   
	testo <- gsub("\n"," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\r"," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\t"," ", testo, fixed=fixed, useBytes=FALSE)
   
 	
  testo
}


### FILE: normalizzahtml.R
#' normalizzahtml
#'
#' \code{normalizzahtml} replaces links and URLs in \code{testo} with the keyword WWWURLWWW
#' 
#' @param testo A set of texts to be preprocessed.
#' @return A set of texts where URLs are replaced with the keyword WWWURLWWW.
#' @param fixed Logical. If \code{TRUE}, pattern is a string to be matched as is. 
#' Overrides all conflicting arguments.
#' @param perl Logical. If \code{TRUE}, Perl-compatible regexes are used.
#' @author Livio Finos
#' @examples 
#' 
#'  testo<-c("http:textwiller.com","www.textwiller.com") 
#'  normalizzahtml(testo)
#'  
#'  

normalizzahtml <-
function(testo,perl=TRUE,fixed=TRUE){
	
	# fonte: http://htmlhelp.com/reference/html40/entities/special.html

  testo <- gsub("https.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("http.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("www.*"," WWWURLWWW ",testo,perl=perl)
  
  
  testo <- gsub("&quot;",'"',testo)
  testo <- gsub("&amp;","&",testo)
  testo <- gsub("&lt;","<",testo)
  testo <- gsub("&gt;",">",testo)
  testo <- gsub("&circ;","^",testo)
  testo <- gsub("&tilde;","~",testo)	
  testo <- gsub("&lsquo;","'",testo)
  testo <- gsub("&rsquo;","'",testo)
  testo <- gsub("&ldquo;",'"',testo)	




#testo <- gsub("\n"," ",testo,fixed=TRUE)
#testo <- gsub("\t"," ",testo,fixed=TRUE)
#testo <- gsub('\\\"','"',testo,perl=TRUE)	


testo <- gsub("[[:blank:]]+"," ",testo, perl=perl)
testo
	}


### FILE: normalizzapunteggiatura.R
#' normalizzapunteggiatura
#'
#' \code{normalizzapunteggiatura} Removes escape characters and punctation from a set of texts.
#' 
#' @param testo A set of texts to be stripped of escape characters and punctation.
#' @param removeUnderscore  Logical. If \code{TRUE}, removes underscore characters ("_") from texts.
#' @param fixed Logical. If \code{TRUE}, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param perl Logical. If \code{TRUE}, Perl-compatible regexes are used.
#' @return A set of processed texts.
#' @author Livio Finos
#' @examples 
#'
#'  testo<-c(testo<-c("@ ","@retweet","# ","#ciao")) 
#'  normalizzapunteggiatura(testo)
#'  
#'  

normalizzapunteggiatura <-
function(testo,removeUnderscore=TRUE, perl=TRUE,fixed=TRUE){
	
	testo <- paste(" ",testo," ", sep="")
	testo <- gsub("#\\s+", "#", testo, perl=perl)
	testo <- gsub("#", " #", testo, perl=perl)
	testo <- gsub("@\\s+", "@", testo, perl=perl)	
	testo <- gsub("@", " @", testo, perl=perl)
	
	testo <- gsub("\n",' ',testo,fixed=fixed)
	testo <- gsub("\t",' ',testo,fixed=fixed)
	testo <- gsub("\r",' ',testo,fixed=fixed)
	testo <- gsub("\\s+", " ", testo, perl=perl)	
  
#	testo <- gsub('^( ["][@]| [@]| \034[@])',' RT @', testo, perl=perl)	
#	testo <- gsub("^( ['][@])",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT: @)",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT '@)",' RT @', testo, perl=perl)			
	
  
  testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	
  
  testo <- gsub('[///]+',' ', testo, perl=perl)
	testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	testo <- gsub('[//|]+',' ', testo, perl=perl)
	testo <- gsub("'"," ",testo,fixed=fixed)
	testo <- gsub("’"," ",testo,fixed=fixed)
	testo <- gsub("‘"," ",testo,fixed=fixed)
	testo <- gsub('"',' ',testo,fixed=fixed)
	testo <- gsub('\uc294|\uc293|\ucc8f|\ucc8e|\ucc8b|\ucbb6|\ucbb5|\ucbae|\ucb9d|\ucaba',' ',testo)
	testo <- gsub('\uc2bb|\uc2ab',' ',testo,fixed=fixed)
	testo <- gsub('\uc291|`|\uc292',' ',testo)	
	testo <- gsub('\uc285',' ',testo,fixed=fixed)	
	testo <- gsub("\\\\"," ",testo)
	testo <- gsub("\\/"," ",testo)
	testo <- gsub("[=]"," ",testo,perl=perl)
	testo <- gsub("[+]"," ",testo,perl=perl)
	testo <- gsub("[-]"," ",testo,perl=perl)
	testo <- gsub('\\*{1}',' ', testo, perl=perl)
	testo <- gsub('«',' ', testo, perl=perl)
	testo <- gsub('»',' ', testo, perl=perl)
	testo <- gsub("[<]"," ",testo,perl=perl)
	testo <- gsub("[>]"," ",testo,perl=perl)
	testo <- gsub("[~]"," ",testo,perl=perl)
	testo <- gsub("[\\^]"," ",testo,perl=perl)
	testo <- gsub("[,]"," ",testo,perl=perl)
	testo <- gsub("[;]"," ",testo,perl=perl)
	testo <- gsub("[:]"," ",testo,perl=perl)
	testo <- gsub('[\\.]',' ', testo, perl=perl)
	testo <- gsub('…',' ', testo, perl=perl)
	testo <- gsub("[&]"," & ",testo,perl=perl)
	# parentesi
	testo <- gsub("[(]"," ",testo,perl=perl)
	testo <- gsub("[)]"," ",testo,perl=perl)
	testo <- gsub("[{]"," ",testo,perl=perl)
	testo <- gsub("[}]"," ",testo,perl=perl)
	testo <- gsub("[[]"," ",testo,perl=perl)
	testo <- gsub("[]]"," ",testo,perl=perl)

  if(removeUnderscore) 
    testo=gsub("_", " ",testo, perl=perl)
    
	testo <- gsub("\\s+", " ", testo, perl=perl)
testo
	}


### FILE: normalizzaslang.R
#' normalizzaslang
#' 
#' \code{normalizzaslang} replaces Italian slang expressions in \code{testo} with
#' a corresponding keyword. 
#' 
#' The keyword can be the normalized word itself (e.g., nn = non)
#' or an emoticon (e.g., zzz = EMOTEZZZ).
#' 
#' @param testo A set of texts to be normalized.
#' @param perl logical. If \code{TRUE}, Perl-compatible regexes are used.
#' @return A set of texts where Italian slang expressions are replaced with a keyword.
#' @author Livio Finos
#' @examples 
#' 
#'  testo<-c("grandissima","aaa","nun")
#'  normalizzaslang(testo)
#'  
#'  


normalizzaslang <-function(testo,perl=TRUE){
	testo <- gsub(" (#?zz+|#?u+ff[aif]+?|#?r+o+n+f+|#uff|ronf) "," EMOTEZZZ ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?sii+|#si+|#?yes+|#?s\uc38c\uc38c+) "," EMOTESIII ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?noo+|#no+|#?nuu+) "," EMOTENOOO ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?ahh+) "," EMOTEAHHH ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?ehh+) "," EMOTEEHHH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?o?h?o+h[oh]+) "," EMOTEOHOH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEIHIH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEUHUH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?a?h?ah[^h][ah]+) "," EMOTEAHAH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?e?h?eh[^h][eh]+) "," EMOTEEHEH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?azz+) "," EMOTEAZZ ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?[dv]aii+|#[dv]ai|forzaa+) "," EMOTEDAIII ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?cazz[oi]+) "," cazzo ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?cazzat[a]+) "," cazzata ",testo, perl=perl, ignore.case=TRUE)  
	testo <- gsub(" (#?merd[a]+) "," merda ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub("ca\\*\\*o|c\\*\\*\\*+o","cazzo",testo, perl=perl)
	testo <- gsub(" (#?aaa+) "," EMOTEAAA ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?ooo+) "," EMOTEOOO ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?eee+) "," EMOTEEEE ",testo, perl=perl, ignore.case=TRUE)		
	testo <- gsub(" (#?l+o+l+|#?r+o+f+t+l+) "," EMOTELOL ",testo, perl=perl, ignore.case=TRUE)		
	testo <- gsub(" (#?aiutoo+|#?sos|#?help+) "," EMOTESOS ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#ba+sta+|ba+staa+) "," EMOTEBASTA ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?([uw]+[ao]+[uw]+) "," EMOTEWOW ",testo, perl=perl, ignore.case=TRUE)		
	
	testo <- gsub(" (#?grande[e]+|#?grandi[i]+) "," grandeee ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(grand(issim)[aeoi]+) "," grandissimo ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(brav[aeoi]+) "," bravo ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(brav(issim)[aeoi]+) "," bravissimo ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(bell[aeoi]+) "," bello ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(mit[t]ic([aio]|he)+) "," mitico ",testo, perl=perl, ignore.case=TRUE)			
	testo <- gsub(" #?(graziee+) "," grazieee ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(stronz[oaie]+) "," stronzooo ",testo, perl=perl, ignore.case=TRUE)	
		
	# ALTRO		
	testo <- gsub(" perch[\uc3a9e\uc3a8] "," perch\uc3a9 ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" x(ch|k)[\uc3a9e\uc3a8] "," perch\uc3a9 ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" nn "," non ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" nun "," non ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("o+","o",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("i+","i",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("e+","e",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("u+","u",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("a+","a",testo, perl=perl, ignore.case=TRUE)
		testo		
	}


### FILE: normalizzaTesti.R
#' Text normalization functions
#' 
#' Normalization of various characteristics of texts for analysis.
#' 
#' \code{stopwords_ita} is a list of Italian stopwords.
#' 
#' @aliases normalizzaTesti normalizzacaratteri normalizzaemoticon normalizzahtml
#' normalizzaslang normalizzapunteggiatura tryTolower stopwords_ita
#' removeStopwords preprocessingEncoding
#' @param testo Character vector of texts.
#' @param tolower \code{TRUE} by default.
#' @param normalizzahtml \code{TRUE} by default.
#' @param normalizzacaratteri \code{TRUE} by default.
#' @param normalizza_emoticon \code{TRUE} by default.
#' @param normalizzapunteggiatura \code{TRUE} by default.
#' @param normalizzaslang \code{TRUE} by default.
#' @param fixed vedi \code{\link[base:gsub]{base:gsub}}. Preferably do not use.
#' @param perl vedi \code{\link[base:gsub]{base:gsub}}. Preferably do not use.
#' @param preprocessingEncoding Logical.
#' @param encoding \code{"UTF-8"} by default. If \code{FALSE}, it avoids conversion.
#' @param sub Character string. If not NA, it is used to replace any
#' non-convertible bytes in the input. See also parameter \code{sub} in
#' function \code{iconv}.
#' @param contaStringhe Strings to count in documents. Default: \code{
#' c("\\?","\\!","#","@", "(€|euro)","(\\$|dollar)","SUPPRESSEDTEXT")}
#' @param suppressInvalidTexts Substitutes strings with invalid multibytes with
#' \code{"SUPPRESSEDTEXT"}, as they would likely produce errors in subsequent
#' normalizations. Default \code{TRUE}.
#' @param removeUnderscore Remove underscores?
#' @param ifErrorReturnText What to return for texts with a wrong encoding.
#' @param stopwords List of words to exclude form analysis. \code{stopwords_ita} by default.
#' @param verbatim Shows statistics during the process. \code{TRUE} by default.
#' @param remove \code{TRUE} by default. A vector of words to be removed.
#' @return For \code{normalizzaTesti} the output is the vector of normalized texts. 
#' The counts table in \code{contaStringhe} is assigned as \code{counts} table in 
#' the vector's \code{attributes}. For all other functions, the output is a \code{vector} 
#' of the same length as \code{testo} but with normalized texts.
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos, Maddalena Branca, Mattia Da Pont
#' @keywords ~kwd1 ~kwd2
#' @examples
#' testoNorm <- normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
#' testoNorm
#' attr(testoNorm,"counts")
#' 
#' @export normalizzaTesti
#' @export normalizza_emoticon
#' @export normalizzacaratteri
#' @export normalizzahtml
#' @export normalizzapunteggiatura
#' @export normalizzaslang
#' @export preprocessingEncoding
#' @export tryTolower 
#' @export removeStopwords

normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE,
                            normalizzacaratteri=TRUE,
                            normalizza_emoticon=TRUE,
                            normalizzapunteggiatura=TRUE,
                            normalizzaslang=TRUE,
                            fixed=TRUE,perl=TRUE,
                            preprocessingEncoding=TRUE, encoding="UTF-8", sub="",
                            contaStringhe=c("\\?","\\!","@","#",
                                            "(\u20AC|euro)","(\\$|dollar)",
                                            "SUPPRESSEDTEXT"),
                            suppressInvalidTexts=TRUE,
                            verbatim=TRUE, remove=TRUE,removeUnderscore=FALSE){
  Sys.setlocale("LC_ALL", "")
  if(preprocessingEncoding) testo<-preprocessingEncoding(testo,
                                                         encoding=encoding,
                                                         sub=sub,
                                                         suppressInvalidTexts=suppressInvalidTexts,
                                                         verbatim=verbatim)
  #######################
  # PREPROCESSING #
  #######################
  # aggiunta spazi per preprocessing
  testo <- paste(" ",testo," ",sep="")
  # normalizza encoding
  if(normalizzacaratteri) testo <- normalizzacaratteri(testo,fixed=fixed)
  # pulizia testo preliminare (html)
  if(normalizzahtml) testo <- normalizzahtml(testo)
  if(!is.null(contaStringhe))
    conteggiStringhe=.contaStringhe(testo,contaStringhe) else
      conteggiStringhe=NULL
  # identifica emoticon
  #source(paste(functiondir,"/normalizza_emoticon.R",sep=""), .GlobalEnv)
   
  if(normalizza_emoticon) testo <- normalizza_emoticon(testo,perl=perl)
  # pulizia punteggiatura
  #source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
  if(normalizzapunteggiatura) testo <- normalizzapunteggiatura(testo,perl=perl,fixed=fixed,removeUnderscore=removeUnderscore)
  # normalizza slang
  #source(paste(functiondir,"/normalizzaslang.R",sep=""), .GlobalEnv)
  if(normalizzaslang) testo <- normalizzaslang(testo,perl=perl)
  # tolower
  if(tolower) testo <- tryTolower(testo,ifErrorReturnText=TRUE)
  if(is.null(remove)) remove=TRUE
  if(is.character(remove)) {
    testo <- removeStopwords(testo,remove)
    } else {
      if(remove) {
        data(stopwords_ita)
        testo <- removeStopwords(testo,stopwords_ita)
      }
    }
  testo <- gsub("\\s+", " ", testo, perl=perl)
  testo <- .togliSpaziEsterni(testo)
  attr(testo,"counts")=conteggiStringhe
  testo
}


### FILE: patternExtract.R
patternExtract <- function(testo, pattern="@( *)\\w+", id = names(testo)){
  
  if(is.null(id)) id=1:length(testo)
     pattern <- str_extract_all(testo, pattern)
  names(pattern)=id
  
  pattern=pattern[sapply(pattern,length)>0]
  db.pattern=lapply(1:length(pattern),function(i){ rbind(id=names(pattern)[i],pattern=unlist(pattern[i]))})
  db.pattern=data.frame(t(data.frame(db.pattern)))
  rownames(db.pattern)=NULL
  return(db.pattern)	
}


### FILE: preprocessingEncoding.R
preprocessingEncoding <- function(testo, 
                                  encoding="UTF-8",
                                  suppressInvalidTexts=TRUE,
                                  verbatim=TRUE, sub = ""){
   if(is.logical(encoding)) {
    if(encoding) {encoding="UTF-8"  
    } else if(encoding=="") encoding="UTF-8"  
  }

  if(is.logical(encoding)&&(!encoding)){  
    #do nothing
    } else     {
      encs=unique(Encoding(testo))
      encs[encs=="unknown"]=""
      for(i in encs){
        idi=(Encoding(testo)==i)
        testo[idi]=iconv(testo[idi],from=i,to = encoding, sub = "")
      }
    }
  
  #  elimina testi con encoding non riconoscibile:
  if(suppressInvalidTexts){
    droptxt=sapply(testo,function(txt) is(try(nchar(txt),silent=TRUE),"try-error"))
    testo[droptxt]="SUPPRESSEDTEXT"
    if(verbatim&(sum(droptxt)>0)) warning(paste(sum(droptxt)," documenti eliminati (",
                     round(mean(droptxt)*100,3),"%) a causa di multibyte invalidi.\n",sep=""))
  }
  testo
}


### FILE: removeStopwords.R
removeStopwords <- function(testo, stopwords=itastopwords){
  testo <- strsplit(testo, "\\s")
  testo <- lapply(testo, function(x) ifelse((x %in% stopwords | nchar(x)<=1),"", x) )
  testo <- unlist(lapply(testo, function(x) paste(x, collapse=" ")))
#   testo <- paste(" ",testo," ", sep="")
#   testo <- gsub("\\s+", " ", testo, perl=FALSE)
#   testo <- gsub("^[[:blank:]]","",testo)
#   testo <- gsub("[[:blank:]]$","",testo)
  testo
}



### FILE: RTHound.R
#' RTHound
#' 
#' Identifies the most frequent retweets through hierarchical clustering on
#' the Levenshtein distance (dissimilarity) matrix.
#' 
#' \code{RTHound} divides \code{testo} in subsets of length \code{S} (from the
#' second subset it also incorporates \code{L} tweets of the previous subset).
#' The function calculates a dissimilarity matrix based on Levenshtein distance for each
#' subset and groups tweets through a hierarchical clustering algorithm.
#' 
#' @param testo Tweets or generic text vector.
#' @param S Number of tweets (or texts) for each subset. \code{500} by
#' deafault.
#' @param L Number of tweets (or texts) belonging to the previous subset to
#' embed in subset analysis. \code{100} by default.
#' @param hclust.dist Numeric scalar containing the height at which the trees should be cut.
#' \code{100} by deafault.
#' @param hclust.method The agglomeration method to be used. This should be an
#' unambiguous abbreviation chosen among \code{"ward"}, \code{"single"},
#' \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or
#' \code{"centroid"}. \code{"complete"} by default.
#' @param showTopN Number of most frequent retweets to dispaly. \code{5} by
#' deafault.
#' @param dist \code{"levenshtein"} by default. The other - quicker - accepted value
#' is \code{"profile"}.
#' @param verbatim Logical.
#' @return \code{RTHound} replaces the tweets belonging to the same cluster with
#' the oldest, identifying them as retweets, and returns a list of the most
#' frequent retweets (\code{top}).
#' @note %% ~~further notes~~
#' @author Federico Ferraccioli, Livio Finos
#' @seealso \code{hclust}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{
#'  testo=c(
#'  "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "RT @orianoPER: La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/" ,
#'  "La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/3MwubXIH8g",
#'  "RT @orianoPER: La #ricerca in #Medicina con #sperimentazioneanimale non e' predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE http://t",
#'  "RT @HuffPostItalia: Il Governo italiano non fermi la sperimentazione animale. Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "RT @HuffPostItalia: \"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "\"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "RT @orianoPER: @EnricoLetta LA #VIVISEZIONE NON SERVE: PAROLA DI GLAXO-APTUIT http://t.co/mtsHJjDIvu #StopVivisection #SperimentazioneAnima&")
#'  
#'  testo=RTHound(testo, S = 3, L = 1, 
#'                  hclust.dist = 100, hclust.method = "complete",
#'                  showTopN=3)
#' 
#' }
#' 
#' @export RTHound
RTHound=function(testo, S = 500, L = 100, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=5, dist="levenshtein",verbatim=TRUE){ 
  testo=iconv(testo,to="UTF-8")
  testo=gsub("^( *)(RT|rt|Rt)( *)","",testo)
  testo.na=which(is.na(testo))
  ntesti=length(testo)
  if(is.null(names(testo))) names(testo)=1:length(testo)
  if(length(testo.na)>0) testo=testo[-testo.na]
  ntestiNA=length(testo)
  
  #   if(dist=="profile") {
  #     if(verbatim) cat("\n Making profile matrix..")
  #     profile=make.profile(testo)
  #   }
  #     
  nWindows=(floor(ntestiNA/S)-1)
  if(verbatim) cat("\n There will be ",nWindows, " sliding windows:")
  s=c(0:nWindows)
  for(l in 1:length(s)) {
    if(verbatim) cat("\nWindow #", l)
    if(l<length(s))  
      select=c(((S)*s[l]+1):((S)*s[l+1])) else
    if(l==length(s))  
      select=((S)*s[l]+1):length(testo)
    
    if(l>1)   {     
      selectPeriodoPrima=((S)*s[l]-(L+1)):((S)*s[l]-1)
      select=c(selectPeriodoPrima,select)
    }
#     m=matrix(ncol=length(select),nrow=length(select))          
#     for(i in 1:(length(select)-1))  {
#       for(j in (i+1):length(select)){
#         m[i,j]=levenshteinDist(testo[i],testo[j])
#       }
#     }
#     m= as.dist(t(m))                            
    if(dist=="levenshtein")
      m= as.dist(adist(testo[select]))
    if(dist=="profile")  
      m= dist(make.profile(testo[select]))


    h=hclust(dist(m),method=hclust.method)
    tree=cutree(h,h=hclust.dist)
    idClusters=sapply(unique(tree), function(x) which(tree==x))
    
    for (i in 1:length(idClusters))
      testo[names(idClusters[[i]])]=testo[names(idClusters[[i]])[1]]
  }
  
  if(showTopN>0) {
    cat("\n",showTopN," most frequent RTs:")
    out=sort(table(testo),decreasing=TRUE)[1:showTopN]
    cat("\n",
      paste("(fr ",out,") ",names(out),sep="","\n")
      )
  }
  
  if(length(testo.na)>0){
    testoOut=rep("",ntesti)
    testoOut[-testo.na]=testo
    testo=testoOut
  }
  return(testo)
}

####### util for profile-based distance
make.profile <- function(testo){
  split=strsplit(testo,"")
  profileNames=table(unlist(split))
  split=sapply(split,function(x)factor(x,levels=names(profileNames)))
  profile=t(sapply(split,function(x)table(x)))
  as.matrix(profile)
}


### FILE: run_app.R
#' Run TextWiller Shiny Application
#' 
#' Launches the TextWiller Shiny application for interactive text analysis.
#' 
#' @param ... Additional parameters passed to \code{shiny::runApp}
#' @export
run_app <- function(...) {
  app_dir <- system.file("apps/textwiller_app", package = "TextWiller3")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `TextWiller3`.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, ...)
}


### FILE: sentiment_utils.R
.sentiment.mattivio<- function(text, vocabularies,...){
  # https://www.tidytextmining.com/ngrams.html
  # 
  vocabolarioMattivio$keyword=gsub("-"," ", vocabolarioMattivio$keyword)
  vocabolarioMattivio$keyword=gsub("^ ","", vocabolarioMattivio$keyword)
  vocabolarioMattivio$keyword=gsub(" $","", vocabolarioMattivio$keyword)
  vocabolarioMattivio$keyword=gsub("  "," ", vocabolarioMattivio$keyword)
  vocabolarioMattivio$ngram=sapply(vocabolarioMattivio$keyword, function(x) length(strsplit(x,split = " ")[[1]]))
  # table(vocabolarioMattivio$ngram)
  # save(vocabolarioMattivio,file="vocabolarioMattivio.rda")


    # vocabolarioMattivio=x[!is.na(x$score),]
  # vocabolarioMattivio$keyword=tolower(vocabolarioMattivio$keyword)
  # vocabolarioMattivio$keyword=gsub(" $","",vocabolarioMattivio$keyword)
  # tt=bind_rows(
  #   data.frame(keyword=vocabolariMadda$positive,code=NA,score=1),
  # data.frame(keyword=vocabolariMadda$negative,code=NA,score=-1))
  # vocabolarioMattivio=bind_rows(vocabolarioMattivio,tt)
  
  textMio<-tibble::tibble(keyword=text,id=1:length(text))
  
  tidy.text <- tidytext::unnest_tokens(tbl = textMio,output = keyword, input = keyword)
  tidy.text$keyword <- wordStem(tidy.text$keyword, language = "italian")
  tidy.text$keyword <- gsub("issim","",tidy.text$keyword)
  
  #useful for ngrams later
  if(sum(unique(vocabolarioMattivio$ngram)!=1)>0)
    textMio$keyword=sapply(unique(tidy.text$id),function(x) paste(tidy.text$keyword[tidy.text$id==x],collapse=" "))

  tt=dplyr::left_join(tidy.text,vocabularies[vocabularies$ngram==1,],"keyword")
  tt=tt[!is.na(tt$score),,with=FALSE]
  tt=tt %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(score = sum(score))
  out=rep(0,length(text))
  out[tt$id]=tt$score
  
  ## n-grams
  ngrams=unique(vocabularies$ngram)
  ngrams=ngrams[ngrams!=1]
  for( n in ngrams){
      
      # STEM ME PLEASE!!
    # tidy.text <- tidytext::unnest_tokens(tbl = textMio,output = keyword, input = keyword)
    # tidy.text$keyword <- wordStem(tidy.text$keyword, language = "italian")
    # tidy.text$keyword <- gsub("issim","",tidy.text$keyword)
    
      tidy.text <-  tidytext::unnest_tokens(tbl = textMio,
                                          output = keyword, input = keyword,token = "ngrams", n = n)
    
    tt=dplyr::left_join(tidy.text,vocabularies[vocabularies$ngram==n,],"keyword")
    tt=tt[!is.na(tt$score),,with=FALSE]
    if(nrow(tt)>0){  
      tt=tt %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(score = sum(score))
      out[tt$id]=out[tt$id]+tt$score
      }  
    }
  return(as.array(as.vector(out)))  
  
}

.sentiment.maddalena<- function(text, vocabularies,...){
  #####################################################sentiment analisys
  
# sfInit(par=T,cp=1) #cp = 10 calculus, devo metterci il numero di processori 
# sfExport("text", "pos.words", "neg.words") 
# sfLibrary(stringr)
#sfLibrary(Snowball)
# sfLibrary(SnowballC)
#sfLibrary(RWeka)
.get.scores <- function(txt, pos.words, neg.words) {
  # tokenaizer parole
  word.list = str_split(txt, '\\s+')  
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  words = wordStem(words, language = "italian")
  # compara le parole nei dizionari
  pos_matches =match (words, pos.words)
  neg_matches =match (words, neg.words)
  ##bgrammi
  pos_matches2<-apply(as.array(cbind(words,c(words[-1],NA))),1,function(x)match(paste(x[1],"-",x[2],sep=""),pos.words))[-c(length(words))]
  neg_matches2<-apply(as.array(cbind(words,c(words[-1],NA))),1,function(x)match(paste(x[1],"-",x[2],sep=""), neg.words))[-c(length(words))]
  
  pos_matches3<-apply(as.array(cbind(words,c(words[-1],NA),c(words[-c(1:2)],NA,NA))),1,function(x)match(paste(x[1],"-",x[2],"-",x[3],sep=""), pos.words))[-c((length(words)-1):length(words))]
  neg_matches3<-apply(as.array(cbind(words,c(words[-1],NA),c(words[-c(1:2)],NA,NA))),1,function(x)match(paste(x[1],"-",x[2],"-",x[3],sep=""), neg.words))[-c((length(words)-1):length(words))]
  
  pos_matches <-c(pos_matches ,pos_matches2,pos_matches3)
  neg_matches<-c(neg_matches,neg_matches2,neg_matches3)
  # TRUE/FALSE:
  pos_matches = !is.na(pos_matches)
  neg_matches = !is.na(neg_matches)
  
  p<-sum(pos_matches) #-sum(p_neutral_matches)
  n<-sum(neg_matches) #-sum(n_neutral_matches)
  score =  p - n 
  return(as.array(as.vector(score)))  
}
scores = sapply(text, .get.scores, vocabularies$positive, vocabularies$negative)
return(scores)
}


### FILE: sentiment.R
#' Sentiment analysis
#' 
#' Assigns a sentiment score to each text in \code{text}.
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda vocabolarioMattivio
#' @param text Vector of texts.
#' @param algorithm \code{"Mattivio"} by default, or \code{"Maddalena"}, or a function returning the sentiment score.
#' @param vocabularies \code{vocabolarioMattivio} by default if \code{algorithm == "Mattivio"}); 
#' \code{vocabolariMadda} by default if \code{algorithm == "Maddalena"}), or an object used by the algorithm.
#' @param normalizzaTesti \code{TRUE} by default.
#' @param get_labels \code{TRUE} by default. If \code{FLASE}, gives the quantitative score; 
#' if \code{TRUE}, provides the labels -1, 0, +1 (i.e. \code{sign(score)}).
#' @return An array containing a single numerical value for each element of \code{text}.
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Mattia Da Pont, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "mi piaci", "wow!!","good","casa", "farabutto!","ti odio"))
#' 
#' @export sentiment
#' 
sentiment <- function(text, algorithm="Mattivio", 
                      vocabularies=NULL,
                      normalizzaTesti=TRUE, get_labels=TRUE,...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Mattivio"
    if(is.null(vocabularies)) {
      if(algorithm=="Mattivio") {
        data(vocabolarioMattivio) 
        vocabularies=vocabolarioMattivio
        } else if(algorithm=="Maddalena") {
          data(vocabolariMadda) 
          vocabularies=vocabolariMadda
        }
    }
    
    if(normalizzaTesti==TRUE)
      text<-normalizzaTesti(text,suppressInvalidTexts=FALSE,contaStringhe=NULL,...)
    
    #choose and perform algorithm
    if(is.function(algorithm)) {
      sent=algorithm(text=text, vocabularies=vocabularies,...)
      return(sent)} else 
        if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      
    } else if(algorithm=="Mattivio") {
      sent=.sentiment.mattivio(text=text, vocabularies=vocabularies,...)
    } else sent=NULL
    
    if(get_labels)
      return(sign(sent)) else
        return(sent)
    
  } #end if(!is.null(text))
}



### FILE: shorturl2url.R
shorturl2url <- function(testo,id=names(testo)){
  if(is.null(id)) {
    id=1:length(testo) 
    names(testo)=id
  }
  db.urls=urlExtract(testo,id=id)
  
  for(i in 1:nrow(db.urls)){
    testo[db.urls$id[i]]=gsub(db.urls$shorturl[i],db.urls$url[i],testo[db.urls$id[i]])
  }
  testo
}


### FILE: TextWiller-package.R
#' A package for text mining, specially devoted to the italian
#' language
#' 
#' A package for text mining, specially devoted to the italian
#' language
#' 
#' \tabular{ll}{ Package: \tab TextWiller\cr Type: \tab Package\cr Version:
#' \tab 2.0\cr Date: \tab 2016-05-12\cr License: GPL (>= 2) }
#' 
#' @name TextWiller-package
#' @aliases TextWiller-package TextWiller
#' @docType package
#' Author: Andrea Sciandra, Dario Solari, Mattia Da Pont, Livio Finos (con contributi di Mattia Uttini, Marco Rinaldo, Maddalena Branca,
#' Federico Ferraccioli, Marco Rinaldo, Matteo Redaelli).
#' Maintainer: Livio Finos <livio.finos@@unipd.it>
#' @keywords package
#' @examples
#' 
#'  \dontrun{# install.packages("devtools") # if you don't already have it.
#' library(devtools)
#' install_github("livioivil/TextWiller")
#' library(TextWiller)
#' }
#' 
#' 
#' ### normalize texts
#' normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
#' 
#' # get the sentiment of a document
#' sentiment(c("ciao bella!","farabutto!","fofi sei figo!"))
#' 
#' 
#' # Classify users' gender by (italian) names
#' classificaUtenti(c('livio','alessandra','andrea'))
#' classificaUtenti(c('alessandroBianchi', 'mariagiovanna', 'corriereDelMezzogiorno'), scan_interno=T)
#' # and classify location
#' data(vocabolarioLuoghi)
#' classificaUtenti(c('Bosa','Pordenone, Italy','Milan'),vocabolarioLuoghi)
#' 
#' 
#' # find re-tweets (RT) by texts similarity:
#' data(TWsperimentazioneanimale)
#' RTHound(TWsperimentazioneanimale[1:10,"text"], S = 3, L = 1, 
#'                  hclust.dist = 100, hclust.method = "complete",
#'                  showTopN=3)
#' 
#' 
#' #extract short urls and get the long ones
#' ## Not run: urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")
#' 
#' #extract short urls and get the long ones
#' \dontrun{urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")}
#' 
#' #extract users:
#' ## Not run: patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\w+")
#' 
#' 
NULL
#' Funzioni di gestione delle date
#' 
#' Funzioni di gestione delle date
#' 
#' aggiungere dettagli qui
#' 
#' @aliases fixTimeStamp selezionaIntervalloTimeStamp
#' @param db data.frame contenente i tweets.
#' @param timeRange due valori di tipo data indicanti inizio e fine.
#' @param campoData "created" e "ts" sono due campi data del db estratto da
#' dump_twitter.R
#' @return l'output e' db "aggiustato"
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{TW=fixTimeStamp(TW)}
#'  \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
#'  \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}
#' 
#' 
#' 
#' Funzioni di gestione delle date
#' 
#' Funzioni di gestione delle date
#' 
#' aggiungere dettagli qui
#' 
#' @aliases fixTimeStamp selezionaIntervalloTimeStamp
#' @param db data.frame contenente i tweets.
#' @param timeRange due valori di tipo data indicanti inizio e fine.
#' @param campoData "created" e "ts" sono due campi data del db estratto da
#' dump_twitter.R
#' @return l'output e' db "aggiustato"
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{TW=fixTimeStamp(TW)}
#'  \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
#'  \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}
#' 
#' 
#' 




### FILE: tryTolower.R
tryTolower <-
function(testo, ifErrorReturnText=FALSE)
{
   # tryCatch error
   try_error = tryCatch(tolower(testo), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
      testo = tolower(testo) else testo = NA
   testo
}


### FILE: urlExtract.R
#' Extrects regexes (users, hashtags) and short URLs.
#' 
#' \code{patternExtract} extracts patterns contained in \code{testo}.
#' \code{urlExtract} extracts and converts the short URLs contained in \code{testo}
#' to URLs. \code{shorturl2url} substitutes the short URLs in \code{testo} with URLs.
#' 
#' 
#' @aliases urlExtract shorturl2url patternExtract
#' @param testo Vector (potentially with names) of texts containing short URLs.
#' @param pattern Text string to find and extract. \code{"@\\w+"}
#' (default) extracts references to a user in tweets. \code{"#\\w+"} extracts hashtags.
#' @param id If \code{testo} is a vector containing names, these are used as IDs. 
#' Otherwise, IDs are consecutive numbers from 1 to \code{length(testo)}.
#' @return \code{patternExtract} restituisce a \code{data.frame} with columns: id, pattern.
#' \code{urlExtract} returns a \code{data.frame} with columns: id, short URL, URL.
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{
#'  testo=c("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00,See All http://t.co/dbtPJRMl00")
#'  shorturl2url(testo,id=names(testo))
#' urls=urlExtract(testo)
#' patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\w+")
#' }
#' 
#' @export urlExtract 
#' @export shorturl2url 
#' @export patternExtract

urlExtract <- function(testo,id=names(testo)){

  if(is.null(id)) id=1:length(testo)
	urls    <- str_extract_all(testo, "http([[:graph:]]+)|www\\.([[:graph:]]+)")
	
	db.urls <- data.frame(cbind(id=rep(id, unlist(lapply(urls,length)) ),shorturl=unlist(urls)))
	db.urls$id <- as.character(db.urls$id)
	db.urls$shorturl <- as.character(db.urls$shorturl)
	db.urls <- db.urls[which(nchar(db.urls$shorturl)>21),]
	
	db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ],1,22)
	db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ],1,23)
	
  uniqueShorturl <- unique(db.urls$shorturl)
  uniqueUrl <- as.character(sapply(db.urls$shorturl, decode_short_url,USE.NAMES = FALSE))
  for( i in 1:length(uniqueShorturl)){
    db.urls$url[db.urls$shorturl==uniqueShorturl[i]] <- uniqueUrl[i]
  }
	db.urls$url[which(db.urls$url=="NULL")] <- NA
	return(db.urls)	
}


### FILE: utili.R
##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]*","",testo)
  testo <- gsub("[[:blank:]]*$","",testo)
  testo
}

##########################

.contaStringhe  <- function(testo,
                            stringhe=c("\\?","\\!","#","@","(€|euro)","(\\$|dollar)")                            
                            ){
  conteggi=sapply(stringhe,function(stringa) str_count(testo,stringa))
  colnames(conteggi)=paste("Conteggi.",colnames(conteggi),sep="")
  conteggi
}


### FILE: vocabolari.R
#' Vocabolari
#' 
#' TextWiller contains various Italian dictionaries for text analysis. This document 
#' describes their uses based on their primary function: sentiment analysis,
#' text normalization, categorization.
#'
#' @name vocabolari
#' @docType data
#' @references
#' Gupta, S., Singh, A., & Ranjan, J. (2021). Emoji Score and Polarity Evaluation 
#' Using CLDR Short Name and Expression Sentiment. Advances in Intelligent Systems 
#' and Computing, 1009-1016. \url{https://doi.org/10.1007/978-3-030-73689-7_95}\cr
#' 
#' Kralj Novak, P., Smailovic, J., Sluban, B., & Mozetic, I. (2015). Sentiment 
#' of Emojis. PLOS ONE, 10(12), e0144296. \url{https://doi.org/10.1371/journal.pone.0144296}\cr
#' 
#' Katherine Roehrick (2020). vader: Valence Aware Dictionary and sEntiment Reasoner 
#' (VADER). R package version 0.2.1. \url{https://CRAN.R-project.org/package=vader}\cr
#' 
#' Tim Loughran & Bill McDonald. (n.d.). Loughran-McDonald Master Dictionary w/ 
#' Sentiment Word Lists [Dataset]. \url{https://sraf.nd.edu/loughranmcdonald-master-dictionary/}\cr
#' 
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. 
#' \url{http://github.com/trinker/lexicon}\cr
#' 
#' Rinker, T. W. (2021). sentimentr: Calculate Text Polarity Sentiment 
#' version 7.1.2. \url{https://github.com/trinker/sentimentr}
#' 
#' @keywords vocabolari, dizionari
#' 
#' @section Sentiment analysis:
#' 
#' \enumerate{
#' \item \code{dizionario_sentiment_ita}:
#' This dictionary contains 3179 general Italian words and 1853 emojis. \cr
#' Type: data.table.
#' \itemize{
#' \item \code{keyword}: Italian word or English emoji identifier, 
#' the latter in the form "emoji_cldr_short_name". \cr
#' Type: character.
#' \item \code{code}: For emojis, their UTF-8 byte representation in the form <xx><xx><xx>. 
#' For words, its value is NA. \cr
#' Type: character.
#' \item \code{score}: Polarity score. Range = -1, +1 where -1 = negative polarity 
#' and +1 = positive polarity. Words are classified as either negative (-1) or positive (+1).
#' Emojis are classified using the full range of values (with 0 = neutral). 
#' Emoji scores were calulated using Gupta et al.'s (2021) procedure: 
#' 1) Polarity scores for 676 emojis were retreived from  \pkg{lexicon::emojis_sentiment} (Rinker, 2019), 
#' a slightly modified version of Novak et al.'s (2015) emoji sentiment data; 
#' 2) Using \pkg{VADER} (Roehrick, 2020)), an additional polarity score was 
#' calculated for all 1853 emojis through a sentiment analysis of their CLDR short name; 
#' 3) The final polarity score therefore is: the VADER score for emojis that 
#' do not appear in  \pkg{lexicon::emojis_sentiment} (n = 1177); the arithmetic mean 
#' of the \pkg{VADER} and \pkg{lexicon::emojis_sentiment} scores for the remaining emojis (n = 676). 
#' All emojis were retrieved from: \url{http://www.unicode.org/emoji/charts/full-emoji-list.html}.\cr 
#' Type: numeric.
#' \item \code{ngram}: Order of the n-gram appearing in the "keyword" variable, 
#' calculated as the number of words that compose it. \cr
#' Type: integer.}
#' 
#' \item \code{dizionario_sentiment_ita_r}: A version of \code{dizionario_sentiment_ita}
#' compatible with \pkg{sentimentr::sentiment}.\cr
#' Type: data.table.
#' \itemize{
#' \item \code{x}: Word.
#' \item \code{y}: Sentiment score.}
#' 
#' \item \code{dizionario_loughran_ita}: 
#' Italian translation of the Loughran-McDonald dictionary for use with finalcial documents. \cr
#' Type: data.table.
#' \itemize{
#' \item \code{x}: Word.\cr
#' Type: character.
#' \item \code{y}: Polarity score (values: -1 = negative, 0 = uncertain, +1 = positive). \cr
#' Type: numeric.}}
#' 
#' @section Text normalization:
#' 
#' \enumerate{
#' \item \code{dizionario_emoji_id}:
#' This dataset contains 1853 emojis and their respective byte representation. 
#' It is compatible with \pkg{textclean::replace_emoji()} and can be used as a 
#' value for the \code{emoji_dt} argument for text normalization, returning emojis 
#' in a document as their CLDR short name. It represents an expansion of the 
#' \code{lexicon::hash_emoji} dataset.\cr
#' Type: data.table. 
#' \itemize{
#' \item \code{x}: UTF-8 byte representation in the form <xx><xx><xx>. \cr
#' Type: character.
#' \item \code{y}: Emoji identifier in the form "emoji_cldr_short_name". \cr
#' Type: character.}
#' 
#' \item \code{stopwords_ita}: A character list of italian stopwords for use in text normalization.\cr
#' Type: character.}
#' 
#' @section Categorization:
#' 
#' \enumerate{
#' \item \code{dizionario_luoghi}:
#' As a value of the \code{vocabolario} argument in \pkg{TextWiller::classificaUtenti()}, 
#' it categorizes city names contained in a character vector according to their 
#' location ("Estero" for non-Italian cities, "Nord-ovest", "Nord-est". "Centro", 
#' "Sud", "Isole" for Italian cities).
#' 
#' \item \code{dizionario_nomi_propri}:
#' As a value of the \code{vocabolario} argument in \pkg{TextWiller::classificaUtenti()},
#' it categorizes (Italian) first names contained in a character vector as 
#' either "Maschio (male)" or "Femmina (female)".
#' 
NULL


### FILE: zzz_internal_utils.R
#' Internal Utility Functions
#' 
#' Internal helper functions for the TextWiller3 package.
#' Not exported for user access.
#' 

# Internal function to check if required packages are available
.check_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    stop("The following packages are required but not installed: ",
         paste(missing_packages, collapse = ", "),
         "\nPlease install them using install.packages()")
  }
}

# Internal function for safe file operations
.safe_file_op <- function(operation, path, ...) {
  tryCatch({
    operation(path, ...)
  }, error = function(e) {
    warning("File operation failed for ", path, ": ", e$message)
    return(NULL)
  })
}

# Internal function to validate text input
.validate_text <- function(text) {
  if (!is.character(text)) {
    stop("Text must be a character vector")
  }
  
  # Remove NULLs and NAs
  text <- text[!is.null(text) & !is.na(text)]
  
  # Convert to character if factor
  if (is.factor(text)) {
    text <- as.character(text)
  }
  
  return(text)
}

# Internal function for progress reporting
.report_progress <- function(current, total, message = "Processing") {
  if (interactive() && total > 10) {
    if (current %% ceiling(total / 10) == 0) {
      cat(sprintf("\r%s: %d/%d (%.0f%%)", 
                  message, current, total, current/total * 100))
    }
    if (current == total) cat("\n")
  }
}


### FILE: app.R
library(shiny)
library(shinythemes)
library(ggplot2)

# SISTEMA DI CARICAMENTO ULTRA-ROBUSTO
source_modules <- function() {
  cat("=== ROBUST MODULE LOADING ===\n")
  
  # Possibili posizioni dei file
  possible_paths <- c(
    getwd(),  # Current directory
    ".",       # Current directory
    "inst/apps/textwiller_app/",  # Common Shiny app structure
    file.path(getwd(), "inst/apps/textwiller_app/")  # Absolute path
  )
  
  module_files <- c(
    "mod_corpus_io.R",
    "mod_preprocess.R", 
    "mod_exploration.R",
    "mod_lexical_analysis.R",
    "mod_sentiment.R",
    "mod_semantic_analysis.R"
  )
  
  # Trova il percorso corretto
  found_path <- NULL
  for (path in possible_paths) {
    test_file <- file.path(path, module_files[1])
    if (file.exists(test_file)) {
      found_path <- path
      cat("✓ Trovata cartella moduli:", path, "\n")
      break
    }
  }
  
  if (is.null(found_path)) {
    cat("✗ Nessuna cartella moduli trovata! Cercato in:\n")
    for (path in possible_paths) {
      cat("  -", path, "\n")
    }
    return(FALSE)
  }
  
  # Carica i moduli
  cat("\nCaricamento moduli da:", found_path, "\n")
  for (file in module_files) {
    file_path <- file.path(found_path, file)
    if (file.exists(file_path)) {
      cat("  Caricando", file, "...\n")
      tryCatch({
        source(file_path, local = FALSE)
        cat("    ✓ SUCCESSO\n")
      }, error = function(e) {
        cat("    ✗ ERRORE:", e$message, "\n")
      })
    } else {
      cat("  ✗ File non trovato:", file_path, "\n")
    }
  }
  
  cat("=== FINE CARICAMENTO ===\n\n")
  return(TRUE)
}

# Carica i moduli
success <- source_modules()

if (!success) {
  stop("Impossibile caricare i moduli. Controlla la struttura delle cartelle.")
}

# UI PRINCIPALE
# app.R - UI semplificata
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("TextWiller 3.0 - Advanced Text Analysis"),
  
  # NAVIGAZIONE PRINCIPALE - Solo tabs
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    tabPanel(
      "📥 Import Corpus",
      icon = icon("upload"),
      mod_corpus_io_ui("import")
    ),
    
    tabPanel(
      "⚙️ Preprocessing", 
      icon = icon("cog"),
      mod_preprocess_ui("preprocess")
    ),
    
    tabPanel(
      "📊 Exploration",
      icon = icon("chart-bar"),
      mod_exploration_ui("exploration")
    ),
    
    tabPanel(
      "📈 Lexical Analysis",
      icon = icon("chart-line"),
      mod_lexical_analysis_ui("lexical")
    ),
    
    tabPanel(
      "😊 Sentiment Analysis",
      icon = icon("smile"),
      mod_sentiment_ui("sentiment")
    ),
    
    tabPanel(
      "🔍 Semantic Analysis",
      icon = icon("project-diagram"),
      mod_semantic_analysis_ui("semantic")
    )
  )
)

# SERVER semplificato - niente più observeEvent per navigation
server <- function(input, output, session) {
  # Modules - nessuna navigazione manuale necessaria
  imported_corpus <- mod_corpus_io_server("import")
  processed_corpus <- mod_preprocess_server("preprocess", imported_corpus)
  mod_exploration_server("exploration", imported_corpus)
  mod_lexical_analysis_server("lexical", imported_corpus)
  mod_sentiment_server("sentiment", imported_corpus)
  mod_semantic_analysis_server("semantic", imported_corpus)
  
  # Global corpus info potrebbe andare in un footer o in un pannello separato
}

shinyApp(ui = ui, server = server)


### FILE: mod_classification.R
# inst/apps/textwiller_app/mod_classification.R
mod_classification_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Classificazione Utenti e Luoghi"),
    
    shiny::wellPanel(
      shiny::h4("Classificazione"),
      shiny::selectInput(ns("class_type"), "Tipo di classificazione:",
        choices = c("Genere per nome" = "gender", "Luogo" = "location")),
      shiny::actionButton(ns("run_classification"), "Classifica", class = "btn-primary")
    )
  )
}


### FILE: mod_corpus_io.R
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


### FILE: mod_exploration.R
# Corpus Exploration Module
mod_exploration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Corpus Exploration & Visualization"),
    
    # Controls
    shiny::wellPanel(
      shiny::h4("Analysis Settings"),
      shiny::fluidRow(
        shiny::column(3,
          shiny::numericInput(ns("top_n_words"), "Top N words:",
            value = 20, min = 5, max = 50, step = 5)
        ),
        shiny::column(3,
          shiny::numericInput(ns("wordcloud_max"), "Wordcloud max words:",
            value = 50, min = 10, max = 100, step = 10)
        ),
        shiny::column(3,
          shiny::checkboxInput(ns("remove_stopwords"), "Remove stopwords", value = TRUE)
        ),
        shiny::column(3,
          shiny::selectInput(ns("plot_language"), "Language:",
            choices = c("Italian" = "it", "English" = "en"),
            selected = "it")
        )
      )
    ),
    
    # Summary Statistics
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Corpus Summary"),
          shiny::tableOutput(ns("summary_table"))
        )
      ),
      shiny::column(8,
        shiny::wellPanel(
          shiny::h4("Document Length Distribution"),
          shiny::plotOutput(ns("length_histogram"))
        )
      )
    ),
    
    # Word Analysis
    shiny::fluidRow(
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Most Frequent Words"),
          shiny::plotOutput(ns("freq_plot"))
        )
      ),
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Word Cloud"),
          shiny::plotOutput(ns("wordcloud_plot"))
        )
      )
    ),
    
    # Detailed Tables
    shiny::wellPanel(
      shiny::h4("Detailed Word Frequencies"),
      DT::dataTableOutput(ns("freq_table"))
    )
  )
}

mod_exploration_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Calculate word frequencies reactively - VERSIONE SICURA
    word_frequencies <- shiny::reactive({
      shiny::req(corpus())
      
      # Usa una funzione sicura che non dipende da TextWiller3
      calculate_word_frequencies_safe <- function(text, remove_stopwords = FALSE, language = "it") {
        if (is.null(text) || length(text) == 0) {
          return(data.frame(word = character(), frequency = numeric()))
        }
        
        # Combine all documents
        all_text <- paste(text, collapse = " ")
        
        # Split into words
        words <- strsplit(tolower(all_text), "\\s+")[[1]]
        words <- words[nchar(words) > 1]  # Remove single characters
        
        # Remove stopwords if requested
        if (remove_stopwords) {
          if (language == "it") {
            stopwords_list <- c("il", "lo", "la", "i", "gli", "le", "un", "uno", "una",
                              "di", "a", "da", "in", "con", "su", "per", "tra", "fra",
                              "è", "sono", "era", "erano", "essere", "avere", "ha", "hanno",
                              "questo", "questa", "quello", "quella", "che", "chi", "cui")
          } else {
            stopwords_list <- c("the", "a", "an", "and", "or", "but", "in", "on", "at",
                              "to", "for", "of", "with", "by", "as", "is", "are", "was")
          }
          words <- words[!words %in% stopwords_list]
        }
        
        # Calculate frequencies
        freq_table <- table(words)
        freq_table <- sort(freq_table, decreasing = TRUE)
        
        data.frame(
          word = names(freq_table),
          frequency = as.numeric(freq_table),
          stringsAsFactors = FALSE
        )
      }
      
      # Usa la funzione sicura
      calculate_word_frequencies_safe(
        corpus(), 
        remove_stopwords = input$remove_stopwords,
        language = input$plot_language
      )
    })
    
    # Get document lengths - VERSIONE SICURA
    doc_lengths <- shiny::reactive({
      shiny::req(corpus())
      
      # Funzione sicura per lunghezze documento
      get_document_lengths_safe <- function(text) {
        if (is.null(text) || length(text) == 0) {
          return(data.frame(
            document_id = integer(),
            word_count = integer(),
            char_count = integer(),
            stringsAsFactors = FALSE
          ))
        }
        
        word_counts <- sapply(strsplit(text, "\\s+"), length)
        char_counts <- nchar(text)
        
        data.frame(
          document_id = seq_along(text),
          word_count = word_counts,
          char_count = char_counts,
          stringsAsFactors = FALSE
        )
      }
      
      get_document_lengths_safe(corpus())
    })
    
    # Get corpus stats - VERSIONE SICURA
    get_corpus_stats_safe <- function(text) {
      if (is.null(text) || length(text) == 0) {
        return(list(
          n_docs = 0,
          total_words = 0,
          total_chars = 0,
          avg_words = 0,
          avg_chars = 0
        ))
      }
      
      word_counts <- sapply(strsplit(text, "\\s+"), length)
      char_counts <- nchar(text)
      
      list(
        n_docs = length(text),
        total_words = sum(word_counts),
        total_chars = sum(char_counts),
        avg_words = round(mean(word_counts), 1),
        avg_chars = round(mean(char_counts), 1),
        max_words = max(word_counts),
        min_words = min(word_counts)
      )
    }
    
    # Summary table
    output$summary_table <- shiny::renderTable({
      shiny::req(corpus())
      
      stats <- get_corpus_stats_safe(corpus())
      freq_data <- word_frequencies()
      
      data.frame(
        Statistic = c(
          "Total Documents",
          "Total Words", 
          "Unique Words",
          "Average Words/Doc",
          "Lexical Diversity"
        ),
        Value = c(
          stats$n_docs,
          stats$total_words,
          nrow(freq_data),
          stats$avg_words,
          ifelse(stats$total_words > 0, 
                round(nrow(freq_data) / stats$total_words, 3), 0)
        )
      )
    }, bordered = TRUE, align = 'l', width = '100%')
    
    # Document length histogram
    output$length_histogram <- shiny::renderPlot({
      shiny::req(doc_lengths())
      
      lengths <- doc_lengths()
      
      ggplot2::ggplot(lengths, ggplot2::aes(x = word_count)) +
        ggplot2::geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7, color = "white") +
        ggplot2::labs(
          title = "Distribution of Document Lengths (Words)",
          x = "Words per Document",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    # Word frequency plot
    output$freq_plot <- shiny::renderPlot({
      shiny::req(word_frequencies())
      
      top_words <- head(word_frequencies(), input$top_n_words)
      
      ggplot2::ggplot(top_words, ggplot2::aes(x = reorder(word, frequency), y = frequency)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Top", input$top_n_words, "Most Frequent Words"),
          x = "Words",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 10)
        )
    })
    
    # Word cloud
    output$wordcloud_plot <- shiny::renderPlot({
      shiny::req(word_frequencies())
      
      freq_data <- head(word_frequencies(), input$wordcloud_max)
      
      if (nrow(freq_data) > 0 && requireNamespace("wordcloud", quietly = TRUE)) {
        wordcloud::wordcloud(
          words = freq_data$word,
          freq = freq_data$frequency,
          max.words = input$wordcloud_max,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.8),
          random.order = FALSE,
          rot.per = 0.3
        )
      } else if (nrow(freq_data) > 0) {
        # Fallback: bar plot se wordcloud non disponibile
        top_words <- head(freq_data, 15)
        ggplot2::ggplot(top_words, ggplot2::aes(x = reorder(word, frequency), y = frequency)) +
          ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Top Words (Wordcloud not available)", x = "Words", y = "Frequency") +
          ggplot2::theme_minimal()
      }
    })
    
    # Frequency table
    output$freq_table <- DT::renderDataTable({
      shiny::req(word_frequencies())
      
      freq_data <- word_frequencies()
      if (nrow(freq_data) > 0) {
        freq_data$percentage <- round(freq_data$frequency / sum(freq_data$frequency) * 100, 2)
      } else {
        freq_data$percentage <- numeric(0)
      }
      
      DT::datatable(
        freq_data,
        options = list(
          pageLength = 10,
          order = list(1, 'desc'),
          dom = 'ltipr'
        ),
        colnames = c('Word', 'Frequency', 'Percentage (%)'),
        rownames = FALSE,
        caption = 'Word Frequency Distribution'
      ) %>% 
        DT::formatStyle('frequency', fontWeight = 'bold') %>%
        DT::formatStyle('percentage', color = 'steelblue')
    })
  })
}


### FILE: mod_lexical_analysis.R
# Lexical Analysis Module
mod_lexical_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Lexical Analysis & Complexity Measures"),
    
    # Analysis Controls
    shiny::wellPanel(
      shiny::h4("Analysis Settings"),
      shiny::fluidRow(
        shiny::column(3,
          shiny::selectInput(ns("analysis_type"), "Analysis Type:",
            choices = c(
              "Lexical Diversity" = "diversity",
              "Complexity Measures" = "complexity", 
              "Keyword Extraction" = "keywords",
              "Vocabulary Analysis" = "vocabulary"
            ),
            selected = "diversity"
          )
        ),
        shiny::column(3,
          shiny::numericInput(ns("top_n"), "Top N items:",
            value = 20, min = 5, max = 50, step = 5)
        ),
        shiny::column(3,
          shiny::selectInput(ns("keyword_method"), "Keyword Method:",
            choices = c("TF-IDF" = "tfidf", "Log-Likelihood" = "loglikelihood"),
            selected = "tfidf"
          )
        ),
        shiny::column(3,
          shiny::actionButton(ns("run_analysis"), "Run Analysis", 
                           class = "btn-primary btn-block")
        )
      )
    ),
    
    # Results Display
    shiny::uiOutput(ns("results_ui"))
  )
}

mod_lexical_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive value for analysis results
    analysis_results <- shiny::reactiveVal()
    
    # Run analysis when button is clicked
    shiny::observeEvent(input$run_analysis, {
      shiny::req(corpus())
      
      shiny::showNotification("Running lexical analysis...", type = "message")
      
      tryCatch({
        results <- switch(input$analysis_type,
          "diversity" = {
            diversity_measures <- TextWiller3::calculate_lexical_diversity(corpus())
            list(
              type = "diversity",
              data = diversity_measures,
              title = "Lexical Diversity Measures"
            )
          },
          "complexity" = {
            complexity_measures <- TextWiller3::calculate_brunato_measures(corpus())
            list(
              type = "complexity", 
              data = complexity_measures,
              title = "Text Complexity Measures (Brunato)"
            )
          },
          "keywords" = {
            keywords <- TextWiller3::extract_keywords(
              corpus(), 
              method = input$keyword_method,
              top_n = input$top_n
            )
            list(
              type = "keywords",
              data = keywords,
              title = paste("Top", input$top_n, "Keywords")
            )
          },
          "vocabulary" = {
            freq_data <- TextWiller3::calculate_word_frequencies_enhanced(corpus())
            vocab_stats <- list(
              total_words = sum(freq_data$frequency),
              unique_words = nrow(freq_data),
              lexical_diversity = TextWiller3::calculate_ttr(corpus()),
              most_frequent = head(freq_data, input$top_n)
            )
            list(
              type = "vocabulary",
              data = vocab_stats,
              title = "Vocabulary Statistics"
            )
          }
        )
        
        analysis_results(results)
        shiny::showNotification("Analysis completed!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Analysis error:", e$message), type = "error")
      })
    })
    
    # Dynamic results UI
    output$results_ui <- shiny::renderUI({
      results <- analysis_results()
      if (is.null(results)) {
        return(shiny::tagList(
          shiny::wellPanel(
            shiny::h4("No Analysis Results"),
            shiny::p("Click 'Run Analysis' to generate lexical analysis results.")
          )
        ))
      }
      
      ns <- session$ns
      
      shiny::tagList(
        shiny::wellPanel(
          shiny::h4(results$title),
          
          switch(results$type,
            "diversity" = {
              shiny::tagList(
                shiny::tableOutput(ns("diversity_table")),
                shiny::plotOutput(ns("diversity_plot"))
              )
            },
            "complexity" = {
              shiny::tagList(
                shiny::tableOutput(ns("complexity_table")),
                shiny::plotOutput(ns("complexity_plot"))
              )
            },
            "keywords" = {
              shiny::tagList(
                DT::dataTableOutput(ns("keywords_table")),
                shiny::plotOutput(ns("keywords_plot"))
              )
            },
            "vocabulary" = {
              shiny::tagList(
                shiny::tableOutput(ns("vocab_stats_table")),
                DT::dataTableOutput(ns("vocab_freq_table"))
              )
            }
          )
        )
      )
    })
    
    # Diversity table
    output$diversity_table <- shiny::renderTable({
      results <- analysis_results()
      if (!is.null(results) && results$type == "diversity") {
        results$data
      }
    }, bordered = TRUE, align = 'l', width = '100%')
    
    # Diversity plot
    output$diversity_plot <- shiny::renderPlot({
      results <- analysis_results()
      if (!is.null(results) && results$type == "diversity") {
        data <- results$data
        ggplot2::ggplot(data, ggplot2::aes(x = measure, y = value, fill = measure)) +
          ggplot2::geom_col(alpha = 0.8) +
          ggplot2::labs(title = "Lexical Diversity Measures", x = "Measure", y = "Value") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        legend.position = "none")
      }
    })
    
    # Complexity table
    output$complexity_table <- shiny::renderTable({
      results <- analysis_results()
      if (!is.null(results) && results$type == "complexity") {
        data <- as.data.frame(results$data)
        data
      }
    }, bordered = TRUE, align = 'l', width = '100%')
    
    # Complexity plot
    output$complexity_plot <- shiny::renderPlot({
      results <- analysis_results()
      if (!is.null(results) && results$type == "complexity") {
        data <- as.data.frame(results$data)
        data_long <- data.frame(
          measure = names(results$data),
          value = unlist(results$data)
        )
        ggplot2::ggplot(data_long, ggplot2::aes(x = measure, y = value, fill = measure)) +
          ggplot2::geom_col(alpha = 0.8) +
          ggplot2::labs(title = "Text Complexity Measures", x = "Measure", y = "Value") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                        legend.position = "none")
      }
    })
    
    # Keywords table
    output$keywords_table <- DT::renderDataTable({
      results <- analysis_results()
      if (!is.null(results) && results$type == "keywords") {
        DT::datatable(
          results$data,
          options = list(
            pageLength = 10,
            order = list(2, 'desc')
          ),
          rownames = FALSE
        )
      }
    })
    
    # Keywords plot
    output$keywords_plot <- shiny::renderPlot({
      results <- analysis_results()
      if (!is.null(results) && results$type == "keywords") {
        data <- head(results$data, 15)  # Top 15 for readability
        ggplot2::ggplot(data, ggplot2::aes(x = reorder(term, score), y = score)) +
          ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Top Keywords by Score", x = "Term", y = "Score") +
          ggplot2::theme_minimal()
      }
    })
    
    # Vocabulary stats table
    output$vocab_stats_table <- shiny::renderTable({
      results <- analysis_results()
      if (!is.null(results) && results$type == "vocabulary") {
        data.frame(
          Metric = c("Total Words", "Unique Words", "Lexical Diversity (TTR)"),
          Value = c(
            results$data$total_words,
            results$data$unique_words,
            round(results$data$lexical_diversity, 4)
          )
        )
      }
    }, bordered = TRUE, align = 'l', width = '100%')
    
    # Vocabulary frequency table
    output$vocab_freq_table <- DT::renderDataTable({
      results <- analysis_results()
      if (!is.null(results) && results$type == "vocabulary") {
        DT::datatable(
          results$data$most_frequent,
          options = list(
            pageLength = 10,
            order = list(1, 'desc')
          ),
          rownames = FALSE,
          caption = "Most Frequent Words"
        )
      }
    })
  })
}


### FILE: mod_pattern.R
pattern_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Estrai pattern da testo"),
    textAreaInput(ns("text"), "Testo:", "Scrivi un testo con numeri 123 e hashtag #prova", rows = 5),
    textInput(ns("pattern"), "Pattern regex:", "#\\w+"),
    actionButton(ns("run"), "Estrai"),
    verbatimTextOutput(ns("out"))
  )
}

pattern_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      req(input$text, input$pattern)
      output$out <- renderPrint({
        TextWiller::patternExtract(input$text, input$pattern)
      })
    })
  })
}


### FILE: mod_preprocess.R
# Enhanced Preprocessing Module
# Enhanced Preprocessing Module with Legacy Support
mod_preprocess_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Configurable Preprocessing Pipeline"),
    
    fluidRow(
      column(12,
        shiny::wellPanel(
          shiny::h4("TextWiller Legacy Features"),
          shiny::checkboxInput(ns("use_legacy"), "Use original TextWiller normalization", value = TRUE),
          shiny::helpText("When enabled, uses the proven TextWiller Italian text normalization pipeline")
        )
      )
    ),
    
    fluidRow(
      column(6,
        shiny::wellPanel(
          shiny::h4("Basic Normalization"),
          shiny::checkboxGroupInput(ns("normalization_steps"), "Text Normalization:",
            choices = c(
              "Convert to lowercase" = "lowercase",
              "Remove punctuation" = "remove_punct", 
              "Remove numbers" = "remove_numbers",
              "Remove extra whitespace" = "remove_whitespace",
              "Normalize URLs" = "normalize_urls",
              "Normalize emoticons" = "normalize_emoticons",
              "Normalize Italian slang" = "normalize_slang"
            ),
            selected = c("lowercase", "remove_punct", "remove_whitespace")
          )
        )
      ),
      column(6,
        shiny::wellPanel(
          shiny::h4("Advanced Processing"),
          shiny::checkboxGroupInput(ns("advanced_steps"), "Advanced Steps:",
            choices = c(
              "Remove stopwords" = "remove_stopwords",
              "Remove short words (<3 chars)" = "remove_short_words",
              "Apply Italian stemming" = "apply_stemming"
            )
          ),
          shiny::selectInput(ns("language"), "Language:",
            choices = c("Italian" = "it", "English" = "en"),
            selected = "it"
          ),
          shiny::numericInput(ns("min_word_length"), "Minimum word length:", 
            value = 2, min = 1, max = 10
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        wellPanel(
          h4("Processing Actions"),
          fluidRow(
            column(4,
              actionButton(ns("preview_btn"), "Preview Processing", 
                         class = "btn-primary btn-block")
            ),
            column(4,
              actionButton(ns("apply_btn"), "Apply to Corpus", 
                         class = "btn-success btn-block")
            ),
            column(4,
              actionButton(ns("reset_btn"), "Reset Pipeline", 
                         class = "btn-warning btn-block")
            )
          )
        )
      )
    ),
    
    hr(),
    
    # Preview Section
    fluidRow(
      column(6,
        wellPanel(
          h4("Original Text Preview"),
          verbatimTextOutput(ns("original_preview"))
        )
      ),
      column(6,
        wellPanel(
          h4("Processed Text Preview"),
          verbatimTextOutput(ns("processed_preview"))
        )
      )
    ),
    
    # Statistics Section
    wellPanel(
      h4("Processing Statistics"),
      fluidRow(
        column(6,
          tableOutput(ns("before_stats"))
        ),
        column(6,
          tableOutput(ns("after_stats"))
        )
      )
    )
  )
}

mod_preprocess_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    processed_corpus <- shiny::reactiveVal()
    
    # Reset pipeline
    shiny::observeEvent(input$reset_btn, {
      shiny::updateCheckboxGroupInput(session, "normalization_steps", 
                              selected = c("lowercase", "remove_punct", "remove_whitespace"))
      shiny::updateCheckboxGroupInput(session, "advanced_steps", selected = character(0))
      shiny::updateSelectInput(session, "language", selected = "it")
      shiny::updateNumericInput(session, "min_word_length", value = 2)
      processed_corpus(NULL)
      shiny::showNotification("Pipeline reset to defaults", type = "message")
    })
    
    # Preview original text
    output$original_preview <- shiny::renderPrint({
      shiny::req(corpus())
      if(length(corpus()) > 0) {
        cat("First 3 documents:\n\n")
        for(i in 1:min(3, length(corpus()))) {
          cat(sprintf("Doc %d: %s\n\n", i, substr(corpus()[i], 1, 200)))
        }
      } else {
        cat("No corpus available")
      }
    })
    
    # Process text with selected pipeline
    process_text <- function(text) {
      if(is.null(text) || length(text) == 0) return(text)
      
      # Apply basic normalization steps
      if("lowercase" %in% input$normalization_steps) {
        text <- tolower(text)
      }
      
      if("remove_punct" %in% input$normalization_steps) {
        text <- gsub("[[:punct:]]", " ", text)
      }
      
      if("remove_numbers" %in% input$normalization_steps) {
        text <- gsub("[[:digit:]]", " ", text)
      }
      
      if("remove_whitespace" %in% input$normalization_steps) {
        text <- gsub("\\s+", " ", text)
        text <- trimws(text)
      }
      
      # Apply advanced steps
      if("remove_stopwords" %in% input$advanced_steps) {
        text <- TextWiller3::remove_stopwords_enhanced(text, language = input$language)
      }
      
      if("remove_short_words" %in% input$advanced_steps) {
        words <- strsplit(text, "\\s+")
        text <- sapply(words, function(x) {
          keep_words <- x[nchar(x) >= input$min_word_length]
          paste(keep_words, collapse = " ")
        })
      }
      
      return(text)
    }
    
    # Preview processing
    shiny::observeEvent(input$preview_btn, {
      shiny::req(corpus())
      
      preview_text <- head(corpus(), 3)
      processed_preview <- process_text(preview_text)
      
      output$processed_preview <- shiny::renderPrint({
        cat("Processed version:\n\n")
        for(i in 1:length(processed_preview)) {
          cat(sprintf("Doc %d: %s\n\n", i, substr(processed_preview[i], 1, 200)))
        }
      })
      
      # Show statistics
      original_stats <- TextWiller3::get_corpus_stats(preview_text)
      processed_stats <- TextWiller3::get_corpus_stats(processed_preview)
      
      output$before_stats <- shiny::renderTable({
        data.frame(
          Metric = c("Documents", "Total Words", "Avg Words/Doc", "Vocabulary"),
          Value = c(
            original_stats$n_docs,
            original_stats$total_words,
            original_stats$avg_words,
            nrow(TextWiller3::calculate_word_frequencies_enhanced(preview_text, preprocess = FALSE))
          )
        )
      }, bordered = TRUE)
      
      output$after_stats <- shiny::renderTable({
        data.frame(
          Metric = c("Documents", "Total Words", "Avg Words/Doc", "Vocabulary"),
          Value = c(
            processed_stats$n_docs,
            processed_stats$total_words,
            processed_stats$avg_words,
            nrow(TextWiller3::calculate_word_frequencies_enhanced(processed_preview, preprocess = FALSE))
          )
        )
      }, bordered = TRUE)
    })
    
    # Apply processing to full corpus
    shiny::observeEvent(input$apply_btn, {
      shiny::req(corpus())
      
      shiny::showModal(shiny::modalDialog(
        title = "Processing Corpus",
        "Applying preprocessing pipeline to all documents...",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Process the entire corpus
      full_processed <- process_text(corpus())
      
      # Update processed corpus
      processed_corpus(full_processed)
      
      shiny::removeModal()
      
      # Show summary
      original_stats <- TextWiller3::get_corpus_stats(corpus())
      processed_stats <- TextWiller3::get_corpus_stats(full_processed)
      
      shiny::showNotification(
        sprintf(
          "Processing complete! Reduced from %d to %d words (%.1f%%)",
          original_stats$total_words,
          processed_stats$total_words,
          (1 - processed_stats$total_words / original_stats$total_words) * 100
        ),
        type = "message",
        duration = 10
      )
    })
    
    # Return processed corpus
    return(processed_corpus)
  })
}


### FILE: mod_semantic_analysis.R
# Semantic Analysis Module
mod_semantic_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Semantic Analysis with Word Embeddings"),
    
    # Model Configuration
    shiny::wellPanel(
      shiny::h4("Embedding Model Configuration"),
      shiny::fluidRow(
        shiny::column(3,
          shiny::selectInput(ns("model_type"), "Model Type:",
            choices = c("FastText" = "fasttext", "Word2Vec" = "word2vec", "BERT" = "bert"),
            selected = "fasttext")
        ),
        shiny::column(3,
          shiny::selectInput(ns("language"), "Language:",
            choices = c("Italian" = "it", "English" = "en", "Multilingual" = "multi"),
            selected = "it")
        ),
        shiny::column(3,
          shiny::selectInput(ns("aggregation"), "Text Aggregation:",
            choices = c("Mean" = "mean", "Sum" = "sum"),
            selected = "mean")
        ),
        shiny::column(3,
          shiny::actionButton(ns("load_model"), "Load Model", class = "btn-primary")
        )
      ),
      shiny::textInput(ns("custom_model_path"), "Custom Model Path (optional):", 
                      placeholder = "/path/to/model.bin"),
      shiny::helpText("Note: First-time model loading may take several minutes")
    ),
    
    # Analysis Tabs
    shiny::tabsetPanel(
      id = ns("semantic_tabs"),
      type = "tabs",
      
      # Similarity Analysis Tab
      shiny::tabPanel(
        "Semantic Similarity",
        shiny::wellPanel(
          shiny::h4("Document Similarity Analysis"),
          shiny::fluidRow(
            shiny::column(4,
              shiny::selectInput(ns("similarity_method"), "Similarity Measure:",
                choices = c("Cosine" = "cosine", "Euclidean" = "euclidean"),
                selected = "cosine")
            ),
            shiny::column(4,
              shiny::numericInput(ns("top_similar"), "Top Similar Documents:", 
                                value = 5, min = 1, max = 20)
            ),
            shiny::column(4,
              shiny::actionButton(ns("calc_similarity"), "Calculate Similarity", 
                                class = "btn-success")
            )
          )
        ),
        shiny::plotOutput(ns("similarity_heatmap")),
        DT::dataTableOutput(ns("similarity_table"))
      ),
      
      # Word Similarity Tab
      shiny::tabPanel(
        "Word Similarity",
        shiny::wellPanel(
          shiny::h4("Word Similarity Search"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::textInput(ns("target_word"), "Target Word:", value = "governo")
            ),
            shiny::column(3,
              shiny::numericInput(ns("top_words"), "Top N Words:", 
                                value = 10, min = 1, max = 20)
            ),
            shiny::column(3,
              shiny::actionButton(ns("find_similar"), "Find Similar Words", 
                                class = "btn-success")
            )
          )
        ),
        shiny::plotOutput(ns("word_similarity_plot")),
        DT::dataTableOutput(ns("word_similarity_table"))
      ),
      
      # Document Clustering Tab
      shiny::tabPanel(
        "Document Clustering",
        shiny::wellPanel(
          shiny::h4("Semantic Document Clustering"),
          shiny::fluidRow(
            shiny::column(3,
              shiny::numericInput(ns("n_clusters"), "Number of Clusters:",
                                value = 3, min = 2, max = 10)
            ),
            shiny::column(3,
              shiny::selectInput(ns("clustering_method"), "Clustering Method:",
                choices = c("K-means" = "kmeans", "Hierarchical" = "hierarchical"),
                selected = "kmeans")
            ),
            shiny::column(3,
              shiny::selectInput(ns("reduction_method"), "Visualization Method:",
                choices = c("PCA" = "pca", "t-SNE" = "tsne", "UMAP" = "umap"),
                selected = "pca")
            ),
            shiny::column(3,
              shiny::actionButton(ns("run_clustering"), "Run Clustering", 
                                class = "btn-success")
            )
          )
        ),
        shiny::plotOutput(ns("clustering_plot")),
        DT::dataTableOutput(ns("cluster_table"))
      )
    )
  )
}

mod_semantic_analysis_server <- function(id, corpus) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values
    embedding_model <- shiny::reactiveVal()
    document_embeddings <- shiny::reactiveVal()
    
    # Load embedding model
    shiny::observeEvent(input$load_model, {
      shiny::showNotification("Loading embedding model... This may take a while.", 
                            type = "message", duration = 10)
      
      tryCatch({
        model <- TextWiller3::load_embedding_model(
          model_type = input$model_type,
          language = input$language,
          model_path = ifelse(input$custom_model_path == "", 
                             NULL, input$custom_model_path)
        )
        
        embedding_model(model)
        shiny::showNotification("Model loaded successfully!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error loading model:", e$message), type = "error")
      })
    })
    
    # Calculate document embeddings when model is loaded
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
          shiny::showNotification("Embeddings calculated!", type = "message")
          
        }, error = function(e) {
          shiny::showNotification(paste("Error calculating embeddings:", e$message), type = "error")
        })
      }
    })
    
    # Semantic similarity analysis
    shiny::observeEvent(input$calc_similarity, {
      shiny::req(document_embeddings(), corpus())
      
      tryCatch({
        similarity_matrix <- TextWiller3::calculate_semantic_similarity(
          document_embeddings(),
          method = input$similarity_method
        )
        
        # Create similarity data for table
        similarity_data <- create_similarity_table(similarity_matrix, corpus())
        
        # Heatmap
        output$similarity_heatmap <- shiny::renderPlot({
          plot_similarity_heatmap(similarity_matrix, corpus())
        })
        
        # Similarity table
        output$similarity_table <- DT::renderDataTable({
          DT::datatable(
            similarity_data,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE,
            caption = "Document Similarity Scores"
          ) %>% DT::formatStyle('similarity', 
                              background = DT::styleInterval(
                                c(0.3, 0.7), 
                                c('white', 'lightyellow', 'lightgreen')
                              ))
        })
        
      }, error = function(e) {
        shiny::showNotification(paste("Similarity calculation error:", e$message), type = "error")
      })
    })
    
    # Word similarity search
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
              ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
              ggplot2::coord_flip() +
              ggplot2::labs(title = paste("Words Similar to:", input$target_word),
                           x = "Word", y = "Similarity") +
              ggplot2::theme_minimal()
          })
          
          output$word_similarity_table <- DT::renderDataTable({
            DT::datatable(
              similar_words,
              options = list(pageLength = 10),
              rownames = FALSE
            )
          })
        } else {
          output$word_similarity_plot <- shiny::renderPlot({ NULL })
          output$word_similarity_table <- DT::renderDataTable({ NULL })
          shiny::showNotification("No similar words found or word not in vocabulary", type = "warning")
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Word similarity error:", e$message), type = "error")
      })
    })
    
    # Document clustering
    shiny::observeEvent(input$run_clustering, {
      shiny::req(document_embeddings(), corpus())
      
      tryCatch({
        # Perform clustering
        clusters <- TextWiller3::cluster_documents(
          document_embeddings(),
          n_clusters = input$n_clusters,
          method = input$clustering_method
        )
        
        # Reduce dimensions for visualization
        reduced_embeddings <- TextWiller3::reduce_dimensions(
          document_embeddings(),
          method = input$reduction_method,
          n_components = 2
        )
        
        # Create clustering plot
        output$clustering_plot <- shiny::renderPlot({
          plot_clustering(reduced_embeddings, clusters, corpus())
        })
        
        # Create cluster table
        cluster_data <- data.frame(
          Document = seq_along(corpus()),
          Text = substr(corpus(), 1, 100),
          Cluster = clusters,
          stringsAsFactors = FALSE
        )
        
        output$cluster_table <- DT::renderDataTable({
          DT::datatable(
            cluster_data,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
          )
        })
        
      }, error = function(e) {
        shiny::showNotification(paste("Clustering error:", e$message), type = "error")
      })
    })
    
    # Helper function for similarity table
    create_similarity_table <- function(similarity_matrix, corpus) {
      top_indices <- apply(similarity_matrix, 1, function(x) {
        head(order(-x), input$top_similar + 1)[-1]  # Exclude self-similarity
      })
      
      similarity_data <- data.frame()
      for (i in 1:nrow(similarity_matrix)) {
        for (j in 1:input$top_similar) {
          if (j <= length(top_indices[, i])) {
            target_idx <- top_indices[j, i]
            similarity_data <- rbind(similarity_data, data.frame(
              document_i = i,
              text_i = substr(corpus[i], 1, 50),
              document_j = target_idx,
              text_j = substr(corpus[target_idx], 1, 50),
              similarity = round(similarity_matrix[i, target_idx], 3)
            ))
          }
        }
      }
      
      return(similarity_data)
    }
    
    # Helper function for similarity heatmap
    plot_similarity_heatmap <- function(similarity_matrix, corpus) {
      # Simplify for visualization (first 20 documents)
      n_show <- min(20, nrow(similarity_matrix))
      show_matrix <- similarity_matrix[1:n_show, 1:n_show]
      show_labels <- paste0("Doc", 1:n_show)
      
      melted_matrix <- reshape2::melt(show_matrix)
      colnames(melted_matrix) <- c("Document1", "Document2", "Similarity")
      
      ggplot2::ggplot(melted_matrix, ggplot2::aes(x = Document1, y = Document2, fill = Similarity)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                     midpoint = 0.5, limits = c(0, 1)) +
        ggplot2::labs(title = "Document Semantic Similarity Heatmap",
                     x = "Documents", y = "Documents") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
    
    # Helper function for clustering plot
    plot_clustering <- function(reduced_embeddings, clusters, corpus) {
      plot_data <- data.frame(
        X = reduced_embeddings[, 1],
        Y = reduced_embeddings[, 2],
        Cluster = as.factor(clusters),
        Text = substr(corpus, 1, 30)
      )
      
      ggplot2::ggplot(plot_data, ggplot2::aes(x = X, y = Y, color = Cluster, label = Text)) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::labs(title = "Document Clustering in Semantic Space",
                     x = "Dimension 1", y = "Dimension 2") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_brewer(palette = "Set1")
    }
  })
}


### FILE: mod_sentiment.R
# Sentiment Analysis Module
mod_sentiment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Sentiment Analysis (Italiano)"),
    
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
        ),
        shiny::column(4,
          shiny::checkboxInput(ns("use_legacy"), "Usa funzioni originali", value = TRUE)
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
    
    # Run sentiment analysis
    shiny::observeEvent(input$run_sentiment, {
      shiny::req(corpus())
      
      shiny::showNotification("Analizzando sentiment...", type = "message")
      
      tryCatch({
        # Perform sentiment analysis - VERSIONE CORRETTA
        if (input$use_legacy && exists("sentiment")) {
          # Usa la funzione originale con i parametri corretti
          scores <- sentiment(
            corpus(),
            algorithm = input$sentiment_algorithm,
            normalizzaTesti = input$normalize_text
          )
        } else {
          # Usa la funzione di fallback
          scores <- TextWiller3::analyze_sentiment_it(
            corpus(),
            use_legacy = FALSE  # Forza l'uso del fallback
          )
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
    })
    
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


### FILE: mod_similarity.R
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
        embeddings <- text::textEmbed(texts, model = input$model)
        
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


### FILE: mod_stopwords.R
stopwords_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Rimozione stopwords"),
    textAreaInput(ns("text"), "Testo:", "Questo è un esempio di frase con molte parole vuote", rows = 5),
    actionButton(ns("run"), "Rimuovi stopwords"),
    verbatimTextOutput(ns("result"))
  )
}

stopwords_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      req(input$text)
      output$result <- renderPrint({
        TextWiller::removeStopwords(input$text)
      })
    })
  })
}


### FILE: mod_url.R
url_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Estrai URL dai testi"),
    textAreaInput(ns("text"), "Testo:", "Leggi qui: https://openai.com e http://example.com", rows = 5),
    actionButton(ns("go"), "Estrai URL"),
    verbatimTextOutput(ns("out"))
  )
}

url_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go, {
      req(input$text)
      output$out <- renderPrint({
        TextWiller::urlExtract(input$text)
      })
    })
  })
}
