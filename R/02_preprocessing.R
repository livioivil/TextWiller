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
  mc <- match.call(expand.dots = TRUE)
  if (use_legacy && exists("normalizzaTesti")) {
    # Use your original function
    result <- normalizzaTesti(text, ...)
  } else {
    # Use new simplified normalization
    result <- clean_text_basic(text, ...)
  }
  
  log_reproducibility_action(
    module = "preprocessing",
    operation = "normalize_text",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(text),
    output_state = snapshot_text_vector(result)
  )
  
  result
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
remove_stopwords_enhanced <- function(text, language = "it", use_legacy = TRUE, custom_stopwords = NULL) {
  mc <- match.call()
  original_text <- text
  
  stopwords_list <- custom_stopwords
  
  if (is.null(stopwords_list)) {
    if (use_legacy && language == "it" && exists("stopwords_ita")) {
      stopwords_list <- stopwords_ita
    } else {
      lang_cfg <- get_language_config()
      stopwords_list <- lang_cfg$get_stopwords(language)
    }
  }
  
  if (length(stopwords_list) > 0) {
    stopwords_lower <- tolower(stopwords_list)
    text <- vapply(text, function(doc) {
      tokens <- unlist(strsplit(doc, "\\s+"))
      if (length(tokens) == 0) {
        return("")
      }
      keep <- tokens[!(tolower(tokens) %in% stopwords_lower)]
      paste(keep, collapse = " ")
    }, character(1), USE.NAMES = FALSE)
  }
  
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  log_reproducibility_action(
    module = "preprocessing",
    operation = "remove_stopwords_enhanced",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = snapshot_text_vector(text)
  )
  
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

#' Download stopwords from stopwords-iso repository
#'
#' Retrieves the latest stopword list for the requested language from the
#' stopwords-iso GitHub project and optionally registers it as a TextWiller
#' language resource for future reuse.
#'
#' @param language ISO language code ("it" or "en")
#' @param auto_register Whether to register the downloaded list via
#'   `register_language_resource()`
#' @param resource_name Optional custom name for the registered resource
#' @return Character vector of stopwords
#' @export
download_stopwords_iso <- function(language = "it", auto_register = TRUE, resource_name = NULL) {
  supported <- c("it", "en")
  if (!language %in% supported) {
    stop("Unsupported language for stopwords-iso download: ", language)
  }
  
  base_url <- sprintf("https://raw.githubusercontent.com/stopwords-iso/stopwords-%s/master/stopwords-%s.txt",
                      language, language)
  
  stopwords <- tryCatch({
    words <- readLines(base_url, encoding = "UTF-8", warn = FALSE)
    words <- unique(trimws(words))
    words[nchar(words) > 0]
  }, error = function(e) {
    stop("Unable to download stopwords from stopwords-iso: ", e$message)
  })
  
  if (auto_register) {
    name <- if (is.null(resource_name)) paste0("stopwords_iso_", language) else resource_name
    register_language_resource(language = language, name = name, content = stopwords, type = "stopwords")
  }
  
  stopwords
}

#' Configurable preprocessing pipeline
#' 
#' @param text Character vector
#' @param pipeline List of processing steps
#' @param use_legacy Use original TextWiller functions when available
#' @return Processed text
#' @export
preprocess_pipeline <- function(text, pipeline = c("lowercase", "remove_punct", "remove_stopwords"), 
                               use_legacy = TRUE, custom_stopwords = NULL) {
  mc <- match.call()
  original_text <- text
  
  for (step in pipeline) {
    text <- switch(step,
      "lowercase" = tolower(text),
      "remove_punct" = if(use_legacy && exists("normalizzapunteggiatura")) {
        normalizzapunteggiatura(text)
      } else {
        gsub("[[:punct:]]", " ", text)
      },
      "remove_numbers" = gsub("[[:digit:]]", " ", text),
      "remove_stopwords" = remove_stopwords_enhanced(text, use_legacy = use_legacy, custom_stopwords = custom_stopwords),
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
  
  log_reproducibility_action(
    module = "preprocessing",
    operation = "preprocess_pipeline",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = snapshot_text_vector(text)
  )
  
  return(text)
}
