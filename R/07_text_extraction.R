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