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
  mc <- match.call()
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
  
  log_reproducibility_action(
    module = "corpus_io",
    operation = "import_text_files",
    parameters = capture_repro_args(mc),
    input_state = list(files = files),
    output_state = snapshot_dataframe(documents)
  )
  
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
  mc <- match.call()
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
  
  log_reproducibility_action(
    module = "corpus_io",
    operation = "import_csv_text",
    parameters = capture_repro_args(mc),
    input_state = list(file_path = file_path, rows = nrow(data)),
    output_state = snapshot_dataframe(documents)
  )
  
  return(documents)
}

#' Get corpus statistics
#' 
#' @param corpus Character vector of documents
#' @return A list with corpus statistics
#' @export
get_corpus_stats <- function(corpus) {
  mc <- match.call()
  if (is.null(corpus) || length(corpus) == 0) {
    empty_stats <- list(
      n_docs = 0,
      total_words = 0,
      total_chars = 0,
      avg_words = 0,
      avg_chars = 0
    )
    log_reproducibility_action(
      module = "corpus_io",
      operation = "get_corpus_stats",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(corpus),
      output_state = empty_stats
    )
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
  
  stats <- list(
    n_docs = length(corpus),
    total_words = sum(word_counts),
    total_chars = sum(char_counts),
    avg_words = round(mean(word_counts), 1),
    avg_chars = round(mean(char_counts), 1),
    max_words = max(word_counts),
    min_words = min(word_counts)
  )
  
  log_reproducibility_action(
    module = "corpus_io",
    operation = "get_corpus_stats",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(corpus),
    output_state = stats
  )
  
  stats
}
