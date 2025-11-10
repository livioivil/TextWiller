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