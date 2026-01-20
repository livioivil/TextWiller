#' Embedding model cache utilities
#' @noRd
get_embedding_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("TextWiller3", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

#' Ensure FastText model is downloaded locally
#' @noRd
ensure_fasttext_model <- function(language) {
  urls <- list(
    it = "https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.it.300.bin.gz",
    en = "https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.en.300.bin.gz"
  )
  url <- urls[[language]]
  if (is.null(url)) {
    stop("Automatic download not available for FastText language: ", language)
  }
  
  cache_dir <- get_embedding_cache_dir()
  gz_path <- file.path(cache_dir, basename(url))
  bin_path <- sub("\\.gz$", "", gz_path)
  
  if (!file.exists(bin_path)) {
    message("Downloading FastText model for language ", language, " ...")
    utils::download.file(url, destfile = gz_path, mode = "wb", quiet = TRUE)
    message("Extracting FastText model ...")
    R.utils::gunzip(gz_path, destname = bin_path, remove = TRUE, overwrite = TRUE)
  }
  
  bin_path
}

#' Clear all cached embedding models
#' 
#' Removes downloaded FastText, Word2Vec, and other embedding models from cache
#' 
#' @param language Optional language code to clear only specific language models
#' @return Number of files removed
#' @export
clear_embedding_cache <- function(language = NULL) {
  cache_dir <- get_embedding_cache_dir()
  
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(0)
  }
  
  # Patterns for embedding cache files
  patterns <- c(
    "\\.bin$", "\\.bin\\.gz$", "\\.vec$", "\\.model$",
    "^cc\\..+\\.bin$", "^google.*\\.bin$", "^fasttext.*",
    "^word2vec.*", "^embedding.*", "\\.rds$"
  )
  
  all_files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  
  if (length(all_files) == 0) {
    message("No files in cache directory")
    return(0)
  }
  
  # Filter by language if specified
  if (!is.null(language)) {
    language_pattern <- paste0("\\.", language, "\\.")
    all_files <- all_files[grepl(language_pattern, all_files, ignore.case = TRUE)]
  }
  
  # Filter by embedding patterns
  cache_files <- character(0)
  for (pattern in patterns) {
    matches <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
    cache_files <- unique(c(cache_files, matches))
  }
  
  if (length(cache_files) == 0) {
    message("No embedding cache files found")
    return(0)
  }
  
  # Delete files
  removed <- 0
  total_size <- 0
  
  for (file in cache_files) {
    if (file.exists(file)) {
      file_size <- file.size(file)
      success <- unlink(file, recursive = TRUE, force = TRUE)
      if (success == 0) {  # 0 indicates success on Unix-like systems
        removed <- removed + 1
        total_size <- total_size + ifelse(is.na(file_size), 0, file_size)
        message("Removed: ", basename(file))
      }
    }
  }
  
  total_size_mb <- round(total_size / (1024^2), 2)
  message(sprintf("Removed %d embedding cache files (%.2f MB)", removed, total_size_mb))
  
  return(removed)
}

#' Get cache statistics
#' 
#' Returns information about cached files and disk usage
#' 
#' @return List with cache statistics
#' @export
get_cache_stats <- function() {
  cache_dir <- get_embedding_cache_dir()
  
  if (!dir.exists(cache_dir)) {
    return(list(
      cache_dir = cache_dir,
      exists = FALSE,
      files = 0,
      size_mb = 0
    ))
  }
  
  all_files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  
  # Exclude . and ..
  all_files <- all_files[!grepl("/\\.$", all_files)]
  
  if (length(all_files) == 0) {
    return(list(
      cache_dir = cache_dir,
      exists = TRUE,
      files = 0,
      size_mb = 0,
      file_types = list()
    ))
  }
  
  # Calculate sizes
  file_sizes <- file.size(all_files)
  total_size <- sum(file_sizes, na.rm = TRUE)
  
  # Categorize by file type
  file_types <- list()
  patterns <- list(
    fasttext = "\\.bin$",
    word2vec = "\\.model$|\\.vec$",
    embeddings = "\\.rds$",
    compressed = "\\.gz$|\\.zip$|\\.tar$",
    other = ".*"
  )
  
  for (type in names(patterns)) {
    matches <- grepl(patterns[[type]], basename(all_files), ignore.case = TRUE)
    if (any(matches)) {
      type_files <- all_files[matches]
      type_size <- sum(file.size(type_files), na.rm = TRUE)
      file_types[[type]] <- list(
        files = length(type_files),
        size_mb = round(type_size / (1024^2), 2)
      )
    }
  }
  
  list(
    cache_dir = cache_dir,
    exists = TRUE,
    files = length(all_files),
    size_mb = round(total_size / (1024^2), 2),
    file_types = file_types,
    last_modified = if (length(all_files) > 0) {
      max(file.info(all_files)$mtime, na.rm = TRUE)
    } else {
      NA
    }
  )
}

#' Clear all TextWiller temporary data
#' 
#' Cleans cache, resources, and temporary files. Use with caution!
#' 
#' @param what What to clean: "cache", "resources", "temp", or "all"
#' @param language Optional language code for selective cleaning
#' @return List with cleaning results
#' @export
clean_textwiller_data <- function(what = "all", language = NULL) {
  results <- list()
  
  if (what %in% c("cache", "all")) {
    results$cache <- clear_embedding_cache(language = language)
  }
  
  if (what %in% c("resources", "all")) {
    resources_dir <- file.path(tools::R_user_dir("TextWiller3", "data"), "language_resources")
    
    if (dir.exists(resources_dir)) {
      if (!is.null(language)) {
        # Clean only specific language
        lang_dir <- file.path(resources_dir, language)
        if (dir.exists(lang_dir)) {
          files <- list.files(lang_dir, full.names = TRUE, recursive = TRUE)
          removed <- sum(unlink(files, recursive = TRUE, force = TRUE) == 0)
          results$resources <- removed
        } else {
          results$resources <- 0
        }
      } else {
        # Clean all resources
        files <- list.files(resources_dir, full.names = TRUE, recursive = TRUE)
        removed <- sum(unlink(files, recursive = TRUE, force = TRUE) == 0)
        results$resources <- removed
      }
    } else {
      results$resources <- 0
    }
  }
  
  if (what %in% c("temp", "all")) {
    temp_dir <- tempdir()
    temp_patterns <- c(
      "^textwiller_", "^embeddings_", "^corpus_", 
      "^udpipe_temp", "^shinyapp", "Rtmp.*\\.rds$", "Rtmp.*\\.csv$"
    )
    
    temp_files <- list.files(temp_dir, full.names = TRUE, 
                           pattern = paste(temp_patterns, collapse = "|"))
    
    if (length(temp_files) > 0) {
      removed <- sum(file.remove(temp_files), na.rm = TRUE)
      results$temp <- removed
    } else {
      results$temp <- 0
    }
  }
  
  # Clear in-memory caches if they exist
  if (exists(".textwiller_cache", envir = .GlobalEnv)) {
    rm(".textwiller_cache", envir = .GlobalEnv)
    results$memory_cache <- TRUE
  }
  
  if (exists(".textwiller_language_config", envir = .GlobalEnv)) {
    config <- get(".textwiller_language_config", envir = .GlobalEnv)
    if (methods::is(config, "R6") && exists("clear", envir = config)) {
      config$clear()
      results$language_config <- TRUE
    }
  }
  
  message("Cleaning complete:")
  for (item in names(results)) {
    message(sprintf("  %s: %s", item, results[[item]]))
  }
  
  invisible(results)
}