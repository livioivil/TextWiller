#' Multi-language Support System
#' 
#' Support for Italian and English with external resource integration
#' 

#' Language Configuration
#' @export
LanguageConfig <- R6::R6Class(
  "LanguageConfig",
  public = list(
    current_language = "it",
    resources = list(),
    inline_resources = list(),
    version = 0,
    
    set_language = function(language) {
      if (language %in% c("it", "en")) {
        self$current_language <- language
      } else {
        stop("Unsupported language: ", language)
      }
    },
    
    add_resource = function(name, path, type = "dictionary", language = self$current_language) {
      if (is.null(self$resources[[language]])) {
        self$resources[[language]] <- list()
      }
      self$resources[[language]][[name]] <- list(
        path = path,
        type = type,
        language = language,
        source = "file",
        created = Sys.time()
      )
      self$version <- self$version + 1
    },
    
    add_inline_resource = function(name, content, type = "lexicon", language = self$current_language) {
      if (is.null(self$inline_resources[[language]])) {
        self$inline_resources[[language]] <- list()
      }
      self$inline_resources[[language]][[name]] <- list(
        content = content,
        type = type,
        language = language,
        source = "inline",
        created = Sys.time()
      )
      self$version <- self$version + 1
    },
    
    load_resources = function() {
      loaded <- list()
      for (lang in names(self$resources)) {
        for (name in names(self$resources[[lang]])) {
          resource <- self$resources[[lang]][[name]]
          tryCatch({
            if (file.exists(resource$path)) {
              if (tools::file_ext(resource$path) == "rda") {
                load(resource$path, envir = .GlobalEnv)
              } else if (tools::file_ext(resource$path) == "csv") {
                assign(name, read.csv(resource$path, stringsAsFactors = FALSE), envir = .GlobalEnv)
              } else if (tools::file_ext(resource$path) %in% c("txt", "csv", "dic")) {
                assign(name, readLines(resource$path, encoding = "UTF-8", warn = FALSE), envir = .GlobalEnv)
              }
              loaded[[name]] <- TRUE
            }
          }, error = function(e) {
            warning("Failed to load resource ", name, ": ", e$message)
            loaded[[name]] <- FALSE
          })
        }
      }
      return(loaded)
    },
    
    list_resources = function(language = NULL, type = NULL, include_content = FALSE) {
      entries <- list()
      languages <- if (is.null(language)) unique(c(names(self$resources), names(self$inline_resources))) else language
      for (lang in languages) {
        # File-based resources
        if (!is.null(self$resources[[lang]])) {
          for (name in names(self$resources[[lang]])) {
            res <- self$resources[[lang]][[name]]
            if (is.null(type) || identical(res$type, type)) {
              entry_content <- NULL
              if (include_content) {
                if (!is.null(res$path) && file.exists(res$path)) {
                  entry_content <- if (tools::file_ext(res$path) == "rds") {
                    readRDS(res$path)
                  } else {
                    readLines(res$path, encoding = "UTF-8", warn = FALSE)
                  }
                }
              }
              entries[[length(entries) + 1]] <- list(
                name = name,
                type = res$type,
                language = res$language,
                source = res$source,
                created = res$created,
                content = entry_content
              )
            }
          }
        }
        # Inline resources
        if (!is.null(self$inline_resources[[lang]])) {
          for (name in names(self$inline_resources[[lang]])) {
            res <- self$inline_resources[[lang]][[name]]
            if (is.null(type) || identical(res$type, type)) {
              entry_content <- if (include_content) res$content else NULL
              entries[[length(entries) + 1]] <- list(
                name = name,
                type = res$type,
                language = res$language,
                source = res$source,
                created = res$created,
                content = entry_content
              )
            }
          }
        }
      }
      
      if (length(entries) == 0) {
        return(data.frame(
          name = character(),
          type = character(),
          language = character(),
          source = character(),
          created = as.POSIXct(character()),
          stringsAsFactors = FALSE
        ))
      }
      
      df <- do.call(rbind, lapply(entries, function(entry) {
        data.frame(
          name = entry$name,
          type = entry$type,
          language = entry$language,
          source = entry$source,
          created = entry$created,
          stringsAsFactors = FALSE
        )
      }))
      if (include_content) {
        df$content <- lapply(entries, function(entry) entry$content)
      }
      df
    },
    
    get_resource_content = function(name, language = self$current_language, type = NULL) {
      pool <- list(self$inline_resources[[language]], self$resources[[language]])
      for (bucket in pool) {
        if (!is.null(bucket) && name %in% names(bucket)) {
          res <- bucket[[name]]
          if (is.null(type) || identical(res$type, type)) {
            if (!is.null(res$content)) {
              return(res$content)
            } else if (!is.null(res$path) && file.exists(res$path)) {
              if (tools::file_ext(res$path) == "rds") {
                return(readRDS(res$path))
              } else {
                return(readLines(res$path, encoding = "UTF-8", warn = FALSE))
              }
            }
          }
        }
      }
      NULL
    },
    
    get_stopwords = function(language = self$current_language) {
      inline <- self$list_resources(language = language, type = "stopwords", include_content = TRUE)
      if (is.data.frame(inline) && nrow(inline) > 0) {
        # Return the first available stopword list for the language
        return(inline$content[[1]])
      }
      
      if (language == "it" && exists("stopwords_ita")) {
        return(stopwords_ita)
      }
      get_basic_stopwords(language)
    },
    
    get_version = function() {
      self$version
    }
  )
)

#' Global Language Configuration
#' @export
get_language_config <- function() {
  if (!exists(".textwiller_language_config", envir = .GlobalEnv)) {
    assign(".textwiller_language_config", LanguageConfig$new(), envir = .GlobalEnv)
  }
  get(".textwiller_language_config", envir = .GlobalEnv)
}

#' Set the global analysis language
#' @param language "it" or "en"
#' @export
set_analysis_language <- function(language) {
  cfg <- get_language_config()
  cfg$set_language(language)
  invisible(language)
}

#' Get the current analysis language
#' @return Language code string
#' @export
get_analysis_language <- function() {
  get_language_config()$current_language
}

#' Register an in-memory language resource
#'
#' @param language Language code ("it" or "en")
#' @param name Resource name
#' @param content Character vector or list with the resource content
#' @param type Resource type (e.g. "stopwords", "lexicon", "multiword")
#' @export
register_language_resource <- function(language, name, content, type = "lexicon") {
  cfg <- get_language_config()
  cfg$add_inline_resource(name = name, content = content, type = type, language = language)
  invisible(TRUE)
}

#' List registered language resources
#'
#' @param language Optional language filter
#' @param type Optional resource type filter
#' @param include_content Whether to include the full resource content
#' @export
list_language_resources <- function(language = NULL, type = NULL, include_content = FALSE) {
  get_language_config()$list_resources(language = language, type = type, include_content = include_content)
}

#' Fetch the content of a registered language resource
#'
#' @param language Language code
#' @param name Resource name
#' @param type Optional type filter
#' @export
get_language_resource <- function(language, name, type = NULL) {
  get_language_config()$get_resource_content(name = name, language = language, type = type)
}
