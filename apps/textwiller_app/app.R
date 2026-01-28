### FILE: inst/apps/textwiller_app/app.R 
library(shiny)
library(shinythemes)
library(ggplot2)
library(text)

# Allow larger uploads for heavy linguistic resources (e.g., UDPipe models)
options(shiny.maxRequestSize = 200 * 1024^2)  # ~200MB

# Clear all caches and temporary data on app start (optional - uncomment if desired)
# try({
#   # Clean temporary files from previous sessions
#   temp_dir <- tempdir()
#   temp_files <- list.files(temp_dir, pattern = "^(textwiller_|embeddings_|corpus_|udpipe_temp|shinyapp)", 
#                           full.names = TRUE)
#   if (length(temp_files) > 0) {
#     file.remove(temp_files[file.exists(temp_files)])
#   }
# }, silent = TRUE)

# Clear history at app start
if (exists("get_auto_history", mode = "function")) {
  try({
    history <- get_auto_history()
    history$clear()
  }, silent = TRUE)
}

# Ensure reticulate points to the textrpp environment before loading modules
tw_configure_textrpp_python <- function() {
  existing <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(existing) && file.exists(existing)) {
    return(existing)
  }
  candidate <- Sys.getenv("TEXTWILLER_PYTHON", unset = "")
  if (!nzchar(candidate)) {
    candidate <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda-arm64", "envs",
      "textrpp_condaenv", "bin", "python")
  }
  if (nzchar(candidate) && file.exists(candidate)) {
    Sys.setenv(RETICULATE_PYTHON = candidate)
    return(candidate)
  }
  NULL
}

tw_configure_textrpp_python()

register_default_resources <- function() {
  if (!requireNamespace("TextWiller3", quietly = TRUE)) return()
  env <- new.env(parent = emptyenv())
  safe_load <- function(name) {
    tryCatch({
      utils::data(list = name, package = "TextWiller3", envir = env, quietly = TRUE)
      exists(name, envir = env, inherits = FALSE)
    }, warning = function(w) FALSE, error = function(e) FALSE)
  }
  # Stopwords
  if (isTRUE(safe_load("stopwords_ita"))) {
    TextWiller3::register_language_resource("it", "stopwords_ita", get("stopwords_ita", envir = env), type = "lexicon")
  }
  # Sentiment keywords
  if (isTRUE(safe_load("dizionario_sentiment_ita"))) {
    dict <- get("dizionario_sentiment_ita", envir = env)
    if (!is.null(dict$keyword)) {
      TextWiller3::register_language_resource("it", "sentiment_keywords", dict$keyword, type = "lexicon")
    }
  }
  # Mattivio / Madda vocabularies (se disponibili)
  if (isTRUE(safe_load("vocabolarioMattivio"))) {
    TextWiller3::register_language_resource("it", "vocabolarioMattivio", get("vocabolarioMattivio", envir = env), type = "lexicon")
  }
  if (isTRUE(safe_load("vocabolariMadda"))) {
    madda <- get("vocabolariMadda", envir = env)
    if (is.list(madda)) {
      TextWiller3::register_language_resource("it", "vocabolariMadda", unlist(madda), type = "lexicon")
    }
  }
}

register_default_resources()

# SISTEMA DI CARICAMENTO SEMPLIFICATO
source_modules <- function() {
  cat("=== LOADING TEXTWILLER MODULES ===\n")
  
  # Percorso base dei moduli
  app_dir <- getwd()
  if (basename(app_dir) != "textwiller_app") {
    app_dir <- file.path(app_dir, "inst", "apps", "textwiller_app")
  }
  
  module_files <- c(
    "mod_corpus_io.R",
    "mod_preprocess.R", 
    "mod_exploration.R",
    "mod_lexical_analysis.R",
    "mod_sentiment.R",
    "mod_ollama.R",
    "mod_semantic_analysis.R",
    "mod_language.R",
    "mod_reproducibility.R"
  )
  
  # Cerca nella cartella dei moduli
  for (file in module_files) {
    file_path <- file.path(app_dir, file)
    if (file.exists(file_path)) {
      cat("Loading:", file_path, "\n")
      source(file_path, local = FALSE)
    } else {
      cat("File not found:", file_path, "\n")
    }
  }
  
  cat("=== MODULES LOADED ===\n\n")
  return(TRUE)
}

# Carica i moduli
success <- source_modules()

# UI principale con sidebar
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .tw-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 10px;
        padding: 10px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 8px;
        color: white;
      }
      .tw-header h2 {
        margin: 0;
        font-size: 1.8rem;
      }
      .tab-pane {
        padding: 15px;
      }
      .well-panel-enhanced {
        background: #f8f9fa;
        border-radius: 6px;
        padding: 15px;
        margin-bottom: 15px;
        border-left: 4px solid #667eea;
      }
      .btn-clean {
        margin: 2px;
      }
    "))
  ),

  shiny::div(
    class = "tw-header",
    shiny::h2("TextWiller 3.0")
  ),

  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    tabPanel("📥 Import Corpus", mod_corpus_io_ui("import")),
    tabPanel("⚙️ Preprocessing", mod_preprocess_ui("preprocess")),
    tabPanel("📊 Explore", mod_exploration_ui("exploration")),
    tabPanel("📈 Lexical Analysis", mod_lexical_analysis_ui("lexical")),
    tabPanel("😊 Sentiment Analysis", mod_sentiment_ui("sentiment")),
    tabPanel("🔍 Semantic Analysis", mod_semantic_analysis_ui("semantic")),
    tabPanel("🤖 Ollama", mod_ollama_ui("ollama")),
    tabPanel("🌍 Language & Resources", mod_language_ui("language")),
    tabPanel(
      "📚 Documentation & References",
      div(
        class = "well-panel-enhanced",
        h3("Documentation & References"),
        h4("Cache Management:"),
        tags$ul(
          tags$li("Go to 'Language & Resources' tab for cache management"),
          tags$li("Use the cleanup buttons to remove temporary files"),
          tags$li("Cache stores: downloaded models, embeddings, and temporary results"),
          tags$li("Resources store: uploaded lexicons, stopwords, UDPipe models")
        ),
        h4("Manual cleanup from R console:"),
        tags$pre("# Clear all cached data
TextWiller3::clean_textwiller_data('all')

# Clear only embedding cache
TextWiller3::clear_embedding_cache()

# Clear only resources
TextWiller3::clean_textwiller_data('resources')

# Get cache statistics
TextWiller3::get_cache_stats()"),
        h4("References:"),
                tags$pre("# Textwiller 3.0
Made by Dario Solari, Andrea Sciandra, Livio Finos, Alessandro Meneghini"),
        hr(),
        div(
          style = "background:#f8f9fa; padding:15px; border-radius:6px; margin-top:20px;",
          mod_reproducibility_ui("reproducibility")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Modules principali
  imported_corpus <- mod_corpus_io_server("import")
  processed_corpus <- mod_preprocess_server("preprocess", imported_corpus)
  mod_exploration_server("exploration", imported_corpus, processed_corpus)
  mod_lexical_analysis_server("lexical", imported_corpus)
  mod_sentiment_server("sentiment", imported_corpus)
  mod_semantic_analysis_server("semantic", imported_corpus)
  mod_ollama_server("ollama", imported_corpus, processed_corpus)
  # Moduli extra
  mod_reproducibility_server("reproducibility") # saves each passage into an history for replicability
  mod_language_server("language")
}

shinyApp(ui = ui, server = server)
