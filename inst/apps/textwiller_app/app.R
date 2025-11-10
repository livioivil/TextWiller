### FILE: inst/apps/textwiller_app/app.R 
library(shiny)
library(shinythemes)
library(ggplot2)

# SISTEMA DI CARICAMENTO SEMPLIFICATO
source_modules <- function() {
  cat("=== LOADING TEXTWILLER MODULES ===\n")
  
  # Carica i moduli base
  module_files <- c(
    "mod_corpus_io.R",
    "mod_preprocess.R", 
    "mod_exploration.R",
    "mod_lexical_analysis.R",
    "mod_sentiment.R",
    "mod_semantic_analysis.R",
    "mod_language.R",
    "mod_reproducibility.R"
  )
  
  # Cerca nella cartella corrente
  for (file in module_files) {
    if (file.exists(file)) {
      cat("Loading:", file, "\n")
      source(file, local = FALSE)
    } else {
      cat("File not found:", file, "\n")
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
  titlePanel("TextWiller 3.0 - u cannot advance without reproducibility"),
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    tabPanel("📥 Import Corpus", mod_corpus_io_ui("import")),
    tabPanel("⚙️ Preprocessing", mod_preprocess_ui("preprocess")),
    tabPanel("📊 Exploration", mod_exploration_ui("exploration")),
    tabPanel("📈 Lexical Analysis", mod_lexical_analysis_ui("lexical")),
    tabPanel("😊 Sentiment Analysis", mod_sentiment_ui("sentiment")),
    tabPanel("🔍 Semantic Analysis", mod_semantic_analysis_ui("semantic")),
    tabPanel("🌍 Language & Resources", mod_language_ui("language"))
  ),
  hr(),
  div(
    style = "background:#f8f9fa; padding:15px; border-radius:6px;",
    mod_reproducibility_ui("reproducibility")
  )
)

server <- function(input, output, session) {
  
  # Modules principali
  imported_corpus <- mod_corpus_io_server("import")
  processed_corpus <- mod_preprocess_server("preprocess", imported_corpus)
  mod_exploration_server("exploration", imported_corpus)
  mod_lexical_analysis_server("lexical", imported_corpus)
  mod_sentiment_server("sentiment", imported_corpus)
  mod_semantic_analysis_server("semantic", imported_corpus)
  
  # Moduli extra
  mod_reproducibility_server("reproducibility")
  mod_language_server("language")
}

shinyApp(ui = ui, server = server)
