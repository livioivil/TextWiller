mod_language_ui <- function(id) {
  ns <- shiny::NS(id)
  current_lang <- TextWiller3::get_language_config()$current_language
  shiny::tagList(
    shiny::h3("🌍 Language & Resources"),
    shiny::p("Carica stopwords, dizionari o liste di multi-word dedicate a Italiano e Inglese per alimentare le analisi."),
    
    shiny::fluidRow(
      shiny::column(
        4,
          shiny::selectInput(
            ns("language"),
            "Lingua di riferimento",
            choices = c("Italiano" = "it", "English" = "en"),
            selected = current_lang
          )
      ),
      shiny::column(
        4,
        shiny::textInput(ns("resource_name"), "Nome risorsa", placeholder = "es. sentiment_ita_2024")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          ns("resource_type"),
          "Tipologia",
          choices = c("Stopwords" = "stopwords", "Lexicon" = "lexicon", "Multiword" = "multiword", "Sentiment" = "sentiment"),
          selected = "lexicon"
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::fileInput(
          ns("resource_file"),
          "Carica file (txt, csv, rds, rda)",
          accept = c(".txt", ".csv", ".rds", ".rda")
        )
      ),
      shiny::column(
        6,
        shiny::actionButton(
          ns("add_resource_btn"),
          "Registra risorsa",
          class = "btn-primary",
          icon = shiny::icon("database")
        ),
        shiny::br(),
        shiny::actionButton(
          ns("download_stopwords_btn"),
          "Scarica stopwords-iso",
          class = "btn-success",
          icon = shiny::icon("cloud-download-alt")
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(ns("preview_resource"), "Anteprima risorsa", choices = character(0))
      ),
      shiny::column(
        6,
        shiny::actionButton(ns("refresh_resources"), "Aggiorna elenco", icon = shiny::icon("sync"))
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::verbatimTextOutput(ns("resource_preview"), placeholder = TRUE)
      )
    ),
    
    shiny::hr(),
    shiny::h4("Risorse registrate"),
    DT::dataTableOutput(ns("resource_table"))
  )
}

mod_language_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang_cfg <- TextWiller3::get_language_config()
    
    resource_data <- shiny::reactivePoll(
      2000, session,
      checkFunc = function() paste(lang_cfg$get_version(), input$language, input$refresh_resources),
      valueFunc = function() {
        TextWiller3::list_language_resources(language = input$language, include_content = FALSE)
      }
    )
    
    update_preview_choices <- function() {
      resources <- resource_data()
      choices <- c("Nessuna" = "")
      if (nrow(resources) > 0) {
        label <- paste0(resources$name, " [", resources$type, "]")
        names(label) <- resources$name
        choices <- c("Nessuna" = "", label)
      }
      shiny::updateSelectInput(session, "preview_resource", choices = choices)
    }
    
    shiny::observe({
      resource_data()
      update_preview_choices()
    })
    
    shiny::observeEvent(input$language, {
      lang_cfg$set_language(input$language)
      TextWiller3::set_analysis_language(input$language)
      shiny::showNotification(
        paste("Lingua impostata su", ifelse(input$language == "it", "Italiano", "English")),
        type = "message"
      )
    })
    
    shiny::observeEvent(input$add_resource_btn, {
      shiny::req(input$resource_file)
      resource_name <- if (nzchar(input$resource_name)) input$resource_name else tools::file_path_sans_ext(input$resource_file$name)
      ext <- tools::file_ext(input$resource_file$name)
      content <- tryCatch({
        if (tolower(ext) == "rds") {
          readRDS(input$resource_file$datapath)
        } else if (tolower(ext) == "rda") {
          env <- new.env()
          load(input$resource_file$datapath, envir = env)
          as.list(env)
        } else if (tolower(ext) %in% c("csv", "tsv")) {
          read.csv(input$resource_file$datapath, stringsAsFactors = FALSE)
        } else {
          readLines(input$resource_file$datapath, encoding = "UTF-8", warn = FALSE)
        }
      }, error = function(e) {
        shiny::showNotification(paste("Errore lettura file:", e$message), type = "error")
        NULL
      })
      shiny::req(!is.null(content))
      
      TextWiller3::register_language_resource(
        language = input$language,
        name = resource_name,
        content = content,
        type = input$resource_type
      )
      shiny::showNotification(paste("Registrata risorsa", resource_name), type = "message")
      update_preview_choices()
    })
    
    shiny::observeEvent(input$download_stopwords_btn, {
      tryCatch({
        name <- paste0("stopwords_iso_", input$language, "_", format(Sys.time(), "%H%M%S"))
        words <- TextWiller3::download_stopwords_iso(language = input$language, auto_register = TRUE, resource_name = name)
        shiny::showNotification(paste("Scaricate", length(words), "stopwords da stopwords-iso"), type = "message")
        update_preview_choices()
      }, error = function(e) {
        shiny::showNotification(paste("Errore download stopwords:", e$message), type = "error")
      })
    })
    
    output$resource_table <- DT::renderDataTable({
      data <- resource_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(
          name = character(),
          type = character(),
          language = character(),
          source = character(),
          created = character(),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        data,
        options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
        rownames = FALSE
      )
    })
    
    output$resource_preview <- shiny::renderPrint({
      req(input$preview_resource)
      if (input$preview_resource == "") {
        cat("Seleziona una risorsa per visualizzare l'anteprima.")
        return()
      }
      content <- TextWiller3::get_language_resource(
        language = input$language,
        name = input$preview_resource
      )
      if (is.null(content)) {
        cat("Nessuna informazione disponibile per la risorsa selezionata.")
      } else if (is.character(content)) {
        preview <- head(content, 20)
        cat(paste(preview, collapse = "\n"))
        if (length(content) > 20) cat("\n...")
      } else if (is.data.frame(content)) {
        print(utils::head(content, 5))
      } else {
        str(content, max.level = 1)
      }
    })
  })
}
