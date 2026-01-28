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