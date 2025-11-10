stopwords_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Rimozione stopwords"),
    textAreaInput(ns("text"), "Testo:", "Questo è un esempio di frase con molte parole vuote", rows = 5),
    actionButton(ns("run"), "Rimuovi stopwords"),
    verbatimTextOutput(ns("result"))
  )
}

stopwords_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      req(input$text)
      output$result <- renderPrint({
        TextWiller::removeStopwords(input$text)
      })
    })
  })
}
