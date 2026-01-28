stopwords_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Stopword removal"),
    textAreaInput(ns("text"), "Text:", "This is an example sentence with many stop words", rows = 5),
    actionButton(ns("run"), "Remove stopwords"),
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
