url_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Extract URLs from text"),
    textAreaInput(ns("text"), "Text:", "Read here: https://openai.com and http://example.com", rows = 5),
    actionButton(ns("go"), "Extract URLs"),
    verbatimTextOutput(ns("out"))
  )
}

url_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go, {
      req(input$text)
      output$out <- renderPrint({
        TextWiller::urlExtract(input$text)
      })
    })
  })
}
