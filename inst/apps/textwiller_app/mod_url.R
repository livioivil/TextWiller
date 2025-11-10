url_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Estrai URL dai testi"),
    textAreaInput(ns("text"), "Testo:", "Leggi qui: https://openai.com e http://example.com", rows = 5),
    actionButton(ns("go"), "Estrai URL"),
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
