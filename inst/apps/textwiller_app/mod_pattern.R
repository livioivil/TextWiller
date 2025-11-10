pattern_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Estrai pattern da testo"),
    textAreaInput(ns("text"), "Testo:", "Scrivi un testo con numeri 123 e hashtag #prova", rows = 5),
    textInput(ns("pattern"), "Pattern regex:", "#\\w+"),
    actionButton(ns("run"), "Estrai"),
    verbatimTextOutput(ns("out"))
  )
}

pattern_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run, {
      req(input$text, input$pattern)
      output$out <- renderPrint({
        TextWiller::patternExtract(input$text, input$pattern)
      })
    })
  })
}
