pattern_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h4("Extract patterns from text"),
    textAreaInput(ns("text"), "Text:", "Write a text with numbers 123 and hashtag #example", rows = 5),
    textInput(ns("pattern"), "Regex pattern:", "#\\w+"),
    actionButton(ns("run"), "Extract"),
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
