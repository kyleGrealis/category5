library(shiny.blueprint)
library(shiny)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    H3("Effect size"),
    RadioGroup.shinyInput(
      inputId = ns("effect"),
      value = 0.3,
      Radio(label = "Small (0.1)", value = 0.1),
      Radio(label = "Medium (0.3)", value = 0.3),
      Radio(label = "Large (0.5)", value = 0.5)
    ),
    textOutput(ns("effectSize")),
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$effectSize <- renderText(input$effect)
  })
}

if (interactive()) shinyApp(ui("app"), function(input, output) server("app"))
