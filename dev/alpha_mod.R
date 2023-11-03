# box::use(
#   shiny[moduleServer, NS, reactive],
#   shiny.blueprint[Select.shinyInput]
# )
library(shiny)
library(shiny.blueprint)

alphaLabels <- list(
  list(text = "Recommended", label = 0.05),
  list(text = "Stronger", label = 0.025),
  list(text = "Strongest", label = 0.01)
)

# alpha module ----
alpha_ui <- function(id) {
  ns <- NS(id)
  shiny.blueprint::Select.shinyInput(
    inputId = ns("alpha"),
    items = alphaLabels,
    selected = "Recommended"
  )
}
alpha_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    alpha <- reactive({input$alpha})
  })
}
