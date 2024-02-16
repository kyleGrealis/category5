box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/plotCard,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("leftPlot"))
  )
}

#' @export
server <- function(id, data, inputs){
  moduleServer(id, function(input, output, session) {

    plot <- reactive({ plotCard$power_effect(data=data(), n=inputs()$n) })

    output$leftPlot <- renderUI({
      plotCard$plotting_cards(
        "Power vs. Effect Size, for number of tests per group",
        plot()
      )
    })
  })
}
