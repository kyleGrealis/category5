box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/plotCard,
  app/logic/t_test/functions,
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

    plot <- reactive({ functions$power_effect(data=data(), n=inputs()$n) })

    output$leftPlot <- renderUI({
      plotCard$plotting_cards(
        "Lines represent group sample size (±30) plotted
        against power and effect size",
        plot()
      )
    })
  })
}
