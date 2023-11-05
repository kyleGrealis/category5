box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/plotCard,
  app/logic/uneq_t_test/functions,
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

    # plot <- reactive({ functions$power_effect(data=data(), n=inputs()$n) })

    output$leftPlot <- renderUI({
      plotCard$plotting_cards(
        "I don't know what to put here yet",
        "something will go here"
        # plot()
      )
    })
  })
}
