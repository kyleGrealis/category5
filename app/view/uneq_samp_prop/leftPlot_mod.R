box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput, span, em],
)

box::use(
  app/logic/plotCard,
  app/logic/uneq_samp_prop/functions,
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

    plot <- reactive({ functions$power_effect(data=data(), n2=inputs()$n2) })

    output$leftPlot <- renderUI({
      plotCard$plotting_cards(
        span(
          "Power vs. Effect Size, for Different", 
          em("Group 2"), "Sample Sizes"
        ),
        plot()
      )
    })
  })
}
