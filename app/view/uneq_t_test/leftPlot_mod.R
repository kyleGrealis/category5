box::use(
  shiny[em, span, moduleServer, NS, tagList, reactive, renderUI, uiOutput],
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

    plot <- reactive({
      plotCard$power_effect(data=data(), n=inputs()$n2, unequal=TRUE)
    })

    output$leftPlot <- renderUI({
      plotCard$plotting_cards(
        span(
          "Power vs. Effect Size, at", 
          em("Group 2"), "sample sizes"
        ),
        plot()
      )
    })
  })
}
