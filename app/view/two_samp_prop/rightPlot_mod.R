box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput]
)

box::use(
  app/logic/plotCard,
  app/logic/two_samp_prop/functions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("rightPlot"))
  )
  
}

#' @export
server <- function(id, data, inputs){
  moduleServer(id, function(input, output, session) {
    
    plot <- reactive({ functions$power_bar(data=data(), n=inputs()$n) })
    
    output$rightPlot <- renderUI({
      plotCard$plotting_cards(
        "Power at Varying Effect Sizes, for Selected Number of Tests",
        plot()
      )
    })
    
  })
}
