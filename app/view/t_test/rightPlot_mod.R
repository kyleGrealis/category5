box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput]
)

box::use(
  app/logic/plots[plotting_cards],
  app/logic/t_test/functions
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
      plotting_cards(
        "Bars represent power varying effect sizes given 
        selected group sample size",
        plot()
      )
    })
    
  })
}
