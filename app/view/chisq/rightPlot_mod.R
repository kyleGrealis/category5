box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput]
)

box::use(
  app/logic/plotCard,
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
    
    plot <- reactive({
      plotCard$power_bar(
        data=data(), 
        n=inputs()$n, 
        effect_values=c(0.1, 0.3, 0.5)
      )
    })
    
    output$rightPlot <- renderUI({
      plotCard$plotting_cards(
        "Power at selected sample size",
        plot()
      )
    })
    
  })
}
