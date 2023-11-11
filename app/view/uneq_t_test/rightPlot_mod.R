box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput]
)

box::use(
  app/logic/plotCard,
  app/logic/uneq_t_test/functions,
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
      functions$power_bar(
        data=data(), 
        n1=inputs()$n1,
        n2=inputs()$n2
      )
    })
    
    output$rightPlot <- renderUI({
      plotCard$plotting_cards(
        "Power at Varying Effect Sizes, for Selected Group Sample Sizes",
        plot()
      )
    })
    
  })
}
