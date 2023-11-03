box::use(
  echarts4r[echarts4rOutput, renderEcharts4r],
  shiny[moduleServer, NS, tagList]
)

box::use(
  app/logic/t_test/functions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    echarts4rOutput(ns("leftPlot"))
  )
  
}

#' @export
server <- function(id, data, inputs){
  moduleServer(id, function(input, output, session) {
    output$leftPlot <- renderEcharts4r(
      power_effect(data=data, n=inputs$n())
    ) 
  })
}
