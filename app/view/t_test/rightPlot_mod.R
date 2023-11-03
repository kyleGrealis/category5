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
    echarts4rOutput(ns("rightPlot"))
  )
  
}

#' @export
server <- function(id, data, n, d){
  moduleServer(id, function(input, output, session) {
    output$rightPlot <- renderEcharts4r(
      power_bar(data=data, n=n, d=d)
    ) 
  })
}
