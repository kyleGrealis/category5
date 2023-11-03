library(shiny)
library(echarts4r)
library(dplyr)

# left plot module ----
plot_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    echarts4rOutput(ns("leftPlot"))
  )
  
}

plot_server <- function(id, data, n){
  moduleServer(id, function(input, output, session) {
    output$leftPlot <- renderEcharts4r(power_effect(data=data, n=n)) 
  })
}


# plot_demo <- function() {
#   data <- res
#   n <- 70
#   ui <- fluidPage(plot_ui("mitter"))
#   server <- function(input, output, session) {
#     plot_server("mitter", data, n)
#   }
#   shinyApp(ui, server)
# }
# 
# plot_demo()
shinyApp(plot_ui, plot_server)
