library(shiny)
library(dplyr)

# call the other module(s)
source("dev/controls_mod_test.R")
source("dev/leftPlot_mod_test.R")
source("dev/alpha_mod.R")

# load the data as if it were created within the app
data <- res

# create the page with inputs and plot
ui <- fluidPage(
  title = "testing modules",
  sidebarLayout(
    sidebarPanel = controls_ui("mitter"),
    mainPanel = mainPanel(
      h2("displaying the left plot"),
      leftPlot_ui("teek"),   # testing if this can be given any namespace
      p("the end!")
    )
  )
)
server <- function(input, output, session) {
  controls_server("mitter")
  leftPlot_server("teek", data, n)
}

shinyApp(ui, server)
