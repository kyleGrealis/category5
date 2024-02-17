box::use(
  shiny[moduleServer, NS, tags, fluidPage, checkboxInput, observeEvent],
)


jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://kylegrealis.shinyapps.io/powerViz/';});"


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(tags$script(jscode)),     
    checkboxInput(ns("Redirect"), "Click this to go to our new location: 
                  kyleGrealis.shinyapps.io/powerViz/", value = T)
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    observeEvent(input$Redirect,{
      if(!input$Redirect){
        session$sendCustomMessage("mymessage", "mymessage")
      }
    })
    
  })
}
