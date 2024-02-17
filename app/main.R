box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu,
        nav_panel, layout_sidebar],
  shiny[div, icon, markdown, moduleServer, NS, tags, fluidPage, checkboxInput, observeEvent],
  shinyjs[runjs, useShinyjs],
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
