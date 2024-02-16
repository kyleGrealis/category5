box::use(
  bsicons[bs_icon],
  bslib[value_box],
  glue[glue],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("minEffect"))
  )
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    n1 <- reactive({ inputs()$n1 })
    n2 <- reactive({ inputs()$n2 })
    
    output$minEffect <- renderUI({
      value_box(
        title="Group sample sizes:",
        value=glue("{n1()} & {n2()}"),
        showcase=bsicons::bs_icon("people-fill"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
