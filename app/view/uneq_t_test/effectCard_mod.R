box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/uneq_t_test,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("effectInfo"))
  )
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    minEffect <- reactive({
      uneq_t_test$min_effect(
        n1=inputs()$n1, 
        n2=inputs()$n2,
        alpha=inputs()$alpha,
        alt=inputs()$alt
      )
    })
    
    output$effectInfo <- renderUI({
      value_box(
        title="Effect size:",
        value=minEffect(),
        showcase=bsicons::bs_icon("graph-up-arrow"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
