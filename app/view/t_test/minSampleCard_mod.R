box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/t_test/functions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("minSample"))
  )
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    minSample <- reactive({
      functions$min_sample(
        t_type=inputs()$type, 
        alt=inputs()$alt, 
        d=inputs()$effect,
        alpha=inputs()$alpha
      )
    })
    
    output$minSample <- renderUI({
      value_box(
        title="Sample size per group:",
        value=minSample(),
        showcase=bsicons::bs_icon("people-fill"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
