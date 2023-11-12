box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/chisq,
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
      chisq$min_sample(
        df=inputs()$df, 
        effect=inputs()$effect,
        alpha=inputs()$alpha
      )
    })
    
    output$minSample <- renderUI({
      value_box(
        title="Total sample size:",
        value=minSample(),
        showcase=bsicons::bs_icon("people-fill"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
