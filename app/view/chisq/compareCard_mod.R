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
    uiOutput(ns("compare"))
  )
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    compare <- reactive({
      chisq$chisq_compare(
        n=inputs()$n,
        effect=inputs()$effect,
        alpha=inputs()$alpha,
        df=inputs()$df
      )
    })
    
    output$compare <- renderUI({
      value_box(
        title="Proposed power is:",
        value=compare(),
        showcase=bsicons::bs_icon("bullseye"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
