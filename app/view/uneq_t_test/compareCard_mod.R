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
    uiOutput(ns("compare"))
  )
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    compare <- reactive({
      uneq_t_test$t2n_compare(
        alpha=inputs()$alpha,
        effect=inputs()$effect,
        n1=inputs()$n1,
        n2=inputs()$n2,
        alt=inputs()$alt
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
