box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/uneq_t_test/functions,
)

#' @export
t2n_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("compare"))
  )
  
}

#' @export
t2n_server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    compare <- reactive({
      functions$t2n_compare(
        t_type=inputs()$type, 
        alt=inputs()$alt,
        group1_n=inputs()$group1_n,
        group2_n=inputs()$group2_n,
        d=inputs()$effect,
        alpha=inputs()$alpha
      )
    })
    
    
    output$compare <- renderUI({
      value_box(
        title="Proposed study power is:",
        value=compare(),
        showcase=bsicons::bs_icon("bullseye"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
