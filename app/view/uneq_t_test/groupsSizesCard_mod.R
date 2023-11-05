box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
  glue[glue]
)

box::use(
  app/logic/uneq_t_test/functions,
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
    
    group1 <- reactive({ inputs()$group1_n })
    group2 <- reactive({ inputs()$group2_n })
    
    output$minEffect <- renderUI({
      value_box(
        title="Selected group sizes:",
        value=glue("{group1()} & {group2()}"),
        showcase=bsicons::bs_icon("people-fill"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
