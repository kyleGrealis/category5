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
    uiOutput(ns("effectInfo"))
  )
  
}

#' @export
t2n_server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    effect <- reactive({ inputs()$effect })
    
    output$effectInfo <- renderUI({
      value_box(
        title = "Measurable effect size:",
        value = effect(),
        showcase = bs_icon("graph-up-arrow"),
        theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
        class = "info-box"
      )
    })
    
  })
}
