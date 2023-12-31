box::use(
  bsicons[bs_icon],
  bslib[value_box],
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput],
)

box::use(
  app/logic/corr,
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
      corr$min_sample(
        alpha=inputs()$alpha,
        effect=inputs()$effect,
        alt=inputs()$alt
      )
    })
    
    output$minSample <- renderUI({
      value_box(
        title="Observations needed:",
        value=minSample(),
        showcase=bsicons::bs_icon("people-fill"),
        theme="white", full_screen=FALSE, fill=TRUE, height=100L,
        class="info-box"
      )
    })
    
  })
}
