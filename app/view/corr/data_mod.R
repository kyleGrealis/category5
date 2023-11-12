box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/corr,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
}

#' @export
server <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      req(inputs()$alpha)
      req(inputs()$effect)
      req(inputs()$n)
      req(inputs()$alt)
      
      corr$corr_table(
        alpha=inputs()$alpha,
        n=inputs()$n, 
        alt=inputs()$alt
      )
    })
    
    return(data)
    
  })
}
