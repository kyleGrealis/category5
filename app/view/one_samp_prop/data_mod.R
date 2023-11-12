box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/one_samp_prop,
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
      req(inputs()$p1)
      req(inputs()$p2)
      req(inputs()$alt)
      
      one_samp_prop$prop_table(
        alpha=inputs()$alpha,
        n=inputs()$n, 
        alt=inputs()$alt
      )
    })
    
    return(data)
    
  })
}
