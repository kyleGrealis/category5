box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/uneq_t_test/functions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
}

#' @export
server <- function(id, inputs){
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(inputs()$alpha)
      req(inputs()$effect)
      req(inputs()$n1)
      req(inputs()$n2)
      req(inputs()$alt)
      
      functions$t2n_table(
        alpha=inputs()$alpha, 
        n1=inputs()$n1, 
        n2=inputs()$n2, 
        alt=inputs()$alt
      )
    })
    
    return(data)
    
  })
}
