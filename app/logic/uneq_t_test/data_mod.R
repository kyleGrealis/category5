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
        n1=inputs()$group1_n, 
        n2=inputs()$group2_n, 
        alt=inputs()$alt
      )
    })
    
    return(data)
    
  })
}
