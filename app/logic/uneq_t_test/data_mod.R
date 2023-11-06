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
      req(inputs()$group1_n)
      req(inputs()$group2_n)
      req(inputs()$alt)
      
      functions$t2n_table(
        inputs()$alpha, inputs()$effect, 
        inputs()$group1_n, inputs()$group2_n, 
        inputs()$alt
      )
    })
    
    return(data)
    
  })
}
