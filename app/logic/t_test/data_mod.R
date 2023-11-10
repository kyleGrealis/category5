box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/t_test/functions
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
      req(inputs()$n)
      req(inputs()$type)
      req(inputs()$alt)
      
      functions$t_table(
        alpha=inputs()$alpha, 
        n=inputs()$n,
        type=inputs()$type, 
        alt=inputs()$alt
      )
    })

    return(data)
    
  })
}
