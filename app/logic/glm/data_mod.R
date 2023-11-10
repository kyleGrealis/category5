box::use(
  shiny[moduleServer, NS, reactive, req, observeEvent],
)

box::use(
  app/logic/glm/functions,
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
      req(inputs()$u)
      
      functions$glm_table(
        alpha=inputs()$alpha,
        v=inputs()$n - inputs()$u -1, 
        u=inputs()$u
      )
    })
    
    return(data)
    
  })
}
