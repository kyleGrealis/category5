box::use(
  shiny[moduleServer, NS, reactive, req,observeEvent],
)

box::use(
  app/logic/glm,
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
      
      glm$glm_table(
        alpha=inputs()$alpha,
        u=inputs()$u,
        n=inputs()$n
      )
    })
    
    return(data)
    
  })
}
