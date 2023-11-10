box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/anova/functions
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
      req(inputs()$k)
      
      functions$anova_table(
        alpha=inputs()$alpha, 
        effect=inputs()$effect, 
        n=inputs()$n, 
        k=inputs()$k
      )
    })

    return(data)
    
  })
}
