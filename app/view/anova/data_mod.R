box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/anova
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
      
      anova$anova_table(
        alpha=inputs()$alpha, 
        n=inputs()$n, 
        k=inputs()$k
      )
    })

    return(data)
    
  })
}
