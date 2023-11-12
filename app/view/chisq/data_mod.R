box::use(
  shiny[moduleServer, NS, reactive, req, observeEvent],
)

box::use(
  app/logic/chisq,
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
      req(inputs()$df)
      
      chisq$chisq_table(
        alpha=inputs()$alpha,
        n=inputs()$n, 
        df=inputs()$df
      )
    })
    
    return(data)
    
  })
}
