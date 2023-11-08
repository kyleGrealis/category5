box::use(
  shiny[moduleServer, NS, reactive, req],
)

box::use(
  app/logic/chisq/functions,
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
    })
    
    functions$chisq_table(
      inputs()$alpha, inputs()$effect,
      inputs()$n, inputs()$df
    )
    
    return(data)
    
  })
}