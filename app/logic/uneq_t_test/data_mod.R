box::use(
  shiny[moduleServer, NS, tagList, reactive, renderUI, uiOutput, req],
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
    
    # if (inputs()$group1_n < 2 | inputs()$group1_n > 100 ) {
    #   validate("Please select a Group 1 sample size between 2 and 100.")
    # }
    # if (inputs()$group2_n < 2 | inputs()$group2_n > 100 ) {
    #   validate("Please select a Group 2 sample size between 2 and 100.")
    # }
    
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
