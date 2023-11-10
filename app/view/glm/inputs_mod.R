box::use( 
  app/view/helpMe_mod
)

box::use(
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive,
        selectInput, tagList],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.025, 0.05),
      selected=0.05
    ),
    numericInput(
      ns("rsq"), "Anticipated model R-squared",
      min=0, max=1, step=0.5, value=0.5
    ),
    textOutput(ns("rsq")),
    radioButtons(
      inputId=ns("effect"),                 # f2 = (R^2)/(1-(R^2))
      label="Effect size",
      selected=0.3, # medium effect 
      choiceNames=list(
        "Small (0.02)", "Medium (0.15)", "Large (0.35)"
      ),
      choiceValues=list(0.02, 0.15, 0.35)
    ),
    helpMe_mod$ui(ns("help")),
    numericInput(
      "n", "Sample size",
      min = 1, max = 700, step = 1, value = 50
    ),
    numericInput(
      "u", "Number of variables in the model", 
      min = 1, max = 100, step = 1, value = 1, 
    ),
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rsq <- reactive({
      round(
        (input$rsq^2) / (1 - (input$rsq^2)),
        digits = 2
      )
    })
    output$rsq <- renderText({glue("Your estimated sample size is: {rsq()}")})
    helpMe_mod$server("help")
    
    reactive({
      list(
        alpha  = as.numeric(input$alpha),
        effect = as.numeric(input$effect),
        n      = input$n,
        u    = input$u
      )
    })
  })
}
