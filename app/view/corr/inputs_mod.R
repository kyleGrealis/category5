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
    radioButtons(
      inputId=ns("effect"),
      label="Effect size",
      selected=0.3, # medium effect
      choiceNames=list(
        "Small (0.1)", "Medium (0.3)", "Large (0.5)"
      ),
      choiceValues=list(0.1, 0.3, 0.5)
    ),
    helpMe_mod$ui(ns("help")),
    numericInput(
      ns("n"), "Number of observations",
      min=0, max=1300, value=85, step=5
    ),
    selectInput(
      ns("alt"), "Alternative hypothesis type",
      choices=c(
        "Two-sided"="two.sided",
        "Greater than the null"="greater"
      ),
      selected="two.sided"
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    helpMe_mod$server("help")
    
    reactive({
      list(
        alpha  = as.numeric(input$alpha),
        effect = as.numeric(input$effect),
        n      = input$n,
        alt    = input$alt
      )
    })
  })
}
