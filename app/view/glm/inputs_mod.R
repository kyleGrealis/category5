
box::use(
  glue[glue],
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive, tagList,
        renderText, textOutput],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    "Generalized Linear Models",
    radioButtons(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.025, 0.05),
      selected=0.05
    ),
    radioButtons(
      inputId=ns("effect"),                 # f2 = (R^2)/(1-(R^2))
      label="Effect size",
      selected=0.15, # medium effect 
      choiceNames=list(
        "Small (0.02)", "Medium (0.15)", "Large (0.35)"
      ),
      choiceValues=list(0.02, 0.15, 0.35)
    ),
    numericInput(
      ns("n"), "Sample size",
      min = 0, max = 1000, step = 5, value = 100
    ),
    numericInput(
      ns("u"), "Number of variables in the model", 
      min = 1, max = 100, step = 1, value = 2, 
    ),
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        alpha  = as.numeric(input$alpha),
        effect = as.numeric(input$effect),
        n      = input$n,
        u      = input$u
      )
    })
  })
}
