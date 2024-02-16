
box::use(
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive, tagList],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    "Chi-squared test",
    radioButtons(
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
    numericInput(
      ns("n"), "Sample size",
      min=10, max=400, value=125, step=5
    ),
    numericInput(
      ns("df"), "Number of categories",       # df = #cat -1
      min=2, max=10, value=3, step=1
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        alpha  = as.numeric(input$alpha),
        effect = as.numeric(input$effect),
        n      = input$n,
        df     = input$df -1                  # df = #cat levels -1
      )
    })
  })
}
