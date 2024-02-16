
box::use(
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive,
        selectInput, tagList],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    "t-Test",
    radioButtons(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.025, 0.05),
      selected=0.05
    ),
    radioButtons(
      inputId=ns("effect"),
      label="Effect size",
      selected=0.5,
      choiceNames=list(
        "Small (0.2)", "Medium (0.5)", "Large (0.8)"
      ),
      choiceValues=list(0.2, 0.5, 0.8)
    ),
    numericInput(
      ns("n"), "Sample size per group",
      min=10, max=300, value=100, step=5
    ),
    radioButtons(
      inputId=ns("type"),
      label="t-test type",
      selected="one.sample",
      choiceNames=list(
        "One sample",
        "Two sample",
        "Paired"
      ),
      choiceValues=list(
        "one.sample",
        "two.sample",
        "paired"
      )
    ),
    selectInput(
      ns("alt"), "Alternative hypothesis type",
      choices=c(
        "Two-sided"="two.sided",
        "> than the null"="greater"
      ),
      selected="two.sided"
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
        type   = input$type,
        alt    = input$alt
      )
    })
  })
}
