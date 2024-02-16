
box::use(
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive, tagList],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    "AVONA",
    radioButtons(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.025, 0.05),
      selected=0.05
    ),
    radioButtons(
      inputId=ns("effect"),
      label="Effect size",
      selected=0.25,
      choiceNames=list(
        "Small (0.1)", "Medium (0.25)", "Large (0.4)"
      ),
      choiceValues=list(0.1, 0.25, 0.4)
    ),
    numericInput(
      ns("n"), "Number of tests (per group)",
      min = 5, max = 600, step = 5, value = 65
    ),
    numericInput(
      ns("k"), "Number of groups (classes)",
      min = 2, max = 6, step = 1, value = 2
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
        k      = input$k
      )
    })
  })
}
