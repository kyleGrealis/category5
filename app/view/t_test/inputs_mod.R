#' User inputs specific to t-test
#' 
#' @param alpha Significance level
#' @param effect Effect size
#' @param n Group sample size
#' @param type t-test type: one- or two-sample, or paired
#' @param alt Alternative hypothesis. Default is two.sided
#' 
#' @return list of input parameters as reactives

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
      selected=0.5,
      choiceNames=list(
        "Small (0.2)", "Medium (0.5)", "Large (0.8)"
      ),
      choiceValues=list(0.2, 0.5, 0.8)
    ),
    helpMe_mod$ui(ns("help")),
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
        alpha  = input$alpha,
        effect = input$effect,
        n      = input$n,
        type   = input$type,
        alt    = input$alt
      )
    })
  })
}
