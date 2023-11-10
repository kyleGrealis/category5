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
      inputId=ns("f"),
      label="Effect size",
      selected=0.5,
      choiceNames=list(
        "Small (0.1)", "Medium (0.25)", "Large (0.4)"
      ),
      choiceValues=list(0.1, 0.25, 0.4)
    ),
    helpMe_mod$ui(ns("help")),
    numericInput(
          ns("n"), "Number of tests (per group)",
          min = 2, max = 80, step = 2, value = 70
        ),
        numericInput(
          ns("k"), "Number of groups (classes)",
          min = 2, max = 6, step = 1, value = 2
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
        k      = input$k
      )
    })
  })
}
