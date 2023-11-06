#' User inputs specific to t-test
#' 
#' @param alpha Significance level
#' @param effect Effect size
#' @param group1_n Group 1 sample size
#' @param group2_n Group 2 sample size
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
      ns("group1_n"), "Sample size (group 1)",
      min=2, max=150, value=65, step=1
    ),
    numericInput(
      ns("group2_n"), "Sample size (group 2)",
      min=2, max=150, value=70, step=1
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
        alpha    = as.numeric(input$alpha),
        effect   = as.numeric(input$effect),
        group1_n = input$group1_n,
        group2_n = input$group2_n,
        alt      = input$alt
      )
    })
  })
}
