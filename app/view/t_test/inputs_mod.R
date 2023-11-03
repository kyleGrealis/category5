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
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive,
        selectInput, tagList],
  shiny.blueprint[Radio, RadioGroup.shinyInput]
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.25, 0.05),
      selected=0.05
    ),
    h6("Effect size"),
    RadioGroup.shinyInput(
      inputId=ns("effect"),
      value=0.3,
      Radio(label="Small (0.1)", value=0.1),
      Radio(label="Medium (0.3)", value=0.3),
      Radio(label="Large (0.5)", value=0.5)
    ),
    numericInput(
      ns("n"), "Sample size per group",
      min=1, max=150, value=70
    ),
    # 
    h6("t-test type"),
    RadioGroup.shinyInput(
      inputId=ns("type"),
      value="one.sample",
      Radio(label="One sample", value="one.sample"),
      Radio(label="Two sample", value="two.sample"),
      Radio(label="Paired", value="paired")
    ),
    selectInput(
      ns("alt"), "Alternative hypothesis type",
      choices=c(
        "Two-sided"="two.sided",
        "Greater than the null"="greater",
        "Less than the null"="less"
      ),
      selected="two.sided"
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      list(
        alpha=reactive({ input$alpha }),
        effect=reactive({ input$effect }),
        n=reactive({ input$n }),
        type=reactive({ input$type }),
        alt=reactive({ input$alt })
      )
    )
  })
}
