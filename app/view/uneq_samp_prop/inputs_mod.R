
box::use(
  glue[glue],
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive,
        selectInput, tagList, renderText, textOutput],
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    "Unequal sample",
    radioButtons(
      ns("alpha"), "Significance level",
      choices=c(0.01, 0.025, 0.05),
      selected=0.05
    ),
    radioButtons(
      inputId=ns("effect"), 
      label="Effect size",
      selected=0.5, # medium effect 
      choiceNames=list(
        "Small (0.2)", "Medium (0.5)", "Large (0.8)"
      ),
      choiceValues=list(0.2, 0.5, 0.8)
    ),
    numericInput(
      ns("n1"), "Sample size (group 1)",
      min=2, max=150, value=65, step=1
    ),
    numericInput(
      ns("n2"), "Sample size (group 2)",
      min=2, max=150, value=70, step=1
    ),
    numericInput(
      ns("p1"), "Null hypothesis proportion",
      min=0, max=1, step=0.05, value=0.5
    ),
    numericInput(
      ns("p2"), "Alternative hypothesis proportion",
      min=0, max=1, step=0.05, value=0.75
    ),
    selectInput(
      ns("alt"), "Alternative hypothesis type",
      choices=c(
        "Two-sided"="two.sided",
        "> than the null"="greater",
        "< than the null"="less"
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
        n1     = as.numeric(input$n1),
        n2     = as.numeric(input$n2),
        p1     = as.numeric(input$p1),
        p2     = as.numeric(input$p2),
        alt    = input$alt
      )
    })
  })
}
