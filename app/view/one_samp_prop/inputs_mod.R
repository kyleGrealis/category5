
box::use(
  glue[glue],
  shiny[h6, moduleServer, NS, numericInput, radioButtons, reactive,
        selectInput, tagList, renderText, textOutput],
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
      selected=0.5, # medium effect 
      choiceNames=list(
        "Small (0.2)", "Medium (0.5)", "Large (0.8)"
      ),
      choiceValues=list(0.2, 0.5, 0.8)
    ),
    numericInput(
      ns("n"), "Number of tests",
      min = 0, max = 1000, step = 5, value = 100
    ),
    numericInput(
      "p1", "Null hypothesis proportion",
      min = 0, max = 1, step = 0.05, value = 0.55
    ),
    numericInput(
      "p2", "Alternative hypothesis proportion",
      min = 0, max = 1, step = 0.05, value = 0.5
    ),
    selectInput(
      "alt", "Alternative hypothesis type",
      choices = c(
        "Two-sided" = "two.sided",
        "Greater than the null" = "greater",
        "Less than the null" = "less"
      ),
      selected = "two.sided"
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
        p1     = input$p1,
        p2     = input$p2,
        alt    = input$alt
      )
    })
  })
}
