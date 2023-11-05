box::use(
  shiny[NS, tags, a, br, div, img, moduleServer, observeEvent, reactiveVal,
        tagList],
  shiny.fluent[PrimaryButton.shinyInput, reactOutput, Callout, renderReact,
               Modal, Stack, IconButton.shinyInput, Text]
)

ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactOutput(ns("modal")),
    PrimaryButton.shinyInput(
      ns("showModal"), 
      text = "Help me choose",
      style="background-color: #f3f2f1; color: black; border: 1px solid gray;"
    ),
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    modalVisible <- reactiveVal(FALSE)
    observeEvent(input$showModal, modalVisible(TRUE))
    observeEvent(input$hideModal, modalVisible(FALSE))
    output$modal <- renderReact({
      Modal(
        isOpen = modalVisible(),
        Stack(
          tokens = list(padding = "15px", childrenGap = "10px"),
          div(
            style = list(display = "flex"),
            Text(
              "About effect sizes", 
              variant = "large", 
              style="font-weight: bold"
            ),
            div(style = list(flexGrow = 1)),
            IconButton.shinyInput(
              ns("hideModal"),
              iconProps = list(iconName = "Cancel"), 
              style="color: #005030"
            )
          ),
          tags$div(
            style = "margin: 10px",
            "Please refer here for more information regarding the ",
            tags$a(
              href="http://cran.nexr.com/web/packages/pwr/vignettes/pwr-vignette.html",
              "{pwr} R package",
              target="_blank",
              style="text-decoration: none; color: #005030"
            ),
            tags$br(), tags$br(),
            '"Let’s say we suspect we have a loaded coin that lands heads 75%
            of the time instead of the expected 50%. We wish to create an
            experiment to test this..."',
            tags$br(),
            '"Cohen describes effect size as "the degree to which the null
            hypothesis is false." In our coin flipping example, this is the
            difference between 75% and 50%. We could say the effect was 25% but
            recall we had to transform the absolute difference in proportions
            to another quantity using the ES.h function. This is a crucial part
            of using the {pwr} package correctly: You must provide an effect size
            on the expected scale. Doing otherwise will produce wrong sample
            size and power calculations."',
            tags$br(), tags$br(),
            tags$img(
              src="static/images/effect-sizes-table.png", 
              alt="effect-sizes-table",
              style="display: block; margin: 0 auto; max-width: 45%; border: 1px solid gray"
            )
          )
        )
      )
    })
  })
}

