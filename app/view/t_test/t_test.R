box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer, layout_columns, value_box],
  bsicons[bs_icon],
  shiny[h4, moduleServer, NS, reactive, validate, selectInput, textOutput, 
        renderText, observeEvent, updateNumericInput],
)

box::use(
  app/logic/callout,
  app/logic/effect[effect_table],
  app/logic/t_test/functions,
  app/logic/plots,

  app/view/t_test/inputs_mod,
  app/view/t_test/leftPlot_mod,
  app/view/t_test/rightPlot_mod,
  app/view/sidebar_buttons[extra_buttons],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Means",
    layout_sidebar(
      sidebar = sidebar(
        class = "my-sidebar",
        inputs_mod$ui(ns("t_test")),
        extra_buttons,
      ),
      callout$ttest,
      layout_column_wrap(
        width = 1/2,
        h4("Left plot here"),
        # plots$plotting_cards(
        #   textOutput(ns("leftCardHeader")),
        #   leftPlot_mod$ui(ns("plot"))
        # ),
        h4("Right plot here")
        # plots$plotting_cards(
        #   textOutput(ns("rightCardHeader")),
        #   rightPlot_mod$server(ns("plot"))
        # )
      ), # layout_column_wrap
      callout$app_note,

      layout_columns(

        # left box
        value_box(
          title = "Measurable effect size:",
          value = textOutput(ns("effectSize")),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "left-box"
        ),

        # center box
        value_box(
          title = "Sample size per group:",
          value = textOutput(ns("minSampleSize")),
          showcase = bsicons::bs_icon("people-fill"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "center-box"
        ),

        # right box
        value_box(
          title = "Proposed study power is:",
          value = textOutput(ns("sampleResult")),
          showcase = bsicons::bs_icon("bullseye"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "right-box"
        )
      ) # layout_columns
    ),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # read in inputs
    inputs <- inputs_mod$server("t_test")
    
    # pass inputs to function that creates the data
    data <- reactive ({
      functions$t_table(
        inputs$alpha(), inputs$effect(), inputs$n(),
        inputs$type(), inputs$alt()
      )
    })
    
    # leftPlot_mod$server("plot", data, inputs)
    # rightPlot_mod$server("plot", data, n, d)
    

  })
}
