box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer, layout_columns, value_box],
  bsicons[bs_icon],
  shiny[moduleServer, NS, reactive, validate, selectInput, textOutput],
)

box::use(
  app/logic/callout,
  app/logic/effect[effect_table],
  app/logic/plots,
  
  app/logic/t_test/data_mod,
  app/logic/t_test/functions,

  app/view/t_test/inputs_mod,
  app/view/t_test/leftPlot_mod,
  app/view/t_test/rightPlot_mod,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Means",
    layout_sidebar(
      sidebar = sidebar(
        class = "my-sidebar",
        inputs_mod$ui(ns("userInputs"))
      ),
      callout$ttest,
      layout_column_wrap(
        width = 1/2,
        leftPlot_mod$ui(ns("plot")),
        rightPlot_mod$ui(ns("plot"))
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
    
    inputs <- inputs_mod$server("userInputs")
    data <- data_mod$server("data", inputs)
    
    leftPlot_mod$server("plot", data, inputs)
    rightPlot_mod$server("plot", data, inputs)
    
    # effectCard_mod$server("card", inputs)
    # minSampleCard_mod$server("card", inputs)
    # compareCard_mod$server("card", inputs)
    
  })
  
  
}
