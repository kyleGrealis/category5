box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer, layout_columns, value_box],
  bsicons[bs_icon],
  dplyr[arrange, between, filter, mutate, pull],
  echarts4r[echarts4rOutput, renderEcharts4r],
  glue[glue],
  pwr[pwr.t.test],
  shiny[moduleServer, NS, reactive, withMathJax, validate, div, a,
        selectInput, numericInput, textOutput, renderText,
        observeEvent, updateNumericInput],
  shiny.blueprint[Callout],
  stats[na.omit],
  utils[head]
)

box::use(
  app/logic/anova,
  app/logic/callout,

  app/view/anova/inputs_mod,
  app/view/anova/data_mod,
  app/view/anova/leftPlot_mod,
  app/view/anova/rightPlot_mod,
  app/view/anova/effectCard_mod,
  app/view/anova/minSampleCard_mod,
  app/view/anova/compareCard_mod,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    
    "ANOVA",
    
    layout_sidebar(
      sidebar=sidebar(
        class="my-sidebar",
        inputs_mod$ui(ns("userInputs"))
      ),
      
      callout$anova,
      
      layout_column_wrap(
        width=1/2,
        leftPlot_mod$ui(ns("plot")),
        rightPlot_mod$ui(ns("plot"))
      ),
      
      callout$app_note,
      
      layout_columns(
        effectCard_mod$ui(ns("info")),
        minSampleCard_mod$ui(ns("info")),
        compareCard_mod$ui(ns("info"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    inputs <- inputs_mod$server("userInputs")
    data <- data_mod$server("data", inputs)
    
    leftPlot_mod$server("plot", data, inputs)
    rightPlot_mod$server("plot", data, inputs)
    
    effectCard_mod$server("info", inputs)
    minSampleCard_mod$server("info", inputs)
    compareCard_mod$server("info", inputs)

  })
}
