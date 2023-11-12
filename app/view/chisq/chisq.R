box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer, layout_columns, value_box],
  bsicons[bs_icon],
  shiny[moduleServer, NS, reactive, validate, selectInput, textOutput,
        withMathJax],
)

box::use(
  app/logic/callout,
  app/logic/chisq,
  app/logic/plotCard,

  app/view/chisq/inputs_mod,
  app/view/chisq/data_mod,
  app/view/chisq/leftPlot_mod,
  app/view/chisq/rightPlot_mod,
  app/view/chisq/effectCard_mod,
  app/view/chisq/minSampleCard_mod,
  app/view/chisq/compareCard_mod,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    
    withMathJax("$$X^2$$"),
    
    layout_sidebar(
      sidebar=sidebar(
        class="my-sidebar",
        inputs_mod$ui(ns("userInputs"))
      ),
      
      callout$chi_sq,
      
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
