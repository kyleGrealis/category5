box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer, layout_columns, value_box],
  bsicons[bs_icon],
  shiny[moduleServer, NS, reactive, validate, selectInput, textOutput],
)

box::use(
  app/logic/callout,
  app/logic/plotCard,
  
  app/view/uneq_t_test/inputs_mod,
  app/view/uneq_t_test/data_mod,
  app/view/uneq_t_test/leftPlot_mod,
  app/view/uneq_t_test/rightPlot_mod,
  app/view/uneq_t_test/effectCard_mod,
  app/view/uneq_t_test/groupsSizesCard_mod,
  app/view/uneq_t_test/compareCard_mod,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    
    "Unequal sample sizes",
    
    layout_sidebar(
      sidebar=sidebar(
        class="my-sidebar",
        inputs_mod$ui(ns("userInputs"))
      ),
      
      callout$uneq_t_test,
      
      layout_column_wrap(
        width=1/2,
        leftPlot_mod$ui(ns("plot")),
        rightPlot_mod$ui(ns("plot"))
      ),
      
      callout$app_note,
      
      layout_columns(
        effectCard_mod$ui(ns("info")),
        groupsSizesCard_mod$ui(ns("info")),
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
    groupsSizesCard_mod$server("info", inputs)
    compareCard_mod$server("info", inputs)

  })
}
