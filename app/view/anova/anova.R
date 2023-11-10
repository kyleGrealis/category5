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
  app/logic/callout,
  app/logic/plotCard,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "ANOVA",
    layout_sidebar(
      sidebar = sidebar(
        class = "my-sidebar",
        
        
      ),
      callout$anova,
      callout$app_note,
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    

  })
}
