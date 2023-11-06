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
        selectInput(
          ns("alpha"), "Significance level",
          choices = c(0.01, 0.025, 0.05),
          selected = 0.05
        ),
        numericInput(
          ns("nTests"), "Number of tests",
          min = 2, max = 80, step = 2, value = 70
        ),
        numericInput(
          ns("k"), "Number of groups (classes)",
          min = 2, max = 6, step = 1, value = 2
        ),
        selectInput(
          ns("effectSize"), "Desired effect size",
          choices = c(
            "Small" = 0.1,
            "Medium" = 0.25,
            "Large" = 0.4
          ),
          selected = 0.25
        ),
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
