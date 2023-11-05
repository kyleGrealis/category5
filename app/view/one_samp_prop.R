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
    "One sample proportion",
    layout_sidebar(
      sidebar = sidebar(
        class = "my-sidebar",
        selectInput(
          ns("alpha"), "Significance level",
          choices = c(0.01, 0.025, 0.05),
          selected = 0.05
        ),
        numericInput(
          "sampleObs", "Number of tests",
          min = 20, max = 700, step = 5, value = 100
        ),
        numericInput(
          "p1_p", "Null hypothesis proportion",
          min = 0, max = 1, step = 0.05, value = 0.55
        ),
        numericInput(
          "p2_p", "Alternative hypothesis proportion",
          min = 0, max = 1, step = 0.05, value = 0.5
        ),
        numericInput(
          "hEffectSize_p", "Desired effect size",
          min = 0, max = 3, step = 0.1, value = 0.5
        ),
        selectInput(
          "alternative_p", "Alternative hypothesis type",
          choices = c(
            "Two-sided" = "two.sided",
            "Less than the null" = "less",
            "Greater than the null" = "greater"
          ),
          selected = "two.sided"
        ),
      ),
      callout$one_samp_prop,
      callout$app_note,
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    

  })
}
