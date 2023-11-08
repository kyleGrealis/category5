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
    "GLM",
    layout_sidebar(
      sidebar = sidebar(
        class = "my-sidebar",
        selectInput(
          ns("alpha"), "Significance level",
          choices = c(0.01, 0.025, 0.05),
          selected = 0.05
        ),
        numericInput(
          ns("effect"), "Desired effect size",
          min = 0.1, max = 3.0, step = 0.1, value = 0.5
        ),
        numericInput(
          ns("sample"), "Sample size per group",
          min = 20, max = 700, step = 5, value = 100
        ),
        selectInput(
          ns("testType"), "t-test type",
          choices = c(
            "Two sample" = "two.sample",
            "One sample" = "one.sample",
            "Paired" = "paired"
          )
        ),
        selectInput(
          ns("alternative"), "Alternative hypothesis type",
          choices = c(
            "Two-sided" = "two.sided",
            "Greater than the null" = "greater"
          ),
          selected = "two.sided"
        ),
      ),
      callout$glm,
      callout$app_note,
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    

  })
}