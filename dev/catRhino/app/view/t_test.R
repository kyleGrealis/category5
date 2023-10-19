box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer],
  echarts4r[echarts4rOutput, renderEcharts4r],
  glue[glue],
  shiny[moduleServer, NS, reactive, withMathJax, validate, div,
        selectInput, numericInput, textOutput, renderText],
  shiny.blueprint[Callout]
)

box::use(
  app/logic/data_tables[app_note],
  app/logic/plots,
  app/logic/t_test_logic
)

display_card <- function(...) card(full_screen = TRUE, ...)

#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Means",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          ns("alpha"), "Choose your significance level",
          choices = c(0.01, 0.025, 0.05),
          selected = 0.05
        ),
        numericInput(
          ns("effect"), "Choose the desired effect size",
          min = 0.1, max = 1.0, step = 0.05, value = 0.5
        ),
        numericInput(
          ns("sample"), "Choose the sample size per group",
          min = 20, max = 300, step = 5, value = 100
        ),
        t_test_logic$extra_sidebar_buttons
      ),
      t_test_logic$callout,
      layout_column_wrap(
        width = 1/2,
        display_card(
          card_header(textOutput(ns("leftCardHeader"))),
          echarts4rOutput(ns("power"))
        ),
        display_card(
          card_header(textOutput(ns("rightCardHeader"))),
          echarts4rOutput(ns("effect"))
        ),
        app_note
      )
    ),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # left plot
    output$power <- renderEcharts4r({
      # validate selected sample size
      if (input$sample < 2 | input$sample > 700 ) {
        validate("Please select a per-group sample size between 2 and 700.")
      }
      plots$left_plot(input$sample, input$alpha)
    })

    output$leftCardHeader <- renderText({
      glue("Sample size: {input$sample} (± 20) per group")
    })

    # right plot
    output$effect <- renderEcharts4r({
      plots$right_plot(input$effect, input$alpha)
    })

    output$rightCardHeader <- renderText({
      glue("Selected effect size: {input$effect}")
    })
  })
}
