box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer,

        layout_columns, value_box],
  bsicons[bs_icon],
  echarts4r[echarts4rOutput, renderEcharts4r],
  glue[glue],
  shiny[moduleServer, NS, reactive, withMathJax, validate, div, a,
        selectInput, numericInput, textOutput, renderText],
  shiny.blueprint[Callout]
)

box::use(
  app/logic/callout,
  app/logic/plots,
  app/logic/sidebar_buttons[extra_buttons],
  app/logic/text[app_note]
)


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
        extra_buttons
      ),
      callout$ttest,
      layout_column_wrap(
        width = 1/2,
        plots$plotting_cards(
          card_header(textOutput(ns("leftCardHeader"))),
          echarts4rOutput(ns("power"))
        ),
        plots$plotting_cards(
          card_header(textOutput(ns("rightCardHeader"))),
          echarts4rOutput(ns("effect"))
        )
      ), # layout_column_wrap
      app_note,

      # TEST: adding the flip boxes (will look like shit until CSS)
      layout_columns(
        # layout_column_wrap(
        #   fillable = FALSE,
        #   width = 1/3,

        # left box
        div( # flip-box
          div( # flip-box-inner
            div( # flip-box-front
              value_box(
                title = "Overall measurable effect size",
                value = textOutput("effectSize"),
                showcase = bsicons::bs_icon("graph-up-arrow"),
                theme = "white",
                class = "left-box"
              ),
              class = "flip-box-front"
            ),
            # back side of left card
            div(
              value_box(
                fill = FALSE,
                title = "Results are based on the `pwr` package by Clay Ford.
              Refer to this vignette:",
              value =
                a(
                  "Getting started with the pwr package",
                  href = "http://cran.nexr.com/web/packages/pwr/vignettes/pwr-vignette.html",
                  target = "_blank",
                  class = "my-links"
                ),
              showcase = bsicons::bs_icon("graph-up-arrow"),
              theme = "white",
              class = "left-box"
              ),
              class = "flip-box-back"
            ),
            class = "flip-box-inner"
          ),
          class = "flip-box"
        ),

        # center box
        div( # flip-box
          div( # flip-box-inner
            div( # flip-box-front
              value_box(
                title = "Minimal sample size per group",
                value = textOutput("minSampleSize"),
                showcase = bsicons::bs_icon("people-fill"),
                theme = "white",
                class = "center-box"
              ),
              class = "flip-box-front"
            ),
            # back side of center card
            div(
              value_box(
                title = "Study design links:",
                value = textOutput("sampleCardBack"),
                showcase = bsicons::bs_icon("people-fill"),
                theme = "white",
                class = "center-box"
              ),
              class = "flip-box-back"
            ),
            class = "flip-box-inner"
          ),
          class = "flip-box"
        ),

        # right box
        value_box(
          title = "Your proposed study power will be:",
          value = textOutput("sampleResult"),
          showcase = bsicons::bs_icon("bullseye"),
          theme = "black",
          class = "right-box"
        )
      ) # layout_columns
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
