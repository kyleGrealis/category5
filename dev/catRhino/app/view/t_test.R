box::use(
  bslib[page_navbar, navset_underline, nav_spacer, nav_menu, nav_panel,
        layout_sidebar, sidebar, layout_column_wrap, card, card_header,
        card_footer,

        layout_columns, value_box],
  bsicons[bs_icon],
  dplyr[arrange, filter, pull],
  echarts4r[echarts4rOutput, renderEcharts4r],
  glue[glue],
  shiny[moduleServer, NS, reactive, withMathJax, validate, div, a,
        selectInput, numericInput, textOutput, renderText],
  shiny.blueprint[Callout],
  utils[head]
)

box::use(
  app/logic/callout,
  app/logic/data_tables[pwrTable],
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
          min = 0.1, max = 3.0, step = 0.1, value = 0.5
        ),
        numericInput(
          ns("sample"), "Choose the sample size per group",
          min = 20, max = 700, step = 5, value = 100
        ),
        selectInput(
          "testType", "t-test type",
          choices = c(
            "Two sample" = "two.sample",
            "One sample" = "one.sample",
            "Paired" = "paired"
          )
        ),
        selectInput(
          "alternative", "Alternative hypothesis type",
          choices = c(
            "Two-sided" = "two.sided",
            "Greater than the null" = "greater"
          ),
          selected = "two.sided"
        ),
        extra_buttons
      ),
      callout$ttest,
      layout_column_wrap(
        width = 1/2,
        plots$plotting_cards(
          textOutput(ns("leftCardHeader")),
          echarts4rOutput(ns("power"))
        ),
        plots$plotting_cards(
          textOutput(ns("rightCardHeader")),
          echarts4rOutput(ns("effect"))
        )
      ), # layout_column_wrap
      app_note,

      # TEST: adding the flip boxes (will look like shit until CSS)
      layout_columns(

                # left box
        div( # flip-box
          div( # flip-box-inner
            div( # flip-box-front
              value_box(
                title = "Overall measurable effect size:",
                value = textOutput(ns("effectSize")),
                showcase = bsicons::bs_icon("graph-up-arrow"),
                theme = "white",
                class = "left-box"
              ),
              class = "flip-box-front"
            ),
            # back side of left card
            div(
              value_box(
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
                title = "Minimal sample size per group:",
                value = textOutput(ns("minSampleSize")),
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
                value = textOutput(ns("sampleCardBack")),
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
          value = textOutput(ns("sampleResult")),
          showcase = bsicons::bs_icon("bullseye"),
          theme = "white",
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
      if (is.na(input$sample) | input$sample < 4 | input$sample > 700 ) {
        validate("Please select a per-group sample size between 4 and 700.")
      }
      plots$left_plot(pwrTable, input$sample, input$alpha)
    })

    output$leftCardHeader <- renderText({
      if (is.na(input$sample) | input$sample < 4 | input$sample > 700 ) {
        validate("Invalid entry!")
      }
      glue("Sample size: {input$sample} (± 20) per group")
    })

    # right plot
    output$effect <- renderEcharts4r({
      if (is.na(input$effect) | input$effect < 0.1 | input$effect > 3 ) {
        validate("Invalid entry!")
      }
      plots$right_plot(pwrTable, input$effect, input$alpha)
    })

    output$rightCardHeader <- renderText({
      if (is.na(input$effect) | input$effect < 0.1 | input$effect > 3 ) {
        validate("Invalid entry!")
      }
      glue("Selected effect size: {input$effect}")
    })

    # flip boxes --------------------------------------------------------------
    # this section is for comparing the selected sample size against the
    # required sample size to achieve >= 80% holding the other variables
    # (alpha, effect size) at a constant. then, a minimal sample size is
    # calculated and displayed in the middle output box, front side. lastly,
    # the minimal calculated sample size is compared to the user-selected
    # sample size to finally render "sufficient" or "too low"
    comparisonTable <- reactive({
      pwrTable |>
        filter(
          power >= 0.8,                    # for minimal acceptable power
          alpha == input$alpha,            # user input
          effectSize >= input$effect,      # user input
        ) |>
        arrange(power, effectSize)
    })

    # front, left value box
    output$effectSize <- renderText({input$effect})

    # front, middle value_box
    output$minSampleSize <- renderText({
      comparisonTable() |>
        pull(sampleSize) |>
        head(1)
    })

    # front, right value_box
    studySampleNeeded <- reactive({
      pwrTable |>
        filter(
          power >= 0.8,
          alpha == input$alpha,
          effectSize >= input$effect
        ) |>
        arrange(power, effectSize) |>
        pull(sampleSize) |>
        head(1)
    })

    # backside of middle box
    output$sampleCardBack <- renderText({
      "...information coming soon!"
    })

    output$sampleResult <- renderText({
      if (is.na(input$sample) | input$sample < 4 | input$sample > 700 ) {
        validate("Invalid entry!")
      } else if (is.na(input$effect) | input$effect < 0.1 | input$effect > 3 ) {
        validate("Invalid entry!")
      } else if (studySampleNeeded() <= input$sample) {
        "SUFFICIENT"
      } else {
        "TOO LOW!"
      }
    })

  })
}
