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
  app/logic/plots,

  app/view/sidebar_buttons[extra_buttons],
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
        extra_buttons
      ),
      callout$anova,
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
      callout$app_note,

      layout_columns(

        # left box
        value_box(
          title = "Measurable effect size:",
          value = textOutput(ns("effectSize")),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "left-box"
        ),

        # center box
        value_box(
          title = "Sample size per group:",
          value = textOutput(ns("minSampleSize")),
          showcase = bsicons::bs_icon("people-fill"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "center-box"
        ),

        # right box
        value_box(
          title = "Proposed study power is:",
          value = textOutput(ns("sampleResult")),
          showcase = bsicons::bs_icon("bullseye"),
          theme = "white", full_screen = FALSE, fill = TRUE, height = 100L,
          class = "right-box"
        )
      ) # layout_columns
    ),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    alpha  <- reactive(input$alpha)
    nTests <- reactive(input$nTests)
    k      <- reactive(input$k)
    effect <- reactive(input$effect)

    pwrTable <- reactive({
      expand.grid(
        alpha = c(0.01, 0.025, 0.05),
        effectSize = seq(0.0, 0.6, by = 0.01),
        k = seq(2, 6, by = 1),
        nTests = seq(2, 80, by = 2)
      ) |>
        mutate(
          power = pwr::pwr.anova.test(
            sig.level = alpha,
            n = nTests,
            k = k,
            f = effectSize,
            power = NULL
          )$power,
          power = round(power, 2) * 100,
        ) |>
        filter(between(power, 40, 99)) |>
        na.omit()
    })

    # left plot
    output$power <- renderEcharts4r({
      # validate selected sample size
      if (is.na(k()) | k() < 2 | k() > 6) {
        validate("Please select a between 2-6 groups.")
      } else if (is.na(nTests()) | nTests() < 2 | nTests() > 80) {
        validate("Please select btween 2-80 tests.")
      }
      plots$anovaLeft(pwrTable(), nTests(), k(), effect(), alpha())
    })

    output$leftCardHeader <- renderText({
      if (is.na(nTests()) | nTests() < 2 | nTests() > 80) {
        validate("Invalid entry!")
      }
      glue("Number of Tests: {nTests()} (± 10)")
    })


    # right plot
    output$effect <- renderEcharts4r({
      plots$anovaRight(pwrTable(), nTests(), k(), effect(), alpha())
    })

    output$rightCardHeader <- renderText({
      glue("Selected effect size: {effect()}")
    })


    # left value box
    output$effectSize <- renderText({effect()})

    # this section is for comparing the selected sample size against the
    # required sample size to achieve >= 80%. a minimal sample size is
    # calculated and displayed in the middle output box, front side. lastly,
    # the minimal calculated sample size is compared to the user-selected
    # sample size to finally render "good" or "too low"
    comparisonTable <- reactive({
      pwrTable() |>
        filter(
          power >= 0.8,                    # for minimal acceptable power
          alpha == alpha(),            # user input
          # effectSize == effect(),      # user input
          k == k()
        ) |>
        arrange(power, effectSize)
    })

    # middle value_box
    output$minSampleSize <- renderText({
      comparisonTable() |>
        pull(nTests) |>
        head(1)
    })

    # right value_box
    studyNTestsNeeded <- reactive({
      pwrTable() |>
        filter(
          power >= 0.8,
          alpha == alpha(),
          # effectSize == effect(),
          k == k()
        ) |>
        arrange(power, effectSize) |>
        pull(nTests) |>
        head(1)
    })

    output$sampleResult <- renderText({
      if (is.na(k()) | k() < 2 | k() > 6) {
        validate("Invalid entry!")
      } else if (studyNTestsNeeded() <= k()) {
        "GOOD"
      } else {
        "LOW!!"
      }
    })

  })
}
