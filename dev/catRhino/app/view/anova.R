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
          ns("effect"), "Desired effect size",
          min = 0.1, max = 2, step = 0.1, value = 0.5
        ),
        numericInput(
          ns("sample"), "Number of tests",
          min = 20, max = 700, step = 5, value = 100
        ),
        numericInput(
          ns("groups"), "Number of groups (classes)",
          min = 2, max = 6, step = 1, value = 2
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

    sample <- reactive(input$sample)
    alpha  <- reactive(input$alpha)
    effect <- reactive(input$effect)

    pwrTable <- reactive({
      expand.grid(
        alpha = c(0.01, 0.025, 0.05),
        effectSize = seq(0.0, 2, by = 0.05),
        sampleSize = seq(2, sample()+100, by = 1)
      ) |>
        mutate(
          power = pwr.t.test(
            n = sampleSize,
            d = effectSize,
            sig.level = alpha,
            power = NULL
          )$power,
          power = round(power, 2)
        ) |>
        filter(between(power, 0.6, 0.99)) |>
        na.omit()
    })

    # ensure that the maximum selectable effect size does not exceed
    # what values are available
    observeEvent(effect(), {
      updateNumericInput(inputId = "effect",
                         max = max(pwrTable()$effectSize)-0.1)
    })

    # left plot
    output$power <- renderEcharts4r({
      # validate selected sample size
      if (is.na(sample()) | sample() < 4 | sample() > 700 ) {
        validate("Please select a per-group sample size between 4 and 700.")
      }
      plots$ttest_left(pwrTable(), sample(), alpha())
    })

    output$leftCardHeader <- renderText({
      if (is.na(sample()) | sample() < 4 | sample() > 700 ) {
        validate("Invalid entry!")
      }
      glue("Sample size: {sample()} (± 20) per group")
    })


    # right plot
    output$effect <- renderEcharts4r({
      if (min(pwrTable()$effectSize) > effect() |
          max(pwrTable()$effectSize) < effect()) {
        validate("Please choose another effect size to render a plot.")
      }
      plots$right_plot(pwrTable(), effect(), alpha())
    })

    output$rightCardHeader <- renderText({
      if (is.na(effect()) | effect() < 0.1 | effect() > 3 ) {
        validate("Invalid entry!")
      }
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
          effectSize >= effect(),      # user input
        ) |>
        arrange(power, effectSize)
    })

    # middle value_box
    output$minSampleSize <- renderText({
      comparisonTable() |>
        pull(sampleSize) |>
        head(1)
    })

    # right value_box
    studySampleNeeded <- reactive({
      pwrTable() |>
        filter(
          power >= 0.8,
          alpha == alpha(),
          effectSize >= effect()
        ) |>
        arrange(power, effectSize) |>
        pull(sampleSize) |>
        head(1)
    })

    output$sampleResult <- renderText({
      if (is.na(sample()) | sample() < 4 | sample() > 700 ) {
        validate("Invalid entry!")
      } else if (is.na(effect()) | effect() < 0.1 | effect() > 3 ) {
        validate("Invalid entry!")
      } else if (studySampleNeeded() <= sample()) {
        "GOOD"
      } else {
        "LOW!!"
      }
    })

  })
}
