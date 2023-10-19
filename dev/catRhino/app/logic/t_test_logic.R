box::use(
  bslib[layout_sidebar, page_navbar, navset_underline, layout_column_wrap,
        nav_spacer, nav_menu, nav_panel, sidebar, card, card_header,
        card_footer, layout_columns, value_box],
  dplyr[between, filter, mutate],
  echarts4r,
  htmlwidgets[JS],
  pwr,
  shiny[selectInput, numericInput, textOutput, ...],
  stats[na.omit],
)


box::use(
  # app/logic/data_tables,
  app/logic/plots
)


# create the sidebar ------------------------------------------------------



#' @export
extra_sidebar_buttons <- list(
  shiny.blueprint::ButtonGroup(
    class = "sidebar-buttons",
    minimal = TRUE,
    shiny.blueprint::Button(
      onClick = shiny.blueprint::triggerEvent("reset"),
      icon = "refresh",
      "Reset"
    ),
    shiny.blueprint::Divider(),
    shiny.blueprint::Button(
      onClick = shiny.blueprint::triggerEvent("help_me"),
      icon = "lightbulb",
      "Help!"
    )
  )
)

#' @export
callout <- shiny.blueprint::Callout(
  title = "This is where the callout will live.",
  "Inside this callout block, I am planning to provide an example of ",
  "the selected statistical test. For example, describe what a test of ",
  "means is with a quick use case."
)





plotting_cards <- function(headerTextOutput, footerTextOutput, displayedPlot) {
  card(
    full_screen = TRUE,
    card_header(textOutput(headerTextOutput)),
    card_footer(footerTextOutput),
    echarts4r$echarts4rOutput(displayedPlot)
  )
}




display_here <- list(


  layout_column_wrap(
    width = 1/2,

    # left plot
    # card(
    #   full_screen = TRUE,
    #   card_header(textOutput("leftCardHeader")),
    #   card_footer(
    #     "Displaying your desired sample size and ± 20 participants per group."
    #   ),
    #   echarts4rOutput("power"),
    # ),
    plotting_cards(
      "leftCardHeader",
      "Displaying your desired sample size and ± 20 participants per group.",
      "power"
    ),

    # right plot
    # card(
    #   full_screen = TRUE,
    #   card_header(textOutput("rightCardHeader")),
    #   card_footer(
    #     "The effect size line displays the necessary sample size and power."
    #   ),
    #   echarts4rOutput("power2"),
    # ),
    plotting_cards(
      "rightCardHeader",
      "The effect size line displays the necessary sample size and power.",
      "power2"
    )
  ), # layout_columns


  # simple dialogue between plots and flip cards
  p("To achieve at least 80% power, your study will need:")
)


