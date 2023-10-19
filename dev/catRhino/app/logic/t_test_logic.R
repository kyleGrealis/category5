box::use(
  bslib[card, card_header, card_footer],
  echarts4r[echarts4rOutput],
  shiny.blueprint[Button, Callout, Divider, triggerEvent]
)


box::use(
  app/logic/plots
)

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
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(textOutput(headerTextOutput)),
    bslib::card_footer(footerTextOutput),
    echarts4rOutput(displayedPlot)
  )
}


