
box::use(
  bslib[card, card_header, card_body],
  dplyr[filter, group_by, mutate, near],
)

# this function will create a card container with a custom heading,
# footer, and plot
#' @export
plotting_cards <- function(headerTextOutput, displayedPlot) {
  card(
    class = "plotting_cards",
    full_screen = TRUE,
    card_header(headerTextOutput),
    card_body(displayedPlot, max_height = "350px"),
  )
}

