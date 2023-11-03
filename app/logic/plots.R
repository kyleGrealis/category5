
box::use(
  bslib[card, card_header, card_body],
  dplyr[filter, group_by, mutate, near],
  # echarts4r[e_charts, e_line, e_tooltip, e_grid, e_color, e_legend,
  #           e_datazoom, e_x_axis, e_y_axis, e_axis_labels, e_mark_line,
  #           e_toolbox_feature, echarts4rOutput],
  shiny[textOutput]
)

# box::use(
#   app/logic/chart_utils[left_label_formatter, right_label_formatter]
# )

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


#' #' @export
#' anova_left <- function(data, inputSample, inputAlpha, inputGroups) {
#'   data |>
#'     filter(
#'       sampleSize %in% c(inputSample - 20, inputSample, inputSample + 20),
#'       alpha == inputAlpha,
#'       groups == inputGroups
#'     ) |>
#'     mutate(effectSize = round(effectSize, 2)) |>
#'     group_by(sampleSize, alpha) |>
#'     e_charts(effectSize) |>
#'     e_line(power) |>
#'     e_tooltip(
#'       trigger = "item",
#'       formatter = left_label_formatter
#'     ) |>
#'     e_grid(right = '15%') |>
#'     e_color(c("#f47321", "#777777", "#005030")) |>
#'     e_legend(
#'       left = '5',
#'       title = list("Sample size")
#'     ) |>
#'     e_datazoom(type = 'inside') |>
#'     e_y_axis(power) |>
#'     e_x_axis(effectSize) |>
#'     e_axis_labels(x = "Effect \nSize", y = "Power") |>
#'     e_toolbox_feature(feature = c("saveAsImage"))
#' }
#' #' @export
#' anovaLeft <- function(data, chosenTests, chosenGroups, chosenEffect, chosenAlpha) {
#'   data |>
#'     filter(
#'       nTests %in% c(chosenTests - 10, chosenTests, chosenTests + 10),
#'       alpha == chosenAlpha,
#'       k == chosenGroups
#'     ) |>
#'     mutate(effectSize = round(effectSize, 2)) |>
#'     group_by(nTests, alpha) |>
#'     e_charts(effectSize) |>
#'     e_line(power) |>
#'     e_y_axis(power) |>
#'     e_x_axis(effectSize) |>
#'     e_mark_line(
#'       data = list(yAxis = 80),
#'       lineStyle = list(color = "#ff7276", opacity = 0.75),
#'       title = "Minimum \nPower"
#'     ) |>
#'     e_mark_line(
#'       data = list(xAxis = chosenEffect),
#'       lineStyle = list(color = "#ff7276", opacity = 0.75),
#'       title = "Selected \nEffect \nSize"
#'     ) |>
#'     e_tooltip(
#'       trigger = "item",
#'       formatter = left_label_formatter
#'     ) |>
#'     e_grid(right = '15%') |>
#'     e_color(c("#f47321", "#777777", "#005030")) |>
#'     e_legend(
#'       left = '5',
#'       title = list("Number of Tests")
#'     ) |>
#'     e_datazoom(type = 'inside') |>
#'     e_axis_labels(x = "Effect \nSize", y = "Power") |>
#'     e_toolbox_feature(feature = c("saveAsImage"))
#' }
#' 
#' #' @export
#' anovaRight <- function(data, chosenTests, chosenGroups, chosenEffect, chosenAlpha) {
#'   data |>
#'     filter(
#'       # effectSize == chosenEffect,
#'       alpha == chosenAlpha,
#'       k == chosenGroups
#'     ) |>
#'     group_by(effectSize, alpha) |>
#'     e_charts(power) |>
#'     e_line(nTests) |>
#'     e_mark_line(
#'       data = list(xAxis = 80),
#'       lineStyle = list(color = "#ff7276", opacity = 0.75),
#'       title = "Minimum \nPower"
#'     ) |>
#'     # e_mark_line(
#'     #   data = list(yAxis = chosenTests),
#'     #   # lineStyle = list(color = , opacity = 0.75),
#'     #   title = "Selected \n# of Tests"
#'     # ) |>
#'     e_color("#777777") |>
#'     e_tooltip(
#'       trigger = "item",
#'       formatter = right_label_formatter
#'     ) |>
#'     e_grid(right = '15%') |>
#'     e_legend(show = FALSE) |>
#'     e_datazoom(type = 'inside') |>
#'     e_y_axis(nTests) |>
#'     e_x_axis(power) |>
#'     e_axis_labels(x = "Power", y = "Number of \nTests") |>
#'     e_toolbox_feature(feature = c("saveAsImage"))
#' }


