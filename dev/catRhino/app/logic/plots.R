
box::use(
  bslib[card, card_header, card_footer],
  dplyr[filter, group_by, mutate, near],
  echarts4r[e_charts, e_line, e_tooltip, e_grid, e_color, e_legend,
            e_datazoom, e_x_axis, e_y_axis, e_axis_labels,
            e_toolbox_feature, echarts4rOutput],
  htmlwidgets[JS],
  shiny[textOutput]
)

box::use(
  app/logic/data_tables[pwrTable]
)


# plotting power by effect size filtered by sample size +/- 20
#' @export
left_plot <- function(inputSample, inputAlpha) {
  pwrTable |>
    filter(
      sampleSize %in% c(inputSample - 20, inputSample, inputSample + 20),
      alpha == inputAlpha
    ) |>
    mutate(effectSize = round(effectSize, 2)) |>
    group_by(sampleSize, alpha) |>
    e_charts(effectSize) |>
    e_line(power) |>
    e_tooltip(
      trigger = "item",
      formatter = JS(
        "
          function(params){
            return(
              'Power: ' + params.value[1] +
              '<br />Effect size: ' + params.value[0]
            )
          }
          "
      )
    ) |>
    e_grid(right = '15%') |>
    e_color(c("#f47321", "#777777", "#005030")) |>
    e_legend(
      left = '5',
      title = list("Sample size")
    ) |>
    e_datazoom(type = 'inside') |>
    e_y_axis(power) |>
    e_x_axis(effectSize) |>
    e_axis_labels(x = "Effect \nSize", y = "Power") |>
    e_toolbox_feature(feature = c("saveAsImage"))
}


# plotting the selected effect size as a line on Power vs sample size plot
#' @export
right_plot <- function(chosenEffect, chosenAlpha) {
  pwrTable |>
    filter(
      near(effectSize, chosenEffect),
      alpha == chosenAlpha
    ) |>
    group_by(effectSize, alpha) |>
    e_charts(power) |>
    e_line(sampleSize) |>
    e_color("#777777") |>
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        "
          function(params){
            return(
              'Sample size: ' + params.value[1] +
              '<br />Power: ' + params.value[0]
            )
          }
          "
      )
    ) |>
    e_grid(right = '15%') |>
    e_legend(show = FALSE) |>
    e_datazoom(type = 'inside') |>
    e_y_axis(sampleSize) |>
    e_x_axis(power) |>
    e_axis_labels(x = "Power", y = "Sample \nSize") |>
    e_toolbox_feature(feature = c("saveAsImage"))
}


# this function will create a card container with a custom heading,
# footer, and plot
#' @export
plotting_cards <- function(headerTextOutput, footerTextOutput, displayedPlot) {
  card(
    full_screen = TRUE,
    card_header(headerTextOutput),
    card_footer(footerTextOutput),
    displayedPlot
  )
}
