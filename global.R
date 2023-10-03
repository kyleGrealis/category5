library(shiny)
library(dplyr)
library(tidyr)
library(bslib)
library(pwr)
library(echarts4r)
library(shinydashboardPlus)
library(sass)
library(fresh)

# sass(
#   sass::sass_file('www/styles.scss'),
#   # cache_options = sass_cache_options(FALSE),
#   options = sass_options(output_style = 'compressed'),
#   output = 'www/styles.scss'
# )


pwrTable <-
  expand.grid(
    alpha = c(0.01, 0.025, 0.05),
    effectSize = seq(0.0, 1.0, by = 0.05),
    sampleSize = seq(2, 700, by = 1)
  ) |> 
  mutate(
    power = pwr::pwr.t.test(
      n = sampleSize,
      d = effectSize,
      sig.level = alpha,
      power = NULL
    )$power,
    power = round(power, 2),
  ) |>
  filter(between(power, 0.4, 0.99)) |>
  na.omit()


# for rendering output plots
# Power vs effect size plot
left_plot <- function(chosenSample, chosenAlpha) {
  pwrTable |>
    filter(
      sampleSize %in% c(chosenSample - 20, chosenSample, chosenSample + 20),
      alpha == chosenAlpha
    ) |>
    mutate(effectSize = round(effectSize, 2)) |> 
    group_by(sampleSize, alpha) |>
    e_charts(effectSize) |>
    e_line(power) |>
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
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
    e_legend(
      # orient = 'vertical', 
      left = '5', 
      # top = '45%',
      title = list("Sample size")
    ) |>
    e_datazoom(type = 'inside') |>
    e_y_axis(power) |>
    e_x_axis(effectSize) |> 
    e_axis_labels(x = "Effect \nSize", y = "Power") |> 
    e_toolbox_feature(feature = c("saveAsImage", "dataView"))
}

# plotting the selected effect size as a line on Power vs sample size plot
right_plot <- function(chosenEffect, chosenAlpha) {
  pwrTable |>
    filter(
      near(effectSize, chosenEffect),
      alpha == chosenAlpha
    ) |>
    group_by(effectSize, alpha) |>
    e_charts(power) |>
    e_line(sampleSize) |>
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
    e_toolbox_feature(feature = c("saveAsImage", "dataView"))
}